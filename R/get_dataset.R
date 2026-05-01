#' Download ESM dataset(s) from openESM repository
#'
#' Downloads one or more Experience Sampling Method (ESM) datasets from the
#' openESM repository hosted on Zenodo. Returns an S3 object containing the
#' dataset and associated metadata.
#'
#' @param dataset_id Character string or vector of dataset IDs. Use
#'   [list_datasets()] to see available datasets.
#' @param version Character string specifying the dataset version. Default is
#'   "latest" which downloads the most recent version.
#' @param path Character string specifying custom download path. If \code{NULL}
#'   (default), files are cached in the user's cache directory.
#' @param cache Logical. If \code{TRUE} (default), uses cached version if
#'   available and not expired.
#' @param force_download Logical. If \code{TRUE}, forces re-download even if
#'   cached version exists. Default is \code{FALSE}.
#' @param sandbox Logical. If \code{TRUE}, uses Zenodo sandbox environment
#'   for testing. Default is \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppresses informational messages.
#'   Default is \code{FALSE}.
#' @param max_attempts Integer, maximum number of retry attempts for Zenodo API calls. Default is 15.
#' @param ... Additional arguments passed to [list_datasets()].
#' This includes \code{metadata_version} to specify the metadata catalog version.
#'
#' @return For single dataset: An S3 object of class \code{openesm_dataset}
#'   containing:
#'   \itemize{
#'     \item \code{data}: A tibble with the ESM data
#'     \item \code{metadata}: List with dataset metadata
#'     \item \code{dataset_id}: Character string with dataset identifier
#'     \item \code{dataset_version}: Character string with dataset version number
#'     \item \code{metadata_version}: Character string with metadata catalog version
#'   }
#'   For multiple datasets: An S3 object of class \code{openesm_dataset_list}
#'   containing a named list of \code{openesm_dataset} objects.
#'
#' @details
#' This function downloads ESM datasets from Zenodo using DOIs stored in the
#' openESM metadata repository. Datasets are cached locally to avoid repeated
#' downloads. Use \code{force_download = TRUE} to refresh cached data.
#'
#' The function handles both individual datasets and batch downloads. When
#' downloading multiple datasets, progress is shown for each download.
#'
#' @seealso
#' [list_datasets()] for available datasets,
#' [cite()] for citation information
#'
#' @importFrom cli cli_abort cli_alert_success cli_inform cli_warn
#' @importFrom readr read_tsv
#' @importFrom fs file_exists path
#'
#' @examples
#' \donttest{
#' # List available datasets first
#' available <- list_datasets()
#' head(available)
#'
#' # Download a single dataset
#' dataset <- get_dataset("0001")
#'
#' # Access the data
#' head(dataset$data)
#'
#' # View metadata and provenance information
#' dataset$metadata
#' dataset$dataset_version  # Dataset version
#' dataset$metadata_version # Metadata catalog version
#'
#' # Download multiple datasets
#' datasets <- get_dataset(c("0001", "0002"))
#'
#' # Access individual datasets from the list
#' datasets[["0001"]]$data
#'
#' # Use specific metadata catalog version
#' dataset_v1 <- get_dataset("0001", metadata_version = "1.0.0")
#'
#' # Force re-download to get latest version
#' dataset_fresh <- get_dataset("0001", force_download = TRUE)
#'
#' 
#' \dontshow{
#' # cleanup
#' clear_cache(force = TRUE)
#' }
#' }
#'
#' @export
get_dataset <- function(dataset_id,
                        version = "latest",
                        cache = TRUE,
                        path = NULL,
                        force_download = FALSE,
                        sandbox = FALSE,
                        quiet = FALSE,
                        max_attempts = 15,
                        ...) {
  # handle multiple datasets
  if (length(dataset_id) > 1) {
    return(get_multiple_datasets(dataset_id, version, cache, force_download, sandbox, max_attempts, quiet = quiet, ...))
  }
  
  # remove all non-numeric characters from dataset_id
  dataset_id <- gsub("[^0-9]", "", dataset_id)
  
  # resolve metadata version to track provenance
  metadata_doi <- "10.5281/zenodo.17182171"
  dots <- list(...)
  metadata_version_requested <- dots$metadata_version %||% "latest"
  resolved_metadata_version <- resolve_zenodo_version(metadata_doi, metadata_version_requested, sandbox = FALSE, max_attempts = max_attempts)
  
  # get dataset catalog
  all_datasets <- list_datasets(...)
  if (!dataset_id %in% all_datasets$dataset_id) {
    cli::cli_abort("Dataset with id {.val {dataset_id}} not found.")
  }
  # get dataset info
  # remove any NA ids
  all_datasets <- all_datasets[!is.na(all_datasets[["dataset_id"]]), ]
  dataset_info <- all_datasets[all_datasets[["dataset_id"]] == dataset_id, ]
  
  author_lower <- tolower(dataset_info$first_author)
  author_lower <- gsub("\u00f6", "oe", author_lower, fixed = TRUE)
  author_lower <- gsub("\u00e4", "ae", author_lower, fixed = TRUE)
  author_lower <- gsub("\u00fc", "ue", author_lower, fixed = TRUE)
  # remove whitespace
  author_lower <- gsub(" ", "", author_lower)
  
  # construct path to individual metadata file in cached Zenodo structure
  metadata_filename <- paste0(dataset_id, "_", author_lower, "_metadata.json")
  metadata_folder <- paste0(dataset_id, "_", author_lower)
  
  # metadata is versioned with the metadata catalog version, not dataset version
  metadata_base_dir <- file.path(get_cache_dir("metadata"), resolved_metadata_version)
  local_metadata_path <- file.path(metadata_base_dir, "datasets", metadata_folder, metadata_filename)
  
  # if metadata doesn't exist or force_download is TRUE, ensure we have the full metadata archive
  if (!fs::file_exists(local_metadata_path) || force_download) {
    # trigger download by calling list_datasets, which will download and extract if needed
    msg_info("Downloading metadata catalog version {.val {resolved_metadata_version}}")
    list_datasets(cache_hours = 0, metadata_version = resolved_metadata_version, max_attempts = max_attempts)
    
    # verify metadata file now exists
    if (!fs::file_exists(local_metadata_path)) {
      cli::cli_abort("Metadata file not found for dataset {.val {dataset_id}} in version {.val {resolved_metadata_version}}")
    }
  }
  
  specific_meta_raw <- read_json_safe(local_metadata_path)
  
  # get concept DOI from metadata
  zenodo_doi <- specific_meta_raw$zenodo_doi
  
  if (is.null(zenodo_doi)) {
    cli::cli_abort("No Zenodo DOI found in metadata for dataset {dataset_id}")
  }
  
  # resolve actual version and look up the version-specific DOI
  actual_version <- resolve_zenodo_version(zenodo_doi, version, sandbox, max_attempts = max_attempts)
  all_versions <- get_zenodo_versions(zenodo_doi, sandbox = sandbox)
  version_doi <- all_versions$doi[all_versions$version == actual_version][1]
  if (is.na(version_doi) || is.null(version_doi)) {
    cli::cli_abort("Could not resolve version-specific DOI for dataset {dataset_id} version {actual_version}")
  }

  # determine cache/destination path
  filename <- paste0(dataset_id, "_", author_lower, "_ts.tsv")
  if (is.null(path)) {
    local_data_path <- get_cache_path(
      dataset_id,
      filename = filename,
      type = "data",
      version = actual_version
    )
  } else {
    # ensure custom path directory exists
    if (!fs::dir_exists(path)) {
      fs::dir_create(path, recurse = TRUE)
    }
    local_data_path <- fs::path(path, filename)
  }
  
  # download from Zenodo if needed
  if (!fs::file_exists(local_data_path) || force_download) {
    download_from_zenodo(
      version_doi = version_doi,
      dataset_id = dataset_id,
      author_name = author_lower,
      sandbox = sandbox,
      dest_path = local_data_path
    )
  }
  
  # load dataset
  cli::cli_alert_success("Loading dataset {.val {dataset_id}} version {.val {actual_version}}")
  data <- readr::read_tsv(local_data_path, show_col_types = FALSE)
  
  # format metadata for cleaner output
  formatted_meta <- as.list(process_specific_metadata(specific_meta_raw))
  
  # add metadata and class
  dataset <- structure(
    list(
      data = data,
      metadata = formatted_meta,
      dataset_id = dataset_id,
      dataset_version = actual_version,
      metadata_version = resolved_metadata_version
    ),
    class = "openesm_dataset"
  )
  
  if (!quiet) {
    repro_call <- paste0(
      'get_dataset("', dataset_id, '", ',
      'metadata_version = "', resolved_metadata_version, '", ',
      'version = "', actual_version, '")'
    )
    cli::cli_inform("For full reproducibility, use:\n{repro_call}")
    print(dataset)
  }

  return(invisible(dataset))
}

#' Helper function for multiple datasets
#'
#' This function handles downloading multiple datasets by calling
#' [get_dataset()] for each dataset ID in the input vector.
#' This is used internally by [get_dataset()] when multiple IDs
#' are provided.
#' @param dataset_ids Character vector of dataset IDs to download.
#' @param version Character string specifying the dataset version. Default is
#'   "latest" which downloads the most recent version.
#' @param cache Logical. If \code{TRUE} (default), uses cached version if
#'   available and not expired.
#' @param force_download Logical. If \code{TRUE}, forces re-download even if
#'  cached version exists. Default is \code{FALSE}.
#' @param sandbox Logical. If \code{TRUE}, uses Zenodo sandbox environment
#'    for testing. Default is \code{FALSE}.
#' @param max_attempts Integer, maximum number of retry attempts for Zenodo API calls.
#' @keywords internal
#' @noRd
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download,
                                  sandbox,
                                  max_attempts,
                                  quiet = FALSE,
                                  ...) {
  n <- length(dataset_ids)

  if (length(version) == 1) {
    # warn only when the user explicitly pinned a version (recycling "latest" is expected)
    if (version != "latest") {
      cli::cli_warn(
        "Recycling {.val {version}} across all {n} datasets. Pass a vector of versions to pin each dataset individually."
      )
    }
    version <- rep(version, n)
  } else if (length(version) != n) {
    cli::cli_abort(
      "{.arg version} must be length 1 or the same length as {.arg dataset_id} ({n}), not {length(version)}."
    )
  }

  result <- list()
  for (i in seq_along(dataset_ids)) {
    result[[dataset_ids[[i]]]] <- get_dataset(
      dataset_ids[[i]],
      version = version[[i]],
      cache = cache,
      force_download = force_download,
      sandbox = sandbox,
      quiet = TRUE,
      max_attempts = max_attempts,
      ...
    )
  }

  result <- structure(result, class = c("openesm_dataset_list", "list"))

  if (!quiet) {
    # build reproducibility message using resolved versions from each downloaded dataset
    resolved_ids      <- vapply(result, \(d) d$dataset_id, character(1))
    resolved_dv       <- vapply(result, \(d) d$dataset_version, character(1))
    resolved_mv       <- result[[1]]$metadata_version  # same for all (single metadata_version per call)

    ids_r   <- paste0('c("', paste(resolved_ids, collapse = '", "'), '")')
    dvs_r   <- paste0('c("', paste(resolved_dv,  collapse = '", "'), '")')
    repro_call <- paste0(
      "get_dataset(", ids_r, ",\n",
      '            metadata_version = "', resolved_mv, '",\n',
      "            version = ", dvs_r, ")"
    )
    cli::cli_inform("For full reproducibility, use:\n{repro_call}")
    print(result)
  }

  return(invisible(result))
}
