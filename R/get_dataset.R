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
#' @importFrom cli cli_abort cli_alert_success
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
    return(get_multiple_datasets(dataset_id, version, cache, force_download, sandbox, max_attempts, ...))
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
  
  # resolve actual version if "latest" is requested
  actual_version <- resolve_zenodo_version(zenodo_doi, version, sandbox, max_attempts = max_attempts)
  
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
      zenodo_doi = zenodo_doi,
      dataset_id = dataset_id,
      author_name = author_lower,
      version = actual_version,
      sandbox = sandbox,
      dest_path = local_data_path,
      max_attempts = max_attempts
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
  
  # explicitly print upon download, unless silenced
  if (!quiet) {
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
                                  ...) {
  result <- list()
  for (id in dataset_ids) {
    # call get_dataset in 'quiet' mode to suppress individual prints
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download,
      sandbox = sandbox,
      quiet = TRUE,
      max_attempts = max_attempts,
      ...
    )
  }
  # assign a special class to the list for custom printing
  result <- structure(result, class = c("openesm_dataset_list", "list"))
  
  # explicitly print the summary for the user
  print(result)
  
  return(invisible(result))
}
