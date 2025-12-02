#' Internal utility functions for openesm package
#' @keywords internal

#' Get the openesm cache directory
#' @param type Optional subdirectory within the cache
#' @return Path to cache directory
#' @keywords internal
#' @noRd
get_cache_dir <- function(type = NULL) {
  # use tools::R_user_dir for standard R cache location
  base_cache <- tools::R_user_dir("openesm", which = "cache")
  
  # if a type is specified, return the subdirectory
  if (!is.null(type)) {
    cache_dir <- fs::path(base_cache, type)
  } else {
    cache_dir <- base_cache
  }
  
  # ensure directory exists
  if (!fs::dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  }
  return(cache_dir)
}

#' Display information about the openesm cache
#'
#' Shows the location and total size of the local file cache.
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' \donttest{
#' # view cache information
#' cache_info()
#' }
#' @importFrom cli cli_alert_info
#' @importFrom fs dir_exists dir_info fs_bytes
#' @export
cache_info <- function() {
  cache_dir <- get_cache_dir()
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist yet.")
    cli::cli_alert_info("It will be created at: {.path {cache_dir}}")
    return(invisible(NULL))
  }
  
  dir_contents <- fs::dir_info(cache_dir, recurse = TRUE)
  total_size <- sum(dir_contents$size)
  
  cli::cli_alert_info("Cache location: {.path {cache_dir}}")
  cli::cli_alert_info("Cache size: {.val {fs::fs_bytes(total_size)}}")
}

#' Clear the openesm cache
#'
#' Removes all cached openesm data from your local machine.
#'
#' @param force Logical. If \code{TRUE}, will not ask for confirmation before 
#'   deleting. Default is \code{FALSE}.
#' @return Invisibly returns \code{NULL}.
#' @examples
#' \donttest{
#' # clear cache with confirmation prompt
#' if(interactive()) {
#'  clear_cache()
#' }
#' 
#' # force clear without confirmation
#' clear_cache(force = TRUE)
#' }
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#' @importFrom fs dir_exists
#' @importFrom utils askYesNo
#' @export
clear_cache <- function(force = FALSE) {
  cache_dir <- get_cache_dir()
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist. Nothing to clear.")
    return(invisible(NULL))
  }

  confirmed <- FALSE
  if (force) {
    confirmed <- TRUE
  } else if (interactive()) {
    cli::cli_alert_info("This will delete all cached data at {.path {cache_dir}}")
    # use the base R function for confirmation
    confirmed <- utils::askYesNo("Are you sure you want to proceed?", default = FALSE)
  } else {
    cli::cli_abort("Cannot ask for confirmation in a non-interactive session. Use {.code clear_cache(force = TRUE)}.")
  }

  # askYesNo can return NA if the user cancels, so check specifically for TRUE
  if (isTRUE(confirmed)) {
    unlink(cache_dir, recursive = TRUE)
    cli::cli_alert_success("Cache cleared.")
  } else {
    cli::cli_alert_info("Cache not cleared.")
  }

  return(invisible(NULL))
}

#' Get path to metadata cache
#' @keywords internal
#' @noRd
get_metadata_dir <- function() {
  get_cache_dir(type = "metadata")
}

#' Get path to data cache
#' @keywords internal
#' @noRd
get_data_dir <- function() {
  get_cache_dir(type = "data")
}

#' Check if running in interactive mode with CLI messaging
#' @keywords internal
#' @noRd
is_interactive_cli <- function() {
  interactive() && !isTRUE(getOption("openesm.quiet"))
}

#' Create a consistent message format
#' @keywords internal
#' @noRd
msg_info <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_info(..., .envir = .envir)
  }
}

#' Create a consistent warning format
#' @keywords internal
#' @noRd
msg_warn <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_warning(..., .envir = .envir)
  }
}

#' Create a consistent success format
#' @keywords internal
#' @noRd
msg_success <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_success(..., .envir = .envir)
  }
}


#' Read JSON with error handling
#' @keywords internal
#' @noRd
read_json_safe <- function(path) {
  tryCatch({
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to read JSON file",
      "x" = "Path: {.path {path}}",
      "i" = "Error: {e$message}"
    ))
  })
}

#' Download file with progress
#' @keywords internal
#' @noRd
download_with_progress <- function(url, destfile) {
  if (is_interactive_cli()) {
    cli::cli_progress_step("Downloading from {.url {url}}")
  }
  
  tryCatch({
    response <- httr2::request(url) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_progress() |>
      httr2::req_perform()
    
    # Write to file
    writeBin(httr2::resp_body_raw(response), destfile)
    
    if (is_interactive_cli()) {
      cli::cli_progress_done()
    }
    
    TRUE
  }, error = function(e) {
    if (is_interactive_cli()) {
      cli::cli_progress_done(result = "failed")
    }
    cli::cli_abort(c(
      "Download failed",
      "x" = "URL: {.url {url}}",
      "i" = "Error: {e$message}"
    ))
  })
}

#' Construct dataset path
#' @keywords internal
#' @noRd
get_cache_path <- function(dataset_id,
                           version,
                           filename,
                           type = c("metadata", "data")) {
  type <- match.arg(type)
  base_dir <- if (type == "metadata")
    get_metadata_dir()
  else
    get_data_dir()
  
  # simplified path
  path <- fs::path(base_dir, dataset_id, version, filename)
  
  # ensure the directory for the file exists before returning the path
  fs::dir_create(fs::path_dir(path))
  
  return(path)
}

#' Process specific dataset metadata
#'
#' Helper function to process the raw list from a specific dataset's
#' metadata JSON into a clean, one-row tibble.
#'
#' @param raw_meta The raw list parsed from the metadata json file.
#' @return A one-row tibble.
#' @keywords internal
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @noRd
process_specific_metadata <- function(raw_meta) {
  # helper to safely get a value, converting NULL or empty list to NA
  get_val <- function(field, type = "character") {
    val <- raw_meta[[field]]
    if (is.null(val) || (is.list(val) && length(val) == 0)) {
      if (type == "character") return(NA_character_)
      if (type == "integer") return(NA_integer_)
      return(NA)
    }
    if (is.list(val) || length(val) > 1) {
      return(paste(val, collapse = ", "))
    }
    return(val)
  }

  # create the nested tibble for features
  features_tibble <- if (!is.null(raw_meta$features) && length(raw_meta$features) > 0) {
    dplyr::bind_rows(raw_meta$features)
  } else {
    tibble::tibble()
  }

  # create a clean, one-row tibble of all metadata fields
  tibble::tibble(
    dataset_id = get_val("dataset_id"),
    first_author = get_val("first_author"),
    year = get_val("year", "integer"),
    reference_a = get_val("reference_a"),
    reference_b = get_val("reference_b"),
    paper_doi = get_val("paper_doi"),
    zenodo_doi = get_val("zenodo_doi"),
    license = get_val("license"),
    link_to_data = get_val("link_to_data"),
    link_to_codebook = get_val("link_to_codebook"),
    link_to_code = get_val("link_to_code"),
    n_participants = get_val("n_participants", "integer"),
    n_time_points = get_val("n_time_points", "integer"),
    n_days = get_val("n_days"),
    n_beeps_per_day = get_val("n_beeps_per_day"),
    passive_data_available = get_val("passive_data_available"),
    cross_sectional_available = get_val("cross_sectional_available"),
    topics = get_val("topics"),
    implicit_missingness = get_val("implicit_missingness"),
    raw_time_stamp = get_val("raw_time_stamp"),
    sampling_scheme = get_val("sampling_scheme"),
    participants = get_val("participants"),
    coding_file = get_val("coding_file"),
    additional_comments = get_val("additional_comments"),
    features = list(features_tibble)
  )
}

#' Download and extract metadata ZIP from Zenodo
#'
#' Downloads the metadata ZIP file from Zenodo and extracts all contents
#' including datasets.json and individual dataset metadata folders.
#'
#' @param version Character string specifying the metadata version
#' @param dest_dir Character string with destination directory for extracted files
#' @param sandbox Logical, whether to use Zenodo sandbox. Default is \code{FALSE}
#' @param max_attempts Integer, maximum number of retry attempts for Zenodo API calls. Default is 15
#' @return Character string with path to destination directory containing all extracted metadata
#' @keywords internal
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom httr2 request req_perform resp_body_json resp_status
#' @importFrom fs dir_exists
#' @noRd
download_metadata_from_zenodo <- function(version = "latest", dest_dir, sandbox = FALSE, max_attempts = 15) {
  metadata_doi <- "10.5281/zenodo.17182171"
  
  # resolve version using existing zenodo utilities with retry logic
  resolved_version <- resolve_zenodo_version(metadata_doi, version, sandbox = sandbox, max_attempts = max_attempts)
  
  # get versions with retry logic
  attempt <- 1
  data_versions <- NULL
  
  while (attempt <= max_attempts) {
    tryCatch({
      data_versions <- get_zenodo_versions(metadata_doi, sandbox = sandbox)
      
      if (is.data.frame(data_versions) && nrow(data_versions) > 0) {
        break
      }
    }, error = function(e) {
      if (attempt < max_attempts) {
        if (!isTRUE(getOption("openesm.quiet"))) {
          cli::cli_alert_warning("Zenodo API call failed (attempt {attempt}/{max_attempts}), retrying...")
        }
        Sys.sleep(0.5 * attempt)
      }
    })
    attempt <- attempt + 1
  }
  
  if (is.null(data_versions) || nrow(data_versions) == 0) {
    cli::cli_abort("Failed to retrieve versions from Zenodo for metadata repository after {max_attempts} attempts")
  }
  
  version_record <- data_versions[data_versions$version == resolved_version, ]
  
  if (nrow(version_record) == 0) {
    cli::cli_abort("Version {resolved_version} not found for metadata repository")
  }
  
  # extract record ID from DOI and construct filename
  specific_version_doi <- version_record$doi
  record_id <- sub(".*zenodo\\.", "", specific_version_doi)
  
  # construct API URL to get file information
  base_url <- if (sandbox) "https://sandbox.zenodo.org/api" else "https://zenodo.org/api"
  api_url <- paste0(base_url, "/records/", record_id)
  
  # fetch record details to get file list
  response <- httr2::request(api_url) |>
    httr2:req_retry(max_tries = 5) |>
    httr2::req_perform()
    
  if (httr2::resp_status(response) != 200) {
    cli::cli_abort("Failed to fetch record details from Zenodo API")
  }
  
  record_data <- httr2::resp_body_json(response)
  files <- record_data$files
  
  # find the ZIP file (should be the only .zip file)
  zip_files <- files[grepl("\\.zip$", sapply(files, function(f) f$key))]
  
  if (length(zip_files) == 0) {
    cli::cli_abort("No ZIP file found in Zenodo metadata record version {resolved_version}")
  }
  
  # use the first (and only) ZIP file
  zip_file <- zip_files[[1]]
  download_url <- zip_file$links$self
  
  # download the ZIP file
  zip_path <- file.path(dest_dir, "metadata.zip")
  download_with_progress(download_url, zip_path)
  
  # extract to temporary directory to inspect structure
  temp_extract_dir <- file.path(dest_dir, "temp_extract")
  if (fs::dir_exists(temp_extract_dir)) {
    unlink(temp_extract_dir, recursive = TRUE)
  }
  
  utils::unzip(zip_path, exdir = temp_extract_dir, overwrite = TRUE)
  
  # find the root folder in the extracted archive (typically openesm-metadata-main or similar)
  extracted_contents <- list.dirs(temp_extract_dir, recursive = FALSE, full.names = TRUE)
  
  if (length(extracted_contents) == 0) {
    cli::cli_abort("No contents found in extracted ZIP file")
  }
  
  # there's a single root folder in the ZIP
  root_folder <- extracted_contents[1]
  
  # find datasets.json to verify structure
  datasets_json_path <- file.path(root_folder, "datasets.json")
  if (!file.exists(datasets_json_path)) {
    # try to find it recursively
    datasets_json_files <- list.files(temp_extract_dir, 
                                      pattern = "^datasets\\.json$", 
                                      recursive = TRUE, 
                                      full.names = TRUE)
    if (length(datasets_json_files) == 0) {
      cli::cli_abort("datasets.json not found in downloaded ZIP file")
    }
    root_folder <- dirname(datasets_json_files[1])
  }
  
  # move all contents from root folder to dest_dir
  all_items <- list.files(root_folder, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  for (item in all_items) {
    item_name <- basename(item)
    dest_item <- file.path(dest_dir, item_name)
    # remove existing if present
    if (file.exists(dest_item)) {
      unlink(dest_item, recursive = TRUE)
    }
    file.rename(item, dest_item)
  }
  
  # clean up temporary files
  unlink(temp_extract_dir, recursive = TRUE)
  file.remove(zip_path)
  
  return(dest_dir)
}

