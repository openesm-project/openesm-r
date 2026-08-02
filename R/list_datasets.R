#' List available ESM datasets from openESM repository
#'
#' Retrieves a list of available Experience Sampling Method (ESM) datasets from 
#' the openESM metadata repository. Returns a tibble with 
#' dataset information and metadata that can be used with [get_dataset()].
#'
#' @param cache_hours Numeric. Number of hours to consider the cached dataset 
#'   index valid. Default is 24. Set to 0 to force fresh download.
#' @param metadata_version Character string specifying the metadata catalog version. 
#'   Default is "latest" which downloads the most recent version.
#' @param max_attempts Integer, maximum number of retry attempts for Zenodo API calls. Default is 15.
#'
#' @return A tibble with one row per dataset containing:
#'   \itemize{
#'     \item \code{dataset_id}: Character string with unique dataset identifier
#'     \item \code{first_author}: Character string with first author's surname
#'     \item \code{year}: Numeric year of publication
#'     \item \code{reference_a}: Character string with primary reference
#'     \item \code{reference_b}: Character string with secondary reference (if available)
#'     \item \code{paper_doi}: Character string with publication DOI
#'     \item \code{zenodo_doi}: Character string with Zenodo dataset DOI
#'     \item \code{license}: Character string with dataset license
#'     \item \code{link_to_data}: Character string with direct data link
#'     \item \code{link_to_codebook}: Character string with codebook link
#'     \item \code{link_to_code}: Character string with analysis code link
#'     \item \code{n_participants}: Numeric number of participants
#'     \item \code{n_time_points}: Numeric number of time points
#'     \item \code{n_beeps_per_day}: Character string with beeps per day information
#'     \item \code{passive_data_available}: Character string indicating passive data availability
#'     \item \code{cross_sectional_available}: Character string indicating cross-sectional data availability
#'     \item \code{topics}: Character string with study topics
#'     \item \code{implicit_missingness}: Character string with missingness information
#'     \item \code{raw_time_stamp}: Character string with timestamp format information
#'     \item \code{sampling_scheme}: Character string with sampling scheme details
#'     \item \code{participants}: Character string with participant information
#'     \item \code{coding_file}: Character string with coding file information
#'     \item \code{additional_comments}: Character string with additional notes
#'     \item \code{features}: List column containing feature tibbles for each dataset
#'   }
#'
#' @details
#' This function downloads and caches a master index of available datasets from 
#' the openESM metadata repository, which is hosted on Zenodo. 
#' The index is cached locally to avoid repeated downloads. 
#' Use \code{cache_hours = 0} to force a fresh download of the index.
#' 
#' The returned tibble can be filtered and explored to identify datasets of
#' interest before downloading with [get_dataset()].
#'
#' Requires an active internet connection. Download time depends on Zenodo
#' availability and the size of the metadata archive.
#'
#' @seealso
#' [get_dataset()] to download specific datasets
#'
#' @importFrom fs file_exists dir_exists dir_create
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
#'
#' @examplesIf interactive()
#' # List all available datasets
#' datasets <- list_datasets()
#' head(datasets)
#'
#' # Force a fresh download regardless of cache age
#' fresh_list <- list_datasets(cache_hours = 0)
#'
#' # Pin a specific metadata catalog version
#' datasets_v1 <- list_datasets(metadata_version = "1.0.0")
#'
#' # Use the returned IDs with get_dataset()
#' dataset <- get_dataset(datasets$dataset_id[1])
#'
#' @export
list_datasets <- function(cache_hours = 24,
                          metadata_version = "latest",
                          max_attempts = 15) {
  # resolve the version first to get the actual version tag
  metadata_doi <- "10.5281/zenodo.17182171"
  resolved_version <- resolve_zenodo_version(metadata_doi, metadata_version, sandbox = FALSE, max_attempts = max_attempts)
  
  # define the path to the cached metadata index file using resolved version
  metadata_dir <- get_cache_dir("metadata")
  version_dir <- file.path(metadata_dir, resolved_version)
  index_path <- file.path(version_dir, "datasets.json")
  
  # determine if we need to download a fresh copy
  use_cache <- FALSE
  if (fs::file_exists(index_path)) {
    file_age_hours <- difftime(Sys.time(), file.info(index_path)$mtime, units = "hours")
    if (file_age_hours < cache_hours) {
      msg_info("Using cached dataset index (less than {.val {cache_hours}} hours old).")
      use_cache <- TRUE
    }
  }
  
  if (!use_cache) {
    # otherwise, download a fresh copy from Zenodo
    msg_info("Downloading fresh metadata index from Zenodo.")
    
    # ensure the version directory exists
    if (!fs::dir_exists(version_dir)) {
      fs::dir_create(version_dir, recurse = TRUE)
    }
    
    # download and extract from Zenodo using resolved version
    # this now returns the base directory containing all metadata
    base_dir <- download_metadata_from_zenodo(resolved_version, version_dir, max_attempts = max_attempts)
    
    # verify datasets.json exists
    if (!fs::file_exists(index_path)) {
      cli::cli_abort("Failed to extract datasets.json from Zenodo metadata")
    }
  }
  
  # read the file and process it
  raw_list <- read_json_safe(index_path)
  return(process_raw_datasets_list(raw_list))
}

#' Process Raw Datasets List
#'
#' Helper function to process the raw list from JSON into a tidy tibble.
#'
#' @param raw_list The raw list parsed from the datasets.json file.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @return A tibble.
#' @noRd
process_raw_datasets_list <- function(raw_list) {
  datasets_list <- raw_list$datasets
  
  # iterate over each dataset, applying the same
  # processing function used by get_dataset()
  purrr::map_dfr(datasets_list, process_specific_metadata)
}
