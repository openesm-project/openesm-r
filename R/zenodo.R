#' Get all versions for a Zenodo record
#'
#' @param zenodo_doi Character string with the Zenodo concept DOI
#' @param sandbox Logical, whether to use Zenodo sandbox
#' @return Dataframe with version information (id, version, doi, date)
#' @keywords internal
#' @importFrom httr2 request req_perform resp_body_json resp_status
#' @importFrom cli cli_abort
#' @noRd
get_zenodo_versions <- function(zenodo_doi, sandbox = FALSE) {
  # extract record ID from DOI
  record_id <- sub(".*zenodo\\.", "", zenodo_doi)
  
  base_url <- if (sandbox) {
    "https://sandbox.zenodo.org/api"
  } else {
    "https://zenodo.org/api"
  }
  
  # get the record to find the versions link
  single_record_url <- paste0(base_url, "/records/", record_id)
  
  response <- httr2::request(single_record_url) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  
  if (httr2::resp_status(response) != 200) {
    cli::cli_abort("Failed to fetch Zenodo record {record_id}")
  }
  
  single_record_data <- httr2::resp_body_json(response)
  
  # use the versions link from the API response
  versions_url <- single_record_data$links$versions
  
  if (is.null(versions_url)) {
    cli::cli_abort("Could not find versions link for record {record_id}")
  }
  
  # get all versions
  response <- httr2::request(versions_url) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  
  if (httr2::resp_status(response) != 200) {
    cli::cli_abort("Failed to fetch Zenodo versions")
  }
  
  data <- httr2::resp_body_json(response)
  
  # extract version information
  versions <- lapply(data$hits$hits, function(hit) {
    meta <- hit$metadata
    version <- meta$version
    if (is.null(version)) {
      version <- meta$publication_date
    }
    
    data.frame(
      id = as.character(hit$id),
      version = version,
      doi = meta$doi,
      date = meta$publication_date,
      stringsAsFactors = FALSE
    )
  })
  
  # combine into data frame
  versions_df <- do.call(rbind, versions)
  
  # sort by date descending (most recent first)
  versions_df <- versions_df[order(versions_df$date, decreasing = TRUE), ]
  
  return(versions_df)
}

#' Resolve a Zenodo version
#'
#' Given a concept DOI, finds the specific version tag. If "latest" is requested,
#' it returns the most recent version tag.
#'
#' @param zenodo_doi Character string with the Zenodo concept DOI.
#' @param version Character string, either "latest" or a specific version tag (e.g., "v1.0.0").
#' @param sandbox Logical, whether to use Zenodo sandbox.
#' @param max_attempts Integer, maximum number of retry attempts for Zenodo API calls. Default is 15.
#' @return Character string with the resolved version tag.
#' @keywords internal
#' @importFrom dplyr arrange desc slice pull
#' @importFrom cli cli_abort cli_alert_warning
#' @noRd
resolve_zenodo_version <- function(zenodo_doi, version = "latest", sandbox = FALSE, max_attempts = 15) {
  # retry logic for flaky zenodo api calls
  attempt <- 1
  data_versions <- NULL
  
  while (attempt <= max_attempts) {
    tryCatch({
      data_versions <- get_zenodo_versions(zenodo_doi, sandbox = sandbox)
      
      # verify we got valid data
      if (is.data.frame(data_versions) && nrow(data_versions) > 0) {
        break
      }
    }, error = function(e) {
      if (attempt < max_attempts) {
        if (!isTRUE(getOption("openesm.quiet"))) {
          cli::cli_alert_warning("Zenodo API call failed (attempt {attempt}/{max_attempts}), retrying...")
        }
        Sys.sleep(0.5 * attempt)  # backoff time
      }
    })
    attempt <- attempt + 1
  }
  
  if (is.null(data_versions) || nrow(data_versions) == 0) {
    cli::cli_abort("Failed to retrieve versions from Zenodo for DOI {zenodo_doi} after {max_attempts} attempts")
  }
  
  if (version == "latest") {
    latest_version_tag <- data_versions |>
      dplyr::arrange(dplyr::desc(date)) |>
      dplyr::slice(1) |>
      dplyr::pull(version)
    return(latest_version_tag)
  } else {
    version <- sub("^v", "", version)
    if (!version %in% data_versions$version) {
      available_versions <- paste(data_versions$version, collapse = ", ")
      cli::cli_abort("Version {version} not found. Available versions: {available_versions}")
    }
    return(version)
  }
}

#' Download dataset from Zenodo
#'
#' Downloads a specific dataset file from Zenodo using a version-specific DOI.
#' The record ID is extracted directly from the DOI, so no additional API calls
#' are needed to resolve the version.
#'
#' @param version_doi Character string with the version-specific Zenodo DOI
#'   (e.g., "10.5281/zenodo.1234567"). Must be a version DOI, not a concept DOI.
#' @param dataset_id Character string with dataset identifier
#' @param author_name Character string with author name
#' @param sandbox Logical, whether to use Zenodo sandbox. Default is \code{FALSE}
#' @param dest_path Character string with destination path. If \code{NULL}, uses filename only
#' @return Character string with path to downloaded file
#' @keywords internal
#' @noRd
download_from_zenodo <- function(version_doi,
                                 dataset_id,
                                 author_name,
                                 sandbox = FALSE,
                                 dest_path = NULL) {
  specific_record_id <- sub(".*zenodo\\.", "", version_doi)

  filename <- paste0(dataset_id, "_", author_name, "_ts.tsv")

  if (isTRUE(sandbox)) {
    download_url <- paste0("https://sandbox.zenodo.org/records/", specific_record_id, "/files/", filename)
  } else {
    download_url <- paste0("https://zenodo.org/records/", specific_record_id, "/files/", filename)
  }

  if (is.null(dest_path)) {
    dest_path <- filename
  }

  download_with_progress(download_url, dest_path)

  return(dest_path)
}