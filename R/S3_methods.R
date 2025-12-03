#' S3 Methods for openesm objects

#' Print method for openesm_dataset
#'
#' @param x An object of class \code{openesm_dataset}.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns the original object.
#' @importFrom cli cli_h1 cli_text cli_alert_info cli_bullets cli_div
#' @examples
#' \donttest{
#' dataset <- get_dataset("0001")
#' print(dataset)
#' }
#' @export
print.openesm_dataset <- function(x, ...) {
  # apply a local theme just for this output
  cli::cli_div(theme = list(
    span.code = list(
      color = "blue",
      "font-style" = "italic",
      before = "", # remove opening `
      after = ""   # remove closing `
    )
  ))

  cli::cli_h1("openESM Dataset: {.val {x$dataset_id}}")

  meta <- x$metadata

  bullets <- c(
    "*" = "Dataset version: {.val {x$dataset_version}}",
    "*" = "Metadata version: {.val {x$metadata_version}}",
    "*" = "Authors: {meta$first_author} et al. ({meta$year})",
    "*" = "Paper DOI: {meta$paper_doi}",
    "*" = "License: {meta$license}",
    "*" = "Data: A tibble with {meta$n_participants} participants and {meta$n_time_points} maximum time points per participant"
  )

  cli::cli_bullets(bullets)
  cli::cli_alert_info("Use {.code cite(dataset)} for citation information.")
  cli::cli_alert_info("Use {.code notes(dataset)} for additional information about the dataset.")
  cli::cli_alert_info("Please ensure you follow the license terms for this dataset.")
  invisible(x)
}



#' Print method for a list of openesm_dataset objects
#'
#' @param x An object of class \code{openesm_dataset_list}.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns the original object.
#' @importFrom cli cli_h1 cli_text cli_bullets cli_alert_info cli_div style_bold
#' @examples
#' \donttest{
#' datasets <- get_dataset(c("0001", "0002"))
#' print(datasets)
#' }
#' @export
print.openesm_dataset_list <- function(x, ...) {
  # apply a local theme just for this output
  cli::cli_div(theme = list(
    span.code = list(
      color = "blue",
      "font-style" = "italic",
      before = "", # remove opening `
      after = ""   # remove closing `
    )
  ))

  num_datasets <- length(x)
  cli::cli_h1("Collection of {num_datasets} openESM Dataset{?s}")

  # show the names of the first few datasets
  max_show <- 5
  dataset_names <- names(x)

  bullets <- paste0("* ", cli::style_bold(dataset_names[1:min(num_datasets, max_show)]))

  if (num_datasets > max_show) {
    bullets <- c(bullets, "  ... and {num_datasets - max_show} more.")
  }

  cli::cli_bullets(bullets)
  cli::cli_alert_info("Access individual datasets using {.code list_name$dataset_id}")

  invisible(x)
}



#' Get citation information for dataset
#' 
#' @param x An object of class \code{openesm_dataset}.
#' @param format Character string specifying citation format. Currently only "bibtex" is supported.
#' @param ... Additional arguments (currently unused).
#' @return Character string with citation information, returned invisibly.
#' @examples
#' \donttest{
#' # Minimal object to demonstrate the method (no API call)
#' dataset <- structure(
#'   list(
#'     dataset_id = "0001",
#'     metadata = list(
#'       reference_a = "@article{fried2022, author = {Fried}, year = {2022}}"
#'     )
#'   ),
#'   class = "openesm_dataset"
#' )
#' cite(dataset)
#' }
#' @importFrom cli cli_abort cli_alert_info cli_text cli_code
#' @export
cite.openesm_dataset <- function(x, format = "bibtex", ...) {
  if (tolower(format) != "bibtex") {
    cli::cli_abort(c("Unsupported format: {.val {format}}. Only {.val bibtex} is currently supported."))
  }
  
  meta <- x$metadata
  
  # Helper to check if a reference string is valid (not NULL, NA, or empty)
  is_valid_ref <- function(ref) {
    !is.null(ref) && !is.na(ref) && nzchar(trimws(ref))
  }
  
  citations <- c()
  if (is_valid_ref(meta$reference_a)) {
    citations <- c(citations, meta$reference_a)
  }
  if (is_valid_ref(meta$reference_b)) {
    citations <- c(citations, meta$reference_b)
  }
  
  if (length(citations) == 0) {
    cli::cli_alert_info("No citation information available for this dataset.")
    return(invisible(NULL))
  }
  
  
  cli::cli_text("To cite this dataset in publications, please use:")
  cli::cli_text("")  # empty line
  
  # Print each citation as code blocks for better formatting
  for (i in seq_along(citations)) {
    if (i > 1) cli::cli_text("") 
    cli::cli_code(citations[i])
  }
  
  # empty line
  cli::cli_text("") 
  
  full_citation_string <- paste(citations, collapse = "\n\n")
  return(invisible(full_citation_string))
}


#' Get additional notes for dataset
#' 
#' @param x An object of class \code{openesm_dataset}.
#' @param ... Additional arguments (currently unused).
#' @return Character vector with dataset notes, returned invisibly.
#' @examples
#' \donttest{
#' dataset <- structure(
#'   list(
#'     dataset_id = "0001",
#'     metadata = list(
#'       additional_comments = "Note about timing; Note about exclusions"
#'     )
#'   ),
#'   class = "openesm_dataset"
#' )
#' notes(dataset)
#' }
#' @importFrom cli cli_h1 cli_bullets cli_alert_info
#' @export
notes.openesm_dataset <- function(x, ...) {
  meta <- x$metadata
  
  # extract the notes string from the metadata tibble
  notes_string <- meta$additional_comments
  
  # check if the notes string is missing, NA, or empty
  if (is.null(notes_string) || is.na(notes_string) || !nzchar(trimws(notes_string))) {
    cli::cli_alert_info("No additional notes available for this dataset.")
    return(invisible(character(0)))
  }
  
  # split the string by semicolons and trim whitespace from each note
  notes_vector <- trimws(strsplit(notes_string, ";")[[1]])
  
  # filter out any empty strings that might result from trailing semicolons
  notes_vector <- notes_vector[nzchar(notes_vector)]
  
  if (length(notes_vector) == 0) {
      cli::cli_alert_info("No additional notes available for this dataset.")
      return(invisible(character(0)))
  }

  # print the notes using a cli header and bullet points
  cli::cli_h1("Notes for dataset {.val {x$dataset_id}}")
  cli::cli_bullets(c("*" = notes_vector))
  
  # return the vector of notes invisibly
  return(invisible(notes_vector))
}
