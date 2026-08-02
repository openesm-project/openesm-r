#' Citation information for openESM datasets
#' @param x An object of class \code{openesm_dataset}.
#' @param ... Additional arguments passed to methods
#' @return Citation information for the dataset(s)
#' @examples
#' \donttest{
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
#' @export
cite <- function(x, ...) {
  UseMethod("cite")
}

#' Additional notes for openESM datasets
#' @param x An object of class \code{openesm_dataset}.
#' @param ... Additional arguments passed to methods
#' @return Additional notes and information about the dataset(s)
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
#' @export
notes <- function(x, ...) {
  UseMethod("notes")
}
