#' Identify all duplicates
#'
#' Identifies all duplicates, see [base::duplicated()] for details.
#'
#' @inheritParams base::duplicated
#'
#' @export
all_duplicated <- function(x, ...) {
    duplicated(x) | duplicated(x, fromLast = TRUE)
}
