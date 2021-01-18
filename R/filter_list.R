#' Filter list by name/value
#'
#' Filters list (at top-level only) by name or value.
#'
#' @section NOTE:
#' `filter_list()` is useful for returning matches of a name or value.
#' Currently, it will only search the top-level in a list and matches must be
#' exact. Unlike [purrr::pluck()], `filter_list()` will return all matches (not
#' just the first match).
#'
#' @param list a list
#' @param name a name to search for within a list; can be anything that can
#' appear as a name in a list
#' @param value a value to search for within a list
#'
#' @examples
#' l <- list(this = 1, that = 2, that = 3, final = 2)
#'
#' purrr::pluck(l, "that") # returns only first match
#' filter_list(l, "that") # returns all matches
#'
#' filter_list(l, value = 2)
#'
#' @export
filter_list <- function(list, name, value) {

    if(all(missing(name), missing(value))) {
        stop("'name' or 'value' must be specified")
    }

    if(isFALSE(missing(name))) {
        return(list[names(list) == name])
    }

    if(isFALSE(missing(value))) {
        return(list[list == value])
    }
}
