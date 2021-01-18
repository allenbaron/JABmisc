#' Round a number up
#'
#' @details
#' Rounding to a negative number of digits means rounding to a power of ten, so
#' for example round_up(x, digits = -2) rounds to the nearest hundred.
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places. Negative
#' values are allowed (see 'Details').
#'
#' @export
round_up <- function(x, digits = -1) {
    # Self-explanatory; use = specify the upper limit for an axis providing some buffer
    #
    # digits indicates how many 0's (1 = 10, 2 = 100),
    #   negative numbers accepted (-1 = 0.1)

    if (digits == 0) {
        return(ceiling(x))
    }

    place <- 10^(-digits)
    x + place - x %% place
}

#' Print comma-delimited large numbers
#'
#' @param input a numeric scalar
#'
#' @export
format_num <- function(input) {
    format(input, big.mark = ",", trim = TRUE)
}


# DOES NOT WORK AS WRITTEN!
#centered_breaks <- function(range, center = 0, by = 1) {
#    stopifnot(length(range) == 2)
#    if (center <= range[1] || center >= range[2]) {
#        stop("center must be within range")
#    }
#
#    c(
#        rev(seq(from = center, to = range[1], by = -by)),
#        seq(from = center + by, to = range[2], by = by)
#    )
#}









