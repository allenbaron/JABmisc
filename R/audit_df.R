#' Audit a data frame
#'
#' Initial data frame inspection. Lists columns that are invariant
#' (single-value) and identical, along with a data frame
#' describing the values by column (column type, missing, and unique values).
#'
#' @section NOTE:
#' Data.frames print as individual list in rmarkdown when knitted.
#'
#' @return List of `length(3)` showing: invariant columns, identical columns,
#' and column descriptions.
#'
#' @inheritParams describe_cols
#'
#' @seealso [describe_cols()]
#' @export
audit_df <- function(df, ...) {

    list(
        invariant_cols = identify_invariant(df),
        identical_cols = identify_identical(df),
        description = describe_cols(df, ...)
    )
}


#' Describe the columns in a data frame
#'
#' For each column in a data.frame, `describe_cols()` shows the column type,
#' number and percentage of missing values, and number of unique values
#' (optionally with the values themselves--see [map_unique()] for details).
#'
#' @inheritParams map_unique
#' @param ... additional arguments passed on to [map_unique()]
#'
#' @export
describe_cols <- function(df, ...) {

    x <- list(
        map_type(df),
        map_missing(df),
        map_unique(df, ...)
    )

    Reduce(
        function(x, y) dplyr::full_join(x, y, by = "var"),
        x
    )
}


#' Returns value for columns with only one value
identify_invariant <- function(df) {

    dplyr::select_if(df, ~dplyr::n_distinct(.) == 1)[1, ]
}


#' Returns columns in a data.frame that are identical (strict)
identify_identical <- function(df) {

    cols <- names(df)
    primary <- cols[1]
    compare <- cols[-1]
    out <- list()
    i <- 1

    while (length(compare) > 0) {
        idx <- purrr::map_lgl(compare, ~identical(df[[primary]], df[[.x]]))

        if (sum(idx) == 0) {
            primary <- compare[1]
            compare <- compare[-1]
        } else {
            out[[i]] <- tibble::tibble(var = primary, identical = compare[idx])
            primary <- compare[!idx][1]
            compare <- compare[!idx][-1]
            i <- i + 1
        }
    }
    dplyr::bind_rows(out)
}


#' Show column types (as tibble)
map_type <- function(df) {

    tibble::tibble(
        var = names(df),
        type = purrr::map_chr(df, vctrs::vec_ptype_abbr)
                # approach w/o vctrs pkg
                #abbreviate(purrr::map_chr(df, typeof), minlength = 3)
    )
}


#' Show missing observations for all columns in a data frame
#'
#' @param df a data.frame
map_missing <- function(df) {

    n <- purrr::map_int(df, ~sum(is.na(.x)))
    percent <- round(n / nrow(df) * 100, 1)
    tibble::tibble(var = names(df), missing_n = n, missing_percent = percent)
}


#' Report unique values for each column of a data.frame
#'
#' For each column in a data.frame, `map_unique()` shows the number of unique
#' values and, optionally, the values themselves.
#'
#' @param df a data.frame
#' @param vals a string describing how to display unique values; one of:
#'
#' * "as_string" (default): for values as a concatenated string
#' (for quick viewing)
#' * "as_list": for values as a column of lists
#' * "both": for concatenated string AND list columns
#' * "none"
#' @param sep a character string to separate the values; used with "as_string"
#' @param ignore_NA logical; whether to include missing (`NA`) values in report;
#' affects both count and values displayed
map_unique <- function(df, vals = "as_string", sep = " | ", ignore_NA = TRUE) {

    if (
        length(vals) > 1 ||
        !(vals %in% c("none", "as_string", "as_list", "both"))
    ) {
        stop("vals must be one of 'as_string', 'as_list', 'both', or 'none'")
    }

    n <- purrr::map_int(df, dplyr::n_distinct, na.rm = ignore_NA)

    if (vals == "none") {
        return(
            tibble::tibble(var = names(df), unique_n = n)
        )
    }

    val_list <- purrr::map(df, unique)

    if (isTRUE(ignore_NA)) {
        val_list <- purrr::map(val_list, ~ .x[!is.na(.x)])
    }

    if (vals == "as_list") {
        return(
            tibble::tibble(
                var = names(df),
                unique_n = n,
                unique_as_list = val_list
            )
        )
    }

    val_strings <- purrr::map_chr(
        val_list,
        function(val_list) {
            if (is.list(val_list[1])) {
                len <- range(purrr::map_int(val_list, length), na.rm = TRUE)
                len_print <- dplyr::if_else(
                    len[1] == len[2],
                    as.character(len[1]),
                    paste0(len[1], ":", len[2])
                )
                paste0(typeof(val_list), "[", len_print, "]")
            } else {
                paste0(val_list, collapse = sep)
            }
        }
    )

    if (vals == "as_string") {
        return(
            tibble::tibble(
                var = names(df), unique_n = n, unique_as_string = val_strings
            )
        )
    }

    tibble::tibble(
        var = names(df),
        unique_n = n,
        unique_as_string = val_strings,
        unique_as_list = val_list
    )
}
