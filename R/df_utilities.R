audit_df <- function(df, ...) {
    # doesn't print well when knitted!

    list(
        invariant_vars = identify_invariant(df),
        identical_vars = identify_identical(df),
        details = map_values(df, ...)
    )
}


map_values <- function(df, ...) {

    x <- list(
        map_type(df),
        map_missing(df),
        map_unique(df, ...)
    )

    Reduce(
        function(x, y) full_join(x, y, by = "var"),
        x
    )
}


identify_invariant <- function(df) {
    # Returns value for variables with only one value

    dplyr::select_if(df, ~n_distinct(.) == 1)[1, ]
}


identify_identical <- function(df) {
    # Returns variables in a data.frame that are identical (strict)

    vars <- names(df)
    primary <- vars[1]
    compare <- vars[-1]
    out <- list()
    i <- 1

    while (length(compare) > 0) {
        idx <- map_lgl(compare, ~identical(df[[primary]], df[[.x]]))

        if (sum(idx) == 0) {
            primary <- compare[1]
            compare <- compare[-1]
        } else {
            out[[i]] <- tibble(var = primary, identical = compare[idx])
            primary <- compare[!idx][1]
            compare <- compare[!idx][-1]
            i <- i + 1
        }
    }
    bind_rows(out)
}


map_type <- function(df) {
    # Returns class of variables in a dataframe

    tibble(
        var = names(df),
        type = map_chr(df, vctrs::vec_ptype_abbr)
                # approach w/o vctrs pkg
                #abbreviate(map_chr(df, typeof), minlength = 3)
    )
}


map_missing <- function(df) {
    # Count missing observations for all variables in a df

    n <- map_int(df, ~sum(is.na(.x)))
    percent <- round(n / nrow(df) * 100, 1)
    tibble(var = names(df), missing_n = n, missing_percent = percent)
}


map_unique <- function(df, vals = "as_string", sep = " | ", ignore_NA = TRUE) {
    # Returns count & list (in nested df) of unique values for
    #   all variables in a df
    #   vals = one of "none", "as_string", "as_list", or "both"; where
    #       "as_string" returns a column with unique values pasted together,
    #       "as_list" returns a column with unique values as a nested list,
    #       "both" returns both of these columns
    #   sep = separator to use in paste

    if (
        length(vals) > 1 ||
        !(vals %in% c("none", "as_string", "as_list", "both"))
    ) {
        stop("vals must be one of 'none', 'as_string', 'as_list', or 'both'")
    }

    n <- purrr::map_int(df, n_distinct, na.rm = ignore_NA)

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
                len <- range(map_int(val_list, length), na.rm = TRUE)
                len_print <- if_else(
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
        var = names(df), unique_n = n, unique_as_string = val_strings, unique_as_list = val_list
    )
}
