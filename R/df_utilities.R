### For manipulating dataframes/tibbles ###

get_list_item <- function(df, list_col, list_item = 0, row = 1) {
    # Extract an item from a list object stored as an observation in a df
    #   (see also tidyr::hoist or purrr:pluck?)
    #
    #   list_col = character; variable name that contains list object
    #   list_item = integer (maybe character); item at top level of list to return, 0 = return whole list
    #   row = integer;  select a specific row from the df


    if (list_item == 0) {
        df[[row, list_col]]
    } else {
        df[[row, list_col]][[list_item]]
    }
}


as_matrix <- function(x){
    # Convert tibble to matrix
    #
    # From hciR package (github: https://github.com/HuntsmanCancerInstitute/hciR/blob/master/R/as_matrix.R)
    # Author: Chris Stubben

    if(!tibble::is_tibble(x) ) stop("x must be a tibble")
    y <- as.matrix.data.frame(x[,-1])
    rownames(y) <- x[[1]]
    y
}


### For exploring data in dataframes ###

audit_df <- function(df, ...) {
    # doesn't print well when knitted!

    list(
        invariant_vars = show_invariant_vars(df),
        identical_vars = show_identical_vars(df),
        details = enumerate_miss_uniq(df, ...)
    )
}


enumerate_miss_uniq <- function(df, ...) {

    full_join(
        show_missing_by_var(df),
        show_unique_by_var(df, ...),
        by = "var"
    )
}


show_invariant_vars <- function(df) {
    # Returns value for variables with only one value

    dplyr::select_if(df, ~n_distinct(.) == 1)[1, ]
}


show_identical_vars <- function(df) {
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


show_missing_by_var <- function(df) {
    # Count missing observations for all variables in a df

    n <- map_int(df, ~sum(is.na(.x)))
    percent <- round(n / nrow(df) * 100, 1)
    tibble(var = names(df), missing_n = n, missing_percent = percent)
}


show_unique_by_var <- function(df, return_vals = "string", sep = " | ", ignore_NA = TRUE) {
    # Returns count & list (in nested df) of unique values for
    #   all variables in a df
    #   return_vals = one of "none", "string", "list", or "both"; where "string"
    #       returns a column with unique values pasted together and "list"
    #       returns a column with unique values as a nested list
    #   sep = separator to use in paste

    if(
        length(return_vals) > 1 ||
        !(return_vals %in% c("none", "string", "list", "both"))
    ) {
        stop("return_vals must be one of 'none', 'string', 'list', or 'both'")
    }

    n <- purrr::map_int(df, n_distinct, na.rm = ignore_NA)

    if (return_vals == "none") {
        return(
            tibble::tibble(var = names(df), unique_n = n)
        )
    }

    vals <- purrr::map(df, unique)

    if (ignore_NA) {
        vals <- purrr::map(vals, ~ .x[!is.na(.x)])
    }

    if (return_vals == "list") {
        return(
            tibble::tibble(var = names(df), unique_n = n, unique_list = vals)
        )
    }

    val_strings <- purrr::map(vals, paste0, collapse = sep) %>%
        unlist()

    if (return_vals == "string") {
        return(
            tibble::tibble(
                var = names(df), unique_n = n, unique_string = val_strings
            )
        )
    }

    tibble::tibble(
        var = names(df), unique_n = n, unique_string = val_strings, unique_list = vals
    )
}
