# QUESTIONING

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

    if (!tibble::is_tibble(x)) stop("x must be a tibble")
    y <- as.matrix.data.frame(x[,-1])
    rownames(y) <- x[[1]]
    y
}
