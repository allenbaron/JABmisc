find_consensus_path <- function(top_lvl_dir) {
    # Identify recursive path of subdirectories all files have in common
    #   starting at top_lvl_dir
    # NOTE: Uses only base R functions for flexibility;
    #   raw implementation = not fast
    dir_list <- list.dirs(top_lvl_dir)
    dir_split <- strsplit(dir_list, "/", fixed = TRUE)
    max_length <- max(vapply(dir_split, function(x) {length(x)}, integer(1)))
    dir_split_std <- as.data.frame(
        t(vapply(
            dir_split,
            function(x) {
                l <- length(x)
                diff_l <- max_length - l
                c(x, rep(NA_character_, times = diff_l))
            },
            character(max_length)
        )),
        stringsAsFactors = FALSE
    )
    dir_count <- lapply(dir_split_std, function(x) table(x, useNA = "no"))
    max_dir_count <- vapply(dir_count, function(x) {max(x)}, integer(1))
    max_dir <- vapply(dir_count, function(x) {
        ifelse(sum(x == max(x)) > 1, NA_character_, names(x)[x == max(x)])
    },
    character(1))
    max_max <- max(max_dir_count)
    last_dir <- max(which(max_dir_count > (max_max * 0.8)))
    paste0(max_dir[1:last_dir], collapse = "/")
}
