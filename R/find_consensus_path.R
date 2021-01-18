#' Identify shared path of subdirectories
#'
#' Identify the recursive path (subdirectories) which most files have in common
#' within a specified directory.
#'
#' @section Example Usage:
#' Identify the consensus path of an `renv` project library when other
#' unrelated and less file-dense directories are present in a given top-level
#' directory. (I used this on a HPCC to determine where R files were being
#' installed to when setting up R with renv and conda.)
#'
#' @section NOTE:
#' This is a first-pass implementation and may be slow. The implementation
#' identifies all files recursively below the specified directory and returns
#' the file path shared by the majority of files (currently 80%).
#'
#' @return a file path, beginning with `top_lvl_dir`
#'
#' @param top_lvl_dir the path to the directory to search within
#'
#' @export
find_consensus_path <- function(top_lvl_dir) {

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
