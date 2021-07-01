#' Confirms Existence of and Separates Files and Directories
#'
#' Confirms all paths exist and separates files from directories
#'
#' @return A 2- or 3-vector, named list with:
#'     - "file" containing path to files,
#'     - "dir" containing path to directories, and
#'     - "nonexistent" containing paths that do not exist, if
#'     `if_nonexistent = "capture"`; otherwise paths that do not exist are
#'     listed in a warning or error message
#'
#' @inheritParams base::dir.exists
#' @param if_nonexistent how to handle paths which do not exist; one of:
#'     "capture" = capture nonexistent paths in a named vector in the list
#'     "warn" = emit a warning listing paths that do not exist
#'     "error" = emit an error listing paths that do not exist
file_dir_exist <- function(paths, if_nonexistent = "capture") {

    if_nonexistent <- match.arg(if_nonexistent, c("capture", "warn", "error"))

    exists <- file.exists(paths)
    is_dir <- dir.exists(paths)

    file_dir_list <- list(
        file = paths[exists & !is_dir],
        dir = paths[exists & is_dir]
    )

    if (!all(exists)) {
        paths_nonexist <- paths[!exists]
        nonexist_message <- c(
            "One or more paths do not exist:\n",
            paste0("  ", paths_nonexist, "\n")
        )

        if (if_nonexistent == "error") {
            stop(nonexist_message, call. = FALSE)
        }

        if (if_nonexistent == "warn") {
            warning(nonexist_message, call. = FALSE)
            return(file_dir_list)
        }

        if(if_nonexistent == "capture") {
            file_dir_list$nonexistent <- paths[!exists]
            return(file_dir_list)
        }
    }

    file_dir_list
}
