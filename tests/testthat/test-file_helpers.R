
# R objects ---------------------------------------------------------------

dir_exist <- c("test_dir1", "test_dir2")
file_exist <- c("test_file1.csv", "test_dir1/test_file2.txt")

dir_nonexist <- c("/absolute/path/", "rel/path/")
file_nonexist <- c("/absolute/path/wrong.txt", "rel/path/wrong.txt")

dir_file_exist <- c(dir_exist, file_exist)
dir_file_nonexist <- c(dir_nonexist, file_nonexist)
dir_file_all <- c(dir_file_exist, dir_file_nonexist)


# Tests -------------------------------------------------------------------

test_that("EXISTENT ONLY: output correct", {
    expect_identical(
        file_dir_exist(file_exist),
        list(
            file = file_exist,
            dir = character(0)
        )
    )

    expect_identical(
        file_dir_exist(dir_exist),
        list(
            file = character(0),
            dir = dir_exist
        )
    )

    expect_identical(
        file_dir_exist(dir_file_exist),
        list(
            file = file_exist,
            dir = dir_exist
        )
    )
})


test_that("NONEXISTENT + CAPTURE: output correct", {
    expect_identical(
        file_dir_exist(dir_file_nonexist),
        list(
            file = character(0),
            dir = character(0),
            nonexistent = dir_file_nonexist
        )
    )

    expect_identical(
        file_dir_exist(dir_file_all),
        list(
            file = file_exist,
            dir = dir_exist,
            nonexistent = dir_file_nonexist
        )
    )
})


test_that("NONEXISTENT + WARN: output correct", {
    expect_identical(
        suppressWarnings(
            file_dir_exist(dir_file_nonexist, if_nonexistent = "warn")
        ),
        list(
            file = character(0),
            dir = character(0)
        )
    )

    expect_warning(
        file_dir_exist(dir_file_nonexist, if_nonexistent = "warn")
    )

    expect_identical(
        suppressWarnings(
            file_dir_exist(dir_file_all, if_nonexistent = "warn")
        ),
        list(
            file = file_exist,
            dir = dir_exist
        )
    )

    expect_warning(
        file_dir_exist(dir_file_all, if_nonexistent = "warn")
    )
})


test_that("NONEXISTENT + ERROR: output correct", {
    expect_error(
        file_dir_exist(dir_file_nonexist, if_nonexistent = "error")
    )

    expect_error(
        file_dir_exist(dir_file_all, if_nonexistent = "error")
    )
})
