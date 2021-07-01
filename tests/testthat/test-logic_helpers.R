vctr_dup_test <- c(1:4, 1:2)
df_dup_test <- data.frame(
    x = vctr_dup_test,
    y = c(1:4, 1, 3)
)

vctr_dup_truth <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
df_dup_truth <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)

test_that("all duplicates identified correctly", {
    expect_identical(all_duplicated(vctr_dup_test), vctr_dup_truth)
    expect_identical(all_duplicated(df_dup_test), df_dup_truth)
})
