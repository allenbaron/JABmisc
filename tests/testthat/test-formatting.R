test_that("round_up() works", {
    expect_equal(round_up(123.456, 0), 124)
    expect_equal(round_up(123.456, 1), 123.5)
    expect_equal(round_up(123.456, -1), 130)
    expect_equal(round_up(123, 0), 123)
})
