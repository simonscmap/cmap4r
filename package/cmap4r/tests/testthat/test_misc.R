context("Test correctness of miscellaneous functions.")

## Test for lasso and cvx lasso
test_that("Time check is correct.", {

  ## Three types of dates
  dates = list(
      "2017-06-08",
      "2017-06-07T06:21:03",
      "2017-06-07 22:43:49 UTC")

  expect_equal(sapply(dates, only_has_date),
               c(TRUE, FALSE, FALSE))
})
