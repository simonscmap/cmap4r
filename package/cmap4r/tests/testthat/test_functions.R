test_that("Validation of arguments is done correctly.", {

  ## Two simple tests, ONLY of the input types.
  expect_true(validate_sp_args("a","b", "c", "d", 1,2,3,4,5,6))
  expect_error(validate_sp_args("a","b", "c", "d", 1,2,3,4,5,"ab"))

})
