test_that("Check number of rows for 2013 accidents file", {
  testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
})
