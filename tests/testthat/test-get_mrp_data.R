test_that("getting market risk premia data works as expected", {

  cntry <- c("ZA", "GB")
  var <- c("Implied cost of capital (ICOC) %")
  p <-  c("t0")

  mrp_test <- get_risk_premia_data(country_iso = cntry, variable = var, period = p)

  # this data is dynamic. t0 will differ with the passage of time so we cannot test for an equal valued data.frame
  expected_cols <- c("country_iso", "date", "variable", "period", "value")
  expected_rown <- 2

  testthat::expect_equal(nrow(mrp_test), 2)
  testthat::expect_equal(names(mrp_test), expected_cols)
})
