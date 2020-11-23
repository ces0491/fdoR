test_that("getting market risk premia data works as expected", {

  cntry <- c("ZA", "GB")
  var <- c("Implied cost of capital (ICOC) %")
  p <-  c("t0")
  mrp_test <- get_mrp_data(country_iso = cntry, variable = var, period = p)
})
