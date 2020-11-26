test_that("retrieving factor data from Peresec", {
  reqd_file <- "long_only_alsi"
  reqd_factor <- "ff3"
  weight <- "market_cap"

  test <- get_legae_factor_data(reqd_file, reqd_factor, weight)

})
