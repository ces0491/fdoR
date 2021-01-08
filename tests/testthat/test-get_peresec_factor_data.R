test_that("retrieving factor data from Peresec", {

  reqd_file <- c("long_only_alsi", "long_short_constrained")
  reqd_factor <- c("ff3", "ff5")
  weight <- "market_cap"

  test <- get_peresec_erf_data(reqd_file, reqd_factor, weight)

  test_file <- "expected_peresec_factor_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected_damodaran <- readRDS(src_file)

  testthat::expect_equal(test, expected)
})
