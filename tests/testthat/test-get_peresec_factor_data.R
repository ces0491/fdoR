test_that("retrieving factor data from Peresec", {

  reqd_file <- c("long_only_alsi", "long_short_constrained")
  reqd_factor <- c("ff3", "ff5")
  weight <- "market_cap"

  test <- get_peresec_erf_data(reqd_file, reqd_factor, weight)

  # these data are dynamic so we can not reliably test a fixed point set of results

  testthat::expect_s3_class(test, "tbl_df")
  testthat::expect_setequal(names(test), c("name", "factor", "raw_data", "clean_data"))
  testthat::expect_type(test$raw_data, "list")
  testthat::expect_type(test$clean_data, "list")
  testthat::expect_setequal(unique(test$name), reqd_file)
  testthat::expect_setequal(unique(test$factor), c('Fama-French 3F', 'Fama-French 5F'))
})
