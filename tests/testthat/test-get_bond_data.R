testthat::test_that("get data for vector of countries and metrics from wgb.com works as expected", {

  test_sovfi_data <- get_bond_data(country_iso = c("US", "ZA"),
                                   metric = c("yield_5", "cds_5"),
                                   start_date = "2020-08-20",
                                   end_date = "2020-08-25",
                                  cfrequency = "daily",
                                   include_forecast = FALSE)

  expected_sovfi_data <- tibble::tibble(country_iso = c(rep("US", 12), rep("ZA", 10)),
                                        date = c(rep(seq(as.Date("2020-08-20"), as.Date("2020-08-25"), by = "day"), 2),
                                              as.Date(c("2020-08-20", "2020-08-21", "2020-08-24", "2020-08-25")),
                                              seq(as.Date("2020-08-20"), as.Date("2020-08-25"), by = "day")),
                                        metric = c(rep("cds_5", 6), rep("yield_5", 6), rep("cds_5", 4), rep("yield_5", 6)),
                                        value = c(18.5, 18.5, 18.5, 18.5, 18.5, 18.5,
                                                 0.272, 0.268, 0.268, 0.268, 0.282, 0.296,
                                                 295.03, 292.96, 283.16, 287.35,
                                                 7.435, 7.37, 7.37, 7.37, 7.4, 7.385))

  testthat::expect_equal(test_sovfi_data, expected_sovfi_data)

})
