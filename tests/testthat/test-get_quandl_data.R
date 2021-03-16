test_that("retrieve data from Quandl", {

  tickers <- c("LBMA/GOLD", "ML/AAAEY")
  start_date <- as.Date("2020-09-30")
  end_date <- as.Date("2020-10-31")
  frequency <- "weekly"

  test <- get_quandl_data(tickers, start_date, end_date, frequency)

  expected <- tibble::tibble(ticker = c(rep("LBMA/GOLD", 4), rep("ML/AAAEY", 4)),
                             date = as.Date(rep(c("2020-10-06", "2020-10-13", "2020-10-20", "2020-10-27"), 2)),
                             variable = rep("close", 8),
                             value = c(1913.4, 1891.3, 1898.4, 1905.7, 1.61, 1.58, 1.65, 1.61))

  testthat::expect_equal(test, expected)

})
