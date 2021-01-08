test_that("retrieve data from Quandl", {

  tickers <- c("WFE/INDEXES_JOHANNESBURGSEFTSEJSEALLSHARE", "FRED/GDP", "ML/AAAEY", "LBMA/GOLD")
  start_date <- as.Date("2020-09-30")
  end_date <- as.Date("2020-10-31")
  frequency <- "weekly"

  test <- get_quandl_data(tickers, start_date, end_date, frequency)

  expected <- tibble::tibble(ticker = c(rep("LBMA/GOLD", 3), rep("ML/AAAEY", 3)),
                             date = as.Date(c("2020-10-06", "2020-10-13", "2020-10-20", "2020-10-06", "2020-10-13", "2020-10-20")),
                             variable = rep("close", 6),
                             value = c(1913.4, 1891.3, 1898.4, 1.61, 1.58, 1.65))

  testthat::expect_equal(test, expected)

})
