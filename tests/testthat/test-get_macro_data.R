test_that("retrieving data from Quandl or Alphavantage works as expected", {

  price_tickers <- c("Q-WFE/INDEXES_JOHANNESBURGSEFTSEJSEALLSHARE", "Q-FRED/GDP", "Q-ML/AAAEY", "Q-LBMA/GOLD", "AVST-NTC.JO", "AVFX-ZAR/USD")
  start_date <- as.Date("2013-09-27")
  end_date <- as.Date("2018-09-27")
  frequency <- "weekly"

  test_price_data <- get_macro_data(price_tickers, start_date, end_date, frequency)

})
