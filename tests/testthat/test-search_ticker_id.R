test_that("correct ticker names are returned", {

  tickers <- c("ZAE000152617", "ZAE000170049")
  test_ticker_id <- get_ticker_names(tickers)

  expected <- data.frame(ticker = c(tickers), full_name = c('Massmart Holdings Ltd', 'PPC Limited'))

  testthat::expect_equal(test_ticker_id, expected)
})
