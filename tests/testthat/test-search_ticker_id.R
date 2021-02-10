test_that("correct ticker names are returned", {

  tickers <- c("Massmart", "NTCJ", "ZAE000006896")
  test_ticker_id <- get_ticker_names(tickers)

  expected <- tibble::tibble(ticker = c("MSMJ", "NTCJ", "SOLJ"),
                             name = c("Massmart Holdings Ltd", "Netcare", "Sasol Ltd"),
                             isin = c("ZAE000152617", "ZAE000011953", "ZAE000006896"))

  testthat::expect_equal(test_ticker_id, expected)
})
