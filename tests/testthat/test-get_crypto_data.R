test_that("get crypto data works as expected", {

  tickers <- c("BTCUSDT", "ETHUSDT")

  start_date <- "2020/07/01"
  end_date <- "2020/10/31"
  frequency <- "monthly"

  test <- get_crypto_data(tickers, start_date, end_date, frequency)

  test_file <- "expected_crypto_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected <- readRDS(src_file)

  testthat::expect_equal(test, expected)

})
