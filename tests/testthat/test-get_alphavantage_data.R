test_that("retrieve data from Alphavantage", {

  tickers <- c("TSCO", "ZAR/USD")
  start_date <- as.Date("2020-09-30")
  end_date <- as.Date("2020-10-31")
  frequency <- "weekly"

  test <- get_alphavantage_data(tickers, start_date, end_date, frequency)

  test_file <- "expected_av_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected <- readRDS(src_file)

  testthat::expect_equal(test, expected)

})
