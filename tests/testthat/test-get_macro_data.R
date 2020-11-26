test_that("retrieving data from Quandl or Alphavantage works as expected", {

  tickers <- c("Q-FRED/GDP", "Q-ML/AAAEY", "AVFX-ZAR/EUR")
  start_date <- as.Date("2020-08-31")
  end_date <- as.Date("2020-10-31")
  frequency <- "monthly"

  test_price_data <- get_macro_data(price_tickers, start_date, end_date, frequency)

  test_file <- "expected_macro_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected <- readRDS(src_file)

})
