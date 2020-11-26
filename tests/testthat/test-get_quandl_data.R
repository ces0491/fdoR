test_that("retrieve data from Quandl", {

  tickers <- c("WFE/INDEXES_JOHANNESBURGSEFTSEJSEALLSHARE", "FRED/GDP", "ML/AAAEY", "LBMA/GOLD")
  start_date <- as.Date("2020-09-30")
  end_date <- as.Date("2020-10-31")
  frequency <- "weekly"

  test <- get_quandl_data(tickers, start_date, end_date, frequency)

  test_file <- "expected_quandl_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected <- readRDS(src_file)

  testthat::expect_equal(test, expected)

})
