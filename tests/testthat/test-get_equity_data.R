test_that("get equity data works as expected", {

  tickers <- c("YAH-DGH.JO", "INV-CLSJ")

  type <- c("price", "IS", "BS", "CFS")
  start_date <- "2020/07/01"
  end_date <- "2020/10/31"
  frequency <- "monthly"

  test <- get_equity_data(tickers, type, start_date, end_date, frequency)

  test_file <- "expected_equity_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected <- readRDS(src_file)

  testthat::expect_equal(test, expected)

})
