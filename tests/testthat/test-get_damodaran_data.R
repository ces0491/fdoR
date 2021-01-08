test_that("retrieving data from Damodaran's site works as expected", {

  reqd_files <- c("EVAGlobal", "waccemerg")
  yr <- 2018

  test_damodaran <- get_damodaran_data(reqd_file = reqd_files, reqd_year = yr)

  test_file <- "expected_damodaran_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected_damodaran <- readRDS(src_file)

  testthat::expect_equal(test_damodaran, expected_damodaran)
})
