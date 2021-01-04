test_that("retrieving data from Damodaran's site works as expected", {

  test_damodaran <- get_damodaran_data(reqd_file = c("EVAGlobal", "waccemerg"), reqd_year = 2018)

  test_file <- "expected_damodaran_data.rds"
  src_dir <- system.file("testdata", package = "fdoR")
  src_file <- paste(src_dir, test_file, sep = "/")

  expected_damodaran <- readRDS(src_file)

  testthat::expect_equal(test_damodaran, expected_damodaran)
})
