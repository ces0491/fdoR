test_that("retrieving data from Damodaran's site works as expected", {

  test_damodaran <- get_damodaran_data(reqd_file = c("waccGlobal", "waccemerg"), reqd_year = 2019)
})
