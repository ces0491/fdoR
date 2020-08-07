# TODO add yahoo fin data here too

#' import financial statement data from the SEC and or Morningstar
#'
#' @param tickers character vector of tickers where SEC data is prefixed with SEC- and Morningstar data is prefixed with MS-
#' @param financial_statement string specifying the type of financial statement required - one of IS, BS or CFS
#'
#' @return a df
#'
get_company_data_single <- function(tickers, financial_statement = c("IS", "BS", "CFS")) {

  assertR::assert_present(c("IS", "BS", "CFS"), financial_statement, "check that you specified the financial statement type correctly")

  # split ticker vector between SEC and Morningstar tickers
  unq_tickers <- unique(tickers)
  ticker_df <- data.frame(ticker = unq_tickers)
  ticker_tbl <- ticker_df %>%
    tidyr::separate(ticker, c("d_source", "ticker"), sep = "-", remove = TRUE)
  sec_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source == "SEC") %$%
    ticker
  mstar_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source == "MS") %$%
    ticker

  # retrieve data from alphavantage and/or quandl
  if (length(sec_tkrs) > 0) {
    message("retrieving financial statements from the SEC")
    sec_data <- get_sec_fs(sec_tkrs, financial_statement)
    assertR::assert_present(names(sec_data), c("date", "ticker", "statement", "fs_data"))
  } else {
    sec_data <- NULL
  }

  if (length(mstar_tkrs) > 0) {
    message("retrieving financial statements from Morningstar")
    mstar_data <- get_mstar_fs(mstar_tkrs, financial_statement)
    assertR::assert_present(names(mstar_data), c("date", "ticker", "statement", "fs_data"))
  } else {
    mstar_data <- NULL
  }

  # combine sec and morningstar data
  all_fs_data <- sec_data %>%
    dplyr::bind_rows(mstar_data)

  dupes <- tickers[which(duplicated(tickers))]
  dupes_str <- paste(dupes, collapse = "','")
  dupes_str <- paste("'", dupes_str, "'", sep = "")
  if (length(dupes) > 0) {
    warning(glue::glue("The following duplicate values were found in argument 'tickers' and were removed: {dupes_str}"))
  }

  all_fs_data
}

#' get financial statement data from Morningstar or the SEC
#'
#' @param tickers character vector of tickers - preferably isins for morningstar data, with the relevent prefix for SEC or MS data
#' @param financial_statements character vector indicating which financial statements to retrieve
#'
#' @return tibble with cols, date, ticker, financial statement and nested fs data
#' @export
#'
get_company_data <- function(tickers, financial_statements = c("IS", "BS", "CFS")) {

  all_fs_data <- tibble::tibble(date = as.Date(character()), ticker = character(), statement = character(), fs_data = list())

  for(fs in financial_statements) {
    print(glue::glue("retrieving {fs} data..."))
    single_fs_data <- get_company_data_single(tickers, financial_statement = fs)
    all_fs_data <- dplyr::bind_rows(all_fs_data, single_fs_data)
  }

  all_fs_data
}
