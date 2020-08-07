#' import market data from Quandl and or AlphaVantage
#'
#' @param tickers character vector of tickers where Quandl data is prefixed with Q- and AlphaVantage data is prefixed with AVST- for stocks and AVFX- for FX
#' @param start_date start date
#' @param end_date end date
#' @param periodicity string indicating the periodicity of data to retrieve - one of daily, weekly, monthly, quarterly or annual
#' @param adjusted boolean indicating whether to retrieve adjusted prices where avaialble
#'
#' @return a tbl_df with cols date, ticker, variable and value
#' @export
#'
get_market_data <- function(tickers,
                               start_date,
                               end_date,
                               periodicity = c("daily", "weekly", "monthly", "quarterly", "annual"),
                               adjusted = TRUE) {

  assertR::assert_true(all(grepl("-", tickers)), "check your tickers, it appears that you've not specified a prefix to indicate the data source")

  assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), periodicity, "check that you specified the periodicity correctly")

  # split ticker vector between alphavantage and quandl tickers
  unq_tickers <- unique(tickers)
  ticker_df <- data.frame(ticker = unq_tickers)
  ticker_tbl <- ticker_df %>%
    tidyr::separate(ticker, c("d_source", "ticker"), sep = "-", remove = TRUE)
  av_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source %in% c("AVST", "AVFX")) %$%
    ticker
  quandl_tkrs <- ticker_tbl %>%
    dplyr::filter(d_source == "Q") %$%
    ticker

  # retrieve data from alphavantage and/or quandl
  if (length(av_tkrs) > 0) {
    message("retrieving AlphaVantage data")
    av_data <- get_av_data(av_tkrs, start_date, end_date, periodicity)
    assertR::assert_present(names(av_data), c("ticker", "date", "variable", "value"))
  } else {
    av_data <- NULL
  }

  if (length(quandl_tkrs) > 0) {
    message("retrieving Quandl data")
    quandl_data <- get_quandl_data(quandl_tkrs, start_date, end_date, periodicity)
    assertR::assert_present(names(quandl_data), c("ticker", "date", "variable", "value"))
  } else {
    quandl_data <- NULL
  }

  # combine alphavantage and quandl data
  all_data <- av_data %>%
    dplyr::bind_rows(quandl_data)

  # order and arrange final output
  reqd_data <- all_data %>%
    # dplyr::group_by(variable) %>%
    # change_periodicity(., periodicity) %>%
    # dplyr::ungroup() %>%
    dplyr::select(date, ticker, variable, value) %>%
    dplyr::arrange(ticker, date, variable) %>%
    dplyr::distinct()

  # alert user if any duplicates were found
  dupes <- tickers[which(duplicated(tickers))]
  dupes_str <- paste(dupes, collapse = "','")
  dupes_str <- paste("'", dupes_str, "'", sep = "")
  if (length(dupes) > 0) {
    warning(glue::glue("The following duplicate values were found in argument 'tickers' and were removed: {dupes_str}"))
  }


  # return required data
  reqd_data
}
