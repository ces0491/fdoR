#' private function to retrieve daily data from alphavantage
#'
#' @param tickers a character vector of alphavantage tickers
#' @param out_size string indicating the output size - either full or compact
#' @param frequency string indicating the frequency - daily, weekly, monthly, quarterly, annual
#'
#' @return a list of dataframes
#'
pvt_retrieve_av_data <- function(tickers, out_size, frequency) {

  ts_data_list <- list()

  for(tkr in tickers) {

    fx_tkr <- grep("/", tkr, value = TRUE) # use the fwd slash in the fx ticker to distinguish fx tickers from others in AV
    base_tkr <- unlist(strsplit(fx_tkr, split = "/"))[2]
    quote_tkr <- unlist(strsplit(fx_tkr, split = "/"))[1]

    stock_tkr <- setdiff(tkr, fx_tkr)

    t <- which(tickers == tkr)
    progress <- round(t/length(tickers), 2) * 100
    print(glue::glue("attempting to retrieve {tkr} data from AlphaVantage"))

    if (identical(fx_tkr, character(0))) {
      ts_data <- try(
        alphavantager::av_get(stock_tkr, av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = out_size),
        silent = FALSE)

    } else {

      ts_data <- try(
        alphavantager::av_get("X", av_fun = "FX_DAILY", from_symbol = base_tkr, to_symbol = quote_tkr, outputsize = out_size),
        silent = FALSE)
    }

    if(any(class(ts_data) == "try-error")) {
      ts_data <- NULL
    }

    print(glue::glue("{progress}% complete"))

    ts_period <- ts_data %>%
      dplyr::rename("date" = "timestamp") %>%
      dateR::to_period(., frequency)

    ts_data_list[[tkr]] <- ts_period
  }

  ts_data_list
}

#' get data from AlphaVantage
#'
#' @param tickers character vector of tickers prefixed with AV-
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency - daily, weekly, monthly, quarterly, annual
#'
#' @return a tbl_df with cols ticker, date, variable and value
#'
get_alphavantage_data <- function(tickers, start_date, end_date, frequency) {

  alphavantager::av_api_key(Sys.getenv("av_api_key"))

  if (end_date - start_date <= 100) {
    out_size <- "compact"

  } else {
    out_size <- "full"
  }

  # use private data retrieval function to access alphavantage api and return list of dataframes containing ticker data
  ts_data_list <- pvt_retrieve_av_data(tickers, out_size, frequency)

  if (is.null(unlist(ts_data_list))) {
    ts_data_df <- NULL
    warning("If you're expecting data from alphavantage, check you've used the correct ticker prefix and ticker spelling")
  } else {
    ts_data_df <- tibble::enframe(ts_data_list, name = "ticker") %>%
      tidyr::unnest(cols = c(value)) %>%
      dplyr::arrange(ticker, date) %>%
      tidyr::gather(variable, value, -date, -ticker) %>%
      dplyr::group_by(ticker, variable) %>%
      tidyr::fill(value) %>%
      dplyr::ungroup() %>%
      dplyr::filter(date >= start_date,
                    date <= end_date)
  }

  unav_tkrs <- setdiff(tickers, names(ts_data_list))
  unav_tickers <- paste(unav_tkrs, collapse = "','")
  unav_tickers <- paste("'", unav_tickers, "'", sep = "")

  if (length(unav_tkrs) > 0) {
    warning(glue::glue("The following tickers were requested but not available from AlphaVantage: {unav_tickers}"))
  }

  ts_data_df
}
