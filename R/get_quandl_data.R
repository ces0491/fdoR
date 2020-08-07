#' get data from Quandl
#'
#' @param tickers character vector of tickers prefixed with Q-
#' @param start_date start date
#' @param end_date end date
#' @param periodicity string indicating the periodicity - daily, weekly, monthly, quarterly, annual
#'
#' @return a tbl_df with cols ticker, date, variable and value
#'
get_quandl_data <- function(tickers, start_date, end_date, periodicity) {

  ts_data_list <- list()

  for(tkr in tickers) {

    t <- which(tickers == tkr)
    progress <- round(t/length(tickers), 2) * 100
    print(glue::glue("attempting to retrieve {tkr} data from Quandl"))

    ts_data <- try(
      Quandl::Quandl(code = tkr,
                     start_date = start_date,
                     end_date = end_date,
                     collapse = "daily",
                     type = "raw"),
      silent = FALSE)

    if(any(class(ts_data) == "try-error")) {
      ts_data <- NULL
    }

    if (colnames(ts_data)[1] == "Date" | colnames(ts_data)[1] == "DATE") {
      colnames(ts_data)[1] <- "date"
    }

    if (ncol(ts_data) >= 4) {
      ts_data <- ts_data %>%
        dplyr::select(date, dplyr::starts_with('USD')) %>%
        dplyr::select(date, dplyr::contains('PM'))
    }

    if (ncol(ts_data) == 2) {
      colnames(ts_data)[2] <- "Value" # note capital V
    }

    print(glue::glue("{progress}% complete"))

    ts_period <- change_periodicity(ts_data, periodicity)

    ts_data_list[[tkr]] <- ts_period
  }

  if(is.null(unlist(ts_data_list))) {
    ts_data_df <- NULL
    warning("check whether you've specified your quandl api key or if you've used the correct tickers")
  } else {
    ts_data_df <- tibble::enframe(ts_data_list, name = "ticker") %>%
      tidyr::unnest() %>%
      dplyr::rename(close = Value) %>%
      dplyr::arrange(ticker, date) %>%
      tidyr::gather(variable, value, -date, -ticker) %>%
      dplyr::group_by(ticker, variable) %>%
      tidyr::fill(value) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = ifelse(variable == 'USD.PM'| variable == 'USD..PM.', 'close', variable)) %>%
      tidyr::drop_na()

  }

  unav_tkrs <- setdiff(tickers, names(ts_data_list))
  unav_tickers <- paste(unav_tkrs, collapse = "','")
  unav_tickers <- paste("'", unav_tickers, "'", sep = "")

  if (length(unav_tkrs) > 0) {
    warning(glue::glue("The following tickers were requested but not available from Quandl: {unav_tickers}"))
  }

  ts_data_df

}