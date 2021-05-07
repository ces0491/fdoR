#' Get crypto asset data from Binance
#'
#' @param tickers a character vector of crypto crosses, e.g. c('BTCBUSD', 'ETHUSDT')
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to convert data
#'
#' @return a tbl_df with columns ticker, date, variable and value
#' @export
#'
get_crypto_data <- function(tickers, start_date, end_date, frequency = c("daily", "weekly", "monthly", "quarterly", "annual")) {

  # binancer supports high frequency data, up to minutely, however we limit to daily frequency here
  assertR::assert_present(c("daily", "weekly", "monthly", "quarterly", "annual"), frequency)

  ticker_list <- list()

  for(ticker in tickers) {

    t <- which(tickers == ticker)
    progress <- round(t/length(tickers), 2) * 100
    print(glue::glue("Attempting to retrieve {ticker} data from Binance"))

    crypto_data <- try(
      binancer::binance_klines(symbol = ticker,
                               interval = "1d",
                               start_time = start_date,
                               end_time = end_date),
      silent = FALSE)

    if(any(class(crypto_data) == "try-error")) {

      crypto_data <- NULL

    } else {
      assertR::assert_present(names(crypto_data), c('symbol', 'close_time', 'open', 'high', 'low', 'close', 'volume'))
    }

    ticker_list[[ticker]] <- crypto_data
  }

  # convert list of data.frames to tibble and select required variables
  crypto_data_tbl <- ticker_list %>%
    tibble::enframe() %>%
    tidyr::unnest(value) %>%
    dplyr::select(symbol, close_time, open, high, low, close, volume) %>%
    dplyr::rename(ticker = symbol,
                  date = close_time) %>%
    dplyr::mutate(date = as.Date(date))

  # make tibble long and convert frequency
  crypto_data_long <- crypto_data_tbl %>%
    tidyr::gather(variable, value, -ticker, -date) %>%
    dplyr::group_by(ticker) %>%
    dateR::to_period(., frequency) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

  crypto_data_long
}
