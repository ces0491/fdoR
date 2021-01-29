#' Wrapper function to companyDataScrapeR::get_ticker_id
#'
#' @param tickers a character vector of tickers, security names or ISINs
#'
#' @return a tbl_df with columns ticker, name and isin
#' @export
#'
get_ticker_names <- function(tickers) {

  tk_list <- list()

  for(ticker in tickers) {
    tk <- companyDataScrapeR::get_ticker_id(ticker)
    tk_list[[ticker]] <- tk
  }

  name_nest <- tibble::enframe(tk_list)
  result <- tidyr::unnest(name_nest[,2], cols = value)

  result
}
