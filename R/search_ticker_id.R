#' Wrapper function to companyDataScrapeR::get_ticker_id
#' This is a convenience function and shouldn't really need to be used. Names are now extracted in the meta data from calls to retrieve equity data
#'
#' @param tickers a character vector of tickers, security names or ISINs
#'
#' @return a tbl_df with columns ticker, name and isin
#' @export
#'
get_ticker_names <- function(tickers) {

  tk_df <- companyDataScrapeR::get_ticker_id(tickers)

  tk_df
}
