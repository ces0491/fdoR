#' Wrapper function to companyDataScrapeR
#'
#' @param tickers character vector of tickers where data is prefixed with the source, e.g. YAH-EXAMPLETICKER
#' @param type string specifying the type of data required - one of is, bs, cfs or price
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the frequency to return price data, e.g. 'monthly'
#'
#' @return object of class \code{tbl_df} with columns, date, ticker, type and nested company data
#' @export
#'
get_equity_data <- function(tickers, type, start_date = NULL, end_date = NULL, frequency = NULL) {

  equity_data <- companyDataScrapeR::get_company_data(tickers, type, start_date, end_date, frequency)

  equity_data
}
