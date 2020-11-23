
#' Wrapper function to wgbDataScrapeR to retrieve sovereign fixed income data
#'
#' @param country_iso character vector containing 2 letter country iso codes
#' @param metric string indicating whether you'd like to return sovereign rating, cds or yield data. sovereign rating defaults to S&P
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the periodicity of data to retrieve - one of daily, weekly, monthly, quarterly or annual
#' @param include_forecast logical indicating whether forecasts should be included for the yield metrics - default to FALSE
#'
#' @return \code{tbl_df} with columns country iso, date, metric and value
#' @export
#'
get_bond_data <- function(country_iso, metric, start_date, end_date, frequency = c('daily', 'weekly', 'monthly', 'annual'), include_forecast = FALSE) {

  bond_data <- wgbDataScrapeR::get_wgb_data(country_iso, metric, start_date, end_date, frequency, include_forecast)

  bond_data

}
