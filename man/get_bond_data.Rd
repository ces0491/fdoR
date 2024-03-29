% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bond_data.R
\name{get_bond_data}
\alias{get_bond_data}
\title{Wrapper function to wgbDataScrapeR to retrieve sovereign fixed income data}
\usage{
get_bond_data(
  country_iso,
  metric,
  start_date,
  end_date,
  frequency = c("daily", "weekly", "monthly", "quarterly", "annual"),
  include_forecast = FALSE
)
}
\arguments{
\item{country_iso}{character vector containing 2 letter country iso codes}

\item{metric}{string indicating whether you'd like to return sovereign rating, cds or yield data. sovereign rating defaults to S&P}

\item{start_date}{start date}

\item{end_date}{end date}

\item{frequency}{string indicating the periodicity of data to retrieve - one of daily, weekly, monthly, quarterly or annual}

\item{include_forecast}{logical indicating whether forecasts should be included for the yield metrics - default to FALSE}
}
\value{
\code{tbl_df} with columns country iso, date, metric and value
}
\description{
Wrapper function to wgbDataScrapeR to retrieve sovereign fixed income data
}
