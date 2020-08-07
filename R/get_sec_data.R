#' get financial statement data from SEC
#'
#' @param tkr string indicating the company ticker
#' @param year numeric indicating the year of the financial statement
#' @param financial_statement string indicating which financial statement you require - one of "IS", "BS" or "CFS"
#'
#' @return \code{data.frame} with columns date, variable, value and unit
#'
get_sec_fs_single <- function(tkr, year, financial_statement) {

  get_fs <- function(tkr, year, financial_statement) {

    switch(financial_statement,
           IS = finreportr::GetIncome(tkr, year),
           BS = finreportr::GetBalanceSheet(tkr, year),
           CFS = finreportr::GetCashFlow(tkr, year))
  }

  sec_fs <- get_fs(tkr, year, financial_statement)

  sec_fs_clean <- sec_fs %>%
    dplyr::select(endDate, Metric, Amount, Units) %>%
    dplyr::mutate(date = as.Date(endDate)) %>% # finreportr returns dates as characters for some genius reason
    dplyr::rename(variable = Metric,
                  value = Amount,
                  unit = Units) %>%
    dplyr::select(date, variable, value, unit)
}


#' import financial statement data from the SEC
#'
#' @param tickers a character vector of tickers
#' @param financial_statement a string indicating which financial statement to retrieve
#' @param year optional numeric indicating which year statement to retrieve
#'
#' @return nested data.frame with columns date, ticker, statement, fs_data
#'
get_sec_fs <- function(tickers, financial_statement = c("IS", "BS", "CFS"), year = NULL) {

  assertR::assert_present(c("IS", "BS", "CFS"), financial_statement, "check that you specified the financial statement type correctly")

  if(is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y")) - 1 # if no year is specified, use last year
  }

  fin_statement_list <- list()

  for(tkr in tickers) {

    t <- which(tickers == tkr)
    progress <- round(t/length(tickers), 2) * 100
    print(glue::glue("attempting to retrieve {tkr} data from the SEC"))

    fin_stmnt <- try(get_sec_fs_single(tkr, year, financial_statement), silent = TRUE)

    if(class(fin_stmnt) == "try-error") {
      fin_stmnt <- NULL
    }

    print(glue::glue("{progress}% complete"))

    fin_statement_list[[tkr]] <- fin_stmnt
  }

  fin_statement_df <- tibble::enframe(fin_statement_list, name = "ticker", value = "fs_data") %>%
    #dplyr::rename_(.dots = setNames("value", financial_statement)) %>%
    dplyr::mutate(date = purrr::map(fs_data, ~max(.$date))[[1]]) %>%
    dplyr::mutate(statement = !!financial_statement) %>%
    dplyr::arrange(ticker) %>%
    dplyr::select(date, ticker, statement, fs_data)

  fin_statement_df
}
