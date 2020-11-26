#' clean mrp data
#'
#' @param RawStr raw table string scraped from market risk premia website
#' @param country_iso string containing 2iso codes
#' @param variable string indicating which variables to return
#' @param period string from t0 to t3
#'
#' @return df
#'
clean_mrp_data <- function(RawStr, country_iso, variable, period) {

  # the data are returned as a single string per row of the table so we need to separate these in to their respective columns

  reqd_vars <- c("Country", "Currency", "Nr of companies", "Avg\\. nr of analysts per company", "Implied cost of capital \\(ICOC\\)",
                 "Dividend yield \\(based on year t=1\\)", "Dividend growth", "Earnings yield \\(based on year t=1\\)", "Value adding earnings growth",
                 "Risk free rate", "Implied market risk premium \\(IMRP\\)", "Market-to-book ratio", "Price-earnings ratio",
                 "Market value \\(in bn\\. local currency\\)", "Book value \\(in bn\\. local currency\\)", "Earnings forecasts \\(in bn\\. local currency\\)",
                 "Dividend forecasts \\(in bn\\. local currency\\)")

  reqdRawStr <- trimws(RawStr[-c(1:8)], "both") # the 1st 8 strings in the vector don't contain data

  cntry <- stringr::str_extract(reqdRawStr, "^.{2}") # 2 letter country iso is 1st 2 chars
  reqdRawStr <- stringr::str_replace(reqdRawStr, "^.{3}", "") # remove the iso and the following space from the raw string
  dt <- stringr::str_extract(reqdRawStr, "\\d{2}-\\d+-\\d{4}") # get the date which will be a sequence of 2 then 2 or 1 then 4 digits
  reqdRawStr <- stringr::str_replace(reqdRawStr, "\\d{2}-\\d+-\\d{4}\\s+", "") # remove the date and the space following it from the raw string

  # here we filter out what's left after cntry and date by using our reqd vars
  reqd_var_list <- list()
  for(var in reqd_vars) {
    y <- stringr::str_extract(reqdRawStr, var)
    reqd_var_list[[var]] <- y
  }
  var_df <- tibble::enframe(reqd_var_list) %>%
    tidyr::spread(name, value) %>%
    tidyr::unnest(cols = c(`Avg\\. nr of analysts per company`, `Book value \\(in bn\\. local currency\\)`,
                           Country, Currency, `Dividend forecasts \\(in bn\\. local currency\\)`,
                           `Dividend growth`, `Dividend yield \\(based on year t=1\\)`,
                           `Earnings forecasts \\(in bn\\. local currency\\)`, `Earnings yield \\(based on year t=1\\)`,
                           `Implied cost of capital \\(ICOC\\)`, `Implied market risk premium \\(IMRP\\)`,
                           `Market-to-book ratio`, `Market value \\(in bn\\. local currency\\)`,
                           `Nr of companies`, `Price-earnings ratio`, `Risk free rate`,
                           `Value adding earnings growth`))

  vars <- base::do.call(dplyr::coalesce, var_df) # merge all columns in to a single column

  quotemeta <- function(string) {
    stringr::str_replace_all(string, "(\\W)", "\\\\\\1") # escape special characters
  }

  reqdRawStr <- stringr::str_replace(reqdRawStr, quotemeta(vars), "") # remove the variable names from the raw string
  vals <- trimws(reqdRawStr, "both")

  mrp_table <- data.frame(country_iso = cntry, date = dt, variable = vars, value = vals, stringsAsFactors = FALSE) %>%
    tidyr::drop_na(date) %>% # rows with no date are some informational string and don't form part of the data
    tidyr::separate(value, c("t0", "t1", "t2", "t3"), sep = " ", convert = TRUE, fill = "right") %>%
    dplyr::filter(variable != "Country") %>% # we have a country id already
    dplyr::mutate(variable = ifelse(t1 %in% "%", paste(variable, t1), variable)) %>%
    dplyr::mutate(t1 = ifelse(t1 == "%", NA, t1)) %>%
    dplyr::mutate(t1 = as.numeric(t1))

  if (any(country_iso == "all")) {
    mrp_table
  } else {
    cntry <- country_iso
    mrp_table <- mrp_table %>%
      dplyr::filter(country_iso %in% cntry)
  }

  if (any(variable == "all")) {
    mrp_table
  } else {
    var <- variable
    mrp_table <- mrp_table %>%
      dplyr::filter(variable %in% var)
  }

  if (any(period == "all")) {
    mrp_table
  } else {
    mrp_table <- mrp_table %>%
      dplyr::select(country_iso, date, variable, dplyr::all_of(period))
  }

  mrp_table <- tidyr::gather(mrp_table, period, value, -country_iso, -date, -variable)
  mrp_table
}

#' get market risk premium data
#'
#' @param country_iso string indicating the countries to return using their 2iso codes - defaults to all available countries
#' @param variable string indicating which variables to return - defaults to all available variables
#' @param period string indicating which period to return - any of t0, t1, t2, t3 - defaults to all periods
#'
#' @return df with country_iso, date, variable and period
#' @export
#'
get_risk_premia_data <- function(country_iso = "all", variable = "all", period = "all") {

  pjs_conn <- webScrapeR::connect_session("http://www.market-risk-premia.com/za.html")
  pjs_session <- pjs_conn$session

  paramsElem <- pjs_session$findElement(xpath = '//*[@id="tabs"]/ul/li[3]/a')
  paramsElem$click()

  Sys.sleep(2)
  dropdownElem <- pjs_session$findElement(xpath = '//*[@id="myDatatable"]/thead/tr/th[1]/span/select')
  dropdownElem$click()

  selectElem <- pjs_session$findElement(xpath = '//*[@id="myDatatable"]/thead/tr/th[1]/span/select/option[1]')
  selectElem$click()

  tableElem <- pjs_session$findElement(xpath = '//*[@id="myDatatable"]')
  tableDataRaw <- tableElem$getText()[[1]]

  pjs_conn$pjs_process$kill() # Delete the session & close open browsers

  RawStr <- stringr::str_split(tableDataRaw, "\n+")[[1]]

  mrp_table <- clean_mrp_data(RawStr, country_iso, variable, period)
  mrp_table
}

