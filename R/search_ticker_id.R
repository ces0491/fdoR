#' private function to find individual ticker names
#'
#' @param pjs_session remote driver object
#' @param ticker_id string indicating an ISIN, CUSIP, SEDOL, or security name or part thereof
#'
#' @return ticker name
#'
pvt_search_ticker_id <- function(pjs_session, ticker_id) {

  #search isin
  searchElem <- pjs_session$findElement(xpath = '//*[@id="appendedInputButton"]')
  searchElem$click()
  searchElem$clear()
  searchElem$sendKeys(list(ticker_id, webdriver::key$enter))

  #get table and extract data
  tableElem <- pjs_session$findElement(id = "results")
  Sys.sleep(1)
  tableDataRaw <- tableElem$getElementText()[[1]]

  RawStr <- stringr::str_split(tableDataRaw, "\n+")[[1]]

  name_str <- stringr::str_replace(RawStr, pattern = ticker_id, replacement = "")

  ticker_name <- unique(trimws(name_str, "both"))

  ticker_name
}

#' find the full names of tickers given an ISIN, CUSIP, SEDOL or security name or part thereof
#'
#' @param tickers a character vector of tickers
#'
#' @return a df of tickers and their full names
#' @export
#'
get_ticker_names <- function(tickers) {

  unq_tickers <- unique(tickers)

  pjs_conn <- webScrapeR::connect_session("https://www.isincodes.net/")
  pjs_session <- pjs_conn$session

  loginElem <- pjs_session$findElement(xpath  = '//*[@id="search-request"]/a/img')
  loginElem$click()

  # send username
  userElem <- pjs_session$findElement(xpath = '//*[@id="user_login"]')
  userElem$clear()
  userElem$sendKeys(list(Sys.getenv("isin_user")))

  # send password
  passwd <- pjs_session$findElement(xpath = '//*[@id="user_pass"]')
  passwd$clear()
  passwd$sendKeys(list(Sys.getenv("isin_pass"), webdriver::key$enter))
  # passwd$sendKeys(webdriver::key$enter)

  # submit
  # submitElem <- pjs_session$findElement(id = "wp-submit")
  # submitElem$click()

  # get ticker names
  ticker_name_list <- list()
  for (tkr in unq_tickers) {

    tkr_data <- try(pvt_search_ticker_id(pjs_session, tkr), silent = TRUE)

    if(class(tkr_data) == "try-error") {
      tkr_data <- NULL
    }

    ticker_name_list[[tkr]] <- tkr_data
    pjs_session$refresh()
  }

  # clean up
  ticker_data <- unlist(ticker_name_list)
  ticker_data_df <- data.frame(ticker_data) %>%
    dplyr::rename(full_name = ticker_data) %>%
    dplyr::mutate(ticker = rownames(.)) %>%
    dplyr::select(ticker, full_name)

  pjs_conn$pjs_process$kill() # Delete the session & close open browsers

  ticker_data_df
}
