#' Access SA Factor Data Library from Legae Peresec
#'
#' @param reqd_file string indicating the required file e.g. long_short_alsi
#' @param reqd_factor string indicating the factors you require from the file e.g. ff3
#' @param download_directory optional string indicating the download directory
#'
#' @return tbl_df returned from tidyxl
#'
scrape_legae_factor_data <- function(reqd_file, reqd_factor = c("ff3", "ff5", "carhart4", "aqr6"), download_directory = NULL) {

  url <- "https://www.legaeperesec.co.za"

  fractalAssert::assert_allpresent(c("long_only_alsi", "long_short_alsi", "quintile_alsi", "decile_alsi",
                                     "long_only_constrained", "long_short_constrained", "quintile_constrained", "decile_constrained"), reqd_file)

  file_name <- switch(reqd_file,
                      long_only_alsi = "LegaePeresecLongOnlyFactorsFullALSI",
                      long_short_alsi = "LegaePeresecLongShortFactorsFullALSI",
                      quintile_alsi = "LegaePeresecQuintileFactorPortfoliosFullALSI",
                      decile_alsi = "LegaePeresecDecileFactorPortfoliosFullALSI",
                      long_only_constrained = "LegaePeresecLongOnlyFactorsConstrainedALSI",
                      long_short_constrained = "LegaePeresecLongShortFactorsConstrainedALSI",
                      quintile_constrained = "LegaePeresecQuintileFactorPortfoliosConstrainedALSI",
                      decile_constrained = "LegaePeresecDecileFactorPortfoliosConstrainedALSI")

  reqd_sheet <- switch(reqd_factor,
                       ff3 = "Fama-French 3F",
                       ff5 = "Fama-French 5F",
                       carhart4 = "Carhart 4F",
                       aqr6 = "AQR 6F")

  if (is.null(download_directory)) {
    download_dir <- glue::glue("{tempdir()}/legae_factor_data")
    message(glue::glue("download directory defaulting to {download_dir}"))
  } else {
    download_dir <- download_directory
  }

  webpage <- xml2::read_html(url)
  urls <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  xlsx_files <- stringr::str_subset(list.files(download_dir, full.names = TRUE), ".xlsx")
  do.call(file.remove, list(xlsx_files)) # remove any existing xlsx_files in the download directory

  data_tbl <- tibble::tibble(filename = urls) %>%
    dplyr::mutate(link = glue::glue("{url}/{filename}"))

  download_file <- data_tbl %>%
    dplyr::filter(filename == glue::glue("/ResearchTab/{file_name}.xlsx")) %$%
    link

  utils::download.file(download_file, destfile = glue::glue("{download_dir}/{basename(download_file)}"), mode = "wb")

  downloaded_files <- file.info(list.files(download_dir, full.names = TRUE))
  last_download <- rownames(downloaded_files)[which.max(downloaded_files$mtime)]

  # raw_data <- readxl::read_xlsx(path = last_download, sheet = reqd_sheet)
  raw_data <- tidyxl::xlsx_cells(path = last_download, sheets = reqd_sheet)

  raw_data
}

#' get factors by weight
#'
#' @param raw_factor_data raw data retreived from legae webiste using tidyxl
#' @param weight string indicating whether to return equal or market cap weighted data
#'
#' @return tbl_df
#'
get_factors_by_weight <- function(raw_factor_data, weight = c("equal", "market_cap")) {

  ewf_top_left <- raw_factor_data %>%
    dplyr::filter(character == "EQUAL-WEIGHT FACTORS") %>%
    dplyr::select(row, col)

  cwf_top_left <- raw_factor_data %>%
    dplyr::filter(character == "CAP-WEIGHT FACTORS") %>%
    dplyr::select(row, col)

  if(weight == "equal") {
    raw_fw_data <- raw_factor_data %>%
      dplyr::filter(row > ewf_top_left$row) %>%
      dplyr::filter(col < cwf_top_left$col) %>%
      dplyr::filter(col >= ewf_top_left$col) %>%
      dplyr::filter(!is_blank)
  } else {
    raw_fw_data <- raw_factor_data %>%
      dplyr::filter(row > cwf_top_left$row) %>%
      dplyr::filter(col >= cwf_top_left$col) %>%
      dplyr::filter(!is_blank)
  }

  factors <- raw_fw_data %>%
    dplyr::filter(!is.na(character)) %>%
    dplyr::select(col, character)

  values <- raw_fw_data %>%
    dplyr::filter(!is.na(numeric)) %>%
    dplyr::select(row, col, numeric)

  dates <- raw_fw_data %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(date = dateUtils::get_eom_dts(date)) %>%
    dplyr::select(row, date)

  ewf_data <- factors %>%
    dplyr::left_join(values, by = "col") %>%
    dplyr::left_join(dates, by = "row") %>%
    dplyr::select(-row, -col) %>%
    tidyr::spread(character, numeric)

  ewf_data
}

#' Get Factor Data for the South African Equity Market from Legae Peresec
#'
#' @param reqd_file string indicating the required file e.g. long_short_alsi
#' @param reqd_factor string indicating the factors you require from the file e.g. ff3
#' @param weight string indicating whether to return equal or market cap weighted data
#' @param download_directory optional string indicating the destination directory for downloaded files - defaults to chromeDL temp dir
#'
#' @return wide tbl_df with date and factors
#' @export
#'
get_legae_factor_data <- function(reqd_file, reqd_factor = c("ff3", "ff5", "carhart4", "aqr6"), weight = c("equal", "market_cap"), download_directory = NULL) {

  scraped_data <- scrape_legae_factor_data(reqd_file, reqd_factor, download_directory)

  result <- get_factors_by_weight(scraped_data, weight)

  result
}
