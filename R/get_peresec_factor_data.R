#' Access SA Factor Data Library from Peresec
#'
#' @param reqd_file string indicating the required file e.g. long_short_alsi
#' @param reqd_factor string indicating the factors you require from the file e.g. ff3
#' @param download_directory optional string indicating the download directory
#'
#' @return tbl_df with nested raw data returned from tidyxl
#'
scrape_peresec_factor_data <- function(reqd_file, reqd_factor = c("ff3", "ff5", "carhart4", "aqr6", "peresec7lowvol", "peresec7lowbeta"), download_directory = NULL) {

  url <- "https://www.peresec.com"

  assertR::assert_present(c("long_only_alsi", "long_short_alsi", "quintile_alsi", "decile_alsi",
                            "long_only_constrained", "long_short_constrained", "quintile_constrained", "decile_constrained"), reqd_file)

  peresec_config <- readRDS("./inst/extdata/peresec_config.rds")

  assertR::assert_present(names(peresec_config), c("short_name", "long_name"))

  file_name <- peresec_config %>%
    dplyr::filter(short_name %in% reqd_file) %$%
    long_name

  reqd_sheet <- peresec_config %>%
    dplyr::filter(short_name %in% reqd_factor) %$%
    long_name

  if (is.null(download_directory)) {
    download_dir <- glue::glue("C:/temp/chromeDL/peresec_factor_data")
    message(glue::glue("download directory defaulting to temporary folder: {download_dir} - temp files are removed after being loaded"))
  } else {
    download_dir <- download_directory
  }

  webpage <- xml2::read_html(glue::glue("{url}/insights"))
  urls <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  data_tbl <- tibble::tibble(filename = urls) %>%
    dplyr::mutate(link = glue::glue("{url}/{filename}"))

  download_link <- data_tbl %>%
    dplyr::mutate(name = basename(link)) %>%
    dplyr::mutate(name = stringr::str_replace(name, ".xlsx", "")) %>%
    dplyr::filter(name %in% file_name) %$%
    link

  dl_list <- list()
  for(dl in download_link) {

    fname <- stringr::str_remove_all(basename(dl), ".xlsx") # remove extension for file name
    destfile <- glue::glue("{download_dir}/{fname}.xlsx") # add extension to file name for path
    utils::download.file(dl, destfile, mode = "wb")
    raw <- tidyxl::xlsx_cells(path = destfile, sheets = reqd_sheet)

    raw_split <- raw %>%
      dplyr::nest_by(sheet)

    dl_list[[fname]] <- raw_split

    if (is.null(download_directory)) {
      fileR::clear_files(download_dir, glue::glue("{fname}.xlsx")) # clearing temp file
    }
  }

  raw_data_df <- tibble::enframe(dl_list, value = "raw_data") %>%
    tidyr::unnest(raw_data) %>%
    dplyr::left_join(peresec_config, by = c("name" = "long_name")) %>%
    dplyr::select(short_name, sheet, data) %>%
    dplyr::rename(name = short_name, factor = sheet, raw_data = data)

  raw_data_df
}

#' get factors by weight
#'
#' a cleaning function applied to the raw data retrieved from Peresec
#'
#' @param raw_factor_data raw data retrieved from peresec website using tidyxl
#' @param weight string indicating whether to return equal or market cap weighted data
#'
#' @return tbl with cols  date, and factors e.g. Market, Size, Value, etc.
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

#' Get Equity Risk Factor Data for the South African Equity Market from Peresec
#'
#' @param reqd_file string indicating the required file e.g. long_short_alsi
#' @param reqd_factor string indicating the factors you require from the file e.g. ff3
#' @param weight string indicating whether to return equal or market cap weighted data
#' @param download_directory optional string indicating the destination directory for downloaded files - defaults to chromeDL temp dir
#'
#' @return tbl_df with
#' @export
#'
get_peresec_erf_data <- function(reqd_file = c("long_only_alsi", "long_short_alsi", "quintile_alsi", "decile_alsi",
                                               "long_only_constrained", "long_short_constrained", "quintile_constrained", "decile_constrained"),
                                reqd_factor = c("ff3", "ff5", "carhart4", "aqr6", "peresec7lowvol", "peresec7lowbeta"),
                                weight = c("equal", "market_cap"),
                                download_directory = NULL) {

  scraped_data <- scrape_peresec_factor_data(reqd_file, reqd_factor, download_directory)

  result <- scraped_data %>%
    dplyr::mutate(clean_data = purrr::map(raw_data, get_factors_by_weight, weight))

  result
}
