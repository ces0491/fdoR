#' get damodaran data
#'
#' @param reqd_file character vector - lookup the name of the file on the website e.g. waccGlobal
#' @param reqd_year numeric indicating the year the dataset was produced, e.g. 2015
#' @param download_directory optional string indicating directory to download files to. Defaults to C:/temp/chromeDL
#'
#' @return tbl_df with columns name, year and raw_df
#' @export
#'
get_damodaran_data <- function(reqd_file, reqd_year, download_directory = NULL) {

  curr_date <- Sys.Date()
  ref_date <- as.Date(glue::glue('{format(curr_date, "%Y")}/01/31')) # files get updated in jan
  req_date <- as.Date(glue::glue("{reqd_year}/01/31"))

  if(req_date >= ref_date) {
    url <- "http://pages.stern.nyu.edu/~adamodar/pc/datasets" # this is url for current datasets
    yr <- format(curr_date, "%y")
  } else {
    url <- "http://pages.stern.nyu.edu/~adamodar/pc/archives" # this is url for archived datasets
    yr <- format(curr_date - 365, "%y")
  }

  if (is.null(download_directory)) {
    download_dir <- "C:/temp/chromeDL/damodaran"
    message(glue::glue("download directory defaulting to temporary folder: {download_dir} - temp files are removed after being loaded"))
  } else {
    download_dir <- download_directory
  }


  webpage <- xml2::read_html(url)

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  xls_files <- stringr::str_subset(list.files(download_dir, full.names = TRUE), ".xls")
  do.call(file.remove, list(xls_files)) # remove any existing xls files in the download directory

  # Extract the URLs
  urls <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  data_tbl <- tibble::tibble(filename = urls) %>%
    dplyr::mutate(link = glue::glue("{url}/{filename}")) %>%
    dplyr::mutate(year = gsub("[aA-zZ,.]", "", filename)) %>% # just get numbers
    dplyr::mutate(year = stringr::str_sub(year, -2)) %>% # get last 2 numbers only
    dplyr::mutate(year = ifelse(year == "", yr, year)) %>%  # empty strings are the most recent - t for current data and t-1 for archived
    dplyr::mutate(name = stringr::str_replace_all(filename, paste0(year, ".xls"), ""))

  download_file <- data_tbl %>%
    dplyr::filter(name %in% reqd_file) %>%
    dplyr::filter(year == stringr::str_sub(reqd_year, -2)) %$%
    link

  file_list <- list()

  for(dfile in download_file) {

    destfile <- glue::glue("{download_dir}/{basename(dfile)}")
    utils::download.file(dfile, destfile, mode = "wb")
    file_list[[dfile]] <- readxl::read_xls(destfile)

    if (is.null(download_directory)) {
      fileR::clear_files(download_dir, basename(dfile))
    }
  }

  cleanup <- function(raw_df) {
    clean <- tidyr::drop_na(raw_df) # remove rows containing NA
    colnames(clean) <- clean[1,] # convert first row to col names
    clean <- clean[-1,] # remove first row
    clean <- utils::type.convert(clean) # convert column data types
    clean
  }

  file_df <- file_list %>%
    tibble::enframe(name = "link", value = "raw_df") %>%
    dplyr::left_join(data_tbl, by = "link") %>%
    dplyr::mutate(clean_df = purrr::map(raw_df, cleanup)) %>%
    dplyr::select(name, year, raw_df, clean_df)

  file_df
}

