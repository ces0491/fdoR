#' get damodaran data
#'
#' @param reqd_file character vector - lookup the name of the file on the website e.g. waccGlobal
#' @param reqd_year numeric indicating the year the dataset was produced
#' @param download_directory optional string indicating directory to download files to. Defaults to C:/temp/chromeDL
#'
#' @return tbl
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
    message(glue::glue("download directory defaulting to {download_dir}"))
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
    dplyr::mutate(year = ifelse(year == "", yr, year)) # empty strings are the most recent - t for current data and t-1 for archived

  download_file <- data_tbl %>%
    dplyr::filter(year == stringr::str_sub(reqd_year, -2)) %>%
    dplyr::filter(filename == glue::glue("{reqd_file}.xls")) %$%
    link

  utils::download.file(download_file, destfile = glue::glue("{download_dir}/{basename(download_file)}"), mode = "wb")

  downloaded_files <- file.info(list.files(download_dir, full.names = TRUE))
  last_download <- rownames(downloaded_files)[which.max(downloaded_files$mtime)]

  raw_data <- readxl::read_xls(last_download)
}

