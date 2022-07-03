library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)

create_post <- function(url = NULL, api_type = NULL, token = NULL, fs_type = NULL,
                        date = NULL, start_date = NULL, end_date = NULL,
                        mutual_markets = NULL, include_delisted = NULL,
                        stock_codes = NULL, metrics = NULL, granularity = NULL) {
  if (is.null(token)) {
    temp_body <- list(token = jsonlite::unbox(Sys.getenv("token_lixingren")))
  } else {
    temp_body <- list(token = jsonlite::unbox(token))
  }
  if (!is.null(fs_type)) {
    temp_body$fsType <- jsonlite::unbox(fs_type)
  }
  if (!is.null(start_date)) {
    temp_body$startDate <- jsonlite::unbox(start_date)
  }
  if (!is.null(end_date)) {
    temp_body$endDate <- jsonlite::unbox(end_date)
  }
  if (!is.null(mutual_markets)) {
    temp_body$mutualMarkets <- mutual_markets
  }
  if (!is.null(include_delisted)) {
    temp_body$includeDelisted <- jsonlite::unbox(include_delisted)
  }
  stock_codes_is_array <- api_type %in% c(
    "company", "company_hot_tr_dri",
    "company_hot_mm_ha", "company_hot_mtasl",
    "company_hot_esc", "company_hot_elr",
    "company_hot_ple", "company_hot_shnc",
    "company_hot_df", "company_fundamental_non_financial",
    "company_fundamental_non_financial",
    "company_fundamental_bank",
    "company_fundamental_security",
    "company_fundamental_insurance",
    "company_fs_non_financial",
    "company_fs_bank", "company_fs_security",
    "company_fs_insurance", "index",
    "index_fundamental", "index_fs",
    "index_constituents", "index_hot_mm_ha",
    "index_hot_mtasl", "index_hot_cp",
    "index_hot_tr_cp"
  )
  if (!is.null(stock_codes) & stock_codes_is_array) {
    temp_body$stockCodes <- stock_codes
  } else if (!is.null(stock_codes) & !stock_codes_is_array) {
    temp_body$stockCode <- jsonlite::unbox(stock_codes)
  }
  if (!is.null(date)) {
    temp_body$date <- jsonlite::unbox(date)
  }
  if (!is.null(metrics)) {
    temp_body$metricsList <- metrics
  }
  if (!is.null(granularity)) {
    temp_body$granularity <- granularity
  }
  temp_POST <- httr::POST(
    url = url,
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(temp_body),
    encode = "raw"
  )
  if (httr::http_error(temp_POST)) {
    temp_error <- temp_POST %>%
      magrittr::use_series(content) %>%
      rawToChar(.) %>%
      jsonlite::fromJSON(.) %>%
      magrittr::use_series(error)
    print(temp_error)
    stop("The query failed, please check the above error.", call. = FALSE)
  } else {
    temp_POST
  }
}

get_cn_company <- function(token = NULL, fs_type = NULL, mutual_markets = NULL,
                           stock_codes = NULL, include_delisted = NULL) {
  url <- "https://open.lixinger.com/api/cn/company"
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace(., "/", "_")
  create_post(
    url = url,
    api_type = api_type,
    token = token,
    fs_type = fs_type,
    mutual_markets = mutual_markets,
    stock_codes = stock_codes,
    include_delisted = include_delisted
  ) %>%
    magrittr::use_series(content) %>%
    rawToChar(.) %>%
    jsonlite::fromJSON(.) %>%
    as.data.frame(.) %>%
    dplyr::select(-c(code, message))
}

get_cn_company_equity_change <- function(token = NULL, start_date = NULL,
                                         end_date = NULL, stock_code = NULL) {
  url <- "https://open.lixinger.com/api/cn/company/equity-change"
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace_all(., "/", "_")
  create_post(
    url = url, api_type = api_type, token = token, start_date = start_date,
    end_date = end_date, stock_codes = stock_code
  ) %>%
    httr::content(., as = "parsed", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}

get_cn_company_indices <- function(token = NULL, stock_code = NULL, date = NULL) {
  url <- "https://open.lixinger.com/api/cn/company/indices"
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace_all(., "/", "_")
  create_post(
    url = url, api_type = api_type, token = token, stock_codes = stock_code,
    date = date
  ) %>%
    httr::content(., as = "parsed", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}

get_cn_company_industries <- function(token = NULL, stock_code = NULL, date = NULL) {
  url <- "https://open.lixinger.com/api/cn/company/industries"
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace_all(., "/", "_")
  create_post(
    url = url, api_type = api_type, token = token, stock_codes = stock_code,
    date = date
  ) %>%
    httr::content(., as = "parsed", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}

get_cn_company_fundamental <- function(financial_report_type = "non_financial",
                               token = NULL, date = NULL,
                               start_date = NULL, end_date = NULL,
                               stock_codes = NULL, metrics = NULL) {
  url <- switch(financial_report_type,
    "bank"          = "https://open.lixinger.com/api/cn/company/fundamental/bank",
    "insurance"     = "https://open.lixinger.com/api/cn/company/fundamental/security",
    "security"      = "https://open.lixinger.com/api/cn/company/fundamental/insurance",
    "non_financial" = "https://open.lixinger.com/api/cn/company/fundamental/non_financial",
    stop("Unknown `financial_report_type`", call. = FALSE)
  )
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace_all(., "/", "_")
  create_post(
    url = url, api_type = api_type, token = token, date = date,
    start_date = start_date, end_date = end_date, stock_codes = stock_codes,
    metrics = metrics
  ) %>%
    httr::content(., as = "parsed", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}

get_cn_company_fs <- function(token = NULL, fs_type = NULL, date = NULL, start_date = NULL,
                              end_date = NULL, stock_codes = NULL, metrics = NULL) {
  url <- switch(fs_type,
                "bank"          = "https://open.lixinger.com/api/cn/company/fs/bank",
                "insurance"     = "https://open.lixinger.com/api/cn/company/fs/insurance",
                "security"      = "https://open.lixinger.com/api/cn/company/fs/security",
                "non_financial" = "https://open.lixinger.com/api/cn/company/fs/non_financial",
                stop("Unknown `fs_type`", call. = FALSE)
  )
  api_type <- url %>%
    stringr::str_match(., "company.*$") %>%
    stringr::str_replace_all(., "/", "_")
  create_post(
    url = url,
    api_type = api_type,
    token = token,
    date = date,
    start_date = start_date,
    end_date = end_date,
    stock_codes = stock_codes,
    metrics = metrics
  ) %>%
    magrittr::use_series(content) %>%
    rawToChar(.) %>%
    jsonlite::fromJSON(.) %>%
    as.data.frame(.) %>%
    dplyr::select(-c(code, message))
}
