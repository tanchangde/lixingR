library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)

create_post <- function(url = NULL, token = NULL, date = NULL, start_date = NULL,
                        end_date = NULL, stock_codes = NULL, metrics = NULL) {
  if (is.null(token)) {
    temp_body <- list(token = jsonlite::unbox(Sys.getenv("token_lixingren")))
  } else {
    temp_body <- list(token = jsonlite::unbox(token))
  }
  if (!is.null(start_date)) {
    temp_body$startDate <- jsonlite::unbox(start_date)
  }
  if (!is.null(end_date)) {
    temp_body$endDate <- jsonlite::unbox(end_date)
  }
  if (!is.null(stock_codes)) {
    temp_body$stockCodes <- stock_codes
  }
  if (!is.null(date)) {
    temp_body$date <- jsonlite::unbox(date)
  }
  if (!is.null(metrics)) {
    temp_body$metricsList <- metrics
  }
  temp_POST <- httr::POST(
    url = url,
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(temp_body),
    encode = "raw"
  )
  if (http_error(temp_POST)) {
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

get_cn_stock_code <- function(token = NULL, stock_codes = NULL) {
  create_post(
    url = "https://open.lixinger.com/api/cn/company",
    token = token,
    stock_codes = stock_codes
  ) %>%
    magrittr::use_series(content) %>%
    rawToChar(.) %>%
    jsonlite::fromJSON(.) %>%
    as.data.frame(.) %>%
    dplyr::select(-c(code, message))
}

get_cn_fundamental <- function(financial_report_type = "non_financial", token = NULL, date = NULL,
                               start_date = NULL, end_date = NULL,
                               stock_codes = NULL, metrics = NULL) {
  url <- switch(financial_report_type,
    "non_financial" = "https://open.lixinger.com/api/cn/company/fundamental/non_financial",
    "bank" = "https://open.lixinger.com/api/cn/company/fundamental/bank",
    "insurance" = "https://open.lixinger.com/api/cn/company/fundamental/security",
    "security" = "https://open.lixinger.com/api/cn/company/fundamental/insurance",
    stop("Unknown `financial_report_type`", call. = FALSE)
  )
  create_post(
    url = url, token = token, date = date, start_date = start_date,
    end_date = end_date, stock_codes = stock_codes, metrics = metrics
  ) %>%
    httr::content(., as = "parsed", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}
