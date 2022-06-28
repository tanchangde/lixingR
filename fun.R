library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)

# 请注册或登录理杏仁在 https://www.lixinger.com/open/api/token 获得自己的 Token
# 替换 “token_lixingren” 的值

Sys.setenv(token_lixingren = "8c1e6a2b-3e45-45bc-952c-84e9df3b3d0a")

create_post <- function(url = NULL, token = NULL, start_date = NULL,
                        end_date = NULL, stock_codes = NULL, metrics = NULL) {
  # 创建一个符合理杏仁要求的 POST 请求
  temp_body <- list(token = jsonlite::unbox(Sys.getenv(token)))
  if (!is.null(start_date) & !is.null(end_date)) {
    temp_body$startDate <- jsonlite::unbox(start_date)
    temp_body$endDate <- jsonlite::unbox(end_date)
  }
  if (length(stock_codes) == 1) {
    temp_body$stockCodes <- stock_codes
  } else if (length(stock_codes) > 1) {
    stop("'stock_codes' must contain only 1 items",
      call. = FALSE
    )
  }
  if (!is.null(metrics)) {
    temp_body$metricsList <- metrics
  }
  httr::POST(
    url = url,
    config = httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(temp_body),
    encode = "raw"
  )
}

get_cn_stock_code <- function(url = NULL, token = NULL) {
  # 获取大陆/公司接口/基础信息/获取所有股票信息
  if (is.null(url)) {
    url <- "https://open.lixinger.com/api/cn/company"
  }
  if (is.null(token)) {
    token <- "token_lixingren"
  }
  create_post(
    url = url,
    token = token
  ) %>%
    httr::content(., as = "parsed") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    tidyr::unnest_longer(., col = mutualMarkets) %>%
    dplyr::select(-c(code, message))
}

get_fundamental <- function(financial_report_type = NULL, token = NULL,
														start_date = NULL, end_date = NULL,
                            stock_codes = NULL, metrics = NULL) {
  url <- switch(financial_report_type,
    "non_financial" = "https://open.lixinger.com/api/cn/company/fundamental/non_financial",
    "bank" = "https://open.lixinger.com/api/cn/company/fundamental/bank",
    "insurance" = "https://open.lixinger.com/api/cn/company/fundamental/security",
    "security" = "https://open.lixinger.com/api/cn/company/fundamental/insurance",
  )
  if (is.null(token)) {
    token <- "token_lixingren"
  }
  create_post(
    url = url, token = token, start_date = start_date,
    end_date = end_date, stock_codes = stock_codes, metrics = metrics
  ) %>%
    httr::content(., as = "parsed") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}
