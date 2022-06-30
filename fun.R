library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)

# 1.注册或登录理杏仁后
# 2.在 https://www.lixinger.com/open/api/token 获得 Token
# 3.R 控制台, file.edit("~/.Renviron") 打开的文件增补以下新行(不含 # 号)
# token_lixingren = "步骤 2 获得的 Token"
# 4. 保存并关闭 .Renviron 文件

create_post <- function(url = NULL, token = NULL, date = NULL, start_date = NULL,
                        end_date = NULL, stock_codes = NULL, metrics = NULL) {
  # 创建一个符合理杏仁要求的 POST 请求
  if (is.null(token)) {
    temp_body <- list(token = jsonlite::unbox(Sys.getenv("token_lixingren")))
  } else {
    temp_body <- list(token = jsonlite::unbox(token))
  }
  if (!is.null(start_date) & !is.null(end_date)) {
    temp_body$startDate <- jsonlite::unbox(start_date)
    temp_body$endDate <- jsonlite::unbox(end_date)
  }
  if (!is.null(start_date) & length(stock_codes) == 1) {
    temp_body$stockCodes <- stock_codes
  } else if (!is.null(start_date) & length(stock_codes) > 1) {
    stop("'stock_codes' must contain only 1 items", call. = FALSE)
  } else if (is.null(start_date) & is.null(end_date) & !is.null(stock_codes)) {
    if (!is.null(date)) {
      temp_body$date <- jsonlite::unbox(date)
      temp_body$stockCodes <- stock_codes
    } else {
      temp_body$stockCodes <- stock_codes
    }
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

get_cn_stock_code <- function(token = NULL, stock_codes = NULL) {
  # 获取大陆/公司接口/基础信息/获取所有&指定股票信息
  create_post(
    url = "https://open.lixinger.com/api/cn/company",
    token = token,
    stock_codes = stock_codes
  ) %>%
    httr::content(., as = "application/json", encoding = "utf-8") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    tidyr::unnest_longer(., col = mutualMarkets) %>%
    dplyr::select(-c(code, message))
}

get_cn_fundamental <- function(financial_report_type = "non_financial", token = NULL, date = NULL,
                               start_date = NULL, end_date = NULL,
                               stock_codes = NULL, metrics = NULL) {
  # 获取基本面数据，如 PE、PB 等
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
    httr::content(., as = "parsed") %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(., col = data) %>%
    dplyr::select(-c(code, message))
}
