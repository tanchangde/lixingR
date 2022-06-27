library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)

# 请注册或登录理杏仁在 https://www.lixinger.com/open/api/token 获得自己的 Token 
# 替换 “token_lixingren” 的值 

Sys.setenv(token_lixingren = "8c1e6a2b-3e45-45bc-952c-84e9df3b3d0a")

create_post <- function(url = NULL, token = NULL) {
	# 创建一个符合理杏仁要求的 POST 请求
	httr::POST(
		url = url,
		config = add_headers("Content-Type" = "application/json"),
		body = toJSON(list(token = unbox(Sys.getenv(token)))),
		encode = "raw"
	)
}

get_cn_stock_code <- function(url = NULL, token = NULL) {
	# 获取大陆/公司接口/基础信息/获取所有股票信息
	if (is.null(url)) 
		url <- "https://open.lixinger.com/api/cn/company"
	if (is.null(token)) 
		token <- "token_lixingren"
	create_post(url = url,
							token = token) %>% 
		httr::content(., as = "parsed") %>% 
		tibble::as_tibble(.) %>% 
		tidyr::unnest_wider(data) %>% 
		tidyr::unnest_longer(mutualMarkets) %>% 
		dplyr::select(-c(code,message))
}
