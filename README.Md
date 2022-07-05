# lixingR

R 语言封装的理杏仁接口。

## 使用说明

### 准备

* 注册或登录理杏仁后，在 [开放平台](https://www.lixinger.com/open/api/token) 获得 Token。
* 在 R 控制台输入 `file.edit("~/.Renviron")` 后，在打开的文件中增补新行：`token_lixingren = "前文获得的 Token"`。
* 保存并关闭 `.Renviron` 文件，然后在 R 会话中运行一遍 `fun.R` 文件，即按需可调用函数。

### 函数调用

参数填写，当前请主要参考[理杏仁 API 文档](https://www.lixinger.com/open/api/doc)。待本封装稳定下来后，将单独提供文档。由于编码风格与语言数据类型存在区别，封装后的函数参数名称做了适当调整。那么调用时，自然也要调整。

调整主要在三点，一是切换与接口文档含义一致的参数。在 IDE， 如 RStudio 中调用时，只需要在函数名后的 `()` 中按下 TAB 键，即可方便的查看该函数有哪些参数可供选择。本封装基本覆盖了原接口的所有参数，只是名称做了形式上的调整或精简。对照 IDE 的提示与[理杏仁 API 文档](https://www.lixinger.com/open/api/doc) ，相信你很快就能找到规律。如果还是有困难，欢迎发起 Issue。

调整的第二点，在于[理杏仁 API 文档](https://www.lixinger.com/open/api/doc) API 试用例子中参数名外面包裹的 `""` 不需要你理会，本封装会自行转换。

调整的第三点，在于当[理杏仁 API 文档](https://www.lixinger.com/open/api/doc)参数类型为 Array 时，我们要用 R 中的向量替代（Vector）。

举个🌰：

[理性仁 API 文档](https://www.lixinger.com/open/api/doc)，大陆/公司接口/基本面/非金融 API, 通过日期获取参数的写法是：

```json
{
	"token": "balalaxiaomoxian",
	"date": "2022-07-05",
	"stockCodes": [
		"300750",
		"600519"
	],
	"metricsList": [
		"pe_ttm",
		"mc"
	]
}
```

而在本封装函数调用中的写法是：

```r
get_cn_company_fundamental <- function(financial_report_type = "non_financial",
                               token = "balalaxiaomoxian", date = "2022-07-05",
                               stock_codes = c("300750", "600519"),
															 metrics = c("pe_ttm", "mc"))
```

### API 与封装函数对应关系

| 理性仁 API 路径           | 封装后 R 函数                                 |
|---------------------------|-----------------------------------------------|
| 大陆/公司/基础信息        | get_cn_company                                |
| 大陆/公司/股本变动        | get_cn_company_equity_change                  |
| 大陆/公司/股票所属指数    | get_cn_company_indices                        |
| 大陆/公司/股票所属行业    | get_cn_company_industries                     |
| 大陆/公司/高管增减持      | get_cn_company_senior_executive_shares_change |
| 大陆/公司/股东人数        | get_cn_company_shareholders_num               |
| 大陆/公司/股权质押        | get_cn_company_pledge                         |
| 大陆/公司/大宗交易        | get_cn_company_block_deal                     |
| 大陆/公司/K线数据         | get_cn_company_candlestick                    |
| 大陆/公司/基本面/非金融   | get_cn_company_fundamental                    |
| 大陆/公司/基本面/银行     | get_cn_company_fundamental                    |
| 大陆/公司/基本面/证券     | get_cn_company_fundamental                    |
| 大陆/公司/基本面/保险     | get_cn_company_fundamental                    |
| 大陆/公司/财务报表/非金融 | get_cn_company_fs                             |
| 大陆/公司/财务报表/银行   | get_cn_company_fs                             |
| 大陆/公司/财务报表/证券   | get_cn_company_fs                             |
| 大陆/公司/财务报表/保险   | get_cn_company_fs                             |

## 动机

* 用于可复现、可复用、版本可控的投研报告撰写
* 减少重复的数据复制粘贴
* 练习接口封装
* 积累 R package 作品

## 目标

- [x] 初级，实现大陆股票下个人关注的基本面指标查询
	- [x] PB
	- [x] PE
	- [x] ROE
	- [x] 负债率
	- [x] 股利支付率
* 中级，实现大部分实用指标查询
* 中高级，打包成 R package
* 高级，提供交互式分析服务

## 为什么选择理杏仁

* [产品、代码及投资 — 产品篇](https://www.lixinger.com/marketing/about-us-product)
* [产品、代码及投资 — 代码篇](https://www.lixinger.com/marketing/about-us-coding)

## 参考文献

* [knitr 与可重复的统计研究（花絮篇） | 统计之都](https://cosx.org/2012/06/reproducible-research-with-knitr/)
* [理杏仁开放平台 API 文档](https://www.lixinger.com/open/api/doc)
* [数据科学中的 R 语言](https://bookdown.org/wangminjie/R4DS/)
* [git - the simple guide - no deep shit!](http://rogerdudler.github.io/git-guide/index.zh.html)

## ChangeLog

* 220705 changde 细化使用说明
* 220630 changde 增补用法
* 220629 changde
	* 调整目标优先级
	*	增补参考文献及润色
* 220627 changde init
