# lixingR

R 语言封装的理杏仁接口。

## 用法

* 注册或登录理杏仁后，在 [开放平台](https://www.lixinger.com/open/api/token) 获得 Token。
* 在 R 控制台输入 `file.edit("~/.Renviron")` 打开的文件增补新行：`token_lixingren = "步骤 2 获得的 Token"`。
* 保存并关闭 `.Renviron` 文件，然后在 R 会话中运行一遍 `fun.R` 文件，即按需可调用函数。用例待补充...

## 动机

* 用于可复现、可复用、版本可控的投研报告撰写
* 减少重复的数据复制粘贴
* 练习接口封装
* 积累 R package 作品

## 目标

* 初级，实现大陆股票下个人关注的基本面指标查询
	-[x] PB
	-[x] PE
	* ROE
	* 负债率
	* 股利支付率
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

* 220629 changde
	* 调整目标优先级
	*	增补参考文献及润色
* 220627 changde init 