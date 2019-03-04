---
title: "Test_Markdown_doc"
author: "AbhinavB"
date: "February 16, 2019"
output: html_document
---



## R Markdown Test Report

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

## Events samples in talkingdataUserDemographics Problem 

```r
dim(events)
```

```
## [1] 3252950       5
```

```r
str(events)
```

```
## 'data.frame':	3252950 obs. of  5 variables:
##  $ event_id : chr  "1" "2" "3" "4" ...
##  $ device_id: chr  "29182687948017175" "-6401643145415154744" "-4833982096941402721" "-6815121365017318426" ...
##  $ timestamp: chr  "2016-05-01 00:55:25" "2016-05-01 00:54:12" "2016-05-01 00:08:05" "2016-05-01 00:06:40" ...
##  $ longitude: num  121 104 107 104 116 ...
##  $ latitude : num  31.2 31 29.7 23.3 28.7 ...
```

## Including Plots

You can also embed plots, for example:


```r
plot(pressure)
```

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
