---
title       : Impact of Severe Weather Events
author      : Yichen Liu
job         : Developing Data Products Project
framework   : revealjs        # {io2012, html5slides, shower, dzslides, ...}
revealjs    : {transition: linear}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Impact of Severe Weather Events

#### Developing Data Products Project

### Yichen Liu

---

## Initiation

* #### Severe weather events can cause both public health and economic problems for communities and municipalities

* #### Understanding the impact of these events provides us insights for the future prevention

* #### This application helps us analyse the impact of severe weather events on the economy and the population in the United States

---

## The Data

* #### Utilises data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database

* #### Raw data can be downloaded from: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

* #### The data is processed using the same data processing method for my Reproducible Research project 

* #### Attributes of the processed data are shown as followed:


```r
processed_data <- read.csv("Data/data.csv", stringsAsFactors = FALSE)
str(processed_data)
```

```
## 'data.frame':	912 obs. of  7 variables:
##  $ REGION         : chr  "SOUTH" "SOUTHWEST" "MID-ATLANTIC" "NORTHEAST" ...
##  $ YEAR           : int  1996 1996 1997 1997 1997 1997 1998 1998 1998 1998 ...
##  $ EVENT_TYPE     : chr  "DROUGHT" "DROUGHT" "DROUGHT" "DROUGHT" ...
##  $ FATALITIES     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ INJURIES       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ DAMAGE_PROPERTY: num  0.00 1.35e+08 0.00 0.00 0.00 ...
##  $ DAMAGE_CROP    : num  4.00e+06 5.00e+08 1.31e+08 2.00e+05 6.65e+07 ...
```

--- &twocol

## How to Use

*** =left

!['Screenshot'](Screenshot.PNG)

*** =right

* #### Access the application from: http://yichenliu.shinyapps.io/Develop_Data_Products/

* #### Click on the "Analysis" tab at the top of the page

* #### Make selections on Year, Events Type and Region on the main selection panel

* #### Select analysis object - "Economic Impact" or "Population Impact"

* #### Select impact measurement

* #### A series of charts will be produced based on your selections

---

## Key Attributes of the Application

* #### Three dimensional analysis - by Region, by Events and by Year

* #### Comprehensive selection panel and your choice of measurement

* #### Clear layout - analyse the impact on the economy or the population