---
title: "Project 1"
author: "Javier"
date: "29/9/2020"
output: html_document
---



### Load packages



```r
library(data.table)
```

```
## data.table 1.12.8 using 1 threads (see ?getDTthreads).  Latest news: r-datatable.com
```

```r
library(purrr)
```

```
## 
## Attaching package: 'purrr'
```

```
## The following object is masked from 'package:data.table':
## 
##     transpose
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(lattice)
library(ggplot2)
```

* * *


#### Load data



```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              "Activity_monitoring.zip")
unzip("Activity_monitoring.zip")
activity <- read.csv("activity.csv")
data <- data.table(activity)
```
* * *

#### Histogram of the total number of steps taken each day

For the first plot i choose use lattice


```r
data[,Date := as.Date(data$date, "%Y-%m-%d")] #transform to a date format
data2 <- data %>% group_by(Date, date) %>% summarise(stepspd = sum(steps, na.rm = T), median = median(steps, na.rm = T), mean = mean(steps, na.rm = T), .groups = "drop")
with(data2, histogram(~stepspd, xlab = "Total Number of Steps per Day", par.settings = list(plot.polygon = list(col = "grey"))))
```

![plot of chunk histogram1](figure/histogram1-1.png)

* * *

#### Mean and median number of steps taken each day



```r
data2 <- data %>% group_by(Date, date) %>% summarise(stepspd = sum(steps, na.rm = T), median = median(steps, na.rm = T), mean = mean(steps, na.rm = T), .groups = "drop")
print(head(data2[,-2]) , type = "html") #mean and median of the data
```

```
## # A tibble: 6 x 4
##   Date       stepspd median    mean
##   <date>       <int>  <dbl>   <dbl>
## 1 2012-10-01       0     NA NaN    
## 2 2012-10-02     126      0   0.438
## 3 2012-10-03   11352      0  39.4  
## 4 2012-10-04   12116      0  42.1  
## 5 2012-10-05   13294      0  46.2  
## 6 2012-10-06   15420      0  53.5
```

* * *


#### Time series plot of the average number of steps taken



```r
int <- data[ , lapply(.SD, mean, na.rm = T), .SDcols = "steps", by = "interval"]
with(int, plot(steps ~ interval, type = "l", ylab = "Average of Steps", xlab = "Interval", col = "grey"))
```

![plot of chunk timeseries1](figure/timeseries1-1.png)

* * *


#### The 5-minute interval that, on average, contains the maximum number of steps

The minute with the highgest average is the 835 with average above 200 steps


```r
int[steps == max(steps),] #interval 835 is the highest point
```

```
##    interval    steps
## 1:      835 206.1698
```

* * *


#### Code to describe and show a strategy for imputing missing data

I just shortcut the missing data, but i don`t eliminate directly becouse in the function mean and median the argument `na.ra = T` if enough look at the different, fisrt the table with the mean for the data whithout missing values 



```r
data3 <- na.omit(data) #data whithout missing values has less rows
data4 <- data3 %>% group_by(Date, date) %>% summarise(stepspd = sum(steps), median = median(steps), mean = mean(steps, na.rm = T), .groups = "drop")
print(head(data4[,-2]) , type = "html") #mean and median of the data
```

```
## # A tibble: 6 x 4
##   Date       stepspd median   mean
##   <date>       <int>  <dbl>  <dbl>
## 1 2012-10-02     126      0  0.438
## 2 2012-10-03   11352      0 39.4  
## 3 2012-10-04   12116      0 42.1  
## 4 2012-10-05   13294      0 46.2  
## 5 2012-10-06   15420      0 53.5  
## 6 2012-10-07   11015      0 38.2
```

and now the data we all of rows using `na.rm = T` for the mean and median calculation



```r
data2 <- data %>% group_by(Date, date) %>% summarise(stepspd = sum(steps, na.rm = T), median = median(steps, na.rm = T), mean = mean(steps, na.rm = T), .groups = "drop")
print(head(data2[,-2]) , type = "html") #mean and median of the data
```

```
## # A tibble: 6 x 4
##   Date       stepspd median    mean
##   <date>       <int>  <dbl>   <dbl>
## 1 2012-10-01       0     NA NaN    
## 2 2012-10-02     126      0   0.438
## 3 2012-10-03   11352      0  39.4  
## 4 2012-10-04   12116      0  42.1  
## 5 2012-10-05   13294      0  46.2  
## 6 2012-10-06   15420      0  53.5
```

i don`t see much difference so i continui with the original all the data


* * *


#### Histogram of the total number of steps taken each day after missing values are imputed



```r
with(data2, histogram(~stepspd, xlab = "Total Number of Steps per Day", par.settings = list(plot.polygon = list(col = "grey"))))
```

![plot of chunk histogram2](figure/histogram2-1.png)

* * *


#### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

The names of the days are in spanish becouse the software that i have is in spanish so in spanis the days names of the weekdays are Lunes, Martes, Miércoles, Jueves and Viernes, in the other hand the names of the weekend days are Sábado and Domingo


```r
data[ , lapply(.SD, weekdays), .SDcols = "Date"]
```

```
##           Date
##     1:   lunes
##     2:   lunes
##     3:   lunes
##     4:   lunes
##     5:   lunes
##    ---        
## 17564: viernes
## 17565: viernes
## 17566: viernes
## 17567: viernes
## 17568: viernes
```

```r
data[ , day := weekdays(Date)]
data[ , typeday := day]
data <- 
data %>% mutate(typeday = ifelse(day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"), "weekday", ifelse(day %in% c("sábado", "domingo"), "weekend", "nothing")))
int1 <- data[ , lapply(.SD, mean, na.rm = T), .SDcols = "steps", by = c("interval", "typeday")]
ggplot(int1, aes(x = interval, y = steps, col = typeday)) + geom_line() + theme_classic() + labs(x = "Interval", y = "Average of Steps", col = "Days") + scale_colour_manual(values = c("grey", "pink"))
```

![plot of chunk weeks](figure/weeks-1.png)
