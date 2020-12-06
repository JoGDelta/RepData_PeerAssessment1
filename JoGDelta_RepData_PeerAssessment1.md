---
title: "PA1_template"
author: "JoGDelta"
date: "12/5/2020"
output: 
  html_document: 
    keep_md: yes
---



## Background information on assignment

This assignment makes use of data from a personal activity monitoring device that collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012, and includes the number of steps taken in 5 minute intervals each day.

The assigment will be completed in a number of parts using both the base and ggplot2 plotting packages.

## Part 1 - Loading and preprocessing the data

#### 1. Load the data.


```r
activity <- read.csv("./activity.csv")

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

#### 2. Process/transform the data into a suitable format for analysis.  

**Note that no processing was required at this time.**

## Part 2 - What is the mean total number of steps taken per day?

For this part of the assignment, missing data values in the dataset are ignored.

#### 1. Make a histogram of the total number of steps taken each day.


```r
StepsByDay <- aggregate(steps~date, activity, sum)

hist(StepsByDay$steps, xlab = "number of steps per day", ylab = "number of days", main = "Total Steps Taken per Day")
```

![](JoGDelta_RepData_PeerAssessment1_files/figure-html/part2_hist-1.png)<!-- -->

#### 2. Calculate and report the mean and median total number of steps taken per day.


```r
mean(StepsByDay$steps)
```

```
## [1] 10766.19
```

```r
median(StepsByDay$steps)
```

```
## [1] 10765
```

## Part 3 - What is the average daily activity pattern?

#### 1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.


```r
StepsByInterval <- aggregate(steps~interval, activity, mean)

with(StepsByInterval, plot(interval, steps, type = "l"))
```

![](JoGDelta_RepData_PeerAssessment1_files/figure-html/part3_series-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
StepsByInterval[which.max(StepsByInterval[,2]),1]
```

```
## [1] 835
```

## Part 4 - Imputing missing values

There are a number of days/intervals where there are missing values coded as NA. These must be addressed to avoid introducing bias into some calculations or summaries of the data.

#### 1. Calculate and report the total number of missing values in the dataset.


```r
sum(is.na(activity[,1]))
```

```
## [1] 2304
```

There are 2304 missing values in the dataset.

#### 2. Devise a strategy to fill in missing values in the dataset.

I will fill in missing data using the average number of steps during that interval.


```r
avg_steps <- mean(StepsByInterval$steps)
```

#### 3. Create a new dataset that is equal to the original dataset, but with the missing data filled in.


```r
missing <- is.na(activity[, 1])

activity_imputed <- activity
activity_imputed[missing, 1] <- avg_steps

head(activity_imputed)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

#### 4. Make a histogram of the number of steps taken each day, and calculate the mean and median total number of steps taken per day.


```r
StepsByDay_imputed <- aggregate(steps~date, activity_imputed, sum)

hist(StepsByDay_imputed$steps, xlab = "number of steps per day", ylab = "number of days", main = "Total Steps Taken per Day (with missing values imputed)")
```

![](JoGDelta_RepData_PeerAssessment1_files/figure-html/part4_hist-1.png)<!-- -->


```r
mean(StepsByDay_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(StepsByDay_imputed$steps)
```

```
## [1] 10766.19
```

With the missing data filled in, the new mean (using imputed data) does not differ from the old mean, while the new median is bit higher than the old median. Also, the new mean and new median are now the same. These results are unsurprising because I used the mean (average) number of steps per interval to fill in the 2304 missing values.

## Part 5 - Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels - weekday and weekend.


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'dplyr'
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
activity_imputed$date <- as.Date(activity_imputed$date)

activity_DOW <- activity_imputed %>%
        mutate(DayType = ifelse(weekdays(activity_imputed$date)=="Saturday" | weekdays(activity_imputed$date)=="Sunday", "weekend", "weekday"))

head(activity_DOW)
```

```
##     steps       date interval DayType
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

#### 2. Make a panel plot containg a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekdays or weekends.


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
StepsByInterval_DayType <- aggregate(steps~interval + DayType, activity_DOW, mean)

head(StepsByInterval_DayType)
```

```
##   interval DayType    steps
## 1        0 weekday 7.006569
## 2        5 weekday 5.384347
## 3       10 weekday 5.139902
## 4       15 weekday 5.162124
## 5       20 weekday 5.073235
## 6       25 weekday 6.295458
```

```r
plot <- ggplot(StepsByInterval_DayType, aes(x = interval, y = steps, color = DayType)) +
        geom_line() +
        labs(title = "Average Steps per Interval, Weekdays vs Weekends", x = "interval", y = "average number of steps") +
        facet_wrap(~DayType, ncol = 1, nrow = 2)
print(plot)
```

![](JoGDelta_RepData_PeerAssessment1_files/figure-html/part5_plot-1.png)<!-- -->
