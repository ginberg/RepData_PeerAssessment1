---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
setwd("~/admin/freelance/Courses/Coursera/ReproResearch")
data <-read.csv("RepData_PeerAssessment1/activity.csv")
data_clean <- file[complete.cases(file),]
```

## What is mean total number of steps taken per day?

```r
library(plyr)
total_by_day <- ddply(data_clean, .(date), summarise, steps=sum(steps))
hist(total_by_day$steps, main="Number of Steps", xlab="Total number of steps taken each day", col="light blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(total_by_day$steps)
```

```
## [1] 10766
```

```r
median(total_by_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
average_by_interval <- ddply(data_clean, .(interval), summarise, steps=mean(steps))
plot(average_by_interval$interval, average_by_interval$steps, type="l", 
     col="blue",
     xlab="5-minute interval", 
     ylab="Average number of steps taken",
     main="Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_by_interval[average_by_interval$steps==max(average_by_interval$steps),]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values
# Total number of missing values in the dataset

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
# Fill NA's with average for that 5-min interval

```r
merged_data <- arrange(join(data, average_by_interval), interval)
```

```
## Joining by: steps, interval
```
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
merged_data$steps[is.na(merged_data$steps)] <- merged_data$intervalAvg[is.na(merged_data$steps)]
```

```
## Error: replacement has length zero
```
# Histogram

```r
new_total_by_day <- ddply(merged_data, .(date), summarise, steps=sum(steps))
hist(new_total_by_day$steps, main="Number of Steps", 
     xlab="Total number of steps taken each day", col="light blue",)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
# mean and median total number of steps taken per day

```r
mean(new_total_by_day$steps)
```

```
## [1] NA
```

```r
median(new_total_by_day$steps)
```

```
## [1] NA
```

```r
total_steps1 <- sum(data_clean$steps)
total_steps2 <- sum(merged_data$steps)
total_steps2 -total_steps1
```

```
## [1] NA
```
## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
weekdays <- weekdays(as.Date(merged_data$date))
data_with_weekdays <- transform(merged_data, day=weekdays)
data_with_weekdays$wk <- ifelse(data_with_weekdays$day %in% c("zaterdag", "zondag"),"weekend", "weekday")
average_by_interval_wk <- ddply(data_with_weekdays, .(interval, wk), summarise, steps=mean(steps))

xyplot(steps ~ interval | wk, data = average_by_interval_wk, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
