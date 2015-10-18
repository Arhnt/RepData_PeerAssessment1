# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?
Count total number of steps per day

```r
total_steps_per_day <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```

And create a histogram for total number of steps per day

```r
hist(total_steps_per_day, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Useful inforamtion about total steps per day

```r
summary(total_steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is the average daily activity pattern?
Average steps by interval

```r
average_steps_by_interval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
```

And build a line chart by time interval with red line which indicates max value

```r
plot(names(average_steps_by_interval), average_steps_by_interval, type="l", xlab = "Time Interval", ylab = "Steps", main = "Average Steps by Daily Time Interval")
abline(v = names(which.max(average_steps_by_interval)), col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Interval with max value corresponds to next time

```r
max_interval = which.max(average_steps_by_interval)
max_interval
```

```
## 835 
## 104
```

```r
sub(pattern = "([0-9]{2})([0-9]{2})",
    replacement = "\\1:\\2",
    x = sprintf("%04s", names(max_interval)))
```

```
## [1] "08:35"
```

## Imputing missing values

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
There are NA's   :2304   missing values in steps.

I will use 5-minutes mean to fill in missed values.

To replace missing values with means - create array of mean values which corresponds to original array and use **is.na()** function as index to replace missing values only

```r
mean_data <- data
mean_steps <- rep_len(average_steps_by_interval, length(data$steps))
mean_data$steps[is.na(data$steps)] <- mean_steps[is.na(data$steps)]
```

There are head-s

```r
head(data)
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
head(mean_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
head(average_steps_by_interval)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

 and tail-s for used datasets

```r
tail(data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
tail(mean_data)
```

```
##           steps       date interval
## 17563 2.6037736 2012-11-30     2330
## 17564 4.6981132 2012-11-30     2335
## 17565 3.3018868 2012-11-30     2340
## 17566 0.6415094 2012-11-30     2345
## 17567 0.2264151 2012-11-30     2350
## 17568 1.0754717 2012-11-30     2355
```

```r
tail(average_steps_by_interval)
```

```
##      2330      2335      2340      2345      2350      2355 
## 2.6037736 4.6981132 3.3018868 0.6415094 0.2264151 1.0754717
```

Count total number of mean steps per day

```r
total_mean_steps_per_day <- tapply(mean_data$steps, mean_data$date, sum, na.rm = TRUE)
```

And create a histogram for total number of mean steps per day

```r
hist(total_mean_steps_per_day, xlab = "Total Steps per Day", main = "Histogram of Total Mean Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Useful inforamtion about total mean steps per day

```r
summary(total_mean_steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

Histogramm looks similary to the original one, except the first bucket which is much smaller.
3rd quantille in new data is the same, median is almost the same as in original data.weekda

## Are there differences in activity patterns between weekdays and weekends?

Mutate date column from character to Date and add a new factor column which identified weekdays or weekends

```r
mean_data$date <- as.Date(mean_data$date)
mean_data$weekday <- factor(rep_len("weekday", length(mean_data$date)), levels = c("weekday", "weekend"));
weekdays <- weekdays(mean_data$date);
mean_data$weekday[weekdays == "Sunday" | weekdays == "Saturday"] <- "weekend"
```

Lets validate results

```r
table(mean_data$weekday)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
table(weekdays(mean_data$date))
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##      2592      2592      2304      2304      2592      2592      2592
```

And finally build a plot for daily activity for weekdays and weekends, I will use ggplot2 as graphics system and dplyr for groupping

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

by_interval_weekday <- group_by(mean_data, interval, weekday) %>%
                       summarize(steps = mean(steps))

qplot(x = interval, y = steps, geom = "line", data = by_interval_weekday, facets = weekday ~ . )
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 
