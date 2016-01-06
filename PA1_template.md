# Reproducible Research: Peer Assessment 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Data For the Analysis

The data can be downloaded from the course web site:   
Dataset: Activity Monitoring Data [52K]  

The variables included in this dataset are:   
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)    
- date: The date on which the measurement was taken in YYYY-MM-DD format    
- interval: Identifier for the 5-minute interval in which measurement was taken      
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data
1. Basic settings

```r
echo = TRUE  # Always make code visible
warning = FALSE # Remove warning messages
options(scipen = 1)  # Turn off scientific notations for numbers
```

2. Download and unzip the activity.csv file into the working directory before reading the data.

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

3. Transform the date to the right format

```r
data$month <- as.numeric(format(data$date, "%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(noNA)
```

```
## [1] 15264     4
```

4. Load the relevant libraries.

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.2
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
total_step <- aggregate(noNA$steps, list(Date=noNA$date), FUN = "sum")$x
head(total_step)
```

```
## [1]   126 11352 12116 13294 15420 11015
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7)+ facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](RepData_files/figure-html/unnamed-chunk-6-1.png) 

3. Calculate and report the mean and median for total number of steps taken per day.

```r
mean(total_step)
```

```
## [1] 10766.19
```


```r
median(total_step)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
avg_step <- aggregate(noNA$steps, list(interval= as.numeric(as.character(noNA$interval))), FUN="mean")
names(avg_step)[2] <- "MeanOfSteps"

ggplot(avg_step, aes(interval, MeanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](RepData_files/figure-html/unnamed-chunk-9-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avg_step[avg_step$MeanOfSteps == max(avg_step$MeanOfSteps), ]
```

```
##     interval MeanOfSteps
## 104      835    206.1698
```

The 835-th 5-minute interval contains the maximum number of steps.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
sum(is.na(data))
```

```
## [1] 2304
```

There are 2304 missing values in the dataset.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Here I use the mean of 5-minute interval to fill in the values of the missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_data <- data # new dataset called new_data
for (i in 1:nrow(new_data)) {
    if (is.na(new_data$steps[i])) {
        new_data$steps[i] <- avg_step[which(new_data$interval[i] == avg_step$interval), ]$MeanOfSteps
    }
}
head(new_data) # no NAs
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```


```r
sum(is.na(new_data)) # should be 0
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
ggplot(new_data, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") +labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x ="Date", y = "Total number of steps")
```

![](RepData_files/figure-html/unnamed-chunk-14-1.png) 


```r
new_total_step <- aggregate(new_data$steps, list(Date = new_data$date),FUN = "sum")$x
 
new_mean <- mean(new_total_step)
```


```r
new_median <- median(new_total_step)
```

Compare the new and old mean and median


```r
old_mean <- mean(total_step)
old_median <- median(total_step)
Diff_Mean <- new_mean - old_mean
Diff_Median <- new_median - old_median
Diff_Mean
```

```
## [1] 0
```

```r
Diff_Median
```

```
## [1] 1.188679
```

The new mean is the same as the old mean from the first part of the assignment, but the new median is not, although their values are close. Imputing missing data using the average of the 5-minute interval results in more data points equal to the mean and smaller variation of the distribution. Since many data points have the same values as the mean, the median is much likely to be the same as the mean as well.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
head(new_data)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
new_data$weekdays <- factor(format(new_data$date, "%A"))
levels(new_data$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(new_data$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", "Thursday","Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(new_data$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(new_data$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avg_step <- aggregate(new_data$steps, list(interval = as.numeric(as.character(new_data$interval)), weekdays = new_data$weekdays), FUN = "mean")
names(avg_step)[3] <- "MeanOfSteps"
xyplot(avg_step$MeanOfSteps ~ avg_step$interval | avg_step$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps" ,
       main = "Average Number of Steps Taken (across all weekday or weekend)")
```

![](RepData_files/figure-html/unnamed-chunk-19-1.png) 
