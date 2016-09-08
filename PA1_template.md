# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
First, we'll load required packages (dplyr and lattice), and then unzip and read our data.


```r
library(dplyr)
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
library(lattice)
unzip("activity.zip")
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
First, we will calculate the total number of steps taken per day:


```r
activity_summary <- activity %>% 
                      group_by(date) %>% 
                      summarize(total_steps = sum(steps))
```

Next, let's make a histogram of the total number of steps taken each day:


```r
hist(activity_summary$total_steps, 
      breaks = seq(0,25000, by=2500), 
      ylim = c(0,20), 
      main = "Steps per day", 
      xlab = "Steps")
```

![](PA1_template_files/figure-html/Q1:steps histogram-1.png)<!-- -->

Finally, let's calculate and report the mean and median of the total number of steps taken per day.

```r
print(mean_daily_steps <- mean(activity_summary$total_steps, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
print(median_daily_steps <- median(activity_summary$total_steps, na.rm = TRUE))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
For the next question, we want to understand average daily activity. As I was doing this analysis, I realized that the interval notation here is rather strange: although ostensibly every 5 min, it's actually in the format hmm; that is, 155 means 1:55 and the next interval is actually 200 (2:00) rather than 160. This manifests itself in hourly gaps in the xyplot. If we REALLY want to look at activity across a day, we should recalculate a "proper" interval to get rid of these meaningless gaps and then plot the data to see activity across the average day.


```r
proper_interval = seq(0,1435,by = 5)
activity <- cbind(activity,proper_interval)
interval_summary <- activity %>%
                      group_by(interval,proper_interval) %>%
                      summarize(mean_steps = mean(steps, na.rm = TRUE)) 
                      
with(interval_summary, plot(proper_interval, mean_steps, type = "l", ylab = ("Mean steps taken")))
```

![](PA1_template_files/figure-html/Q2:daily activity-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
This can be answered 2 ways - either as the original interval reading or as our "proper" recomputed interval. Both are shown here.

```r
print(max_original_interval <- interval_summary$interval[which.max(interval_summary$mean_steps)])
```

```
## [1] 835
```

```r
print(max_proper_interval <- interval_summary$proper_interval[which.max(interval_summary$mean_steps)])
```

```
## [1] 515
```

## Imputing missing values
The assigment states: "Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data."

Let's calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
print(total_rows_with_NA <- sum(!complete.cases(activity)))
```

```
## [1] 2304
```

Here, I've decided to impute the values based on the average of that 5 minute interval across the entire period.

```r
impute_NA <- function(x,proper_interval) ifelse(is.na(x), interval_summary$mean_steps[(proper_interval/5)+1],x) 

activity_imputed <- activity %>%
                mutate(imputed_steps = impute_NA(steps,proper_interval))
```

Note that by doing this imputation strategy the we mitigate any differences that might occur in mean/median values as shown here (although of course, we'll see more days with a given number of steps in the histogram since NAs have been removed)


```r
imputed_activity_summary <- activity_imputed %>% 
                      group_by(date) %>% 
                      summarize(total_imputed_steps = sum(imputed_steps))

hist(imputed_activity_summary$total_imputed_steps, 
      breaks = seq(0,25000, by=2500), 
      ylim = c(0,30), 
      main = "Steps per day", 
      xlab = "Steps")
```

![](PA1_template_files/figure-html/Q3: hist mean and median-1.png)<!-- -->

```r
print(mean_daily_steps <- mean(imputed_activity_summary$total_imputed_steps, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
print(median_daily_steps <- median(imputed_activity_summary$total_imputed_steps, na.rm = TRUE))
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
Finally, we want to see differences in activity patterns between weekdays and weekends.


```r
activity_imputed$day_of_week <- weekdays(activity_imputed$date)

is_weekend_day <- function(x) factor(as.integer(x == "Saturday" | x == "Sunday"), labels = c("Weekday", "Weekend"))

week_vs_weekend_summary <- activity_imputed %>%
                              mutate(part_of_week = is_weekend_day(day_of_week)) %>%
                              group_by(part_of_week,proper_interval) %>%
                              summarize(mean_steps = mean(imputed_steps, na.rm = TRUE))
                              
xyplot(mean_steps ~ proper_interval | part_of_week, week_vs_weekend_summary, type = "l", layout = c(1,2), xlab = "5 minute interval", ylab = "Mean Number of Steps")
```

![](PA1_template_files/figure-html/Q4: weekends-1.png)<!-- -->

## Thank you!!
