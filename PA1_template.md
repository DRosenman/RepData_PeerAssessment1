# Reproducible Research: Peer Assessment 1

```r
library(tidyverse)
library(knitr)
```


## Loading and preprocessing the data
### Extracting 'activity.csv' from 'activity.zip'

```r
unzip('activity.zip')
```
### Loading 'activity.csv' as 'activity' DataFrame

```r
activity <- readr::read_csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
total_steps_each_day <- activity %>% group_by(date) %>% summarize(Total_Steps = sum(steps, na.rm = TRUE))
mean_steps <- mean(total_steps_each_day$Total_Steps)
median_steps <- median(total_steps_each_day$Total_Steps)
steps_df <- data.frame(Mean.Steps= c(round(mean_steps,0)),
                       Median.Steps = c(median_steps))
steps_df
```

```
##   Mean.Steps Median.Steps
## 1       9354        10395
```


```r
ggplot(data = total_steps_each_day, aes(Total_Steps)) + 
  geom_histogram(bins = 20, fill = 'blue', color = 'black') + ylab('Steps') + ggtitle('Distribution of Steps Taken Per Day') + scale_x_continuous(breaks = seq(from = 0, to = 22000, by = 2000))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



## What is the average daily activity pattern?

```r
interval_averages <- activity %>% group_by(interval) %>% summarize(Average.Steps = mean(steps,na.rm = TRUE))
ggplot(data = interval_averages, aes(x = interval, y = Average.Steps)) + 
      geom_point() +
      geom_line() +
      ggtitle('Activity Pattern Time Series')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
inter_max_activity <- interval_averages[which.max(interval_averages$Average.Steps),'interval']
average_steps_max_interval <- max(interval_averages$Average.Steps)
data.frame(Max.Activitiy.Interval = inter_max_activity, Average_Steps = round(average_steps_max_interval,0))
```

```
##   interval Average_Steps
## 1      835           206
```



## Imputing missing values

```r
missing <- activity %>% filter(is.na(steps))
num_missing_intervals <- nrow(missing)
print(paste('Number of missing values = ',num_missing_intervals))
```

```
## [1] "Number of missing values =  2304"
```

### Strategy for Dealing With Missing Values
I am going to replace missing values the number 0.


```r
data_filled <- activity
data_filled <- data_filled %>% mutate(steps= ifelse(is.na(steps),0,steps))
total_steps_filled <- data_filled %>% group_by(date) %>% summarize(Total_Steps = sum(steps))
mean_steps_filled <- mean(total_steps_filled$Total_Steps)
median_steps_filled <- median(total_steps_filled$Total_Steps)
steps_df_filled <- data.frame(Mean.Steps= c(round(mean_steps_filled,0)),
                       Median.Steps = c(median_steps_filled))
steps_df_filled
```

```
##   Mean.Steps Median.Steps
## 1       9354        10395
```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Replacing the missing values with 0 has no impact on the total number of steps each day and the mean and median total number of steps taken per day.


```r
ggplot(data = total_steps_filled, aes(Total_Steps)) + 
  geom_histogram(bins = 20, fill = 'blue', color = 'black') + ylab('Steps') + ggtitle('Distribution of Steps Taken Per Day') + scale_x_continuous(breaks = seq(from = 0, to = 22000, by = 2000))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
activity$day <- weekdays(activity$date)
activity_weekdays <-activity %>% filter((day != 'Sunday') & day != 'Saturday')
activity_weekends <- activity %>% filter((day == 'Sunday') | (day == 'Saturday'))
weekday_interval_averages <- activity_weekdays %>% group_by(interval,) %>% summarize(Average_Steps = mean(steps,na.rm = TRUE))
weekend_interval_averages <- activity_weekends %>% group_by(interval) %>% summarize(Average_Steps = mean(steps,na.rm = TRUE))
```


```r
activity_by_part_of_week <- activity %>% mutate(Part_of_Week = ifelse(((weekdays(date) != 'Sunday') & 
                                                                        (weekdays(date) != 'Saturday')),'Weekday','Weekend'))
activity_by_part_of_week_by_interval <- activity_by_part_of_week %>% group_by(interval,Part_of_Week) %>% summarize(Average_Steps = mean(steps,na.rm = TRUE)) 

ggplot(data = activity_by_part_of_week_by_interval, aes(x = interval, y = Average_Steps,color = Part_of_Week)) +
  geom_point() +
  geom_line() +
  ylab('Average Steps') +
  ggtitle('Weekends Vs Weekdays')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
The activity for each interval seems to be very similar for both weekends and weekdays, with slightly greater activity during the weekend.


```r
activity_by_part_of_week_comparison <- activity_by_part_of_week %>% group_by(Part_of_Week) %>% summarize(Mean_Steps = mean(steps,na.rm = TRUE))
activity_by_part_of_week_comparison
```

```
## # A tibble: 2 x 2
##   Part_of_Week Mean_Steps
##          <chr>      <dbl>
## 1      Weekday   35.33796
## 2      Weekend   43.07837
```
