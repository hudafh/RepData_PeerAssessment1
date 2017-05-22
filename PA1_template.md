# Project 1
study <- function() {

1. Code for reading in the dataset and/or processing the data


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
        activityData <- read.csv("./activity.csv")
        
        daily <- summarise(group_by(activityData, date,interval,steps))
        colnames(daily) <- c("date", "interval","steps")
        
        original_df <- daily
        #BEGINING
        nrNAs <- length(is.na(daily))
        
        noNAs <- !is.na(daily[,"steps"])
        
        daily <- daily[noNAs,]
```
        
2. Histogram of the total number of steps each day


```r
        total_steps_by_day <- summarise( group_by(daily,date), sum(steps))
        
        colnames(total_steps_by_day) <- c("date", "steps")
        #total_steps_by_day
        
        days <- as.POSIXct(total_steps_by_day$date, format = '%Y-%m-%d')
        
        hist(total_steps_by_day$steps,xlab = "Total Steps by Day",main = "Total number of Steps each day" , col = "red")  #plot histogram
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
        
3. Mean and median number of steps taken each day        


```r
        avg_steps_day <- mean(total_steps_by_day$steps)
        
        median_steps_day <- median(total_steps_by_day$steps)
        print(avg_steps_day)
```

```
## [1] 10766.19
```

```r
        print(median_steps_day)
```

```
## [1] 10765
```
        
        
4. Time Series plot of the average number of steps taken


```r
        interval_avg <- summarise(group_by(daily,interval), mean(steps))
        colnames(interval_avg) <- c("interval","steps")
        plot(interval_avg$interval,as.numeric(interval_avg$steps), type = "l", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
        
5. The 5-minute interval that, on average, contains the maximum number of steps


```r
        highest_mean_interval <- as.integer(interval_avg[interval_avg$steps==max(interval_avg$steps),"interval"])
        
        highest_mean_interval
```

```
## [1] 835
```
        
6. Code to describe and show a strategy for inputting missing data


```r
        incompleteCases <- original_df[!complete.cases(original_df),]   
        
        incompleteCases[,"steps"]<-interval_avg$steps # to each interval with missing value, assign the average for that interval on dates where they are                                                                #available
        
        complete_df <- rbind(daily,incompleteCases)  # subset of the original dataset but without missing values + values that were NA on the original dataset
        
        
        complete_df <- summarise(group_by(complete_df,date,interval,steps))  #dataset equal to the original but with the missing data filled in
```
        
        
7. Histogram of the total number of steps taken each day after missing values are inputed


```r
        complete_df_daily_steps <- summarise(group_by(complete_df,date),sum(steps))
        colnames(complete_df_daily_steps) <- c("date","steps")
        
        hist(complete_df_daily_steps$steps, col = "red",xlab = "Steps",main = "Total number of Steps each day(Complete Dataset)")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
        library(ggplot2)
        complete_df$weekday <-weekdays(as.Date(complete_df$date, format = '%Y-%m-%d'))
        
        complete_df$dayOfWeek <- "Weekday"
        
        complete_df$dayOfWeek[complete_df$weekday %in% c("Saturday","Sunday")] <- "Weekend"
        
        complete_df$dayOfWeek <- as.factor(complete_df$dayOfWeek)
        
        complete_df_plot <- summarise(group_by(complete_df,interval,dayOfWeek),mean(steps)) #Starting to prepare for last plot
       
        colnames(complete_df_plot) <- c("interval","dayOfWeek","avgSteps")
       
        ggplot(complete_df_plot, aes(interval,avgSteps)) + geom_line() +facet_grid(dayOfWeek~.) + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
        
        
        
        
        
        
     
