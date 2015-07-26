---
title: "RR_Peer_Assignment1"
author: "Charles F."
date: "Saturday, December 13, 2014"
output:
  html_document:
    fig_height: 6
    fig_width: 8
---


```r
library(lubridate)
library(dplyr)
library(ggplot2)
library(lattice)
```

#Loading and preprocessing the data
##Load the data (i.e. read.csv())

```r
getfile <- tempfile()
download.file ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" , getfile)
ac = read.csv(unz(getfile, "activity.csv"))
unlink(getfile)
```

##Process/transform the data (if necessary) into a format suitable for your analysis

```r
str(ac) ; head(ac)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
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
ac$Date = as.Date(ac$date)
ac$date = NULL
```

Changing date to actual date as was implied in the assignment
I could transfrom the interval as a massive factor variable but decided to leave as is for this analysis. Steps are numberic so they are fine.

##What is mean total number of steps taken per day?

```r
ac_days = ac %>% group_by(Date) %>% 
 summarize( D_steps = sum(steps, na.rm=T))

mean(ac_days$D_steps)
```

```
## [1] 9354.23
```

I take the total steps for each day and then take the overall mean of the summed steps.

##Make a histogram of the total number of steps taken each day

```r
ggplot(ac_days, aes(x=D_steps)) + geom_histogram(bindwidth=1 , colour="black", fill="orange" ,  position="identity") +
 geom_vline(aes(xintercept=mean(D_steps, na.rm=T)),  color="black", linetype="dashed", size=1) +
 geom_vline(aes(xintercept=median(D_steps, na.rm=T)),  color="blue", linetype="dashed", size=1) +
  xlab("Steps Per Day") + ylab("Count") + ggtitle("Steps Histogram") +
  theme_bw()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk Firsthist](figure/Firsthist-1.png) 

##Calculate and report the mean and median total number of steps taken per day

The median and mean are colored in the above histogram as well.
The mean is colored in black and the median is colored in blue
Here they are in actual hard numbers

```r
mean(ac_days$D_steps)
```

```
## [1] 9354.23
```

```r
median(ac_days$D_steps)
```

```
## [1] 10395
```

It appears that the median is slightly higer than mean implying that the data is skewed, as the histogram confrims.

#What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
time = ac %>% select(interval, steps ) %>% group_by (interval) %>%
  summarize( Avg_Steps = mean(steps , na.rm=T)) %>%
  arrange(desc(interval)) 

ggplot(data=time, aes(x=interval, y=Avg_Steps , file="blue" )) + 
    geom_line( size=1.2) +                      
    xlab("5 Minute Interval") + ylab("Average Steps Taken") + 
    ggtitle("Average Activity Pattern By Internal") + 
    theme_bw() 
```

![plot of chunk Ts_int](figure/Ts_int-1.png) 

The above chart has the 5 Minute Interval as the horizontal axis and the Average steps per day as the vertical axis. Can't wait for Rcharts to make labeling easier.


##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Max_num = max(time$Avg_Steps)
Max_num
```

```
## [1] 206.1698
```

```r
time %>% filter( Avg_Steps ==  Max_num ) %>% select(interval)
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

The interval with the higest value is at 835.

#Imputing missing values

##Calculate and report the total number of missing values in the dataset

```r
sum(is.na(ac))
```

```
## [1] 2304
```

2304 or 13% of the data has missing values.

###Devise a strategy for filling in all of the missing values in the dataset. 
###Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
ac2 = ac %>% select(Date, interval, steps) %>%
  group_by(Date, interval) %>% 
  summarize( D_steps = sum(steps, na.rm=T)) %>%
  select(D_steps, interval, Date) %>% 
  rename(steps=D_steps)

miss_val = merge(ac2, time , by ="interval", sort=F )
miss_val$Steps = ifelse( miss_val$steps == 0 , miss_val$Avg_Steps, miss_val$steps)
miss_val$steps = NULL ; miss_val$Avg_Steps = NULL;

head(miss_val) ; head(ac)
```

```
##   interval       Date    Steps
## 1        0 2012-10-01 1.716981
## 2        0 2012-11-23 1.716981
## 3        0 2012-10-28 1.716981
## 4        0 2012-11-06 1.716981
## 5        0 2012-11-24 1.716981
## 6        0 2012-11-15 1.716981
```

```
##   steps interval       Date
## 1    NA        0 2012-10-01
## 2    NA        5 2012-10-01
## 3    NA       10 2012-10-01
## 4    NA       15 2012-10-01
## 5    NA       20 2012-10-01
## 6    NA       25 2012-10-01
```

```r
sum(is.na(miss_val))
```

```
## [1] 0
```

The dplyr takes the orginal data set and rolls it up by the Date and interval. It then calculates the sum and renames that to the orginal step name. Values are then imputed when the are equal to missing, in this case 0, with the interval average generated from the time plot.

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newhist = miss_val %>% group_by(Date) %>% 
  summarize( D_steps = sum(Steps, na.rm=T)) 

ggplot(as.data.frame(newhist) , aes(x=D_steps)) + geom_histogram(bindwidth=1 , colour="black", fill="orange" ,  position="identity") +
  xlab("Steps Per Day") + ylab("Count") + ggtitle("Steps Histogram") + theme_bw()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk Newhist](figure/Newhist-1.png) 

```r
mean(newhist$D_steps, na.rm=T) ; median(newhist$D_steps, na.rm=T)
```

```
## [1] 15875.99
```

```
## [1] 15837.74
```

```r
mean(ac_days$D_steps, na.rm=T) ; median(ac_days$D_steps, na.rm=T)
```

```
## [1] 9354.23
```

```
## [1] 10395
```

The mean drastically increased whem compared to the data set that was not imputed. There is now a drastic increase the in the overall steps per day because previous values were excluded from the totals.
There is almost a 5000 increase in the median.


#Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 

```r
miss_val$Wk = weekdays(miss_val$Date)
#table(miss_val$Wk)
miss_val$Wkday = factor(ifelse(miss_val$Wk %in% c("Saturday" , "Sunday"), "Weekend", "Weekday"))
#table(miss_val$Wk , miss_val$Wkday)
```

Saturday and Sunday are now classfied as the Weekend. Overall, the measurements for the 7 days of tthe week - when looking at record count - appear to be even.


```r
Wks = miss_val %>% select(Wkday, interval, Steps) %>% 
  group_by(Wkday, interval) %>%
  summarize( W_steps = mean(Steps, na.rm=T)) 
  
xyplot( W_steps~ interval | Wkday, data=Wks, layout=c(1,2), 
        type="l",    xlab = "Interval",  ylab = "Number of steps")
```

![plot of chunk Weeks_plot](figure/Weeks_plot-1.png) 

Based on the above, I see that weekdays have spike in the 800 to 900 interval range.
Weekends appear to be absent of that, and what's more look like more steps are occuring overall.

