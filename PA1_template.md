# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. `read.csv()`)
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
##setwd('D:\\rvillegas\\Mio\\coursera\\rr\\wd\\pa1\\RepData_PeerAssessment1')
#unzip and read data
library(dplyr)
library(ggplot2)
data <- read.csv(unz("activity.zip", "activity.csv"))
```
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
dt<-data[(!is.na(data$steps)),]
stepsxday<-summarize(group_by(dt,date),sum(steps))
colnames(stepsxday)=c("date","steps")

binsize <- diff(range(stepsxday$steps))/20
ggplot(stepsxday, aes(x=steps)) +
  geom_histogram(binwidth=binsize, fill="white", colour="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
mean_first<-mean(stepsxday$steps)
mean_first
```

```
## [1] 10766.19
```


```r
median_first<-median(stepsxday$steps)
median_first
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stpsxtime<-summarize(group_by(dt,interval),mean(steps))
colnames(stpsxtime)=c("Interval","steps")
ggplot(stpsxtime, aes(x=Interval, y=steps)) + geom_line(colour="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stpsxtime_max<-summarize(group_by(dt,interval),max(steps))
colnames(stpsxtime_max)=c("interval","steps")
ggplot(stpsxtime_max, aes(x=interval, y=steps)) + geom_line(colour="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
t_datos<-dim(data)[1]
t_NA<-sum(is.na(data$steps))
```
There are 2304 missing values.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_data<-data
for (i in 1:nrow(new_data))
 {
     if (is.na(new_data[i,1])) {
         new_data[i,1]<-stpsxtime[(stpsxtime$Interval==new_data[i,3]),2]
     }
 }
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dt<-new_data
stepsxday<-summarize(group_by(dt,date),sum(steps))
colnames(stepsxday)=c("date","steps")

binsize <- diff(range(stepsxday$steps))/20
ggplot(stepsxday, aes(x=steps)) +
  geom_histogram(binwidth=binsize, fill="white", colour="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


```r
mean_second<-mean(stepsxday$steps)
mean_second
```

```
## [1] 10766.19
```


```r
median_second<-median(stepsxday$steps)
median_second
```

```
## [1] 10766.19
```

The mean do not change 1.0766189\times 10^{4} = 1.0766189\times 10^{4}, the median is a little different 10765 != 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?


1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
typeofDay<-vector()
typeofDay<-as.factor(ifelse(as.POSIXlt(new_data$date)$wday %in% c(0,6),"weekend","weekday"))
new_data<-cbind(new_data,typeofDay)
stpsxtime<-summarize(group_by(new_data,interval,typeofDay),mean(steps))
colnames(stpsxtime)[3]="steps"
```


2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using 



```r
 ggplot(stpsxtime, aes(x=interval, y=steps)) + geom_line(colour="blue")+ facet_wrap(~ typeofDay , ncol=1)+theme(strip.text = element_text(size=rel(1.5)),strip.background = element_rect(fill="pink",size=2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

