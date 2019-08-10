# Reproducible Research: Peer-graded Assignment - Course Project 1
##### Rohan Thite
##### 2019 Aug 8

### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Question 1

Loading and preprocessing the data
Show any code that is needed to

* Load the data (i.e. read.csv())

* Process/transform the data (if necessary) into a format suitable for your analysis


```r
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
data <- read.csv('activity.csv')
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

## Question 2

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsPerDay <- aggregate(steps~date,data,sum)
hist(stepsPerDay$steps,xlab = 'Number of steps', ylab='Number of days', main='Days vs Steps',breaks = 50,col='dark gray')
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" width="672" />


```r
stepsMean <-mean(stepsPerDay$steps)

stepsMedian <-median(stepsPerDay$steps)

# mean  of the total number of steps taken per day
stepsMean
```

```
## [1] 10766.19
```

```r
# median of the total number of steps taken per day
stepsMedian
```

```
## [1] 10765
```

# Question 3

What is the average daily activity pattern?

1. Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
library(ggplot2)
stepsPerInterval <- aggregate(steps~interval,data,mean)

ggplot(data=stepsPerInterval, aes(x=interval, y=steps)) +
  geom_line() +
  ggtitle("Time Series: average number of steps") +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-13-1.png" width="672" />


```r
#maximum steps for an interval averaged acrossed all the days

stepsPerInterval[which.max(stepsPerInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


# Question 4

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of missing values in dataset

sum(is.na(data$steps))
```

```
## [1] 2304
```


```r
# create meansteps accross intervals

meaninterval<- aggregate(steps ~ interval,data, FUN=mean)

# merge data and meansteps data frames

dataNew <- merge(x=data, y=meaninterval, by="interval")


# replace NA value with the mean steps for that perticalar interval 


dataNew$steps <- ifelse(is.na(dataNew$steps.x), dataNew$steps.y, dataNew$steps.x)

dataNew <- dataNew[,c(5,3,1)]

aggsteps_new<- aggregate(steps ~ date, dataNew, FUN=sum)
par(mfrow=c(1,2))
hist(aggsteps_new$steps, 
     col="green",
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(After imputing NA values with \n mean of 5-min interval)",
     cex.main = 0.7)

#Histogram with the orginal dataset
hist(stepsPerDay$steps, 
     col="red", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(Orginal Dataset)",
     cex.main = 0.7)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" width="672" />

```r
par(mfrow=c(1,1)) #Resetting the panel

amean_new <- mean(aggsteps_new$steps)
amedian_new <- median(aggsteps_new$steps)

#Comparing Means
paste("New Mean      :", round(amean_new,2), "," ,  
      " Original Mean :", round(stepsMean,2),"," , 
      " Difference :",round(amean_new,2) -  round(stepsMean,2))
```

```
## [1] "New Mean      : 10766.19 ,  Original Mean : 10766.19 ,  Difference : 0"
```

```r
#Comparing Medians
paste("New Median    :", amedian_new, ",", 
      " Original Median :", stepsMedian,"," , 
      " Difference :",round(amedian_new-stepsMedian,2))
```

```
## [1] "New Median    : 10766.1886792453 ,  Original Median : 10765 ,  Difference : 1.19"
```

The Mean are same but New Median differs from Original Median by 1.19


# Question 5

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
#install.packages('chron')
library(chron)


table(is.weekend(dataNew$date))
```

```
## 
## FALSE  TRUE 
## 12960  4608
```

```r
#Adding new factor variable "dayofweek" indicating whether a given date is a weekday or weekend day
dataNew$dayofweek <- ifelse(is.weekend(dataNew$date), "weekend", "weekday")

#Number of Weekdays and Weekends
table(dataNew$dayofweek)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
#New Data after adding factor variable for weekday or weekend
head(dataNew)
```

```
##      steps       date interval dayofweek
## 1 1.716981 2012-10-01        0   weekday
## 2 0.000000 2012-11-23        0   weekday
## 3 0.000000 2012-10-28        0   weekend
## 4 0.000000 2012-11-06        0   weekday
## 5 0.000000 2012-11-24        0   weekend
## 6 0.000000 2012-11-15        0   weekday
```

```r
#Aggregating(mean) steps over interval and day of week
meaninterval_new<- aggregate(steps ~ interval + dayofweek, dataNew, FUN=mean)

#Aggregated Data
head(meaninterval_new)
```

```
##   interval dayofweek      steps
## 1        0   weekday 2.25115304
## 2        5   weekday 0.44528302
## 3       10   weekday 0.17316562
## 4       15   weekday 0.19790356
## 5       20   weekday 0.09895178
## 6       25   weekday 1.59035639
```

```r
#Time Series plot using ggplot
ggplot(meaninterval_new, aes(x=interval, y=steps)) + 
  geom_line(color="blue", size=1) + 
  facet_wrap(~dayofweek, nrow=2) +
  labs(x="\nInterval", y="\nNumber of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" width="672" />

