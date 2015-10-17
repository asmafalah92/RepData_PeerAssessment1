---
output: html_document
---
# Peer Assessment 1
==================
## R markdwon file

## *Regarding the assignment instructions please see* [**this**](https://github.com/asmafalah92/RepData_PeerAssessment1/blob/master/README.md)

# Data

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
- The variables included in this dataset are:

1- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

2- **date**: The date on which the measurement was taken in YYYY-MM-DD format

3- **interval**: Identifier for the 5-minute interval in which measurement was taken

- The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Setting the global options (echo = TRUE is a requirement)


```r
library(knitr)
opts_chunk$set ( echo = TRUE , results = "asis" )
```

## Loading and preprocessing the data

### 1. Load the data (i.e. read.csv())
Read the data, the file name is "activity" and the type is ".csv"


```r
my_data <- read.csv('activity.csv', header = TRUE, sep = ",", colClasses = c("numeric", "character", "numeric"))
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis

data preprocessing, interval is converted to integer insrtead of numeric class, date to date instead of charecter class.


```r
my_data$interval <- as.factor(my_data$interval)
my_data$date <- as.Date(my_data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day



```r
steps1 <- aggregate(steps ~ date, my_data, sum)
colnames(steps1) <- c("date","steps")
str(steps1)
```

'data.frame':	53 obs. of  2 variables:
 $ date : Date, format: "2012-10-02" "2012-10-03" ...
 $ steps: num  126 11352 12116 13294 15420 ...

### Make a histogram of the total number of steps taken each day


```r
library (ggplot2)
ggplot(steps1 , aes(x = steps)) + geom_histogram(color = "black", fill = "red", binwidth = 1000) + labs(title="Steps Taken per Day Plot", x = "Steps per day", y = "Times per day") + theme_grey()
```

![plot of chunk plotting1](figure/plotting1-1.png?raw=true) 

### Calculate and report the mean and median of the total number of steps taken per day


```r
themean   <- mean(steps1$steps, na.rm = T)
themedian <- median(steps1$steps, na.rm = T)
themean
```

[1] 10766.19

```r
themedian
```

[1] 10765

## Are there differences in activity patterns between weekdays and weekends?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps2 <- aggregate(my_data$steps, by = list(interval = my_data$interval), FUN=mean, na.rm=TRUE)
steps2$interval <- as.integer(levels(steps2$interval)[steps2$interval])
colnames(steps2) <- c("interval", "steps")
```

- Plot: number of steps and interval


```r
library(ggplot2)
ggplot(steps2, aes(x=interval, y=steps)) + geom_line(color="purple", size= 2 ) + labs(title="Average daily activity pattern", x ="5 minute interval", y = "average No. of steps taken") + theme_grey()
```

![plot of chunk plotting 2](figure/plotting 2-1.png?raw=true) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

- 5 minute interval with the maximum number of steps


```r
maximumsteps <- steps2[which.max(steps2$steps),]
maximumsteps
```

    interval    steps
104      835 206.1698

## Imputing Missing Values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

- Creating a logical vector of TRUE for every NA and finding the sum of those since TRUE = 1


```r
numberofNAs <- sum(is.na(my_data$steps))
numberofNAs
```

[1] 2304

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Replacing the NAs

we replace them with the mean by creating a function replace (data, byinterval) which the data arguement is the my_data variable , and byinterval arguement is steps2 variable.

###Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
replaceNA <- function(data, byinterval) {
        NAs <- which(is.na(data$steps))
        replacing <- unlist(lapply(NAs, FUN=function(idx){
                interval = data[idx,]$interval
                byinterval[byinterval$interval == interval,]$steps
        }))
        newsteps <- data$steps
        newsteps[NAs] <- replacing
        newsteps
}

my_data2 <- data.frame(  
        steps = replaceNA(my_data, steps2),  
        date = my_data$date,  
        interval = my_data$interval)
str(my_data2)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" ...
 $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

###Make a histogram of the total number of steps taken each day 

- plotting a histogram of the total number of steps taken each day

using the most recently created data frame


```r
newdata <- aggregate(steps ~ date, my_data2, sum)
colnames(newdata) <- c("date","steps")
```

- plotting the histogram


```r
library(ggplot2)
ggplot(newdata, aes(x = steps)) +  geom_histogram(color = "black" , fill = "yellow", binwidth = 1000) +  labs(title="Plot: Total number of steps taken each day",  x = "No. steps a day", y = "No. times in each day") + theme_grey()
```

![plot of chunk plotting 3](figure/plotting 3-1.png?raw=true) 

###calculating  and reporting the mean and median total number of steps taken per day


```r
newdatamean <- mean(newdata$steps, na.rm=TRUE)
newdatamean 
```

[1] 10766.19

```r
newdatamedian <- median(newdata$steps, na.rm=TRUE)
newdatamedian
```

[1] 10766.19

###Do these values differ from the estimates from the first part of the assignment?
Answer : There is a slight difference when comparing the two:

###what is the impact of imputing missing data on the estimates of the total daily number of steps?
The median was affected (shifted), the mean however does not seem to experience any noticeable change.

##Are there differences in activity patterns between weekdays and weekends?

###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

- How to perform comparison
1. Add a column to the table which indicates the day of the week
2. Subset the table into two subdivisons, weekends (Saturday & Sunday) and weekdays (Monday - Friday).
3. Calculate the average steps per interval for each new data set.


```r
weekdaysteps <- function(mydata) {
  weekdaysteps <- aggregate(mydata$steps, by=list(interval = mydata$interval),
                            FUN=mean, na.rm=TRUE)
  weekdaysteps$interval <- as.integer(levels(weekdaysteps$interval)[weekdaysteps$interval])
  colnames(weekdaysteps) <- c("interval", "steps")
  weekdaysteps
}

dataweekdays <- function(mydata) {
  mydata$weekday <- as.factor(weekdays(mydata$date))
  weekenddata <- subset(mydata, weekday %in% c("Saturday","Sunday"))
  weekdaydata <- subset(mydata, !weekday %in% c("Saturday","Sunday"))
  
  weekendsteps <- weekdaysteps(weekenddata)
  weekdaysteps <- weekdaysteps(weekdaydata)
  
  weekendsteps$dayofweek <- rep("weekend", nrow(weekendsteps))
  weekdaysteps$dayofweek <- rep("weekday", nrow(weekdaysteps))
  
  dataweekdays <- rbind(weekendsteps, weekdaysteps)
  dataweekdays$dayofweek <- as.factor(dataweekdays$dayofweek)
  dataweekdays
}

data_weekdays <- dataweekdays(my_data2)
```

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

- constructing a plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
library (ggplot2)
ggplot(data_weekdays, aes(x=interval, y=steps)) +  geom_line(color="blue", size= 2) +  facet_wrap(~ dayofweek, nrow=2, ncol=1) + labs(x="5 minute interval", y="No. steps") + theme_grey()
```

![plot of chunk plotting 4](figure/plotting 4-1.png?raw=true) 

it seems that activity on the weekdays has the highest peak in terms of step intervals. Also, weekends activities has more peaks over a hundred than weekdays.

