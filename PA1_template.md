# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 2.15.3
```

```r

activitydata <- read.csv("activity.csv")
```


## Part I Histogram, Mean, and Median 

### Histogram

```r
mysum <- aggregate(activitydata$steps, list(Date = activitydata$date), sum)

names(mysum)[names(mysum) == "x"] <- "Sum"
hist(mysum$Sum, main = "Histogram of the Number of Steps taken each Day", xlab = "Number of Steps taken in a Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

mymean <- mean(mysum$Sum, na.rm = TRUE)
mymedian <- median(mysum$Sum, na.rm = TRUE)
```


### Mean

#### The mean number of steps each day is: 1.0766 &times; 10<sup>4</sup>

### Median

#### The median number of steps each day is: 10765


## Part II What is the average daily activity pattern?


```r
myavgdaily <- aggregate(activitydata$steps, list(Interval = activitydata$interval), 
    mean, na.rm = TRUE)
plot(myavgdaily$x, myavgdaily$interval, type = "l", main = "Average Daily Activity Pattern", 
    ylab = "AVG Steps Across all Days", xlab = "5 Minute Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 




```r
myavgdailymax <- max(myavgdaily$x)
whichrow <- which(myavgdaily$x == max(myavgdaily$x))
myavgdailymaxinterval <- myavgdaily$Interval[whichrow]
```


#### The Interval that has the Maximum number of steps is 835 which has a value of 206.1698. 


## Part III Inputing missing values


```r
incompletecases <- nrow(activitydata) - sum(complete.cases(activitydata))
```


#### The total number of rows with missing values is 2304.

### The strategy for replacing missing values will be to replace each missing missing value by the average across all Intervals and Days.

```r
averageall <- mean(activitydata$steps, na.rm = TRUE)
activitydata$steps[is.na(activitydata$steps)] <- averageall
```


### New Histogram, Mean, and Median 


```r
mysum <- aggregate(activitydata$steps, list(Date = activitydata$date), sum)
names(mysum)[names(mysum) == "x"] <- "Sum"
hist(mysum$Sum, main = "Histogram of the New Number of Steps taken each Day", 
    xlab = "Number of Steps taken in a Day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r

mymean <- mean(mysum$Sum, na.rm = TRUE)
mymedian <- median(mysum$Sum, na.rm = TRUE)
```


### Mean

#### The new mean number of steps each day is: 1.0766 &times; 10<sup>4</sup>.

### Median

#### The new median number of steps each day is: 1.0766 &times; 10<sup>4</sup>


These new mean and median have not changed.The new histogram has higher frequencey for number of steps between 10000 and 15000.

## Part IV Are there differences in activity patterns between weekdays and weekends?



```r
activitydatanew = aggregate(steps ~ interval + date, activitydata, mean)

activitydatanew$day = ifelse(as.POSIXlt(as.Date(activitydatanew$date))$wday%%6 == 
    0, "weekend", "weekday")

activitydatanew$day = factor(activitydatanew$day, levels = c("weekday", "weekend"))

xyplot(steps ~ interval | factor(day), data = activitydatanew, aspect = 1/2, 
    type = "l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


#### The weekend numbers are somewhat left skewed with more activity mid to end of day and weekday numbers are more evenly distribtured with higher levels at the beginning of the day.  


