---
output: html_document
---
# Reproducible Research: Peer Assessment 1

Cos D. Fi
Data Science Track - JHS
August 2014

## Loading and preprocessing the data

```{r}
setwd("~/Desktop/MOOCs/Coursera/DataScience/ReproducibleDataAnalysis_Course")

X <- read.csv("activity.csv", header=TRUE, na.strings = "", colClasses="character")
names(X)

#Recode the variables
X$steps <- as.numeric(X$steps)
X$date <- as.Date(X$date, format="%Y-%m-%d")
X$interval <- as.numeric(X$interval)

#The following are used to surpress printing of additional review of the data
#str(X)
#summary(X)

#summary(X$interval)

#X$interval <- format(X$interval, width = 4, format = "d", flag = "0")
#summary(X$interval)

#X$interval = as.factor(X$interval)
#summary(X$interval)
```

## What is mean and median number of steps taken per day?

```{r}
library(plyr)

totalStepsDay <- na.omit(ddply(X, "date", summarise, TotalSteps = sum(steps, na.rm=TRUE)))
#totalStepsDay
```
### Histogram of Total Steps per Day

```{r}
hist(totalStepsDay$TotalSteps, breaks = 10, main = ("Histogram of Total Steps per Day"), xlab = "Total Steps per Day", ylab = "Number of Days", col = "lightblue", border = "pink")
```

```{r}
meanStepsDay <- na.omit(ddply(X, "date", summarise, MeanSteps = mean(steps, na.rm=TRUE)))
```

### Mean Steps per Day

```{r}
meanStepsDay
#Note that some days do not have numeric averages since data for those days are missing
```

### Median Steps per Day
```{r}
medianStepsDay <- na.omit(ddply(X, "date", summarise, MedianSteps = quantile(steps, probs = 0.5, na.rm=TRUE)))
medianStepsDay
```

## What is the average daily activity pattern?

```{r}
meanStepsInterval <- na.omit(ddply(X, "interval", summarise, AvgSteps = mean(steps, na.rm=TRUE)))
meanStepsInterval

function(dat) dat[order(dat$AvgSteps), ]

maxAvgStepsInterval <- ddply(meanStepsInterval, "AvgSteps", function(dat) dat[order(dat$AvgSteps), ])
```

### Here is the max avergae steps and its interval

```{r}
maxAvgStepsInterval[nrow(maxAvgStepsInterval), ]
```

## Here we visualize the avearge daily activity pattern

```{r}
plot(meanStepsInterval, type="l")
#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
meanStepstimeseries <- ts(meanStepsInterval$AvgSteps, start = 0, end = 288)
plot.ts(meanStepstimeseries, xlab = "Time in 5 minute intervals")
```

## Imputing missing values

```{r}
# Missing Values
Y <- sapply(X, function(x) sum(is.na(x)))
#Y
Y <- Y[Y>0] # To count only missing values
#Y
mean(is.na(X$steps))
count <- table(is.na(X$steps))
count
barplot(count)
```

### Here are the observations with missing steps values

```{r}
Z <- X[is.na(X$steps), ]
#Z
#tail(X)  # checking the later rows of X to make sure that the subetting worked
```

#### Further Recoding of the data

```{r}
library(lubridate)
#month(Z$date) # Used to test the function
#weekdays(Z$date) # Used to test the function
#The missing values do not "seem" to be related to a day of the week
#Except there are no missing values for Tuesdays
```

#### Missing values by day of the week
```{r}
table(weekdays(Z$date))
```

#### Compare with the count of observations by day of the week
```{r}
table(weekdays(X$date))
```

### Mean imputation

```{r}
#W <- X
#Needs more work
library(Amelia)
#a.out <- amelia(x=W, m=1, ...) # Need more work with Amelia
mean.imp <- function (dat){
        missing <- is.na(dat)
        n.missing <- sum(missing)
        dat.obs <- dat[!missing]
        imputed <- dat
        imputed[missing] <- mean(dat.obs, n.missing, replace=TRUE)
        return(imputed)
}
X.imp <- mean.imp(X$steps)
X$steps2 <- X.imp
```

## Are there differences in activity patterns between weekdays and weekends?

```{r}
#Create weekday and weekend labels or the days in the dataset
X$days <- as.factor(weekdays(X$date))
X$weekdays <- X$days
from1 <- c("Saturday", "Sunday")
to1 <- c("weekend", "weekend")
from2 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
to2 <- c("weekday", "weekday", "weekday", "weekday", "weekday")
gsub1 <- function(pattern, replacement, x){
        for(i in 1:length(pattern))
                x <- gsub(pattern[i], replacement[i], x)
        x
}
X$weekdays <- gsub1(from1, to1, X$weekdays)
X$weekdays <- gsub1(from2, to2, X$weekdays)
X$weekdays <- as.factor(X$weekdays)
#str(X)

library(lattice)
meanStepsIntDay <- na.omit(ddply(X, c("interval", "weekdays"), summarise, AvgSteps = mean(steps2, na.rm=TRUE)))
#meanStepsIntDay
xyplot(AvgSteps ~ interval | weekdays, data = meanStepsIntDay, scales=list(cex=.8, col="red"), xlim = c(0, range(X$interval)), layout = c(1,2), type= "l", xlab = "Interval", ylab = "Average Number of Steps")
```

# The End
