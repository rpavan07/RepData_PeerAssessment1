---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
data <- read.csv("activity.csv")


## What is mean total number of steps taken per day?
#1 - histogram of steps 
> steps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
> png(filename = "plot1.png", width=480, height=480)
> qplot(steps, xlab="steps", ylab="frequency")
> dev.off()

#2 - mean and median
> meansteps <- mean(steps)
> meansteps
9354.23
> mediansteps <- median(steps)
> mediansteps
10395


## What is the average daily activity pattern?
#1 - average
> avg <- aggregate(x = list(steps = data$steps), by = list(int = data$interval), FUN = sum, na.rm=TRUE)

#2 - timeseries plot
> png(filename = "plot2.png", width=480, height=480)
> ggplot(avg, aes(x=steps, y=int)) +
> geom_line() +
> xlab("5 minute interval") +
> ylab("average steps taken")
> dev.off()

#3 - max number of steps
> maxsteps <- which.max(avg$steps)
> maxsteps
104

## Imputing missing values
#1 - number of missing values 
> missing <- length(which(is.na(data$steps)))
> missing
2304

#2 - replacing, mean, median and plotting histogram from the updated data
> datanotmissing <- data[complete.cases(data), ]
> stepsnewdata <- tapply(datanotmissing$steps, datanotmissing$date, sum, na.rm=TRUE)
> newmean <- mean(stepsnewdata)
> newmedian <- median(stepsnewdata)
> png(filename = "plot3.png", width=480, height=480)
> qplot(stepsnewdata, xlab="total number of steps taken", main="DATA WITH MISSING VALUES REPLACED")
> dev.off()


## Are there differences in activity patterns between weekdays and weekends?
#1 - weekend or weekday
datanotmissing$dateType <- ifelse(as.POSIXlt(datanotmissing$date)$wday %in% c(0,6), 'weekend', 'weekday')

#2 - timeseries plot
newavg <- aggregate(steps ~ interval + dateType, data=datanotmissing, mean)
> png("plot4.png")
> ggplot(newavg, aes(interval, steps)) +
+ geom_line() +
+ facet_grid(dateType ~ .) +
+ xlab("5 minute inteval") +
+ ylab("average steps taken")
> dev.off()
