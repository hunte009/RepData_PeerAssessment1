---
title: "Reproducible Data Course Project"
author: "Arjen Hunter"
date: "23 January 2019"
output: html_document
---
#Introduction

In this assignment I use data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This assignment is intended to show my proficiency in R by completing a complete assignments and a series of questions in a single R markdown document. The document can be processed by knitr and be transformed into an HTML file. 

##Loading and preprocessing the data
Show any code that is needed to

1) Load the data (i.e. read.csv())

2) Process/transform the data (if necessary) into a format suitable for your analysis

```{r setup, include=TRUE}
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
activity <- read_csv("activity.csv", 
  col_types = cols(date = col_character(), 
  interval = col_number(), steps = col_number()), 
  na = "NA")
```

## Eyeball the data

Let's see what we have in terms of data type, completeness and how it is sorted. According to the documentation the variables included in this dataset are:

-steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)  
-date: The date on which the measurement was taken in YYYY-MM-DD format  
-interval: Identifier for the 5-minute interval in which measurement was taken  
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
```{r summary, echo=TRUE}
## Eyeball the data and put everything in an appropriate format
str(activity); dim(activity); head(activity);summary(activity)
suppressMessages(library(lubridate))
activity$date <- ymd(activity$date)
```

The data does indeed contain 3 variables with 17,568 datapoints and be sorted by the date, though the date is not stored as such. It is actually stored as text. So that is the first thing to correct. For doing so, the lubridate package is required.

## 1. Calculate the total number of steps taken per day
Every individual day contains various 5-minute measurements. We want to report the total number of steps taken on each day. In order to do so, I aggregate the data by date, using the sum function. It is stored in a dataframe with an entry for each of the 61 unique date, including those dates having only NA.

```{r sum, echo=TRUE}
sum <- aggregate(activity$steps, by=list(Category=activity$date), FUN=sum)
str(sum)
```

## 2. A histogram of the total number of steps taken each day
With bar charts, the labels on the X axis are categorical; with histograms, the labels are quantitative. By giving the plot an impressive color scheme, we draw the attention away from that aspect.

```{r plot}
hist(sum$x, 
     main="Total number of steps taken each day",
     xlab="steps per day",breaks=20,
     col=(1:20)
)
```

## 3. Calculate the mean and median
The mean and median of the total number of steps taken per day can be calculated by actual day. It can also be calculated over the total, since that was measured per day. So to prevent misunderstandings and since I need the practise, let do both. First, let's calculate the mean and median in for each day.

```{r mean and median}
mean <- aggregate(activity$steps, by=list(Category=activity$date), FUN=mean)
median <- aggregate(activity$steps, by=list(Category=activity$date), FUN=median)
str(mean); str(median)
```

Secondly, for the fun of it, let's calculate the overall the mean and median, without actually storing them.
```{r overall mean and median}
mean(na.omit(sum$x))
median(na.omit(sum$x))
```

## 4. Time series plot of the average number of steps taken
Here I'm assuming the intention is to plot the average number of steps per days and not overall. Additionally I've added the mean in red to give an impression where it is located.

```{r plot mean}
plot(mean$Category,mean$x,type="l",
     main=("The average number of steps per day"),
     xlab="Date",
     ylab="Average number of steps",
     col="blue"
    )
abline(h=mean(na.omit(mean$x)), lty=2,col="red")
```

## 5. The 5-minute interval that contains the maximum number
Determine the number of the interval that contains the maximum number of steps. By reverse sorting the data by the number of steps, we ensure the first cell of the dataframe contains the maximum. The number of the 5-minute interval is in the interval column.
```{r}
arrange(activity,-steps)$interval[1]
```
The number of the 5-minute interval that, on average, contains the maximum number of steps is 615.

## 6. Code to describe and show a strategy for imputing missing data
In orde to determine a strategy to fill in the missing (NA) data, I have looked at which data is missing. If many single points are missing, interpolation would be an option. So firstly let's check how many values are actually missing and where those values are missing.
```{r}
sum(is.na(activity$steps))
plot(is.na(activity$steps), col="red",
     main="Missing datapoints", at=c(0,1),
     ylab="1 = NA", xlab="datapoint#")
text(9000, 0.95, "these upper values represent the missing datapoints",
     cex = .8)
```


There is a total of 2304 measurements that are missing. We see that not individual measurements are missing, but a few larger chunks, so the simplest option would be to substitute them with the mean of the series. The difference between mean and median is very small, so discussing which is best is academic.
```{r add missing data}
activity$steps[is.na(activity$steps)] <- round(mean(na.omit(activity$steps)))
sum(is.na(activity$steps))
```
Now the series contains no more missing data (NA).

## 7. Histogram of the total number of steps taken each day
Let make another histogram, this time of the total number of steps taken each day after missing values are imputed. Previous results require that this histogram has an equally impresive color scheme.
```{r}
hist(sum$x, 
     main="Total number of imputed steps taken each day",
     xlab="steps per day",breaks=20, col=(21:40)
)
```

## 8. Panel plot comparing the average number across weekdays and weekends
Here I'll make a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends. Foremost, I choose to plot the panel horizontally, because I think comparing the average number of steps between weekends and weekdays is easier that way. The y-axes of both plots have been set identically. And I decided to add some color to the otherwise boring image, for the sake of it.

```{r}
activity <- mutate(activity,wday(activity$date))
par(mfrow=c(1,2))
plot(activity$steps[activity$`wday(activity$date)`==1 | activity$`wday(activity$date)`==7],
     ylab="Average number of steps", col=activity$`wday(activity$date)`,
     xlab="On weekends",ylim=c(0,820))
abline(h=mean(activity$steps[activity$`wday(activity$date)`==1 | activity$`wday(activity$date)`==7]),
       col="darkgrey",lwd=3)
plot(activity$steps[activity$`wday(activity$date)`>1 & activity$`wday(activity$date)`<7],
     ylab="Average number of steps", col=activity$`wday(activity$date)`,
     xlab="On weekdays",ylim=c(0,820))
abline(h=mean(activity$steps[activity$`wday(activity$date)`>1 & activity$`wday(activity$date)`<7]),
       col="darkgrey",lwd=3)
```

Just for practise of it, I have plotted the average number of steps in red to both plots. Note there is a slightly higher average number of steps on weekends.

## 9. All of the R code
All the code used in producing the numbers and plots has been reported in this document.