
setwd("D:/1. Cursos/Data Science/Modulo 5. Reproducible Research/Week2/Project 1")
library(lubridate)

#General options

```r
opts_chunk$set(echo=TRUE)
```

1. Loading and preprocessing the data


```r
data<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): no fue posible abrir el archivo
## 'activity.csv': No such file or directory
```

```
## Error in file(file, "rt"): no se puede abrir la conexión
```

```r
data$date<-as.Date(data$date)
```

2. What is mean total number of steps taken per day?


```r
sum_steps_d<-sapply(split(data,as.factor(data$date)),function(x) sum(x$steps,na.rm=T))
hist(sum_steps_d,
	xlab="number of daily steps",
	ylab="number of days",
	labels=T,
	main="histogram of number of steps per day",
	col="blue")
```

![plot of chunk steps_daily](figure/steps_daily-1.png) 

```r
mean_steps<-mean(sum_steps_d)
median_steps<-median(sum_steps_d)
cat("mean of the total number of steps taken per day:\n");mean_steps
```

```
## mean of the total number of steps taken per day:
```

```
## [1] 9354.23
```

```r
cat("median of the total number of steps taken per day:\n");median_steps
```

```
## median of the total number of steps taken per day:
```

```
## [1] 10395
```

3. What is the average daily activity pattern?


```r
ave_steps_i<-sapply(split(data,as.factor(data$interval)),function(x) mean(x$steps,na.rm=T))
plot(ave_steps_i,type="l",
	xlab="daily interval",
	ylab="average of number of steps",
	col="purple",
	lwd=8)
```

![plot of chunk steps_interval](figure/steps_interval-1.png) 

```r
max_i<-ave_steps_i[ave_steps_i==max(ave_steps_i)]
cat("5 min interval with the maximum number of steps is:\n");names(max_i)
```

```
## 5 min interval with the maximum number of steps is:
```

```
## [1] "835"
```

```r
cat("5 min interval with the maximum number of steps is:\n");cat(names(max_i));cat("\n")
```

```
## 5 min interval with the maximum number of steps is:
```

```
## 835
```

4. Imputing missing values


```r
cat("The number of NAs on each variable is the following\n");apply(data,2,function(x) sum(is.na(x)))
```

```
## The number of NAs on each variable is the following
```

```
##    steps     date interval day_type 
##     2304        0        0        0
```

```r
#NA imputation
cat("NA imputation\n")
```

```
## NA imputation
```

```r
ave_steps_i<-sapply(split(data,as.factor(data$interval)),function(x) mean(x$steps,na.rm=T))
data[is.na(data$steps),]$steps<-ave_steps_i[data[is.na(data$steps),]$interval]
```

```
## Error in `$<-.data.frame`(`*tmp*`, "steps", value = structure(c(0.0754716981132075, : replacement has 2296 rows, data has 2304
```

```r
cat("The number of NAs on each variable is now\n");apply(data,2,function(x) sum(is.na(x)))
```

```
## The number of NAs on each variable is now
```

```
##    steps     date interval day_type 
##     2304        0        0        0
```

```r
#Recalculation of point 1 without NAs
cat("Recalculations of point 2, histogram of total number of steps per day, mean and median but now without NAs\n")
```

```
## Recalculations of point 2, histogram of total number of steps per day, mean and median but now without NAs
```

```r
sum_steps_d<-sapply(split(data,as.factor(data$date)),function(x) sum(x$steps,na.rm=T))
hist(sum_steps_d,
	xlab="number of daily steps",
	ylab="number of days",
	labels=T,
	main="histogram of number of steps per day",
	col="blue")
```

![plot of chunk nas](figure/nas-1.png) 

```r
mean_steps<-mean(sum_steps_d)
median_steps<-median(sum_steps_d)
cat("mean of the total number of steps taken per day:\n");mean_steps
```

```
## mean of the total number of steps taken per day:
```

```
## [1] 9354.23
```

```r
cat("median of the total number of steps taken per day:\n");median_steps
```

```
## median of the total number of steps taken per day:
```

```
## [1] 10395
```

##New values differs from the calculated on point 2.
##Number of steps per days is greater than in point 2

5. Are there differences in activity patterns between weekdays and weekends?


```r
#Creation of a variable differentiation weekday and weekday

data$day_type<-"weekday"
data$day_type[weekdays(data$date)%in%weekdays(as.Date(c("2012-12-02","2012-12-01")))]<-"weekend"

#Average number of steps per interval depending on weekday/weekend
ave_steps_i_wd<-sapply(split(data[data$day_type=="weekday",],as.factor(data$interval)),function(x) mean(x$steps,na.rm=T))
```

```
## Warning in split.default(x = seq_len(nrow(x)), f = f, drop = drop, ...):
## largo de datos no es múltiplo de la variable de separación
```

```r
ave_steps_i_we<-sapply(split(data[data$day_type=="weekend",],as.factor(data$interval)),function(x) mean(x$steps,na.rm=T))
```

```
## Warning in split.default(x = seq_len(nrow(x)), f = f, drop = drop, ...):
## largo de datos no es múltiplo de la variable de separación
```

```r
ave_steps_i_dt<-data.frame(ave_wd=ave_steps_i_wd,ave_we=ave_steps_i_we)

#plot
par(mfrow=c(2,1))
with(ave_steps_i_dt,{
plot(ave_steps_i_dt$ave_wd,type="l",
	xlab="daily interval",
	ylab="average of number of steps",
	col="purple",
	main="weekday",
	lwd=4)
plot(ave_steps_i_dt$ave_we,type="l",
	xlab="daily interval",
	ylab="average of number of steps",
	col="purple",
	main="weekend",
	lwd=4)
})
```

![plot of chunk weekends](figure/weekends-1.png) 

