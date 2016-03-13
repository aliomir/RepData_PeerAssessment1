---
output: html_document
---

#Reproducible Research Assignment 1

##Ali Mir

##Loading and preprocessing the data

#####The data should be present in your wroking directory and already unzipped. The data will be loaded in variable called data. We will process the data as we go along.

```{r}
data<-read.csv("activity.csv")
```

##What is mean total number of steps taken per day?

#####For this part of the assignment the missing values can be ignored

* Calculate the total number of steps taken per day.
* Make a histogram of the total number of steps taken each day.
* Calculate and report the mean and median of the total number of steps taken per day.

#####For the frist part we could simply apply tapply function to find out steps taken each day. In order to understand the distribution - a simple histogram will idicate how steps taken per day varies


```{r}
stepstakenperday<-tapply(data$steps, data$date, sum)
hist(stepstakenperday)
```


Where as the mean and median of the steps taken are:

```{r}
mean<-mean(stepstakenperday, na.rm=TRUE)
median<-median(stepstakenperday,na.rm=TRUE)
mean
median
```

##What is the average daily activity pattern?

Once again I will use tapply to find out avg no of steps during an interval - while ignoring the NA values.

```{r}
avgstepsperinterval<-tapply(data$steps, data$interval, mean,na.rm=TRUE)
plot(unique(data$interval),avgstepsperinterval, type = "l")
```

The 5-minute interval contains the maximum number of steps can be deduced by using which.max function

```{r}
which.max(avgstepsperinterval)
```

That is 835 interval which has an idex of 104


##Missing Values

Total number of missing values are:

```{r}
sum(is.na(data))
```

The strategy for filling in all of the missing values in the dataset is to use mean of the day.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r eval=TRUE}
stepsinterval<-data.frame(tapply(data$steps, data$interval, mean,na.rm=TRUE))
stepsinterval[,2]<-dimnames(stepsinterval)
names(stepsinterval)<-c("Mean","interval")
df2<-data.frame(data[is.na(data$steps),])
new<-merge(stepsinterval,df2,by.x = "interval", by.y = "interval")
data2<-data
data2[is.na(data2)]<-new[,2]
head(data2)
```

Histogram of the total number of steps taken each day

```{r eval=TRUE}
stepstakenperday2<-tapply(data2$steps, data2$date, sum)
hist(stepstakenperday2)
```

Where as the mean and median of the steps taken in the new data are:

```{r}
mean<-mean(stepstakenperday2, na.rm=TRUE)
median<-median(stepstakenperday2,na.rm=TRUE)
mean
median
```


Data has slightly changed but not significantly.

##Are there differences in activity patterns between weekdays and weekends?


```{r}
data2$date<-as.Date(data2$date)
data2$weekday<-weekdays(data2$date)
df3<-data.frame(c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Firday"),c("Weekend","Weekend","Weekday","Weekday","Weekday","Weekday","Weekday"))
names(df3)<-c("weekday","weekdayweekend")
last<-merge(data2,df3,by.x = "weekday", by.y = "weekday")
library(ggplot2)
qplot(interval,steps,data = last, facets = .~weekdayweekend, geom = "smooth")
```


From the above graph indiactes that activities are different on weekdays and weekend. Where on weekdays we witness hikes but on weeknds there is more uniformity.


