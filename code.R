##Packages Used in this Analysis
library(ggplot2)

##Initial Data Load into R, and Processing
###Load Data
unzip("activity.zip")
activitydata <- read.csv("activity.csv")

###Process Data - Transform date from factor to date
activitydata$date <- as.Date(activitydata$date,format = "%Y-%m-%d")

##Objective 1 - Mean Number of Steps Taken Per Day
###Histogram of Mean Steps Taken Per Day
total.steps.day <- setNames(with(activitydata, aggregate(steps, by = list(date), 
                                                FUN = sum)),
                                 c("date","steps"))

#Create histogram of steps per day
hist(total.steps.day$steps, main = "Total Steps Taken Per Day",
     xlab = "Steps per day",
     col = "blue",
     border = "black",
     labels = TRUE)

#Calculate mean and median steps taken each day
meansteps <- round(mean(total.steps.day$steps, na.rm = TRUE), digits = 0)
mediansteps <- round(median(total.steps.day$steps, na.rm = TRUE), digits = 0)

#Calculate average steps per 5-min interval
avg.steps.interv <- setNames(aggregate(steps ~ interval,                                                 
                                                 activitydata,
                                                 mean),
                             c("interval", "avg.steps"))

#Create the timeseries plot of average steps per interval
with(avg.steps.interv, plot(interval, avg.steps, type = "l",
                            col = "red",
                            main = "Average Steps Per Interval",
                            xlab = "5 Minute Interval",
                            ylab = "Average Steps"))

#Which interval has the most steps, on average
max.interval <- avg.steps.interv[avg.steps.interv$avg.steps == max(avg.steps.interv$avg.steps),1]

#Number of NAs in dataset
sum(is.na(activitydata))

#Using the mean for each interval, fill in intervals with NA for steps
#Create a dataframe using the original data frame
imputed.data <- activitydata
                           
for (i in 1:nrow(imputed.data)) {
    if(is.na(imputed.data[i,]$steps)) {
        imputed.data[i,]$steps <- avg.steps.interv[imputed.data[i,]$interval == 
                                                       avg.steps.interv$interval,]$avg.steps
    }
}
    
#Calculate a imputed total steps per day, then plot a histogram of total steps per day
imputed.total.steps.day <- setNames(with(imputed.data, aggregate(steps, by = list(date), 
                                                                 FUN = sum)),
                                    c("date","steps"))

hist(imputed.total.steps.day$steps, main = "Total Steps Taken Per Day",
     xlab = "Steps per day",
     col = "blue",
     border = "black",
     labels = TRUE)

#Calculate mean and median steps per day for the imputed data
imputed.meansteps <- round(mean(imputed.total.steps.day$steps, na.rm = TRUE), digits = 0)
imputed.mediansteps <- round(median(imputed.total.steps.day$steps, na.rm = TRUE), digits = 0)

#Add a new column to imputed.data that specifies if the day is a weekday or weekend
imputed.data$daytype <- NA

for (i in 1:nrow(imputed.data)) {
    if (weekdays(imputed.data[i,]$date, abbreviate = TRUE) < "Sat") {
        imputed.data[i,]$daytype <- "weekday"
    } else {imputed.data[i,]$daytype <- "weekend"
    }
}

imputed.data$daytype <- as.factor(imputed.data$daytype)

#Make a panel plot containing a time series plot of the 5-minute interval and the average 
#number of steps taken, averaged across all weekday days or weekend days.
library(dplyr)
library(lattice)
groupedbydata <- imputed.data %>% group_by(daytype, interval) %>% summarize_each(funs(mean))
par(mfrow = c(2,1))
xyplot(steps ~ interval | daytype, groupedbydata,
       type = "l",
       col = "red",
       main = "Average Total Steps by Interval",
       xlab = "5-minute Interval",
       ylab = "Average Steps")