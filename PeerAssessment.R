fileName <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(fileName, "activity.zip")

unzip("activity.zip")
rawData <- read.csv("activity.csv")


rawData$date <- as.Date(rawData$date, "%Y-%m-%d")
totalSteps <- aggregate(steps ~ date, data = rawData, sum, na.rm = TRUE)

hist(totalSteps$steps, main="Total number of steps taken", ylab="Frequency", xlab="Number of Steps", col="red")

mean(totalSteps$steps, na.rm = T)

median(totalSteps$steps, na.rm = T)

intervalSteps <- aggregate(steps ~ interval, data = rawData, mean, na.rm = TRUE)

plot(intervalSteps$interval, intervalSteps$steps, type="l", ylab ="mean steps", xlab = "interval")

#total number of missing data
sum(as.numeric(is.na(rawData$steps)))


#Filling in missing data
# the mean for that 5-minute interval is taken to be the value 
newData <- rawData

for (i in 1:nrow(rawData) )
{
  if ( is.na(rawData[i,1]) ) {
    interval <- rawData[i,3]
    correspondingRow <- interval / 5 + 1
    newData[i,1] <- intervalSteps[correspondingRow,2]
  }
}

#Histogram of the total number of steps with data filled in
newTotalSteps <- aggregate(steps ~ date, data = newData, sum, na.rm = TRUE)
hist(newTotalSteps$steps, main="Total number of steps taken", ylab="Frequency", xlab="Number of Steps", col="red")

mean(newTotalSteps$steps, na.rm = T)

median(newTotalSteps$steps, na.rm = T)


#Are there differences in activity patterns between weekdays and weekends?

#newData$day <- weekdays(rawData$date)

newData$posixdate <- as.POSIXlt(newData$date)

#weekends
weekends <- newData[which(newData$posixdate$wday == 0 | newData$posixdate$wday == 6),]
intervalStepsWeekends <- aggregate(steps ~ interval, data = weekends, mean, na.rm = TRUE)

#weekdays
weekdays <- newData[which(newData$posixdate$wday > 0 & newData$posixdate$wday < 6),]
intervalStepsWeekdays <- aggregate(steps ~ interval, data = weekdays, mean, na.rm = TRUE)

par(mfrow=c(1,1))
plot(intervalStepsWeekdays$interval, intervalStepsWeekdays$steps, type="l", ylab ="mean steps", xlab = "interval")
lines(intervalStepsWeekends$interval, intervalStepsWeekends$steps, col="blue")
legend("topright", legend=c("Weekdays", "Weekends"), col=c("black", "blue"), lty=1, lwd=2)