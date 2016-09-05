library(dplyr)

activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date)

activityNaOmit <- na.omit(activity)

x <- tapply(activityNaOmit$steps, activityNaOmit$date, sum)
hist(x)
mean(x)
median(x)

y <- tapply(activityNaOmit$steps, activityNaOmit$interval, mean)
z <- activityNaOmit[,3]
stepsOverAverageDay <- data.frame(z[1:length(y)])
stepsOverAverageDay <- cbind(stepsOverAverageDay, y)
names(stepsOverAverageDay) <- c("interval", "steps")
dimnames(stepsOverAverageDay)[[1]] <- c(1:length(y))
plot(stepsOverAverageDay, type="l")
stepsOverAverageDay[which(stepsOverAverageDay$steps == max(stepsOverAverageDay$steps)), ]

sum(is.na(activity$steps))

activityImputeNA <- activity
for (i in 1:length(activityImputeNA$steps)) {
    if (is.na(activityImputeNA$steps[i])){
        blankStep <- activityImputeNA$interval[i]
        activityImputeNA$steps[i] <- as.numeric(stepsOverAverageDay$steps[which(stepsOverAverageDay$interval == blankStep)])
    }
}
a <- tapply(activityImputeNA$steps, activityImputeNA$date, sum)
hist(a)
mean(a)
median(a)

activityImputeNA$weekday <- ifelse(weekdays(activityImputeNA$date) == "Saturday" | weekdays(activityImputeNA$date) == "Sunday", "weekend", "weekday")

activityImputeNAWeekday <- filter(activityImputeNA, activityImputeNA$weekday == "weekday")
activityImputeNAWeekend <- filter(activityImputeNA, activityImputeNA$weekday == "weekend")

par(mfrow = c(2,1))
plot(activityImputeNAWeekday$interval, activityImputeNAWeekday$steps, type = "l")
plot(activityImputeNAWeekend$interval, activityImputeNAWeekend$steps, type = "l")
