## This is the main R script

# load the libraries
library(lattice)

# unzip, read in and preprocess the data
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)

# calculate total number of steps taken per day
spd <- aggregate(steps ~ date, data, sum)

# make a histogram of the total number of steps taken each day
par(mfcol = c(1, 1))
hist(spd$steps,
     col = "blue",
     main = "Total number of steps taken each day",
     xlab = "Number of steps")

# calculate the mean and median total number of steps taken per day
spd_mean <- mean(spd$steps)
spd_median <- median(spd$steps)

# calculate the number of steps taken in each 5-minute interval, 
# averaged across all days
aspi <- aggregate(steps ~ interval, data, mean)

# make a time series plot of the 5-minute interval
# and the average number of steps taken, averaged across all days
plot(type = "l", aspi$interval, aspi$steps,
     col = "red",
     xlab = "Interval",
     ylab = "Number of steps",
     main = "Average daily activity pattern")

# find the interval (average across all the day) with maximum number of steps
# (and the number of steps, too)
int_max <- aspi[which.max(aspi$steps), 1]
aspi_max <- max(aspi$steps)

# calculate the total number of missing values in the dataset

na_rows <- sum(is.na(data$steps))

# prepare a new data set with imputed NA values
## add column with an daily-averaged number of steps for a given interval
## (will be used later for substitution of NA values)
data_imp <- merge(data, aspi, by = "interval")
data_imp <- data_imp[with(data_imp, order(date, interval)),]
## substitute missing data with the mean for that 5-minute interval
data_na <- is.na(data_imp$steps.x)
data_imp$steps.x[data_na] <- data_imp$steps.y[data_na]
## sort the data frame and make some cleanup of row and column names
row.names(data_imp) <- 1:nrow(data_imp)
colnames(data_imp)[2] <- "steps"
data_imp$steps.y <- NULL

# calculate total number of steps taken per day
spd_imp <- aggregate(steps ~ date, data_imp, sum)

# make a histogram of the total number of steps taken each day
hist(spd_imp$steps,
     col = "green",
     main = "Total number of steps taken each day
(after imputing missing values)",
     xlab = "Number of steps")

# calculate the mean and median total number of steps taken per day
spd_imp_mean <- mean(spd_imp$steps)
spd_imp_median <- median(spd_imp$steps)

# calculate and plot the impact of imputing missing data on the estimates 
# of the total daily number of steps

spd_imp <- aggregate(steps ~ date, data_imp, sum)
plot(spd$date, spd$steps, 
     type = "l",
     col = "blue",
     ylab = "Number of steps",
     xlab = "Day",
     main = "Impact of imputing missing data
on the estimates of the total daily number of steps")
lines(spd_imp$date, spd_imp$steps, col = "green")
legend("topleft", c("Original data", "Imputed data"),
       lty = rep(1, 2),
       col = c("blue", "green"))

# create a new factor variable in the dataset with two levels -- "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.

data_imp <- cbind(data_imp, "")
colnames(data_imp)[4] <- "day"
Sys.setlocale(category = "LC_TIME", locale = "us_US")
data_imp[, 4] <- "weekday"
data_imp[weekdays(data_imp$date) %in% c("Saturday", "Sunday"), 4] <- "weekend"

# make a panel plot
aspi_imp <- aggregate(steps ~ interval + day, data_imp, mean)
p <- xyplot(steps ~ interval | day, 
            type = "l",
            data = aspi_imp, 
            layout = c(1, 2),
            xlab = "Interval",
            ylab = "Number of steps",
            main = "Average daily activity pattern")
print(p)