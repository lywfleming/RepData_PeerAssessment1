activity_file <- "activity.csv"

if(file.exists(activity_file)) {
  activity_data <- read.csv(activity_file)
} else {
  cat("Activity file does not exist")
  stop()
}

#Calculate total, mean, and median steps taken per day
days <- group_by(activity_data, date)
sum_data <- summarize(days, total_steps = sum(steps), 
   mean_steps = mean(steps), median_steps = median(steps))

#Histogram of total number of steps taken each day

#dev.copy(png, file = "Hist_steps.png")

par(mfrow=c(1,1))

hist(sum_data$total_steps, breaks = 10, main = 
  "Histogram of Total Number of Steps", xlab = "Total Number of Steps", 
  xlim = c(0, 25000), ylim = c(0, 20), col = "magenta")

#dev.off()

#Plot time series of 5-minute interval and average number of steps taken across
#all days
interval_data <- group_by(activity_data,interval)
interval_data_ave <- summarize(interval_data, ave_steps = mean(steps, na.rm = TRUE))

#dev.copy(png, file = "Ave_steps.png")

par(mfrow=c(1,1))
plot(interval_data_ave$interval, interval_data_ave$ave_steps, type = "l",
     main = "Average # of Steps", xlab = "Interval", ylab = "Average Steps",
     col = "cyan")

#dev.off()

#Find maximum 5-minute interval on average across all days contains the maximum number of steps
hold_max<-summary(interval_data_ave$ave_steps) #First find the maximum value
hold_interval<-subset(interval_data_ave, ave_steps > as.numeric(hold_max[6])-1)
hold_interval <- as.integer(hold_integer)

#Calculate the total number of rows with NAs
na_percent <- as.integer(mean(is.na(activity_data$steps)) * 100) #Gives percentage
count_na <- length(which(is.na(activity_data$steps))) #Gives number of NAs in "steps" data

#Impute NAs in "steps" column by replacing with above average calculated across all days

new_data<-merge(activity_data,interval_data_ave, sort = FALSE)
new_data <- arrange (new_data, date)
new_activity_data <- mutate(activity_data, steps = ifelse(is.na(new_data$steps), new_data$ave_steps , new_data$steps))

#Histogram of total number of steps taken each day with new data (no NAs)
new_days <- group_by(new_activity_data, date)
new_sum_data <- summarize(new_days, total_steps = sum(steps), 
                      mean_steps = mean(steps), median_steps = median(steps))
write.table(new_sum_data, file="new_summary.txt")

#dev.copy(png, file = "Hist_steps_clean.png")

par(mfrow=c(1,1))
hist(new_sum_data$total_steps, breaks = 10, main = 
       "Histogram of Total Number of Steps", xlab = "Total Number of Steps",
       col = "lightblue")

#dev.off()

#Add new factor variable for "weekday" or "weekend"
new_activity_data <- mutate(new_activity_data, 
    day_of_week = ifelse(!(weekdays(as.Date(date)) %in% c('Saturday','Sunday')),
                         "weekday","weekend"))

#Plot new data (no NAs) separated for weekend/weekdays time series of 5-minute 
#interval and average number of steps taken across all days

#First, separate data into "weekend" and "weekday" dates
weekend_activity_data <- filter(new_activity_data, day_of_week == "weekend")
weekday_activity_data <- filter(new_activity_data, day_of_week == "weekday")

wend_interval_data <- group_by(weekend_activity_data,interval)
wend_interval_data_ave <-summarize(wend_interval_data, ave_steps = mean(steps))

wday_interval_data <- group_by(weekday_activity_data,interval)
wday_interval_data_ave <-summarize(wday_interval_data, ave_steps = mean(steps))

#dev.copy(png, file = "Ave_steps_clean.png")

par(mfrow = c(2,1), mar = c(4,4,3,3))
plot(wend_interval_data_ave$interval, wend_interval_data_ave$ave_steps, type = "l", main =
       "Average # of Steps on Weekends", xlab = "Interval", ylab = "Average Steps", col = "blue",
       font.main = 3, ylim = c(0,200))

plot(wday_interval_data_ave$interval, wday_interval_data_ave$ave_steps, type = "l", main =
       "Average # of Steps on Weekdays", xlab = "Interval", ylab = "Average Steps", col = "red",
       font.main = 3, ylim = c(0,200))
#dev.off()

