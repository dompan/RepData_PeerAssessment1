#I need this for weekdays in english
Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Loading and preprocessing the data
# Show any code that is needed to

# 1. Load the data (i.e. read.csv())

dataset<-read.csv("./5/RepData_PeerAssessment1/activity.csv",header=TRUE,sep=",")

# 2. Process/transform the data (if necessary) into a format suitable for your analysis
dataset$num_interval<-rep(seq(288),61)
# I do just one simple addition, anyway the data is tidy: 
# 1. Each variable forms a column
# 2. Each observation forms a row
# 3. Each table stores data about one kind of observation

# ==============================================================================
#  What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the dataset.
dataset_no_na<-dataset[complete.cases(dataset),]

# 1. Make a histogram of the total number of steps taken each day 
tot_steps_day<-tapply(dataset$steps,dataset$date,sum)
barplot(tot_steps_day,col='blue')

# 2. Calculate and report the mean and median total number of steps taken per day
mean_steps=mean(tot_steps_day)
median_steps=median(tot_steps_day)
# ==============================================================================
#  What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

# In a day there are 288 5-minutes intervals.
mean_interval<-tapply(dataset_no_na$steps,dataset_no_na$interval,mean)
plot(mean_interval,xlab="", ylab="",type="l",col='blue')

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
max(mean_interval)
which.max(mean_interval)
# ==============================================================================
#  Imputing missing values
# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)

# 'steps' is the only variable with NA.
# There are 2304 NA values
summary(dataset)

# I make step 2 and step 3 together

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use 
#    the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.
# I decide to use the 5-minute interval because there are entire days NA.

filled_dataset<-dataset

for(i in 1:nrow(dataset)){
        if(is.na(dataset[i,1])){
                num_interval<-dataset[i,4]
                filled_dataset[i,1]<-mean_interval[num_interval]
        }
}

# 4. Make a histogram of the total number of steps taken each day and Calculate 
#    and report the mean and median total number of steps taken per day. Do these 
#    values differ from the estimates from the first part of the assignment? What 
#    is the impact of imputing missing data on the estimates of the total daily 
#    number of steps?

filled_tot_steps_day<-tapply(filled_dataset$steps,filled_dataset$date,sum)
barplot(filled_tot_steps_day,col='blue')

filled_mean_steps=mean(filled_tot_steps_day)
filled_median_steps=median(filled_tot_steps_day)

# ==============================================================================
# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and 
# “weekend” indicating whether a given date is a weekday or weekend day.

day_ofthe_week<-weekdays(as.Date(filled_dataset$date))

for(i in 1:length(day_ofthe_week)){
        if(day_ofthe_week[i]=="Saturday" | day_ofthe_week[i]=="Sunday") day_ofthe_week[i]<-"weekend"
        else day_ofthe_week[i]<-"weekday"
}

filled_dataset$day_ofthe_week<-day_ofthe_week

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the
#    5-minute interval (x-axis) and the average number of steps taken, averaged 
#    across all weekday days or weekend days (y-axis). See the README file in 
#    the GitHub repository to see an example of what this plot should look like 
#    using simulated data.

week<-with(filled_dataset,tapply(steps,list(interval,day_ofthe_week),mean))
# Week is a matrix
# I need to convert it in a data.frame
df_week<-data.frame(week)

par(mfrow=c(2,1))
with(df_week,{
        plot(weekday,xlab="", ylab="",type="l",col='blue')
        plot(weekend,xlab="", ylab="",type="l",col='blue')
})









