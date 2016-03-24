# Selcuk Fidan
# RR Assignment.1

########################################################### Part 1
# clear the Environment
rm(list=ls())
# close all the plots
dev.off(dev.list()["RStudioGD"])

# set working directory
setwd("C:/Users/Selcuk Fidan/Desktop/0_Programming_Assignment/RR_Assignment_1")

# install packages
install.packages("ggplot2")
install.packages("lubridate")
install.packages("mice")
install.packages("VIM")
# call packages
library(ggplot2)
library(lubridate)
library(mice)
library(VIM)

# activity monitoring data
act.mon.data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  
  # make sure the site is live, if it is not live stop function terminate the program
  check.url <- file(act.mon.data,"r")
  if (!isOpen(check.url)) {
    stop(paste("There's a problem with the data:",geterrmessage()))
  }
  # zipfile.data is the variable to keep the *.zip file
  zipfile.data = "repdata-data-activity.zip"
  
  # make sure the data in the working directory if not download the zip file into the to zipfile.data and unzip the zipfile.data
  if(!file.exists(zipfile.data)) {        
        download.file(act.mon.data,zipfile.data)
        data.file <- unzip(zipfile.data)
} else {
        data.file <-"activity.csv"
}

activity.mon.data <- read.csv(data.file)
str(activity.mon.data)
# data structure
# 'data.frame':	17568 obs. of  3 variables:
# steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# date: The date on which the measurement was taken in YYYY-MM-DD format
# $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
# interval: Identifier for the 5-minute interval in which measurement was taken
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...


# What is mean total number of steps taken per day?
# must be aware of that tapply treats the NAs as 0

# remove NAs 
new.activity.mon.data<-na.omit(activity.mon.data)
str(new.activity.mon.data)
#new data structure
# 'data.frame':	15264 obs. of  3 variables:
# $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
# - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
#  .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...


# From the summary of the dataset we can see that the column containing the dates is not properly formatted to a Date class so we will adjust that now.
new.activity.mon.data$date<-as.Date(new.activity.mon.data$date)
# ref : http://stackoverflow.com/questions/9216138/find-the-day-of-a-week-in-r
# new.activity.mon.data$Weekday<-wday(new.activity.mon.data$date, label = TRUE, abbr = FALSE)
str(new.activity.mon.data)

# The number of steps taken is measured in timeslots, 5-minute intervals, so in order to compute the total number of steps taken for each day we will aggregate the data by day.
per.day<-tapply(new.activity.mon.data$steps, new.activity.mon.data$date, FUN=sum)
# So, now the per day dataset contains the total number of steps taken for each day of October and November (total 53 days)
# Let's make a histogram of the total number of steps taken each day.# 
########################################################### Part 3
mean.per.day<-mean(per.day)
median.per.day<-median(per.day)

########################################################### Part 2
number.add.width<-800
number.add.height<-720

png("plot1.png", width=number.add.width, height=number.add.height)
hist(per.day,53, main = "Total number of steps taken per day", xlab = "steps per day",col="black")
abline(v = mean.per.day, col = "red", lwd = 2)
# lines(density(per.day)) #Get a density curve to go along with your AirPassengers histogram
dev.off()
# ggplot(perday, aes(x = date, y = total_steps, fill = Weekday))
# qplot(per.day, binwidth = 1000, xlab = "total number of steps taken each day")
########################################################### Part 4
# What is the average daily activity pattern?

daily.act.pattern<-tapply(new.activity.mon.data$steps, new.activity.mon.data$interval, FUN=mean)
# alternative solution for five.min.interval
five.min.interval.alt<-daily.act.pattern[match(max(daily.act.pattern),daily.act.pattern)]


png("plot2.png", width=number.add.width, height=number.add.height)
plot( x = names(daily.act.pattern),y = daily.act.pattern, type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps",col="red",lwd=3)
# box()
# points(x=names(five.min.interval.alt),y=five.min.interval.alt,col="blue",pch=1,cex=3, bg = "blue")
grid()
# abline(v = five.min.interval.alt, col = "blue", lwd = 2)
#lines(five.min.interval.alt,type="l",col=3)
# par(new=T)
# plot(five.min.interval.alt,type="l",col=3)
# par(new=F)
dev.off()
# Use which.max() to find out the maximum steps, on average, across all the days:
# daily.act.pattern[which.max(daily.act.pattern$steps),]
########################################################### Part 5
five.min.interval<-daily.act.pattern[daily.act.pattern==max(daily.act.pattern)]

########################################################### Part 6
# Imputing missing values
md.pattern(activity.mon.data)
#plot pattern
png("plot3.png", width=number.add.width, height=number.add.height)
aggr_plot <- aggr(activity.mon.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()
png("plot4.png", width=number.add.width, height=number.add.height)
marginmatrix(activity.mon.data)
dev.off()

# Replace each missing value with the mean value of its 5-minute interval
# fill.value <- function(steps, interval) {
    # filled <- NA
    # if (!is.na(steps)) 
        # filled <- c(steps) else filled <- (five.min.interval[five.min.interval$interval == interval, "steps"])
    # return(filled)
# }
# # new data set
new.activity.mon.data<-activity.mon.data
new.activity.mon.data[which(is.na(new.activity.mon.data$steps)),1]<-daily.act.pattern[as.character(new.activity.mon.data[which(is.na(new.activity.mon.data$steps)),3])]
# No missing values are now in the new dataset:
sum(is.na(new.activity.mon.data))
########################################################### Part 7
# Now let's make the same histogram, that we made in the first part of the analysis, in order to visually see if there is a big effect.
new.per.day <- tapply(new.activity.mon.data$steps, new.activity.mon.data$date, FUN=sum)
mean(new.per.day)
median(new.per.day)
png("plot5.png", width=number.add.width, height=number.add.height)
hist(new.per.day,53, main = "Total number of steps taken per day", xlab = "steps per day",col="black")
abline(v = mean(new.per.day), col = "red", lwd = 2)
dev.off()

png("plot6.png", width=number.add.width, height=number.add.height)
par(mfrow=c(1,2))
hist(per.day,53, main = "Total number of steps taken per day", xlab = "steps per day",col="black")
abline(v = median(per.day), col = "red", lwd = 2)
hist(new.per.day,53, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "steps per day",col="black")
abline(v = median(new.per.day), col = "red", lwd = 2)
dev.off()
########################################################### Part 8
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
new.activity.mon.data$date <- as.Date(new.activity.mon.data$date)
new.activity.mon.data$day <- sapply(new.activity.mon.data$date, FUN = weekday.or.weekend)
########################################################### Part 9
png("plot7.png", width=number.add.width, height=number.add.height)
aggregated.avgs <- aggregate(steps ~ interval + day, data = new.activity.mon.data, mean)

#weekday.aggregated.avgs<-aggregated.avgs[,aggregated.avgs$day=="weekday"]
# subsetting data
weekday.aggregated.avgs <- subset(aggregated.avgs, day=="weekday")
weekend.aggregated.avgs <- subset(aggregated.avgs, day!="weekday")
par(mfrow = c(2, 2))

plot( x = weekday.aggregated.avgs$interval,y = weekday.aggregated.avgs$steps, type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps",col="red",lwd=3)
# box()
# points(x=names(five.min.interval.alt),y=five.min.interval.alt,col="blue",pch=1,cex=3, bg = "blue")
grid()
plot( x = weekend.aggregated.avgs$interval,y = weekend.aggregated.avgs$steps, type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps",col="red",lwd=3)
grid()


sf<-ggplot(aggregated.avgs, aes(interval, steps,color = day))+scale_colour_manual(values=c("red","black")) + geom_line(size=2) + facet_grid(. ~ day,scales = "free", space = "free") + 
    xlab("5-minute interval") + ylab("Number of steps")+theme(axis.text=element_text(size=20,colour="blue"),legend.position='none',
        axis.title=element_text(size=20,face="bold"),legend.background = element_rect(),panel.background = element_rect(fill = "grey"),strip.text.x = element_text(size=24, face="bold",colour="white"),
          strip.background = element_rect(colour="red", fill="blue"))
print(sf)

c.y1<-max(weekend.aggregated.avgs$steps)
c.y2<-max(weekday.aggregated.avgs$steps)

c.x1<-weekend.aggregated.avgs$interval[match(c.y1,weekend.aggregated.avgs$steps)]
c.x2<-weekday.aggregated.avgs$interval[match(c.y2,weekday.aggregated.avgs$steps)]

# mean.wt <- data.frame(y.cord = c(c.y1,c.y2),x.cord = c(c.x1,c.x2))
# sf + geom_vline(aes(xintercept = x.cord), mean.wt)

dev.off()