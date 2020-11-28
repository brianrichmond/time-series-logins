# Import libraries
library(jsonlite) # import json files
library(lattice) # graphics including xyplot
library(plyr) # data structure (split-apply-combine), e.g., data.frame
library(chron) # create chronological objects
library(TSA) # Fourier analysis, periodogram


# Import, aggregate, & view data
logins.data<-fromJSON("new_logins.json")
logins.data$login_time <- as.POSIXct(logins.data$login_time)  # convert to date-time class (POSIXct)
summary(logins.data)
summary(logins.data$login_time)  # View summary of raw data
# Aggregate logins per 15 minutes
logins.table <- table(cut(logins.data$login_time, breaks="15 min"))
# Summary stats of aggregated 15-min logins
loginsdf.data<-as.data.frame(t(logins.table), row.names=NULL)
summary(loginsdf.data$Freq)
hist(loginsdf.data$Freq, xlab="Logins (per 15-min interval)", main = "Histogram of Logins")


# Aggregate logins per 3 hours for exploratory plot
logins.3hours.table <- table(cut(logins.data$login_time, breaks="3 hours"))
# Plot login count over time
plot(logins.3hours.table, type="l", xlab="Date-Time (per 3 hours)",
     ylab="Number of User Logins", main="Uber User Logins")


## Use Fourier transform to identify cycles
p <-periodogram(loginsdf.data$Freq, xlim=c(0, 0.06))
dd <-data.frame(freq=p$freq, spec=p$spec)
freq.order <-dd[order(-dd$spec),]
freq.top10 <- head(freq.order,10)
cycle.time <- ((1/freq.top10$freq)/96)
cycle.time.table <- cbind(cycle.time,freq.top10$freq,freq.top10$spec)
colnames(cycle.time.table) <- c("Cycle (# Days)","Frequency","Spec (Strength)")
cycle.time.table


## stl (Seasonal Decomposition of Time Series by Loess) analysis: daily
cycle <- ts(logins.table, frequency=96)
cycle.daily <- stl(cycle, s.window = "per", robust = TRUE)
plot(cycle.daily)
# decompose time series for daily cycle figure
df <- decompose(cycle)
plot(df$figure, type="b", xaxt="n", xlab="Time of Day (hours)", ylab = "User Logins")
# get list of hours per day
hours<-matrix(0:24,1,25)
hours.text<-c(paste(hours,":00", sep=""))
# label x-axis with hours
# las is set to 2 for vertical label orientation
at.hours <- seq(from = 0, to = 96, by = 4)
axis(1, at=at.hours, labels=hours.text, las=2)

## stl (Seasonal Decomposition of Time Series by Loess) analysis: weekly
cycle.w <- ts(logins.table, frequency=672)
cycle.weekly <- stl(cycle.w, s.window = "per", robust = TRUE)
plot(cycle.weekly)
# decompose by week
wf <- decompose(cycle.w)
plot(wf$figure, type="l", xaxt="n", xlab="Time of Week (0:00-24:00 each day)",
     ylab = "User Logins")
# add days of week to x-axis
wday<-c("Fri","Sat","Sun","Mon","Tue","Wed","Thu")
# label x-axis with hours
# las is set to 2 for vertical label orientation
at.days <- seq(from = 1, to = 672, by = 96)
axis(1, at=at.days+48, labels=wday, las=2, tick=FALSE)
axis(1, at=at.days, labels=FALSE, tick=TRUE)

## stl (Seasonal Decomposition of Time Series by Loess) analysis: ~monthly
cycle.m <- ts(logins.table, frequency=2880)
cycle.monthly <- stl(cycle.m, s.window = "per", robust = TRUE)
plot(cycle.monthly)
# decompose by week
mf <- decompose(cycle.m)
plot(mf$figure, type="l", xlab="Monthly Time", ylab = "User Logins")

## END ##
