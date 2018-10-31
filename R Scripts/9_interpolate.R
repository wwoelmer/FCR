
library(tidyverse)

#master dataset
data <- read.csv("./FCR_Master_2013_2017.csv")
data$Date <- as.Date(data$Date)
yes_dates <- unique(data$Date)
#vector of weeks from 2013-2017
week13 <- data.frame(seq(as.Date("2013-03-01"), as.Date("2013-10-31"), by = "week"))
week14 <- data.frame(seq(as.Date("2014-03-01"), as.Date("2014-10-31"), by = "week"))
week15 <- data.frame(seq(as.Date("2015-03-01"), as.Date("2015-10-31"), by = "week"))
week16 <- data.frame(seq(as.Date("2016-03-01"), as.Date("2016-10-31"), by = "week"))
colnames(week13)[1] <- "Date"
colnames(week14)[1] <- "Date"
colnames(week15)[1] <- "Date"
colnames(week16)[1] <- "Date"
#ok, we can make a vector that consists of all the weeks in 2013-2017 that we want to have data for:
weeks <- rbind(week13, week14, week15, week16)

#now, we want to compare this list of dates to the list of dates that we have data, yes_dates
plot(weeks)

