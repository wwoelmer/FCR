### interpolating FCR LT dataset

library(tidyverse)
library(ggplot2)

#use the site 50 ctd chla data because this is the response variable
## ie, I want to know the timestep of my response variable first
data <- read.csv("./CTD/FCR_CTD_50_binned.csv")

# first look at where the gaps are in each year
### #2013 dates
#make a dataframe that has every day in 2013
years <- seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "day")
dates <- data.frame(matrix(ncol = 2, nrow = length(years)))
dates[1] <- years
colnames(dates) <- c("Date", "Y_N")

#make a dataframe consisting of dates where we have chla data (ie, no NAs)
#first subset data to get rid of any days where we do not have chla data
chla <- data[!is.na(data$Chla_ugL), ]
chladates <- chla %>% select(Date)
chladates <- as.Date(chladates$Date)
yes <- unique(chladates)

y <- ifelse(years %in% yes, 1, 0)
dates[2] <- y

plot(dates[dates$Y_N==1,], dates$Date, dates$Y_N)
abline(v =  seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "week"))

dates$Date <- as.Date(dates$Date)

#the time period from Mar 1 to Oct 31 is chosen to represent the stratified period

#look at 2013 alone
thirteen <- dates[dates$Date  < "2013-12-31",]
x <- thirteen[thirteen$Date > "2013-03-01" & thirteen$Date < "2013-10-31",]
yes <- x[x$Y_N==1,]
plot(yes)
abline(v =  seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "week"))
title("timestep 2013")

#look at 2014 alone
fourteen <- dates[dates$Date  < "2014-12-31" & dates$Date > "2014-01-01",]
y <- fourteen[fourteen$Date > "2014-03-01" & fourteen$Date < "2014-10-31",]
yes.y <- y[y$Y_N==1,]
plot(yes.y)
abline(v =  seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "week"))
title("timestep 2014")

#look at 2015 alone
fifteen <- dates[dates$Date  < "2015-12-31" & dates$Date > "2015-01-01",]
z <- fifteen[fifteen$Date > "2015-03-01" & fifteen$Date < "2015-10-31",]
yes.z <- z[z$Y_N==1,]
plot(yes.z)
abline(v =  seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "week"))
title("timestep 2015")


#look at 2016 alone
sixteen <- dates[dates$Date  < "2016-12-31" & dates$Date > "2016-01-01",]
a <- sixteen[sixteen$Date > "2016-03-01" & sixteen< "2016-10-31",]
yes.a <- a[a$Y_N==1,]
plot(yes.a)
abline(v =  seq(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "week"))
title("timestep 2016")







