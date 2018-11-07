# script to select one date only from within each weekly timeframe for the entire 2013-2017 dataset

# read in the dataset that includes only the timeframe of the model (May-Oct 2013-2016) and includes the weeks that have been
# interpolated
data <- read.csv("./CTD/CTD_interpolated_MayOct13_16.csv")

# create a week number
data$week_num <- as.POSIXlt(data$Date)$yday %/% 7
look <- select(data, Date, week_num)

# these sequences start over each year
# so add 53 to 2014 week #, 106 to 2015, and 159 to 2016
data <- data %>%
  group_by(Date)%>%
  mutate(week_num_series = week_num -16)




































#the time period from May 1 to Oct 31 is chosen to represent the stratified period
#vector of weeks from 2013-2017
week13 <- data.frame(seq(as.Date("2013-05-01"), as.Date("2013-10-31"), by = "week"))
week14 <- data.frame(seq(as.Date("2014-05-01"), as.Date("2014-10-31"), by = "week"))
week15 <- data.frame(seq(as.Date("2015-05-01"), as.Date("2015-10-31"), by = "week"))
week16 <- data.frame(seq(as.Date("2016-05-01"), as.Date("2016-10-31"), by = "week"))
colnames(week13)[1] <- "Date"
colnames(week14)[1] <- "Date"
colnames(week15)[1] <- "Date"
colnames(week16)[1] <- "Date"
#ok, we can make a vector that consists of all the weeks in 2013-2017 that we want to have data for:
weeks <- rbind(week13, week14, week15, week16)
weeks[2]<-seq(1:nrow(weeks))
colnames(weeks)[2] <- "week_number"

