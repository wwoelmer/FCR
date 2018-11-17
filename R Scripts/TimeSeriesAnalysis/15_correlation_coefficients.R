# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("Hmisc")
library(Hmisc)

data <- read.csv("model_transformed_2013_2016.csv")
# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data <- data %>%
  select(-Site, -Depth,-Date, -(week_julian:week_cum))



##############################################################################################################################################
####################  create correlation matrices for each year #########################################################################

x <- chart.Correlation(data, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data), type = "spearman")
spear <- cor$r
write.csv(spear, "correlation_matrix.csv", row.names = FALSE)

# do this again with the natural log of chlorophyll
data_log <- data%>% mutate(Chl_log = log(Chla_ugL))# %>%
  select(-Chla_ugL) %>%
  select(Chl_log, everything())

cor_log <- rcorr(as.matrix(data_log), type = "spearman")
spear_log <- cor_log$r
write.csv(spear_log, "correlation_matrix_chllog.csv", row.names = FALSE)


# look at 2013 only
dataall <- read.csv("model_lag_2013_2016.csv")
dataall$Date <- as.Date(dataall$Date)
data13 <- dataall[dataall$Date<"2013-12-31",]
data13 <- data13 %>% select(-Site, -Depth,-Date, -(week_julian:week_cum))

cor_13 <- rcorr(as.matrix(data13), type = "spearman")
spear_13 <- cor_13$r
write.csv(spear_13, "correlation_matrix_2013.csv", row.names = FALSE)

# take a look at some of the variables that are correlated vs. chl
plot(data_log$Chl_log)
plot(data$Date, data$NO3NO2_inf, type = 'l')

plot(data$Temp_C, data$Chla_ugL)
plot(data$TN_load, data$Chla_ugL)
plot(data$Temp_inf_max, data$Chla_ugL)
plot(data$Temp_inf_mean, data$Chla_ugL)
plot(data$Temp_inf_min, data$Chla_ugL)
plot(data$AirTemp_mean, data$Chla_ugL)
plot(data$AirTemp_mean)
plot(data13$NH4_inf, data13$Chla_ugL)
plot(data13$SRP_ugL, data13$Chla_ugL)

# windspeed max, mean or median
plot(data13$WindSpeed_max, data13$Chla_ugL)
plot(log(data13$WindSpeed_max), log(data13$Chla_ugL))
plot(data13$WindSpeed_mean, data13$Chla_ugL)
plot(data13$WindSpeed_median, data13$Chla_ugL)

#shortwave max or mean
plot(data13$ShortWave_max, data13$Chla_ugL)
plot(log(data13$ShortWave_max), log(data13$Chla_ugL))
plot(data13$ShortWave_mean, data13$Chla_ugL)

#relative humidity max, mean, or median
plot(data13$RelHum_max, data13$Chla_ugL)
plot(log(data13$RelHum_max), data13$Chla_ugL)
plot(data13$RelHum_mean, data13$Chla_ugL)
plot(data13$RelHum_median, data13$Chla_ugL)
plot(data13$Chla_ugL)
plot(data$Chla_ugL)

# airtemp mean, median, or max 
plot(data13$AirTemp_max, data13$Chla_ugL)
plot(data13$AirTemp_median, data13$Chla_ugL)
plot(data13$AirTemp_mean, data13$Chla_ugL)
# airtemp max or temp_c (water temp) or nitrate/nitrite variables
plot(data13$AirTemp_max, data13$Chla_ugL)
plot(data13$Temp_C, data13$Chla_ugL)
plot(log(data13$Temp_C), data13$Chla_ugL)


# airtemp max or nitrate/nitrite variables



# temp inflow meann, median, or max 
plot(data13$Temp_inf_mean, data13$Chla_ugL)
plot(data13$Temp_inf_min, data13$Chla_ugL)
plot(data13$Temp_inf_max, data13$Chla_ugL)

#flow max, min, or median
plot(log(data13$flow_max), data13$Chla_ugL)
plot(data13$flow_min, data13$Chla_ugL)
plot(data13$flow_median, data13$Chla_ugL)
plot(data13$mean_wrt, data13$Chla_ugL)
plot(data13$Chla_ugL)
plot(data13$flow_max)
hist(log(data13$flow_max))
hist(data13$Temp_C)
hist(log(data13$Chla_ugL))
hist(dataall$Chla_ugL)
