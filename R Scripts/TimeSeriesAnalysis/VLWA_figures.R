library(tidyverse)

data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)

# read in correlation matrix for selected predictable variable names
varall <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_selected_predictable.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09",]
# because rmse retains native units, create a chl column that is not square rooted
dataall <- dataall %>% mutate(Chla_ugL = (Chla_sqrt)^2)


# build a global model with all possible predictor variables
model_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  mean_flow + AirTemp_mean_log + WindSpeed_mean_log + 
                    +RelHum_median + ShortWave_mean,
                  data = dataall, family = gaussian, na.action = 'na.fail')
glm_1316 <- dredge(model_1316, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_1316 <- subset(glm_1316, delta<2 )

# now build selected models
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')
# make predictions
pred1_1316 <- predict(mod1_1316, newdata = dataall)

# plot the predictions for the entire dataset
plot(dataall$Date, dataall$Chla_ugL, type = 'l')
points(dataall$Date, (pred1_1316)^2, col = 'red', type = 'l')

# subset each of the years so the plot is still 2013-2016 but each line stops in october
# and doesn't connect to the may of the following year??

a <- data[data$Date < "2013-12-01",]
a$Date <- as.Date(a$Date)

b <- data[data$Date < "2014-12-01" & data$Date > "2013-12-01",]
b$Date <- as.Date(b$Date)

c <- data[data$Date < "2015-12-01" & data$Date > "2014-12-01",]
c$Date <- as.Date(c$Date)

d <- data[data$Date < "2016-12-01" & data$Date > "2015-12-01",]
d$Date <- as.Date(d$Date)

plot(dataall$Date, (dataall$Chla_sqrt)^2, type = 'n', xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2, lwd = 2, type = 'l')
points(b$Date, (b$Chla_sqrt)^2, lwd = 2, type = 'l')
points(c$Date, (c$Chla_sqrt)^2, lwd = 2, type = 'l')
points(d$Date, (d$Chla_sqrt)^2, lwd = 2, type = 'l')
points(dataall$Date, (pred1_1316)^2, col = 'orangered3', type = 'l', lwd = 2)
legend('topleft', c('Observed', 'ARIMA Modeled'), lty = c(1,1), col = c('black', 'orangered3'), bty = 'n')



