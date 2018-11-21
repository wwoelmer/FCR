# read in selected correlation matrices for each year and entire dataset to pair selected variables back with dataset
# and then run iteratire linear model selection

#install.packages("MuMIn")
library(MuMIn)
library(knitr)


#all data
data <- read.csv("model_transformed_2013_2016.csv")


# 2013
var13 <- read.csv("./correlation matrices/2013_selected_correlations.csv")
var13$Date <- NA
var13 <- var13%>%select(Date, everything())
data13 <- data[,colnames(var13)]
data13$Date <- as.Date(data13$Date)
# subset to get rid of NA's at beginning?
data13 <- data13[data13$Date>"2013-06-20",]

# build a global model for 2013
model13 <- glm(Chla_sqrt~Chla_ARlag1_sqrt +Turb_NTU_log+NO3NO2_log+SRP_log+mean_flow+Temp_inf_max+Rain_sum_log+WindSpeed_mean_log
               +ShortWave_mean, data = data13, family = gaussian, na.action = "na.fail" )
glm13 <- dredge(model13, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select13 <- subset(glm13, delta<2 )

# models <2 units different AICc values have been selected
# now build those models individually and run summary statistics
mod1 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Turb_NTU_log, data = data13, family = gaussian, na.action = na.fail)
pred.1 <- predict(mod1, newdata=data13)
mod2 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+ShortWave_mean+Turb_NTU_log, data = data13, 
            family = gaussian, na.action = "na.fail")
pred.2 <- predict(mod2, newdata=data13)
mod3 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+Rain_sum_log+ShortWave_mean+Turb_NTU_log,data = data13, family = gaussian, na.action = na.fail)
pred.3 <- predict(mod3, newdata=data13)
mod4 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Temp_inf_max+Turb_NTU_log, data = data13, family = gaussian, na.action = "na.fail")
pred4 <- predict(mod4, newdata=data13)
mod5 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+Rain_sum_log+ShortWave_mean+Turb_NTU_log, data = data13, family = gaussian, na.action = "na.fail")
pred.5 <- predict(mod5, newdata=data13)

# attempt at calculating R2 deviation from the 1:1 line
r2.pred1 <- (1-((sum(data13$Chla_sqrt)-sum(pred.1))^2)/
               (sum(data13$Chla_sqrt)-mean(data13$Chla_sqrt))^2)
(1-(sum(data13$Chla_sqrt-pred.1))^2/(sum(data13$Chla_sqrt)-mean(data13$Chla_sqrt))^2)


plot(pred.1, type = 'l', col = "red", xlab = "Date", ylab = "Chla (ug/L)", ylim = c(0,3.3))
points(data13$Chla_sqrt, type = 'l', col = "black")
points(pred.2, type = 'l', col = 'orange')
points(pred.3, type = 'l', col = 'pink')
points(pred.4, type = 'l', col = 'yellow')
points(pred.5, type = 'l', col = 'green')
title("2013 models")

# make a table of model diagnostics and descriptions
table2013 = array(NA,dim=c(5,3))
row.names(table2013) = c('Mod 1','Mod 2','Mod 3', 'Mod 4', 'Mod 5')
colnames(table2013) = c("Model Equation", "AICc", "R2")
table2013[1,1] = "0.32Chla(t-1) - 3.6mean_flow - 0.002ShortWave_mean + 0.14Turbidity_log + 1.58"
table2013[1,2] = round(select13[1,13], digits = 2)
table2013[2,1] = "0.30Chla(t-1) - 3.9mean_flow - 0.002ShortWave_mean + 0.15Turbidity_log - 0.09NO3NO2_log + 1.7"
table2013[2,2] = round(select13[2,13], digits = 2)
table2013[3,1] = "0.33Chla(t-1) - 3.7mean_flow - 0.002ShortWave_mean + 0.13Turbidity_log + 0.017Rain_sum_log + 1.6"
table2013[3,2] = round(select13[3,13], digits = 2)
table2013[4,1] = "0.33Chla(t-1) - 3.4mean_flow - 0.003ShortWave_mean + 0.15Turbidity_log + 0.019Temp_inflow_max + 1.3"
table2013[4,2] = round(select13[4,13], digits = 2)
table2013[5,1] = "0.32Chla(t-1) - 4.0mean_flow - 0.002ShortWave_mean + 0.15Turbidity_log - 0.09NO3NO2_log + 0.016Rain_sum_log + 1.7"
table2013[5,2] = round(select13[5,13], digits = 2)
kable(table2013)

