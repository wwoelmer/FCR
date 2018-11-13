# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

data <- read.csv("model_lag_2013_2016.csv")
