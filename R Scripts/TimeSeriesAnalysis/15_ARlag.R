# examine data wihtin astsa package to determine what time lag is appropriate for an auto-regressive time series model
# add in a new column in the dataset with the AR-lag response variable (chlorophyll t-?)

data <- read.csv("model_all_2013_2016.csv")

plot(data$Chla_interp, type="b") #time series plot of x with points marked as “o”
install.packages("astsa")
library(astsa) # See note 1 below
lag1.plot(data$Chla_interp,1) # Plots x versus lag 1 of x.
acf(data$Chla_interp, xlim=c(1,30)) # Plots the ACF of x for lags 1 to 19
xlag1=lag(data$Chla_interp,-15) # Creates a lag 1 of x variable. See note 2
y=cbind(data$Chla_interp,xlag1) # See note 3 below
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18


# subset to just surface chlorophyll data and remove major outlier
data2 <- data[data$Chla_interp<40,]
data2 <- data2[data$Depth==0.1,]
plot(data2$Chla_interp, type="b") #time series plot of x with points marked as “o”
lag1.plot(data2$Chla_interp,30) # Plots x versus lag 1 of x.
acf(data2$Chla_interp, xlim=c(1,25)) # Plots the ACF of x for lags 1 to 19
xlag1=lag(data2$Chla_interp,-1) # Creates a lag 1 of x variable. See note 2
y=as.data.frame(cbind(data2$Chla_interp,xlag1)) # See note 3 below
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18

# create an AR column (chlorophyll a at the previous time: ar lag = 1)

