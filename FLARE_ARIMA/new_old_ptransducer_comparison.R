# script to look at how the new diana pressure transducer and the old wvwa pressure transducer compare

library(tidyverse)
library(lubridate)

# download the latest diana weir file
#download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','FCRweir.csv')

dianaheader<-read.csv("./FLARE_ARIMA/FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
dianadata<-read.csv("./FLARE_ARIMA/FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(dianadata)<-names(dianaheader) #combine the names to deal with Campbell logger formatting
dianadata$TIMESTAMP <- as.POSIXct(dianadata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
colnames(dianadata)[colnames(dianadata)=="Lvl_psi"] <- "diana_psi_corr"
dianadata <- dianadata %>% select("TIMESTAMP", "diana_psi_corr")

# download the latest wvwa weir file
#download.file('https://github.com/CareyLabVT/Reservoirs/blob/master/Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV/FCR_15min_Inf_20190429.csv', 'FCR_15min_Inf_20190429.csv')
wvwadata <- read.csv("./FLARE_ARIMA/FCR_15min_Inf_20190429.csv", skip = 28, header = T)
colnames(wvwadata)[colnames(wvwadata)=="Date.Time"] <- "TIMESTAMP"
wvwadata <- wvwadata %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwadata$TIMESTAMP <- floor_date(wvwadata$TIMESTAMP, "15 minutes")
colnames(wvwadata)[colnames(wvwadata)=="Pressure.psi."] <- "wvwa_psi_uncorr"
wvwadata <- wvwadata %>% select("TIMESTAMP", "wvwa_psi_uncorr")


# wvwa psi needs to be corrected for barometric pressure, so pull in WVWA DO sonde barometric pressure data
wvwa_bp <- read.csv("./FLARE_ARIMA/FCR_WVWA_BarometricPressure_20190429.csv", skip = 28, header =T)
colnames(wvwa_bp)[colnames(wvwa_bp)=="Date.Time"] <- "TIMESTAMP"
wvwa_bp <- wvwa_bp %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwa_bp$TIMESTAMP <- floor_date(wvwa_bp$TIMESTAMP, "15 minutes")
colnames(wvwa_bp)[colnames(wvwa_bp)=="Pressure.psi."] <- "wvwa_bp_psi"
wvwa_bp <- wvwa_bp %>% select("TIMESTAMP", "wvwa_bp_psi")

# a check to see if the met station barometric pressure is comparable to the wvwa sonde barometric pressure
download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','FCRmet.csv')
metheader<-read.csv("FCRmet.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
metdata<-read.csv("FCRmet.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(metdata)<-names(metheader) #combine the names to deal with Campbell logger formatting
metdata <- metdata %>% mutate(met_bp_psi = BP_kPa_Avg*0.145038)
metdata <- metdata %>% select("TIMESTAMP", "met_bp_psi")
metdata$TIMESTAMP <- as.POSIXct(metdata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")


# join the three together based on TIMESTAMP
baro_correct <- left_join(wvwadata, metdata)
baro_correct <- left_join(baro_correct, wvwa_bp)
baro_correct <- baro_correct %>% mutate(wvwa_psi_corr_met = wvwa_psi_uncorr - met_bp_psi) %>% 
  mutate(wvwa_psi_corr_sonde = wvwa_psi_uncorr - wvwa_bp_psi)


weir <- left_join(dianadata, baro_correct)
weir <- na.omit(weir)

# visualize the data
plot(weir$TIMESTAMP, weir$met_bp_psi, type = 'l', ylim = c(13.6, 14.9), main = 'Figure 1')
points(weir$TIMESTAMP, weir$wvwa_bp_psi, type  = 'l', col = 'red')
legend('center', c('met BP', 'wvwa BP'), col = c('black', 'red'), lty = c(1,1), bty = 'n')

plot(weir$met_bp_psi, weir$wvwa_bp_psi, main = 'Figure 2')
abline(a=0,b=1)
lm(weir$wvwa_bp_psi~weir$met_bp_psi)

plot(weir$diana_psi_corr, weir$wvwa_psi_corr_sonde, type = 'l', xlab = 'Diana psi', ylab = 'WVWA psi, BP correction from WVWA sonde')
plot(weir$diana_psi_corr, weir$wvwa_psi_corr_met, type = 'l', xlab = 'Diana psi', ylab = 'WVWA psi, BP correction from met station')
plot(weir$TIMESTAMP, weir$diana_psi_corr, type = 'l', ylim = c(0.4, 0.52))
points(weir$TIMESTAMP, weir$wvwa_psi_corr_sonde, col = 'blue', type = 'l')
legend('center', c('Diana', 'WVWA'), col = c('black', 'blue'), lty = c(1,1), bty = 'n')
plot(weir$TIMESTAMP, weir$wvwa_psi_corr_met, col = 'blue', type = 'l')

plot(weir$wvwa_psi_corr_met, weir$wvwa_psi_corr_sonde)
# well these do not match up well...

# calculate discharge from psi
# using numbers from MEL's inflow aggregation script
weir <- weir %>% mutate(diana_flow1 = (diana_psi_corr )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(diana_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (diana_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(diana_flow_cms = diana_flow_cfs*0.028316847   )

weir <- weir %>% mutate(wvwa_flow1 = wvwa_psi_corr_sonde*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(wvwa_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (wvwa_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(wvwa_flow_cms = wvwa_flow_cfs*0.028316847)

plot(weir$TIMESTAMP, weir$diana_flow_cms, type = 'l', ylim= c(0.124, 0.24))
points(weir$TIMESTAMP, weir$wvwa_flow_cms, type = 'l', col = 'red')

plot(weir$diana_flow_cms, weir$wvwa_flow_cms)
summary(lm(weir$diana_flow_cms~weir$wvwa_flow_cms))

#calculate daily means of both discharge estimates
daily <- weir %>% group_by()

# what is the magnitude of past inflow data? bring in older data file
hist_inf <- read.csv("C:/Users/wwoel/Dropbox/Historical_TimeSeries/FCR/Inflow/inflow_2013_2018.csv")
hist_inf$DateTime <- as.POSIXct(hist_inf$DateTime, format = "%Y-%m-%d %H:%M:%S")
plot(hist_inf$DateTime, hist_inf$Flow_cms)
wvwa_baro <- hist_inf %>% select("DateTime", "Baro_pressure_psi")
colnames(wvwa_baro)[colnames(wvwa_baro)=="DateTime"] <- "TIMESTAMP"

# compare barometric pressure
baromet <- left_join(catdata, wvwa_baro )
par(mfrow = c(1,2))
plot(baromet$TIMESTAMP, baromet$Baro_pressure_psi, col= 'red')
plot(baromet$TIMESTAMP, baromet$cat_baro_psi)
