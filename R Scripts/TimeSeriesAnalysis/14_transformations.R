library(tidyverse)

data <- read.csv("model_lag_2013_2016.csv")

################################################################################################################################################
############################ assess histograms of each variable to see if transformation is needed ######################################
# assess the entire dataset (2013-2016) so that all transformations are consistent among years

for(i in 1:ncol(data)) {
  hist(data[,i], main = paste("Histogram of",colnames(data)[i]))
}

#"Chla_ugL" SQUARE ROOT
#data <- data %>% mutate(Chla_log = log(Chla_ugL))
data <- data %>% mutate(Chla_sqrt = sqrt(Chla_ugL)) %>%
  select(-Chla_ugL)
      #hist(Chla_log)
      #hist(Chla_ugL)
      #hist(data$Chla_sqrt)
#"Chla_ARlag1" LOG     
data <- data %>% mutate(Chla_ARlag1_sqrt = sqrt(Chla_ARlag1)) %>%
  select(-Chla_ARlag1)
      #hist(data$Chla_ARlag1_sqrt)
#"Temp_C"         OK  
#"DO_mgL"           OK
#"Turb_NTU"         LOG
data <- data %>% mutate(Turb_NTU_log = log(Turb_NTU)) %>%
  select(-Turb_NTU)
      #hist(data$Turb_NTU_log)
      #hist(data$Turb_NTU)
#"SpCond_uScm"      OK--no transformations are helpful
      #data <- data %>% mutate(SpCond_log = log(SpCond_uScm))
      #data <- data %>% mutate(SpCond_sqrt = sqrt(SpCond_uScm))
      #data <- data %>% mutate(SpCond_cubert = (SpCond_uScm)^0.33)
      #hist(data$SpCond_cubert)
      #hist(data$SpCond_sqrt)
      #hist(data$SpCond_log)
      #hist(data$SpCond_uScm)
# "TP_ugL"       LOG
data <- data %>% mutate(TP_log = log(TP_ugL)) %>%
  select(-TP_ugL)
#hist(data$TP_log)
#"TN_ugL"        LOG
data <- data %>% mutate(TN_log = log(TN_ugL))%>% select(-TN_ugL)
#hist(data$TN_log)
#"NH4_ugL"       LOG
data <- data %>% mutate(NH4_log = log(NH4_ugL))%>%
  select(-NH4_ugL)
#hist(data$NH4_log)
#"NO3NO2_ugL"     LOG
data <- data %>% mutate(NO3NO2_log = log(NO3NO2_ugL)) %>%
  select(-NO3NO2_ugL)
#hist(data$NO3NO2_log)
#"SRP_ugL"        LOG?
data <- data %>% mutate(SRP_log = log(SRP_ugL)) %>%
  select(-SRP_ugL)
#hist(data$SRP_log)
#hist(data$SRP_ugL)
#"DOC_mgL"       LOG
data <- data %>% mutate(DOC_log = log(DOC_mgL))%>%
  select(-DOC_mgL)
#hist(data$DOC_log)
#hist(data$DOC_mgL)
#"TP_inf"        LOG
data <- data %>% mutate(TP_inf_log = log(TP_inf))%>%
  select(-TP_inf)
#hist(data$TP_inf_log)
#"TN_inf"           LOG
data <-  data %>% mutate(TN_inf_log = log(TN_inf)) %>%
  select(-TN_inf)
#hist(data$TN_inf_log)
#hist(data$TN_inf)
#"NH4_inf"         LOG
data <- data %>% mutate(NH4_inf_log = log(NH4_inf)) %>%
  select(-NH4_inf)
#hist(data$NH4_inf_log)
#hist(data$NH4_inf)
#"NO3NO2_inf"      OK
#data <- data %>% mutate(NO3NO2_inf_log = log(NO3NO2_inf))
#hist(data$NO3NO2_inf_log)
#"SRP_inf"    OK     
#data <- data %>% mutate(SRP_inf_log = log(SRP_inf))
#hist(data$SRP_inf_log)
#hist(data$SRP_inf)
#"DOC_inf"          LOG
data <- data %>% mutate(DOC_inf_log = log(DOC_inf))%>%
  select(-DOC_inf)
#hist(data$DOC_inf_log)
#hist(data$DOC_inf)
#"Total_chlorophyll" LOG
data <- data %>% mutate(Total_chlorophyll_log = log(Total_chlorophyll)) %>%
  select(-Total_chlorophyll)
#hist(data$Total_chloro_log)
#"Green_algae"     LOG
data <- data %>% mutate(greens_log = log(Green_algae)) %>%
  select(-Green_algae)
#hist(data$greens_log)
#"Cyanobacteria"    LOG
data <- data %>% mutate(cyano_log = log(Cyanobacteria))%>%
  select(-Cyanobacteria)
#hist(data$cyano_log)
#"Diatoms"         LOG 
data <- data %>% mutate(diatoms_log = log(Diatoms))%>%
  select(-Diatoms)
#hist(data$diatoms_log)
#"Cryptophyta"  LOG     
data <- data %>% mutate(crypto_log = log(Cryptophyta)) %>%
  select(-Cryptophyta)
#hist(data$crypto_log)
#"Kd"        OK
#"TN_TP"  LOG          
data <- data %>% mutate(TN_TP_log = log(TN_TP)) %>%
  select(-TN_TP)
#hist(data$TN_TP_log)
#"NH4NO3NO2_SRP"    LOG
data <- data %>% mutate(NH4NO3NO2_SRP_log = log(NH4NO3NO2_SRP))%>%
  select(-NH4NO3NO2_SRP)
#hist(data$NH4NO3NO2_SRP_log)
#"TN_TP_inf"        LOG
data <- data %>% mutate(TN_TP_inf_log = log(TN_TP_inf)) %>%
  select(-TN_TP_inf)
        #hist(data$TNTP_inf_log)
        #hist(data$TN_TP_inf)
        #"NH4NO3_SRP_inf"   OK
        #data <- data %>% mutate(NH4NO3NO2_SRP_inf_log = log(NH4NO3_SRP_inf))
        #hist(data$NH4NO3NO2_SRP_inf_log)
        #hist(data$NH4NO3_SRP_inf)
#"mean_flow"        OK
#"TN_load"         LOG
data <- data %>% mutate(TN_load_log = log(TN_load))%>%
  select(-TN_load)
      #hist(data$TN_load_log)
      #hist(data$TN_load)
# "TP_load"     LOG     
data <- data %>% mutate(TP_load_log = log(TP_load))%>%
  select(-TP_load)
#hist(data$TP_load_log)
#hist(data$TP_load)
#"NH4_load"  LOG       
data <- data %>% mutate(NH4_load_log = log(NH4_load))%>%
  select(-NH4_load)
      #hist(data$NH4_load_log)
      #hist(data$NH4_load)
#"NO3NO2_load"   OK
      #data <- data %>% mutate(NO3NO2_load_log = log(NO3NO2_load))
      #hist(data$NO3NO2_load_log)
#"SRP_load"         OK
      #data <- data %>% mutate(SRP_load_log = log(SRP_load))
      #hist(data$SRP_load_log)
      #hist(data$SRP_load)
#"DOC_load"     LOG    
data <- data %>% mutate(DOC_load_log = log(DOC_load))%>%
  select(-DOC_load)
    #hist(data$DOC_load_log)
#"mean_wrt"         LOG
data <- data %>% mutate(mean_wrt_log = log(mean_wrt))%>%
  select(-mean_wrt)
      #hist(data$mean_wrt_log)
      #hist(data$mean_wrt)
# "Temp_inf_mean"   OK
#"Temp_inf_max"     OK
#"Temp_inf_min"     OK
#"flow_max"      LOG   
data <- data%>% mutate(flow_max_log = (log(flow_max)))%>%
  select(-flow_max)
      #hist(data$flow_max_log)
      #hist(data$flow_max)
#"flow_min"         OK
#"flow_median"      OK
# "AirTemp_max"     LOG
data <- data %>% mutate(AirTemp_max_log = log(AirTemp_max))%>%
  select(-AirTemp_max)
      #hist(data$AirTemp_max_log)
      #hist(data$AirTemp_max)
#"AirTemp_mean"     LOG
data <- data %>% mutate(AirTemp_mean_log = log(AirTemp_mean))%>%
  select(-AirTemp_mean)
      #hist(data$AirTemp_mean_log)
      #hist(data$AirTemp_mean)
#"AirTemp_median"   LOG
data <- data %>% mutate(AirTemp_median_log = log(AirTemp_median))%>%
  select(-AirTemp_median)
      #hist(data$AirTemp_median_log)
      #hist(data$AirTemp_median)
#"Rain_sum"         LOG
data <- data %>% mutate(Rain_sum_log = log(Rain_sum))%>%
  select(-Rain_sum)
    #hist(data$Rain_sum_log)
#"RelHum_max"      LOG
data <- data %>% mutate(RelHum_max_log = log(RelHum_max))%>%
  select(-RelHum_max)
      #hist(data$RelHum_max_log)
      #hist(data$RelHum_max)
#"RelHum_mean"      OK
# "RelHum_median"    OK
#"ShortWave_max"     OK
      #data <- data %>% mutate(ShortWave_max_log = log(ShortWave_max))
      #data <- data %>% mutate(ShortWave_max_sqrt = sqrt(ShortWave_max))
      #hist(data$ShortWave_max_sqrt)
      #hist(data$ShortWave_max_log)
      #hist(data$ShortWave_max)
#"ShortWave_mean"   OK
      #data <- data %>% mutate(ShortWave_mean_log = log(ShortWave_mean))
      #hist(data$ShortWave_mean_log)
      #hist(data$ShortWave_mean)
#"WindSpeed_max"    LOG
data <- data %>% mutate(WindSpeed_max_log = log(WindSpeed_max))%>%
  select(-WindSpeed_max)
      #hist(data$WindSpeed_max_log)
#"WindSpeed_mean"  LOG 
data <- data %>% mutate(WindSpeed_mean_log = log(WindSpeed_mean))%>%
  select(-WindSpeed_mean)
      #hist(data$WindSpeed_mean_log)
#"WindSpeed_median" LOG
data <- data %>% mutate(WindSpeed_median_log = log(WindSpeed_median))%>%
  select(-WindSpeed_median)


# rearrange varibales
data <- data %>% select(Date:Depth, Chla_sqrt, Chla_ARlag1_sqrt, Temp_C:SpCond_uScm, Turb_NTU_log, Kd, TP_log:DOC_inf_log, 
                        NO3NO2_inf:SRP_load, TN_TP_log:flow_max_log, flow_min:flow_median, mean_flow, Temp_inf_mean:Temp_inf_min,
                        Total_chlorophyll_log:crypto_log, RelHum_max_log, AirTemp_max_log:windspeed_median_log, everything())

write.csv(data, "model_transformed_2013_2016.csv", row.names = FALSE)