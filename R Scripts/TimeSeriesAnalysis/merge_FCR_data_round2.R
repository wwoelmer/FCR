############## script to merge various datasets together to create dataframe for FCR time series ##############
library(tidyverse)
library(ggplot2)

#####################################################################################################################
############################ start with CTD and chem data  ########################################################
ctd <- read.csv("CTD/FCR_CTD_50_binned.csv") #ctd data for FCR at site 50, processed for layers
ctd<- ctd %>% select(-X, -Cond_uScm)
ctd$Date <- as.Date(ctd$Date)

chem <- read.csv("water_chemistry/FCR_chemistry.csv") #chemistry data for FCR at all sites
colnames(chem)[3] <- "Date"
#calculate TP/TN ratios
chem <- mutate(chem, TN_TP = TN_ugL/TP_ugL)
chem <- mutate(chem, NH4NO3_SRP = (NH4_ugL + NO3NO2_ugL)/SRP_ugL)
#subset chem to include only Site 50
chem50 <- chem[chem$Site==50,]


########### select only inflow data, format as new columns to be added separately to chem as inflow data#################
chem_inflow <- chem[chem$Site==100,]
colnames(chem_inflow) <- c("Reservoir", "Site", "Date", "Depth_m", "TN_inf","TP_inf", "NH4_inf", "NO3NO2_inf", 
                           "SRP_inf", "DOC_inf", "Flag_TN", "Flag_TP", "Flag_NH4", "Flag_NO3NO2", "Flag_SRP",
                           "Flag_DOC", "TN_TP_inf", "NH4NO3_SRP_inf" )
chem_inflow <- chem_inflow %>% select(-Reservoir, -Depth_m, -Site, -(Flag_TN:Flag_DOC))
# use left join because i only want data when there is chlorophyll data
join_chem <- left_join(chem50, chem_inflow, by = c("Date"), all = TRUE) #this is all chemistry data from site 50 and inflow (site100)
join_chem <- join_chem %>% select(-(Flag_TN:Flag_DOC), -Reservoir)

# now put together chem and CTD data
join_chem$Date <- as.Date(join_chem$Date)
join1 <- left_join(ctd, join_chem, by = c("Date", "Depth_m", "Site"), all = TRUE)

#clean up the columns
join1 <- join1 %>% select(-Reservoir)
#rename depths column
colnames(join1)[3] <- "Depth"

# look at some data for fun
ggplot(join1, aes(x = TP_ugL, y = Chla_ugL)) +
  geom_point() + ylim(c(0,100))
ggplot(join1[join1$Depth<1,], aes(x = TP_inf, y = Chla_ugL)) +
  geom_point() + ylim(c(0,10))


#####################################################################################################################
############################ now add in fluoroprobe data to join1 (CTD and chem)  ########################################################

fluoro <- read.csv("Fluoroprobe/Fluoro_FCR50_2014_2017.csv")
#remove 'X' column
fluoro <- fluoro %>% select(-X)
fluoro$Date <- as.Date(fluoro$Date)
join1$Date <- as.Date(join1$Date)
join3 <- left_join(join1, fluoro)

####################################################################################################################################
############################ now add in kd (light extinction coefficient) data #####################################################################################

kd <- read.csv("YSI_PAR_SECCHI/FCR_Kd.csv")
kd$Date<- as.Date(kd$Date)
join4 <- left_join(join3, kd, by = c("Date"), all = TRUE)
join4 <- join4 %>% select(-X)


#########################################################################################################################################
############################ add inflow-created data: residence time, and nutrient load ########################################
inf <- read.csv("./Inflow/inflow_loads_wrt.csv")
inf$Date <- as.Date(inf$Date)
join5 <- left_join(join4, inf, by = "Date", all = TRUE)

#####################################################################################################################################

write.csv(join5, "FCR_VT_data_2013_2017.csv", row.names = FALSE)

