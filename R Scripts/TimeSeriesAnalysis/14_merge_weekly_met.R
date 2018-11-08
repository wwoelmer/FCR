# take the weekly dataset and add in meteorological data

data <- read.csv("modelweeks_2013_2016.csv")
met <- read.csv("./MET/Met_FCR_daily.csv")
met <- met %>% select(-X)

all <- left_join(data, met)
all <- all %>% select(-(week_julian:week_cum), everything())

write.csv(all, "model_all_2013_2016.csv", row.names = FALSE)
