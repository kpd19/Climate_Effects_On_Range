library(tidyverse)
library(geonames)

options(geonamesUsername="kpdixon")

loc_data <- read_csv("/Volumes/My Book/Synchrony/_data/trap_locations.csv")

elev <- c()
for(i in 1:dim(loc_data[1])){
  elev[i] <- GNsrtm3(loc_data$lat[i],loc_data$lon[i])  
} 
loc_data$elev2 <- unlist(elev)

loc_data %>% ggplot() + aes(x = elev, y = elev2) + geom_point() + theme_classic()

write_csv(loc_data, "/Volumes/My Book/Synchrony/_data/trap_locations2.csv")


## IDAHO REFORMATING

loc_data <-  read_csv("_gendata/_ID/idaho_summary_stats_long.csv")

elev <- c()
for(i in 1:dim(loc_data[1])){
  elev[i] <- GNsrtm3(loc_data$lat[i],loc_data$lon[i])  
}
loc_data$elev2 <- unlist(elev)

write_csv(loc_data, "_gendata/_ID/idaho_summary_stats_elev.csv")


## BRITISH COLUMBIA REFORMATING

loc_data <-  bc_info

elev <- c()
for(i in 1:dim(loc_data[1])){
  elev[i] <- GNsrtm3(loc_data$lat[i],loc_data$lon[i])  
}
loc_data$elev2 <- unlist(elev)

write_csv(loc_data, "_gendata/bc_loc_info_elev.csv")


## WASHINGTON & OREGON REFORMATING

loc_data <-  years_trapped2

elev <- c()
for(i in 1:dim(loc_data[1])){
  elev[i] <- GNsrtm3(loc_data$lat[i],loc_data$lon[i])  
}
loc_data$elev2 <- unlist(elev)

write_csv(loc_data, "_gendata/updated_WAOR_info_elev.csv")

loc_data <-  read_csv("_gendata/_ID/idaho_summary_stats_long.csv")
elev_data <-  read_csv("_gendata/_ID/idaho_summary_stats_elev.csv")
elev_data <- elev_data %>% select(manual_id,elev2)

loc_data <- merge(loc_data,elev_data)
write_csv(loc_data, "_gendata/_ID/idaho_summary_stats_elev.csv")


##########################

loc_data <- read_csv("/Volumes/My Book/Synchrony/_model_dat/forest_comp_usaca_nbtrap0_gc.csv")

elev <- c()
for(i in 1:dim(loc_data[1])){
  elev[i] <- GNsrtm3(loc_data$lat[i],loc_data$lon[i])  
}
loc_data$elev2 <- unlist(elev)

loc_dat <- loc_data %>% select(lat,lon,manual_id,PlotName,elev2)

#loc_data %>% ggplot() + aes(x = elev, y = elev2) + geom_point() + theme_classic()

write_csv(loc_data, "/Volumes/My Book/Synchrony/_model_dat/tnb_elev.csv")
