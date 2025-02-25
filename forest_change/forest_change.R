library(raster)
library(tidyverse)
library(sp)

`%ni%` <- Negate(`%in%`)

tree_files <- read_csv("data/file_info.csv")
latlong <- read_csv("../landscape/data/all_habitat_features.csv")

latlong %>% filter(source == 'iNaturalist', is.na(elev2))
latlong[latlong$manual_id %in% c(50315,50223),]$elev2 <- 0
latlong <- latlong %>% drop_na(elev2)
latlong %>% count(source)

latlong$df_id <- 1:length(latlong$lat)

sites <- latlong %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is

#plot(forest, xlim = c(-125,-110),ylim = c(30,55))
#points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

forest_presence <- c()
for(i in 1:length(tree_files$filename)){
  
  forest <- raster(paste0("data/",tree_files$filename[i]))
  
  a.time <- Sys.time()
  sp_forest = raster::extract(forest,sites, buffer = 3000, factors = TRUE)
  b.time <- Sys.time()
  print(i)
  print(b.time - a.time)
  
  sp_forest_long <- sp_forest %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
  sp_forest_long <- sp_forest_long %>% rename(df_id = name,number = value)
  
  sp_forest_long <- merge(sp_forest_long,latlong,all = TRUE)
  
  head(sp_forest_long)
  
  forest_summary <- sp_forest_long %>% mutate(number = ifelse(is.na(number),-1,number)) %>% 
    mutate(number2 = ifelse(number == 0,1,0)) %>% mutate(n = 1) %>% 
    group_by(lat,lon,manual_id,source) %>% 
    summarize(sum_n = sum(number2)) %>% mutate(present = ifelse(sum_n >0,1,0))
  
  forest_summary$tree_sp <- tree_files$species[i]
  forest_summary$time_period <- tree_files$year[i]
  
  forest_presence <- rbind(forest_presence,forest_summary)
  
}

write_csv(forest_presence,"data/forest_climate_change.csv")

