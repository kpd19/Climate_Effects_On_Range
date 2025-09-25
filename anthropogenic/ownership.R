library(tidyverse)
library(raster)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/anthropogenic/")

canada_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))
all_geo <- rbind(canada_data,us_data)

latlong <- read_csv("data/state_info.csv")

`%ni%` <- Negate(`%in%`)

forest_ownership <- raster("/Volumes/My Book/Forest/ForestOwnership/US_forest_ownership.tif")

ownership_df <- data.frame(number = 0:8, ownership = c('Unknown Forest', 'Non-Forest',
                                                       'Water', 'Family (Private)', 'Corporate/Other (Private)',
                                                       'Tribal','Federal (Public)',
                                                       'State (Public)', 'Local (Public)'))

ll_info <- latlong %>% filter(country == "United States of America") 
ll_info$df_id <- 1:length(ll_info$lat)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, crs(forest_ownership))

plot(forest_ownership)
points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(forest_ownership,sites_transformed, buffer = NULL, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_long <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_pop_long <- sp_pop_long %>% rename(df_id = name,number = value)

sp_pop_long <- merge(sp_pop_long,ll_info,all = TRUE)

sp_pop_long <- merge(sp_pop_long,ownership_df)

head(sp_pop_long)

sp_pop_long %>% 
  ggplot() + aes(x = lon, y = lat, color = ownership) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ownership)

write_csv(sp_pop_long,'data/usa_forest_ownership')

###################
###################
###################
###################
###################

can_ownership <- raster("/Volumes/My Book/Forest/Canada_MFv2020/Canada_MFv2020.tif")

ownership_ca <- data.frame(number = c(0, 100,20,40,32,31,33,50,13,12,11),
                           ownership = c('Non-Forest','Water','State (Public)','Tribal','Tribal',
                                         'Federal (Public)','State (Public)','Corporate/Other (Private)','Unknown Forest',
                                         'Federal (Public)','Federal (Public)'),
                           ownership2 = c('Non-Forest',"Water",'Protected','Treaty/Settlement','Tribal',
                                          'Federal Reserve', 'Restricted','Private',
                                          'Other','Short-term tenure','Long-term tenure'))

ownership_df <- data.frame(number = 0:8, ownership = c('Unknown Forest', 'Non-Forest',
                                                       'Water', 'Family (Private)', 'Corporate/Other (Private)',
                                                       'Tribal','Federal (Public)',
                                                       'State (Public)', 'Local (Public)'))

ll_info_ca <- latlong %>% filter(country == "Canada") 
ll_info_ca$df_id <- 1:length(ll_info_ca$lat)

sites <- ll_info_ca %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed_ca <-spTransform(sites, crs(can_ownership))

plot(can_ownership)
points(sites_transformed_ca, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(can_ownership,sites_transformed_ca, buffer = NULL, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_long <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_pop_long <- sp_pop_long %>% rename(df_id = name,number = value)

sp_pop_long <- merge(sp_pop_long,ll_info_ca,all = TRUE)

unique(sp_pop_long$number)

sp_pop_long <- merge(sp_pop_long,ownership_ca)

sp_pop_long %>% 
  ggplot() + aes(x = lon, y = lat, color = ownership) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ownership)

write_csv(sp_pop_long,'data/can_forest_ownership')


canada <- read_csv('data/can_forest_ownership')
usa <- read_csv('data/usa_forest_ownership')

canada <- canada %>% dplyr::select(-ownership2)

ownership <- rbind(canada,usa)

ownership %>% 
  ggplot() + aes(x = lon, y = lat, color = ownership) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ownership)
