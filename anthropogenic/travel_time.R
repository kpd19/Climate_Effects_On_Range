library(tidyverse)
library(raster)
library(gridExtra)
library(sf)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/anthropogenic/")

canada_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))
all_geo <- rbind(canada_data,us_data)

latlong <- read_csv("../landscape/data/all_habitat_features.csv")

`%ni%` <- Negate(`%in%`)

travel_time <- raster("/Volumes/My Book/Synchrony/spatial/travel_time_to_cities_6.tif")
crop_extent = extent(-130,-100,30,55)

travel_time_na <- crop(travel_time,crop_extent)

ll_info <- latlong %>% dplyr::select(lat,lon,manual_id,source,elev2)
ll_info$df_id <- 1:length(ll_info$lat)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, crs(travel_time_na))

plot(travel_time_na)
points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(travel_time_na,sites_transformed, buffer = NULL, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_long <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_pop_long <- sp_pop_long %>% rename(df_id = name,number = value)

sp_pop_long <- merge(sp_pop_long,ll_info,all = TRUE)

head(sp_pop_long)

sp_pop_long %>% 
  ggplot() + aes(x = lon, y = lat, color = log10(number)) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(option = 'turbo')

write_csv(sp_pop_long,'data/travel_time_6.csv')


tt1 <- read_csv('data/travel_time_1.csv')
tt2 <- read_csv('data/travel_time_2.csv')
tt3 <- read_csv('data/travel_time_3.csv')
tt4 <- read_csv('data/travel_time_4.csv')
tt5 <- read_csv('data/travel_time_5.csv')
tt6 <- read_csv('data/travel_time_6.csv')


tt1 <- tt1 %>% mutate(size = '5,000,000 to 50,000,000 inhabitants')
tt2 <- tt2 %>% mutate(size = '1,000,000 to 5,000,000 inhabitants')
tt3 <- tt3 %>% mutate(size = '500,000 to 1,000,000 inhabitants')
tt4 <- tt4 %>% mutate(size = '200,000 to 500,000 inhabitants')
tt5 <- tt5 %>% mutate(size = '100,000 to 200,000 inhabitants')
tt6 <- tt6 %>% mutate(size = '50,000 to 100,000 inhabitants')

tt <- rbind(tt1,tt2,tt3,tt5,tt4,tt6)
tt <- tt %>% mutate(size = factor(size, levels = c('50,000 to 100,000 inhabitants',
                                                   '100,000 to 200,000 inhabitants',
                                                   '200,000 to 500,000 inhabitants',
                                                   '500,000 to 1,000,000 inhabitants',
                                                   '1,000,000 to 5,000,000 inhabitants', 
                                                   '5,000,000 to 50,000,000 inhabitants')))


plt_all <- tt %>% drop_na(number) %>% mutate(number = ifelse(number == 0, 1,number)) %>% 
  ggplot() + aes(x = lon, y = lat, color = log10(number)) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(option = 'turbo') + 
  facet_wrap(~size)


plt_min <- tt %>% drop_na(number) %>% filter(size != '50,000 to 100,000 inhabitants') %>% 
  group_by(lat,lon,manual_id,source,elev2) %>% summarize(min_time = min(number)) %>% 
  mutate(min_time = ifelse(min_time == 0, 1,min_time)) %>% 
  ggplot() + geom_point(aes(x = lon, y = lat, color = log10(min_time)), shape = 19, size = 1) +
  theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(expression(log[10]~'travel time'), option = 'turbo') +
  geom_sf(data = all_geo, aes(geometry =geometry), fill = NA, color = 'grey55', size = 1.2) +
  coord_sf(ylim = c(31,54), xlim = c(-128,-104))

pdf("figures/travel_time.pdf",height = 6, width = 6)
plt_min
dev.off()

min_travel <- tt %>% drop_na(number) %>% filter(size != '50,000 to 100,000 inhabitants') %>% 
  group_by(lat,lon,manual_id,source,elev2) %>% summarize(min_time = min(number))

write_csv(min_travel, 'data/min_travel_time_1-5.csv')