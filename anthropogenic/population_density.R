library(tidyverse)
library(raster)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/anthropogenic/")

latlong <- read_csv("../landscape/data/all_habitat_features.csv")

`%ni%` <- Negate(`%in%`)

pop_density <- raster("/Volumes/My Book/Synchrony/spatial/gpw_v4_population_density_rev11_2020_30s.tif")
crop_extent = extent(-130,-100,30,55)

pop_density_na <- crop(pop_density,crop_extent)

plot(pop_density_na) 

ll_info <- latlong %>% dplyr::select(lat,lon,manual_id,source,elev2)
ll_info$df_id <- 1:length(ll_info$lat)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, crs(pop_density_na))

plot(pop_density_na)
points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(pop_density_na,sites_transformed, buffer = 3000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_long <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_pop_long <- sp_pop_long %>% rename(df_id = name,number = value)

sp_pop_long <- merge(sp_pop_long,ll_info,all = TRUE)

head(sp_pop_long)

pop_summary <- sp_pop_long %>%
  group_by(lat,lon,manual_id,source, elev2) %>% summarize(avg_density = mean(number,na.rm=TRUE),
                                                          sd_density = sd(number,na.rm=TRUE)) 


pop_summary %>% drop_na(avg_density) %>% mutate(avg_density = ifelse(avg_density ==0, 0.1,avg_density)) %>% 
  ggplot() + aes(x = lon, y = lat, color = log10(avg_density)) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(option = 'turbo')

write_csv(pop_summary,'data/population_density.csv')
