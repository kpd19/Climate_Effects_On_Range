library(tidyverse)
library(sf)

sf_use_s2(FALSE)

canada_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming",
                                           "North Dakota",'South Dakota','Nebraska','Texas'))

latlong <- read_csv("../landscape/data/all_habitat_features.csv")

all_geo <- rbind(canada_data,us_data)

ggplot() + geom_sf(data = all_geo, aes(geometry = geometry))

ll_info <- latlong %>% dplyr::select(lat,lon,manual_id,source,elev2)

ll_info <- st_as_sf(ll_info,coords = c('lon','lat'), crs = st_crs(all_geo))

intersection <- apply(st_intersects(all_geo, ll_info, sparse = FALSE), 2, 
                      function(col) { 
                        all_geo[which(col), ]$NAME_1
                      })

latlong2$state <- unlist(lapply(intersection,function(x) if(identical(x,character(0))) NA else x))

latlong2 <- latlong2 %>% mutate(state = ifelse(is.na(state),'Ocean',state))
latlong2 <- latlong2 %>% mutate(country =
                                  ifelse(state %in% c("Alberta",'British Columbia','Saskatchewan'),
                                         'Canada',"United States of America")) %>% 
  mutate(country = ifelse(state %in% c("Ocean"),
                      'Ocean',country))

latlong2 <- latlong2 %>% select(lat,lon,manual_id,state,source,elev2,country)

latlong2 %>% ggplot() + aes(x= lon, y = lat, color = state) + geom_point() +
  theme_classic()

latlong2 %>% ggplot() + aes(x= lon, y = lat, color = country) + geom_point() +
  theme_classic()

write_csv(latlong2,"data/state_info.csv")
