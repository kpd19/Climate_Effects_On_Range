library(tidyverse)
library(sf)

sf_use_s2(FALSE)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

can_states <- all_geo2 %>% filter(COUNTRY == "Canada") %>% pull(NAME_1)
can_states <- unique(can_states)

us_states <- all_geo2 %>% filter(COUNTRY == "United States") %>% pull(NAME_1)
us_states <- unique(us_states)

mex_states <- all_geo2 %>% filter(COUNTRY == "Mexico") %>% pull(NAME_1)
mex_states <- unique(mex_states)

ll_info <- latlong %>% dplyr::select(lat,lon,manual_id,source,elev2)

ll_info <- st_as_sf(ll_info,coords = c('lon','lat'), crs = st_crs(all_geo))

intersection <- apply(st_intersects(all_geo, ll_info, sparse = FALSE), 2, 
                      function(col) { 
                        all_geo[which(col), ]$NAME_1
                      })

latlong2$state <- unlist(lapply(intersection,function(x) if(identical(x,character(0))) NA else x))

latlong2 <- latlong2 %>% mutate(state = ifelse(is.na(state),'Ocean',state))
latlong2 <- latlong2 %>% mutate(country =
                                  case_when(state %in% can_states ~ "Canada",
                                            state %in% us_states ~ 'United States of America',
                                            state %in% mex_states ~ 'Mexico')) %>% 
  mutate(country = ifelse(state %in% c("Ocean"),
                      'Ocean',country))

latlong2 <- latlong2 %>% select(lat,lon,manual_id,state,source,elev2,country)

latlong2 %>% ggplot() + aes(x= lon, y = lat, color = state) + geom_point() +
  theme_classic()

latlong2 %>% ggplot() + aes(x= lon, y = lat, color = country) + geom_point() +
  theme_classic()

write_csv(latlong2,"data/state_info.csv")


########
########

lats <- seq(30,60,0.25)
lons <- seq(-130,-100,0.25)
gridded <- expand.grid(lats,lons)
gridded <- gridded %>% rename(lat = 'Var1', lon = 'Var2')

gridded_info <- st_as_sf(gridded,coords = c('lon','lat'), crs = st_crs(all_geo))

gridded_intersection <- apply(st_intersects(all_geo, gridded_info, sparse = FALSE), 2, 
                      function(col) { 
                        all_geo[which(col), ]$NAME_1
                      })

gridded$state <- unlist(lapply(gridded_intersection,function(x) if(identical(x,character(0))) NA else x))

gridded <- gridded %>% mutate(state = ifelse(is.na(state),'Ocean',state))
gridded <- gridded %>% mutate(country =
                                  case_when(state %in% can_states ~ "Canada",
                                            state %in% us_states ~ 'United States of America',
                                            state %in% mex_states ~ 'Mexico')) %>% 
  mutate(country = ifelse(state %in% c("Ocean"),
                          'Ocean',country))

gridded %>% ggplot() + aes(x = lon, y = lat, color = state) + geom_point() + theme_classic()
write_csv(gridded,"data/gridded_states.csv")