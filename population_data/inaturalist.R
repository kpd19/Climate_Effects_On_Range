library(tidyverse)

inat <- read_csv("data/iNaturalist_Dec102024.csv")
lab <- read_csv("data/lab_obs.csv")

inat <- inat %>% filter(is.na(geoprivacy)) %>% drop_na(latitude)

inat <- inat %>% mutate(lat_round = round(latitude,digits = 3),
                lon_round = round(longitude,digits = 3),
                year = year(observed_on)) 

inat_locs <- inat %>% mutate(n = 1) %>% group_by(lat_round,lon_round) %>% 
  summarize(sum_n = sum(n)) %>% mutate(source = 'iNaturalist')

inat_small <- inat %>% dplyr::select(id,lat_round,lon_round,year,positional_accuracy) %>% mutate(source = 'iNaturalist')

lab_locs <- lab %>% mutate(lat_round = round(latitude,digits = 3),
                             lon_round = round(longitude,digits = 3)) %>% 
  mutate(n = 1) %>% group_by(lat_round,lon_round) %>% 
  summarize(sum_n = sum(n)) %>% mutate(source = 'Lab Collections')

all_obs <- rbind(inat_locs,lab_locs)

lab_small <- lab %>% mutate(lat_round = round(latitude,digits = 3),
                                 lon_round = round(longitude,digits = 3)) %>% 
  dplyr::select(lat_round,lon_round,year) %>% mutate(source = 'Lab Collections', id = 1:55, positional_accuracy = 1)
  
all_obs %>% ggplot() + aes(x = lon_round,y = lat_round, color = source) + geom_point() + theme_classic()

all_obs$manual_id <- 1:length(all_obs$sum_n)
all_obs$manual_id <- all_obs$manual_id + 40000

records <- rbind(inat_small,lab_small)

write_csv(all_obs,"data/inaturalist_locations.csv")
write_csv(records,"data/inaturalist_observations.csv")



