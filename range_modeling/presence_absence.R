library(tidyverse)
library(geodata)
library(sf)

canada_data = gadm(country="CAN", level = 1, path = tempdir())
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
bc_data =st_as_sf(bc_data)

us_data = gadm(country="USA", level = 1, path = tempdir())
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
state_data = st_as_sf(state_data)
state_data = fortify(state_data)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/population_data")

population_records <- read_csv("data/population_records.csv")
locations <- read_csv("data/range_locations.csv")
habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")
nearby_coords <- read_csv("data/nearby_coords_all.csv")

habitat_coords <- habitat_features %>% mutate(lat_coord = round(lat/0.25)*0.25,
                            lon_coord = round(lon/0.25)*0.25) %>% 
  group_by(lat_coord,lon_coord) %>% count(lon_coord) %>% select(-n)

population_records %>% ggplot() + aes(x = lon,y = lat, color = source) + 
  geom_point() + facet_wrap(~year)

population_records %>% filter(year >=1980) %>% mutate(period = ifelse(year <= 2005,"1980-2005","2006-2023")) %>% 
  group_by(lat_coord,lon_coord,period) %>% count(lat) %>% 
  ggplot() + aes(x = lon_coord,y = lat_coord, color = period) + geom_tile(color = 'black') + 
  theme_classic(base_size = 15) + 
  facet_wrap(~period)

present_early_pop <- population_records %>% filter(year >= 1980, year <= 2005) %>% filter(manual_id %ni% c(50445,50381,50223))

uni_early <- present_early_pop %>% mutate(n = 1) %>% group_by(lat_coord,lon_coord) %>% count()

habitat_coords %>% ggplot() + aes(x = lon_coord, y = lat_coord) +
  geom_tile(fill = 'yellow',color = 'yellow') +
  theme_classic(base_size = 15) +
  geom_point(data = uni_early,aes(x = lon_coord, y = lat_coord))

coords_early <- habitat_coords

coords_early$track <- "a"

for(i in 1:length(coords_early$lat_coord)){
  
  in_track = 0
  near_track = 0
  near_track2 = 0
  near_track3 = 0
  
  for(j in 1:length(uni_early$lat_coord)){
    
    temp_lat <- abs(coords_early$lat_coord[i] - uni_early$lat_coord[j])
    temp_lon <- abs(coords_early$lon_coord[i] - uni_early$lon_coord[j])
    
    if(temp_lat == 0 & temp_lon == 0){
      in_track = in_track + 1
    } else if (temp_lat <= 0.25 & temp_lon <= 0.25){
      near_track = near_track + 1
    } else if (temp_lat <= 0.5 & temp_lon <= 0.5){
      near_track2 = near_track2 + 1
    } else if (temp_lat <= 0.75 & temp_lon <= 0.75){
      near_track3 = near_track3 + 1
    }
  }
  
  if(in_track >0){
    coords_early$track[i] <- 'in'
  } else if (near_track >0){
    coords_early$track[i] <- 'near-1'
  } else if (near_track2 >0){
    coords_early$track[i] <- 'near-2'
  }else if (near_track3 >0){
    coords_early$track[i] <- 'near-3'
  }
  else{
    coords_early$track[i] <- 'out'
  }
  
}

pdf("figures/early_coordinates.pdf", height = 8, width = 12)
coords_early %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = track, color = track) + geom_tile() + theme_classic() + 
  geom_point(data = uni_early, aes(lon_coord,lat_coord), fill = NA, color = 'black') +
  scale_color_brewer(palette = 'Set1')+
  scale_fill_brewer(palette = 'Set1')
dev.off()

present_late_pop <- population_records %>% filter(year >= 2006) %>% filter(manual_id %ni% c(50445,50381,50223))

uni_late <- present_late_pop %>% mutate(n = 1) %>% group_by(lat_coord,lon_coord) %>% count()

coords_late <- habitat_coords

coords_late$track <- "a"

for(i in 1:length(coords_late$lat_coord)){
  
  in_track = 0
  near_track = 0
  near_track2 = 0
  near_track3 = 0
  
  for(j in 1:length(uni_late$lat_coord)){
    
    temp_lat <- abs(coords_late$lat_coord[i] - uni_late$lat_coord[j])
    temp_lon <- abs(coords_late$lon_coord[i] - uni_late$lon_coord[j])
    
    if(temp_lat == 0 & temp_lon == 0){
      in_track = in_track + 1
    } else if (temp_lat <= 0.25 & temp_lon <= 0.25){
      near_track = near_track + 1
    } else if (temp_lat <= 0.5 & temp_lon <= 0.5){
      near_track2 = near_track2 + 1
    } else if (temp_lat <= 0.75 & temp_lon <= 0.75){
      near_track3 = near_track3 + 1
    }
  }
  
  if(in_track >0){
    coords_late$track[i] <- 'in'
  } else if (near_track >0){
    coords_late$track[i] <- 'near-1'
  } else if (near_track2 >0){
    coords_late$track[i] <- 'near-2'
  }else if (near_track3 >0){
    coords_late$track[i] <- 'near-3'
  }
  else{
    coords_late$track[i] <- 'out'
  }
  
}

pdf("figures/late_coordinates.pdf", height = 8, width = 12)
plt1 <- ggplot() + geom_tile(data = coords_late, aes(x = lon_coord, y = lat_coord, fill = track, color = track)) +
  theme_classic() + 
  geom_point(data = uni_late, aes(lon_coord,lat_coord), fill = NA, color = 'black') +
  scale_color_brewer(palette = 'Set1')+
  scale_fill_brewer(palette = 'Set1') 
  
  
plt1 + geom_sf(data = state_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = bc_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(30,54), xlim = c(-128,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top')
dev.off()

habitat_unique <- habitat_features %>% select(-c(manual_id,source))
habitat_unique <- habitat_unique[!duplicated(habitat_unique),]

habitat_unique2 <- merge(habitat_unique,locations)

locations <- locations %>% mutate(lat_coord = round(lat/0.25)*0.25,
                     lon_coord = round(lon/0.25)*0.25) 

synthetic_locations <- merge(locations, coords_early)
synthetic_locations <- synthetic_locations %>% rename(track_early = track)
synthetic_locations <- merge(synthetic_locations, coords_late)
synthetic_locations <- synthetic_locations %>% rename(track_late = track)
synthetic_locations <- synthetic_locations %>% filter(source == "Synthetic data") %>% 
  mutate(present = 0)

synthetic_habitat <- merge(synthetic_testing,habitat_unique)
synthetic_habitat <- synthetic_habitat %>% mutate(n_obs = 1)

present_early <- population_records %>% filter(year >= 1980, year <= 2005) %>%
  filter(manual_id %ni% c(50445,50381,50223)) %>% mutate(n = 1) %>% 
  group_by(lat,lon,source,manual_id,lat_coord,lon_coord) %>% summarize(n_obs = sum(n)) %>% 
  mutate(present = 1) %>% mutate(track_early = 'in', track_late = NA)

present_late <- population_records %>% filter(year >= 2006) %>%
  filter(manual_id %ni% c(50445,50381,50223)) %>% mutate(n = 1) %>% 
  group_by(lat,lon,source,manual_id,lat_coord,lon_coord) %>% summarize(n_obs = sum(n)) %>% 
  mutate(present = 1) %>% mutate(track_early = NA, track_late = 'in')

present_early_habitat <- merge(present_early,habitat_unique)
present_late_habitat <- merge(present_late,habitat_unique)

synthetic_early <- synthetic_habitat %>% filter(track_early != 'in')

early_dataset <- rbind(present_early_habitat,synthetic_early)

colnames(synthetic_early)[colnames(synthetic_early) %ni% colnames(present_early_habitat)]
colnames(present_early_habitat)[colnames(present_early_habitat) %ni% colnames(synthetic_early)]

early_dataset %>% filter(track_early %in% c("in",'near-1')) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() +
  theme_classic(base_size = 15) + 
  facet_wrap(~track_early)

synthetic_late <- synthetic_habitat %>% filter(track_late != 'in')

late_dataset <- rbind(present_late_habitat,synthetic_late)

late_dataset %>% #filter(track_late %in% c("in",'near-1')) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() +
  theme_classic(base_size = 15) + 
  facet_wrap(~track_late)

late_dataset %>% count(track_late)

write_csv(early_dataset,'data/')
