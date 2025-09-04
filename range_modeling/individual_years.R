library(tidyverse)
library(gridExtra)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/range_modeling/")

population_records <- read_csv("../population_data/data/population_records_1947-2024.csv")

locations <- read_csv("../population_data/data/range_locations_1947_2024.csv")

habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")

habitat_coords <- habitat_features %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                              lon_coord = round(lon/0.25)*0.25) %>% 
  group_by(lat_coord,lon_coord) %>% count(lon_coord) %>% select(-n)

population_records %>% ggplot() + aes(x = lon,y = lat, color = source) + 
  geom_point() + facet_wrap(~year)

plt1 <- population_records %>% filter(year >=1985) %>% mutate(period = ifelse(year <= 2010,"1985-2010","2011-2024")) %>% 
  group_by(lat_coord,lon_coord,period) %>% count(lat) %>% 
  ggplot() + aes(x = lon_coord,y = lat_coord, color = period) + geom_tile(color = 'black') + 
  theme_classic(base_size = 15) + 
  facet_wrap(~period)

plt2 <- population_records %>% filter(year >=1990) %>% mutate(period = ifelse(year <= 2005,"1985-2010","2011-2024")) %>% 
  group_by(lat_coord,lon_coord,period) %>% count(lat) %>% 
  ggplot() + aes(x = lon_coord,y = lat_coord, color = period) + geom_tile(color = 'black') + 
  theme_classic(base_size = 15) + 
  facet_wrap(~period)

grid.arrange(plt1,plt2)

present_early_pop <- population_records %>% filter(year >= 1985, year <= 2010) %>% filter(manual_id %ni% c(50445,50381,50223))

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

present_late_pop <- population_records %>% filter(year >= 2011) %>% filter(manual_id %ni% c(50445,50381,50223))
present_late_pop2 <- population_records %>% filter(year >= 2011, source == "Defoliation surveys") %>%
  filter(manual_id == 50223)
present_late_pop <- rbind(present_late_pop, present_late_pop2)

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
coords_late %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = track, color = track) + geom_tile() + theme_classic() + 
  geom_point(data = uni_late, aes(lon_coord,lat_coord), fill = NA, color = 'black') +
  scale_color_brewer(palette = 'Set1')+
  scale_fill_brewer(palette = 'Set1')
dev.off()

locations <- locations %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                  lon_coord = round(lon/0.25)*0.25) 

synthetic_locations <- merge(locations, coords_early)

synthetic_locations <- synthetic_locations %>% rename(track_early = track)
synthetic_locations <- merge(synthetic_locations, coords_late)
synthetic_locations <- synthetic_locations %>% rename(track_late = track)
synthetic_locations <- synthetic_locations %>% filter(source == "Synthetic data") %>% 
  mutate(present = 0)

present_early_pop2 <- present_early_pop %>% mutate(present = 1, track_early = 'in')

synthetic_early <- synthetic_locations %>% filter(track_early != 'in')

prob_years_early <- present_early_pop %>% count(year) %>% 
  mutate(sum_n = sum(n)) %>% mutate(p = n/sum_n)

synthetic_early <- synthetic_early %>%
  mutate(year = sample(x = prob_years_early$year, size = length(synthetic_early$lat), replace = TRUE, prob = prob_years_early$p)) %>% 
  select(-track_late) %>% mutate(count = 0)

early_dataset_yrs <- rbind(present_early_pop2,synthetic_early)

early_dataset_yrs %>% filter(source == "Synthetic data") %>%  
  group_by(lat_coord,lon_coord,year) %>% count(lat_coord) %>% 
  ggplot() + aes(x = lon_coord,y = lat_coord, color = as.factor(n), fill = as.factor(n)) + geom_tile() + 
  theme_classic() + 
  facet_wrap(~year)

early_dataset_yrs %>% filter(track_early %in% c("in",'near-1','near-2','near-3')) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() +
  theme_classic(base_size = 15) + 
  facet_wrap(~track_early)

early_dataset_yrs %>% ungroup() %>% count(track_early)

early_dataset_yrs %>% group_by(present) %>% count(year) %>% 
  mutate(sum_n = sum(n)) %>% 
  ggplot() + aes(x = year, y = n/sum_n, group = present, color = as.factor(present), fill = as.factor(present)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_classic() + ylab("Proportion of dataset") +
  scale_color_brewer("Present", palette = "Set2") +
  scale_fill_brewer("Present", palette = "Set2")

present_late_pop2 <- present_late_pop %>% mutate(present = 1, track_late = 'in')

synthetic_late <- synthetic_locations %>% filter(track_late != 'in')

prob_years_late <- present_late_pop %>% count(year) %>% 
  mutate(sum_n = sum(n)) %>% mutate(p = n/sum_n)

synthetic_late <- synthetic_late %>%
  mutate(year = sample(x = prob_years_late$year, size = length(synthetic_late$lat), replace = TRUE, prob = prob_years_late$p)) %>% 
  select(-track_early) %>% mutate(count = 0)

late_dataset_yrs <- rbind(present_late_pop2,synthetic_late)

late_dataset_yrs %>% filter(source == "Synthetic data") %>%  
  group_by(lat_coord,lon_coord,year) %>% count(lat_coord) %>% 
  ggplot() + aes(x = lon_coord,y = lat_coord, color = as.factor(n), fill = as.factor(n)) + geom_tile() + 
  theme_classic() + 
  facet_wrap(~year)

late_dataset_yrs %>% group_by(present) %>% count(year) %>% 
  mutate(sum_n = sum(n)) %>% 
  ggplot() + aes(x = year, y = n/sum_n, group = present, color = as.factor(present), fill = as.factor(present)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_classic() + ylab("Proportion of dataset") +
  scale_color_brewer("Present", palette = "Set2") +
  scale_fill_brewer("Present", palette = "Set2")

write_csv(early_dataset_yrs,'data/training_dataset_1985-2010_yrs.csv')
write_csv(late_dataset_yrs,'data/testing_dataset_2011-2024_yrs.csv')


