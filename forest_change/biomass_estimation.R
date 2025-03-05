library(tidyverse)
library(geodata)
library(geosphere)
library(gridExtra)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/forest_change/")

canada_data = gadm(country="CAN", level = 1, path = tempdir())
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
bc_data =st_as_sf(bc_data)

us_data = gadm(country="USA", level = 1, path = tempdir())
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
state_data = st_as_sf(state_data)

sf_use_s2(TRUE)

latlong <- read_csv("../landscape/data/all_habitat_features.csv")

latlong %>% filter(source == 'iNaturalist', is.na(elev2))
latlong[latlong$manual_id %in% c(50315,50223),]$elev2 <- 0
latlong %>% count(source)

forest_current_wide <- read_csv("data/forest_composition_2005.csv")
forest_2050_wide <- read_csv("data/forest_composition_2050.csv")
forest_2100_wide <- read_csv("data/forest_composition_2100.csv")

ll_biomass <- latlong %>% select(lat,lon,manual_id,source,elev2,mean_biomass,max_biomass) %>%
  mutate_at(c('mean_biomass'), ~replace_na(.,0))

fc_pres_only <- forest_current_wide %>% select(lat,lon,manual_id,source,time_period,type, host_trees)
fc_pres_only <- merge(fc_pres_only,ll_biomass)

f50_pres_only <- forest_2050_wide %>% select(lat,lon,manual_id,source,time_period,type, host_trees)
f50_pres_only <- merge(f50_pres_only,ll_biomass)

f100_pres_only <- forest_2100_wide %>% select(lat,lon,manual_id,source,time_period,type, host_trees)
f100_pres_only <- merge(f100_pres_only,ll_biomass)

fc_pres_only %>% ggplot() + aes(x = mean_biomass, group = host_trees, fill = host_trees) +
  geom_histogram(position = 'identity', alpha =0.5) + 
  theme_classic()

f50_pres_only %>% ggplot() + aes(x = mean_biomass, group = host_trees, fill = host_trees) +
  geom_histogram(position = 'identity', alpha =0.5) + 
  theme_classic()

f100_pres_only %>% ggplot() + aes(x = mean_biomass, group = host_trees, fill = host_trees) +
  geom_histogram(position = 'identity', alpha =0.5) + 
  theme_classic()

f50_pres_only2 <- f50_pres_only %>% select(lat,lon,manual_id,host_trees) %>% rename(host_trees_2050 = host_trees)
f100_pres_only2 <- f100_pres_only %>% select(lat,lon,manual_id,host_trees) %>% rename(host_trees_2100 = host_trees)

fc_pres_only2 <- merge(fc_pres_only,f50_pres_only2)
fc_pres_only2 <- merge(fc_pres_only2,f100_pres_only2)

fc_pres_only2 %>% filter(host_trees == 0, host_trees_2050 == 1) %>% 
  ggplot() + aes(x = lon, y= lat, color = mean_biomass) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo')

missing_biomass_2050 <- fc_pres_only2 %>% filter(host_trees == 0, host_trees_2050 == 1) %>% filter(mean_biomass == 0)
missing_biomass_2100 <- fc_pres_only2 %>% filter(host_trees == 0, host_trees_2100 == 1) %>% filter(mean_biomass == 0)

missing_biomass_ids <- unique(c(missing_biomass_2050$manual_id,missing_biomass_2100$manual_id))

present_locs <- fc_pres_only2 %>% filter(host_trees == 1, mean_biomass>0) %>% select(lat,lon,manual_id,mean_biomass,max_biomass)

xy <- cbind(present_locs$lon,present_locs$lat)

a.time <- Sys.time()
new_biomass_df <- c()
for(i in 1:length(missing_biomass_ids)){
  xy2 <- c(fc_pres_only2[fc_pres_only2$manual_id == missing_biomass_ids[i],]$lon,fc_pres_only2[fc_pres_only2$manual_id == missing_biomass_ids[i],]$lat)
  
  present_locs$temp_dist <- distm(xy,xy2)
  
  temp_df <- present_locs %>% arrange(temp_dist) %>% head(10) %>% mutate(dist_rank = 1:10)
  temp_df$manual_id <- missing_biomass_ids[i]
  
  new_biomass_df <- rbind(new_biomass_df,temp_df)
  
}
b.time <- Sys.time()
b.time-a.time

new_biomass_df %>% filter(dist_rank <=5) %>% ggplot() + aes(x = temp_dist/1000) + geom_histogram() + theme_classic(base_size = 15) 

new_biomass_df %>%  filter(dist_rank <=5) %>%
  ggplot() + aes(x = lon, y= lat, color = temp_dist) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo')

new_biomass_means <- new_biomass_df %>% filter(dist_rank <=5) %>% summarize(new_mean = mean(mean_biomass),
          new_max = mean(max_biomass))

forest_2050_wide <- merge(forest_2050_wide,ll_biomass)

for(i in 1:length(missing_biomass_2050$manual_id)){
  forest_2050_wide[forest_2050_wide$manual_id == missing_biomass_2050$manual_id[i],]$mean_biomass <- 
    new_biomass_df[new_biomass_df$manual_id == missing_biomass_2050$manual_id[i],]$new_mean
  
  forest_2050_wide[forest_2050_wide$manual_id == missing_biomass_2050$manual_id[i],]$max_biomass <- 
    new_biomass_df[new_biomass_df$manual_id == missing_biomass_2050$manual_id[i],]$new_max
}

forest_2100_wide <- merge(forest_2100_wide,ll_biomass)

for(i in 1:length(missing_biomass_2100$manual_id)){
  forest_2100_wide[forest_2100_wide$manual_id == missing_biomass_2100$manual_id[i],]$mean_biomass <- 
    new_biomass_df[new_biomass_df$manual_id == missing_biomass_2100$manual_id[i],]$new_mean
  
  forest_2100_wide[forest_2100_wide$manual_id == missing_biomass_2100$manual_id[i],]$max_biomass <- 
    new_biomass_df[new_biomass_df$manual_id == missing_biomass_2100$manual_id[i],]$new_max
}

forest_current_wide <- merge(forest_current_wide,ll_biomass)
forest_current_wide %>% filter(host_trees == 1, mean_biomass == 0) %>% 
  ggplot() + aes(x = lon, y= lat, color = mean_biomass) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo')

forest_current_wide %>% 
  ggplot() + aes(x = lon, y= lat, color = mean_biomass) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo')

present_missing <- forest_current_wide  %>% filter(host_trees == 1, mean_biomass == 0) %>% pull(manual_id)

a.time <- Sys.time()
pres_biomass_df <- c()
for(i in 1:length(present_missing)){
  xy2 <- c(forest_current_wide[forest_current_wide$manual_id == present_missing[i],]$lon,forest_current_wide[forest_current_wide$manual_id == present_missing[i],]$lat)
  
  present_locs$temp_dist <- distm(xy,xy2)
  
  temp_df <- present_locs %>% arrange(temp_dist) %>% head(10) %>% mutate(dist_rank = 1:10)
  temp_df$manual_id <- present_missing[i]
  
  pres_biomass_df <- rbind(pres_biomass_df,temp_df)
  
}
b.time <- Sys.time()
b.time-a.time

latlong %>% mutate(test = ifelse(mean_biomass == max_biomass,1,0)) %>% 
  ggplot() + aes(x = lon, y= lat, color = mean_biomass) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo') + 
  facet_wrap(~test)

present_locs %>% ggplot() + aes(x = mean_biomass, y = max_biomass) + geom_point() + 
  geom_abline(yintercept = 0, slope = 1, color = 'red')

pres_biomass_df %>% filter(dist_rank <=5) %>% ggplot() + aes(x = temp_dist/1000) + geom_histogram() + theme_classic(base_size = 15) 

pres_means <- pres_biomass_df %>% filter(dist_rank <=5) %>% group_by(manual_id) %>% 
  summarize(new_mean = mean(mean_biomass),
            new_max = mean(max_biomass)) 

for(i in 1:length(present_missing)){
  forest_current_wide[forest_current_wide$manual_id == present_missing[i],]$mean_biomass <- 
    pres_means[pres_means$manual_id == present_missing[i],]$new_mean
  
  forest_current_wide[forest_current_wide$manual_id == present_missing[i],]$max_biomass <- 
    pres_means[pres_means$manual_id == present_missing[i],]$new_max
}

forest_current_wide %>% mutate(bio_change = ifelse(manual_id %in% present_missing,1,0)) %>% 
  ggplot() + aes(x = lon, y= lat, color = mean_biomass) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo') + 
  facet_wrap(~bio_change)

dim(forest_current_wide)
dim(forest_2100_wide)
dim(forest_2050_wide)

write_csv(forest_current_wide,"data/forest_composition_2005_biomass.csv")
write_csv(forest_2050_wide,"data/forest_composition_2050_biomass.csv")
write_csv(forest_2100_wide,"data/forest_composition_2100_biomass.csv")

latlong2 <- latlong %>% select(colnames(latlong)[1:30]) %>% select(-c(mean_biomass,max_biomass))

forest_current_all <- merge(latlong2,forest_current_wide)
forest_2050_all <- merge(latlong2,forest_2050_wide)
forest_2100_all <- merge(latlong2,forest_2100_wide)

write_csv(forest_current_wide,"data/all_habitat_features_present.csv")
write_csv(forest_2050_wide,"data/all_habitat_features_2050.csv")
write_csv(forest_2100_wide,"data/all_habitat_features_2100.csv")
