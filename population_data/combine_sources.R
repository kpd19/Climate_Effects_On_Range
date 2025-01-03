library(tidyverse)

`%ni%` <- Negate(`%in%`)

iNat <- read_csv("data/inaturalist_observations.csv")
defoliation <- read_csv("data/defoliation_split.csv")
pheromone <- read_csv("data/north_america_trap_data.csv")
synthetic <- read_csv("data/nearby_coords_all.csv")

iNat_problems <- read_csv("data/inat_problems.csv")
defoliation_problems <- read_csv("data/defo_problems.csv")
pheromone_problems <- read_csv("data/trap_problems.csv")

iNat <- iNat %>% filter(id %ni% iNat_problems$id)
iNat$manual_id <- 1:length(iNat$id)
iNat$manual_id <- iNat$manual_id + 50000

iNat_locs <- iNat %>% mutate(n = 1) %>% group_by(lat_round,lon_round,source,manual_id) %>% summarize(sum_n = sum(n)) %>% 
  rename(lat = lat_round,lon = lon_round)

defoliation <- defoliation %>% filter(manual_id %ni% defoliation_problems$manual_id) %>% 
  mutate(manual_id = manual_id + 40000) # %>% filter(year >= 1990)

defoliation_locs <- defoliation %>% mutate(n = 1) %>% group_by(lat_cent,lon_cent,manual_id) %>% summarize(sum_n = sum(n)) %>% 
  rename(lat = lat_cent,lon = lon_cent) %>% mutate(source = 'Defoliation surveys') 

synthetic_locs <- synthetic %>% mutate(n = 1) %>% group_by(lat_push,lon_push,manual_id) %>% summarize(sum_n = sum(n)) %>% 
  rename(lat = lat_push,lon = lon_push) %>% mutate(source = 'Synthetic data')

pheromone <- pheromone %>% filter(manual_id %ni% pheromone_problems$manual_id)# %>% filter(year >= 1990)

pheromone_locs <- pheromone %>% mutate(n = 1) %>% group_by(lat,lon,manual_id) %>% summarize(sum_n = sum(n)) %>% 
  mutate(source = 'Pheromone trapping')

all_locs <- rbind(iNat_locs,defoliation_locs,synthetic_locs,pheromone_locs)
all_locs <- all_locs %>% select(-sum_n)

write_csv(all_locs, "data/range_locations.csv")

iNat_records <- iNat %>%
  rename(lat = lat_round,lon = lon_round) %>% 
  select(manual_id,lat,lon,source,year)

defoliation_records <- defoliation %>%
  rename(lat = lat_cent,lon = lon_cent) %>% mutate(source = 'Defoliation surveys') %>% 
  select(manual_id,lat,lon,source,year)

pheromone_records <- pheromone %>% filter(trap_mean >0) %>% 
  mutate(source = 'Pheromone trapping') %>% 
  select(manual_id,lat,lon,source,year)

population_records <- rbind(iNat_records,defoliation_records,pheromone_records)
population_records <- population_records %>% mutate(lat_coord = round(lat/0.25)*0.25,
                              lon_coord = round(lon/0.25)*0.25)

write_csv(population_records, "data/population_records.csv")

