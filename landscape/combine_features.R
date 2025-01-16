library(tidyverse)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/landscape")

forest <- read_csv("data/forest_composition_example.csv")
biomass <- read_csv("data/biomass_all_sites_example.csv")
modis <- read_csv('data/modis_5k_nearneedle_example.csv') 

species_class <- read_csv('data/species_class.csv')

latlong <- read_csv("data/range_locations_example.csv")
elevation <- read_csv("data/elevation_example.csv")

ll1 <- merge(latlong,elevation)

biomass <- biomass %>% mutate(mean_biomass = ifelse(is.infinite(max_biomass),NA,mean_biomass)) %>% 
  mutate(max_biomass = ifelse(is.infinite(max_biomass),NA,max_biomass))

ll2 <- merge(ll1,biomass)  

modis_cols <- colnames(modis)[colnames(modis) %ni% c('total','lat','lon','manual_id')]

modis <- modis %>% mutate_at(modis_cols, ~replace_na(.,0))

ll3 <- merge(ll2,modis)

forest_cols <- colnames(forest)[colnames(forest) %ni% c('total','lat','lon','manual_id','source',"No trees")]

forest <- forest %>% mutate_at(forest_cols, ~replace_na(.,0))

species_class <- species_class %>% select(name,Clade,Genus,multi_species) %>% 
  mutate(multi_species = ifelse(is.na(multi_species),1,multi_species))

forest_long <- forest %>% pivot_longer(forest_cols) %>% filter(value>0) %>% rename(no_trees = `No trees`)

forest_long2 <- merge(forest_long,species_class, all.x = TRUE)

forest_long2 %>% filter(is.na(Genus))

all_long_genus <- forest_long2 %>% mutate(value_m = value/multi_species) %>%
  group_by(manual_id,lat,lon,source,Genus) %>% summarize(p_genus = sum(value_m))

all_long_clade <- forest_long2 %>% mutate(value_m = value/multi_species) %>%
  group_by(manual_id,lat,lon,source,Clade) %>% summarize(p_clade = sum(value_m))

all_wide_genus <- all_long_genus %>% pivot_wider(names_from = Genus, values_from = p_genus)
all_wide_clade <- all_long_clade %>% pivot_wider(names_from = Clade, values_from = p_clade)

all_wide <- merge(all_wide_clade,all_wide_genus)

forest_cols2 <- colnames(all_wide)[colnames(all_wide) %ni% c('lat','lon','manual_id','source')]

all_wide <- all_wide %>% mutate_at(forest_cols2, ~replace_na(.,0))
all_wide$trees <- 1

no_trees <- forest %>% filter(`No trees` == 1) %>% select(lat,lon,manual_id,source)
no_trees[,forest_cols2] <- 0
no_trees$trees <- 0

all_wide2 <- rbind(all_wide,no_trees)

all_wide2 %>% ggplot() + aes(x = lon,y = lat, color = Abies) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'mako')

ll4 <- merge(ll3,all_wide2)

write_csv(ll4,"data/all_habitat_features_example.csv")
