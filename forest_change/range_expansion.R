#library(raster)
library(tidyverse)
library(geodata)
library(sf)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/forest_change/")

canada_data = gadm(country="CAN", level = 1, path = tempdir())
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
bc_data =st_as_sf(bc_data)

us_data = gadm(country="USA", level = 1, path = tempdir())
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
state_data = st_as_sf(state_data)

sf_use_s2(TRUE)

tree_files <- read_csv("data/file_info.csv")
latlong <- read_csv("../landscape/data/all_habitat_features.csv")

latlong %>% filter(source == 'iNaturalist', is.na(elev2))
latlong[latlong$manual_id %in% c(50315,50223),]$elev2 <- 0
latlong %>% count(source)

elev_info <- latlong %>% select(lat,lon,manual_id,source,elev2)

species_class <- read_csv("../landscape/data/species_class.csv")
forest_presence <- read_csv("data/forest_climate_change2.csv")

forest1 <- read_csv("../landscape/data/forest_composition.csv")
forest2 <- read_csv("../landscape/data/forest_composition_new.csv")

forest_current <- rbind(forest1,forest2)

forest_presence %>% filter(tree_sp == "Abies grandis") %>% 
  ggplot() + aes(x = lon,y = lat, color = present) + geom_point() + theme_classic() + 
  facet_wrap(~time_period)

species_names <- species_class %>% filter(Genus %in% c('Abies','Pseudotsuga')) %>% pull(name)
species_names <- unique(species_names)

current_df <- data.frame(common_name = species_names, tree_sp = c("Pseudotsuga menziesii","Abies grandis","Abies concolor",
                                                                  'Abies lasiocarpa', 'Abies concolor','Pseudotsuga macrocarpa',
                                                                  'Abies lasiocarpa', 'Abies amabilis','Abies magnifica',
                                                                  'Abies procera','Abies amabilis', 'Abies balsamea'))

forest_current_long <- forest_current %>% select(total,lat,lon,manual_id,source,species_names) %>% 
  pivot_longer(species_names) %>% rename(common_name = name)
forest_current_long <- merge(forest_current_long, current_df,all = TRUE)
forest_current_long2 <- forest_current_long %>% group_by(lat,lon,manual_id,source, tree_sp) %>%
  summarize(value = sum(value,na.rm=TRUE)) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>% 
  mutate(present = ifelse(value >0,1,0)) %>% select(-c(value)) %>% mutate(time_period = 2005) %>% 
  mutate(type = 'Historical records')

forest_current_long2 %>% filter(tree_sp == 'Pseudotsuga menziesii') %>% 
  ggplot() + aes(x = lon,y = lat, color = present) + geom_point() + theme_classic()

forest_presence <- forest_presence %>% select(-sum_n) %>% mutate(type = 'Climate change projection')

forest_all <- rbind(forest_current_long2,forest_presence)
forest_all2 <- merge(forest_all,elev_info)

write_csv(forest_all2, "data/forest_pa_historical_cc.csv")

forest_all2 %>% drop_na(elev2) %>% filter(present == 1) %>% 
  filter(tree_sp == 'Pseudotsuga menziesii') %>% 
  ggplot() + aes(x = lon,y = lat, color = as.factor(present)) + geom_point() + theme_classic() + 
  facet_wrap(~time_period) + 
  scale_color_brewer(palette = "Set1")

range_df <- forest_all2 %>% drop_na(elev2) %>%
  mutate(lat_coord = round(lat/0.25)*0.25,
         lon_coord = round(lon/0.25)*0.25) %>%
  group_by(lat_coord,lon_coord,tree_sp,time_period) %>% 
  summarize(sum_pres = sum(present)) %>% pivot_wider(values_from = sum_pres, names_from = time_period) %>% 
  mutate(`2005` = ifelse(is.na(`2005`),0,`2005`),
         `2050` = ifelse(is.na(`2050`),0,`2050`),
         `2100` = ifelse(is.na(`2100`),0,`2100`))

range_df <- range_df %>% mutate(exp_2050 = case_when(`2005` == 0 & `2050` >= 1 ~ 'expansion',
                                                     `2005` >= 1 & `2050` == 0 ~ 'contraction',
                                                     `2005` >= 1 & `2050` >= 1 ~ 'current range',
                                                     `2005` == 0 & `2050` == 0 ~ 'absent')) %>% 
  mutate(exp_2100 = case_when(`2005` == 0 & `2100` >= 1 ~ 'expansion',
                              `2005` >= 1 & `2100` == 0 ~ 'contraction',
                              `2005` >= 1 & `2100` >= 1 ~ 'current range',
                              `2005` == 0 & `2100` == 0 ~ 'absent'))

range_long <- range_df %>% select(lat_coord,lon_coord,tree_sp,exp_2050, exp_2100) %>% pivot_longer(cols = c(exp_2050,exp_2100)) %>% 
  mutate(period = ifelse(name == 'exp_2050', 2050,2100))

uni_sp <- unique(range_long$tree_sp)
for(i in 1:length(uni_sp)){
  ab_plt <- ggplot() +
    geom_tile(data = range_long[range_long$tree_sp == uni_sp[i],],
              aes(x = lon_coord, y= lat_coord, color = value, fill = value)) + theme_classic() + 
    scale_color_manual("", values = c('expansion' = 'blue','contraction' = 'orange','current range' = 'green4','absent' = 'grey75')) +
    scale_fill_manual("", values = c('expansion' = 'blue','contraction' = 'orange','current range' = 'green4','absent' = 'grey75')) +
    xlab("Longitude") + ylab("Latitude") + 
    facet_wrap(~period)
  
  plt_diff <- ab_plt + geom_sf(data = state_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
    geom_sf(data = bc_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
    theme_classic(base_size = 15) +
    coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
    guides(shape = guide_legend(override.aes = list(size = 0.25))) +
    theme(plot.title = element_text(hjust = 0.5))+ 
    theme(legend.position = 'top')
  
  pdf(paste0("figures/",uni_sp[i],"_change.pdf"),height = 6, width = 8)
  print(plt_diff)
  dev.off()
}

needleleaf <- forest_all2 %>% drop_na(elev2) %>%
  mutate(lat_coord = round(lat/0.25)*0.25,
         lon_coord = round(lon/0.25)*0.25) %>%
  group_by(lat_coord,lon_coord,time_period) %>% 
  summarize(sum_pres = sum(present)) %>% pivot_wider(values_from = sum_pres, names_from = time_period) %>% 
  mutate(`2005` = ifelse(is.na(`2005`),0,`2005`),
         `2050` = ifelse(is.na(`2050`),0,`2050`),
         `2100` = ifelse(is.na(`2100`),0,`2100`))

needleleaf <- needleleaf %>% mutate(exp_2050 = case_when(`2005` == 0 & `2050` >= 1 ~ 'expansion',
                                                     `2005` >= 1 & `2050` == 0 ~ 'contraction',
                                                     `2005` >= 1 & `2050` >= 1 ~ 'current range',
                                                     `2005` == 0 & `2050` == 0 ~ 'absent')) %>% 
  mutate(exp_2100 = case_when(`2005` == 0 & `2100` >= 1 ~ 'expansion',
                              `2005` >= 1 & `2100` == 0 ~ 'contraction',
                              `2005` >= 1 & `2100` >= 1 ~ 'current range',
                              `2005` == 0 & `2100` == 0 ~ 'absent'))

nl_long <- needleleaf %>% select(lat_coord,lon_coord,exp_2050, exp_2100) %>% pivot_longer(cols = c(exp_2050,exp_2100)) %>% 
  mutate(period = ifelse(name == 'exp_2050', 2050,2100))

nl_plt <- ggplot() +
  geom_tile(data = nl_long,
            aes(x = lon_coord, y= lat_coord, color = value, fill = value)) + theme_classic() + 
  scale_color_manual("", values = c('expansion' = 'blue','contraction' = 'orange','current range' = 'green4','absent' = 'grey75')) +
  scale_fill_manual("", values = c('expansion' = 'blue','contraction' = 'orange','current range' = 'green4','absent' = 'grey75')) +
  xlab("Longitude") + ylab("Latitude") + 
  facet_wrap(~period)

plt_diff <- nl_plt + geom_sf(data = state_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = bc_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top')
plt_diff

pdf(paste0("figures/host_tree_change.pdf"),height = 6, width = 8)
print(plt_diff)
dev.off()

forest_current_wide <- forest_current_long2 %>% pivot_wider(values_from = present, names_from = tree_sp) %>% 
  mutate(Abies = `Abies amabilis` + `Abies balsamea` + `Abies concolor` + `Abies grandis` + 
           `Abies lasiocarpa` + `Abies magnifica` + `Abies procera`,
         Pseudotsuga = `Pseudotsuga menziesii` + `Pseudotsuga macrocarpa`) %>% 
  mutate(host_trees = Abies + Pseudotsuga)

forest_current_wide <- forest_current_wide %>% mutate(Abies = ifelse(Abies >=1,1,0),
                               Pseudotsuga= ifelse(Pseudotsuga >=1,1,0),
                               host_trees = ifelse(host_trees >=1,1,0))

forest_2050_wide <- forest_presence %>% filter(time_period == 2050) %>% pivot_wider(values_from = present, names_from = tree_sp) %>% 
  mutate(Abies = `Abies amabilis` + `Abies balsamea` + `Abies concolor` + `Abies grandis` + 
           `Abies lasiocarpa` + `Abies magnifica` + `Abies procera`,
         Pseudotsuga = `Pseudotsuga menziesii` + `Pseudotsuga macrocarpa`) %>% 
  mutate(host_trees = Abies + Pseudotsuga) %>% mutate(Abies = ifelse(Abies >=1,1,0),
                                                     Pseudotsuga= ifelse(Pseudotsuga >=1,1,0),
                                                     host_trees = ifelse(host_trees >=1,1,0))

forest_2100_wide <- forest_presence %>% filter(time_period == 2100) %>% pivot_wider(values_from = present, names_from = tree_sp) %>% 
  mutate(Abies = `Abies amabilis` + `Abies balsamea` + `Abies concolor` + `Abies grandis` + 
           `Abies lasiocarpa` + `Abies magnifica` + `Abies procera`,
         Pseudotsuga = `Pseudotsuga menziesii` + `Pseudotsuga macrocarpa`) %>% 
  mutate(host_trees = Abies + Pseudotsuga) %>% mutate(Abies = ifelse(Abies >=1,1,0),
                                                      Pseudotsuga= ifelse(Pseudotsuga >=1,1,0),
                                                      host_trees = ifelse(host_trees >=1,1,0))

forest_2100_wide %>% ggplot() + aes(x = lon, y = lat, color = host_trees) + geom_point() + 
  theme_classic()

write_csv(forest_current_wide,"data/forest_composition_2005.csv")
write_csv(forest_2050_wide,"data/forest_composition_2050.csv")
write_csv(forest_2100_wide,"data/forest_composition_2100.csv")
