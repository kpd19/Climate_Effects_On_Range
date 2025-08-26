library(tidyverse)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/outbreak_size")

habitat <- read_csv("data/all_habitat_features_sites.csv")
weather <- read_csv("../climate/data/all_annual_1940-2024_sites.csv")

locations <- read_csv("data/north_america_trap_locations_051225.csv")
clust_avg <- read_csv('clusters/clusters_140002.csv')
outbreak_sizes <- read_csv('data/cluster_max_sum_clustn2.csv')

clust_avg <- clust_avg %>% select(clust,PlotName,lat,lon,manual_id,years_trapped,max_trap,X,Y) %>% rename(clust_n = clust)

weather <- weather %>% filter(year_tm >= 1970, year_tm <= 2024)

weather %>% filter(year_tm >= 1970, year_tm <= 2024) %>% ggplot() + aes(x = longitude, y = latitude, color = sum_tp, fill = sum_tp) + geom_tile() + 
  theme_classic() + scale_color_viridis_c(option = 'turbo')+ 
  scale_fill_viridis_c(option = 'turbo') + 
  facet_wrap(~year_tm)

habitat %>% mutate(lat_coord = round(lat/0.25)*0.25,
                   lon_coord = round(lon/0.25)*0.25) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = mean_cover, fill = mean_cover) + geom_tile() + 
  theme_classic() + scale_color_viridis_c(option = 'turbo')+ 
  scale_fill_viridis_c(option = 'turbo')

habitat <- habitat %>% select(-manual_id)

habitat2 <- merge(clust_avg, habitat)

habitat2 <- habitat2[!duplicated(habitat2),]

summ_vars <- colnames(habitat2)[c(10:23,27:71)]

cluster_averages <- habitat2 %>% group_by(clust_n, X,Y) %>% summarize_at(vars(summ_vars),mean)

write_csv(cluster_averages, "data/cluster_habitat_averages.csv")
