library(tidyverse)
library(gridExtra)
library(geosphere)
library(ggmap)
library(sp)
#library(rgeos)
library(sf)
library(pals)

`%ni%` <- Negate(`%in%`)

locations <- read_csv("outbreak_size/data/north_america_trap_locations_051225.csv")
remove <- read_csv('outbreak_size/data/plots_to_remove.csv')
trap_data <- read_csv("outbreak_size/data/north_america_trap_data_051225.csv")

remove_ids <- remove %>% pull(manual_id)

locations <- locations %>% filter(manual_id %ni% remove_ids)

trap_wide <- trap_data %>% drop_na(manual_id) %>% select(manual_id,year,trap_mean) %>% arrange(year) %>%
  pivot_wider(names_from = year, values_from = trap_mean)

trap_t <- t(trap_wide[,2:49])

trap_correlations <- cor(trap_t,trap_t, use = 'pairwise.complete.obs')

corr_df <- c()
for(i in 1:length(trap_wide$manual_id)){
  temp <- data.frame(manual_id = trap_wide$manual_id[i], id2 = trap_wide$manual_id, corr = trap_correlations[i,])
  
  corr_df <- rbind(corr_df,temp)
}

write_csv(corr_df,'outbreak_size/data/trap_correlations.csv')

corr_df <- corr_df %>% filter(manual_id %ni% remove_ids, id2 %ni% remove_ids)

loc_small <- locations %>% select(manual_id,lat,lon)

id_vals <- unique(locations$manual_id)

xy <- cbind(locations$lon, locations$lat)

distances <- data.frame(distm(xy,xy))
colnames(distances) <- locations$manual_id
distances$manual_id <- locations$manual_id

distances <- distances %>% pivot_longer(cols = as.character(locations$manual_id)) %>% 
  rename(site2 = name, dist = value)

distances <- merge(distances, loc_small)
loc_small2 <- loc_small %>% rename(lat2 = lat, lon2 = lon, site2 = manual_id)
distances <- merge(distances,loc_small2)

distances$dist_km <-  distances$dist/1000

#write_csv(distances,"outbreak_size/data/trap_distances.csv")     
distances <- read_csv("outbreak_size/data/trap_distances.csv")     

distances_df2 <- distances

corr_df <- corr_df %>% rename(site2 = id2)

distances_df2 <- merge(distances_df2,corr_df)

pdf('outbreak_size/figures/corr_distances_400km.pdf',height = 4, width = 6)
distances_df2 %>% filter(dist >0, dist_km <= 400) %>% 
  drop_na(corr) %>% mutate(dist_bin = floor(dist_km/10)*10) %>% 
  ggplot() + aes(x = dist_bin, y = corr, group = dist_bin) +
  geom_boxplot(outliers = FALSE) + theme_classic() + 
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') + 
  xlab("Distance") + ylab("Correlation in annual trap counts")
dev.off()
