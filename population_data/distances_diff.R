library(tidyverse)
library(geosphere)
library(cowplot)
library(gridExtra)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

pop_records <- read_csv("data/population_records_1947-2025.csv")
pop_records <- pop_records %>% filter(year >= 1985) %>%
  mutate(period = case_when(year <= 1984 ~ 'p0',
                            year >= 1985 & year <= 2010 ~ "p1",
                            year >= 2011 ~ "p2"))

states <- read_csv("../anthropogenic/data/state_info.csv")

states <- states %>% rename(new_lat = lat, new_lon = lon) %>% select(country, state, new_lat,new_lon)

obs_per <-  pop_records %>% mutate(n = 1) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_n = sum(n))

obs_per2 <- obs_per %>% 
  pivot_wider(names_from = period, values_from = sum_n) %>%  mutate_at(c('p1','p2'), ~replace_na(.,0)) %>% 
  mutate(bin = case_when(p1 >=1 & p2 >=1 ~'Both time periods',
                         p1 <1 & p2 >=1 ~ "2011-2025 only",
                         p1 >=1 & p2 <=1 ~ "1985-2010 only"))

pop_records2 <- merge(pop_records, obs_per2, all = TRUE)

pop_records_old <- pop_records2 %>% filter(bin %in% c("Both time periods", "1985-2010 only")) %>% filter(year <= 2010)
pop_records_new <- pop_records2 %>% filter(bin %in% c("2011-2025 only")) 

new_ids <- dim(pop_records_new)[1]

new_dist <- c()
for(i in 1:new_ids){
  new_ll <- pop_records_new[i,]
  
  dist_to_old <- distm(c(new_ll$lon, new_ll$lat), cbind(pop_records_old$lon, pop_records_old$lat))
  
  old_id <- which.min(dist_to_old)
  
  temp <- data.frame(new_lat = new_ll$lat, new_lon = new_ll$lon, new_year = new_ll$year,
                     new_id = new_ll$manual_id, new_source = new_ll$source,
             old_lat = pop_records_old$lat[old_id], old_lon = pop_records_old$lon[old_id], 
             old_year = pop_records_old$year[old_id],
             distm = dist_to_old[old_id])
  
  new_dist <- rbind(new_dist, temp)
  
}

new_dist %>% 
  ggplot() + aes(x = new_year, y = old_year, color = distm) + geom_point() + theme_classic()

new_dist$new_lat <- round(new_dist$new_lat, 5)
new_dist$new_lon <- round(new_dist$new_lon, 5)

states$new_lat <- round(states$new_lat, 5)
states$new_lon <- round(states$new_lon, 5)

new_dist2 <- merge(new_dist,states, all.x = TRUE)
new_dist2 <- new_dist2[!duplicated(new_dist2),]

plt1 <- new_dist2 %>% filter(country != "Mexico") %>%
  ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey90', color = 'grey35')  + 
  geom_point(aes(x = new_lon, y = new_lat, color = distm/1000)) +
  theme_classic(base_size = 15) + 
  scale_color_viridis_c("Distance (km)", option = 'plasma', trans = 'log10', labels = scales::comma) +
  coord_sf(ylim = c(31.25,52.75), xlim = c(-128.125,-103.875))  + 
  theme(legend.position = 'top') + 
  xlab("Longitude") + ylab("Latitude") 

plt2 <- new_dist2 %>% filter(country != "Mexico") %>%
  ggplot() + aes(x = distm/1000) + geom_histogram() + 
  theme_classic(base_size = 15) + xlab("Distance (km)") + ylab("Count")

new_dist2 %>% filter(country != "Mexico") %>% 
  ggplot() + aes(x = new_year-old_year, y = distm/1000) + geom_point() + theme_classic() +
  xlab("Difference in year observed") +
  ylab("Distance (km)") + 
  scale_color_viridis_c("Distance (km)", option = 'plasma', trans = 'log10', labels = scales::comma) 

pdf("figures/compare_four/p2_distance3.pdf", height = 6, width = 12)
plot_grid(plt2, plt1, nrow = 1, align = 'h', rel_widths = c(1.2,1))
dev.off()

median(new_dist2[new_dist2$country!= "Mexico",]$distm/1000)
max(new_dist2[new_dist2$country!= "Mexico",]$distm/1000)