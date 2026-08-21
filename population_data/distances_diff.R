library(tidyverse)
library(geosphere)
library(cowplot)
library(gridExtra)

`%ni%` <- Negate(`%in%`)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")
states <- read_csv("../anthropogenic/data/state_info.csv")

pop_records <- read_csv("data/population_records_1947-2025.csv")
pop_records <- pop_records %>% filter(year >= 1985) %>%
  mutate(period = case_when(year <= 1984 ~ 'p0',
                            year >= 1985 & year <= 2010 ~ "p1",
                            year >= 2011 ~ "p2"))

states <- states %>% rename(new_lat = lat, new_lon = lon) %>% select(country, state, new_lat,new_lon)

obs_per <-  pop_records %>% mutate(n = 1) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_n = sum(n))

obs_per2 <- obs_per %>% 
  pivot_wider(names_from = period, values_from = sum_n) %>%  mutate_at(c('p1','p2'), ~replace_na(.,0)) %>% 
  mutate(bin = case_when(p1 == 0 & p2 >= 1 ~ "2011-2025 only",
                         p1 >= 1 & p2 == 0 ~ "1985-2010 only",
                         p1 >= 1 & p2 >= 1 ~ "Both time periods"))

unique(obs_per2$bin)

obs_per2 %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = bin, fill = bin) + geom_tile() +
  theme_classic()

pop_records2 <- merge(pop_records, obs_per2, all = TRUE)

pop_records_old <- pop_records2 %>% filter(bin %ni% c("2011-2025 only")) %>% filter(year <= 2010)
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

write_csv(new_dist2, "data/distance_to_p2_obs.csv")

#new_year_prob <- new_dist2

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
min(new_dist2[new_dist2$country!= "Mexico",]$distm/1000)

###################
###################

pop_records <- read_csv("data/population_records_1947-2025.csv")

xy <- SpatialPointsDataFrame(
  matrix(c(pop_records$lon,pop_records$lat), ncol=2), data.frame(ID=seq(1:length(pop_records$lon))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

d = 5000
xy$clust <- cutree(hc, h=d)

xy2 <- xy[,c('clust')]

xy_sf <- st_as_sf(xy2)

centroids_grouped <- xy_sf %>% group_by(clust) %>% summarize(st_union(geometry)) %>%
  st_centroid() #%>% st_geometry()

centroids_df <- data.frame(clust = centroids_grouped$clust, cbind(st_coordinates(centroids_grouped)))

dim(centroids_df)

ll_clust <- pop_records

ll_clust$ID <- xy$ID
ll_clust$clust <- xy$clust

ll_clust <- merge(ll_clust,centroids_df)

ll_clust$dist_cent <- distGeo(ll_clust[,4:3],ll_clust[,11:12])

ll_clust %>% group_by(clust) %>% count(clust) %>% arrange(desc(n)) %>% head(5)
ll_clust %>% group_by(clust) %>% count(clust) %>% arrange(desc(n)) %>% tail(5)

ll_clust %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(clust), label = clust) + geom_text() +geom_point() +
  theme_classic() + 
  theme(legend.position = 'none')

clust_counts <- ll_clust %>% count(clust)

dim(clust_counts)
median(clust_counts$n)
median(ll_clust$dist_cent)

xy_cent <- SpatialPointsDataFrame(
  matrix(c(centroids_df$X,centroids_df$Y), ncol=2), data.frame(ID=centroids_df$clust),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist_cent <- distm(xy_cent)/1000

num_clusts <- dim(xy_cent)[1]
nums <- 1:num_clusts

cent_dist <- c()
for(i in 1:dim(mdist_cent)[1]){
  
  nums_vals <- nums!=i
  nums_clust = nums[nums_vals]
  dist_vals <- mdist_cent[i,nums_vals]
  
  min_dist <- dist_vals[which.min(dist_vals)]
  min_clust <- nums_clust[which.min(dist_vals)]
  
  temp <- data.frame(clust_a = i, clust_b = min_clust, dist = min_dist)
  cent_dist <- rbind(temp, cent_dist)
  
}

median(cent_dist$dist)

cent_dist %>% ggplot() + aes(x = dist) + geom_histogram() + theme_classic() +
  scale_x_log10()

min_clust_observed <- ll_clust %>% 
  group_by(clust, X, Y) %>% summarize(min_year = min(year)) 

min_clust_observed <- min_clust_observed %>% rename(lon = X, lat = Y)

new_dist_all <- c()
for(i in 1:dim(min_clust_observed)[1]){
  new_ll <- min_clust_observed[i,]
  
  temp_pop <- min_clust_observed %>% filter(clust != new_ll$clust, min_year < new_ll$min_year)
  
  if (length(temp_pop$clust) >0){
    dist_to_old <- distm(c(new_ll$lon, new_ll$lat), cbind(temp_pop$lon, temp_pop$lat))
    
    diff_year = new_ll$min_year - temp_pop$min_year
    
    rate_to_old <- dist_to_old/(diff_year)
    
    old_twenty <- order(rate_to_old)[1]
    
    temp <- data.frame(new_lat = new_ll$lat, new_lon = new_ll$lon, new_year = new_ll$min_year,
                       new_id = new_ll$clust,
                       old_lat = temp_pop$lat[old_twenty], old_lon = temp_pop$lon[old_twenty], 
                       old_year = temp_pop$min_year[old_twenty],
                       distm = dist_to_old[old_twenty],
                       diffyear = diff_year[old_twenty], 
                       rate = rate_to_old[old_twenty],
                       old_id = temp_pop$clust[old_twenty])
  } else{
    temp <- data.frame(new_lat = new_ll$lat, new_lon = new_ll$lon, new_year = new_ll$min_year,
                       new_id = new_ll$clust, 
                       old_lat = NA, old_lon = NA, 
                       old_year = NA,
                       distm = NA,
                       diffyear = NA, 
                       rate = NA,
                       old_id = NA)
  }
  
  
  
  new_dist_all <- rbind(new_dist_all, temp)
  
  if (i %% 1000 == 0){
    print(i)
  }
}

new_dist_all <- new_dist_all %>% mutate(p2 = case_when(new_year < 1985 ~ "1947-1984", 
                                       new_year >= 1985 & new_year <= 2010 ~ "1985-2010", 
                                       new_year >=2011 ~ "2011-2025")) 

write_csv(new_dist_all, "data/distance_to_all_clust.csv")


p2_only_clust <- new_dist_all %>% filter(new_year > 2010, new_id %ni% c(211,179))

median_val2 <- median(p2_only_clust$rate, na.rm=TRUE)

min(p2_only_clust$rate, na.rm=TRUE)/1000
max(p2_only_clust$rate, na.rm=TRUE)/1000

new_p2_meds <- p2_only_clust %>% drop_na(rate) %>% filter(rate>0) %>% mutate(n = 1) %>%
  group_by(p2) %>% summarize(median_rate = median(rate),
                             mean_rate = mean(rate),
                             sum_n = sum(n),
                             sd_rate = sd(rate),
                             q025 = quantile(rate, probs = 0.025),
                             q975 = quantile(rate, probs = 0.975)) %>% 
  mutate(se_rate = sd_rate/sqrt(sum_n))

plt3 <- p2_only_clust %>% 
  ggplot() + aes(x = rate/1000, fill = after_stat(x), color = after_stat(x)) + geom_histogram(bins = 30) +
  theme_classic() +
  #scale_x_log10(breaks = c(1e-4, 1e-3, 1e-2, 1e-1, 1, 10), labels = c('0.0001','0.001', "0.01", "0.1", "1", '10'), limits = c(1e-2,11)) +
  geom_vline(xintercept = median_val2/1000, linetype = 'dashed', color = 'red') +
  xlab("Rate (km/year)") + ylab("Count") +
  scale_fill_gradient2("km/year", low = "#fed98e", mid = "#ed5f4e", high = "black", midpoint = 5, limits = c(0,max(p2_only_clust$rate)*1.05/1000)) +
  scale_color_gradient2("km/year", low = "#fed98e", mid = "#ed5f4e", high = "black", midpoint = 5, limits = c(0,max(p2_only_clust$rate)*1.05/1000)) +
  theme(legend.position = 'none') +
  geom_text(data = new_p2_meds[new_p2_meds$p2 == "2011-2025",], 
            aes(x = 8, y = Inf,
                label = paste0("Mean: ", 
                               round(mean_rate/1000,3), " \u00B1 ", 
                               round(se_rate/1000,3),
                               "\n95%: (", round(q025/1000,3),",", round(q975/1000,3), ")")),
            vjust = 1, color = 'black')

plt4 <- p2_only_clust %>% 
  ggplot() + 
  geom_sf(data = all_geo2, aes(geometry = geometry), inherit.aes = FALSE, color = 'grey45', fill = 'grey98') +
  geom_point(data = min_clust_observed[min_clust_observed$min_year <= 2010,], aes(x = lon, y = lat), size = 0.1, color = 'grey35') +
  geom_segment(aes(x = old_lon, y = old_lat, xend = new_lon, yend = new_lat, color = rate/1000),
               alpha = 0.8,  arrow = arrow(length = unit(0.1,'cm'))) +
  theme_classic() +
  #scale_color_viridis_c("km/year", option = pal, limits = c(0, max(p2_only$rate)*1.05/1000), direction = -1) +
  scale_color_gradient2("km/year", low = "#fed98e", mid = "#ed5f4e", high = "black", midpoint = 5, limits = c(0,max(p2_only_clust$rate)*1.05/1000)) +
  coord_sf(xlim = c(-126,-103), ylim = c(32.5,51.5)) +
  xlab("Longitude") + ylab("Latitude")

pdf("figures/rates_p2_only_clust2.pdf",height = 10, width = 8)
plot_grid(plt3,plt4,nrow = 2, rel_heights = c(0.3, 1), align = "hv")
dev.off()