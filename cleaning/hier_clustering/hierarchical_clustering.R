library(tidyverse)
library(gridExtra)
library(geosphere)
library(ggmap)
library(geosphere)
library(sp)
#library(rgeos)
library(sf)

api_key <- "AIzaSyD7M-IQpEPZeDPRiZeMU0e8CAYWrK_yYWg"
register_google(api_key)

`%ni%` <- Negate(`%in%`)

locations <- read_csv("_gendata/all_trap_max_edits.csv")
remove <- read_csv('/Volumes/My Book/Synchrony/_model_dat/plots_to_remove.csv')
corr_df <- read_csv('/Volumes/My Book/Synchrony/_trap/trap_correlations.csv')
trap_data <- read_csv("/Volumes/My Book/Synchrony/_data/north_america_trap_data_021624.csv")

remove_ids <- remove %>% pull(manual_id)

locations <- locations %>% filter(manual_id %ni% remove_ids)
corr_df <- corr_df %>% filter(manual_id %ni% remove_ids, site2 %ni% remove_ids)

id_vals <- unique(locations$manual_id)

xy <- cbind(locations$lon, locations$lat)

distance_df <- c()

a.time <- Sys.time()
for(i in 1:length(id_vals)){
  
  ref_lat <- locations[locations$manual_id == id_vals[i],]$lat
  ref_lon <- locations[locations$manual_id == id_vals[i],]$lon
  
  xy2 <- c(ref_lon, ref_lat)
  
  distances <- distm(xy, xy2, fun = distHaversine)
  
  temp <- locations
  temp$site2 <- id_vals[i]
  temp$dist <- distances
  
  distance_df <- rbind(distance_df, temp)
  
}

b.time <- Sys.time()
b.time - a.time

site2_locations <- locations %>% rename(lat2 = lat, lon2 =lon) %>% select(lat2,lon2,manual_id)

distance_df2 <- merge(distance_df, site2_locations, by.x = 'site2', by.y ='manual_id' )

distance_df2$dist <- distance_df2$dist[,1]

distance_df2$dist_km <-  distance_df2$dist/1000

write_csv(distance_df2,"/Volumes/My Book/Synchrony/_data/trap_distances_32924.csv")     

#distance_df2 <- distance_df2 %>% filter(manual_id %ni% remove_ids, site2 %ni% remove_ids)

uni_ids <- sort(unique(distance_df2$manual_id))

latlong_other <- locations %>% select(manual_id,max_trap) %>% rename(site2 = manual_id, other_max = max_trap)

ll_dist <- merge(distance_df2,latlong_other)

ll_dist_int <- ll_dist %>% filter(dist <= 7500)

ll_dist_int <- ll_dist_int %>% group_by(manual_id) %>% mutate(loc_max = max(other_max)) %>% 
  mutate(is_max = ifelse(max_trap == loc_max,TRUE,FALSE))

corr_df <- corr_df %>% rename(site2 = id2)

distance_df3 <- merge(distance_df2,corr_df)

distance_df3 %>% filter(dist >0, dist_km <= 500) %>% 
  drop_na(corr) %>% mutate(dist_bin = floor(dist_km/10)*10) %>% 
  ggplot() + aes(x = dist_bin, y = corr, group = dist_bin) +
  geom_boxplot() + theme_classic() + 
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed')

pdf('_plots/corr_distances_50km.pdf',height = 4, width = 6)
distance_df3 %>% filter(dist >0, dist_km <= 50) %>% 
  drop_na(corr) %>% mutate(dist_bin = floor(dist_km/1)*1) %>% 
  ggplot() + aes(x = dist_bin, y = corr, group = dist_bin) +
  geom_boxplot() + theme_classic() + 
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') + 
  xlab("Distance (km)") + ylab("Correlation coefficient")
dev.off()

loc_sat <- get_map(location = c(lon= -116.7392,lat = 46.82744), maptype = 'satellite',
                   source = 'google', api_key = api_key)


# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(locations$lon,locations$lat), ncol=2), data.frame(ID=seq(1:length(locations$lon))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

d = 10000
xy$clust <- cutree(hc, h=d)

xy2 <- xy[,c('clust')]

xy_sf <- st_as_sf(xy2)

centroids_grouped <- xy_sf %>% group_by(clust) %>% summarize(st_union(geometry)) %>%
  st_centroid() #%>% st_geometry()

centroids_df <- data.frame(clust = centroids_grouped$clust, cbind(st_coordinates(centroids_grouped)))

dim(centroids_df)

ll_clust <- locations %>% select(lat,lon,manual_id,State,PlotName,years_trapped,max_trap)

ll_clust$ID <- xy$ID
ll_clust$clust <- xy$clust

ll_clust <- merge(ll_clust,centroids_df)

colnames(ll_clust)

ll_clust$dist_cent <- distGeo(ll_clust[,3:2],ll_clust[,10:11])


xy_cent <- SpatialPointsDataFrame(
  matrix(c(centroids_df$X,centroids_df$Y), ncol=2), data.frame(ID=centroids_df$clust),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist_cent <- distm(xy_cent)/1000

cent_dist <- c()
for(i in 1:dim(mdist_cent)[1]){
  
  temp <- data.frame(clust1 = xy_cent$ID[i], clust2 = xy_cent$ID, dist = mdist_cent[i,])
  cent_dist <- rbind(cent_dist, temp)
}


min_dist <- cent_dist %>% filter(clust1 != clust2) %>% group_by(clust1) %>% summarize(min_dist = min(dist))

write_csv(ll_clust,paste0('/Volumes/My Book/Synchrony/_model_dat/clusters_',d,'.csv'))

pdf("_plots/_clusters/cluster_trap_distances.pdf",height = 4, width = 6)
ll_clust %>% group_by(clust) %>% summarize(mean_dist = mean(dist_cent)/1000,
                                           max_dist = max(dist_cent)/1000,
                                           median_dist = median(dist_cent)/1000) %>% 
  ggplot() + aes(x = median_dist) + geom_histogram() +
  xlab("Median distance to centroid (km)") + 
  theme_classic(base_size = 15)
dev.off()

pdf("_plots/_clusters/cluster_numbers.pdf",height = 4, width = 6)
ll_clust %>% group_by(clust) %>% count(clust) %>% 
  ggplot() + aes(x = n) + geom_histogram() +
  xlab("Number of traps in a cluster") + 
  theme_classic(base_size = 15)
dev.off()

trap_small <-
  trap_data %>% filter(lat <= 47.1, lat >= 46.5, lon <= -116.25, lon >= -117.25)

trap_small3 <- merge(trap_small,ll_clust)

plt1 <- ggmap(loc_sat) + 
  geom_text(data = trap_small3, 
            aes(x = lon, y = lat, label = clust, group = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(data = trap_small3, aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  geom_point(data = trap_small3, aes(x = X, y = Y), color = 'red') +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("2020 Trap Means")

plt2 <- ggmap(loc_sat) + 
  geom_text(data = trap_small3, 
            aes(x = lon, y = lat, label = round(max_trap,0), color = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("2020 Trap Means")

pdf("_plots/_clusters/idaho_clust_max_xsmall.pdf",height = 10, width = 30)
grid.arrange(plt1,plt2, nrow = 1)
dev.off()

pdf("_plots/_clusters/all_clust_max_xsmall.pdf",height = 15, width = 15)
ll_clust %>% ggplot() +
  geom_point(aes(x = lon, y = lat, color = clust), size = 5) +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  geom_point(aes(x = X, y = Y), color = 'red') +
  theme_classic() + xlab("Latitude") + ylab("Longitude")
dev.off()

pdf("_plots/_clusters/idaho_clust_ts_space.pdf",height = 10, width = 20)
trap_small3 %>% mutate(lat_coord = round(lat/0.25)*0.25, lon_coord = round(lon/0.25)*0.25) %>% 
  ggplot() + aes(x = year, y = trap_mean, color = as.factor(clust), group = manual_id) + geom_line() + theme_classic(base_size = 15) +
  facet_grid(-lat_coord~lon_coord) +
  ylab('Mean pheromone trap') +
  scale_x_continuous(breaks = c(1980,2000,2020))
dev.off()

trap_small3 %>% mutate(lat_coord = round(lat/0.1)*0.1, lon_coord = round(lon/0.1)*0.1) %>% 
  ggplot() + aes(x = year, y = trap_mean, color = as.factor(clust), group = manual_id) +
  geom_line() + theme_classic(base_size = 15) +
  facet_wrap(~clust) +
  xlab('Mean pheromone trap')

distance_between_cent <- distm(centroids_df[2:3])

cent_dist <- c()
for(i in 1:length(centroids_df$clust)){
  temp <- data.frame(clust1 = centroids_df$clust[i], clust2 = centroids_df$clust, dist = distance_between_cent[i,])
  cent_dist <- rbind(cent_dist,temp)
}

write_csv(cent_dist,'/Volumes/My Book/Synchrony/_data/clustering/distance_between_clusters.csv')
distances <- cent_dist %>% filter(clust1 != clust2) %>% group_by(clust1) %>% summarize(closest = min(dist)/1000)

pdf("_plots/_clusters/cluster_distances.pdf",height = 4, width = 6)
distances %>% ggplot() + aes(x = closest) + geom_histogram() + theme_classic(base_size = 15) +
  scale_x_log10() +
  xlab("Distance to closest centroid (km)")
dev.off()


cent_dist %>% filter(clust1 == 12, clust2 == 11) %>% mutate(dist = dist/1000)


cent_dist %>% filter(clust1 != clust2) %>%
  filter(clust1 %in% trap_small3$clust, clust2 %in% trap_small3$clust) %>% 
  mutate(dist = dist/1000)

ll_clust %>% group_by(clust)

ll_dc <- merge(ll_dist,ll_clust)

ll_clust %>% mutate(n =1) %>% group_by(clust) %>% summarize(sum_n = sum(n),
                                                            diff_lat = max(lat)- min(lat),
                                                            diff_lon = max(lon)- min(lon))


ll_dc %>% filter(clust == 1, site2 %in% ll_clust[ll_clust$clust == 1,]$manual_id)


ll_clust %>% filter(State == "WA") %>%  ggplot() + 
  geom_text(aes(x = lon, y = lat, label = clust, group = clust, color = clust), size = 5) +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(aes(x = lon, y = lat, group = clust), color = 'black', linetype = 'dashed', type = 't') +
  #geom_point(aes(x = X, y = Y), color = 'red') +
  #xlim(-117.2,-116.3) + 
  #ylim(46.75,47.1) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("2020 Trap Means")

ll_clust %>% filter(State == "OR") %>%  ggplot() + 
  geom_text(aes(x = lon, y = lat, label = round(max_ampl), group = clust, color = clust), size = 5) +
  scale_color_viridis_c(option = 'mako') + 
  #stat_ellipse(aes(x = lon, y = lat, group = clust), color = 'black', linetype = 'dashed', type = 't') +
  #geom_point(aes(x = X, y = Y), color = 'red') +
  #xlim(-117.2,-116.3) + 
  #ylim(46.75,47.1) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("2020 Trap Means")


loc_sat_wa <- get_map(location = c(lon= -118.6,lat = 48.5), maptype = 'satellite',
                   source = 'google', api_key = api_key, zoom = 7)

plt1 <- ggmap(loc_sat_wa) + geom_text(data = ll_clust[ll_clust$State %in% c("WA","BC"),], 
                                aes(x = lon, y = lat, label = clust, color = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(data = ll_clust[ll_clust$State %in% c("WA","BC"),],
               aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  xlim(-122,-117) + 
  ylim(47.5,49) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("Trap Clusters")

plt2 <- ggmap(loc_sat_wa) + geom_text(data = ll_clust[ll_clust$State %in% c("WA","BC"),], 
                              aes(x = lon, y = lat, label = round(max_trap,0), color = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(data = ll_clust[ll_clust$State %in% c("WA","BC"),],
               aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  xlim(-122,-117) + 
  ylim(47.5,49) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("Trap Clusters")

pdf("_plots/_clusters/washington_clust_max2.pdf",height = 10, width = 30)
grid.arrange(plt1,plt2, nrow = 1)
dev.off()



loc_sat_or <- get_map(location = c(lon= -118.5,lat = 45.0), maptype = 'satellite',
                      source = 'google', api_key = api_key, zoom = 7)

plt1 <- ggmap(loc_sat_or) + geom_text(data = ll_clust[ll_clust$State %in% c("WA","BC","OR","ID"),], 
                                      aes(x = lon, y = lat, label = clust, color = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(data = ll_clust[ll_clust$State %in% c("WA","BC","OR","ID"),],
               aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  xlim(-119,-116) + 
  ylim(44.5,46.25) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("Trap Clusters")

plt2 <- ggmap(loc_sat_or) + geom_text(data = ll_clust[ll_clust$State %in% c("WA","BC","OR"),], 
                                      aes(x = lon, y = lat, label = round(max_trap,0), color = clust), size = 5, color = 'yellow') +
  scale_color_viridis_c(option = 'mako') + 
  stat_ellipse(data = ll_clust[ll_clust$State %in% c("WA","BC","OR"),],
               aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  xlim(-119,-116) + 
  ylim(44.5,46.25) +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle("Trap Max")

pdf("_plots/_clusters/oregon_clust_max3.pdf",height = 10, width = 30)
grid.arrange(plt1,plt2, nrow = 1)
dev.off()



ll_clust %>% ggplot() + aes(x = clust, y = max_ampl) + geom_point() + theme_classic(base_size = 15) + 
  facet_wrap(~State, scales = 'free')

write_csv(ll_clust,'/Volumes/My Book/Synchrony/_data/clustering/trap_clusters_22924.csv')
ll_clust <- read_csv('/Volumes/My Book/Synchrony/_data/clustering/trap_clusters_22924.csv')

ll_clust %>% drop_na(mean_ampl) %>% ungroup() %>% mutate(n = 1) %>% dplyr::group_by(clust,X,Y) %>%
  dplyr::summarize(mean_ampl2 = mean(mean_ampl,na.rm=TRUE),
            max_ampl = max(mean_ampl),
            sum_n = sum(n))  %>% 
  ggplot() + aes(x = X, y = Y, color = max_ampl) + 
  geom_point() + theme_classic(base_size = 15) + 
  scale_color_viridis_c(option = 'turbo')
  
  
a <- ll_clust %>% drop_na(mean_ampl) %>% ungroup() %>% mutate(n = 1) %>% dplyr::group_by(clust,X,Y) %>%
    dplyr::summarize(mean_ampl2 = mean(mean_ampl,na.rm=TRUE),
                     max_ampl = max(mean_ampl),
                     sum_n = sum(n)) 



length(unique(ll_clust$clust))

ll_clust_small <- ll_clust %>% select(clust,manual_id)
ll_clust2_small <- ll_clust %>% select(clust,manual_id) %>% rename(site2 = manual_id, clust2 = clust) 

distance_df4 <- merge(distance_df3,ll_clust_small)
distance_df4 <- merge(distance_df4,ll_clust2_small)

dist_corr <- distance_df4 %>% filter(manual_id != site2) %>% drop_na(corr) 

corr_test <- dist_corr %>% group_by(clust,clust2) %>% summarize(mean_corr = mean(corr)) %>% 
  mutate(test = ifelse(clust == clust2,'within','between'))


ratios <- corr_test %>% group_by(test) %>% summarize(mean_corr = mean(mean_corr)) %>% 
  pivot_wider(names_from = test, values_from = mean_corr) %>% 
  mutate(ratio = within/between)

pdf("_plots/_clusters/mean_corr30k.pdf")
corr_test %>% ggplot() + aes(x = test, y = mean_corr,group = test) + geom_boxplot() + theme_classic(base_size = 15) +
  annotate(geom = 'text', x = 0.75, y = 1, label = paste0('within = ', round(ratios$within,2),
                                                       '\nbetween = ',round(ratios$between,2),
                                                       '\nratio = ', round(ratios$ratio,2)))
dev.off()


c1 <- read_csv('/Volumes/My Book/Synchrony/_model_dat/clusters_1e+05.csv')
c2 <- read_csv('/Volumes/My Book/Synchrony/_model_dat/clusters_50000.csv')
c3 <- read_csv('/Volumes/My Book/Synchrony/_model_dat/clusters_25000.csv')
c4 <- read_csv('/Volumes/My Book/Synchrony/_model_dat/clusters_10000.csv')

c1 <- c1 %>% select(manual_id,clust) %>% rename(clust_l = clust)
c2 <- c2 %>% select(manual_id,clust) %>% rename(clust_m = clust)
c3 <- c3 %>% select(manual_id,clust) %>% rename(clust_s = clust)
c4 <- c4 %>% select(manual_id,clust) %>% rename(clust_xs = clust)

clust_all <- merge(c1,c2)
clust_all <- merge(clust_all,c3)
clust_all <- merge(clust_all,c4)

write_csv(clust_all,'/Volumes/My Book/Synchrony/_model_dat/clusters_all.csv')

