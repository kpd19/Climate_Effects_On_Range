library(tidyverse)
library(gridExtra)
library(geosphere)
library(ggmap)
library(sp)
#library(rgeos)
library(sf)
library(pals)

api_key <- "your_key_here"
register_google(api_key)

`%ni%` <- Negate(`%in%`)

locations <- read_csv("outbreak_size/data/north_america_trap_locations_051225.csv")
remove <- read_csv('outbreak_size/data/plots_to_remove.csv')
corr_df <- read_csv('outbreak_size/data/trap_correlations.csv')

remove_ids <- remove %>% pull(manual_id)

locations <- locations %>% filter(manual_id %ni% remove_ids)

loc_sat <- get_map(location = c(lon= -116.7392,lat = 47), maptype = 'satellite',
                   source = 'google', api_key = api_key)


# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(locations$lon,locations$lat), ncol=2), data.frame(ID=seq(1:length(locations$lon))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="average")

d = 14000
xy$clust <- cutree(hc, h=d)

xy2 <- xy[,c('clust')]

xy_sf <- st_as_sf(xy2)

centroids_grouped <- xy_sf %>% group_by(clust) %>% summarize(st_union(geometry)) %>%
  st_centroid() #%>% st_geometry()

centroids_df <- data.frame(clust = centroids_grouped$clust, cbind(st_coordinates(centroids_grouped)))

dim(centroids_df)

ll_clust <- locations %>% select(lat,lon,manual_id,PlotName,years_trapped,max_trap)

ll_clust$ID <- xy$ID
ll_clust$clust <- xy$clust

ll_clust <- merge(ll_clust,centroids_df)

ll_clust$dist_cent <- distGeo(ll_clust[,3:2],ll_clust[,9:10])

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

min_dist <- cent_dist %>% filter(clust1 != clust2) %>% group_by(clust1) %>% summarize(min_dist = min(dist)) %>% arrange(min_dist)

min_dist %>% filter(min_dist <= 50) %>% ggplot() + aes(x = min_dist) + geom_histogram() + theme_classic()

min_dist %>% head(10)

write_csv(ll_clust,paste0('outbreak_size/clusters/clusters_',d,'.csv'))
write_csv(cent_dist,paste0('outbreak_size/clusters/distances_',d,'.csv'))

pdf(paste0("outbreak_size/clusters/clust_info_",d,".pdf"),height = 4, width = 6)
ll_clust %>% group_by(clust) %>% summarize(mean_dist = mean(dist_cent)/1000,
                                           max_dist = max(dist_cent)/1000,
                                           median_dist = median(dist_cent)/1000) %>% 
  ggplot() + aes(x = median_dist) + geom_histogram() +
  xlab("Median distance to centroid (km)") + 
  theme_classic(base_size = 15)

ll_clust %>% group_by(clust) %>% summarize(mean_dist = mean(dist_cent)/1000,
                                           max_dist = max(dist_cent)/1000,
                                           median_dist = median(dist_cent)/1000) %>% 
  ggplot() + aes(x = max_dist) + geom_histogram() +
  xlab("Max distance to centroid (km)") + 
  theme_classic(base_size = 15)
ll_clust %>% group_by(clust) %>% count(clust) %>% 
  ggplot() + aes(x = n) + geom_histogram() +
  xlab("Number of traps in a cluster") + 
  theme_classic(base_size = 15) + 
  scale_x_log10()
dev.off()

trap_small <-
  trap_data %>% filter(lat <= 47.3, lat >= 46.5, lon <= -116.25, lon >= -117.25)

trap_small3 <- merge(trap_small,ll_clust)

trap_small3 %>% ggplot() + aes(x = year, y = trap_mean, group = manual_id) + geom_line() + 
  facet_wrap(~clust)

plt1 <- ggmap(loc_sat) + 
  geom_text(data = trap_small3, 
            aes(x = lon, y = lat, label = clust, group = clust, color = as.factor(clust)), size = 6) +
  #scale_color_viridis_c(option = 'mako') + 
  #stat_ellipse(data = trap_small3, aes(x = lon, y = lat, group = clust), color = 'yellow', linetype = 'dashed', type = 't') +
  geom_point(data = trap_small3, aes(x = X, y = Y), color = 'red') +
  theme_classic() + xlab("Latitude") + ylab("Longitude") +
  ggtitle(paste0("Cluster depth: ",d)) + 
  theme(legend.position = 'none') +
  scale_color_manual(values = unname(glasbey(length(unique(trap_small3$clust)))))

plt2 <- trap_small3 %>% mutate(lat_coord = round(lat/0.25)*0.25, lon_coord = round(lon/0.25)*0.25) %>% 
  ggplot() + aes(x = year, y = trap_mean, color = as.factor(clust), group = manual_id) + geom_line() +
  theme_classic(base_size = 15) +
  facet_grid(-lat_coord~lon_coord) +
  ylab('Mean pheromone trap') +
  scale_x_continuous(breaks = c(1980,2000,2020))+
  scale_color_manual(values = unname(glasbey(length(unique(trap_small3$clust)))))

pdf(paste0("outbreak_size/clusters/idaho_clust_max_", d,".pdf"),height = 10, width = 20)
grid.arrange(plt1, plt2,nrow = 1)
dev.off()

distance_between_cent <- distm(centroids_df[2:3])

cent_dist <- c()
for(i in 1:length(centroids_df$clust)){
  temp <- data.frame(clust1 = centroids_df$clust[i], clust2 = centroids_df$clust, dist = distance_between_cent[i,])
  cent_dist <- rbind(cent_dist,temp)
}

write_csv(cent_dist,paste0('outbreak_size/clusters/distance_between_clusters_',d,'.csv'))
distances <- cent_dist %>% filter(clust1 != clust2) %>% group_by(clust1) %>% summarize(closest = min(dist)/1000)


