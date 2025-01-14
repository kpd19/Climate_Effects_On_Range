library(raster)
library(tidyverse)
library(sp)

`%ni%` <- Negate(`%in%`)

forest <- raster("data/conus_foresttype.img")
tree_labels <- read_csv("data/tree_labels.csv")
latlong <- read_csv("data/range_locations_example.csv")

ll_info_us <- latlong %>% filter(lat <= 49)
ll_info_us$df_id <- 1:length(ll_info_us$lat)

tree_labels <- tree_labels %>% dplyr::select(number,name)

sites <- ll_info_us %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

plot(forest)
points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_forest = raster::extract(forest,sites_transformed, buffer = 3000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_forest_long <- sp_forest %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_forest_long <- sp_forest_long %>% rename(df_id = name,number = value)

sp_forest_long <- merge(sp_forest_long,tree_labels)

sp_forest_long <- merge(sp_forest_long,ll_info_us,all = TRUE)

head(sp_forest_long)

forest_summary <- sp_forest_long %>% mutate(n = 1) %>% 
  group_by(lat,lon,manual_id,source) %>% mutate(total = sum(n)) %>% 
  mutate(n = 1) %>% group_by(lat,lon,manual_id,total,name,source) %>%
  summarize(sum_n = sum(n))

forest_summary <- forest_summary %>% mutate(p = sum_n/total)

forest_summary_wide <- forest_summary %>% select(lat,lon,manual_id,name,p,source) %>% pivot_wider(names_from = name,values_from = p)

forest_summary_wide <- forest_summary_wide %>% rename(`No trees` =`NA`)

forest_summary_wide %>% ungroup() %>% count(`No trees`)

forest_summary_wide %>% ggplot() + aes(x = lon,y = lat, color = `Douglas-fir`) + geom_point() + theme_classic()

##########################
##########################
##########################
##########################

missing_us <- forest_summary_wide %>% filter(`No trees` == 1, source != "Synthetic data")

if (length(missing_us$total) >0){
  missing_us$df_id <- 1:length(missing_us$total)
  
  sites2 <- missing_us %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
  num_sites2 <- dim(sites2)[1]
  coordinates(sites2)  <-  c("Longitude",  "Latitude")
  proj4string(sites2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
  sites_transformed2 <-spTransform(sites2, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))
  
  plot(forest)
  points(sites_transformed2)
  
  forest_us2 = raster::extract(forest,sites_transformed2, buffer = 10000, factors = TRUE)
  
  forest_us2_long <- forest_us2 %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
  forest_us2_long <- forest_us2_long %>% rename(df_id = name,number = value)
  
  forest_us2_long <- merge(forest_us2_long,tree_labels)
  
  forest_us2_long <- merge(forest_us2_long,missing_us)
  
  forest_us2_summary <- forest_us2_long %>% mutate(n = 1) %>% 
    group_by(lat,lon,manual_id,source) %>% mutate(total = sum(n)) %>% 
    mutate(n = 1) %>% group_by(lat,lon,manual_id,total,name,source) %>%
    summarize(sum_n = sum(n))
  
  forest_us2_summary <- forest_us2_summary %>% mutate(p = sum_n/total)
  
  forest_us2_summary_wide <- forest_us2_summary %>% select(lat,lon,manual_id,name,p,source) %>% pivot_wider(names_from = name,values_from = p)
  
}

##########################
##########################
##########################
##########################

canada <- raster("data/ca_tree_sp5.tif")

tree_names_ca <- c("Amabilis fir",
                "Balsam fir",
                "Subalpine fir",
                "Bigleaf maple",
                "Red maple",
                "Sugar maple",
                "Gray alder",
                "Red alder",
                "Yellow birch",
                "White birch",
                "Yellow-cedar",
                "Black ash",
                "Tamarack",
                "Western larch",
                "Norway spruce",
                "Engelmann spruce",
                "White spruce",
                "Black spruce",
                "Red spruce",
                "Sitka spruce",
                "Whitebark pine",
                "Jack pine",
                "Lodgepole pine",
                "Ponderosa pine",
                "Red pine",
                "Eastern white pine",
                "Balsam poplar",
                "Largetooth aspen",
                "Trembling aspen",
                "Douglas-fir",
                "Red oak",
                "Eastern white-cedar",
                "Western redcedar",
                "Eastern hemlock",
                "Western hemlock",
                "Mountain hemlock",
                "White elm")

tree_labels_ca <- data.frame(name = tree_names_ca, number = 1:length(tree_names_ca))

successful_sites <- forest_summary_wide %>% filter(is.na(`No trees`))

if (length(missing_us$total) >0){
  successful_sites2 <- forest_us2_summary_wide
  ll_info_ca <- latlong %>% filter(manual_id %ni% c(successful_sites$manual_id,successful_sites2$manual_id), lat >= 48)
} else {
  ll_info_ca <- latlong %>% filter(manual_id %ni% c(successful_sites$manual_id), lat >= 48)
  
} 
  
ll_info_ca$df_id <- 1:length(ll_info_ca$lat)

ll_info_ca %>% summarize(min_lat = min(lat), max_lat = max(lat),
                      min_lon = min(lon), max_lon = max(lon))

sites <- ll_info_ca %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=77 +lat_2=49 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

plot(canada)
points(sites_transformed, pch = 19, col = 'black', cex = 0.5)

a.time <- Sys.time()
forest_ca = raster::extract(canada,sites_transformed, buffer = 3000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

forest_ca_long <- forest_ca %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
forest_ca_long <- forest_ca_long %>% rename(df_id = name,number = value)

forest_ca_long <- merge(forest_ca_long,tree_labels_ca)

forest_ca_long <- merge(forest_ca_long,ll_info_ca, all =TRUE)

head(forest_ca_long)

forest_ca_summary <- forest_ca_long %>% mutate(n = 1) %>% 
  group_by(lat,lon,manual_id,source) %>% mutate(total = sum(n)) %>% 
  mutate(n = 1) %>% group_by(lat,lon,manual_id,total,name,source) %>%
  summarize(sum_n = sum(n))

forest_ca_summary <- forest_ca_summary %>% mutate(p = sum_n/total)

forest_ca_summary_wide <- forest_ca_summary %>% select(lat,lon,manual_id,name,p,source) %>% pivot_wider(names_from = name,values_from = p)

forest_ca_summary_wide <- forest_ca_summary_wide %>% rename(`No trees` =`NA`)

forest_ca_summary_wide %>% ungroup() %>% count(`No trees`)

forest_ca_summary_wide %>% ggplot() + aes(x = lon,y = lat, color = `Douglas-fir`) + geom_point() + theme_classic()

##################
##################
##################

missing_ca <- forest_ca_summary_wide %>% filter(`No trees` == 1, source != "Synthetic data")

if (length(missing_ca$total) >0){
  missing_ca$df_id <- 1:length(missing_ca$total)
  sites2 <- missing_ca %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
  num_sites <- dim(sites2)[1]
  coordinates(sites2)  <-  c("Longitude",  "Latitude")
  proj4string(sites2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
  sites_transformed2 <-spTransform(sites2, CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=77 +lat_2=49 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  forest_ca2 = raster::extract(canada,sites_transformed2, buffer = 10000, factors = TRUE)
  
  forest_ca2_long <- forest_ca2 %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
  forest_ca2_long <- forest_ca2_long %>% rename(df_id = name,number = value)
  
  forest_ca2_long <- merge(forest_ca2_long,tree_labels_ca)
  
  forest_ca2_long <- merge(forest_ca2_long,missing_ca)
  
  forest_ca2_summary <- forest_ca2_long %>% mutate(n = 1) %>% 
    group_by(lat,lon,manual_id,source) %>% mutate(total = sum(n)) %>% 
    mutate(n = 1) %>% group_by(lat,lon,manual_id,total,name,source) %>%
    summarize(sum_n = sum(n))
  
  forest_ca2_summary <- forest_ca2_summary %>% mutate(p = sum_n/total)
  
  forest_ca2_summary_wide <- forest_ca2_summary %>% select(lat,lon,manual_id,name,p,source) %>% pivot_wider(names_from = name,values_from = p)
  
  forest_ca2_summary_wide %>% ggplot() + aes(x = lon,y = lat, color = `Douglas-fir`) + geom_point() + theme_classic()
}
################
################
################
################

if (length(missing_ca$total) >0){
  fca1 <- forest_ca_summary_wide %>% filter(manual_id %ni% forest_ca2_summary_wide$manual_id)
  fca2 <- forest_ca2_summary_wide
  fca <- rbind(fca1,fca2)
} else {
   fca <- forest_ca_summary_wide
}

canada_successful <- fca %>% filter(is.na(`No trees`)) 

if (length(missing_us$total) >0){
  fus2 <- forest_us2_summary_wide
  fus1 <- forest_summary_wide %>% filter(manual_id %ni% c(fus2$manual_id,canada_successful$manual_id))
  
  fus <- rbind(fus1,fus2)
} else {
  fus <- forest_summary_wide %>% filter(manual_id %ni% c(canada_successful$manual_id))
}

problem_ids <- fus$manual_id[fus$manual_id %in% fca$manual_id]

issues <- fus %>% filter(manual_id %in% problem_ids)

fca <- fca %>% filter(manual_id %ni% issues$manual_id)

missing_cols1 <- colnames(fca)[colnames(fca) %ni% colnames(fus)]
missing_cols2 <- colnames(fus)[colnames(fus) %ni% colnames(fca)]

fus[,missing_cols1] <- NA
fca[,missing_cols2] <- NA

fall <- rbind(fus,fca)

write_csv(fall,"data/forest_composition_example.csv")

fall %>% ggplot() + aes(x = lon, y = lat, color = `Ponderosa pine`) + geom_point() +
  theme_classic(base_size = 15)
