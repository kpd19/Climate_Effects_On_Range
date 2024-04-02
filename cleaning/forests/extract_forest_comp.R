library(raster)
library(tidyverse)
library(sp)
library(rgdal)
library(terra)
library(corrplot)
library(scatterpie)

`%ni%` <- Negate(`%in%`)

forest <- raster("/Volumes/My Book/Forest/conus_forest-type/conus_foresttype.img")
tree_labels <- read_csv("/Volumes/My Book/Forest/conus_forest-type/tree_labels.csv")
latlong <- read_csv("/Volumes/My Book/Synchrony/_model_dat/modis_cover_nbtrap.csv")

ll_info <- latlong %>% filter(lat < 49)

tree_labels <- tree_labels %>% dplyr::select(number,name)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

forest@crs
forest@data@attributes

plot(forest)
points(sites_transformed, pch = 16, col = 'white', cex = 0.75)

a.time <- Sys.time()
sp_cover = raster::extract(forest,sites_transformed, buffer = 1000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time


sp_cover_df <- c()
for(i in 1:num_sites){
  temp <- data.frame(site = ll_info$manual_id[i], number = sp_cover[[i]])
  sp_cover_df <- rbind(sp_cover_df,temp)
}

unique(sp_cover_df$vals)

sp_cover_df <- merge(sp_cover_df,tree_labels,all_x = TRUE)

sp_cover_df <- sp_cover_df %>% drop_na(number)

forest_counts <- sp_cover_df %>% group_by(site) %>% count(name) %>% group_by(site) %>% mutate(sum_n = sum(n)) %>% 
  rename(manual_id = site)

lat_long_wide <- forest_counts %>% drop_na(name) %>% mutate(prop = n/sum_n) %>%
  dplyr::select(-n) %>% pivot_wider(names_from = name, values_from = prop)

lat_long_wide <- merge(ll_info,lat_long_wide,all = TRUE)

# 
# lat_long_wide <- lat_long_wide %>% mutate_at(colnames(lat_long_wide)[17:38], ~replace_na(.,0))

lat_long_wide %>% ggplot() + 
  aes(x = lon, y = lat, color = `Douglas-fir`) + geom_point() + theme_classic(base_size = 15)

lat_long_wide %>% ggplot() + 
  aes(x = lon, y = lat, color = `Ponderosa pine`) + geom_point() + theme_classic(base_size = 15)


##################


missing_sites <- lat_long_wide %>% filter(is.na(sum_n), type == 'site')

sites2 <- missing_sites %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
coordinates(sites2)  <-  c("Longitude",  "Latitude")
proj4string(sites2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed2<-spTransform(sites2, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

sp_cover2 = raster::extract(forest,sites_transformed2, buffer = 5000, factors = TRUE)

sp_cover_df2 <- c()
for(i in 1:length(missing_sites$manual_id)){
  temp <- data.frame(site = missing_sites$manual_id[i], number = sp_cover2[[i]])
  sp_cover_df2 <- rbind(sp_cover_df2,temp)
}

sp_cover_df2 <- merge(sp_cover_df2,tree_labels,all_x = TRUE)

sp_cover_df2 <- sp_cover_df2 %>% drop_na(number)

forest_counts2 <- sp_cover_df2 %>% group_by(site) %>% count(name) %>% group_by(site) %>% mutate(sum_n = sum(n)) %>% 
  rename(manual_id = site) %>% drop_na(name) %>% mutate(prop = n/sum_n) %>%
  dplyr::select(-n) %>% pivot_wider(names_from = name, values_from = prop)

forest_counts2 <- merge(forest_counts2,ll_info)

f1 <- lat_long_wide %>% filter(manual_id %ni% forest_counts2$manual_id)
f2 <- forest_counts2

missing_cols2 <- colnames(f1)[colnames(f1) %ni% colnames(f2)]

f2[,missing_cols2] <- NA

fall <- rbind(f1, f2)

write_csv(fall,"/Volumes/My Book/Synchrony/_model_dat/forest_comp_usa.csv")



##########################
##########################
##########################
##########################

forest <- raster("/Volumes/My Book/Forest/conus_forest-type/ca_tree_sp3.tif")

# x <- rasterToPoints(forest)
# xdf <- data.frame(x)
# 
# write_csv(xdf,"/Volumes/My Book/Forest/conus_forest-type/ca_tree_sp2.csv")

tree_names <- c("Amabilis fir",
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

tree_labels <- data.frame(name = tree_names, number = 1:length(tree_names))


ll_info <- latlong %>% filter(lat >= 49)

ll_info %>% summarize(min_lat = min(lat), max_lat = max(lat),
                      min_lon = min(lon), max_lon = max(lon))

plot(forest)
dim(forest)

proj4string(forest)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=77 +lat_2=49 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

plot(forest)
points(sites_transformed, pch = 19, col = 'black', cex = 0.5)

a.time <- Sys.time()
sp_cover = raster::extract(forest,sites_transformed, buffer = 1000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time


sp_cover_df <- c()
for(i in 1:num_sites){
  temp <- data.frame(site = ll_info$manual_id[i], number = sp_cover[[i]])
  sp_cover_df <- rbind(sp_cover_df,temp)
}

unique(sp_cover_df$vals)

sp_cover_df <- merge(sp_cover_df,tree_labels,all_x = TRUE)

sp_cover_df <- sp_cover_df %>% drop_na(number)

forest_counts_ca <- sp_cover_df %>% group_by(site) %>% count(name) %>% group_by(site) %>% mutate(sum_n = sum(n)) %>% 
  rename(manual_id = site) %>% drop_na(name) %>% mutate(prop = n/sum_n) %>%
  dplyr::select(-n) %>% pivot_wider(names_from = name, values_from = prop)

forest_counts_ca <- merge(forest_counts_ca,ll_info, all = TRUE)

forest_counts_ca %>% ggplot() + aes(x = lon, y = lat, color = `Douglas-fir`) + geom_point() + theme_classic(base_size = 15) + 
  scale_color_viridis_c()

forest_counts_ca %>% ggplot() + aes(x = lon, y = lat, color = `Ponderosa pine`) + geom_point() + theme_classic(base_size = 15) + 
  scale_color_viridis_c()

##################
##################
##################

missing_ca <- forest_counts_ca %>% filter(is.na(sum_n), type == 'site')
write_csv(forest_counts_ca,"/Volumes/My Book/Synchrony/_model_dat/forest_comp_ca.csv")

missing_cols3 <- colnames(fall)[colnames(fall) %ni% colnames(forest_counts_ca)]

forest_counts_ca[,missing_cols3] <- NA

missing_cols4 <- colnames(forest_counts_ca)[colnames(forest_counts_ca) %ni% colnames(fall)]

fall[,missing_cols4] <- NA

fall2 <- rbind(fall, forest_counts_ca)

write_csv(fall2,"/Volumes/My Book/Synchrony/_model_dat/forest_comp_usaca_nbtrap.csv")


##########################
##########################
##########################
##########################
##########################

colnames(fall2)

fall3 <- fall2 %>% mutate_at(colnames(fall2)[34:108], ~replace_na(.,0))

fall4 <- fall3 %>% select(c('manual_id','lat','lon','sum_n',colnames(fall2)[34:108]))

fall4_long <- fall4 %>% pivot_longer(colnames(fall2)[34:108]) %>% mutate(n = value*sum_n)

write_csv(fall4_long,"/Volumes/My Book/Synchrony/_model_dat/forest_long_usaca_nbtrap.csv")

dom_species <- fall4_long %>% group_by(manual_id,lat,lon) %>% mutate(max_n = max(n)) %>% filter(max_n == n)

unique(dom_species$name)

dom_species2 <- dom_species %>% ungroup() %>% count(name) %>% arrange(desc(n))
dom_species_count <- dom_species2 %>% filter(n >= 100) %>% pull(name)

pdf("_Plots/tree_sp_distribution_alpha.pdf",height = 10, width = 15)
latlong2 %>% filter(name %in% dom_species_count) %>%  ggplot() + aes(x = lon,y = lat, color = name, alpha = n/sum_n) + 
  geom_point() + theme_classic(base_size = 15) + 
  facet_wrap(~name)
dev.off()


pdf("_Plots/tree_sp_distribution.pdf",height = 10, width = 15)
fall4_long %>% filter(name %in% dom_species_count) %>% filter(value >0) %>% 
  ggplot() + aes(x = lon,y = lat, color = value) + 
  geom_point() + theme_classic(base_size = 15) + 
  facet_wrap(~name)
dev.off()

write_csv(lat_long_wide,"_Data/forest_composition_wide_USA.csv")

# edited CSV manually to fix CR3
#write_csv(lat_long_wide, "forest_composition_manual_wide.csv")


lat_long_wide %>% filter(`Douglas-fir` >0) %>% ggplot() + aes(x = lon,y = lat,color = `Douglas-fir`) + 
  geom_point() + theme_classic(base_size = 15) + 
  scale_color_viridis_c(option = 'viridis')

latlong %>% group_by(State) %>% summarize(max_lon = max(lon),
                                          min_lon = min(lon),
                                          max_lat = max(lat),
                                          min_lat = min(lat)) %>% 
  filter(State %in% c("AZ","CO","CA",'NM'))


##############################
##############################
##############################
##############################


fall3 <- fall2 %>% mutate_at(colnames(fall2)[34:108], ~replace_na(.,0))
fall3 <- fall3 %>% mutate_at(colnames(fall2)[11:28], ~replace_na(.,0))

write_csv(fall3,"/Volumes/My Book/Synchrony/_model_dat/forest_comp_usaca_nbtrap0.csv")


a <- fall3 %>% filter(type == 'site', drop == TRUE)
b <- fall3 %>% filter(type == 'site', near_needle == 0)
c <- fall3 %>% filter(type == 'site', is.na(sum_n))

fall3 %>% filter(PlotName == "RY_Timber")

