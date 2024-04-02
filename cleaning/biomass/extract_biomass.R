library(raster)
library(tidyverse)
library(sp)
library(sf)
#library(ggmap)
#library(rgdal)
library(terra)
#library(corrplot)
#library(scatterpie)

`%ni%` <- Negate(`%in%`)

forest <- raster("/Volumes/My Book/Forest/conus_forest_biomass/conus_forest_biomass_mg_per_ha.img")
canada <- raster("/Volumes/My Book/Forest/nfi_kNN2011/kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif")
latlong <- read_csv("_gendata/all_trap_max_edits.csv")
coords <- read_csv("/Volumes/My Book/Synchrony/_model_dat/coord_info2.csv")

modis <- raster("/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/NA_NALCMS_landcover_2010v2_250m.tif")
modis_ids_vals <- read_csv("/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/modis_ids.csv")

##############
##############
##############
##############

nearby_coords <- coords %>% filter(track %ni% c("in"))

nearby_coords <- merge(nearby_coords, data.frame(plt = 1:3))

nearby_coords$lat_push <- runif(n = dim(nearby_coords)[1], min = -0.125, max = 0.125)
nearby_coords$lon_push <- runif(n = dim(nearby_coords)[1], min = -0.125, max = 0.125)

nearby_coords <- nearby_coords %>% mutate(Latitude = latitude + lat_push, 
                                          Longitude = longitude + lon_push) %>% mutate(j = 1:length(lat_push))

write_csv(nearby_coords,"/Volumes/My Book/Synchrony/_model_dat/nearby_coords_large.csv")

site_lls <- latlong %>% select(lat,lon,manual_id) %>% mutate(type = 'site')
nb_lls <- nearby_coords %>% select(Latitude,Longitude,j) %>% rename(lat = Latitude, lon = Longitude, manual_id = j) %>% 
  mutate(manual_id = manual_id + 1e4) %>% mutate(type = 'not site')

all_lls <- rbind(site_lls,nb_lls)

proj4string(modis)

ll_info_modis <- all_lls %>% dplyr::select(manual_id,lat,lon)

sites_modis <- ll_info_modis %>% ungroup() %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites_modis)[1]
coordinates(sites_modis)  <-  c("Longitude",  "Latitude")
proj4string(sites_modis)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_modis_transformed<-spTransform(sites_modis, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"))

plot(modis)
points(sites_modis_transformed, pch = 16, col = 'black', cex = 0.25)

a.time <- Sys.time()
modis_id = raster::extract(modis,sites_modis_transformed, buffer = NA)
b.time <- Sys.time()
b.time - a.time

all_lls$modis_id <- unlist(modis_id)

all_lls <- merge(all_lls,modis_ids_vals)

pdf("_plots/_modis/modis_vals.pdf", height = 10, width = 20)
all_lls %>% #filter(type == 'site') %>%
  ggplot() + aes(x = lon,y = lat, color = classification) + geom_point() + theme_classic(base_size = 15) + 
  facet_wrap(~classification)
dev.off()

water_probs <- all_lls %>% filter(type == 'site', classification %in% c("Water",'Urban and Built-up','Cropland','Barren Lands'))

water_probs2 <- merge(water_probs,latlong) %>% arrange(classification)


write_csv(water_probs2, "/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/modis_problems2.csv")
water_probs2 <- read_csv("/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/modis_problems2.csv")

water_keep <- water_probs2 %>% filter(drop == FALSE)
water_drop <-  water_probs2 %>% filter(drop == TRUE)

sites_water <- sites_modis_transformed[sites_modis_transformed$manual_id %in% water_keep$manual_id,]

keep_modis <- raster::extract(modis,sites_water, buffer = 1000)

keep_modis_df <- c()
for(i in 1:length(sites_water$manual_id)){
  
  cover = keep_modis[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = sites_water$manual_id[i], modis_id = cover)
  } else{
    temp <- data.frame(manual_id = sites_water$manual_id[i], modis_id = NA)
  }
  
  keep_modis_df <- rbind(keep_modis_df,temp)
}

modis_updates <- keep_modis_df %>% group_by(manual_id) %>% summarize(modis_id = min(modis_id))

modis_updates <- merge(modis_updates,modis_ids_vals) %>% mutate(type = 'site')

all_lls1 <- all_lls %>% filter(manual_id %ni% sites_water$manual_id)

modis_fix <- all_lls %>% filter(manual_id %in% sites_water$manual_id) %>% select(-modis_id) %>% select(-classification)
modis_fix <- merge(modis_fix,modis_updates)

all_lls1 <- rbind(all_lls1,modis_fix)

all_lls1 <- all_lls1 %>% mutate(drop = ifelse(manual_id %in% water_drop$manual_id,TRUE,FALSE))
all_lls1 %>% filter(type == 'site', drop == FALSE) %>%
  ggplot() + aes(x = classification, y = max_trap, group = classification)+ geom_boxplot() + theme_classic() +
  facet_wrap(~State)

write_csv(all_lls1, "/Volumes/My Book/Synchrony/_model_dat/modis_classifications.csv")

#all_lls1 <- merge(all_lls1,latlong)
# all_lls1 <- merge(all_lls1,cover_total)
# write_csv(all_lls1, "/Volumes/My Book/Synchrony/_model_dat/modis_meanco.csv")

a.time <- Sys.time()
modis_id = raster::extract(modis,sites_modis_transformed, buffer = 5000)
b.time <- Sys.time()
b.time - a.time

modis_1k <- c()
for(i in 1:length(sites_modis_transformed$manual_id)){
  
  cover = modis_id[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = sites_modis_transformed$manual_id[i], modis_id = cover)
  } else{
    temp <- data.frame(manual_id = sites_modis_transformed$manual_id[i], modis_id = NA)
  }
  
  modis_1k <- rbind(modis_1k,temp)
}

modis_1k <- merge(modis_1k,modis_ids_vals)

write_csv(modis_1k,'/Volumes/My Book/Synchrony/_model_dat/modis_5k_long.csv') 


modis_1k_counts <- modis_1k %>% group_by(manual_id) %>% count(classification) %>% group_by(manual_id) %>% 
  mutate(total = sum(n)) %>% mutate(p = n/total)

modis_1k_wide <- modis_1k_counts %>% select(manual_id, classification,p) %>% pivot_wider(names_from = classification, values_from = p)

modis_1k_wide <- modis_1k_wide %>% mutate(near_needle = ifelse(is.na(`Temperate or sub-polar needleleaf forest`) == TRUE, FALSE,TRUE))

test <- merge(modis_1k_wide,latlong)

test %>% ggplot() +
  aes(x = `Temperate or sub-polar shrubland` + `Temperate or sub-polar needleleaf forest` + `Temperate or sub-polar needleleaf forest`, y = max_trap) + geom_point() + theme_classic()

test_off <- test %>% filter(is.na(`Temperate or sub-polar needleleaf forest`))

write_csv(test_off, "/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/modis_problems_5k2.csv")

test_off2 <- read_csv("/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/modis_problems_5k2.csv")

trap_fine <- test_off2 %>% filter(drop == FALSE) %>% pull(manual_id)

modis_1k_counts %>% filter(manual_id <= 9000) %>% filter(classification == 'Temperate or sub-polar needleleaf forest')

modis_1k_wide <- modis_1k_wide %>% mutate(near_needle = ifelse(manual_id %in% trap_fine,1,near_needle))

modis_1k_wide2 <- merge(modis_1k_wide,all_lls1)

pdf("_plots/_modis/needleleaf_prop.pdf",height = 10, width = 15)
modis_1k_wide2 %>% drop_na(`Temperate or sub-polar needleleaf forest`) %>%
  ggplot() + aes(x = lon,y = lat, color = `Temperate or sub-polar needleleaf forest`) +
  geom_point() + theme_classic(base_size = 15) +
  scale_color_viridis_c()
dev.off()

write_csv(modis_1k_wide2,'/Volumes/My Book/Synchrony/_model_dat/modis_5k_nearneedle.csv') 

needle_filter <- modis == 1

plot(needle_filter)

writeRaster(needle_filter, "/Volumes/My Book/Forest/Land_cover_2010v2_250m_TIF/NA_NALCMS_landcover_2010v2_250m/data/needle_layer.tiff", format = 'GTiff')



####################
####################

ll_info <- all_lls %>% dplyr::select(manual_id,lat,lon) %>% filter(lat < 49)

plot(forest)
proj4string(forest)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

forest@crs
forest@data@attributes

plot(forest)
points(sites_transformed, pch = 16, col = 'black', cex = 0.25)

a.time <- Sys.time()
sp_cover = raster::extract(forest,sites_transformed, buffer = 3000, fn = 'mean')
b.time <- Sys.time()
b.time - a.time

sp_cover

sp_cover_df <- c()
for(i in 1:num_sites){
  
  cover = sp_cover[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = ll_info$manual_id[i], cover = cover)
  } else{
    temp <- data.frame(manual_id = ll_info$manual_id[i], cover = NA)
  }
  
  sp_cover_df <- rbind(sp_cover_df,temp)
}


sp_cover_df <- merge(sp_cover_df, all_lls1)

cover_means <- sp_cover_df %>% 
  group_by(lat,lon,manual_id) %>% summarize(mean_cover = mean(cover),
                                            max_cover = max(cover))

cover_means %>% ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')

write_csv(cover_means, "/Volumes/My Book/Synchrony/_model_dat/cover_conus_all_3kmean_updated.csv")

##############
##############
##############
##############


plot(canada)
cat(canada@srs)

ll_info_bc <- all_lls %>% dplyr::select(manual_id,lat,lon) %>% filter(lat >= 49)

sites <- ll_info_bc %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
#proj4string(sites)  <- CRS(SRS_string = "EPSG:4326") # this is what google is
sites_transformed<- spTransform(sites, CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +a=6378137 +rf=298.257221999999 +units=m +no_defs"))

proj4string(canada)

pdf("_plots/canada.pdf")
plot(canada)
points(sites_transformed, pch = 16, col = 'black', cex = 0.25)
dev.off()

a.time <- Sys.time()
sp_cover = raster::extract(canada,sites_transformed, buffer = 3000, fn = 'mean')
b.time <- Sys.time()
b.time - a.time

sp_cover_df <- c()
for(i in 1:num_sites){
  
  cover = sp_cover[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = ll_info_bc$manual_id[i], cover = cover)
  } else{
    temp <- data.frame(manual_id = ll_info_bc$manual_id[i], cover = NA)
  }
  
  sp_cover_df <- rbind(sp_cover_df,temp)
}


cover_all_bc <- merge(sp_cover_df, all_lls1)

cover_means_bc <- cover_all_bc %>% 
  group_by(lat,lon,manual_id) %>%
  summarize(mean_cover = mean(cover,na.rm=TRUE)*0.907185,
            max_cover = max(cover,na.rm=TRUE)*0.907185)

cover_means_bc %>% ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')


cover_ttrap <- rbind(cover_means_bc,cover_means)

write_csv(cover_ttrap, "/Volumes/My Book/Synchrony/_model_dat/cover_usaca_all_3kmean_updated.csv")

cover_ttrap %>% ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')


cover_ttrap %>% %>% ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')

##############
##############
##############
##############

site_lls <- latlong %>% mutate(type = 'site')
nb_lls <- nearby_coords %>% select(Latitude,Longitude,j) %>% rename(lat = Latitude, lon = Longitude, manual_id = j) %>% 
  mutate(manual_id = manual_id + 1e4) %>% mutate(type = 'not site')  %>%
  mutate(PlotName = paste0('synthetic-',manual_id), State = NA, years_trapped = 0, max_trap = NA)

all_ll_info <- rbind(site_lls,nb_lls)

all_ll_co <- merge(all_ll_info,cover_ttrap)


modis_1k_wide2 %>% ggplot() + aes(x = lon,y = lat, color = classification) + geom_point() + theme_classic(base_size = 15) +
  facet_wrap(~classification)


modis_1k_wide2 %>% ggplot() + aes(x = lon,y = lat, color = classification) + geom_point() + theme_classic(base_size = 15) +
  facet_wrap(~classification)

pdf("_plots/_modis/modis_vals_trap.pdf",height = 10, width = 15)
modis_1k_wide2 %>% filter(type == 'site', drop == FALSE) %>%
  ggplot() + aes(x = lon,y = lat, color = classification) + geom_point() + theme_classic(base_size = 15) +
  facet_wrap(~classification) +
  theme(legend.position = 'none')
dev.off()


all_ll_co2 <- merge(all_ll_co, modis_1k_wide)
all_ll_co2 <- merge(all_ll_co2, all_lls1)


all_ll_co2 %>% filter(classification == 'Temperate or sub-polar grassland',type == 'site')

pdf("_plots/_modis/near_needle_cover.pdf",height = 10, width = 15)
all_ll_co2 %>% #filter(near_needle == FALSE) %>%
  ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo') + facet_wrap(~near_needle)
dev.off()

pdf("_plots/_modis/near_needle_cover_class.pdf",height = 10, width = 15)
all_ll_co2 %>% filter(near_needle == TRUE) %>%
  ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo') + facet_wrap(~classification)
dev.off()

all_ll_co2 %>% filter(near_needle == TRUE, 
                      classification %in% c("Temperate or sub-polar shrubland", 'Temperate or sub-polar needleleaf forest',
                                            "Temperate or sub-polar grassland")) %>% 
  ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo') + facet_wrap(~classification)

write_csv(all_ll_co2, "/Volumes/My Book/Synchrony/_model_dat/modis_cover_nbtrap.csv")


##############
##############
##############
##############

latlong <- read_csv("_gendata/all_trap_max.csv")

ll_info <- latlong %>% filter(manual_id %in% c(1725,978))

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

proj4string(forest)
forest@data@attributes

plot(forest)
points(sites_transformed, pch = 16, col = 'black', cex = 0.25)

a.time <- Sys.time()
sp_cover = raster::extract(forest,sites_transformed, buffer = 1000, fn = 'mean')
b.time <- Sys.time()
b.time - a.time

sp_cover_df <- c()
for(i in 1:num_sites){
  
  cover = sp_cover[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = ll_info$manual_id[i], cover = cover)
  } else{
    temp <- data.frame(manual_id = ll_info$manual_id[i], cover = NA)
  }
  
  sp_cover_df <- rbind(sp_cover_df,temp)
}


sp_cover_df <- merge(sp_cover_df, ll_info)

cover_all <- merge(sp_cover_df,latlong)

cover_means <- cover_all %>% group_by(lat,lon,manual_id,max_trap,State,PlotName) %>% summarize(mean_cover = mean(cover))

cover_means %>% 
  ggplot() + aes(x = mean_cover, y = max_trap) + geom_point() + theme_classic() 

cover_means %>% ggplot() + aes(x = lon, y = lat, color = mean_cover) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')

write_csv(cover_means, "/Volumes/My Book/Synchrony/_forestypes/cover_missing_all_1kmean.csv")



######################
######################
######################
######################

ll2 <- latlong %>% filter(State == "WA") 

sites <- ll_info %>% filter(State == "ID") %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=longlat +datum=NAD27 +no_defs"))

proj4string(forest_dftm)

ll_vals <- latlong %>% filter(State == "ID") %>% summarize(min_lat = min(lat),
                                                max_lat = max(lat),
                                                min_lon = min(lon),
                                                max_lon = max(lon))

forest2 <- projectRaster(forest, crs = '+proj=longlat +datum=NAD27')
dftm_box = extent(ll_vals$min_lon, ll_vals$max_lon, ll_vals$min_lat, ll_vals$max_lat)

forest_dftm <- forest2 %>% raster::crop(dftm_box)

plot(forest_dftm)
points(sites_transformed, pch = 16, col = 'black', cex = 0.25)

id_df <- as.data.frame(forest_dftm, xy = TRUE)

wa_df %>% filter(Layer_1 >0) %>% # , y <= 43.1, y >= 42.75, x <= -116.0, x >= -116.5) %>%
  ggplot() + aes(x = x, y = y, color = Layer_1, fill = Layer_1) +
  geom_tile() + theme_classic(base_size = 15) + 
  scale_color_viridis_c(option = 'mako') + 
  scale_fill_viridis_c(option = 'mako') +
  geom_point(data = ll2, aes(x = lon, y = lat), color = 'red', fill = NA,size = 0.5)

uni_x <- unique(wa_df$x)
uni_y <- unique(wa_df$y) 


ll2$x <- 0
ll2$y <- 0

for(i in 1:length(ll2$lat)){
  
  ll2[i,]$y <- uni_y[which.min(abs(ll2[i,]$lat - uni_y))]
  ll2[i,]$x <- uni_x[which.min(abs(ll2[i,]$lon - uni_x))]
  
}


ll2 <- merge(ll2,wa_df)


write_csv(wa_df, "/Volumes/My Book/Synchrony/_defoliation/washington_raster_fcover.csv")
write_csv(id_df, "/Volumes/My Book/Synchrony/_defoliation/idaho_raster_fcover.csv")


test <- read_csv("/Volumes/My Book/Synchrony/_forestypes/cover_usaca_all_1kmean.csv")

a <- test %>% filter(mean_cover ==0, State == "WA")

id_test <- 1229

lat_t <- a %>% filter(manual_id == id_test) %>% pull(lat) + 0.1
lat_b <- a %>% filter(manual_id == id_test) %>% pull(lat) - 0.1

lon_t <- a %>% filter(manual_id == id_test) %>% pull(lon) + 0.1
lon_b <- a %>% filter(manual_id == id_test) %>% pull(lon) - 0.1

wa_df %>% filter(Layer_1 >0 , y <= lat_t, y >= lat_b, x <= lon_t, x >= lon_b) %>%
  ggplot() + aes(x = x, y = y, color = Layer_1, fill = Layer_1) +
  geom_tile() + theme_classic(base_size = 15) + 
  scale_color_viridis_c(option = 'mako') + 
  scale_fill_viridis_c(option = 'mako') +
  geom_point(data = ll2[ll2$manual_id %in% id_test,], aes(x = lon, y = lat), color = 'red', fill = NA,size = 0.5)

########################
########################
########################
########################
########################

