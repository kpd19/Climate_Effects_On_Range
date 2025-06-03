library(raster)
library(tidyverse)
#library(sp)
#library(sf)
library(gridExtra)
#library(geosphere)

`%ni%` <- Negate(`%in%`)

us_biomass <- raster("landscape/data/conus_forest_biomass_mg_per_ha.img")
canada_biomass <- raster("landscape/data/kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif")
latlong <- read_csv("population_data/data/range_locations_example.csv")

latlong <- latlong %>% dplyr::select(manual_id,lat,lon) #%>% mutate(source = "Synthetic data")

latlong %>% ggplot() + aes(x = lon,y = lat) + geom_point() + theme_classic(base_size = 15)

####################
####################

ll_info_us <- latlong %>% dplyr::select(manual_id,lat,lon) %>% filter(lat <= 49)

ll_info_us$df_id <- 1:length(ll_info_us$manual_id)

sites <- ll_info_us %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs"))

#plot(us_biomass)
#points(sites_transformed, pch = 16, col = 'black', cex = 0.25)

a.time <- Sys.time()
sp_biomass_us = raster::extract(us_biomass,sites_transformed, buffer = 3000)
b.time <- Sys.time()
b.time - a.time

sp_biomass_us_long <- sp_biomass_us %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_biomass_us_long <- sp_biomass_us_long %>% rename(df_id = name,number = value)

biomass_us <- sp_biomass_us_long %>% group_by(df_id) %>% summarize(mean_biomass = mean(number,na.rm=TRUE),
                                                                   max_biomass = max(number,na.rm = TRUE))

biomass_us <- biomass_us %>% mutate(mean_biomass = ifelse(is.nan(mean_biomass), NA,mean_biomass),
                      max_biomass = ifelse(max_biomass == -Inf, NA,max_biomass))

biomass_us <- merge(biomass_us,ll_info_us, all = TRUE)

biomass_us %>% ggplot() + aes(x = lon, y = lat, color = mean_biomass) + geom_point() + theme_classic(base_size = 15) + 
  scale_color_viridis_c()

us_notna <- biomass_us %>% drop_na(mean_biomass) %>% filter(lat >= 48) %>% pull(manual_id)

##############
##############
##############
##############

ll_info_ca <- latlong %>% dplyr::select(manual_id,lat,lon) %>% filter(lat >= 48, manual_id %ni% us_notna)
ll_info_ca$df_id <- 1:length(ll_info_ca$manual_id)

ll_info_ca2 <- ll_info_ca %>% rename(lat_site = lat, lon_site = lon)

sites <- ll_info_ca %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
#proj4string(sites)  <- CRS(SRS_string = "EPSG:4326") # this is what google is
sites_transformed<- spTransform(sites, CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +a=6378137 +rf=298.257221999999 +units=m +no_defs"))

proj4string(canada_biomass)
plot(canada_biomass)
points(sites_transformed, pch = 16, col = 'black', cex = 0.25)

a.time <- Sys.time()
sp_biomass_ca = raster::extract(canada_biomass,sites_transformed, buffer = 3000,cellnumbers = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_biomass_ca_long <- sp_biomass_ca %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('value'))

sp_biomass_ca_long$cellnumber <- sp_biomass_ca_long[,2]$value[,1]
sp_biomass_ca_long$biomass <- sp_biomass_ca_long[,2]$value[,2]

coords <- unique(sp_biomass_ca_long$cellnumber)

xy_vals <- data.frame(xyFromCell(canada_biomass,coords))
xy_vals$cell <- coords
xy_vals <- xy_vals %>% drop_na(x)
coordinates(xy_vals) <- ~x+y
proj4string(xy_vals)  <- CRS("+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +a=6378137 +rf=298.257221999999 +units=m +no_defs")

xy_vals2 <- spTransform(xy_vals, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # this is what google is)

xy_vals2 <- data.frame(xy_vals2)
colnames(xy_vals2) <- c('cellnumber','lon','lat','optional')

sp_biomass_ca_long <- sp_biomass_ca_long %>% select(name,cellnumber,biomass)

xy_vals2 <- merge(xy_vals2,sp_biomass_ca_long)

xy_vals2 %>% ggplot() + aes(x = lon, y = lat, color = biomass) + geom_point(alpha = 0.5, shape = 19, size = 1) + theme_classic() +
  coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(option = 'turbo')

xy_vals2 <- xy_vals2 %>% rename(df_id = name)

xy_vals2 <- merge(xy_vals2,ll_info_ca2)

biomass_small <- xy_vals2 %>% select(lat,lon,biomass,df_id,lon_site,lat_site)

##############
##############
##############
##############

biomass_coords <- biomass_small %>% group_by(lat,lon) %>% count(lat) %>% arrange(desc(n)) %>% select(-n)

summary(biomass_coords)

`%ni%` <- Negate(`%in%`)

corners <- data.frame(grid = c(1:9),
                      lat_top = c(60,60,60,50,50,50,40,40,40),
                      lon_left = c(130,120,110,130,120,110,130,120,110))
corners <- corners %>%
  mutate(cover_file = paste0("landscape/data/Hansen_GFC-2023-v1.11_treecover2000_",lat_top,"N_",lon_left,"W.tif"),
         loss_file = paste0("landscape/data/Hansen_GFC-2023-v1.11_lossyear_",lat_top,"N_",lon_left,"W.tif"),
         gain_file = paste0("landscape/data/Hansen_GFC-2023-v1.11_gain_",lat_top,"N_",lon_left,"W.tif"))

biomass_coords <- biomass_coords %>% mutate(grid = case_when(lat <= 60 & lat >50 & lon >= -130 & lon <=-120 ~ 1,
                                            lat <= 60 & lat >50 & lon >= -120 & lon <=-110 ~ 2,
                                            lat <= 60 & lat >50 & lon >= -110 & lon <=-100 ~ 3,
                                            lat <= 50 & lat >40 & lon >= -130 & lon <=-120 ~ 4,
                                            lat <= 50 & lat >40 & lon >= -120 & lon <=-110 ~ 5,
                                            lat <= 50 & lat >40 & lon >= -110 & lon <=-100 ~ 6,
                                            lat <= 40 & lat >30 & lon >= -130 & lon <=-120 ~ 7,
                                            lat <= 40 & lat >30 & lon >= -120 & lon <=-110 ~ 8,
                                            lat <= 40 & lat >30 & lon >= -110 & lon <=-100 ~ 9))
grid_need <- biomass_coords %>% ungroup() %>% count(grid)

cover_df <- c()
for(i in 1:length(grid_need$grid)){
  
  biomass_grid <- biomass_coords %>% filter(grid ==grid_need$grid[i])

  sites <- biomass_grid %>% ungroup() %>% rename(Latitude = lat, Longitude = lon)
  num_sites <- dim(sites)[1]
  coordinates(sites)  <-  c("Longitude",  "Latitude")
  proj4string(sites)  <- CRS("+proj=longlat +datum=WGS84 +no_defs") # this is what google is
  
  grid_id <- corners %>% filter(grid == grid_need$grid[i]) 
  
  cover <- raster(grid_id$cover_file)
  loss <- raster(grid_id$loss_file)
  gain <- raster(grid_id$gain_file)
  
  cover_values <- raster::extract(cover,sites)
  loss_values <- raster::extract(loss,sites)
  gain_values <- raster::extract(gain,sites)
  
  biomass_grid$cover <- cover_values
  biomass_grid$loss <- loss_values
  biomass_grid$gain <- gain_values
  
  cover_df <- rbind(cover_df,biomass_grid)
  
}

biomass_small2 <- merge(biomass_small,cover_df)

biomass_small2 %>% filter(loss == 0) %>% ggplot()+ aes(x = lon, y = lat, color = biomass) + geom_point() + theme_classic() +
  scale_color_viridis_c(option = 'turbo')

biomass_small2 <- biomass_small2 %>% mutate(biomass2 = ifelse(cover <= 25,NA,biomass)) %>% 
  mutate(biomass2 = ifelse(loss >0 & loss <= 11,NA,biomass2))

summary_biomass <- biomass_small2 %>% group_by(df_id,lat_site,lon_site) %>% 
  summarize(mean_biomass = mean(biomass2,na.rm=TRUE),
            max_biomass = max(biomass2,na.rm=TRUE),
            mean_cover = mean(cover))

summary_biomass %>% drop_na(mean_biomass) %>% 
  ggplot() + aes(x = lon_site, y = lat_site,color = mean_biomass) + geom_point() + theme_classic(base_size = 15) +
  scale_color_viridis_c(option = 'turbo')

summary_biomass <- summary_biomass %>% mutate(mean_biomass = ifelse(is.na(mean_biomass),0,mean_biomass),
                           max_biomass = ifelse(is.na(max_biomass),0,max_biomass))

biomass_ca <- merge(ll_info_ca,summary_biomass)

##############
##############
##############

biomass_ca <- biomass_ca %>% select(manual_id,lat,lon,mean_biomass,max_biomass) %>% mutate(mean_biomass = mean_biomass*0.907185,
                                                                                           max_biomass = max_biomass*0.907185)
biomass_us <- biomass_us %>% select(manual_id,lat,lon,mean_biomass,max_biomass)

in_both <- biomass_ca$manual_id[biomass_ca$manual_id %in% biomass_us$manual_id]

biomass_us <- biomass_us %>% filter(manual_id %ni% in_both)

biomass_all <- rbind(biomass_ca,biomass_us)

biomass_all <- biomass_all %>% mutate(max_biomass = ifelse(is.infinite(max_biomass),0,max_biomass))

biomass_all %>% ggplot() + aes(x = lon,y = lat, color = mean_biomass) + geom_point() + 
  theme_classic() +
  scale_color_viridis_c(option = 'turbo')

write_csv(biomass_all,"landscape/data/biomass_all_sites_example.csv")
