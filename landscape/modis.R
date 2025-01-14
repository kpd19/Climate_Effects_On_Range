library(sp)
library(raster)
library(tidyverse)

`%ni%` <- Negate(`%in%`)

latlong <- read_csv("data/range_locations.csv")

modis <- raster("data/NA_NALCMS_landcover_2010v2_250m.tif")
modis_ids_vals <- read_csv("data/modis_ids.csv")

##############
##############
##############
##############

ll_info_modis <- latlong %>% dplyr::select(manual_id,lat,lon)
ll_info_modis$df_id <- 1:length(ll_info_modis$manual_id)

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

latlong$modis_id <- unlist(modis_id)

latlong <- merge(latlong,modis_ids_vals)

latlong %>% #filter(type == 'site') %>%
  ggplot() + aes(x = lon,y = lat, color = classification) + geom_point() + theme_classic(base_size = 15) + 
  facet_wrap(~classification)

non_habitat <- latlong %>% filter(source != 'Synthetic data',
                                  classification %in% c("Water",'Urban and Built-up','Cropland','Barren Lands','Wetland','No data'))

sites_keep <- sites_modis_transformed[sites_modis_transformed$manual_id %in% non_habitat$manual_id,]

keep_modis <- raster::extract(modis,sites_keep, buffer = 2000)

keep_modis_df <- c()
for(i in 1:length(sites_keep$manual_id)){
  
  cover = keep_modis[[i]]
  
  if (length(cover>0)){
    temp <- data.frame(manual_id = sites_keep$manual_id[i], modis_id = cover)
  } else{
    temp <- data.frame(manual_id = sites_keep$manual_id[i], modis_id = NA)
  }
  
  keep_modis_df <- rbind(keep_modis_df,temp)
}

modis_updates <- keep_modis_df %>% mutate(modis_id =ifelse(modis_id ==0,NA,modis_id)) %>% 
  group_by(manual_id) %>% summarize(modis_id = min(modis_id,na.rm=TRUE))

modis_updates <- merge(modis_updates,modis_ids_vals)

modis_updates %>% count(classification)

latlong1 <- latlong %>% filter(manual_id %ni% sites_keep$manual_id)

modis_fix <- latlong %>% filter(manual_id %in% sites_keep$manual_id) %>% select(-modis_id) %>% select(-classification)
modis_fix <- merge(modis_fix,modis_updates)

latlong1 <- rbind(latlong1,modis_fix)

latlong1 <- latlong1 %>% mutate(drop = ifelse(manual_id %in% non_habitat_drop$manual_id,TRUE,FALSE))
latlong1 %>% filter(source != 'Synthetic data') %>%
  ggplot() + aes(x = lon, y = lat, color = classification)+ geom_point() + theme_classic() 

write_csv(latlong1, "data/modis_classifications_example.csv")

a.time <- Sys.time()
modis_id = raster::extract(modis,sites_modis_transformed, buffer = 5000)
b.time <- Sys.time()
b.time - a.time

modis_id_long <- modis_id %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
modis_id_long <- modis_id_long %>% rename(df_id = name,modis_id = value)

modis_id_long <- merge(modis_id_long,modis_ids_vals)

modis_id_long <- merge(modis_id_long,ll_info_modis)

head(modis_id_long)

modis_summary <- modis_id_long %>% mutate(n = 1) %>% 
  group_by(lat,lon,manual_id) %>% mutate(total = sum(n)) %>% 
  mutate(n = 1) %>% group_by(lat,lon,manual_id,total,classification) %>%
  summarize(sum_n = sum(n))

modis_summary <- modis_summary %>% mutate(p = sum_n/total)

modis_summary_wide <- modis_summary %>% select(lat,lon,manual_id,classification,p) %>% pivot_wider(names_from = classification,values_from = p)

modis_summary_wide <- modis_summary_wide %>% mutate(near_needle = ifelse(is.na(`Temperate or sub-polar needleleaf forest`) == TRUE, FALSE,TRUE))

test <- merge(modis_summary_wide,latlong1)

test_off <- test %>% filter(is.na(`Temperate or sub-polar needleleaf forest`),source != "Synthetic data") %>% select(lat,lon,manual_id,near_needle,classification)

write_csv(test_off, "data/modis_problems_5k_example.csv")

# there are no issues in the sample file

if (length(test_off$lat) >0){
  test_off2 <- read_csv("data/modis_problems_5k_example_edit.csv")
  
  trap_fine <- test_off2 %>% filter(keep == TRUE) %>% pull(manual_id)
  
  modis_summary_wide <- modis_summary_wide %>% mutate(near_needle = ifelse(manual_id %in% trap_fine,1,near_needle))
}

modis_summary_wide %>% drop_na(`Temperate or sub-polar needleleaf forest`) %>%
  ggplot() + aes(x = lon,y = lat, color = `Temperate or sub-polar needleleaf forest`) +
  geom_point() + theme_classic(base_size = 15) +
  scale_color_viridis_c()

write_csv(modis_summary_wide,'data/modis_5k_nearneedle_example.csv') 

