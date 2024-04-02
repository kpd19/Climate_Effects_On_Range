library(sf)
library(tidyverse)

`%ni%` <- Negate(`%in%`)

latlong <- read_csv("_gendata/all_trap_max_edits.csv")

huc2 <- st_read("/Volumes/My Book/Synchrony/_watershed/wbdhu2_a_us_september2023.gdb")

sites <- latlong %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-st_as_sf(sites, crs = st_crs(huc2))

sort(unique(huc2$name))

latlong %>% ggplot() + aes(x = lon, y = lat) + geom_point() + theme_classic()

names <- c("Pacific Northwest Region",'Missouri Region','California Region',
           'Great Basin Region','Upper Colorado Region','Lower Colorado Region',
           "Arkansas-White-Red Region","Rio Grande Region")

length(names) == length(unique(names))

which_huc2 <- c()

sf_use_s2(FALSE)

for(i in 1:length(names)){
  small_region <- huc2 %>%
    filter(name %in% names[i])
  
  in_small <- st_within(sites_transformed,small_region)
  
  temp <- c()
  for(j in 1:length(sites)){
    if (length(in_small[[j]]) > 0) {
      temp[j] <- in_small[[j]]
    } else{
      temp[j] <- 0
    }
  }

  which_huc2 <- rbind(which_huc2, data.frame(manual_id = latlong$manual_id, in_huc = temp, region = names[i], huc2 = small_region$huc2))
  
}


latlong_huc2 <- merge(which_huc2, latlong, all = TRUE)
latlong_huc2 %>% filter(in_huc == 1) %>% 
  ggplot() + aes(x = lon, y = lat, color = region) + geom_point() + theme_classic(base_size = 15) + 
  geom_point(data = latlong, aes(x = lon, y = lat), color = 'black', size = 0.25)

successful_ids <- latlong_huc2 %>% filter(in_huc == 1) %>% pull(manual_id)

bc_missing <- latlong_huc2 %>% filter(manual_id %ni% successful_ids, region == "Pacific Northwest Region", State == "BC") %>% 
  mutate(in_huc = 1)

latlong_huc2 <- rbind(latlong_huc2,bc_missing)

huc2_info <- latlong_huc2 %>% filter(in_huc == 1) %>% select(manual_id,lat,lon,region,huc2,PlotName,State)

huc2_info %>% count(manual_id) %>% arrange(desc(n)) %>% filter(n>1)

write_csv(huc2_info, "/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc2.csv")

