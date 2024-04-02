library(sf)
library(tidyverse)

`%ni%` <- Negate(`%in%`)

latlong <- read_csv("_gendata/all_trap_max_edits.csv")

unique(latlong$State)

bc_watershed <- read_csv("/Volumes/My Book/Synchrony/_trap/bc_trap_complete.csv")


bc_watershed_loc <- bc_watershed %>% mutate(manual_id = case_when(region == "Cariboo" ~ Site_no + 3000,
                                             region == "Thompson Oakanogan" ~ Site_no + 4000,
                                             region == "Kootenay" ~ Site_no + 5000)) %>% 
  mutate(n = 1) %>% drop_na(trap_mean) %>% 
  group_by(manual_id,Location,lat,lon,region) %>% summarize(years_trapped = sum(n)) %>% 
  mutate(id = case_when(region == "Cariboo" ~ 1,
                        region == "Thompson Oakanogan" ~ 2,
                        region == "Kootenay" ~ 3)) %>% 
  mutate(huc4 = case_when(region == "Cariboo" ~ 2300,
                        region == "Thompson Oakanogan" ~ 2301,
                        region == "Kootenay" ~ 2302)) %>% 
  rename(PlotName = Location, name = region)

huc4 <- st_read("/Volumes/My Book/Synchrony/_watershed/wbdhu4_a_us_september2023.gdb")

huc4_info <- data.frame(name = huc4$name,huc4 = as.numeric(huc4$huc4), id = 1:length(huc4$huc4))
huc4_info %>% arrange(huc4)

sites <- latlong %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-st_as_sf(sites, crs = st_crs(huc4))

huc4_coords <- st_intersects(sites_transformed, huc4)

temp <- c()
for(j in 1:length(sites)){
  if (length(huc4_coords[[j]]) > 0) {
    temp[j] <- huc4_coords[[j]]
  } else{
    temp[j] <- 0
  }
}
  
which_huc4 <- data.frame(manual_id = latlong$manual_id, id = temp)

which_huc4 <- merge(which_huc4,huc4_info)

latlong_huc4 <- merge(which_huc4, latlong, all = TRUE)

successful_ids <- latlong_huc4 %>% drop_na(huc4) %>% pull(manual_id)

bc_missing <- latlong_huc4 %>% filter(manual_id %ni% successful_ids) %>% select(-huc4) %>%
  select(-name) %>% select(-years_trapped) %>% select(-id)

bc_missing <- merge(bc_missing,bc_watershed_loc, all = TRUE)
bc_missing$State <- "BC"

latlong_huc4 <- rbind(latlong_huc4[latlong_huc4$State != "BC",],bc_missing)

huc4_info <- latlong_huc4 %>% select(manual_id,lat,lon,name,huc4,PlotName,State) %>% drop_na(manual_id) %>% 
  drop_na(name)

huc4_info %>% count(manual_id) %>% arrange(desc(n)) %>% filter(n>1)

latlong_huc4 %>%
  ggplot() + aes(x = lon, y = lat, color = name) + geom_point() + theme_classic(base_size = 15) + 
  geom_point(data = latlong, aes(x = lon, y = lat), color = 'black', size = 0.25)

write_csv(huc4_info, "/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc4.csv")

