library(sf)
library(tidyverse)

`%ni%` <- Negate(`%in%`)

latlong <- read_csv("_gendata/all_trap_max_edits.csv")

bc_watershed <- read_csv("/Volumes/My Book/Synchrony/_trap/bc_trap_complete.csv")

bc_watershed_loc <- bc_watershed %>% mutate(manual_id = case_when(region == "Cariboo" ~ Site_no + 3000,
                                                                  region == "Thompson Oakanogan" ~ Site_no + 4000,
                                                                  region == "Kootenay" ~ Site_no + 5000)) %>% 
  mutate(n = 1) %>% drop_na(trap_mean) %>% 
  group_by(manual_id,Location,lat,lon,region) %>% summarize(years_trapped = sum(n)) %>% 
  mutate(id = case_when(region == "Cariboo" ~ 1,
                        region == "Thompson Oakanogan" ~ 2,
                        region == "Kootenay" ~ 3)) %>% 
  mutate(huc8 = case_when(region == "Cariboo" ~ 2300,
                          region == "Thompson Oakanogan" ~ 2301,
                          region == "Kootenay" ~ 2302)) %>% 
  rename(PlotName = Location, name = region)

huc8 <- st_read("/Volumes/My Book/Synchrony/_watershed/wbdhu8_a_us_september2023.gdb")

huc8_info <- data.frame(name = huc8$name,huc8 = as.numeric(huc8$huc8), id = 1:length(huc8$huc8))
huc8_info %>% arrange(huc8)

sites <- latlong %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-st_as_sf(sites, crs = st_crs(huc8))

huc8_coords <- st_intersects(sites_transformed, huc8)

temp <- c()
for(j in 1:length(sites)){
  if (length(huc8_coords[[j]]) > 0) {
    temp[j] <- huc8_coords[[j]]
  } else{
    temp[j] <- 0
  }
}

which_huc8 <- data.frame(manual_id = latlong$manual_id, id = temp)

which_huc8 <- merge(which_huc8,huc8_info)

latlong_huc8 <- merge(which_huc8, latlong, all = TRUE)

unique(latlong_huc8$name)

latlong_huc8 %>% filter(State %in% c('WA'), lon <= -111) %>% 
  ggplot() + aes(x = lon, y = lat, color = name) + geom_point() + theme_classic(base_size = 15)

successful_ids <- latlong_huc8 %>% drop_na(huc8) %>% pull(manual_id)

bc_missing <- latlong_huc8 %>% filter(manual_id %ni% successful_ids) %>% select(-huc8) %>%
  select(-name) %>% select(-years_trapped) %>% select(-id)

bc_missing <- merge(bc_missing,bc_watershed_loc, all = TRUE)
bc_missing$State <- "BC"

latlong_huc8 <- rbind(latlong_huc8[latlong_huc8$State != "BC",],bc_missing)

huc8_info <- latlong_huc8 %>% select(manual_id,lat,lon,name,huc8,PlotName,State) %>% drop_na(manual_id) %>% drop_na(name)

huc8_info %>% count(manual_id) %>% arrange(desc(n)) %>% filter(n>1)

write_csv(huc8_info, "/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc8.csv")

