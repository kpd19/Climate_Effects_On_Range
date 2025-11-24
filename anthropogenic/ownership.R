library(tidyverse)
library(raster)
library(sf)
library(geosphere)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/anthropogenic/")

canada_data <- st_read("/Volumes/My Book/gadm/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/gadm/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))

all_geo <- rbind(canada_data,us_data)

latlong <- read_csv("data/state_info.csv")

`%ni%` <- Negate(`%in%`)

forest_ownership <- raster("/Volumes/My Book/Forest/ForestOwnership/US_forest_ownership.tif")

ownership_df <- data.frame(number = 0:8, ownership = c('Unknown Forest', 'Non-Forest',
                                                       'Water', 'Family (Private)', 'Corporate/Other (Private)',
                                                       'Tribal','Federal (Public)',
                                                       'State (Public)', 'Local (Public)'))

ll_info <- latlong %>% filter(country == "United States of America") 
ll_info$df_id <- 1:length(ll_info$lat)

sites <- ll_info %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon) 
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed<-spTransform(sites, crs(forest_ownership))

plot(forest_ownership)
points(sites_transformed, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(forest_ownership,sites_transformed, buffer = 500, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_long <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('value'))

write_csv(sp_pop,'testing_sp_pop.csv')
sp_pop <- read_csv('testing_sp_pop.csv')
sp_pop1 <- sp_pop %>% head(10000)
write_csv(sp_pop1,'testing_sp_pop1_1k.csv')
sp_pop1 <- sp_pop1 %>% unnest(cols = c('value'))
sp_pop2 <- sp_pop[10001:20000,]
sp_pop2 <- sp_pop2 %>% unnest(cols = c('value'))
write_csv(sp_pop2,'testing_sp_pop2_1k.csv')
sp_pop3 <- sp_pop[20001:30000,]
sp_pop3 <- sp_pop3 %>% unnest(cols = c('value'))
write_csv(sp_pop3,'testing_sp_pop3_1k.csv')
sp_pop4 <- sp_pop[30001:37812,]
sp_pop4 <- sp_pop4 %>% unnest(cols = c('value'))
write_csv(sp_pop4,'testing_sp_pop4_1k.csv')

sp_pop_long <- sp_pop_long %>% rename(df_id = name,number = value)

sp_pop_sum <- sp_pop_long %>% group_by(df_id) %>% count(number)

sp_pop_sum <- merge(sp_pop_sum,ll_info,all = TRUE)

sp_pop_sum <- merge(sp_pop_sum,ownership_df)

head(sp_pop_sum)

sp_pop_sum <- sp_pop_sum %>% group_by(df_id) %>% mutate(tot = sum(n))

sp_pop_sum %>%
  ggplot() + aes(x = lon, y = lat, color = n/tot) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_viridis_c(option = "mako", direction = -1) +
  facet_wrap(~ownership)

sp_pop_sum %>% filter(ownership == "Water", n/tot == 1) %>%
  ggplot() + aes(x = lon, y = lat, color = as.factor(source)) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  #scale_color_viridis_c(option = "mako", direction = -1) +
  facet_wrap(~ownership)

write_csv(sp_pop_sum,'data/usa_forest_ownership_temp.csv')

max_pops <- sp_pop_sum %>% filter(ownership %ni% c('Non-Forest','Water')) %>%
  mutate(p = n/tot) %>% 
  group_by(df_id) %>% filter(p == max(p))

max_pops %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(source)) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  #scale_color_viridis_c(option = "mako", direction = -1) +
  facet_wrap(~ownership)

missing_ids <- sp_pop_sum$df_id[sp_pop_sum$df_id %ni% max_pops$df_id]

max_pops_non <- sp_pop_sum %>% filter(df_id %in% missing_ids) %>% 
  mutate(p = n/tot) %>% 
  group_by(df_id) %>% filter(p == max(p))

max_pops_us <- rbind(max_pops,max_pops_non)

max_pops_us %>% count(df_id) %>% filter(n>1)

max_pops_us1 <- max_pops_us %>% filter(df_id == 22818, ownership == "Local (Public)") 
max_pops_us2 <- max_pops_us %>% filter(df_id == 9147, ownership == "Corporate/Other (Private)") 

max_pops_us <- max_pops_us %>% filter(df_id %ni% c(22818, 9147))

max_pops_us <- rbind(max_pops_us,max_pops_us1,max_pops_us2)

write_csv(max_pops_us,'data/usa_forest_ownership.csv')

###################
###################
###################
###################
###################

can_ownership <- raster("/Volumes/My Book/Forest/Canada_MFv2020/Canada_MFv2020.tif")

ownership_ca <- data.frame(number = c(0, 100,20,40,32,31,33,50,13,12,11),
                           ownership = c('Non-Forest','Water','State (Public)','Tribal','Tribal',
                                         'Federal (Public)','State (Public)','Corporate/Other (Private)','Unknown Forest',
                                         'Federal (Public)','Federal (Public)'),
                           ownership2 = c('Non-Forest',"Water",'Protected','Treaty/Settlement','Tribal',
                                          'Federal Reserve', 'Restricted','Private',
                                          'Other','Short-term tenure','Long-term tenure'))

ownership_df <- data.frame(number = 0:8, ownership = c('Unknown Forest', 'Non-Forest',
                                                       'Water', 'Family (Private)', 'Corporate/Other (Private)',
                                                       'Tribal','Federal (Public)',
                                                       'State (Public)', 'Local (Public)'))

ll_info_ca <- latlong %>% filter(country == "Canada") 
ll_info_ca$df_id <- 1:length(ll_info_ca$lat)

sites <- ll_info_ca %>% ungroup() %>% dplyr::select(lat,lon) %>% rename(Latitude = lat, Longitude = lon)
num_sites <- dim(sites)[1]
coordinates(sites)  <-  c("Longitude",  "Latitude")
proj4string(sites)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # this is what google is
sites_transformed_ca <-spTransform(sites, crs(can_ownership))

plot(can_ownership)
points(sites_transformed_ca, pch = 16, col = 'black', cex = 0.1)

a.time <- Sys.time()
sp_pop = raster::extract(can_ownership,sites_transformed_ca, buffer = 1000, factors = TRUE)
b.time <- Sys.time()
b.time - a.time

sp_pop_ca <- sp_pop %>% set_names(seq_along(.)) %>% enframe %>% unnest(cols = c('name','value'))
sp_pop_ca <- sp_pop_ca %>% rename(df_id = name,number = value)

sp_pop_ca <- merge(sp_pop_ca,ll_info_ca,all = TRUE)

unique(sp_pop_ca$number)

sp_pop_ca <- merge(sp_pop_ca,ownership_ca)

sp_pop_ca %>% 
  ggplot() + aes(x = lon, y = lat, color = ownership) +
  geom_point(shape = 19, size = 1) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ownership)

write_csv(sp_pop_ca,'data/can_forest_ownership')


canada <- read_csv('data/can_forest_ownership')
usa <- read_csv('data/usa_forest_ownership')

canada <- canada %>% dplyr::select(-ownership2)

ownership <- rbind(canada,usa)

ownership %>% 
  ggplot() + aes(x = lon, y = lat, color = ownership) +
  geom_point(shape = 19, size = 0.5) + theme_classic() +
  #coord_cartesian(xlim = c(-121,-120.5),ylim = c(50.5,51)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ownership)


write_csv(ownership,'data/forest_ownership')
