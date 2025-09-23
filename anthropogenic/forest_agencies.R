library(tidyverse)
library(geosphere)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/anthropogenic/")

canada_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/Synchrony/spatial/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))

all_geo <- rbind(canada_data,us_data)

`%ni%` <- Negate(`%in%`)

canada <- read_csv("data/Canadian_Forestry.csv")
nps <- read_csv("data/Western_USA_NPS.csv")
usa_state <- read_csv("data/USA_State_Forestry.csv")
usfs <- read_csv("data/Forest_Service_Office_Locations.csv")
caps <- read_csv("data/Canadian_NPS.csv")

canada <- canada %>% select(agency, state,name,address,latitude,longitude,level) %>% mutate(country = "Canada")
nps <- nps %>% rename(name = agency) %>% mutate(agency = "National Park System") %>%
  select(agency, address,name,state,level,latitude,longitude) %>% mutate(country = "United States of America")
usa_state <- usa_state %>% select(agency,name,state,address,latitude,longitude,level) %>%
  mutate(country = "United States of America")
usfs <- usfs %>% filter(STATE %in% c('WA',"OR","ID","MT",'ND',"SD",'CO','CA','UT',"NV",'AZ','NM','TX','OK','WY',"KS",'NE')) %>%
  drop_na(REGION) %>% filter(REGION != "Washington Office") %>% rename(name = NAME, latitude = Y, longitude = X, state2 = STATE) %>% 
  mutate(agency = "USFS", level = 'federal',address = paste0(STREET,',', CITY,',', state2," ", ZIP_CODE)) %>% 
  select(agency,name,state2,address,latitude,longitude,level) %>% 
  mutate(country = "United States of America")
caps <- caps %>% mutate(country = "Canada") %>% mutate(agency = "Canadian National Park System")

states <- data.frame(state2 = c('WA',"OR","ID","MT",'ND',"SD",'CO','CA','UT',"NV",'AZ','NM','TX','OK','WY',"KS",'NE'),
                state = c("Washington",'Oregon','Idaho',"Montana",'North Dakota','South Dakota','Colorado','California',
                'Utah','Nevada',"Arizona",'New Mexico','Texas',"Oklahoma",'Wyoming','Kansas',"Nebraska"))


usfs <- merge(usfs,states)
usfs <- usfs %>% select(-state2)

forest_agencies <- rbind(canada,nps,usa_state,usfs,caps)

forest_agencies %>% 
  ggplot() + aes(x = longitude, y = latitude, color = state) + geom_point() + 
  theme_classic() +
  facet_wrap(~level)

forest_agencies %>% filter(is.na(latitude))

write_csv(forest_agencies,'data/forest_agencies.csv')

latlong <- read_csv("data/state_info.csv")

ll_canada <- latlong %>% filter(country == "Canada")
ll_usa <- latlong %>% filter(country == "United States of America")
ll_ocean <- latlong %>% filter(country == "Ocean")

agency_canada <- forest_agencies %>% filter(country == "Canada")
agency_usa <- forest_agencies %>% filter(country == "United States of America")

distance_matrix <- distm(cbind(ll_canada$lon, ll_canada$lat), cbind(agency_canada$longitude, agency_canada$latitude))

ll_canada_dist <- c()
for(i in 1:length(ll_canada$lat)){

  temp <- agency_canada %>% mutate(dist_km = distance_matrix[i,]/1000,
                                   ll_id = i)
  
  ll_canada_dist <- rbind(ll_canada_dist,temp)
}

ll_canada_dist <- ll_canada_dist %>% rename(agency_state = state)

ll_canada$ll_id <- 1:length(ll_canada$lat)

ll_canada_distances <- merge(ll_canada,ll_canada_dist)

write_csv(ll_canada_distances,'data/ll_canada_distances_1.csv')


#################
#################
#################
#################
#################

ll_can_dist_fed <- ll_canada_distances %>% filter(level == 'federal') %>% group_by(lat,lon,manual_id) %>% filter(dist_km == min(dist_km))

write_csv(ll_can_dist_fed,'data/ll_can_distances_fed.csv')

pdf("figures/can_fed_dist.pdf",height = 2, width = 6)
ll_can_dist_fed %>% 
  ggplot() + geom_point(aes(x = lon,y = lat, color = dist_km)) +
  theme_classic() +
  scale_color_viridis_c("Distance (km)", option = 'turbo') +
  xlab("Longitude") + ylab("Latitude") +
  geom_sf(data = canada_data, aes(geometry =geometry), fill = NA, color = 'grey85', size = 1.2) +
  coord_sf(ylim = c(49,54), xlim = c(-128,-104))
dev.off()  

#################
#################
#################
#################
#################


ll_can_dist_state <- ll_canada_distances %>% filter(level == 'state') %>% group_by(lat,lon,manual_id) %>% filter(dist_km == min(dist_km))

write_csv(ll_can_dist_state,'data/ll_can_distances_state.csv')

pdf("figures/can_state_dist.pdf",height = 2, width = 6)
ll_can_dist_state %>% 
  ggplot() + geom_point(aes(x = lon,y = lat, color = dist_km)) +
  theme_classic() +
  scale_color_viridis_c("Distance (km)", option = 'turbo') +
  xlab("Longitude") + ylab("Latitude") +
  geom_sf(data = canada_data, aes(geometry =geometry), fill = NA, color = 'grey85', size = 1.2) +
  coord_sf(ylim = c(49,54), xlim = c(-128,-104))
dev.off()  

#################
#################
#################
#################
#################


agency_usa_fed <- agency_usa %>% filter(level == 'federal') %>% rename(agency_state = state) %>% 
  select(-c(address,country,level))

distance_usa_fed <- distm(cbind(ll_usa$lon, ll_usa$lat), cbind(agency_usa_fed$longitude, agency_usa_fed$latitude))

dim(distance_usa_fed)

min_dist_ids <- apply(distance_usa_fed,1,which.min)

ll_usa_dist_fed <- c()
for(i in 1:length(ll_usa$lat)){
  
  temp <- ll_usa[i,]
  temp2 <- agency_usa_fed[min_dist_ids[i],]
  
  temp$dist_km = distance_usa_fed[i, min_dist_ids[i]]/1000
  
  temp <- cbind(temp,temp2)
  
  ll_usa_dist_fed <- rbind(ll_usa_dist_fed,temp)
  
  if(i %% 1000 == 0){
    print(paste0("Finished ", i))
  }
}


write_csv(ll_usa_dist_fed,'data/ll_usa_distances_federal.csv')

pdf("figures/us_fed_dist.pdf",height = 6, width = 6)
ll_usa_dist_fed %>% 
  ggplot() + geom_point(aes(x = lon,y = lat, color = dist_km)) +
  theme_classic() +
  scale_color_viridis_c("Distance (km)", option = 'turbo') +
  xlab("Longitude") + ylab("Latitude") +
  geom_sf(data = us_data, aes(geometry =geometry), fill = NA, color = 'grey85', size = 1.2) +
  coord_sf()
dev.off()  

###################
###################
###################

agency_usa_state <- agency_usa %>% filter(level == 'state') %>% rename(agency_state = state) %>% 
  select(-c(address,country,level)) 

uni_states <- unique(agency_usa_state$agency_state)

ll_usa_dist_state <- c()
for(s in 1:length(uni_states)){
  
  agency_state <- agency_usa_state %>% filter(agency_state == uni_states[s])
  ll_state <- ll_usa %>% filter(state == uni_states[s])
  
  distance_state <- distm(cbind(ll_state$lon, ll_state$lat), cbind(agency_state$longitude, agency_state$latitude))
  
  min_dist_state <- apply(distance_state,1,which.min)
  
  for(i in 1:length(ll_state$lat)){
    
    temp <- ll_state[i,]
    temp2 <- agency_state[min_dist_state[i],]
    
    temp$dist_km = distance_state[i, min_dist_state[i]]/1000
    
    temp <- cbind(temp,temp2)
    
    ll_usa_dist_state <- rbind(ll_usa_dist_state,temp)
    
  }
  print(paste0("Finishing ", uni_states[s]))
}

# missing texas, nebraska, north and south dakota
write_csv(ll_usa_dist_state,'data/ll_usa_distances_state.csv')

pdf("figures/us_state_dist.pdf",height = 6, width = 6)
ll_usa_dist_state %>% 
  ggplot() + geom_point(aes(x = lon,y = lat, color = dist_km)) +
  theme_classic() +
  scale_color_viridis_c("Distance (km)", option = 'turbo') +
  xlab("Longitude") + ylab("Latitude") +
  geom_sf(data = us_data, aes(geometry =geometry), fill = NA, color = 'grey85', size = 1.2) +
  coord_sf()
dev.off()  
