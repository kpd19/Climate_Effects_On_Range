library(sf)
library(sp)
library(tidyverse)
library(geodata)
library(dismo)
library(deldir)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/population_data/")

sf_use_s2(FALSE)

`%ni%` <- Negate(`%in%`)

st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}

canada_data = gadm("GADM", country="CAN", level = 1)
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
#bc_data = fortify(bc_data)

us_data = gadm("GADM", country="USA", level = 1)
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
#state_data = fortify(state_data)

dftm_all2 <- st_read('data/defoliation_all_1947-2024.shp')

split_poly <- function(sf_poly, n_areas) {
  # Create random points
  points_rnd <- st_sample(sf_poly, size = 10000)
  # k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # Create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # Clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area_km <- st_area(equal_areas)
  return(equal_areas)
}

yrs <- sort(unique(dftm_all2$year))

dftm_summary <- dftm_all2 %>% mutate(area_meters = as.vector(st_area(geometry))) %>% 
  mutate(area_km = area_meters/(1000^2))

dftm_summary %>% filter(year >= 1985) %>% mutate(n = 1) %>%
  filter(area_km >0) %>%  summarize(min_km = min(area_km),
                                    max_km = max(area_km),
                                    min_m = min(area_meters),
                                    max_m = max(area_meters),
                                    sum_n = sum(n))

dftm_merged <- dftm_all2 %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup() %>% 
  st_cast("POLYGON") %>% mutate(area_meters = as.vector(st_area(geometry))) %>% 
  mutate(area_km = area_meters/(1000^2))

dftm_merged2 <- c()
for(i in 1:length(yrs)){
  temp_merged <- dftm_all2 %>% filter(year == yrs[i]) %>%
    summarize(geometry = st_union(geometry)) %>% ungroup() %>% 
    st_cast("POLYGON") %>% mutate(area_meters = as.vector(st_area(geometry))) %>% 
    mutate(area_km = area_meters/(1000^2)) %>% mutate(year = yrs[i])
  dftm_merged2 <- rbind(dftm_merged2,temp_merged)
}

dftm_merged_large <- dftm_merged2 %>% filter(area_km >= 9) %>% 
  mutate(num = round(area_km/3), type = 'to break')

dftm_merged_small <- dftm_merged2 %>% filter(area_km < 9) %>% 
  mutate(num = NA, type = 'not to break')

max(dftm_merged_large$area_km)

dftm_split_large <- c()
for(i in 1:length(dftm_merged_large$geometry)){
  temp <- split_poly(dftm_merged_large[i,], dftm_merged_large[i,]$num)
  temp <- temp %>%
    mutate(area_km = area_km/(1000^2),
           type = 'broken') %>% dplyr::select(-id) %>% 
    mutate(year = dftm_merged_large[i,]$year)
  
  dftm_split_large <- rbind(dftm_split_large,temp)
  
}

dftm_large <- rbind(dftm_split_large,dftm_merged_large)
dftm_large$area_km <- as.vector(dftm_large$area_km)

dftm_large %>% filter(type %in% c("broken",'to break')) %>% ggplot() +
  geom_sf(color = 'black', aes(fill = area_km),alpha = 0.75) +
  theme_classic(base_size = 10) + 
  scale_fill_scico(palette = "bamako") +
  facet_wrap(~type) +
  coord_sf(xlim = c(-120,-116),ylim = c(45,47))

dftm_merged_split <- rbind(dftm_split_large,dftm_merged_small)

dftm_merged_split <- dftm_merged_split %>% 
  mutate(lon_cent = map_dbl(geometry, ~st_centroid_within_poly(.x)[[1]]),
         lat_cent = map_dbl(geometry, ~st_centroid_within_poly(.x)[[2]])) %>% 
  mutate(area_km = as.vector(area_km))

st_write(dftm_merged_split,'data/defoliation_merged_split_1947-2024.shp', append = FALSE)

dftm_merged_split %>% ggplot() + aes(x = lon_cent, y = lat_cent, color = area_km) + 
  geom_point() + theme_classic(base_size = 15)

dftm_no_geo <- dftm_merged_split %>% st_drop_geometry()

dftm_no_geo$manual_id <- 1:10234

write_csv(dftm_no_geo,'data/defoliation_split_1947-2024.csv')

dftm_no_geo %>% filter(year >= 1985) %>% mutate(n = 1) %>% 
  summarize(min_km = min(area_km),
            max_km = max(area_km),
            sum_n = sum(n))
