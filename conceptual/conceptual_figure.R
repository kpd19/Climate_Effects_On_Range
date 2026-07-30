library(tidyverse)
library(scico)
library(sf)
library(ggmap)
library(metR)
library(ggforce)
library(gridExtra)

`%ni%` <- Negate(`%in%`)

api_key <- "your_API_here"
register_google(api_key)

clls <- read_csv('data/concept_lls2.csv')
elevs <- read_csv('data/concept_elevs.csv')

var_names <- read_csv("../range_modeling/data/var_names_pa2.csv")
ann_weather_stats <- read_csv("../range_modeling/data/avg5_weather_1940-2025.csv")
forest_comp <- read_csv('data/concept_lls2_forestcomp.csv')
near_needle <- read_csv('data/concept_lls2_modis_5k_nearneedle.csv',guess_max = Inf)
modis <- read_csv('data/concept_lls2_modis_class.csv')
biomass <- read_csv('data/concept_lls2_biomass.csv')

dftm_all2 <- st_read('../population_data/data/defoliation_all_1947-2024.shp')
latlong <- read_csv("../population_data/data/population_records_1947-2025.csv")
synthetic <- read_csv("../population_data/data/synthetic_data_habitat.csv")

synthetic <- synthetic %>% dplyr::select(lat,lon,manual_id,source)

aspect <- read_csv('/Volumes/My Book/QGIS/elevation/concept_aspect2.csv')

#rf_all <- read_csv("/Volumes/My Book/Synchrony/presence/_rfmod2/predicted_presence_all_lags_pref4.csv")

forest_comp <- forest_comp %>% mutate_at(colnames(forest_comp)[c(4:39)], ~replace_na(.,0))
near_needle <- near_needle %>% mutate_at(colnames(near_needle)[c(3:18)], ~replace_na(.,0))

clls <- clls %>% mutate(lat_coord = round(lat/0.25)*0.25,lon_coord = round(lon/0.25)*0.25)

weather <- merge(clls,ann_weather_stats, by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'))

width_p <- 1.4
height_p = 0.5

leg_pos <- 'none'

needle_plt <- near_needle %>% rename(needle = `Temperate or sub-polar needleleaf forest`) %>% 
  ggplot() + aes(x = lon, y = lat, color = needle, fill = needle) + geom_tile() +
  scale_color_scico("", palette = 'davos', direction = -1) +
  scale_fill_scico("", palette = 'davos', direction = -1) +
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

shrub_plt <- near_needle  %>% 
  pivot_longer(cols = colnames(near_needle)[3:17]) %>%
  filter(name == 'Temperate or sub-polar shrubland') %>% 
  ggplot() + aes(x = lon, y = lat, color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'davos', direction = -1) +
  scale_fill_scico("", palette = 'davos', direction = -1) + 
  theme_classic() +
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

max_t2m_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = max_t2m, fill = max_t2m) + geom_tile() +
  scale_color_scico("", palette = 'romaO', direction = -1) +
  scale_fill_scico("", palette = 'romaO', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

min_t2m_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = min_t2m, fill = min_t2m) + geom_tile() +
  scale_color_scico("", palette = 'romaO', direction = -1) +
  scale_fill_scico("", palette = 'romaO', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

min_rh_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = min_rh, fill = min_rh) + geom_tile() +
  scale_color_scico("", palette = 'lapaz', direction = -1) +
  scale_fill_scico("", palette = 'lapaz', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

max_rh_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = max_rh, fill = max_rh) + geom_tile() +
  scale_color_scico("", palette = 'davos', direction = -1) +
  scale_fill_scico("", palette = 'davos', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

sum_tp_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = sum_tp, fill = sum_tp) + geom_tile() +
  scale_color_scico("", palette = 'devon', direction = -1) +
  scale_fill_scico("", palette = 'devon', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position =leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm')) 

coldest_plt <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = coldest, fill = coldest) + geom_tile() +
  scale_color_scico("", palette = 'devon', direction = 1) +
  scale_fill_scico("", palette = 'devon', direction = 1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm')) 

gdd_season <- weather %>% filter(year == 2024) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = gdd_season, fill = gdd_season) + geom_tile() +
  scale_color_scico("", palette = 'batlowK', direction = 1) +
  scale_fill_scico("", palette = 'batlowK', direction = 1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

colSums(forest_comp)

abies_plt <- forest_comp %>% 
  mutate(Abies = ifelse(`Subalpine fir` + `Balsam fir` + `Amabilis fir`>0,1,0)) %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(Abies), fill = as.factor(Abies)) +
  geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = 'forestgreen')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = 'forestgreen')) +
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

df_plt <- forest_comp %>% 
  mutate(df = ifelse(`Douglas-fir` >0,1,0)) %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(df), fill =  as.factor(df)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = '#004D40')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = '#004D40')) +
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos)  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p, 'cm'))

elev_plt <- elevs %>% mutate(elev2 = ifelse(is.na(elev2),0,elev2)) %>% 
  ggplot() + aes(x = lon, y = lat, color = elev2, fill = elev2) + geom_tile() +
  scale_color_scico("", palette = 'grayC', direction = 1) +
  scale_fill_scico("", palette = 'grayC', direction = 1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1)) +
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

biomass_plt <- biomass %>% rename(lat = lat_site, lon = lon_site) %>% filter(lat > 49) %>%  #mutate(elev2 = ifelse(is.na(elev2),0,elev2)) %>% 
  ggplot() + aes(x = lon, y = lat,  fill = mean_biomass) + geom_tile(color = NA) +
  scale_color_scico("", palette = 'navia', direction = -1) +
  scale_fill_scico("", palette = 'navia', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1)) +
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

cover_plt <- biomass %>% rename(lat = lat_site, lon = lon_site) %>% filter(lat > 49) %>%  #mutate(elev2 = ifelse(is.na(elev2),0,elev2)) %>% 
  ggplot() + aes(x = lon, y = lat, fill = mean_cover) + geom_tile(color = NA) +
  #scale_color_scico("", palette = 'bamako', direction = -1) +
  scale_fill_scico("", palette = 'bamako', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = leg_pos) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1)) +
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

aspect_plt <- aspect %>% rename(apsect2 = Aspect) %>% mutate(aspect3 = case_when(aspect2 >=0 & aspect2 <= 22.5 ~ "North",
                                                    aspect2 >22.5 & aspect2 <= 67.5 ~ "Northeast",
                                                    aspect2 >67.5 & aspect2 <= 112.5 ~ "East",
                                                    aspect2 >112.5 & aspect2 <= 157.5 ~ "Southeast",
                                                    aspect2 >157.5 & aspect2 <= 202.5 ~ "South",
                                                    aspect2 >202.5 & aspect2 <= 247.5 ~ "Southwest",
                                                    aspect2 >247.5 & aspect2 <= 292.5 ~ "West",
                                                    aspect2 >292.5 & aspect2 <= 337.5 ~ "Northwest",
                                                    aspect2 >337.5 & aspect2 <= 360 ~ "North",
                                                    aspect2 == -1 ~ "Flat")) %>% 
  ggplot() + aes(x = lon, y = lat, fill = aspect3) + geom_tile() + 
  theme_classic() + 
  scale_fill_manual("", values = c("North" = 'red', 'Northeast' = 'Orange', 'East' = 'Yellow',
                                   'Southeast' = 'green', 'South' = 'cyan', 'Southwest' = 'steelblue2',
                                   'West' = 'Blue', 'Northwest' = 'violet', 
                                   "Flat" = 'black')) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3))

aspect_plt <- aspect %>%
  ggplot() + aes(x = X, y = Y, fill = Aspect) + geom_tile() + 
  theme_classic() + 
  scale_fill_gradient2("",high = 'orange', mid = 'blue', low = 'orange', midpoint = 180) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3))

defo_years <- dftm_all2 %>% filter(lat_cent >= 49) %>% pull(year) 
st_crs(dftm_all2) <- NA

max(defo_years)

defo_plt <- dftm_all2 %>% filter(lat_cent >= 48,lon_cent >=-124) %>% ggplot() +
  geom_sf(aes(geometry = geometry), color = 'red4', fill = 'red4', size = 10) + 
  coord_sf(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3), expand = FALSE) +
  theme_bw(base_size = 10) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.position = 'none',
        aspect.ratio = 1)

min(dftm_all2$year)

ll_small <- latlong %>% filter(source != 'Defoliation surveys', lat >= 48)

trap_pts <- ll_small %>% ggplot() +
  geom_point(aes(x = lon, y = lat), shape = 2, size = 1.75, color = 'black') +
  theme_bw(base_size = 10) + 
  #stat_ellipse(aes(x = lon, y = lat, group = clust), linetype = 'dashed', type = 'norm', size = 0.5) +
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.position = 'none') #,
        #legend.position.inside = c(0.1,0.9),
        #legend.title = element_blank()) 

synth_pts <- synthetic %>% ggplot() +
  geom_point(aes(x = lon, y = lat), shape = 1, size = 1.75, color = 'blue4') +
  theme_bw(base_size = 10) + 
  #stat_ellipse(aes(x = lon, y = lat, group = clust), linetype = 'dashed', type = 'norm', size = 0.5) +
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.position = 'none') 

grid.arrange(defo_plt,trap_pts, synth_pts,nrow = 1)

loc_sat <- get_map(location = c(lon= -120.5,lat = 50.5), maptype = 'satellite',
                   source = 'google', api_key = api_key, zoom = 7)


sat_map <- ggmap(loc_sat) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme_classic() + xlab("Latitude") + ylab("Longitude")

pdf("figures/concept_graphs1.pdf",height = 12, width = 14)
grid.arrange(sat_map,defo_plt,trap_pts, synth_pts,
             df_plt,abies_plt,cover_plt,biomass_plt,
             max_t2m_plt,sum_tp_plt,max_rh_plt,gdd_season,
             ncol = 4,
             heights = c(0.85,1,1))
dev.off()

pdf("figures/concept_graphs2.pdf",height = 10, width = 14)
grid.arrange(sat_map,defo_plt,trap_pts, synth_pts,
             df_plt,abies_plt,cover_plt,biomass_plt,
             max_t2m_plt,sum_tp_plt,max_rh_plt,gdd_season,
             ncol = 4,
             heights = c(1,1,1))
dev.off()

leg1 <- data.frame(x = c(1,2), y = c(1,2), df = c(0,1)) %>% 
  ggplot() + aes(x = x, y = y, color = as.factor(df), fill =  as.factor(df)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = '#004D40')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = '#004D40')) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg2 <- data.frame(x = c(1,2), y = c(1,2), df = c(0,1)) %>% 
  ggplot() + aes(x = x, y = y, color = as.factor(df), fill =  as.factor(df)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = 'forestgreen')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = 'forestgreen')) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg3 <- biomass %>% summarize(min = min(mean_cover), max = max(mean_cover)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'bamako', direction = -1) +
  scale_fill_scico("", palette = 'bamako', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg4 <- biomass %>% summarize(min = min(mean_biomass), max = max(mean_biomass)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'navia', direction = -1) +
  scale_fill_scico("", palette = 'navia', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg5 <- weather %>% filter(year == 2024) %>% ungroup() %>% summarize(min = min(max_t2m), max = max(max_t2m)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'romaO', direction = -1) +
  scale_fill_scico("", palette = 'romaO', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg6 <- weather %>% filter(year == 2024) %>% ungroup() %>% summarize(min = min(sum_tp), max = max(sum_tp)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'devon', direction = -1) +
  scale_fill_scico("", palette = 'devon', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg7 <- weather %>% filter(year == 2024) %>% ungroup() %>% summarize(min = min(max_rh), max = max(max_rh)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'davos', direction = -1) +
  scale_fill_scico("", palette = 'davos', direction = -1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm'))

leg8 <- weather %>% filter(year == 2024) %>% ungroup() %>% summarize(min = min(gdd_season), max = max(gdd_season)) %>% 
  pivot_longer(cols = c('min','max')) %>% 
  ggplot() + aes(x = c(1,2), y = c(1,2), color = value, fill = value) + geom_tile() +
  scale_color_scico("", palette = 'batlowK', direction = 1) +
  scale_fill_scico("", palette = 'batlowK', direction = 1)+
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom', axis.title = element_blank())  + 
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p,'cm')) 


pdf("figures/legend_middle.pdf",height = 3,width = 14)
grid.arrange(leg1,leg2,leg3,leg4,nrow = 1)
dev.off()

pdf("figures/legend_bottom.pdf",height = 3,width = 14)
grid.arrange(leg5,leg6,leg7,leg8,nrow = 1)
dev.off()

forest_change <- read_csv('data/forest_change.csv')

forest_change %>% filter(tree_sp == 'Pseudotsuga menziesii', time_period == 2100) %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(present), fill =  as.factor(present)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'top')  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p, 'cm'))

forest_change %>% filter(time_period == 2100) %>% drop_na(tree_sp) %>% 
  filter(tree_sp %ni% c("Pseudotsuga menziesii",'Pseudotsuga macrocarpa')) %>% 
  group_by(lat,lon) %>% summarize(sum_p = sum(present)) %>% mutate(p = ifelse(sum_p >0,1,0)) %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(p), fill =  as.factor(p)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'top')  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p, 'cm')) 


forest_change %>% filter(tree_sp == 'Pseudotsuga menziesii', time_period == 2100) %>% 
  ggplot() + aes(x = lon, y = lat, color = as.factor(present), fill =  as.factor(present)) + geom_tile() +
  scale_color_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  scale_fill_manual("", values = c(`0` = 'beige', `1` = 'darkgreen')) +
  theme_classic(base_size = 10) +
  theme(legend.position = 'top')  + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme(legend.key.height = unit(height_p, 'cm'),
        legend.key.width = unit(width_p, 'cm'))
  

d1 <- forest_change %>% filter(tree_sp == 'Pseudotsuga menziesii', time_period == 2100) %>%
  filter(present == 1, time_period == 2100)
d2 <- forest_comp %>% 
  mutate(df = ifelse(`Douglas-fir` >0,1,0)) %>% filter(`Douglas-fir` == 1)

plt1 <- ggmap(loc_sat) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme_classic() + xlab("Latitude") + ylab("Longitude") + 
  geom_tile(data = d1, aes(x = lon, y = lat), fill = 'blue', color = 'blue', alpha = 0.5)
plt2 <- ggmap(loc_sat) + 
  coord_cartesian(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  scale_x_longitude(breaks = seq(-123,-118,1)) +
  scale_y_latitude(breaks = seq(49,52,1))+
  theme_classic() + xlab("Latitude") + ylab("Longitude") + 
  geom_tile(data = d2, aes(x = lon, y = lat), fill = 'blue', color = 'blue', alpha = 0.5)
grid.arrange(plt1,plt2)
