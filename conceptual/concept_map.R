library(tidyverse)
library(gridExtra)
library(sf)

`%ni%` <- Negate(`%in%`)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

pheromone <- read_csv("../outbreak_size/data/north_america_trap_data_051225.csv")

population_records <- read_csv("../population_data/data/population_records_1947-2025.csv")
dist <- population_records %>% summarize(min_lat = min(lat), min_lon = min(lon),
                                 max_lat = max(lat), max_lon = max(lon))

dftm_all2 <- st_read('../population_data/data/defoliation_all_1947-2024.shp')
synthetic_data <- read_csv('../population_data/data/synthetic_data_habitat.csv')
synthetic_data <- synthetic_data %>% select(lat,lon,manual_id,source,elev2)

test_all <- read_csv('../range_modeling/data/test_all_1985-2025.csv')
cc_proj <- read_csv('../projection/data/populations_for_cc_proj_synthetic.csv')

state_info <- read_csv('../anthropogenic/data/state_info.csv')

test_all$lat <- round(test_all$lat,5)
test_all$lon <- round(test_all$lon,5)

state_info$lat <- round(state_info$lat,5)
state_info$lon <- round(state_info$lon,5)

state_info <- state_info %>% select(-manual_id)

test_all2 <- merge(test_all,state_info, all.x = TRUE)
test_all2 <- test_all2[!duplicated(test_all2),]

early_dataset_yrs <- test_all2 %>% filter(dataset == 'training',
                                          track_early %in% c("in",'near-1','near-2','near-3')) %>%
  drop_na(elev2)# %>% filter(country %in% c("United States of America", "Canada"))

late_dataset_yrs <- test_all2 %>% filter(dataset == 'testing') %>% drop_na(elev2) %>% 
  filter(country %in% c("United States of America", "Canada"))

late_dataset_yrs %>% filter(track_late %in% c("in",'near-1','near-2','near-3')) %>% 
  count(present)

early_dataset_yrs %>% filter(track_early %in% c("in",'near-1','near-2','near-3')) %>% 
  count(present)

test_all2 %>% filter(is.na(track_early)) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point()

dftm_all2 %>% ggplot() +  geom_sf(aes(geometry = geometry)) +
  coord_sf(ylim = c(48.8,52.2), xlim = c(-123.2,-118.3)) +
  theme_classic()

population_records %>% 
  filter(lat <= 52.2, lat >= 48.8,
         lon <= -118.3, lon >= -123.2) %>%
  filter(year >= 1985) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025')) %>%
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() + theme_classic() +
  facet_wrap(~period) +
  scale_color_brewer(palette = "Dark2")

col1 <- '#1E88E5'
col2 <- '#FFC107'
col3 <- '#004D40'
col4 <- "#C85409"
col5 <- '#61A6E2'
col6 <- '#6D8598'
col7 <- '#1E88E5'
col8 <- '#145B91'

df1 <- early_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  filter(year >= 1985, track_early %in% c("in",'near-1','near-2','near-3')) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% 
  group_by(lat,lon,present2) %>% count(lon)

plt1 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry)) +
  geom_point(data = df1, aes(x = lon,y = lat, color = present2),size = 0.1)+
  coord_sf(ylim = c(32,53.325), xlim = c(-128.125,-103.875)) +
  theme_classic() +
  scale_color_manual(values = c(col8,col2)) +
  theme(legend.position = 'none')+
  xlab("Longitude") + ylab("Latitude")

df2 <- late_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  #filter(year >= 1985, track_late %in% c("in",'near-1','near-2','near-3')) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% 
  group_by(lat,lon,present2) %>% count(lon)

plt2 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry)) +
  geom_point(data = df2, aes(x = lon,y = lat, color = present2),size = 0.1)+
  coord_sf(ylim = c(32,53.325), xlim = c(-128.125,-103.875)) +
  theme_classic() +
  scale_color_manual(values = c(col8,col2)) +
  theme(legend.position = 'none')+
  xlab("Longitude") + ylab("Latitude")
  
df3 <- late_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  filter(track_late %in% c("in",'near-1','near-2','near-3')) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% 
  group_by(lat,lon,present2) %>% count(lon)

plt3 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry)) +
  geom_point(data = df3, aes(x = lon,y = lat, color = present2),size = 0.1)+
  coord_sf(ylim = c(32,53.325), xlim = c(-128.125,-103.875)) +
  theme_classic() +
  scale_color_manual(values = c(col8,col2)) +
  theme(legend.position = 'none')+
  xlab("Longitude") + ylab("Latitude")

plt7 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry)) +
  geom_point(data = cc_proj, aes(x = lon,y = lat),size = 0.1, color = '#717171')+
  coord_sf(ylim = c(32,53.325), xlim = c(-128.125,-103.875)) +
  theme_classic() +
  theme(legend.position = 'none')+
  xlab("Longitude") + ylab("Latitude")

plt4 <- early_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  filter(year >= 1985, track_early %in% c("in",'near-1','near-2','near-3')) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% mutate(n = 1) %>% group_by(present2) %>% 
  mutate(tot = sum(n)) %>% 
  group_by(year,present2,tot) %>% summarize(sum_n = sum(n)) %>% 
  ggplot() + aes(x = year, y = sum_n/tot, fill = present2, group = present2) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  theme_classic()+
  scale_fill_manual(values = c(col8,col2)) +
  ylab("Proportion") + xlab("Year") +
  theme(legend.position = 'none')


plt5 <- late_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  filter(year >= 1985) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% mutate(n = 1) %>% group_by(present2) %>% 
  mutate(tot = sum(n)) %>% 
  group_by(year,present2,tot) %>% summarize(sum_n = sum(n)) %>% 
  ggplot() + aes(x = year, y = sum_n/tot, fill = present2, group = present2) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  theme_classic()+
  scale_fill_manual(values = c(col8,col2)) +
  ylab("Proportion") + xlab("Year")+
  theme(legend.position = 'none')

plt6 <- late_dataset_yrs %>% 
  # filter(lat <= 52.2, lat >= 48.8,
  #        lon <= -118.3, lon >= -123.2) %>%
  filter(track_late %in% c("in",'near-1','near-2','near-3')) %>% 
  mutate(period = ifelse(year <= 2010, '1985-2010', '2011-2025'),
         present2 = ifelse(present == 1, 'present','absent')) %>% mutate(n = 1) %>% group_by(present2) %>% 
  mutate(tot = sum(n)) %>% 
  group_by(year,present2,tot) %>% summarize(sum_n = sum(n)) %>% 
  ggplot() + aes(x = year, y = sum_n/tot, fill = present2, group = present2) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  theme_classic()+
  scale_fill_manual(values = c(col8,col2)) +
  ylab("Proportion") + xlab("Year")+
  theme(legend.position = 'none')

pdf("figures/tt_maps7.pdf",height = 5.75, width = 15)
grid.arrange(plt1,plt2,plt3,plt7,
             plt4,plt5,plt6, heights = c(1,0.4),nrow = 2)
dev.off()


df <- data.frame(x = c(1,2,3), y = c(1,2,3), z = c("Present", 'Absent', 'Projection'))
pdf("figures/legend.pdf",height = 3,width = 3)
df %>% ggplot() + aes(x = x, y = y, color = z, fill = z) + geom_tile() + theme_classic(base_size = 15) + 
  scale_color_manual("Observation", values = c( 'Present' = col2, 'Absent' = col8, 'Projection' = '#717171')) +
  scale_fill_manual("Observation",values = c('Present' = col2, 'Absent' = col8, 'Projection' = '#717171'))
dev.off()



