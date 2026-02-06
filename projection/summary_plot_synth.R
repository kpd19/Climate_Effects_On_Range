library(tidyverse)
library(gridExtra)
library(ggnewscale)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/projection/")

canada_data <- st_read("/Volumes/My Book/gadm/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/gadm/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming",
                                           "North Dakota",'South Dakota'))

all_dataset2 <- read_csv('data/populations_for_cc_proj.csv')
present_df <- all_dataset2 %>% select(lat,lon,source,present,lat_coord,lon_coord)

cc_proj_all <- read_csv('output/proj_indiv_synth/cc_proj_update_0_lag353535_all.csv')

proj_summary <- cc_proj_all %>% select(lat_coord,lon_coord,year,PA_pred,model) %>% 
  mutate(prediction = ifelse(PA_pred>=0.325,1,0)) 
proj_summary <- proj_summary %>% group_by(lat_coord,lon_coord,year,model) %>% 
  summarize(sum_present = sum(prediction)) %>% mutate(pres = ifelse(sum_present>1,1,0)) %>% 
  select(-sum_present)

present_summary <- present_df %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                         lon_coord = round(lon/0.25)*0.25) %>% 
  group_by(lat_coord,lon_coord) %>% summarize(sum_present = sum(present)) %>% 
  mutate(pres = ifelse(sum_present>1,1,0)) 

present_summary %>% 
  mutate(p = ifelse(sum_present>1,1,0)) %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = pres, fill = pres) + geom_tile() + 
  theme_classic()

present_summary <- present_summary %>% select(-sum_present) %>% rename(present = pres)

proj_summary2 <- merge(proj_summary,present_summary)

proj_summary2 <- proj_summary2 %>% mutate(change = case_when(present == 0 & pres == 0 ~ 'No insects',
                                                             present == 1 & pres == 1 ~ 'Insects present',
                                                             present == 0 & pres == 1 ~ 'Expansion',
                                                             present == 1 & pres == 0 ~ 'Contraction'))

proj_summary2 %>% filter(year == 2100,model == 'KACE-1-0-G') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = change, fill = change) + geom_tile() + 
  theme_classic() + 
  facet_wrap(~model)

#write_csv(proj_summary2, 'output/proj_trees_update2/change_update_0_000_trees.csv')

change_df <- proj_summary2 %>% group_by(lat_coord,lon_coord,year) %>% count(change) 

change_df %>% 
  filter(change == 'Contraction') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, color = n, fill = n) + geom_tile() + 
  theme_classic() + 
  facet_grid(~year)

change_df$trees_change <- "No forest change"


tc <- 'No forest change'

df11 <- change_df %>% filter(change %in% c("No insects")) %>% filter(n ==10)%>%
  filter(trees_change == tc, year == 2100)
df21 <- change_df %>% filter(change %in% c("Insects present")) %>% filter(n ==10)%>%
  filter(trees_change == tc, year == 2100)
df31 <- change_df %>% filter(change %in% c("Contraction"))%>%
  filter(trees_change == tc, year == 2100)
df41 <- change_df %>% filter(change %in% c("Expansion"))%>%
  filter(trees_change == tc, year == 2100)

diffs <- rbind(df11,df21,df31,df41)

diffs <- diffs %>% mutate(diff = case_when(change == 'No insects' & n ==10 ~ 'Absent + no insects 10 models',
                                           change == 'Insects present' & n ==10 ~ 'Current range + present 10 models',
                                           change == 'Contraction' & n <= 3 ~ 'Contraction 1-3 models',
                                           change == 'Contraction' & n>3 & n <= 6 ~ 'Contraction 4-6 models',
                                           change == 'Contraction' & n >6 ~ 'Contraction 7-10 models',
                                           change == 'Expansion' & n <= 3 ~ 'Expansion 1-3 models',
                                           change == 'Expansion' & n>3 & n <= 6 ~ 'Expansion 4-6 models',
                                           change == 'Expansion' & n >6 ~ 'Expansion 7-10 models')) %>% 
  mutate(diff = factor(diff, levels = c('Absent + no insects 10 models', 'Contraction 7-10 models', 'Contraction 4-6 models', 
                                        'Contraction 1-3 models', 'Expansion 1-3 models', 'Expansion 4-6 models',
                                        'Expansion 7-10 models', 'Current range + present 10 models')))

pdf("figures/summaryplt/proj_update_0_353535_nfc_synth.pdf",height = 9, width = 9)
plt <- ggplot() + geom_tile(data = diffs,aes(x = lon_coord, y = lat_coord, color = diff, fill = diff)) + 
  theme_classic() + 
  scale_color_manual('', values = c('Absent + no insects 10 models' = 'grey90', 'Contraction 7-10 models' = '#b2abd2',
                                    'Contraction 4-6 models' = '#8073ac', 
                                    'Contraction 1-3 models' = '#542788', 'Expansion 1-3 models' = '#fee8c8',
                                    'Expansion 4-6 models' = '#fdbb84',
                                    'Expansion 7-10 models' = '#ef6548', 'Current range + present 10 models' = '#b30000'))+
  scale_fill_manual('', values = c('Absent + no insects 10 models' = 'grey90', 'Contraction 7-10 models' = '#b2abd2',
                                   'Contraction 4-6 models' = '#8073ac', 
                                   'Contraction 1-3 models' = '#542788', 'Expansion 1-3 models' = '#fee8c8',
                                   'Expansion 4-6 models' = '#fdbb84',
                                   'Expansion 7-10 models' = '#ef6548', 'Current range + present 10 models' = '#b30000')) + 
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = 'top') 
plt + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 0.1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 0.1) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-104)) 

dev.off()
