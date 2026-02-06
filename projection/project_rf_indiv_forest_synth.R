library(tidyverse)
library(gridExtra)
library(sf)
library(scico)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/projection/")

set.seed <- 11

canada_data <- st_read("/Volumes/My Book/gadm/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Alberta', 'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/gadm/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming",
                                           "North Dakota",'South Dakota'))

forest_2050_wide <- read_csv("../forest_change/data/forest_composition_2050_biomass.csv")
forest_2100_wide <- read_csv("../forest_change/data/forest_composition_2100_biomass.csv")

forest_2050_wide <- forest_2050_wide %>% select(-c(`Abies bracteata`, `Pseudotsuga macrocarpa`)) %>% select(-manual_id)
forest_2100_wide <- forest_2100_wide %>% select(-c(`Abies bracteata`, `Pseudotsuga macrocarpa`)) %>% select(-manual_id)

forest_2050_wide <- forest_2050_wide[!duplicated(forest_2050_wide),]
forest_2100_wide <- forest_2100_wide[!duplicated(forest_2100_wide),]

all_dataset2 <- read_csv('data/populations_for_cc_proj_synthetic.csv')

all_dataset2 <- all_dataset2 %>% 
  mutate(lat_coord = round(lat/0.25)*0.25,
         lon_coord = round(lon/0.25)*0.25)

new_cols <- colnames(forest_2050_wide)[c(6:16,18:20)]

all_dataset_2050 <- all_dataset2 %>% select(-new_cols) %>% select(-c(Abies_pres,Pseudotsuga_pres))
all_dataset_2100 <- all_dataset2 %>% select(-new_cols) %>% select(-c(Abies_pres,Pseudotsuga_pres))

forest_2050_wide <- forest_2050_wide %>%
  mutate(lon = round(lon,5),
         lat = round(lat,5))

forest_2100_wide <- forest_2100_wide %>%
  mutate(lon = round(lon,5),
         lat = round(lat,5))

all_dataset_2050 <- merge(all_dataset_2100,forest_2050_wide, all.x = TRUE)
all_dataset_2100 <- merge(all_dataset_2050,forest_2100_wide, all.x = TRUE)

all_dataset_2050 <- all_dataset_2050 %>% drop_na(mean_cover)
all_dataset_2100 <- all_dataset_2100 %>% drop_na(mean_cover)

all_dataset_2050[all_dataset_2050$near_needle == 0 & all_dataset_2050$host_trees == 1,]$near_needle <- 1
all_dataset_2100[all_dataset_2100$near_needle == 0 & all_dataset_2100$host_trees == 1,]$near_needle <- 1

all_dataset_2050 <- all_dataset_2050 %>% 
  rename(Abies_pres = Abies,
         Pseudotsuga_pres = Pseudotsuga) %>%
  mutate(host_trees = ifelse(Abies_pres + Pseudotsuga_pres > 0,1,0),
         host_trees2 = Abies_pres + Pseudotsuga_pres)

all_dataset_2100 <- all_dataset_2100 %>% 
  rename(Abies_pres = Abies,
         Pseudotsuga_pres = Pseudotsuga) %>%
  mutate(host_trees = ifelse(Abies_pres + Pseudotsuga_pres > 0,1,0),
         host_trees2 = Abies_pres + Pseudotsuga_pres)

##################################
##################################
##################################
##################################
##################################

#cc_weather <- read_csv('../climate/data/annual_simple_stats_all_models.csv')
avg5_historical <- read_csv('../range_modeling/data/avg5_weather_1940-2025.csv')

# avg5_cc <- cc_weather %>%
#   group_by(lat,lon,model) %>% arrange(year) %>%
#   mutate(min_t2m_r = rollmean(min_t2m, k = 5, na.pad = TRUE, align = 'right'),
#          max_t2m_r = rollmean(max_t2m, k = 5, na.pad = TRUE, align = 'right'),
#          min_rh_r = rollmean(min_rh, k = 5, na.pad = TRUE, align = 'right'),
#          max_rh_r = rollmean(max_rh, k = 5, na.pad = TRUE, align = 'right'),
#          min_tp_r = rollmean(min_tp, k = 5, na.pad = TRUE, align = 'right'),
#          max_tp_r = rollmean(max_tp, k = 5, na.pad = TRUE, align = 'right'),
#          julian_r = rollapply(julian, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = 'right'),
#          sum_tp_r = rollmean(sum_tp, k = 5, na.pad = TRUE,align = 'right'),
#          gdd_season_r = rollapply(gdd_season, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = 'right'),
#          coldest_r = rollmean(coldest, k = 5, na.pad = TRUE, align = 'right')) %>%
#   select(lat,lon,year,model,min_t2m_r,max_t2m_r,min_rh_r, max_rh_r, min_tp_r, max_tp_r, sum_tp_r,
#          julian_r,gdd_season_r,coldest_r) %>%
#   rename(min_t2m = min_t2m_r, max_t2m = max_t2m_r,min_rh = min_rh_r, max_rh = max_rh_r,
#          min_tp = min_tp_r, max_tp = max_tp_r, sum_tp = sum_tp_r,
#          julian = julian_r, gdd_season = gdd_season_r,coldest = coldest_r)

#write_csv(avg5_cc, 'data/avg5_weather_cc.csv')
avg5_cc <- read_csv('data/avg5_weather_cc.csv')
avg5_cc <- avg5_cc %>% drop_na(sum_tp) %>% drop_na(min_rh)

avg5_cc <- avg5_cc %>% filter(year %in% c(2050,2065,2100))

uni_mod <- unique(avg5_cc$model)
uni_years <- c(2050,2100)
#uni_mod <- uni_mod[2:10]

load(file = '/Volumes/My Book/Synchrony/rf_lags3/rf_models/rf_models1_0_lag353535.RData')
#load(file = '/Volumes/My Book/Synchrony/rf_update2/rf_update_0_lag353535.RData')
var_used <- rownames(rf_model$importance)

lag_temp <- 35
lag_rh <- 35
lag_pr <- 35

climate_change_proj <- c()
for(j in 1:length(uni_mod)){
  
  for(i in 1:length(uni_years)){
    cc_temp <- avg5_cc %>% filter(year == uni_years[i], model == uni_mod[j]) 
    
    if(lag_temp >0){
      
      if(uni_years[i] - lag_temp <= 2025){
        temp_vars <- avg5_historical %>% filter(year == uni_years[i] - lag_temp) %>%
          select(lat,lon,year,min_t2m,max_t2m,coldest,julian,gdd_season) %>% 
          mutate(year = year + lag_temp) %>%
          rename(min_t2m_lag = min_t2m, max_t2m_lag = max_t2m, coldest_lag = coldest,
                 julian_lag =julian,gdd_season_lag = gdd_season)
        pr_vars <- avg5_historical %>% filter(year == uni_years[i] - lag_temp) %>% 
          select(lat,lon,year,min_tp,max_tp,sum_tp) %>% 
          mutate(year = year + lag_pr) %>% rename(min_tp_lag = min_tp,max_tp_lag = max_tp,sum_tp_lag = sum_tp)
        rh_vars <- avg5_historical %>% filter(year == uni_years[i] - lag_temp) %>% 
          select(lat,lon,year,min_rh,max_rh) %>% 
          mutate(year = year + lag_rh) %>% rename(min_rh_lag = min_rh, max_rh_lag = max_rh)
      } else{
        temp_vars <- avg5_cc %>% filter(model == uni_mod[j], year == uni_years[i] - lag_temp) %>%
          select(lat,lon,year,min_t2m,max_t2m,coldest,julian,gdd_season) %>% 
          mutate(year = year + lag_temp) %>%
          rename(min_t2m_lag = min_t2m, max_t2m_lag = max_t2m, coldest_lag = coldest,
                 julian_lag =julian,gdd_season_lag = gdd_season)
        pr_vars <- avg5_cc %>% filter(model == uni_mod[j], year == uni_years[i] - lag_temp) %>% 
          select(lat,lon,year,min_tp,max_tp,sum_tp) %>% 
          mutate(year = year + lag_pr) %>% rename(min_tp_lag = min_tp,max_tp_lag = max_tp,sum_tp_lag = sum_tp)
        rh_vars <- avg5_cc %>% filter(model == uni_mod[j], year == uni_years[i] - lag_temp) %>% 
          select(lat,lon,year,min_rh,max_rh) %>% 
          mutate(year = year + lag_rh) %>% rename(min_rh_lag = min_rh, max_rh_lag = max_rh)
      }
      
      lagged_weather <- merge(temp_vars,pr_vars, all = TRUE)
      lagged_weather <- merge(lagged_weather, rh_vars, all = TRUE)
      
      lagged_weather <- merge(cc_temp, lagged_weather)
    } else{
      lagged_weather <- cc_temp
    }
    
    if(uni_years[i] == 2050){
      ds_temp <- merge(all_dataset_2050, lagged_weather, by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'),
                       all.x = TRUE)
    } else{
      ds_temp <- merge(all_dataset_2100, lagged_weather, by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'),
                       all.x = TRUE)
    }
    
    ds_temp <- ds_temp %>% drop_na(max_rh) %>% 
      select(lat,lon,source,manual_id,lat_coord,lon_coord,all_of(var_used))
    
    ds_temp$PA_pred <- predict(object=rf_model, newdata=ds_temp, type = 'prob')[,1]
    
    ds_temp$model <- uni_mod[j]
    ds_temp$year <- uni_years[i]
    
    climate_change_proj <- rbind(climate_change_proj,ds_temp)
  }
 
}
print(uni_years[i])
write_csv(climate_change_proj, paste0("output/proj_trees_synth/cc_proj_0_lag353535.csv"))

climate_change_proj <- read_csv(paste0("output/proj_trees_synth/cc_proj_0_lag353535.csv"))


thresh <- 0.325

mean_lags <- climate_change_proj %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0)) %>%
  group_by(lon_coord,lat_coord,model,year) %>%
  summarize(mean_pred = mean(PA_group),
            mean_presence = mean(present)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0),
         pres = ifelse(mean_presence > 0,1,0)) %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)

no_insects = 'grey55'
insects = 'red'
expansion = '#FFC107'
contraction = '#1E88E5'

no_insects = 'grey85'
per1 = '#FF7913'
per2 = '#00195F'
both = '#87B1D0'

pdf("figures/proj_trees_update2/model_projections_0_353535.pdf",height = 8, width = 24)
mean_lags %>% ggplot() + 
  geom_tile(aes(x = lon_coord, y = lat_coord, 
                color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                    'Expansion' = expansion, 'Contraction' = contraction)) +
  scale_fill_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                   'Expansion' = expansion, 'Contraction' = contraction))+ 
  xlab("Longitude") + ylab("Latitude") +
  facet_grid(year~model) +
  theme_classic()
dev.off()



################
################
################

all_dataset2 <- read_csv('data/populations_for_cc_proj.csv')
present_df <- all_dataset2 %>% select(lat,lon,lat_coord,lon_coord,source,present)

pres_coord <- present_df %>% 
  group_by(lon_coord,lat_coord) %>%
  summarize(mean_presence = mean(present)) %>% 
  mutate(pres = ifelse(mean_presence > 0,1,0)) %>% select(-mean_presence)

################
################
################

climate_change_proj <- read_csv("output/proj_trees_synth/cc_proj_update_0_lag353535.csv")
cc_proj_update <- read_csv("output/proj_indiv_synth/cc_proj_update_0_lag353535_all.csv")

climate_change_proj$trees_change <- "Including forest change"
cc_proj_update$trees_change <- "No forest change"

cc_proj_all <- rbind(climate_change_proj,cc_proj_update)

proj_summary <- cc_proj_all %>% select(lat_coord,lon_coord,year,trees_change,PA_pred,model) %>% 
  mutate(prediction = ifelse(PA_pred>=0.325,1,0)) %>% 
  group_by(lat_coord,lon_coord,year,trees_change,model) %>% 
  summarize(mean_pred = mean(prediction)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0)) %>% select(-mean_pred)

proj_summary <- merge(proj_summary, pres_coord,all.x = TRUE)

proj_summary <- proj_summary %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)


proj_summary$lag <- 35
proj_summary$fitting <- '1985-2025'

proj_summary %>% filter(year == 2050, model == 'NorESM2-MM') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = change, color = change) + geom_tile() + theme_classic() +
  facet_wrap(~trees_change)

write_csv(proj_summary,paste0("output/proj_trees_synth/summary_l35_update.csv"))

################
################
################

climate_change_proj <- read_csv("output/proj_trees_synth/cc_proj_update_0_lag000.csv")
cc_proj_update <- read_csv("output/proj_indiv_synth/cc_proj_update_0_lag000_all.csv")

climate_change_proj$trees_change <- "Including forest change"
cc_proj_update$trees_change <- "No forest change"

cc_proj_all <- rbind(climate_change_proj,cc_proj_update)

proj_summary <- cc_proj_all %>% select(lat_coord,lon_coord,year,trees_change,PA_pred,model) %>% 
  mutate(prediction = ifelse(PA_pred>=0.325,1,0)) %>% 
  group_by(lat_coord,lon_coord,year,trees_change,model) %>% 
  summarize(mean_pred = mean(prediction)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0)) %>% select(-mean_pred)

proj_summary <- merge(proj_summary, pres_coord,all.x = TRUE)

proj_summary <- proj_summary %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)


proj_summary$lag <- 0
proj_summary$fitting <- '1985-2025'

proj_summary %>% filter(year == 2050, model == 'NorESM2-MM') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = change, color = change) + geom_tile() + theme_classic() +
  facet_wrap(~trees_change)

write_csv(proj_summary,paste0("output/proj_trees_synth/summary_l0_update.csv"))

################
################
################

climate_change_proj <- read_csv("output/proj_trees_synth/cc_proj_0_lag353535.csv")
cc_proj_update <- read_csv("output/proj_indiv_synth/cc_proj_0_lag353535_all.csv")

climate_change_proj$trees_change <- "Including forest change"
cc_proj_update$trees_change <- "No forest change"

cc_proj_all <- rbind(climate_change_proj,cc_proj_update)

proj_summary <- cc_proj_all %>% select(lat_coord,lon_coord,year,trees_change,PA_pred,model) %>% 
  mutate(prediction = ifelse(PA_pred>=0.325,1,0)) %>% 
  group_by(lat_coord,lon_coord,year,trees_change,model) %>% 
  summarize(mean_pred = mean(prediction)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0)) %>% select(-mean_pred)

proj_summary <- merge(proj_summary, pres_coord,all.x = TRUE)

proj_summary <- proj_summary %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)


proj_summary$lag <- 35
proj_summary$fitting <- '1985-2010'

proj_summary %>% filter(year == 2050, model == 'NorESM2-MM') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = change, color = change) + geom_tile() + theme_classic() +
  facet_wrap(~trees_change)

write_csv(proj_summary,paste0("output/proj_trees_synth/summary_l35.csv"))
################
################
################

climate_change_proj <- read_csv("output/proj_trees_synth/cc_proj_0_lag000.csv")
cc_proj_update <- read_csv("output/proj_indiv_synth/cc_proj_0_lag000_all.csv")

climate_change_proj$trees_change <- "Including forest change"
cc_proj_update$trees_change <- "No forest change"

cc_proj_all <- rbind(climate_change_proj,cc_proj_update)

proj_summary <- cc_proj_all %>% select(lat_coord,lon_coord,year,trees_change,PA_pred,model) %>% 
  mutate(prediction = ifelse(PA_pred>=0.325,1,0)) %>% 
  group_by(lat_coord,lon_coord,year,trees_change,model) %>% 
  summarize(mean_pred = mean(prediction)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0)) %>% select(-mean_pred)

proj_summary <- merge(proj_summary, pres_coord,all.x = TRUE)

proj_summary <- proj_summary %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)


proj_summary$lag <- 0
proj_summary$fitting <- '1985-2010'

proj_summary %>% filter(year == 2050, model == 'NorESM2-MM') %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord, fill = change, color = change) + geom_tile() + theme_classic() +
  facet_wrap(~trees_change)

write_csv(proj_summary,paste0("output/proj_trees_synth/summary_l0.csv"))

################
################
################

################
################
################

l0 <- read_csv("output/proj_trees_synth/summary_l0.csv")
l35 <- read_csv("output/proj_trees_synth/summary_l35.csv")
l0_update <- read_csv("output/proj_trees_synth/summary_l0_update.csv")
l35_update <- read_csv("output/proj_trees_synth/summary_l35_update.csv")

cc_proj_all <- rbind(l0,l35,l0_update,l35_update)
cc_proj_all <- cc_proj_all %>% filter(year %in% c(2050,2100), fitting == '1985-2025', lag == 35)
fit_differences <- cc_proj_all %>% mutate(tree = ifelse(trees_change == 'No forest change','tree1','tree2')) %>% 
  select(lat_coord,lon_coord,year,tree,bin,model,lag,fitting) %>% pivot_wider(names_from = tree, values_from = bin) %>% 
  mutate(pred2 = case_when(tree1 == 0 & tree2 == 0 ~ 'No insects',
                           tree1 > 0 & tree2 == 0 ~ 'Projected in no forest change only',
                           tree1 == 0 & tree2 > 0 ~ 'Projected in forest change only',
                           tree1 >0  & tree2 >0 ~ 'Projected in both'))

no_insects = 'grey55'
insects = 'red'
expansion = '#FFC107'
contraction = '#1E88E5'

no_insects = 'grey85'
per1 = '#FF7913'
per2 = '#00195F'
both = '#87B1D0'

uni_mod <- unique(cc_proj_all$model)

for(j in 1:length(uni_mod)){
  pdf(paste0("figures/trees_comparison/proj_comparison_",uni_mod[j],".pdf"),height = 12, width = 7)
  plt1 <- cc_proj_all %>% filter(model == uni_mod[j]) %>% ggplot() + 
    geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
    scale_color_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                      'Expansion' = expansion, 'Contraction' = contraction)) +
    scale_fill_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                     'Expansion' = expansion, 'Contraction' = contraction))+ 
    xlab("Longitude") + ylab("Latitude") +
    facet_grid(trees_change~year) +
    theme_classic() +
    theme(legend.position = 'top')
  
  plt2 <- fit_differences %>% filter(model == uni_mod[j]) %>% ggplot() + 
    geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(pred2), fill = as.factor(pred2))) + 
    scale_color_manual("", values = c('No insects' = no_insects, 'Projected in no forest change only' = per1,
                                      'Projected in forest change only' = per2, 'Projected in both' = both)) +
    scale_fill_manual("", values = c('No insects' = no_insects, 'Projected in no forest change only' = per1,
                                     'Projected in forest change only' = per2, 'Projected in both' = both))+ 
    xlab("Longitude") + ylab("Latitude") +
    facet_grid(model~year) +
    theme_classic()+
    theme(legend.position = 'bottom')
  
  grid.arrange(plt1,plt2,nrow = 2, heights = c(2,1.2))
  dev.off()
  
  
}

pres_summary1 <- fit_differences %>% mutate(forest_change = ifelse(tree2 >0, 'present','absent')) %>%
  group_by(lat_coord,lon_coord,year,lag,fitting) %>% count(forest_change) %>% 
  pivot_wider(names_from = 'forest_change', values_from = 'n') %>% mutate(present = ifelse(is.na(present),0,present),
                                                                 absent = ifelse(is.na(absent),0,absent)) %>% 
  mutate(present = ifelse(absent == 10, 0,present)) %>% mutate(type = 'forest change')

pres_summary2 <- fit_differences %>% mutate(no_change = ifelse(tree1 >0, 'present','absent')) %>%
  group_by(lat_coord,lon_coord,year,lag,fitting) %>% count(no_change) %>% 
  pivot_wider(names_from = 'no_change', values_from = 'n') %>% mutate(present = ifelse(is.na(present),0,present),
                                                                          absent = ifelse(is.na(absent),0,absent)) %>% 
  mutate(present = ifelse(absent == 10, 0,present))%>% mutate(type = 'no change')

pres_summary <- rbind(pres_summary1,pres_summary2)

pres_summary %>% ggplot() +
   geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  theme(legend.position = 'top') +
  facet_grid(year~type) +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  theme_classic()

cols <- rev(scico(21, palette = 'vik'))
cols[11] <- '#FFFFFF'
cols2 <- rev(scico(7, palette = 'vik'))
cols2[4] <- '#FFFFFF'

pres_summary %>% select(-absent) %>% pivot_wider(values_from = 'present', names_from = 'type') %>% 
  mutate(diff = `forest change` - `no change`) %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  theme(legend.position = 'right') +
  facet_wrap(~year,nrow = 2, strip.position = 'right') +
  scale_color_manual("Difference in\n# models by\nadding forest change", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nadding forest change", values = cols2) +
  theme_classic() +
  xlab("Longitude") + ylab("Latitude") 

diff_pres <- ggplot() + 
  geom_tile(data = pres_summary, aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  facet_grid(year~type) +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 


plt_diff <- diff_pres + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'left') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

plt_change <- pres_summary %>% select(-absent) %>% pivot_wider(values_from = 'present', names_from = 'type') %>% 
  mutate(diff = `forest change` - `no change`) %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  theme(legend.position = 'right') +
  facet_wrap(~year,nrow = 2, strip.position = 'right') +
  scale_color_manual("Difference in\n# models by\nincluding change", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding change", values = cols2) +
  theme_classic() +
  xlab("Longitude") + ylab("Latitude") 

plt_change2 <- plt_change + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

pdf("figures/trees_comparison/model_present_l35_update.pdf",height = 10, width = 20)
grid.arrange(plt_diff,plt_change2,nrow = 1, widths = c(1,0.7))
dev.off()

