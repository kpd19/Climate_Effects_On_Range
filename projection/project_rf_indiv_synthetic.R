library(tidyverse)
library(gridExtra)
library(sf)
library(scico)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/projection/")

`%ni` <- Negate(`%in%`)

canada_data <- st_read("/Volumes/My Book/gadm/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/gadm/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))

# all_dataset2 <- read_csv('data/populations_for_cc_proj.csv')
# synthetic_data <- read_csv(file = '../population_data/data/synthetic_data_habitat.csv')
# 
# forest_current <- read_csv("../landscape/data/forest_species_pa.csv")
# habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")
# 
# forest_current <- forest_current %>% select(-c(manual_id,source)) %>%
#   rename(Abies_pres = sum_Abies,
#          Pseudotsuga_pres = sum_Pseudotsuga) %>%
#   mutate(Abies_pres = ifelse(Abies_pres >0,1,0)) %>%
#   mutate(host_trees = ifelse(Abies_pres + Pseudotsuga_pres > 0,1,0),
#          host_trees2 = Abies_pres + Pseudotsuga_pres)
# forest_current <- forest_current[!duplicated(forest_current),]
# 
# habitat_features <- habitat_features %>% mutate(lon = round(lon,5),
#                                                 lat = round(lat,5))
# forest_current <- forest_current %>% mutate(lon = round(lon,5),
#                                             lat = round(lat,5))
# 
# habitat_features2 <- merge(habitat_features,forest_current, all = TRUE)
# 
# habitat_features2 <- habitat_features2 %>% select(-manual_id)
# 
# habitat_features2 <- habitat_features2 %>% mutate(aspect2 = case_when(aspect >=0 & aspect <= 22.5 ~ "North",
#                                                                       aspect >22.5 & aspect <= 67.5 ~ "Northeast",
#                                                                       aspect >67.5 & aspect <= 112.5 ~ "East",
#                                                                       aspect >112.5 & aspect <= 157.5 ~ "Southeast",
#                                                                       aspect >157.5 & aspect <= 202.5 ~ "South",
#                                                                       aspect >202.5 & aspect <= 247.5 ~ "Southwest",
#                                                                       aspect >247.5 & aspect <= 292.5 ~ "West",
#                                                                       aspect >292.5 & aspect <= 337.5 ~ "Northwest",
#                                                                       aspect >337.5 & aspect <= 360 ~ "North",
#                                                                       aspect == -1 ~ "Flat"))
# 
# all_dataset <- synthetic_data
# 
# all_dataset <- all_dataset %>%
#   mutate(lon = round(lon,5),
#          lat = round(lat,5))
# 
# all_dataset2 <- merge(all_dataset, habitat_features2, all.x = TRUE)
# 
# all_dataset2 <- all_dataset2[!duplicated(all_dataset2),]
# 
# all_dataset2 <- all_dataset2 %>% drop_na(elev2) %>%
#   mutate_at(c('mean_biomass','max_biomass','mean_cover','slope'), ~replace_na(.,0))
# 
# 
# all_dataset2 %>% ggplot() + aes(x = lon,y = lat, color = elev2) + geom_point() + 
#   theme_classic()
# 
# all_dataset2 %>% mutate(lat_coord = round(lat/0.25)*0.25,
#                           lon_coord = round(lon/0.25)*0.25,
#                           n = 1) %>% 
#   group_by(lat_coord,lon_coord) %>% count(n) %>%
#   ggplot() + aes(x = lon_coord,y = lat_coord, fill = nn) + geom_tile(color = NA) + 
#   theme_classic()
# 
# write_csv(all_dataset2, 'data/populations_for_cc_proj_synthetic.csv')

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

# avg5_cc %>% filter(model == 'INM-CM5-0', lat == 52.25, lon == -117)
# 
# prev_test <- avg5_historical %>% filter(lat == 45, lon == -117.5) %>% 
#   mutate(model = 'Historical data')
# cc_test <- avg5_cc %>% filter(lat == 45, lon == -117.5)
# 
# test_all <- rbind(prev_test,cc_test)
# 
# pdf("figures/proj_indiv/cc_comparison.pdf",height = 6, width = 15)
# test_all %>% pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
#                                    'julian','gdd_season','sum_tp','coldest')) %>% 
#   ggplot() + aes(x = year, y = value, color = model) + geom_point() +
#   theme_classic() + facet_wrap(~name, scales = 'free_y')
# dev.off()
# 
# avg5_cc %>% drop_na(sum_tp) %>% filter(year == 2035) %>%
#   ggplot() + aes(x = lon,y = lat, color = julian, fill = julian) + geom_tile() +
#   theme_classic() + facet_wrap(~model)

load(file = '/Volumes/My Book/Synchrony/rf_lags3/rf_models/rf_models1_0_lag000.RData')
#load(file = '/Volumes/My Book/Synchrony/rf_update2/rf_update_0_lag353535.RData')
var_used <- rownames(rf_model$importance)
all_dataset2 <- read_csv('data/populations_for_cc_proj_synthetic.csv')

all_dataset2 <- all_dataset2 %>% 
  mutate(lat_coord = round(lat/0.25)*0.25,
         lon_coord = round(lon/0.25)*0.25)

avg5_cc_pick <- avg5_cc %>% filter(year %in% c(2040,2050,2060,2070,2080,2090,2100))

uni_years <- unique(avg5_cc_pick$year)
uni_mod <- unique(avg5_cc_pick$model)
#uni_mod <- uni_mod[2:10]

lag_temp <- 0
lag_rh <- 0
lag_pr <- 0

for(j in 1:length(uni_mod)){
  climate_change_proj <- c()
  for(i in 1:length(uni_years)){
    cc_temp <- avg5_cc_pick %>% filter(year == uni_years[i], model == uni_mod[j]) 
    
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
    
    ds_temp <- merge(all_dataset2, lagged_weather, by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'),
                     all.x = TRUE)
    
    ds_temp <- ds_temp %>% drop_na(max_rh) %>% 
      select(lat,lon,source,manual_id,lat_coord,lon_coord,all_of(var_used))
    
    ds_temp$PA_pred <- predict(object=rf_model, newdata=ds_temp, type = 'prob')[,1]
    
    ds_temp$model <- uni_mod[j]
    ds_temp$year <- uni_years[i]
      
    climate_change_proj <- rbind(climate_change_proj,ds_temp)
  }
  print(uni_years[i])
  write_csv(climate_change_proj, paste0("output/proj_indiv_synth/cc_proj_0_lag000_",uni_mod[j],".csv"))
  
}

all_proj <- c()
for(j in 1:length(uni_mod)){
  climate_change_proj <- read_csv(paste0("output/proj_indiv_synth/cc_proj_0_lag000_",uni_mod[j],".csv"))
  all_proj <- rbind(all_proj, climate_change_proj)
}

write_csv(all_proj, "output/proj_indiv_synth/cc_proj_0_lag000_all.csv")

################
################
################

all_dataset2 <- read_csv('data/populations_for_cc_proj.csv')
present_df <- all_dataset2 %>% select(lat,lon,lat_coord,lon_coord,source,present)

cc_proj_period35 <- read_csv("output/proj_indiv_synth/cc_proj_0_lag353535_all.csv")
cc_proj_period0 <- read_csv("output/proj_indiv_synth/cc_proj_0_lag000_all.csv")
cc_proj_update35 <- read_csv("output/proj_indiv_synth/cc_proj_update_0_lag353535_all.csv")
cc_proj_update0 <- read_csv("output/proj_indiv_synth/cc_proj_update_0_lag000_all.csv")

cc_proj_period35$fitting <- "1985-2010"
cc_proj_update35$fitting <- "1985-2025"

cc_proj_period0$fitting <- "1985-2010"
cc_proj_update0$fitting <- "1985-2025"

cc_proj_period35$lag <- 35
cc_proj_update35$lag <- 35

cc_proj_period0$lag <- 0
cc_proj_update0$lag <- 0

cc_proj_period0[,c('max_rh_lag','max_t2m_lag','max_tp_lag','min_rh_lag',
  'min_t2m_lag', 'min_tp_lag','coldest_lag','julian_lag',
  'sum_tp_lag','gdd_season_lag')] <- NA

cc_proj_update0[,c('max_rh_lag','max_t2m_lag','max_tp_lag','min_rh_lag',
                   'min_t2m_lag', 'min_tp_lag','coldest_lag','julian_lag',
                   'sum_tp_lag','gdd_season_lag')] <- NA

cc_proj_all <- rbind(cc_proj_period35,cc_proj_update35,cc_proj_period0,cc_proj_update0)

write_csv(cc_proj_all,'/Volumes/My Book/Synchrony/projections/cc_proj_all_synthetic.csv')

thresh <- 0.325

mean_lags <- cc_proj_all %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0)) %>%
  group_by(lon_coord,lat_coord,model,year,fitting,lag) %>%
  summarize(mean_pred = mean(PA_group)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0))

mean_lags %>% filter(model == "ACCESS-ESM1-5", year == 2100) %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = bin, fill = bin) + geom_tile() + theme_classic() +
  facet_grid(lag~fitting)

pres_coord <- present_df %>% 
  group_by(lon_coord,lat_coord) %>%
  summarize(mean_presence = mean(present)) %>% 
  mutate(pres = ifelse(mean_presence > 0,1,0))

mean_lags <- merge(mean_lags, pres_coord,all.x = TRUE)
  
mean_lags2 <- mean_lags %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'Contraction',
                            bin == 0 & pres == 0 ~ 'No insects',
                            bin == 1 & pres == 0 ~ 'Expansion',
                            bin == 1 & pres == 1 ~ 'Insects present')) %>% drop_na(change)


mean_lags2 %>% filter(model == "ACCESS-ESM1-5", year == 2100) %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = change, fill = change) + geom_tile() + theme_classic() +
  facet_grid(lag~fitting)

write_csv(mean_lags2, "output/proj_indiv_synth/fit_differences_cc.csv")

#####################
#####################
#####################


mean_lags2 <- read_csv("output/proj_indiv_synth/fit_differences_cc.csv")

fit_differences <- mean_lags2 %>% mutate(per = ifelse(fitting == '1985-2010','per1','per2')) %>% ungroup() %>% 
  select(lat_coord,lon_coord,year,per,mean_pred,model,lag) %>% pivot_wider(names_from = per, values_from = mean_pred) %>% 
  mutate(pred2 = case_when(per1 == 0 & per2 == 0 ~ 'No insects',
                           per1 > 0 & per2 == 0 ~ 'Projected in first period fit only',
                           per1 == 0 & per2 > 0 ~ 'Projected in both periods fit only',
                           per1 >0  & per2 >0 ~ 'Projected in both fits'))

no_insects = 'grey55'
insects = 'red'
expansion = '#FFC107'
contraction = '#1E88E5'

no_insects = 'grey85'
per1 = '#FF7913'
per2 = '#00195F'
both = '#87B1D0'

mean_lags2 %>% filter(lag == 0) %>% ggplot() + 
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                    'Expansion' = expansion, 'Contraction' = contraction)) +
  scale_fill_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                   'Expansion' = expansion, 'Contraction' = contraction))+ 
  xlab("Longitude") + ylab("Latitude") +
  facet_grid(model~year) +
  theme_classic()

for (l in c(0,35)){
  for(j in 1:length(uni_mod)){
    pdf(paste0("figures/synth_comparison/proj_comparison_",uni_mod[j],"_",l,".pdf"),height = 7, width = 10)
    plt1 <- mean_lags2 %>% filter(model == uni_mod[j], lag == l) %>% ggplot() + 
      geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
      scale_color_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                        'Expansion' = expansion, 'Contraction' = contraction)) +
      scale_fill_manual("", values = c('No insects' = no_insects, 'Insects present' = insects,
                                       'Expansion' = expansion, 'Contraction' = contraction))+ 
      xlab("Longitude") + ylab("Latitude") +
      facet_grid(fitting~year) +
      theme_classic() +
      theme(legend.position = 'top')
    
    plt2 <- fit_differences %>% filter(model == uni_mod[j], lag == l) %>% ggplot() + 
      geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(pred2), fill = as.factor(pred2))) + 
      scale_color_manual("", values = c('No insects' = no_insects, 'Projected in first period fit only' = per1,
                                        'Projected in both periods fit only' = per2, 'Projected in both fits' = both)) +
      scale_fill_manual("", values = c('No insects' = no_insects, 'Projected in first period fit only' = per1,
                                       'Projected in both periods fit only' = per2, 'Projected in both fits' = both))+ 
      xlab("Longitude") + ylab("Latitude") +
      facet_grid(model~year) +
      theme_classic()+
      theme(legend.position = 'bottom')
    
    grid.arrange(plt1,plt2,nrow = 2, heights = c(1,0.75))
    dev.off()
    
    
  }
}


pres_summary_both <- fit_differences %>% mutate(qual = ifelse(per2 >0, 'present','absent')) %>%
  group_by(lat_coord,lon_coord,year,lag) %>% count(qual) %>% 
  pivot_wider(names_from = 'qual', values_from = 'n') %>% mutate(present = ifelse(is.na(present),0,present),
                                                                 absent = ifelse(is.na(absent),0,absent)) %>% 
  mutate(present = ifelse(absent == 10, 0,present)) %>% mutate(fitting = '1985-2025')

pres_summary_per1 <- fit_differences %>% mutate(qual = ifelse(per1 >0, 'present','absent')) %>%
  group_by(lat_coord,lon_coord,year,lag) %>% count(qual) %>% 
  pivot_wider(names_from = 'qual', values_from = 'n') %>% mutate(present = ifelse(is.na(present),0,present),
                                                                 absent = ifelse(is.na(absent),0,absent)) %>% 
  mutate(present = ifelse(absent == 10, 0,present)) %>% mutate(fitting = '1985-2005')

pres_summary_both %>% ggplot() +
  #geom_sf(data = canada_data, aes(geometry = geometry), color = 'grey55',size = 0.5) +
  #geom_sf(data = us_data, aes(geometry = geometry), color = 'grey55',size = 0.5) +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  #coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  theme(legend.position = 'top') + 
  facet_grid(lag~year) +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  theme_classic()

diff_pres <- ggplot() + 
  geom_tile(data = pres_summary_per1[pres_summary_per1$lag == 35,], aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  facet_wrap(~year,nrow = 2)+
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 

plt_diff <- diff_pres + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'top') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

pdf("figures/synth_comparison/model_present_per1_lag35.pdf",height = 10, width = 15)
plt_diff
dev.off()

cols <- rev(scico(21, palette = 'vik'))
cols[11] <- '#FFFFFF'
cols2 <- rev(scico(7, palette = 'vik'))
cols2[4] <- '#FFFFFF'

plt_change <- pres_summary_per1 %>% mutate(lag_name = paste0("lag_",lag)) %>% ungroup() %>% select(-c(lag,absent)) %>% 
  pivot_wider(names_from = lag_name, values_from = present) %>% 
  mutate(diff = lag_35 - lag_0)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  theme(legend.position = 'top') +
  facet_wrap(~year,nrow = 2, strip.position = 'right') +
  scale_color_manual("Difference in\n# models", values = cols2)+
  scale_fill_manual("Difference in\n# models", values = cols2) +
  theme_classic() +
  xlab("Longitude") + ylab("Latitude") 


plt_change2 <- plt_change + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))


pdf("figures/synth_comparison/model_present_lag_diff_per1.pdf",height = 10, width = 15)
plt_change2
dev.off()


###########################
###########################
###########################

per1_0 <- ggplot() + 
  geom_tile(data = pres_summary_per1[pres_summary_per1$lag == 0 & pres_summary_per1$year == 2100,], aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 

plt_per1_0 <- per1_0 + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

per1_35 <- ggplot() + 
  geom_tile(data = pres_summary_per1[pres_summary_per1$lag == 35 & pres_summary_per1$year == 2100,], aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 

plt_per1_35 <- per1_35 + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

per2_0 <- ggplot() + 
  geom_tile(data = pres_summary_both[pres_summary_both$lag == 0 & pres_summary_both$year == 2100,], aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 

plt_per2_0 <- per1_0 + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

per2_35 <- ggplot() + 
  geom_tile(data = pres_summary_both[pres_summary_both$lag == 35 & pres_summary_both$year == 2100,], aes(x = lon_coord, y = lat_coord, color = as.factor(present), fill = as.factor(present))) + 
  theme_classic() +
  scale_color_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako'))))+
  scale_fill_manual("# CMIP6\nmodels", values = c('grey90',rev(scico(10, palette = 'bamako')))) +
  xlab("Longitude") + ylab("Latitude") 

plt_per2_35 <- per1_35 + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plt_per1_0, plt_per1_35,
             plt_per2_0, plt_per2_35,nrow = 2)


pdf("figures/synth_comparison/all_2100.pdf",height = 10, width = 15)
grid.arrange(plt_per1_0, plt_per1_35,
             plt_per2_0, plt_per2_35,nrow = 2)
dev.off()

cols <- rev(scico(21, palette = 'vik'))
cols[11] <- '#FFFFFF'
cols2 <- rev(scico(7, palette = 'vik'))
cols2[4] <- '#FFFFFF'

diff_per1_lag <- pres_summary_per1 %>% filter(year == 2100) %>% mutate(lag_name = paste0("lag_",lag)) %>% ungroup() %>% select(-c(lag,absent)) %>% 
  pivot_wider(names_from = lag_name, values_from = present) %>% 
  mutate(diff = lag_35 - lag_0)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  scale_color_manual("Difference in\n# models by\nincluding lag", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding lag", values = cols2) +
  theme_classic() +
  theme(legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") 

plt_per1_lag <- diff_per1_lag + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

diff_per2_lag <- pres_summary_both %>% filter(year == 2100) %>% mutate(lag_name = paste0("lag_",lag)) %>% ungroup() %>% select(-c(lag,absent)) %>% 
  pivot_wider(names_from = lag_name, values_from = present) %>% 
  mutate(diff = lag_35 - lag_0)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  scale_color_manual("Difference in\n# models by\nincluding lag", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding lag", values = cols2) +
  theme_classic() +
  theme(legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") 

plt_per2_lag <- diff_per2_lag + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))


fit1 <- pres_summary_per1 %>% filter(year == 2100) 
fit2 <- pres_summary_both %>% filter(year == 2100) 

fit_all <- rbind(fit1,fit2)

diff_lag0_per <- fit_all %>% filter(lag == 0) %>%
  mutate(fit_name = ifelse(fitting == '1985-2005','per1', 'per2')) %>% 
  ungroup() %>% select(-c(lag,fitting,absent)) %>% 
  pivot_wider(names_from = fit_name, values_from = present) %>% 
  mutate(diff = per2 - per1)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  scale_color_manual("Difference in\n# models by\nincluding testing data", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding testing data", values = cols2) +
  theme_classic() +
  theme(legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") 

plt_lag0_per <- diff_lag0_per + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

diff_lag35_per <- fit_all %>% filter(lag == 35) %>%
  mutate(fit_name = ifelse(fitting == '1985-2005','per1', 'per2')) %>% 
  ungroup() %>% select(-c(lag,fitting,absent)) %>% 
  pivot_wider(names_from = fit_name, values_from = present) %>% 
  mutate(diff = per2 - per1)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  scale_color_manual("Difference in\n# models by\nincluding testing data", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding testing data", values = cols2) +
  theme_classic() +
  theme(legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") 

plt_lag35_per <- diff_lag35_per + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

fit3 <- pres_summary_per1 %>% filter(year == 2100, lag == 0) 
fit4 <- pres_summary_both %>% filter(year == 2100, lag == 35) 

fit_34 <- rbind(fit3,fit4)

diff_x2 <- fit_34 %>% 
  mutate(fit_name = ifelse(lag == 0,'base', 'extended')) %>% 
  ungroup() %>% select(-c(lag,fitting,absent)) %>% 
  pivot_wider(names_from = fit_name, values_from = present) %>% 
  mutate(diff = extended - base)  %>% 
  mutate(diff2 = case_when(diff <= -7 ~'-10 to -7',
                           diff <= -4 & diff >= -7 ~'-6 to -4',
                           diff <= -1 & diff >= -3 ~ '-3 to -1',
                           diff == 0 ~ "0",
                           diff <= 3 & diff >= 1 ~ '1 to 3',
                           diff <= 6 & diff >= 4 ~ '4 to 6',
                           diff >= 7 ~ '7 to 10')) %>% 
  drop_na(diff2) %>% 
  mutate(diff2 = factor(diff2, levels = c('-10 to -7','-6 to -4','-3 to -1',"0",
                                          '1 to 3','4 to 6','7 to 10'))) %>% ggplot() +
  geom_tile(aes(x = lon_coord, y = lat_coord, color = as.factor(diff2), fill = as.factor(diff2))) + 
  coord_sf(xlim = c(-128,-103), ylim = c(33,53)) + 
  scale_color_manual("Difference in\n# models by\nincluding both lag\nand testing data", values = cols2)+
  scale_fill_manual("Difference in\n# models by\nincluding both lag\nand testing data", values = cols2) +
  theme_classic() +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = 'none') 
  

plt_diff_x2 <- diff_x2 + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  theme(legend.position = 'none') +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  theme(plot.title = element_text(hjust = 0.5))

pdf("figures/synth_comparison/all_comparison.pdf",height = 20, width = 20)
grid.arrange(plt_per1_0, plt_per1_35, plt_per1_lag,
             plt_per2_0, plt_per2_35, plt_per2_lag,
             plt_lag0_per, plt_lag35_per, plt_diff_x2, nrow = 3)

dev.off()
