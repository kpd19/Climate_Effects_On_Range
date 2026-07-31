library(tidyverse)
library(randomForest)
#library(MESS)
library(pROC)
library(zoo)
library(foreach)
library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

t0 <- Sys.time()

print(paste0("Universe size = ", mpi.universe.size()))
print(paste0("Number of workers = ", mpi.comm.size(0) - 1))

num_trees_per_worker <- 200
num_workers <- mpi.comm.size(0) - 1
print(num_workers)

source("fitting_functions.R")

#set.seed <- 11

testing_dataset <- read_csv('data/testing_dataset_2011-2024_yrs.csv', guess_max = Inf)
training_dataset <- read_csv('data/training_dataset_1985-2010_yrs.csv', guess_max = Inf)

forest_current <- read_csv("../landscape/data/forest_species_pa.csv")
habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")

forest_current <- forest_current %>% select(-c(manual_id,source)) %>%
  rename(Abies_pres = sum_Abies,
         Pseudotsuga_pres = sum_Pseudotsuga) %>%
  mutate(Abies_pres = ifelse(Abies_pres >0,1,0)) %>%
  mutate(host_trees = ifelse(Abies_pres + Pseudotsuga_pres > 0,1,0),
         host_trees2 = Abies_pres + Pseudotsuga_pres)

forest_current <- forest_current[!duplicated(forest_current),]

habitat_features <- habitat_features %>% mutate(lon = round(lon,5),
                                                lat = round(lat,5))
forest_current <- forest_current %>% mutate(lon = round(lon,5),
                                            lat = round(lat,5))

habitat_features2 <- merge(habitat_features,forest_current, all = TRUE)

habitat_features2 <- habitat_features2 %>% select(-manual_id)

habitat_features2 <- habitat_features2 %>% mutate(aspect2 = case_when(aspect >=0 & aspect <= 22.5 ~ "North",
                                                                      aspect >22.5 & aspect <= 67.5 ~ "Northeast",
                                                                      aspect >67.5 & aspect <= 112.5 ~ "East",
                                                                      aspect >112.5 & aspect <= 157.5 ~ "Southeast",
                                                                      aspect >157.5 & aspect <= 202.5 ~ "South",
                                                                      aspect >202.5 & aspect <= 247.5 ~ "Southwest",
                                                                      aspect >247.5 & aspect <= 292.5 ~ "West",
                                                                      aspect >292.5 & aspect <= 337.5 ~ "Northwest",
                                                                      aspect >337.5 & aspect <= 360 ~ "North",
                                                                      aspect == -1 ~ "Flat"))

testing_dataset <- testing_dataset[!duplicated(testing_dataset),]
training_dataset <- training_dataset[!duplicated(training_dataset),]

testing_dataset <- testing_dataset %>%
  select(lat,lon,source,manual_id,lat_coord,lon_coord,count,present,track_late,year) %>%
  mutate(dataset = "testing", period = '2011-2024')%>% mutate(lon = round(lon,5),
                                                              lat = round(lat,5)) %>% mutate(track_early = NA)
training_dataset <- training_dataset %>%
  select(lat,lon,source,manual_id,lat_coord,lon_coord,count,present,track_early,year) %>%
  mutate(dataset = "training", period = '1985-2010')%>% mutate(lon = round(lon,5),
                                                               lat = round(lat,5))%>% mutate(track_late = NA)

testing_dataset2 <- merge(testing_dataset, habitat_features2, all.x = TRUE)
training_dataset2 <- merge(training_dataset, habitat_features2, all.x = TRUE)

ann_weather_stats <- read_csv("../climate/data/all_annual_1940-2024_2.csv")

print("Finished reading in weather")

ann_weather_stats <- ann_weather_stats %>% rename(year = year_tm)

test_all <- rbind(training_dataset2,testing_dataset2)

test_all <- test_all %>% mutate_at(c('mean_biomass','max_biomass'), ~replace_na(.,0)) %>%
  drop_na(elev2) %>%
  select(-c(modis_id,short))

test_all[test_all$near_needle == 0 & test_all$host_trees == 1,]$near_needle <- 1

avg5_weather <- ann_weather_stats %>% rename(lon = longitude, lat = latitude) %>%
  group_by(lat,lon) %>% arrange(year) %>%
  mutate(min_t2m_r = rollmean(min_t2m, k = 5, na.pad = TRUE, align = 'right'),
         max_t2m_r = rollmean(max_t2m, k = 5, na.pad = TRUE, align = 'right'),
         min_rh_r = rollmean(min_rh, k = 5, na.pad = TRUE, align = 'right'),
         max_rh_r = rollmean(max_rh, k = 5, na.pad = TRUE, align = 'right'),
         min_tp_r = rollmean(min_tp, k = 5, na.pad = TRUE, align = 'right'),
         max_tp_r = rollmean(max_tp, k = 5, na.pad = TRUE, align = 'right'),
         julian_r = rollapply(julian, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = 'right'),
         sum_tp_r = rollmean(sum_tp, k = 5, na.pad = TRUE,align = 'right'),
         gdd_season_r = rollapply(gdd_season, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = 'right'),
        coldest_r = rollmean(coldest, k = 5, na.pad = TRUE, align = 'right')) %>%
  select(lat,lon,year,min_t2m_r,max_t2m_r,min_rh_r, max_rh_r, min_tp_r, max_tp_r, sum_tp_r, julian_r,gdd_season_r,coldest_r) %>%
  rename(min_t2m = min_t2m_r, max_t2m = max_t2m_r,min_rh = min_rh_r, max_rh = max_rh_r,
         min_tp = min_tp_r, max_tp = max_tp_r, sum_tp = sum_tp_r, julian = julian_r, gdd_season = gdd_season_r,coldest = coldest_r)

lag_temp <- 15
lag_rh <- 15
lag_pr <- 15

if(lag_temp >0){
  temp_vars <- ann_weather_stats %>% select(lat,lon,year,min_t2m,max_t2m,coldest,julian,gdd_season) %>%
    mutate(year = year + lag_temp) %>%
    rename(min_t2m_lag = min_t2m, max_t2m_lag = max_t2m, coldest_lag = coldest,
           julian_lag =julian,gdd_season_lag = gdd_season) 
  pr_vars <- ann_weather_stats %>% select(lat,lon,year,min_tp,max_tp,sum_tp) %>%
    mutate(year = year + lag_pr) %>% rename(min_tp_lag = min_tp,max_tp_lag = max_tp,sum_tp_lag = sum_tp)
  rh_vars <- ann_weather_stats %>% select(lat,lon,year,min_rh,max_rh) %>%
    mutate(year = year + lag_rh) %>% rename(min_rh_lag = min_rh, max_rh_lag = max_rh) 

  lagged_weather <- merge(temp_vars,pr_vars)
  lagged_weather <- merge(lagged_weather, rh_vars)
  
  lagged_weather <- merge(ann_weather_stats, lagged_weather)
} else{
  lagged_weather <- ann_weather_stats  
}

test_all_lag0 <- merge(test_all,lagged_weather, by.x = c('lat_coord','lon_coord','year'), by.y = c('lat','lon','year'))

print("Finished merging")

train_lag0 <- test_all_lag0 %>% filter(track_early %in% c('in','near-1','near-2','near-3'),
                                       dataset == 'training')

var_names_long <- read_csv("data/var_names_pa2.csv")

keep_col <- var_names_long %>% filter(Use_newtree ==1) %>% pull(variables)

if(lag_temp >0){
  remove_cols <- c()
} else {
  remove_cols <- c('max_rh_lag','max_t2m_lag','max_tp_lag','min_rh_lag',
                   'min_t2m_lag', 'min_tp_lag','coldest_lag','julian_lag',
                   'sum_tp_lag','gdd_season_lag')
}


keep_col <- keep_col[keep_col %ni% remove_cols]

if(lag_temp >0){
  rf_train_lag0 <- train_lag0 %>% select('present',c(keep_col))  %>%
    mutate(present = ifelse(present == 1, 'present','absent')) %>%
    mutate(present = factor(present, levels = c('present','absent'))) %>% drop_na(julian) %>% drop_na(julian_lag)
} else{
  rf_train_lag0 <- train_lag0 %>% select('present',c(keep_col))  %>%
    mutate(present = ifelse(present == 1, 'present','absent')) %>%
    mutate(present = factor(present, levels = c('present','absent'))) %>% drop_na(julian)
}
find_nas(rf_train_lag0)

rf_train_lag0 <- train_lag0 %>% select('present',c(keep_col))  %>%
  mutate(present = ifelse(present == 1, 'present','absent')) %>%
  mutate(present = factor(present, levels = c('present','absent'))) %>% drop_na(julian)

set.seed <- 11
rf_model <- foreach(ntree = rep(num_trees_per_worker,num_workers), .combine = combine,
                   .packages = "randomForest") %dopar% {
                     randomForest(x = rf_train_lag0[-1],
                                  y = rf_train_lag0$present,
                                  ntree = ntree, keep.forest = TRUE,
                                  importance = TRUE, mtry = 7)
                   }

paste0("Finished fitting rf in ", round(difftime(Sys.time(), t0, units = 'mins'),2), " minutes")

save(rf_model, file = paste0('rf_output/rf_models1_lag', lag_temp,lag_pr,lag_rh, '.RData'))
paste0("Finished saving rf in ", round(difftime(Sys.time(), t0, units = 'mins'),2), " minutes")

test_all_lag0_rf <- test_all_lag0 %>%
  select(keep_col,present,lat,lon,lat_coord,lon_coord,dataset,period,source,manual_id) %>% drop_na()

test_all_lag0_rf$PA_pred <- predict(object=rf_model, newdata=test_all_lag0_rf, type = 'prob')[,1]

rfl0 <- test_all_lag0_rf %>% select(lat,lon,lat_coord,lon_coord,period,present,
                                    PA_pred,dataset,source,manual_id,year) %>% mutate(lag_pr = lag_pr,
                                                                                 lag_rh = lag_rh,
                                                                                 lag_temp = lag_temp)
rocl0 <- get_roc(rfl0[rfl0$dataset == 'testing',])
rocl0 <- rocl0 %>% mutate(lag_pr = lag_pr,
                          lag_rh = lag_rh,
                          lag_temp = lag_temp)


thresh <- rocl0[rocl0$maximized == 1,]$thresh

stats <- get_stats(rfl0[rfl0$dataset == 'testing',], thresh = thresh)

stats1 <- stats[[1]]
stats1 <- stats1 %>%  mutate(lag_pr = lag_pr,
                             lag_rh = lag_rh,
                             lag_temp = lag_temp)

stats2 <- stats[[2]]
stats2 <- stats2 %>%  mutate(lag_pr = lag_pr,
                             lag_rh = lag_rh,
                             lag_temp = lag_temp)

thresh_vals <- seq(0.0,1.0,0.025)

stats_thresh <- c()
nums_thresh <- c()
for(j in 1:length(thresh_vals)){
    temp <- get_stats_thresh(rfl0[rfl0$dataset == 'testing',], thresh = thresh_vals[j])
    
    temp_stats <- temp[[1]]
    temp_nums <- temp[[2]]
    
    temp_stats$thresh <- thresh_vals[j]
    temp_nums$thresh <- thresh_vals[j]
    
    stats_thresh <- rbind(stats_thresh,temp_stats)
    nums_thresh <- rbind(nums_thresh,temp_nums)
    
}

stats_thresh <- stats_thresh %>%  mutate(lag_pr = lag_pr,
                                         lag_rh = lag_rh,
                                         lag_temp = lag_temp)


nums_thresh <- nums_thresh %>%  mutate(lag_pr = lag_pr,
                                       lag_rh = lag_rh,
                                       lag_temp = lag_temp)

write_csv(stats1, paste0("rf_output5/stats_0_lag",lag_temp,lag_pr,lag_rh,".csv"))
write_csv(nums_thresh, paste0("rf_output5/nums_0_lag",lag_temp,lag_pr,lag_rh,".csv"))
write_csv(rocl0, paste0("rf_output5/roc_0_lag",lag_temp,lag_pr,lag_rh,".csv"))
write_csv(rfl0, paste0("rf_output5/predictions_0_lag",lag_temp,lag_pr,lag_rh,".csv"))
write_csv(stats_thresh, paste0("rf_output5/thresh_0_lag",lag_temp,lag_pr,lag_rh,".csv"))
write_csv(rf_train_lag0, paste0("rf_output5/training_0_lag",lag_temp,lag_pr,lag_rh,".csv"))

paste0("Finished script in ", round(difftime(Sys.time(), t0, units = 'mins'),2), " minutes")

closeCluster(cl)
mpi.quit()

print(paste0("Finished script in ", round(difftime(Sys.time(), t0, units = 'mins'),2), " minutes"))
