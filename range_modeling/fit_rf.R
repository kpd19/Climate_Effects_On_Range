library(tidyverse)
#library(ggbiplot)
library(gridExtra)
#library(mgcv)
library(geodata)
library(sf)
library(randomForest)
library(MESS)
library(pROC)
#library(pdp)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/range_modeling/")

source("fitting_functions.R")

set.seed <- 11

canada_data = gadm(country="CAN", level = 1, path = tempdir())
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
bc_data =st_as_sf(bc_data)

us_data = gadm(country="USA", level = 1, path = tempdir())
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
state_data = st_as_sf(state_data)

testing_dataset <- read_csv('data/testing_dataset_2006-2024.csv', guess_max = Inf)
training_dataset <- read_csv('data/training_dataset_1990-2005.csv', guess_max = Inf)

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

habitat_features2 %>% filter(is.na(aspect)) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() + theme_classic()

testing_dataset <- testing_dataset[!duplicated(testing_dataset),]
training_dataset <- training_dataset[!duplicated(training_dataset),]

testing_dataset <- testing_dataset %>% 
  select(lat,lon,source,manual_id,lat_coord,lon_coord,n_obs,present,track_early,track_late) %>% 
  mutate(dataset = "testing", period = '2006-2024')%>% mutate(lon = round(lon,5),
                                                              lat = round(lat,5))
training_dataset <- training_dataset %>% 
  select(lat,lon,source,manual_id,lat_coord,lon_coord,n_obs,present,track_early,track_late) %>% 
  mutate(dataset = "training", period = '1990-2005')%>% mutate(lon = round(lon,5),
                                                               lat = round(lat,5))

testing_dataset2 <- merge(testing_dataset, habitat_features2, all.x = TRUE)
training_dataset2 <- merge(training_dataset, habitat_features2, all.x = TRUE)

testing_dataset2 %>% filter(is.na(elev2)) %>% 
  ggplot() + aes(x = lon,y = lat, color = source) + geom_point() + theme_classic()

testing_dataset2 %>% filter(manual_id == 48938)

testing_dataset2 %>% drop_na(host_trees) %>% 
  ggplot() + aes(x = lon,y = lat, color = host_trees2) + geom_point() + theme_classic() + 
  scale_color_viridis_c(option = 'turbo')

ann_weather_stats <- read_csv("../climate/data/all_annual_1940-2024_2.csv")

ann_weather_stats <- ann_weather_stats %>% rename(year = year_tm)

lag0_weather <- ann_weather_stats %>% rename(lon = longitude, lat = latitude) %>% 
  filter(year >= 1990, year <= 2024) %>% mutate(period = case_when(year <= 2005~ '1990-2005',
                                                                   year > 2005 ~'2006-2024')) %>% 
  group_by(lat,lon,period) %>% summarize(min_tp = mean(min_tp,na.rm=TRUE), max_tp = mean(max_tp,na.rm=TRUE),
                                         sum_tp = mean(sum_tp,na.rm=TRUE), gdd_season = mean(gdd_season,na.rm=TRUE),
                                         min_rh = mean(min_rh,na.rm=TRUE), max_rh = mean(max_rh,na.rm=TRUE),
                                         min_t2m = mean(min_t2m,na.rm=TRUE), max_t2m = mean(max_t2m,na.rm=TRUE),
                                         julian = mean(julian,na.rm=TRUE),
                                         coldest = mean(coldest,na.rm = TRUE))

test_all <- rbind(training_dataset2,testing_dataset2)

test_all <- test_all %>% mutate_at(c('mean_biomass','max_biomass'), ~replace_na(.,0)) %>% 
  drop_na(elev2) %>% 
  select(-c(modis_id,short))

test_all[test_all$near_needle == 0 & test_all$host_trees == 1,]$near_needle <- 1

test_all %>% drop_na(elev2) %>% ggplot() + aes(x = lon_coord, y = lat_coord, color = mean_biomass, fill = mean_biomass) + 
  geom_tile() + theme_classic(base_size = 15)

test_all_lag0 <- merge(test_all,lag0_weather, by.x = c('lat_coord','lon_coord','period'), by.y = c('lat','lon','period'))

train_lag0 <- test_all_lag0 %>% filter(track_early %in% c('in','near-1'), dataset == 'training')

train_lag0 %>% mutate(n = 1) %>% group_by(source) %>% count(n)

test_all_lag0 %>% mutate(n = 1) %>% group_by(source) %>% count(n)

train_lag0 %>% filter(host_trees == 1, near_needle == 0)

train_lag0 %>% ggplot() + aes(x = lon, y = lat, color = present) + geom_point() + 
  theme_classic() + facet_wrap(~dataset)

################

var_names_long <- read_csv("data/var_names_pa2.csv")

keep_col <- var_names_long %>% filter(Use ==1) %>% pull(variables)

remove_cols <- c()

keep_col <- keep_col[keep_col %ni% remove_cols]

rf_train_lag0 <- train_lag0 %>% select('present',c(keep_col))  %>% mutate(present = ifelse(present == 1, 'present','absent')) %>% 
  mutate(present = factor(present, levels = c('present','absent')))

find_nas(rf_train_lag0)

pa_rf_lag0 <- randomForest(x = rf_train_lag0[-1],
                           y = rf_train_lag0$present,
                           ntree = 4000, keep.forest = TRUE, 
                           importance = TRUE)


save(pa_rf_lag0, file = 'output/rf_models1.RData')

test_all_lag0_rf <- test_all_lag0 %>% 
  select(keep_col,present,lat,lon,lat_coord,lon_coord,dataset,period,source,manual_id) %>% drop_na()

test_all_lag0_rf$PA_pred <- predict(object=pa_rf_lag0, newdata=test_all_lag0_rf, type = 'prob')[,1]

rfl0 <- test_all_lag0_rf %>% select(lat,lon,lat_coord,lon_coord,period,present,PA_pred,dataset,source,manual_id) %>% mutate(lag = 0)
roc1 <- get_roc(rfl0[rfl0$dataset == 'testing',])

pdf("figures/roc_curve.pdf",height = 5, width = 7)
roc1 %>% 
  ggplot() + aes(x = FPR, y = TPR, color = as.factor(lag)) + 
  geom_point(size = 1)+ geom_line(size = 1) + theme_classic(base_size = 15) + 
  xlab("False Positive Rate") + ylab("True Positive Rate") +
  coord_cartesian(xlim = c(0,.65), ylim = c(0,1)) +
  geom_abline(intercept =0, slope = 1, linetype = 'dashed', color = 'grey55') +
  scale_color_brewer('Time Lag', palette = "Dark2") +
  ggtitle("Reciever operating characteristic (ROC)") +
  geom_point(data = roc1[roc1$maximized == 1,], aes(x = FPR,y = TPR), color = 'black', size = 2)
dev.off()

thresh <- roc1[roc1$maximized == 1,]$thresh

get_stats(rfl0[rfl0$dataset == 'testing',], thresh = thresh, lag = 0)

var_imp_lag0 <- data.frame(importance(pa_rf_lag0))
var_imp_lag0$variables <- rownames(var_imp_lag0)
var_imp_lag0 <- var_imp_lag0 %>% mutate(lag = 0)

var_imp_all <- rbind(var_imp_lag0)

var_imp_all <- merge(var_names_long,var_imp_all)

write_csv(var_imp_all,"output/variable_importance.csv")

pdf("figures/variable_importance.pdf",height = 10, width = 15)
plt1 <- var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseGini) %>% filter(MeanDecreaseGini>0) %>% 
  ggplot(aes(x=reorder(name, MeanDecreaseGini), y=MeanDecreaseGini, color = category, fill = category)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  ylab('Mean Decrease in Gini Index') + xlab("Variables") +
  theme_minimal(base_size = 15) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20)) + 
  facet_grid(~lag_name) +
  scale_color_brewer("", palette = "Set1")+
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = 'top')


plt2 <- var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseAccuracy) %>% filter(MeanDecreaseAccuracy>0) %>% 
  ggplot(aes(x=reorder(name, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, color = category, fill = category)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  ylab('Mean Decrease in Accuracy') + xlab("Variables") +
  theme_minimal(base_size = 15) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20)) + 
  facet_grid(~lag_name) +
  scale_color_brewer("", palette = "Set1")+
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = 'none')
grid.arrange(plt1,plt2)

dev.off()


acc_ranking <- var_imp_all %>% arrange(desc(MeanDecreaseAccuracy)) %>% mutate(rank = length(var_imp_all$variables):1) %>% 
  mutate(var = "Accuracy")
gini_ranking <- var_imp_all %>% arrange(desc(MeanDecreaseGini)) %>% mutate(rank = length(var_imp_all$variables):1) %>% 
  mutate(var = "Gini")

ranking <- rbind(acc_ranking,gini_ranking)
ranking %>% ggplot() + aes(x= var, y = rank, color = category, fill = category, group = variables) +
  geom_line() + geom_point() + 
  theme_classic() +
  scale_color_brewer("", palette = "Set1")+
  scale_fill_brewer("", palette = "Set1") +
  geom_text(data = acc_ranking, aes(x = 1 - 0.05, y = rank, label = name, hjust = 1)) +
  geom_text(data = gini_ranking, aes(x = 2 + 0.05, y = rank, label = name, hjust = 0))
  

zero_vars <- var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseGini) %>% filter(MeanDecreaseGini==0) %>% pull(variables)

mean_lags <- rfl0 %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0),
                               synth = ifelse(source %in% c("Synthetic data","Synthetic"),'synthetic','population')) %>%
  filter(dataset == 'testing') %>% group_by(lon_coord,lat_coord,lag,synth) %>%
  summarize(mean_pred = mean(PA_group),
            mean_presence = mean(present)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0),
         pres = ifelse(mean_presence > 0,1,0)) %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'False negative',
                            bin == 0 & pres == 0 ~ 'True negative',
                            bin == 1 & pres == 0 ~ 'False positive',
                            bin == 1 & pres == 1 ~ 'True positive')) %>% drop_na(change)


mean_lags <- mean_lags %>% mutate(change = ifelse(change == 'False positive' & synth == 'synthetic', 'False positive- no traps', change),
                                  change = ifelse(change == 'False positive' & synth == 'population', 'False positive- traps', change))

mean_lags %>% ungroup() %>% count(change)

tn_col = 'grey5'
tp_col = '#1E88E5'
fn_col = 'red'
fp1_col = '#FFC107'
fp2_col = '#7B4578'

diff_pres <- ggplot() + geom_tile(data = mean_lags[mean_lags$lag == 0,],
                                  aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive- traps' = fp1_col, 'False positive- no traps' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive- traps' = fp1_col, 'False positive- no traps' = fp2_col))+ 
  xlab("Longitude") + ylab("Latitude") 

plt_diff <- diff_pres + geom_sf(data = state_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = bc_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top')

pdf("figures/testing_data_proj.pdf")
plt_diff
dev.off()


var_imp_all %>% ggplot() + aes(x = MeanDecreaseGini, y = MeanDecreaseAccuracy) + geom_point() + theme_classic()

imp_vars <- var_imp_all %>% filter(lag == 0, MeanDecreaseGini >0) %>% arrange(desc(MeanDecreaseGini)) %>% head(20)

test_all_lag0_rf %>% 
  mutate(present_name = ifelse(present == 1, 'present','absent'),
         pred_name = ifelse(PA_pred >=thresh,'present','absent')) %>%
  mutate(type = case_when(present_name == 'absent' & pred_name == 'absent' ~'true_negative', 
                          present_name == 'absent' & pred_name == 'present' ~'false_positive', 
                          present_name == 'present' & pred_name == 'absent' ~'false_negative', 
                          present_name == 'present' & pred_name == 'present' ~'true_positive')) %>%  
  filter(period == '2006-2024', type %in% c('true_positive','false_negative')) %>% 
  pivot_longer(cols = imp_vars$variables[1:20], names_to = 'name',values_to = 'variable') %>% 
  ggplot() + aes(x = variable, group = type, color = type, fill = type) + geom_density(alpha = 0.75) +
  theme_classic()+
  facet_wrap(~name, scales = 'free') 
  
  
  
imp_vars <- var_imp_all %>% filter(lag == 0) %>% arrange(desc(MeanDecreaseGini)) %>% filter() %>% head(20)

a.time <- Sys.time()
all_pds <- c()
for(i in 1:length(imp_vars$variables)){
  
  temp <- get_conf_int(pa_rf_lag0, rf_train_lag0,imp_vars$variables[i],imp_vars$type[i], mod = 'rf')
  temp$param <- imp_vars$variables[i]
  
  all_pds <- rbind(all_pds,temp)
  print(i)
  b.time <- Sys.time()
  c.time <- b.time - a.time
  print(paste0("Finished ", i, " in: ", c.time))
}

write_csv(all_pds,"output/partial_dependence_lag0.csv")

all_rugs <- c()
for(i in 1:length(imp_vars$variables)){
  
  temp_vals <- train_lag0 %>% pull(imp_vars$variables[i])
  
  probs <- data.frame(q = seq(0,1,0.1), value = as.numeric(quantile(temp_vals, prob = seq(0,1,0.1))))
  probs$param <- imp_vars$variables[i]
  
  all_rugs <- rbind(all_rugs,probs)
  print(i)
}

all_rugs2 <- merge(all_rugs,var_names_long,by.x = c('param'),by.y = c('variables'))
all_rugs2 <- all_rugs2 %>% mutate(name = factor(name, levels = imp_vars$name)) 

all_pds2 <- merge(all_pds,var_names_long, by.x = c('param'),by.y = c('variables'))
all_pds2 <- all_pds2 %>% mutate(name = factor(name, levels = imp_vars$name)) 

all_pds2$training <- '1990-2005'

pdf("figures/partial_dependence_pa.pdf", height = 10, width = 16)
all_pds2 %>% 
  ggplot() + #geom_ribbon(aes(x = value, ymin = lb3, ymax = ub3), linetype = 'dashed',
                         #fill = 'skyblue1', linetype = 'dashed', color = 'grey55', alpha = 0.5) +
  #geom_line(aes(x = value, y = ub3), linetype = 'dashed') +
  geom_line(aes(x = value, y = yhat), color = 'navyblue',size = 1.5) +
  theme_classic(base_size = 15) + 
  facet_wrap(~name, scales = 'free_x',nrow = 3) + 
  ylab("Partial dependence") + 
  #geom_segment(data = all_rugs2,
  #             aes(x = value, y = -0.1, yend = 0), color = 'black') +
  scale_color_brewer("", palette = "Set1")+
  theme(legend.position = 'top') 
dev.off()


ann_weather_stats %>% ggplot() + aes(x = year, y= gdd_season) + geom_point() +
  theme_classic()

