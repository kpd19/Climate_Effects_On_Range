library(tidyverse)
#library(ggbiplot)
library(gridExtra)
#library(mgcv)
library(geodata)
library(sf)
library(randomForest)
library(MESS)
library(pROC)
library(pdp)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/range_modeling/")

set.seed <- 11

`%ni%` <- Negate(`%in%`)

find_nas <- function(dataset){
  
  probs <- c()
  for(i in 1:dim(dataset)[2]){
    
    test <- sum(is.na(dataset[,i]))
    test2 <- sum(is.logical(dataset[,i]))
  
    test3 <- sum(test,test2)
    
    if (test3 > 0){
      probs <- c(probs,colnames(dataset)[i])
    }
  }
  
  if (length(probs)>0){
    print(paste0("List of problems: ", probs))
  } else{
    print("No problems in dataset found")
  }
}


canada_data = gadm(country="CAN", level = 1, path = tempdir())
bc_data = canada_data[canada_data$NAME_1 == "British Columbia", ]
bc_data =st_as_sf(bc_data)

us_data = gadm(country="USA", level = 1, path = tempdir())
state_data = us_data[us_data$NAME_1 %in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona", "New Mexico", "Utah", "Colorado","Montana","Wyoming"), ]
state_data = st_as_sf(state_data)

testing_dataset <- read_csv('data/testing_dataset_2006-2023.csv', guess_max = Inf)
training_dataset <- read_csv('data/training_dataset_1990-2005.csv', guess_max = Inf)

testing_dataset <- testing_dataset[!duplicated(testing_dataset),]
training_dataset <- training_dataset[!duplicated(training_dataset),]

testing_dataset <- testing_dataset %>% mutate(dataset = "testing", period = '2006-2023')
training_dataset <- training_dataset %>% mutate(dataset = "training", period = '1990-2005')

var_names_long <- read_csv("data/var_names_pa.csv")

ann_weather_stats <- read_csv("../climate/data/all_annual_1940-2100.csv")
ann_weather_stats <- ann_weather_stats %>% rename(year = year_tm)

lag0_weather <- ann_weather_stats %>%
  filter(year >= 1990, year <= 2023) %>% mutate(period = case_when(year <= 2005~ '1990-2005',
                                                                   year > 2005 ~'2006-2023')) %>% 
  group_by(lat,lon,period) %>% summarize(min_tp = mean(min_tp,na.rm=TRUE), max_tp = mean(max_tp,na.rm=TRUE),
                                         sum_tp = mean(sum_tp,na.rm=TRUE), gdd_season = mean(gdd_season,na.rm=TRUE),
                                         min_rh = mean(min_rh,na.rm=TRUE), max_rh = mean(max_rh,na.rm=TRUE),
                                         min_t2m = mean(min_t2m,na.rm=TRUE), max_t2m = mean(max_t2m,na.rm=TRUE),
                                         julian = mean(julian,na.rm=TRUE))

test_all <- rbind(training_dataset,testing_dataset)

test_all <- test_all %>% mutate_at(c('mean_biomass','max_biomass'), ~replace_na(.,0)) %>% 
  drop_na(elev2) %>% mutate(preferred = Abies + Pseudotsuga) %>% mutate(preferred_pres = ifelse(preferred >0,1,0)) %>% 
  select(-c(modis_id,short,Gymnospermae, Angiosperms))

test_all %>% drop_na(elev2) %>% ggplot() + aes(x = lon_coord, y = lat_coord, color = mean_biomass, fill = mean_biomass) + 
  geom_tile() + theme_classic(base_size = 15)

test_all_lag0 <- merge(test_all,lag0_weather, by.x = c('lat_coord','lon_coord','period'), by.y = c('lat','lon','period'))

train_lag0 <- test_all_lag0 %>% filter(track_early %in% c('in','near-1'), dataset == 'training')

train_lag0 %>% mutate(n = 1) %>% group_by(source) %>% count(n)

test_all_lag0 %>% mutate(n = 1) %>% group_by(source) %>% count(n)

colnames(train_lag0)[c(12:66,68:78)][colnames(test_all_lag0)[c(12:66,68:78)] %ni% var_names_long$variables]

################

keep_col <- colnames(train_lag0)[c(9,12:66,68:78)]

rf_train_lag0 <- train_lag0 %>% select(keep_col)  %>% mutate(present = ifelse(present == 1, 'present','absent')) %>% 
  mutate(present = factor(present, levels = c('present','absent')))

find_nas(rf_train_lag0)

pa_rf_lag0 <- randomForest(x = rf_train_lag0[-1],
                           y = rf_train_lag0$present,
                           ntree = 2000, keep.forest = TRUE, 
                           importance = TRUE)

#save(pa_rf_lag0, pa_rf_lag5, pa_rf_lag10,pa_rf_lag15,pa_rf_lag20,pa_rf_lag30,pa_rf_lag40, file = '_data/trained_rf_models_pref4.RData')
#load(file = '_data/trained_rf_models_pref4.RData')

test_all_lag0_rf <- test_all_lag0 %>% 
  select(keep_col,lat,lon,lat_coord,lon_coord,dataset,period,source,manual_id) %>% drop_na()

test_all_lag0_rf$PA_pred <- predict(object=pa_rf_lag0, newdata=test_all_lag0_rf, type = 'prob')[,1]

rfl0 <- test_all_lag0_rf %>% select(lat,lon,lat_coord,lon_coord,period,present,PA_pred,dataset,source,manual_id) %>% mutate(lag = 0)

rf_all <- rfl0

rf_all_thresh <- merge(rf_all, data.frame(thresh = seq(0.0,1.0,0.025)))

roc <- rf_all_thresh %>% filter(dataset =='testing') %>% mutate(present_name = ifelse(present == 1, 'present','absent'),
                                                             pred_name = ifelse(PA_pred >=thresh,'present','absent')) %>%
  group_by(present_name,lag,thresh) %>% count(pred_name) %>% 
  group_by(present_name,lag,thresh) %>% mutate(tot = sum(n), 
                                               p = n/tot) %>% 
  mutate(type = case_when(present_name == 'absent' & pred_name == 'absent' ~'true_negative', 
                          present_name == 'absent' & pred_name == 'present' ~'false_positive', 
                          present_name == 'present' & pred_name == 'absent' ~'false_negative', 
                          present_name == 'present' & pred_name == 'present' ~'true_positive')) %>%  
  ungroup() %>% 
  select(lag,thresh,p,type) %>% pivot_wider(names_from = 'type', values_from = 'p') %>% 
  mutate(TPR = true_positive/(true_positive + false_negative),
         FPR = false_positive/(false_positive + true_negative))

maximized <- roc %>% mutate(diff = abs(TPR-FPR)) %>% group_by(lag) %>% filter(diff == max(diff,na.rm=TRUE)) %>% arrange(desc(diff))

pdf("_plots/_rfpa4/roc_curve.pdf",height = 5, width = 7)
roc %>% 
  ggplot() + aes(x = FPR, y = TPR, color = as.factor(lag)) + 
  geom_point(size = 1)+ geom_line(size = 1) + theme_classic(base_size = 15) + 
  xlab("False Positive Rate") + ylab("True Positive Rate") +
  coord_cartesian(xlim = c(0,.65), ylim = c(0,1)) +
  geom_abline(intercept =0, slope = 1, linetype = 'dashed', color = 'grey55') +
  scale_color_brewer('Time Lag', palette = "Dark2") +
  ggtitle("Reciever operating characteristic (ROC)") +
  geom_point(data = maximized, aes(x = FPR,y = TPR), color = 'black', size = 2)
dev.off()

# auc_df <- c()
# for (i in c(0,5,10,15,20,30,40)){
#   dat <- roc %>% filter(lag == i) %>% arrange(FPR)
#   
#   auc_val <- auc(dat$FPR, dat$TPR, from = 0, to = 1, type = 'spline')
#   
#   temp <- data.frame(auc = auc_val, lag = i)
#   auc_df <- rbind(auc_df,temp)
#   
# }

auc_df <- c()
for (i in c(0)){
  ci_val <- ci.auc(rf_all[rf_all$dataset =='testing' & rf_all$lag == i,]$present, rf_all[rf_all$dataset =='testing'& rf_all$lag == i,]$PA_pred)
  auc_val <- auc(rf_all[rf_all$dataset =='testing'& rf_all$lag == i,]$present, rf_all[rf_all$dataset =='testing' & rf_all$lag == i,]$PA_pred)
  
  temp <- data.frame(auc = as.numeric(auc_val),ci_0.025 = as.numeric(ci_val)[1],ci_0.975 = as.numeric(ci_val)[3], lag = i)
  auc_df <- rbind(auc_df,temp)
  
}

thresh_df <- maximized %>% select(lag,thresh)

rf_all <- merge(rf_all,thresh_df)

#write_csv(rf_all,"/Volumes/My Book/Synchrony/presence/_rfmod2/predicted_presence_all_lags_pref4.csv")
#rf_all <- read_csv("/Volumes/My Book/Synchrony/presence/_rfmod2/predicted_presence_all_lags_pref4.csv")

#pdf("_plots/_rfpa4/confusion_matrix.pdf",height = 8, width = 10)
rf_all %>% filter(dataset =='testing') %>% mutate(present_name = ifelse(present == 1, 'present','absent'),
                                               pred_name = ifelse(PA_pred >=thresh,'present','absent')) %>%
  group_by(present_name,lag) %>% count(pred_name) %>% 
  group_by(present_name,lag) %>% mutate(tot = sum(n)) %>%  
  ggplot() + aes(x = present_name, y = pred_name, fill = n/tot, color = n/tot) + 
  geom_tile() + theme_classic(base_size = 15) + 
  geom_text(aes(x = present_name, y = pred_name, label = n), color = 'grey75', size = 10) +
  scale_color_viridis_c(option = 'viridis') +
  scale_fill_viridis_c(option = 'viridis') +
  xlab("Data") + ylab("Prediction") +
  theme(legend.position = 'none') +
  facet_wrap(~lag)
#dev.off()

scores <- rf_all %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
                            present_name = ifelse(present == 1, 'present','absent')) %>% 
  group_by(lag,dataset,present_name) %>% count(pred_class) %>% filter(dataset == 'testing') %>% 
  mutate(type = case_when(present_name == 'absent' & pred_class == 'absent' ~'true_negative', 
                          present_name == 'absent' & pred_class == 'present' ~'false_positive', 
                          present_name == 'present' & pred_class == 'absent' ~'false_negative', 
                          present_name == 'present' & pred_class == 'present' ~'true_positive')) %>% group_by(lag) %>% 
  mutate(tot = sum(n)) %>% 
  select(lag,tot,n,type) %>% pivot_wider(names_from = 'type', values_from = 'n')

accuracy <- scores %>% summarize(acc = (true_positive + true_negative)/(tot))

recall_beta <- c()
precision_beta <- c()
acc_beta <- c()
for (i in c(0)){
  lag_vals <- rf_all %>% filter(dataset == 'testing', lag == i) %>% drop_na(present) %>% mutate(pred_num = ifelse(PA_pred >= thresh,1,0)) %>%  
    mutate(type = case_when(present == 0 & pred_num == 0 ~'true_negative', 
                            present == 0 & pred_num == 1 ~'false_positive', 
                            present == 1 & pred_num == 0 ~'false_negative', 
                            present == 1 & pred_num == 1 ~'true_positive'))
  
  success_type <- lag_vals %>% count(type) %>% pivot_wider(names_from = c('type'), values_from = c('n'))
  
  beta_precision <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + 1), shape2 = (success_type$false_positive + 1))
  beta_recall <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + 1), shape2 = (success_type$false_negative + 1))
  beta_acc <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + success_type$true_negative+ 1), shape2 = (success_type$false_positive + success_type$false_negative + 1))
  
  recall_temp <- data.frame(lag = i, b1 = beta_recall[1],b2 = beta_recall[2])
  precision_temp <- data.frame(lag = i, b1 = beta_precision[1],b2 = beta_precision[2])
  beta_temp <- data.frame(b1 = beta_acc[1],b2 = beta_acc[2])
  
  recall_beta <- rbind(recall_beta,recall_temp)
  precision_beta <- rbind(precision_beta,precision_temp)
  acc_beta <- rbind(acc_beta,beta_temp)
  
}

recall_df <- scores %>% summarize(recall = true_positive/(true_positive + false_negative),
                                  tot = (true_positive + false_negative)) %>% 
  mutate(ci1 = recall - (1.96*sqrt(recall*(1-recall)/tot)),
         ci2 = recall + (1.96*sqrt(recall*(1-recall)/tot)))

precision_df <- scores %>% summarize(precision = true_positive/(true_positive + false_positive),
                                     tot = (true_positive + false_positive)) %>% 
  mutate(ci1 = precision - (1.96*sqrt(precision*(1-precision)/tot)),
         ci2 = precision + (1.96*sqrt(precision*(1-precision)/tot)))

accuracy_df <- scores %>% summarize(acc = (true_positive + true_negative)/(true_positive + true_negative + false_positive + false_negative),
                                     tot = (true_positive + true_negative + false_positive + false_negative)) %>% 
  mutate(ci1 = acc - (1.96*sqrt(acc*(1-acc)/tot)),
         ci2 = acc + (1.96*sqrt(acc*(1-acc)/tot)))

recall_plt <- recall_df %>% ggplot() + aes(x = as.factor(lag), y = recall) + geom_point() + theme_classic(base_size = 15) +
  geom_errorbar(aes(ymin = ci1, ymax = ci2)) + 
  xlab("Weather lag (years)") + ylab("Recall")
precision_plt <- precision_df %>% ggplot() + aes(x = as.factor(lag), y = precision) + geom_point() + theme_classic(base_size = 15) +
  geom_errorbar(aes(ymin = ci1, ymax = ci2)) + 
  xlab("Weather lag (years)") + ylab("Precision")
acc_plt <- accuracy_df %>% ggplot() + aes(x = as.factor(lag), y = acc) + geom_point() + theme_classic(base_size = 15) +
  geom_errorbar(aes(ymin = ci1, ymax = ci2)) + 
  xlab("Weather lag (years)") + ylab("Accuracy")
auc_plt <- auc_df %>% ggplot() + aes(x = as.factor(lag), y = auc) + geom_point() + theme_classic(base_size = 15) +
  geom_errorbar(aes(ymin = ci_0.025, ymax = ci_0.975)) + 
  xlab("Weather lag (years)") + ylab("AUC-ROC")

#pdf("_plots/_rfpa4/performance_metrics.pdf",height = 6, width = 8)
grid.arrange(auc_plt,acc_plt,recall_plt,precision_plt)
#dev.off()

precision_all <- merge(precision_beta,precision_df)
recall_all <- merge(recall_beta,recall_df)

accuracy_df <- scores %>% mutate(acc = (true_positive + true_negative)/(tot)) %>% 
  mutate(ci1 = acc - (1.96*sqrt(acc*(1-acc)/tot)),
         ci2 = acc + (1.96*sqrt(acc*(1-acc)/tot))) %>% 
  select(lag,acc,ci1,ci2)

precision_all <- precision_all %>% select(lag,precision,b1,b2,ci1,ci2)
recall_all <- recall_all %>% select(lag,recall,b1,b2,ci1,ci2)

all_metrics <- merge(auc_df,accuracy)
all_metrics <- merge(all_metrics,recall_df)
all_metrics <- merge(all_metrics,precision_df)
all_metrics <- merge(all_metrics,thresh_df)

all_metrics <- all_metrics %>% arrange(lag)

write_csv(all_metrics,"/Volumes/My Book/Synchrony/presence/_rfmod2/model_metrics.csv")
all_metrics <- read_csv("/Volumes/My Book/Synchrony/presence/_rfmod2/model_metrics.csv")


accuracy %>% arrange(desc(acc))
recall_df %>% arrange(desc(recall))
precision_df %>% arrange(desc(precision))

rf_all %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
                  present_name = ifelse(present == 1, 'present','absent'),
                  synth = ifelse(source %in% c("Synthetic data","Synthetic"),'synthetic','population')) %>% 
  group_by(lag,dataset,present_name,synth) %>% count(pred_class) %>% filter(dataset == 'testing') %>% 
  mutate(type = case_when(present_name == 'absent' & pred_class == 'absent' ~'true_negative', 
                          present_name == 'absent' & pred_class == 'present' ~'false_positive', 
                          present_name == 'present' & pred_class == 'absent' ~'false_negative', 
                          present_name == 'present' & pred_class == 'present' ~'true_positive')) %>% group_by(lag,synth) %>% 
  mutate(tot = sum(n)) %>% 
  select(lag,tot,n,type) %>% pivot_wider(names_from = 'type', values_from = 'n') %>% filter(lag == 0)


rf_all %>% filter(lag == 0, source %ni% c("Synthetic data","Synthetic",'Pheromone trapping'),
                  dataset == 'test', present == 0, PA_pred >= thresh) %>% 
  ggplot() + aes(x = lon_coord, y = lat_coord) + geom_tile() + theme_classic(base_size = 15)

var_imp_lag0 <- data.frame(importance(pa_rf_lag0))
var_imp_lag0$variables <- rownames(var_imp_lag0)
var_imp_lag0 <- var_imp_lag0 %>% mutate(lag = 0)

var_imp_all <- rbind(var_imp_lag0)

var_imp_all <- merge(var_names_long,var_imp_all)

#write_csv(var_imp_all,"/Volumes/My Book/Synchrony/presence/_rfmod2/variable_importance3.csv")
#write_csv(var_names1,"/Volumes/My Book/Synchrony/presence/_rfmod2/var_names_pa.csv")
#var_imp_all <- read_csv("/Volumes/My Book/Synchrony/presence/_rfmod2/variable_importance3.csv")

pdf("_plots/_rfpa4/variable_importance_gini.pdf",height = 10, width = 15)
var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseGini) %>% filter(MeanDecreaseGini>0) %>% 
  ggplot(aes(x=reorder(name, MeanDecreaseGini), y=MeanDecreaseGini, color = type, fill = type)) + 
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

var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseAccuracy) %>% filter(MeanDecreaseAccuracy>0) %>% 
  ggplot(aes(x=reorder(name, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, color = type, fill = type)) + 
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
  theme(legend.position = 'top')
dev.off()

zero_vars <- var_imp_all %>% mutate(lag_name = paste0('lag = ',lag)) %>% 
  mutate(lag_name = factor(lag_name, levels = c('lag = 0','lag = 5','lag = 10','lag = 15','lag = 20','lag = 30','lag = 40'))) %>% 
  group_by(lag) %>% 
  arrange(MeanDecreaseGini) %>% filter(MeanDecreaseGini==0) %>% pull(variables)

unique(zero_vars)

mean_lags <- rf_all %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0),
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

# diff_pres <- ggplot() + geom_tile(data = mean_lags,
#                                   aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
#   scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
#                                     'False negative' = fn_col, 'False positive' = fp_col)) +
#   scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
#                                    'False negative' = fn_col, 'False positive' = fp_col))+ 
#   xlab("Longitude") + ylab("Latitude") +
#   facet_wrap(~lag)

plt_diff <- diff_pres + geom_sf(data = state_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = bc_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top')


imp_vars <- var_imp_all %>% filter(lag == 0) %>% arrange(desc(MeanDecreaseGini)) %>% head(20) %>% pull(variables)

imp_vars_df2 <- var_imp_all %>% filter(lag == 0) %>% arrange(desc(MeanDecreaseGini)) %>% head(20)

temp <- partial(pa_rf_lag0, pred.var = imp_vars[1])
colnames(temp) <- c("value",'yhat')
temp$param <- imp_vars[1]

temp %>% ggplot() + aes(x = value, y = yhat) + geom_point() + theme_classic()

all_pds <- rbind(all_pds,temp)
print(i)

all_pds <- c()
for(i in 1:length(imp_vars)){
  temp <- partial(pa_rf_lag20, pred.var = imp_vars[i])
  colnames(temp) <- c("value",'yhat')
  temp$param <- imp_vars[i]
  
  all_pds <- rbind(all_pds,temp)
  print(i)
}

#write_csv(all_pds,"/Volumes/My Book/Synchrony/presence/_rfmod2/partial_dependence.csv")
all_pds <- read_csv("/Volumes/My Book/Synchrony/presence/_rfmod2/partial_dependence.csv")

all_rugs <- c()
for(i in 1:length(imp_vars)){
  
  temp_vals <- train_lag20 %>% pull(imp_vars[i])
  
  probs <- data.frame(q = seq(0,1,0.1), value = as.numeric(quantile(temp_vals, prob = seq(0,1,0.1))))
  probs$param <- imp_vars[i]
  
  all_rugs <- rbind(all_rugs,probs)
  print(i)
}

all_rugs <- merge(all_rugs,var_names_long,by.x = c('param'),by.y = c('variables'))
all_rugs <- all_rugs %>% mutate(name = factor(name, levels = imp_vars_df2$name)) 

all_pds2 <- merge(all_pds,variable_class, by.x = c('param'),by.y = c('variables'))
all_pds2 <- merge(all_pds2,var_names_long, by.x = c('param','type'),by.y = c('variables','type'))
all_pds2 <- all_pds2 %>% mutate(name = factor(name, levels = imp_vars_df2$name)) 

#pdf("_plots/_rfpa4/partial_dependence_pa.pdf", height = 10, width = 16)
all_pds2 %>% filter(param %ni% c('near_needle','preferred_pres')) %>%
  ggplot() + aes(x = value, y = yhat, color = type) + geom_line(size = 1.5) + theme_classic(base_size = 15) + 
  facet_wrap(~name, scales = 'free_x',nrow = 3) + 
  ylab("Partial dependence") + 
  geom_segment(data = all_rugs[all_rugs$param %ni% c('near_needle','preferred_pres'),],
               aes(x = value, y = -1.3, yend = -1.1), color = 'black') +
  scale_color_brewer("", palette = "Set1")+
  theme(legend.position = 'top')
#dev.off()

partial(pa_rf_lag0, pred.var = c("min_tp","max_tp"))

m <- pa_rf_lag0
x <- rf_train_lag0
xname <- "min_tp"
lci <- 0.25
uci <- 0.75
delta <- TRUE

pd <- partial(pa_rf_lag0, pred.var = xname)
colnames(pd) <- c("value",'yhat')
pd$param <- xname

conf.int <-(uci-lci)*100
temp <- pd$value
y.hat.mean <- vector()
y.hat.lb <- vector()
y.hat.ub <- vector()
y <- stats::predict(m, x, type = 'prob')
for (i in 1:length(temp)){
  xtemp <- x
  xtemp[, xname] <- temp[i]
  y.hat <- stats::predict(m, xtemp, type = 'prob')
  if (delta == TRUE){ y.hat <- y.hat - y }
  y.hat.mean[i] <- stats::weighted.mean(y.hat)
  y.hat.lb[i] <- stats::quantile(y.hat, lci)
  y.hat.ub[i] <- stats::quantile(y.hat, uci)
}
m.ci <- as.data.frame(cbind(temp, y.hat.mean, y.hat.lb, y.hat.ub))
colnames(m.ci) <- c('value','yhat2','lower','upper')

pd2 <- merge(m.ci,pd)

pd2 %>% ggplot() + geom_line(aes(x = value, y = upper), linetype = 'dashed') + 
  geom_line(aes(x = value, y = lower), linetype = 'dashed') +
  geom_line(aes(x = value, y = yhat), color = 'blue', size = 2)

multiclass_logit <- function(x, which.class = 1L) {
  if (is.data.frame(x)) {
    x <- data.matrix(x)
  }
  stopifnot(is.matrix(x))  # x should be a nclass by n probability matrix
  eps <- .Machine$double.eps
  log(ifelse(x[, which.class] > 0, x[, which.class], eps)) -
    rowMeans(log(ifelse(x > 0, x, eps)))
}

a <- multiclass_logit(y, which.class = 'present')

plot(x$min_tp,a)

get_probs.RandomForest <- function(object, newdata, which.class, logit, ...) {
  pr <- stats::predict(object, newdata = newdata, type = "prob", ...)
  if (isTRUE(logit)) {
    multiclass_logit(do.call(rbind, pr), which.class = which.class)
  } else {
    do.call(rbind, pr)[, which.class]
  }
}
