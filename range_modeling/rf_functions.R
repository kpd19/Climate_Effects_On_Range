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

get_roc <- function(df){
  df <- merge(df, data.frame(thresh = seq(0.0,1.0,0.025)))
  
  roc <- df %>% 
    mutate(present_name = ifelse(present == 1, 'present','absent'),
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
    mutate_at(c('false_positive','false_negative','true_positive','true_negative'), ~replace_na(.,0)) %>% 
    mutate(TPR = true_positive/(true_positive + false_negative),
           FPR = false_positive/(false_positive + true_negative))
  
  maximized <- roc %>% mutate(diff = abs(TPR-FPR)) %>% group_by(lag) %>%
    filter(diff == max(diff,na.rm=TRUE)) %>% arrange(desc(diff)) %>% pull(thresh)
  
  roc$maximized <- 0
  roc[roc$thresh == maximized,]$maximized <- 1
  
  return(roc)
  
}

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


get_stats <- function(df, thresh, lag){
  ci_val <- ci.auc(df$present, df$PA_pred)
  auc_val <- auc(df$present, df$PA_pred)
  
  auc_df <- data.frame(metric = "AUC", estimate = as.numeric(auc_val),
                       ci1 = as.numeric(ci_val)[1],ci2 = as.numeric(ci_val)[3], b1 = NA, b2 = NA)

  scores <- df %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
                              present_name = ifelse(present == 1, 'present','absent')) %>% 
    group_by(present_name) %>% count(pred_class) %>% 
    mutate(type = case_when(present_name == 'absent' & pred_class == 'absent' ~'true_negative', 
                            present_name == 'absent' & pred_class == 'present' ~'false_positive', 
                            present_name == 'present' & pred_class == 'absent' ~'false_negative', 
                            present_name == 'present' & pred_class == 'present' ~'true_positive')) %>% ungroup() %>% 
    mutate(tot = sum(n)) %>% 
    select(tot,n,type) %>% pivot_wider(names_from = 'type', values_from = 'n')
  
  lag_vals <- df %>% drop_na(present) %>% mutate(pred_num = ifelse(PA_pred >= thresh,1,0)) %>%  
    mutate(type = case_when(present == 0 & pred_num == 0 ~'true_negative', 
                            present == 0 & pred_num == 1 ~'false_positive', 
                            present == 1 & pred_num == 0 ~'false_negative', 
                            present == 1 & pred_num == 1 ~'true_positive'))
    
  success_type <- lag_vals %>% count(type) %>% pivot_wider(names_from = c('type'), values_from = c('n'))
  
  beta_precision <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + 1), shape2 = (success_type$false_positive + 1))
  beta_recall <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + 1), shape2 = (success_type$false_negative + 1))
  beta_acc <- qbeta(p = c(0.025,0.975),shape1 = (success_type$true_positive + success_type$true_negative+ 1), shape2 = (success_type$false_positive + success_type$false_negative + 1))
  
  recall_beta <- data.frame(b1 = beta_recall[1],b2 = beta_recall[2])
  precision_beta <- data.frame(b1 = beta_precision[1],b2 = beta_precision[2])
  acc_beta <- data.frame(b1 = beta_acc[1],b2 = beta_acc[2])
  
  recall_df <- scores %>% summarize(recall = true_positive/(true_positive + false_negative),
                                    tot = (true_positive + false_negative)) %>% 
    mutate(ci1 = recall - (1.96*sqrt(recall*(1-recall)/tot)),
           ci2 = recall + (1.96*sqrt(recall*(1-recall)/tot)))
  
  precision_df <- scores %>% summarize(precision = true_positive/(true_positive + false_positive),
                                       tot = (true_positive + false_positive)) %>% 
    mutate(ci1 = precision - (1.96*sqrt(precision*(1-precision)/tot)),
           ci2 = precision + (1.96*sqrt(precision*(1-precision)/tot)))
  
  accuracy_df <- scores %>% mutate(acc = (true_positive + true_negative)/(tot)) %>% 
    mutate(ci1 = acc - (1.96*sqrt(acc*(1-acc)/tot)),
           ci2 = acc + (1.96*sqrt(acc*(1-acc)/tot))) %>% 
    select(acc,ci1,ci2)

  precision_all <- merge(precision_df,precision_beta)
  recall_all <- merge(recall_df,recall_beta)
  acc_all <- merge(accuracy_df,acc_beta)
  
  precision_all <- precision_all %>% select(precision,b1,b2,ci1,ci2) %>% rename(estimate = precision) %>% 
    mutate(metric = "precision")
  recall_all <- recall_all %>% select(recall,b1,b2,ci1,ci2) %>% rename(estimate = recall) %>% 
    mutate(metric = "recall")
  acc_all <- acc_all %>% select(acc,b1,b2,ci1,ci2)%>% rename(estimate = acc) %>% 
    mutate(metric = "accuracy")
  
  all_metrics <- rbind(auc_df,recall_all, acc_all, precision_all)
  all_metrics$thresh <- thresh
  all_metrics$lag <- lag
  
  return(list(all_metrics,scores))
  
  }


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

get_conf_int <- function(m, x, xname, type){
  
  if (type == 'numeric'){
    pvals <- seq(min(x[,xname]), max(x[,xname]),length.out = 51)
  } else if (type == 'categorical'){
    pvals <- unique(x[,xname])    
  }
  
  y.hat.mean <- vector()
  y.hat.lb1 <- vector()
  y.hat.ub1 <- vector()
  y.hat.lb2 <- vector()
  y.hat.ub2 <- vector()
  y.hat.lb3 <- vector()
  y.hat.ub3 <- vector()
  for (i in 1:length(pvals)){
    xtemp <- x
    xtemp[, xname] <- pvals[i]
    y.hat <- stats::predict(m, xtemp, type = 'prob')[,1]
    y.hat.mean[i] <- stats::weighted.mean(y.hat)
    y.hat.lb1[i] <- stats::quantile(y.hat, 0.025)
    y.hat.lb2[i] <- stats::quantile(y.hat, 0.1)
    y.hat.lb3[i] <- stats::quantile(y.hat, 0.25)
    y.hat.ub3[i] <- stats::quantile(y.hat, 0.75)
    y.hat.ub2[i] <- stats::quantile(y.hat, 0.9)
    y.hat.ub1[i] <- stats::quantile(y.hat, 0.975)
    
  }
  m.ci <- as.data.frame(cbind(pvals, y.hat.mean, y.hat.lb1, y.hat.lb2, y.hat.lb3, y.hat.ub1,y.hat.ub2,y.hat.ub3))
  colnames(m.ci) <- c('value','yhat','lb1','lb2','lb3','ub1','ub2','ub3')
  
  return(m.ci)
  
}

