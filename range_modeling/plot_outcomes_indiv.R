library(tidyverse)
library(gridExtra)
library(scale)
library(cowplot)

`%ni%` <- Negate(`%in%`)

thresh <- 0.325
preds <- read_csv("/Volumes/My Book/Synchrony/rf_lags3/predictions/predictions_0_lag353535.csv")

all_dataset2 <- read_csv('data/populations_for_cc_proj.csv')
avg5_historical <- read_csv('data/avg5_weather_1940-2025.csv')

both_avg <- read_csv("../model_comparison/data/compare_four_avg_rank.csv")

pop_dens <- read_csv("../anthropogenic/data/population_density.csv")
trav_time <- read_csv("../anthropogenic/data/min_travel_time_1-5.csv")
ownership <- read_csv("../anthropogenic/data/forest_ownership.csv")

agency_can_state <- read_csv("../anthropogenic/data/ll_can_distances_state.csv")
agency_can_fed <- read_csv("../anthropogenic/data/ll_can_distances_fed.csv")
agency_usa_state <- read_csv("../anthropogenic/data/ll_usa_distances_state.csv")
agency_usa_fed <- read_csv("../anthropogenic/data/ll_usa_distances_federal.csv")

tn_col = 'grey5'
tp_col = '#1E88E5'
fn_col = 'red'
fp1_col = '#FFC107'
fp2_col = '#7B4578'

agency1 <- agency_can_state %>% select(country,lat,lon,state,dist_km) %>%
  rename(dist_state = dist_km)

agency2 <- agency_can_fed %>% select(country,lat,lon,state,dist_km) %>%
  rename(dist_fed = dist_km)

agency_can <- merge(agency1,agency2, all = TRUE)
agency_can <- agency_can[!duplicated(agency_can),]

agency3 <- agency_usa_state %>% select(country,lat,lon,state,dist_km) %>%
  rename(dist_state = dist_km)

agency4 <- agency_usa_fed %>% select(country,lat,lon,state,dist_km) %>%
  rename(dist_fed = dist_km)

agency_usa <- merge(agency3,agency4, all = TRUE)
agency_usa %>% filter(is.na(dist_state))

agency_usa <- agency_usa[!duplicated(agency_usa),]

agency_all <- rbind(agency_can,agency_usa)

anth_info <- merge(pop_dens,trav_time)
anth_info <- merge(anth_info,ownership)
anth_info <- merge(anth_info, agency_all)

anth_info <- anth_info %>% select(-manual_id) %>% mutate(lat = round(lat,5), lon = round(lon,5))

anth_info <- anth_info[!duplicated(anth_info),]

preds2 <- preds %>% filter(dataset == 'testing') %>% 
  mutate(present_pred = ifelse(PA_pred >= thresh,1,0))

preds2 %>% group_by(present) %>% count(present_pred)

all_dataset2 <- all_dataset2 %>% select(-c(present))

preds2 <- merge(preds2,all_dataset2)

preds2 <- merge(preds2,avg5_historical, by.x = c('lat_coord','lon_coord','year'), 
                  by.y = c('lat','lon','year'))

preds3 <- merge(preds2, anth_info)

lag <- 35

avg5_lag <- avg5_historical %>% select(lat,lon,year,min_t2m,max_t2m,coldest,julian,gdd_season,min_tp,max_tp,sum_tp,min_rh,max_rh) %>%
  mutate(year = year + lag) %>% 
  rename(min_t2m_lag = min_t2m, max_t2m_lag = max_t2m, coldest_lag = coldest,
         julian_lag =julian,gdd_season_lag = gdd_season, min_tp_lag = min_tp,max_tp_lag = max_tp,sum_tp_lag = sum_tp, 
         min_rh_lag = min_rh, max_rh_lag = max_rh)

preds3 <- merge(preds3,avg5_lag,by.x = c('lat_coord','lon_coord','year'), 
                  by.y = c('lat','lon','year'))

preds3 <- preds3 %>% mutate(change = case_when(present == 0 & present_pred == 1 ~ 'False positive',
                                               present == 0 & present_pred == 0 ~ 'True negative',
                                               present == 1 & present_pred == 0 ~ 'False negative',
                                               present == 1 & present_pred == 1 ~ 'True positive')) %>%
  drop_na(change) %>% 
  mutate(change = factor(change, levels = c('True negative', 'False positive','False negative','True positive')))

preds3 <- preds3[!duplicated(preds3),]

pdf("figures/population_density.pdf",height = 5, width = 10)
change_df <- preds3 %>% filter(near_needle == TRUE) %>% count(change)
median_df <- preds3 %>% filter(near_needle == TRUE)  %>% group_by(change) %>% 
  summarize(median = quantile(avg_density,probs = c(0.5)),
            q1 = quantile(avg_density, probs = c(0.25)),
            q2 = quantile(avg_density, probs = c(0.75)))
median_df <- preds3 %>% filter(near_needle == TRUE)  %>% group_by(change) %>% 
  summarize(median = quantile(avg_density,probs = c(0.5)))
plt_pd <- preds3 %>% filter(near_needle == TRUE) %>% ggplot() +
  aes(x = change, y = avg_density, color = change, fill = change) +
  #geom_violin(width = 1, position = position_dodge(0.7), alpha = 0) +
  geom_boxplot(outliers = FALSE, alpha = 0.2) +
  theme_classic(base_size = 15) + 
  scale_y_log10(limits = c(1e-3,1.2e4)) + 
  xlab("") + ggtitle("Average population density") +
  ylab("") +
  #geom_text(data = change_df, aes(x = change, y = Inf, label = paste0('n = ',n)), vjust = 1, size = 5, show_guide = FALSE)+
  geom_text(data = median_df, aes(x = change, y = Inf, label = paste0(round(median,2))), vjust = 2.5, size = 5, show_guide = FALSE) +
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive' = fp2_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive' = fp2_col,
                                   'False positive- not monitored' = fp2_col)) +
  theme(legend.position = 'none',
        plot.tag = element_text()) +
  labs(tag = "A)") 
plt_pd
dev.off()

do_wilcoxon_anthro <- function(change_a,change_b){
  temp <- preds3 %>% filter(near_needle == TRUE) %>%
    filter(change %in% c(change_a,change_b)) %>% 
    mutate(dist_state = ifelse(is.na(dist_state), 1000,dist_state)) %>% 
    mutate(min_agency = ifelse(dist_fed <= dist_state,dist_fed,dist_state)) 
  
  temp$change <- factor(temp$change, levels = c(change_a,change_b))
  
  result_dens <- wilcox.test(avg_density ~ change, data = temp, 
                        paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  
  temp_dens <- data.frame(A = change_a, B = change_b, pval = result_dens$p.value,
                          HL = as.numeric(result_dens$estimate), cl = result_dens$conf.int[1], cu = result_dens$conf.int[2],
                          param = "Average density")
  
  result_tt <- wilcox.test(min_time ~ change, data = temp, 
                             paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  
  temp_tt <- data.frame(A = change_a, B = change_b, pval = result_tt$p.value,
                          HL = as.numeric(result_tt$estimate), cl = result_tt$conf.int[1], cu = result_tt$conf.int[2],
                          param = "Min travel time")
  
  result_ag <- wilcox.test(min_agency ~ change, data = temp, 
                           paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  
  temp_ag <- data.frame(A = change_a, B = change_b, pval = result_ag$p.value,
                        HL = as.numeric(result_ag$estimate), cl = result_ag$conf.int[1], cu = result_ag$conf.int[2],
                        param = "Min agency")
  
  temp_all <- rbind(temp_dens, temp_tt, temp_ag)
  
  
  
  return(temp_all)
}

A <- c("True negative", "False positive", "False negative", "True positive")
B <- c("True negative", "False positive", "False negative", "True positive")

all_combos <- expand.grid(A,B)
all_combos <- all_combos %>% rename(A = Var1, B = Var2)
all_combos <- all_combos %>% filter(A != B)

pairs_df <- data.frame(A = c("True negative", "True negative", "True negative", "False positive", "False positive", 'False negative'),
                       B = c("False positive", "False negative", "True positive", "False negative", "True positive", "True positive"))


w_anthro <- c()
for (i in 1:length(all_combos$A)){
  temp = do_wilcoxon_anthro(all_combos$A[i], all_combos$B[i])  
  w_anthro <- rbind(w_anthro, temp)
}

pdf("figures/travel_time.pdf",height = 5, width = 10)
change_df <- preds3 %>% filter(near_needle == TRUE) %>% count(change)
median_df <- preds3 %>% filter(near_needle == TRUE)  %>% group_by(change) %>% 
  summarize(median = quantile(min_time,probs = c(0.5)))

plt_tt <- preds3 %>% filter(near_needle == TRUE) %>% mutate(min_time = ifelse(min_time == 0, 1,min_time)) %>% ggplot() +
  aes(x = change, y = min_time, color = change, fill = change) +
  #geom_violin(width = 1, position = position_dodge(0.7), alpha= 0) +
  geom_boxplot(outliers = FALSE, alpha = 0.2) +
  theme_classic(base_size = 15) + 
  scale_y_log10(limits = c(1,4100)) + 
  xlab("") + ggtitle("Minimum time to nearest city ") + 
  ylab("")+
  # geom_text(data = change_df, aes(x = change, y = Inf, label = paste0('n = ',n)), vjust = 1, size = 5,
  #           show.legend = FALSE)+
  geom_text(data = median_df, aes(x = change, y = Inf, label = paste0(round(median,1))), vjust = 2.5, size = 5,
            show.legend = FALSE)+
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive' = fp2_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive' = fp2_col,
                                   'False positive- not monitored' = fp2_col)) +
  theme(legend.position = 'none',
        plot.tag = element_text()) +
  labs(tag = "B)") 
plt_tt
dev.off()

pdf("figures/travel_time_agency.pdf", height = 5, width = 10)
change_df <- preds3 %>% filter(near_needle == TRUE) %>% count(change)
median_df <- preds3 %>% filter(near_needle == TRUE) %>% 
  mutate(dist_state = ifelse(is.na(dist_state), 1000,dist_state)) %>% 
  mutate(min_agency = ifelse(dist_fed <= dist_state,dist_fed,dist_state)) %>%
  group_by(change) %>% 
  summarize(median = quantile(min_agency,probs = c(0.5)))

plt_ag <- preds3 %>% filter(near_needle == TRUE) %>% 
  mutate(dist_state = ifelse(is.na(dist_state), 1000,dist_state)) %>% 
  mutate(min_agency = ifelse(dist_fed <= dist_state,dist_fed,dist_state)) %>% ggplot() +
  aes(x = change, y = min_agency, color = change, fill = change) +
  #geom_violin(width = 1, position = position_dodge(0.7), alpha = 0) +
  geom_boxplot(outliers = FALSE, alpha = 0.2) +
  theme_classic(base_size = 15) + 
  scale_y_log10(limits = c(1,250)) + 
  xlab("") + ggtitle("Minimum km to nearest agency") + 
  ylab("") +
  # geom_text(data = change_df, aes(x = change, y = Inf, label = paste0('n = ',n)), vjust = 1, size = 5,
  #           show.legend = FALSE)+
  geom_text(data = median_df, aes(x = change, y = Inf, label = paste0(round(median,1))), vjust = 2.5, size = 5,
            show.legend = FALSE) +
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive' = fp2_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive' = fp2_col,
                                   'False positive- not monitored' = fp2_col)) + 
  theme(legend.position = 'none',
        plot.tag = element_text()) +
  labs(tag = "C)") 
plt_ag
dev.off()

pdf("figures/outcomes2.pdf",height = 8, width = 18)
grid.arrange(plt_pd,plt_tt,plt_ag,nrow = 1)
dev.off()

w_pd <- w_anthro %>% filter(param == "Average density") %>%
  mutate(HL2 = case_when(abs(HL) <= 1e-5 ~ round(HL,8),
                         abs(HL) <= 1 & abs(HL) > 1e-5 ~ round(HL,4),
                         abs(HL) >1 ~ round(HL, 2))) %>% 
  mutate(sig = case_when(pval > 0.5 ~ "",
                             pval <= 0.05 & pval> 0.01 ~ "*", 
                             pval <= 0.01 & pval> 0.001 ~ "**",
                             pval <= 0.001 ~ "***"),
             ylev = as.numeric(B)) %>% 
  ggplot() + aes(x = A, y = B, color = HL, fill =HL) + geom_tile() + 
  theme_classic(base_size = 15) + 
  scale_color_gradient2("HL", high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0)+ 
  scale_fill_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0) +
  geom_text(aes(x = A, y = B, label = HL2), color = 'black') + 
  geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
  xlab("X") + ylab("Y") + 
  #ggtitle("Average density") + 
  theme(legend.position = 'bottom',
    plot.tag = element_text())+
  labs(tag = "D)") 

 

w_tt <- w_anthro %>% filter(param == "Min travel time") %>% 
  mutate(HL2 = round(HL,3)) %>% 
  mutate(sig = case_when(pval > 0.5 ~ "",
                         pval <= 0.05 & pval> 0.01 ~ "*", 
                         pval <= 0.01 & pval> 0.001 ~ "**",
                         pval <= 0.001 ~ "***"),
         ylev = as.numeric(B)) %>% 
  ggplot() + aes(x = A, y = B, color = HL, fill =HL) + geom_tile() + 
  theme_classic(base_size = 15) + 
  scale_color_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0)+ 
  scale_fill_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0) +
  geom_text(aes(x = A, y = B, label = HL2), color = 'black') + 
  geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
  xlab("X") + ylab("Y") + 
  #ggtitle("Minimum time to nearest 100,000+ inhabitants city") + 
  theme(legend.position = 'bottom',
        plot.tag = element_text())+
  labs(tag = "E)")

w_ag <- w_anthro %>% filter(param == "Min agency") %>% 
  mutate(HL2 = round(HL,2)) %>% 
  mutate(sig = case_when(pval > 0.5 ~ "",
                         pval <= 0.05 & pval> 0.01 ~ "*", 
                         pval <= 0.01 & pval> 0.001 ~ "**",
                         pval <= 0.001 ~ "***"),
         ylev = as.numeric(B)) %>% 
  ggplot() + aes(x = A, y = B, color = HL, fill =HL) + geom_tile() + 
  theme_classic(base_size = 15) + 
  scale_color_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0)+ 
  scale_fill_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0) +
  geom_text(aes(x = A, y = B, label = HL2), color = 'black') + 
  geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
  xlab("X") + ylab("Y") + 
  #ggtitle("Minimum kilometers to nearest agency") + 
  theme(legend.position = 'bottom',
        plot.tag = element_text())+
  labs(tag = "F)")

pdf("figures/outcomes_anth.pdf",height = 10, width = 20)
plot_grid(plt_pd,plt_tt,plt_ag,
             w_pd, w_tt, w_ag, nrow = 2,
          align = "v")
dev.off()

pdf("figures/ownership.pdf",height = 6, width = 10)
preds3 %>% filter(near_needle == TRUE) %>%
  mutate(ownership = ifelse(ownership == "Water", "Non-Forest",ownership)) %>%
  group_by(change) %>% count(ownership) %>% group_by(change) %>% mutate(sum_n = sum(n)) %>% 
  mutate(type2 = case_when(ownership %in% c('Corporate/Other (Private)', 'Family (Private)') ~ "Private",
                           ownership %in% c('Federal (Public)', 'Local (Public)', "State (Public)") ~ "Public",
                           ownership %in% c('Tribal') ~ "Tribal",
                           ownership %in% c('Unknown Forest') ~ "Unknown Forest",
                           ownership %in% c('Non-Forest') ~ "Non-Forest")) %>% 
  mutate(type2 = factor(type2, levels = c('Public', "Private", 'Tribal','Unknown Forest',"Non-Forest"))) %>% 
  ggplot() + aes(x = change, y = n/sum_n, color = ownership, fill = ownership) + geom_bar(stat = 'identity') +
  theme_classic() +
  scale_color_brewer("", palette = "Set1", direction = -1)+
  scale_fill_brewer("", palette = "Set1", direction = -1) + 
  facet_wrap(~type2, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  xlab("") + ylab("Proportion") +
  theme(legend.position = 'top')
dev.off()



weather_vars <- both_avg %>% arrange(score2) %>%
  filter(category %in% c("climatic variable- lag",
                         'climatic variable')) 
preds_weather <- preds3 %>% select('lat','lon','change','near_needle',weather_vars$variables) %>% 
  pivot_longer(cols = weather_vars$variables) %>% rename(variables = name)

preds_weather2 <- merge(preds_weather, both_avg)
preds_weather2$name <- factor(preds_weather2$name, levels = c(weather_vars$name))

weather_median <- preds_weather2 %>% filter(near_needle == TRUE) %>% 
  drop_na(value) %>% mutate(n = 1) %>%
  group_by(change,name) %>%
  summarize(median = quantile(value,0.5),
            var25 = quantile(value,0.25),
            var75 = quantile(value,0.75),
            varmean = mean(value),
            varsd = sd(value),
            sum_n = sum(n)) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP'))

weather_median <- merge(weather_median,weather_vars)
weather_median <- weather_median %>% 
  mutate(name = factor(name, levels = weather_vars$name),
         variables = factor(variables, levels = weather_vars$variables))

preds_weather3 <- preds_weather2 %>% filter(near_needle == TRUE) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP')) %>%
  mutate(change2 = factor(change2, levels = c('TN', 'FP','FN','TP')))

uni_weather <- weather_vars %>% pull(name)
uni_change2 <- c('TN', 'FP','FN','TP')

do_wilcoxon_weather <- function(name_a, change_a, change_b){
  temp <- preds_weather3 %>% 
    filter(name == name_a,
           change2 %in% c(change_a,change_b))
  
  temp$change2 <- factor(temp$change2, levels = c(change_a,change_b))
  temp$scaled <- rescale(temp$value)
    
  result <- wilcox.test(value ~ change2, data = temp, 
                        paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  result_scaled <- wilcox.test(scaled ~ change2, data = temp, 
                        paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  temp_res <- data.frame(A = change_a, B = change_b, pval = result$p.value,
                        HL = as.numeric(result$estimate), cl = result$conf.int[1],
                        cu = result$conf.int[2],
                        param = name_a,
                        pval_scaled = result_scaled$p.value,
                        HL_scaled = as.numeric(result_scaled$estimate), cl_scaled = result_scaled$conf.int[1],
                        cu_scaled = result_scaled$conf.int[2])
  
  return(temp_res)
}

pval_df_weather <- c()
for(i in 1:length(uni_weather)){
  for(j in 1:length(uni_change2)){
    for(k in 1:length(uni_change2)){
      if (uni_change2[k] != uni_change2[j]){
        pval_wilcoxon <- do_wilcoxon_weather(uni_weather[i], uni_change2[j], uni_change2[k])
        
        pval_df_weather <- rbind(pval_df_weather,pval_wilcoxon)
        
      }
    }
  }
}

#weather_vars <- sort(unique(pval_df_weather$param))
pval_df_weather$param <- factor(pval_df_weather$param, levels = weather_vars$name)
pval_df_weather$A <- factor(pval_df_weather$A, levels = c('TN', 'FP','FN','TP'))
pval_df_weather$B <- factor(pval_df_weather$B, levels = c('TN', 'FP','FN','TP'))
preds_weather2$variables <- factor(preds_weather2$variables, levels = weather_vars$name)

pval_df_weather <- pval_df_weather %>% mutate(sig = case_when(pval > 0.05 ~ "",
                       pval <= 0.05 & pval> 0.01 ~ "*", 
                       pval <= 0.01 & pval> 0.001 ~ "**",
                       pval <= 0.001 ~ "***"),
       ylev = as.numeric(B))


var_pick <- weather_vars$name

box_top <- list()
grid_bottom <- list()

for (i in 1:length(var_pick)) {
  
  p <- preds_weather2 %>% filter(near_needle == TRUE, name == var_pick[i]) %>% 
    mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                               change == 'True positive' ~ 'TP',
                               change == 'False negative' ~ 'FN',
                               change == 'False positive' ~ 'FP')) %>%
    mutate(change2 = factor(change2, levels = c('TN', 'FP','FN','TP'))) %>% 
    ggplot() +
    aes(x = change2, y = value, color = change2, fill = change2) +
    geom_boxplot(outliers = FALSE, alpha = 0.2) +
    theme_classic(base_size = 15) + 
    scale_color_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                      'FN' = fn_col, 'FP' = fp2_col,
                                      'False positive- not monitored' = fp2_col)) +
    scale_fill_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                     'FN' = fn_col, 'FP' = fp2_col,
                                     'False positive- not monitored' = fp2_col)) +
    geom_text(data = weather_median[weather_median$name == var_pick[i],],
              aes(x = change2, y = Inf, label = paste0(round(median,2))), vjust = 1, size = 5,
              show.legend = FALSE) + 
    xlab("") + ylab("") +
    theme(legend.position = 'none', 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle(var_pick[i])
  
  box_top[[i]] <- p
  
  p2 <- pval_df_weather %>%
    filter(param %in% var_pick[i]) %>%
    mutate(HL2 = case_when(abs(HL_scaled) <= 1e-5 ~ round(HL_scaled,8),
                           abs(HL_scaled) <= 1 & abs(HL_scaled) > 1e-5 ~ round(HL_scaled,4),
                           abs(HL_scaled) >1 ~ round(HL_scaled, 2))) %>% 
    ggplot() + aes(x = A, y = B, color = HL_scaled, fill =HL_scaled) + geom_tile() + 
    theme_classic(base_size = 15) + 
    scale_color_gradient2("HL", high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0, limits = c(-0.338, 0.338))+ 
    scale_fill_gradient2("HL", high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0, limits = c(-0.338, 0.338)) +
    geom_text(aes(x = A, y = B, label = HL2), color = 'black') + 
    geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
    xlab("X") + ylab("Y") + 
    #ggtitle("Average density") + 
    theme(legend.position = 'none') 
  
  grid_bottom[[i]] <- p2
  
}

combined_grid <- plot_grid(
  plotlist = c(box_top[11:15], grid_bottom[11:15],
               box_top[16:20], grid_bottom[16:20]), 
  nrow = 4,              
  labels = c(paste0(LETTERS[11:15], ")"), rep("", 5), paste0(LETTERS[16:20], ")")),
  align = "h"
)


pdf("figures/weather_KL10_2.pdf",height = 16, width = 18)
combined_grid
dev.off()

pval_df_weather %>% filter(A == "FN", B == "TP") %>% 
  ggplot() + aes(x = param, y = HL_scaled) + geom_point() + theme_classic() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red')

pval_df_weather %>% filter(param %in% var_pick) %>%
  mutate(A = factor(A, levels = c('TN', 'FP','FN','TP')),
                           B = factor(B, levels = c('TN', 'FP','FN','TP'))) %>% 
  ggplot() + aes(x = A, y = B, color = HL, fill = HL) + geom_tile() + 
  theme_classic(base_size = 15) + 
  scale_color_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0)+ 
  scale_fill_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0) +
  geom_text(aes(x = A, y = B, label = HL), color = 'black') + 
  xlab("X") + ylab("Y") + 
  facet_wrap(~param)

pval_df_weather %>% filter(pval_w >0.05)
pval_df_weather %>% filter(pval_ks >0.05)

uni_change2 <- c('TN', 'FP','FN','TP')
xx = 1
ranked_plts <- list()
for(i in 1:length(uni_change2)){

    pval_df_weather_small <- pval_df_weather %>% filter(A == uni_change2[i]) %>%
        select(A,B, HL_scaled, param,sig) %>% 
        mutate(HL_abs = abs(HL_scaled)) 
      
    pval_df_weather_ranked <-  pval_df_weather_small %>% 
      group_by(param) %>% summarize(max_HL = max(HL_abs)) %>% arrange(desc(max_HL))
    
    pval_df_weather_small$param <- factor(pval_df_weather_small$param, levels = pval_df_weather_ranked$param)
    
    ranked_plts[[i]] <- pval_df_weather_small %>% 
      ggplot() + aes(x = param, y = HL_abs, color = B) + geom_point() + theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste0("X = ", uni_change2[i])) +
      scale_color_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                        'FN' = fn_col, 'FP' = fp2_col)) 

}





combined_grid <- plot_grid(
  plotlist = ranked_plts, 
  nrow = 2,              
  #labels = c(paste0(LETTERS[11:15], ")"), rep("", 5), paste0(LETTERS[16:20], ")")),
  align = "h"
)




pdf("figures/weather_outcomes2.pdf",height = 20, width = 20)
preds_weather2 %>% filter(near_needle == TRUE) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP')) %>%
  mutate(change2 = factor(change2, levels = c('TN', 'FP','FN','TP'))) %>% 
  ggplot() +
  aes(x = change2, y = value, color = change2, fill = change2) +
  #geom_violin(width = 1, position = position_dodge(0.7), alpha = 0) +
  geom_boxplot(outliers = FALSE, alpha = 0.2) +
  theme_classic(base_size = 15) + facet_wrap(~name, scales = 'free') +
  scale_color_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                    'FN' = fn_col, 'FP' = fp2_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                   'FN' = fn_col, 'FP' = fp2_col,
                                   'False positive- not monitored' = fp2_col)) +
  geom_text(data = weather_median,
            aes(x = change2, y = Inf, label = paste0(round(median,2))), vjust = 1, size = 5,
            show.legend = FALSE) + 
  theme(legend.position = 'none',
        strip.background = element_blank()) + 
  xlab("Outcome") + 
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) 
dev.off()


################
################


habitat_vars <- both_avg %>% arrange(score2) %>% 
  filter(category %ni% c("climatic variable- lag",
                         'climatic variable')) %>% filter(type != 'categorical', type != 'binary',
                                                          variables != 'near_needle') %>%
  head(6)
preds_habitat <- preds3 %>% rename("Latitude" = "lat", "Longitude" = "lon") %>% 
  select('Latitude','Longitude','change','near_needle',habitat_vars$variables) %>% 
  pivot_longer(cols = c(habitat_vars$variables,'Latitude','Longitude')) %>% rename(variables = name) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP'))

preds_habitat2 <- merge(preds_habitat, habitat_vars, all.x = TRUE)

preds_habitat2 <- preds_habitat2 %>% mutate(name2 = case_when(variables == "Latitude" ~ "Latitude",
                                                              variables == "Longitude" ~ "Longitude",
                                                              variables %ni% c('lat','lon')~ name))
preds_habitat2$name2 <- factor(preds_habitat2$name2, levels = c(habitat_vars$name, 'Latitude','Longitude'))

habitat_median <- preds_habitat2 %>% filter(near_needle == TRUE) %>% 
  drop_na(value) %>% mutate(n = 1) %>%
  group_by(change,name2) %>%
  summarize(median = quantile(value,0.5),
            var25 = quantile(value,0.25),
            var75 = quantile(value,0.75),
            varmean = mean(value),
            varsd = sd(value),
            sum_n = sum(n)) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP'))


preds_habitat3 <- preds_habitat2 %>% filter(near_needle == TRUE) %>% 
  mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                             change == 'True positive' ~ 'TP',
                             change == 'False negative' ~ 'FN',
                             change == 'False positive' ~ 'FP')) %>%
  mutate(change2 = factor(change2, levels = c('TN', 'FP','FN','TP'))) 

uni_habitat <- unique(preds_habitat3$name2)
uni_change2 <- c('TN', 'FP','FN','TP')

do_wilcoxon_habitat <- function(name_a, change_a, change_b){
  temp <- preds_habitat3 %>% 
    filter(name2 == name_a,
           change2 %in% c(change_a,change_b))
  
  temp$change2 <- factor(temp$change2, levels = c(change_a,change_b))
  temp$scaled <- rescale(temp$value)
  
  result <- wilcox.test(value ~ change2, data = temp, 
                        paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  result_scaled <- wilcox.test(scaled ~ change2, data = temp, 
                               paired = FALSE, conf.int = TRUE, conf.level = 0.95)
  
  temp_res <- data.frame(A = change_a, B = change_b, pval = result$p.value,
                         HL = as.numeric(result$estimate), cl = result$conf.int[1],
                         cu = result$conf.int[2],
                         param = name_a,
                         pval_scaled = result_scaled$p.value,
                         HL_scaled = as.numeric(result_scaled$estimate), cl_scaled = result_scaled$conf.int[1],
                         cu_scaled = result_scaled$conf.int[2])
  
  return(temp_res)
}

pval_df_habitat <- c()
for(i in 1:length(uni_habitat)){
  for(j in 1:length(uni_change2)){
    for(k in 1:length(uni_change2)){
      if (uni_change2[k] != uni_change2[j]){
        pval_wilcoxon <- do_wilcoxon_habitat(uni_habitat[i], uni_change2[j], uni_change2[k])
        
        pval_df_habitat <- rbind(pval_df_habitat,pval_wilcoxon)
        
      }
    }
  }
}



#weather_vars <- sort(unique(pval_df_weather$param))
pval_df_habitat$param <- factor(pval_df_habitat$param, levels = c(habitat_vars$name, "Longitude","Latitude"))
pval_df_habitat$A <- factor(pval_df_habitat$A, levels = c('TN', 'FP','FN','TP'))
pval_df_habitat$B <- factor(pval_df_habitat$B, levels = c('TN', 'FP','FN','TP'))


pval_df_habitat <- pval_df_habitat %>% mutate(sig = case_when(pval > 0.05 ~ "",
                                                              pval <= 0.05 & pval> 0.01 ~ "*", 
                                                              pval <= 0.01 & pval> 0.001 ~ "**",
                                                              pval <= 0.001 ~ "***"),
                                              ylev = as.numeric(B))

pval_df_habitat %>% 
  mutate(A = factor(A, levels = c('TN', 'FP','FN','TP')),
         B = factor(B, levels = c('TN', 'FP','FN','TP'))) %>% 
  ggplot() + aes(x = A, y = B, color = HL_scaled, fill = HL_scaled) + geom_tile() + 
  theme_classic(base_size = 15) + 
  scale_color_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0)+ 
  scale_fill_gradient2(high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0) +
  geom_text(aes(x = A, y = B, label = round(HL_scaled,3)), color = 'black') + 
  geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
  xlab("X") + ylab("Y") + 
  facet_wrap(~param)

var_pick <- c(habitat_vars$name, 'Latitude','Longitude')

box_top <- list()
grid_bottom <- list()

for (i in 1:length(var_pick)) {
  
  p <- preds_habitat2 %>% filter(near_needle == TRUE, name2 == var_pick[i]) %>% 
    mutate(change2 = case_when(change == 'True negative' ~ 'TN',
                               change == 'True positive' ~ 'TP',
                               change == 'False negative' ~ 'FN',
                               change == 'False positive' ~ 'FP')) %>%
    mutate(change2 = factor(change2, levels = c('TN', 'FP','FN','TP'))) %>% 
    ggplot() +
    aes(x = change2, y = value, color = change2, fill = change2) +
    geom_boxplot(outliers = FALSE, alpha = 0.2) +
    theme_classic(base_size = 15) + 
    scale_color_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                      'FN' = fn_col, 'FP' = fp2_col,
                                      'False positive- not monitored' = fp2_col)) +
    scale_fill_manual("", values = c('TN' = tn_col, 'TP' = tp_col,
                                     'FN' = fn_col, 'FP' = fp2_col,
                                     'False positive- not monitored' = fp2_col)) +
    geom_text(data = weather_median[weather_median$name == var_pick[i],],
              aes(x = change2, y = Inf, label = paste0(round(median,2))), vjust = 1, size = 5,
              show.legend = FALSE) + 
    xlab("") + ylab("") +
    theme(legend.position = 'none', 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle(var_pick[i])
  
  box_top[[i]] <- p
  
  p2 <- pval_df_habitat %>%
    filter(param %in% var_pick[i]) %>%
    mutate(HL2 = case_when(abs(HL_scaled) <= 1e-5 ~ round(HL_scaled,8),
                           abs(HL_scaled) <= 1 & abs(HL_scaled) > 1e-5 ~ round(HL_scaled,4),
                           abs(HL_scaled) >1 ~ round(HL_scaled, 2))) %>% 
    ggplot() + aes(x = A, y = B, color = HL_scaled, fill =HL_scaled) + geom_tile() + 
    theme_classic(base_size = 15) + 
    scale_color_gradient2("HL", high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0, limits = c(-max(pval_df_habitat$HL_scaled)*1.1, max(pval_df_habitat$HL_scaled)*1.1))+ 
    scale_fill_gradient2("HL", high = 'darkgreen', low = 'orange', mid = 'grey85', midpoint = 0, limits = c(-max(pval_df_habitat$HL_scaled)*1.1, max(pval_df_habitat$HL_scaled)*1.1)) +
    geom_text(aes(x = A, y = B, label = HL2), color = 'black') + 
    geom_text(aes(x = A, y = ylev + 0.1, label = sig), color = 'black', hjust = 0.5) + 
    xlab("X") + ylab("Y") + 
    #ggtitle("Average density") + 
    theme(legend.position = 'none') 
  
  grid_bottom[[i]] <- p2
  
}


# 5. Combine all the stored plots using cowplot
combined_grid <- plot_grid(
  plotlist = c(box_top[1:4], grid_bottom[1:4],
               box_top[5:8], grid_bottom[5:8]), 
  nrow = 4,              
  labels = c(paste0(LETTERS[1:4], ")"), rep("",4), paste0(LETTERS[5:8], ")")),
  align = "h"
)

pdf("figures/habitat_KL8_1.pdf",height = 16, width = 16)
combined_grid
dev.off()