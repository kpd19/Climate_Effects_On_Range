library(tidyverse)
library(gridExtra)
library(sf)

`%ni%` <- Negate(`%in%`)

source("fitting_functions.R")

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

pheromone <- read_csv("../outbreak_size/data/north_america_trap_data_051225.csv")
defoliation <- read_csv("../population_data/data/defoliation_split_1947-2024.csv")

pheromone <- pheromone %>% drop_na(trap_mean) %>% 
  mutate(period = case_when(year < 1985 ~'p0',
                            year >= 1985 & year <= 2010 ~ 'p1',
                            year >=2011 ~ 'p2')) %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                                            lon_coord = round(lon/0.25)*0.25)
pcount <- pheromone  %>%
  group_by(lat_coord,lon_coord) %>% 
  count(period)

p_count_wide <- pcount %>% pivot_wider(names_from = period, values_from = n) 

defoliation <- defoliation %>%  mutate(period = case_when(year < 1985 ~'d0',
                                                          year >= 1985 & year <= 2010 ~ 'd1',
                                                          year >=2011 ~ 'd2')) %>% mutate(lat_coord = round(lat_cent/0.25)*0.25,
                                                                                          lon_coord = round(lon_cent/0.25)*0.25)


dcount <- defoliation  %>%
  group_by(lat_coord,lon_coord) %>% 
  count(period)

d_count_wide <- dcount %>% pivot_wider(names_from = period, values_from = n) 

pop_counts <- merge(p_count_wide, d_count_wide)

lag_int <- "353535"

rfl0 <- read_csv(paste0("/Volumes/My Book/Synchrony/rf_lags3/predictions/predictions_0_lag", lag_int,".csv"))
all_stats3 <- read_csv(paste0('/Volumes/My Book/Synchrony/rf_lags3/nums2/nums2_0_lag', lag_int,'.csv'))
all_stats3 %>% filter(thresh == 0.325)

thresh <- 0.325 

mean_lags <- rfl0 %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0),
                             synth = ifelse(source %in% c("Synthetic data"),'synthetic','population')) %>%
  group_by(lon_coord,lat_coord,synth,dataset) %>%
  summarize(mean_pred = mean(PA_group),
            mean_presence = mean(present)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0),
         pres = ifelse(mean_presence > 0,1,0)) %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'False negative',
                            bin == 0 & pres == 0 ~ 'True negative',
                            bin == 1 & pres == 0 ~ 'False positive',
                            bin == 1 & pres == 1 ~ 'True positive')) %>% drop_na(change)

mean_lags2 <- merge(mean_lags, pop_counts, all.x = TRUE)

cols <- c("p1",'p2','p0','d1','d2','d0')
mean_lags2[cols][is.na(mean_lags2[cols])] <- 0

mean_lags2 <- mean_lags2 %>% mutate(period_any = p1 + d1 + p2 + d2) %>% 
  mutate(change = ifelse(change == "False positive" & period_any >0, "False positive- monitored",change)) %>% 
  mutate(change = ifelse(change == "False positive" & period_any == 0, "False positive- not monitored",change)) 

mean_lags2 %>% group_by(dataset,change) %>% count(change)

mean_lags2 <- mean_lags2 %>% mutate(dat2 = ifelse(dataset == 'training', 'Training (1985-2010)', 'Testing (2011-2025)')) %>% 
  mutate(dat2 = factor(dat2, levels = c('Training (1985-2010)','Testing (2011-2025)')))
 
tn_col = 'grey5'
tp_col = '#1E88E5'
fn_col = 'red'
fp1_col = '#FFC107'
fp3_col = '#FF7642'
fp2_col = '#7B4578'
fp4_col <- "#C44474"

diff_pres <- ggplot() + geom_tile(data = mean_lags2,
                                  aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                   'False positive- not monitored' = fp2_col))+ 
  xlab("Longitude") + ylab("Latitude")  + 
  facet_wrap(~dat2)

plt_diff <- diff_pres + geom_sf(data = all_geo2, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top') 
pdf(paste0("figures/indiv3/testing_data_proj_",lag_int,".pdf"),height = 8, width = 12)
plt_diff
dev.off()


outcomes <- rfl0 %>% filter(dataset == 'testing') %>% mutate(pred = ifelse(PA_pred >= thresh,1,0),
                synth = ifelse(source %in% c("Synthetic data"),'synthetic','population')) %>%
  mutate(change = case_when(pred == 0 & present == 1 ~ 'false_negative',
                            pred == 0 & present == 0 ~ 'true_negative',
                            pred == 1 & present == 0 ~ 'false_positive',
                            pred == 1 & present == 1 ~ 'true_positive')) %>% 
  count(change) %>% pivot_wider(names_from = change, values_from = n)

TNR <- outcomes$true_negative/(outcomes$true_negative + outcomes$false_positive)*100
TPR <- outcomes$true_positive/(outcomes$true_positive + outcomes$false_negative)*100

100- TNR
100- TPR

outcomes_group <- rfl0 %>% filter(dataset == 'testing') %>% mutate(pred = ifelse(PA_pred >= thresh,1,0),
                                                             synth = ifelse(source %in% c("Synthetic data"),'synthetic','population')) %>%
  mutate(change = case_when(pred == 0 & present == 1 ~ 'false_negative',
                            pred == 0 & present == 0 ~ 'true_negative',
                            pred == 1 & present == 0 ~ 'false_positive',
                            pred == 1 & present == 1 ~ 'true_positive')) %>% 
  group_by(source) %>% 
  count(change) %>% pivot_wider(names_from = change, values_from = n)

outcomes_group %>% ungroup() %>% 
  mutate(fn_tot = sum(false_negative,na.rm=TRUE)) %>% 
  mutate(fnp = false_negative/fn_tot*100)

fp_areas <- rfl0 %>% filter(dataset == 'testing') %>% 
  mutate(bin = ifelse(PA_pred >= thresh, 1,0)) %>% 
  mutate(change = case_when(bin == 0 & present == 1 ~ 'False negative',
                            bin == 0 & present == 0 ~ 'True negative',
                            bin == 1 & present == 0 ~ 'False positive',
                            bin == 1 & present == 1 ~ 'True positive')) %>% 
  filter(change == "False positive") %>% group_by(lat_coord,lon_coord) %>% count(change)

fp_areas <- merge(fp_areas, pop_counts, all.x = TRUE)
cols <- c("p1",'p2','p0','d1','d2','d0')
fp_areas[cols][is.na(fp_areas[cols])] <- 0
fp_areas %>% mutate(period_one = p1 + d1, period_two = p2 + d2) %>% 
  mutate(change = ifelse(change == "False positive" & period_one >0 & period_two ==0, "False positive- p1 monitoring",change)) %>% 
  mutate(change = ifelse(change == "False positive" & period_one ==0 & period_two >0, "False positive- p2 monitoring",change)) %>% 
  mutate(change = ifelse(change == "False positive" & period_one >0 & period_two >0, "False positive- p1 & p2 monitoring",change)) %>% 
  mutate(change = ifelse(change == "False positive" & period_one == 0 & period_two == 0, "False positive- not monitored",change))  %>% 
  group_by(change) %>% summarize(sum_n = sum(n))


rfl0_update <- read_csv(paste0("/Volumes/My Book/Synchrony/rf_update2/predictions_update_0_lag", lag_int,".csv"))

thresh <- 0.325 

mean_lags <- rfl0_update %>% mutate(PA_group = ifelse(PA_pred >= thresh,1,0),
                             synth = ifelse(source %in% c("Synthetic data"),'synthetic','population')) %>%
  group_by(lon_coord,lat_coord,synth,dataset) %>%
  summarize(mean_pred = mean(PA_group),
            mean_presence = mean(present)) %>% 
  mutate(bin = ifelse(mean_pred > 0,1,0),
         pres = ifelse(mean_presence > 0,1,0)) %>% 
  mutate(change = case_when(bin == 0 & pres == 1 ~ 'False negative',
                            bin == 0 & pres == 0 ~ 'True negative',
                            bin == 1 & pres == 0 ~ 'False positive',
                            bin == 1 & pres == 1 ~ 'True positive')) %>% drop_na(change)

mean_lags2 <- merge(mean_lags, pop_counts, all.x = TRUE)

cols <- c("p1",'p2','p0','d1','d2','d0')
mean_lags2[cols][is.na(mean_lags2[cols])] <- 0

mean_lags2 <- mean_lags2 %>% mutate(period_any = p1 + d1 + p2 + d2) %>% 
  mutate(change = ifelse(change == "False positive" & period_any >0, "False positive- monitored",change)) %>% 
  mutate(change = ifelse(change == "False positive" & period_any == 0, "False positive- not monitored",change)) 

mean_lags2 %>% group_by(dataset,change) %>% count(change)

mean_lags2 <- mean_lags2 %>% mutate(dat2 = ifelse(dataset == 'training', 'Training (1985-2010)', 'Training (2011-2025)')) %>% 
  mutate(dat2 = factor(dat2, levels = c('Training (1985-2010)','Training (2011-2025)')))


diff_pres <- ggplot() + geom_tile(data = mean_lags2,
                                  aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                   'False positive- not monitored' = fp2_col))+ 
  xlab("Longitude") + ylab("Latitude")  + 
  facet_wrap(~dat2)

plt_diff <- diff_pres + geom_sf(data = all_geo2, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top') 
pdf(paste0("figures/update3/testing_data_proj_update_",lag_int,".pdf"),height = 8, width = 12)
plt_diff
dev.off()

outcomes_update <- rfl0_update %>% #filter(dataset == 'testing') %>%
  mutate(pred = ifelse(PA_pred >= thresh,1,0)) %>%
  mutate(change = case_when(pred == 0 & present == 1 ~ 'false_negative',
                            pred == 0 & present == 0 ~ 'true_negative',
                            pred == 1 & present == 0 ~ 'false_positive',
                            pred == 1 & present == 1 ~ 'true_positive')) %>% 
  group_by(dataset) %>% 
  count(change) %>% pivot_wider(names_from = change, values_from = n)

if("false_negative" %in% colnames(outcomes_update) == FALSE){
  outcomes_update$false_negative <- 0
}

TNR <- outcomes_update$true_negative/(outcomes_update$true_negative + outcomes_update$false_positive)*100
TPR <- outcomes_update$true_positive/(outcomes_update$true_positive + outcomes_update$false_negative)*100