library(tidyverse)
library(gridExtra)
library(sf)
library(randomForest)
library(zoo)

`%ni%` <- Negate(`%in%`)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/range_modeling/")

source("fitting_functions.R")

set.seed <- 11

canada_data <- st_read("/Volumes/My Book/gadm/gadm41_CAN_shp/gadm41_CAN_1.shp")
canada_data <- canada_data %>% filter(NAME_1 %in% c("British Columbia",'Saskatchewan'))

us_data <- st_read("/Volumes/My Book/gadm/gadm41_USA_shp/gadm41_USA_1.shp")
us_data <- us_data %>% filter(NAME_1%in% c("Washington", "Oregon", "Idaho", "California", "Nevada", "Arizona",
                                           "New Mexico", "Utah", "Colorado","Montana","Wyoming"))


lag_int <- "353535"

load(paste0('/Volumes/My Book/Synchrony/rf_lags3/rf_models/rf_models1_0_lag', lag_int,'.RData'))
rfl0 <- read_csv(paste0("/Volumes/My Book/Synchrony/rf_lags3/predictions/predictions_0_lag", lag_int,".csv"))

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


mean_lags <- mean_lags %>% mutate(change = ifelse(change == 'False positive' & synth == 'synthetic', 'False positive- not monitored', change),
                                  change = ifelse(change == 'False positive' & synth == 'population', 'False positive- monitored', change))

mean_lags %>% group_by(dataset) %>% count(change)

mean_lags <- mean_lags %>% mutate(dat2 = ifelse(dataset == 'training', 'Training (1985-2010)', 'Testing (2011-2025)')) %>% 
  mutate(dat2 = factor(dat2, levels = c('Training (1985-2010)','Testing (2011-2025)')))
 
tn_col = 'grey5'
tp_col = '#1E88E5'
fn_col = 'red'
fp1_col = '#FFC107'
fp2_col = '#7B4578'

diff_pres <- ggplot() + geom_tile(data = mean_lags,
                                  aes(x = lon_coord, y = lat_coord, color = as.factor(change), fill = as.factor(change))) + #theme_classic(base_size = 15) + 
  scale_color_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                    'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                    'False positive- not monitored' = fp2_col)) +
  scale_fill_manual("", values = c('True negative' = tn_col, 'True positive' = tp_col,
                                   'False negative' = fn_col, 'False positive- monitored' = fp1_col,
                                   'False positive- not monitored' = fp2_col))+ 
  xlab("Longitude") + ylab("Latitude") 

plt_diff <- diff_pres + geom_sf(data = us_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  geom_sf(data = canada_data, aes(geometry = geometry), color = "grey55", fill = NA, size = 1) +
  theme_classic(base_size = 15) +
  coord_sf(ylim = c(32,52), xlim = c(-127,-103)) +
  guides(shape = guide_legend(override.aes = list(size = 0.25))) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = 'top') + 
  facet_wrap(~dat2)

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
