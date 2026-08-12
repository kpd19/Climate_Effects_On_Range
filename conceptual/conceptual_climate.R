library(tidyverse)
library(zoo)
library(scico)
library(gridExtra)
library(RColorBrewer)

models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4",
            "HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")
cats <- c("medium",'high','medium','high','medium','high','low','medium','low','low')
t100 <- c(4.64,7.01,5.42,5.01,3.7,5.99,3.59,5.67,3.97,3.77)
ECS_num <- c(3.87,5.62,4.31,4.76,3.9,5.42,1.92,4.48,2.68,2.5)

variables <- read_csv("../range_modeling/data/var_names_pa2.csv")
variables <- variables %>%  filter(category == 'climatic variable')%>% select(variables,name) %>% 
  rename(name2 = name) %>% rename(name = variables)

mod_df <- data.frame(model = models,ECS_cat = cats,t100 = t100, ECS_num = ECS_num)
mod_df <- mod_df %>% arrange(ECS_num)

h_avgs <- read_csv("../weather/data/annual_averages_1940-2025.csv")
cc_avgs <- read_csv("../weather/data/annual_avgs_all_models.csv")
land_lls <- read_csv("../weather/data/land_lls.csv")

h_avgs <- h_avgs %>% rename(lat = latitude, lon = longitude)

cc_avgs <- merge(cc_avgs,land_lls)
h_avgs <- merge(h_avgs,land_lls)

in_cc_ll <- cc_avgs %>% filter(year == 2049, land == TRUE,model == 'INM-CM5-0') %>%
  drop_na(mean_t2m) %>% 
  select(lat,lon) %>% mutate(in_cc = TRUE)

cc_avgs <- merge(cc_avgs,in_cc_ll, all = TRUE)
h_avgs <- merge(h_avgs,in_cc_ll, all = TRUE)

cc_avgs <- cc_avgs %>% filter(land == TRUE) %>% drop_na(in_cc) 
h_avgs <- h_avgs %>% filter(land == TRUE) %>% drop_na(in_cc)

regional_avgs_cc <- cc_avgs %>% filter(land == TRUE) %>% group_by(model,year) %>%
  summarize(regional_temp = mean(mean_t2m,na.rm = TRUE),
            regional_pr = mean(sum_tp,na.rm = TRUE),
            regional_rh = mean(mean_rh,na.rm=TRUE))

regional_avgs_hist <- h_avgs %>% filter(land == TRUE) %>% group_by(year) %>%
  summarize(regional_temp = mean(mean_t2m,na.rm = TRUE),
            regional_pr = mean(sum_tp,na.rm = TRUE),
            regional_rh = mean(mean_rh,na.rm=TRUE))

regional_avgs_cat1 <- merge(regional_avgs_hist,mod_df)
regional_avgs_cat2 <- merge(regional_avgs_cc,mod_df)
regional_avgs_cat1$dat <- "Historical data"
regional_avgs_cat2$dat <- "Climate change projection"

regional_avgs <- rbind(regional_avgs_cat1, regional_avgs_cat2)

rolling_means <- regional_avgs %>% arrange(year) %>% group_by(dat,model) %>% 
  mutate(roll_temp = rollmean(regional_temp,k = 10, align = 'center',na.pad= TRUE),
         roll_rh = rollmean(regional_rh,k = 10, align = 'center',na.pad= TRUE),
         roll_pr = rollmean(regional_pr,k = 10, align = 'center',na.pad= TRUE))

regional_avgs$model <- factor(regional_avgs$model, levels = mod_df$model)
rolling_means$model <- factor(rolling_means$model, levels = mod_df$model)

pdf("figures/aregional_temp.pdf",height = 5.2, width = 4)
rolling_means %>% filter(dat == 'Climate change projection') %>% 
  ggplot() + 
  geom_line(aes(x = year, y = roll_temp,  color = ECS_num, group = model), size = 1.5) + 
  theme_classic(base_size = 12) + 
  scale_color_scico("", palette = 'roma', direction = -1) + 
  ylab(expression("Regional Avg. Temperature " (degree*C))) +
  xlab("Year")+
  geom_line(data = rolling_means[rolling_means$dat == 'Historical data' & rolling_means$ model == "INM-CM5-0",], 
            aes(x = year, y = roll_temp), color = 'grey55', size = 1.5) +
  theme(legend.position = 'top', 
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm")) 
dev.off()

#############
#############
#############

avg5_historical <- read_csv('../range_modeling/data/avg5_weather_1940-2025.csv')
avg5_cc <- read_csv('../range_modeling/data/avg5_weather_cc.csv')

avg5_historical2 <- merge(avg5_historical, in_cc_ll, all = TRUE)
avg5_cc2 <- merge(avg5_cc, in_cc_ll, all = TRUE)

regional_avg5_h <- avg5_historical2 %>% filter(in_cc == TRUE) %>%
  pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
                      'julian','gdd_season','sum_tp','coldest')) %>%
  group_by(year,name) %>% summarize(mean_val = mean(value,na.rm=TRUE),
                                    median_val = median(value, na.rm=TRUE))

regional_avg5_c <- avg5_cc2 %>% filter(in_cc == TRUE) %>%
  pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
                        'julian','gdd_season','sum_tp','coldest')) %>%
  group_by(year,name,model) %>% summarize(mean_val = mean(value,na.rm=TRUE))

regional_avg5_h$model <- "Historical"

regional_avg5_all <- rbind(regional_avg5_c, regional_avg5_h)

regional_avg5_all %>% 
  ggplot() + aes(x = year, y = mean_val, color = model) + geom_point() +
  theme_classic() + facet_wrap(~name, scales = 'free_y')


hist_avg <- avg5_historical2 %>% filter(in_cc == TRUE, year <= 2025, year >= 2010) %>%
  pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
                        'julian','gdd_season','sum_tp','coldest')) %>%
  group_by(name,lat,lon) %>% summarize(mean_val = mean(value,na.rm=TRUE))

cc_avg <- avg5_cc2 %>% filter(in_cc == TRUE) %>%
  pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
                        'julian','gdd_season','sum_tp','coldest')) 
cc_avg1 <- cc_avg %>% filter(year %in% c(2040,2050,2060,2070,2080,2090,2100))
ind_compare <- merge(cc_avg1,hist_avg,by.x = c("lat",'lon','name'), by.y = c("lat",'lon','name'))

ind_compare <- merge(ind_compare, variables)

ind_compare2 <- merge(ind_compare,mod_df)

pdf("figures/differences_from_baseline2.pdf",height = 6, width = 12)
ind_compare2 %>% mutate(diff = value - mean_val) %>% group_by(name2,year,model, ECS_num) %>% 
  summarize(mean_diff = mean(diff),
           sd_diff = sd(diff)) %>% 
  mutate(name2 = factor(name2,levels = c("Min Avg Temp",'Max Avg Temp',
                                         'Coldest Temperature', 
                                         'Season degree days', 'Predicted hatch',
                                         'Annual Precipitation',
                                         'Min Precipitation', 'Max Precipitation',
                                         'Min Avg %RH','Max Avg %RH'))) %>% 
  mutate(model = factor(model, levels = mod_df$model)) %>% 
  ggplot() + geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey55') +
  aes(x = year, y = mean_diff, color = interaction(model,ECS_num)) + geom_point() + geom_line() + 
  theme_classic() + facet_wrap(~name2, scales = 'free_y',nrow = 2) +
  scale_color_scico_d("Model", palette = 'roma', direction = -1) + 
  xlab("Year") + ylab("Difference change from 2010-2025") +
  theme(legend.position = 'top')
dev.off()

#############
#############
#############

avg5_long <- avg5_historical %>% pivot_longer(cols = c('min_t2m','max_t2m','sum_tp','min_tp','max_tp',
                               'min_rh','max_rh','coldest','julian','gdd_season')) %>%
  mutate(per = case_when(year < 1985 ~ "Pre-1985",
                                      year >= 1985 & year <= 2010 ~ "Period one (1985-2010)", 
                                      year >= 2011 ~ "Period two (2011-2025)"))

avg5_long <- merge(avg5_long, variables)

avg5_means <- avg5_long %>% group_by(year,name2,per) %>% summarize(mean_val = mean(value,na.rm=TRUE)) 
avg5_means <- avg5_means %>% mutate(name2 = ifelse(name2 == "Predicted hatch", "Predicted hatch date", name2))

pdf("figures/historical_weather_vars2.pdf",height = 6, width = 12)
avg5_means %>% 
  mutate(name2 = factor(name2,levels = c("Min Avg Temp",'Max Avg Temp',
                                             'Coldest Temperature', 
                                             'Season degree days', 'Predicted hatch date',
                                             'Annual Precipitation',
                                             'Min Precipitation', 'Max Precipitation',
                                             'Min Avg %RH','Max Avg %RH'))) %>% 
  mutate(per = factor(per, levels = c("Pre-1985", "Period one (1985-2010)", "Period two (2011-2025)" ))) %>% 
  ggplot() + aes(x = year, y = mean_val, color = per) + geom_line() + geom_point() + 
  theme_classic() +
  facet_wrap(~name2, scales = 'free',nrow = 2) + 
  scale_color_brewer("Period", palette = "Dark2")+
  theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5)) + xlab("Year") + 
  ylab("Regional average")
dev.off()
