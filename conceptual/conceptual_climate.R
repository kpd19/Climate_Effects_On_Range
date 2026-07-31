library(tidyverse)
library(zoo)
library(scico)
library(gridExtra)
library(RColorBrewer)
library(rmapshaper)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/conceptual/")

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

h_avgs <- read_csv("../climate/data/annual_averages_1940-2025.csv")
cc_avgs <- read_csv("../climate/data/annual_avgs_all_models.csv")
land_lls <- read_csv("../climate/data/land_lls.csv")

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

cc_avgs %>% filter(year == 2049, land == TRUE,model == 'INM-CM5-0') %>% drop_na(in_cc) %>% 
  ggplot() + aes(x = lon,y = lat, color = sum_tp, fill = sum_tp) + geom_tile() + 
  theme_classic() + geom_vline(xintercept = -123.5) + geom_hline(yintercept = 49)

h_avgs %>% filter(year == 2020, land == TRUE) %>% drop_na(in_cc) %>% 
  ggplot() + aes(x = lon,y = lat, color = sum_tp, fill = sum_tp) + geom_tile() + 
  theme_classic() 

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

plt_t <- regional_avgs %>% 
  ggplot() + aes(x = year, y = regional_temp, group = model, color = ECS_num) +
  geom_point(size = 2) + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_temp), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1) +
  scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Annual Temperature " (degree*C)))

plt_pr <- regional_avgs %>% ggplot() + aes(x = year, y = regional_pr, group = model, color = ECS_num) +
  geom_point() + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_pr), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1)  +
  scale_color_scico("ECS", palette = 'roma', direction = -1) +
  ylab("Annual Precipitation (m)")

plt_rh <- regional_avgs %>% ggplot() + aes(x = year, y = regional_rh, group = model, color = ECS_num) +
  geom_point() + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_rh), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1) +
  scale_color_scico("ECS", palette = 'roma', direction = -1)+
  ylab("Annual % Relative Humidity")

pdf("figures/CMIP6_regional_averages2.pdf",height = 10, width = 18)
grid.arrange(plt_t,plt_pr,plt_rh,nrow = 3)
dev.off()

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

rolling_means %>% filter(dat == 'Climate change projection') %>% 
  ggplot() + 
  geom_line(aes(x = year, y = roll_pr,  color = ECS_num, group = model), size = 1.5) + 
  theme_classic() + 
  scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Regional Avg. Temperature " (degree*C))) +
  geom_line(data = rolling_means[rolling_means$dat == 'Historical data' & rolling_means$model == "INM-CM5-0",], 
            aes(x = year, y = roll_pr), color = 'grey55', size = 1.5) +
  theme(legend.position = 'top', legend.key.width = unit(1.5,'cm'))

test <- read_csv("downsampled/testing.csv")
test_itp <- read_csv("downsampled/testing_interp.csv")


test1 <- test %>% drop_na(hurs) %>% mutate(lon = lon - 360) %>% group_by(lat,lon,year,month) %>% summarize(mean_hr = mean(hurs, na.rm=TRUE))
test2 <- test_itp %>% drop_na(hurs) %>% mutate(lon = lon - 360) %>% group_by(lat,lon,year) %>% summarize(mean_hr = mean(hurs, na.rm=TRUE))
test3 <- test %>% drop_na(hurs) %>% mutate(lon = lon - 360,
                                           hurs = ifelse(hurs >100,100,hurs)) %>%
  group_by(lat,lon,year) %>% summarize(mean_hr = mean(hurs, na.rm=TRUE))
test4 <- test_itp %>% drop_na(hurs) %>% mutate(lon = lon - 360,
                                           hurs = ifelse(hurs >100,100,hurs)) %>%
  group_by(lat,lon,year) %>% summarize(mean_hr = mean(hurs, na.rm=TRUE))

test1 %>% ggplot() + aes(x = lon, y = lat, color = mean_hr, fill = mean_hr) + geom_tile() + theme_classic()
test2 %>% ggplot() + aes(x = lon, y = lat, color = mean_hr, fill = mean_hr) + geom_tile() + theme_classic()

test1 %>% ungroup %>% summarize(tot_mean = mean(mean_hr))
test2 %>% ungroup %>% summarize(tot_mean = mean(mean_hr))
test3 %>% ungroup %>% summarize(tot_mean = mean(mean_hr))
test4 %>% ungroup %>% summarize(tot_mean = mean(mean_hr))


regional_periods <- regional_avgs %>% filter(year >= 1985) %>%
  mutate(period = case_when(year <= 2010 ~'1985-2010',
                            year > 2010 & year <= 2025 ~'2011-2025',
                            year >= 2030 & year <= 2039 ~'2030-2039',
                            year >= 2040 & year <= 2049 ~'2040-2049',
                            year >= 2050 & year <= 2059 ~'2050-2059',
                            year >= 2060 & year <= 2069 ~'2060-2069',
                            year >= 2070 & year <= 2079 ~'2070-2079',
                            year >= 2080 & year <= 2089 ~'2080-2089',
                            year >= 2090 & year <= 2100 ~'2090-2100')) %>% 
  mutate(yr2 = case_when(year <= 2010 ~ 1997.5,
                         year > 2010 & year <= 2025 ~ 2018,
                         year >= 2030 & year <= 2039 ~2035,
                         year >= 2040 & year <= 2049 ~2045,
                         year >= 2050 & year <= 2059 ~2055,
                         year >= 2060 & year <= 2069 ~2065,
                         year >= 2070 & year <= 2079 ~2075,
                         year >= 2080 & year <= 2089 ~2085,
                         year >= 2090 & year <= 2100 ~2095)) %>% group_by(model,dat,period,yr2) %>% 
  summarize(avg_temp = mean(regional_temp),
            avg_pr = mean(regional_pr),
            avg_rh = mean(regional_rh))
write_csv(regional_periods,'data/regional_averages_period.csv')
write_csv(regional_avgs,'data/regional_averages.csv')

regional_periods2 <- regional_periods %>%
  mutate(p1_temp = regional_periods[regional_periods$period == '1985-2010' & regional_periods$model == 'INM-CM5-0',]$avg_temp,
        p2_temp = regional_periods[regional_periods$period == '2011-2025' & regional_periods$model == 'INM-CM5-0',]$avg_temp,
        p1_pr = regional_periods[regional_periods$period == '1985-2010' & regional_periods$model == 'INM-CM5-0',]$avg_pr,
        p2_pr = regional_periods[regional_periods$period == '2011-2025' & regional_periods$model == 'INM-CM5-0',]$avg_pr,
        p1_rh = regional_periods[regional_periods$period == '1985-2010' & regional_periods$model == 'INM-CM5-0',]$avg_rh,
        p2_rh = regional_periods[regional_periods$period == '2011-2025' & regional_periods$model == 'INM-CM5-0',]$avg_rh)
regional_periods2 <- regional_periods2 %>% mutate(diff1_temp = avg_temp - p1_temp,
                             diff2_temp = avg_temp - p2_temp,
                             diff1_pr = avg_pr - p1_pr,
                             diff2_pr = avg_pr - p2_pr,
                             diff1_pr_perc = (avg_pr - p1_pr)/p1_pr,
                             diff2_pr_perc = (avg_pr - p2_pr)/p1_pr,
                             diff1_rh = avg_rh - p1_rh,
                             diff2_rh = avg_rh - p2_rh)
temp_p2_diffs <- regional_periods2 %>% group_by(dat,period) %>% 
  summarize(mean_temp = mean(diff2_temp),
            sd_temp = sd(diff2_temp))

temp_p2_diffs %>% ggplot() + aes(x = period, y = mean_temp) + geom_point() + theme_classic()

write_csv(temp_p2_diffs, "data/temperature_differences.csv")

a <- regional_periods2 %>% filter(dat == 'Climate change projection')
            
Tpr_p2_diffs <- regional_periods2 %>% group_by(dat,period) %>% 
  summarize(mean_pr = mean(diff2_pr_perc)*100,
            sd_pr = sd(diff2_pr_perc)*100,
            min_pr = min(diff2_pr_perc)*100,
            max_pr = max(diff2_pr_perc)*100)

regional_periods2 %>% 
  ggplot() + aes(x = yr2, y = avg_temp, color = model) + geom_point() + theme_classic() +
  scale_color_brewer(palette = 'Paired') + ylim(0,17.5)
  

#############
#############
#############

regional_periods_20 <- regional_avgs %>% filter(year >= 1985) %>%
  mutate(period = case_when(year <= 2010 ~'1985-2010',
                            year > 2010 & year <= 2025 ~'2011-2025',
                            year >= 2030 & year <= 2039 ~'2030-2039',
                            year >= 2040 & year <= 2059 ~'2040-2059',
                            year >= 2060 & year <= 2079 ~'2060-2079',
                            year >= 2080 & year <= 2100 ~'2080-2100')) %>% 
  filter(period != '2030-2039') %>% 
  group_by(model,dat,period) %>% 
  summarize(avg_temp = mean(regional_temp),
            avg_pr = mean(regional_pr),
            avg_rh = mean(regional_rh))
write_csv(regional_periods_20,'data/regional_averages_period_20.csv')

regional_periods_20 <- regional_periods_20 %>%
  mutate(p2_temp = regional_periods_20[regional_periods_20$period == '2011-2025' & regional_periods_20$model == 'INM-CM5-0',]$avg_temp,
         p2_pr = regional_periods_20[regional_periods_20$period == '2011-2025' & regional_periods_20$model == 'INM-CM5-0',]$avg_pr,
         p2_rh = regional_periods_20[regional_periods_20$period == '2011-2025' & regional_periods_20$model == 'INM-CM5-0',]$avg_rh)
regional_periods_20 <- regional_periods_20 %>% mutate(diff2_temp = avg_temp - p2_temp,
                                                  diff2_pr = avg_pr - p2_pr,
                                                  diff2_pr_perc = (avg_pr - p2_pr)/p2_pr,
                                                  diff2_rh = avg_rh - p2_rh)


a <- regional_periods_20 %>% filter(dat!='Historical data') %>%
  select(model,period,diff2_temp) %>% mutate(diff2_temp = round(diff2_temp,2))

Tpr_p2_diffs <- regional_periods2 %>% group_by(dat,period) %>% 
  summarize(mean_pr = mean(diff2_pr_perc)*100,
            sd_pr = sd(diff2_pr_perc)*100,
            min_pr = min(diff2_pr_perc)*100,
            max_pr = max(diff2_pr_perc)*100)

regional_periods2 %>% 
  ggplot() + aes(x = yr2, y = avg_temp, color = model) + geom_point() + theme_classic() +
  scale_color_brewer(palette = 'Paired') + ylim(0,17.5)


#############
#############
#############

col_vals <- setNames(c(brewer.pal(10,'Paired'),'grey55'),c(models,"Historical data"))
regional_avgs_hist$dat <- "Historical data"
regional_avgs_hist$model <- "Historical data"
regional_avgs_cc$dat <- "Climate change projection"

regional_avgs2 <- rbind(regional_avgs_hist, regional_avgs_cc)

rolling_means2 <- regional_avgs2 %>% arrange(year) %>% group_by(dat,model) %>% 
  mutate(roll_temp = rollmean(regional_temp,k = 10, align = 'center',na.pad= TRUE),
         roll_rh = rollmean(regional_rh,k = 10, align = 'center',na.pad= TRUE),
         roll_pr = rollmean(regional_pr,k = 10, align = 'center',na.pad= TRUE))

plt1 <- regional_avgs2 %>% filter(year != 2025) %>% 
  ggplot() + aes(x = year, y = regional_temp, group = model, color = model) +
  geom_point(size = 2, alpha = 0.5) + theme_classic() + 
  geom_line(data = rolling_means2, aes(x = year, y = roll_temp, color = model), size = 1.5) + 
  #scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Annual Temperature " (degree*C))) +
  scale_color_manual(values = col_vals)

plt2 <- regional_avgs2 %>% filter(year != 2025) %>% 
  ggplot() + aes(x = year, y = regional_pr, group = model, color = model) +
  geom_point(size = 2, alpha = 0.5) + theme_classic() + 
  geom_line(data = rolling_means2, aes(x = year, y = roll_pr, color = model), size = 1.5) + 
  #scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Annual Temperature " (degree*C))) +
  scale_color_manual(values = col_vals)

plt3 <- regional_avgs2 %>% filter(year != 2025) %>% 
  ggplot() + aes(x = year, y = regional_rh, group = model, color = model) +
  geom_point(size = 2, alpha = 0.5) + theme_classic() + 
  geom_line(data = rolling_means2, aes(x = year, y = roll_rh, color = model), size = 1.5) + 
  #scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Annual Temperature " (degree*C))) +
  scale_color_manual(values = col_vals)

avg5_historical <- read_csv('../range_modeling/data/avg5_weather_1940-2025.csv')
avg5_cc <- read_csv('../range_modeling/data/avg5_weather_cc.csv')

avg5_cc %>% filter(model == 'INM-CM5-0', lat == 52.25, lon == -117)

prev_test <- avg5_historical %>% filter(lat == 45, lon == -117.5) %>%
  mutate(model = 'Historical data')
cc_test <- avg5_cc %>% filter(lat == 45, lon == -117.5)

test_all <- rbind(prev_test,cc_test)

pdf("figures/cc_comparison.pdf",height = 6, width = 15)
test_all %>% pivot_longer(cols = c('min_t2m','max_t2m','min_tp', 'max_tp', 'min_rh', 'max_rh',
                                   'julian','gdd_season','sum_tp','coldest')) %>%
  ggplot() + aes(x = year, y = value, color = model) + geom_point() +
  theme_classic() + facet_wrap(~name, scales = 'free_y')
dev.off()

avg5_cc %>% drop_na(sum_tp) %>% filter(year == 2035) %>%
  ggplot() + aes(x = lon,y = lat, color = julian, fill = julian) + geom_tile() +
  theme_classic() + facet_wrap(~model)

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

rh_avgs_cc <- cc_avgs %>% rename(mean_t2m_cc = mean_t2m, sum_tp_cc = sum_tp, mean_rh_cc = mean_rh,
           max_t2m_cc = max_t2m, min_t2m_cc = min_t2m, julian_cc = julian,
           gdd_season_cc = gdd_season) %>% mutate(bin = floor(year/10)*10) %>% 
  group_by(lat,lon,bin,model) %>% summarize(mean_rh_cc = mean(mean_rh_cc))

rh_avgs_h <- h_avgs %>% mutate(model = "Historical") %>% filter(year >= 2000, year <= 2010) %>% 
  group_by(lat,lon) %>% summarize(mean_rh = mean(mean_rh)) 


avgs2 <- merge(rh_avgs_cc,rh_avgs_h)
avgs2 %>% filter(model %in% c("ACCESS-ESM1-5")) %>%
  ggplot() + aes(x = lon, y = lat, color = mean_rh_cc - mean_rh, fill = mean_rh_cc - mean_rh) + geom_tile() + theme_classic()  +
  facet_wrap(~bin) +
  scale_color_gradient2(high = 'red', low ='blue', mid = 'white', midpoint= 0)+
  scale_fill_gradient2(high = 'red', low ='blue', mid = 'white', midpoint= 0)

h_avgs

regional_avg5_h %>% filter(year >= 1945, year <= 2025) %>% 
  mutate(per = case_when(year < 1985 ~ "Lagged weather inputs",
                         year >= 1985 & year <= 2010 ~ "Period one", 
                         year >= 2011 ~"Period two")) %>% 
  ggplot() + aes(x = year, median_val, color = per) + geom_line() + geom_point() +
  theme_classic()  +
  facet_wrap(~name, scales = 'free_y')


h_avgs %>% arrange(year) %>% group_by(lat,lon) %>% 
  filter(year >= 1940, year <= 2024, land == TRUE) %>% drop_na(in_cc) %>% 
  pivot_longer(cols = c("mean_t2m",'min_t2m','max_t2m','sum_tp', 'mean_rh','julian','gdd_season')) %>% 
  group_by(year,name) %>% summarize(mean_val = mean(value,na.rm=TRUE),
                                    median_val = median(value,na.rm=TRUE)) %>% 
  mutate(per = case_when(year < 1985 ~ "Lagged weather inputs",
                         year >= 1985 & year <= 2010 ~ "Period one", 
                         year >= 2011 ~"Period two")) %>% 
   ggplot() + aes(x = year, mean_val, color = per) + geom_line() + geom_point() +
   theme_classic()  +
   facet_wrap(~name, scales = 'free_y')



avg5 <- read_csv('../range_modeling/data/avg5_weather_1940-2025.csv')

avg5 <- avg5 %>%
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
  select(lat,lon,year,min_t2m_r,max_t2m_r,min_rh_r, max_rh_r, min_tp_r, max_tp_r, sum_tp_r,
         julian_r,gdd_season_r,coldest_r) %>%
  rename(min_t2m = min_t2m_r, max_t2m = max_t2m_r,min_rh = min_rh_r, max_rh = max_rh_r,
         min_tp = min_tp_r, max_tp = max_tp_r, sum_tp = sum_tp_r,
         julian = julian_r, gdd_season = gdd_season_r,coldest = coldest_r)


avg5_long <- avg5 %>% pivot_longer(cols = c('min_t2m','max_t2m','sum_tp','min_tp','max_tp',
                               'min_rh','max_rh','coldest','julian','gdd_season')) 
avg5_means <- avg5_long %>% group_by(year,name) %>% summarize(mean_val = mean(value,na.rm=TRUE)) 

avg5_means <- merge(avg5_means, variables)
avg5_means <- avg5_means %>% mutate(name2 = factor(name2,levels = c("Min Avg Temp",'Max Avg Temp',
                                       'Coldest Temperature', 
                                       'Season degree days', 'Predicted hatch',
                                       'Annual Precipitation',
                                       'Min Precipitation', 'Max Precipitation',
                                       'Min Avg %RH','Max Avg %RH'))) 

pdf("figures/historical_weather_vars.pdf",height = 6, width = 12)
avg5_means %>% mutate(per = case_when(year < 1985 ~ "Lagged weather inputs",
                                        year >= 1985 & year <= 2010 ~ "Period one", 
                                        year >= 2011 ~"Period two")) %>% 
  ggplot() + aes(x = year, y = mean_val, color = per) + geom_line() + geom_point() + 
  theme_classic() +
  facet_wrap(~name2, scales = 'free',nrow = 2) + 
  ggtitle("5-year rolling averages") +
  scale_color_brewer("Period", palette = "Dark2")+
  theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5))
dev.off()
