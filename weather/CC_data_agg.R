library(tidyverse)
library(gridExtra)
library(lubridate)
library(zoo)
library(scico)

models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4","HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")
cats <- c("medium",'high','medium','high','medium','high','low','medium','low','low')
t100 <- c(4.64,7.01,5.42,5.01,3.7,5.99,3.59,5.67,3.97,3.77)
ECS_num <- c(3.87,5.62,4.31,4.76,3.9,5.42,1.92,4.48,2.68,2.5)

mod_df <- data.frame(model = models,ECS_cat = cats,t100 = t100, ECS_num = ECS_num)
mod_df <- mod_df %>% arrange(ECS_num)

habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")

cc_simple <- c()
cc_avgs <- c()
for(i in 1:length(models)){
  
  temp_simple <- read_csv(paste0('/Volumes/My Book/Synchrony/cc_downsampled/simple_avg_',models[i], "_update.csv"))
  temp_avg <- read_csv(paste0("/Volumes/My Book/Synchrony/cc_downsampled/annual_summary_",models[i],"_update2.csv"))
  
  temp_simple$model <- models[i]
  temp_avg$model <- models[i]
  
  cc_simple <- rbind(cc_simple,temp_simple)
  cc_avgs <- rbind(cc_avgs,temp_avg)
  
}

write_csv(cc_simple,"data/annual_simple_stats_all_models.csv")
write_csv(cc_avgs,"data/annual_avgs_all_models.csv")

land_lls <- habitat_features %>% drop_na(elev2) %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                               lon_coord = round(lon/0.25)*0.25) %>% 
  group_by(lat_coord,lon_coord) %>% 
  count(lat_coord)

land_lls <- land_lls %>% rename(lat = lat_coord,lon = lon_coord) %>% select(-n) %>% mutate(land = TRUE)

cc_avgs2 <- merge(cc_avgs,land_lls, all.x = TRUE)

write_csv(land_lls,"data/land_lls.csv")

regional_avgs <- cc_avgs2 %>% fisdlter(land == TRUE) %>% group_by(model,year) %>%
  summarize(regional_temp = mean(mean_temp,na.rm = TRUE),
            regional_pr = mean(total_pr,na.rm = TRUE),
            regional_rh = mean(mean_rh,na.rm=TRUE))

regional_avgs_cat <- merge(regional_avgs,mod_df)

rolling_means <- regional_avgs %>% arrange(year) %>% group_by(model) %>% 
  mutate(roll_temp = rollmean(regional_temp,k = 10, align = 'center',na.pad= TRUE),
         roll_rh = rollmean(regional_rh,k = 10, align = 'center',na.pad= TRUE),
         roll_pr = rollmean(regional_pr,k = 10, align = 'center',na.pad= TRUE))

regional_avgs_cat$model <- factor(regional_avgs_cat$model, levels = mod_df$model)
rolling_means$model <- factor(rolling_means$model, levels = mod_df$model)

plt_t <- regional_avgs_cat %>% 
  ggplot() + aes(x = year, y = regional_temp, group = model, color = ECS_num) +
  geom_point(size = 2) + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_temp), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1) +
  scale_color_scico("ECS", palette = 'roma', direction = -1) + 
  ylab(expression("Annual Temperature " (degree*C)))

plt_pr <- regional_avgs_cat %>% ggplot() + aes(x = year, y = regional_pr, group = model, color = ECS_num) +
  geom_point() + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_pr), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1)  +
  scale_color_scico("ECS", palette = 'roma', direction = -1) +
  ylab("Annual Precipitation (m)")

plt_rh <- regional_avgs_cat %>% ggplot() + aes(x = year, y = regional_rh, group = model, color = ECS_num) +
  geom_point() + theme_classic() + 
  geom_line(data = rolling_means, aes(x = year, y = roll_rh), color = 'grey55', size = 1.5) + 
  facet_wrap(~model,nrow = 1) +
  scale_color_scico("ECS", palette = 'roma', direction = -1)+
  ylab("Annual % Relative Humidity")

pdf("figures/CMIP6_regional_averages.pdf",height = 10, width = 18)
grid.arrange(plt_t,plt_pr,plt_rh,nrow = 3)
dev.off()


