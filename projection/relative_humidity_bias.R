library(tidyverse)
library(cowplot)
library(scico)

models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4",
            "HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")
cats <- c("medium",'high','medium','high','medium','high','low','medium','low','low')
t100 <- c(4.64,7.01,5.42,5.01,3.7,5.99,3.59,5.67,3.97,3.77)
ECS_num <- c(3.87,5.62,4.31,4.76,3.9,5.42,1.92,4.48,2.68,2.5)

mod_df <- data.frame(model = models,ECS_cat = cats,t100 = t100, ECS_num = ECS_num)
mod_df <- mod_df %>% arrange(ECS_num)

land_lls <- read_csv("../climate/data/land_lls.csv")

avg5_historical <- read_csv('../range_modeling/data/avg5_weather_1940-2025.csv')
avg5_cc <- read_csv('data/avg5_weather_cc.csv')

avg5_historical <- merge(avg5_historical, land_lls)
avg5_cc <- merge(avg5_cc, land_lls)

avg5_cc$model <- factor(avg5_cc$model, levels = mod_df$model)

pd_rh <- split_all2_num2 %>% filter(param %in% c("max_rh", "min_rh", "max_rh_lag", "min_rh_lag"))

avg5_cc %>% filter(year == 2050, land == TRUE, model == 'EC-Earth3-Veg-LR') %>% drop_na(max_rh) %>% 
  ggplot() + aes(x = lon, y = lat, color = min_rh, fill = min_rh) + geom_tile() + theme_classic()

avg5_historical %>% filter(year == 2025, land == TRUE) %>% drop_na(max_rh) %>%  
  ggplot() + aes(x = lon, y = lat, color = max_rh, fill = max_rh) + geom_tile() + theme_classic()

max(avg5_historical$lat)
max(avg5_cc$lat)

median_cc <- avg5_cc %>% drop_na(min_rh) %>% filter(year == 2100, land == TRUE) %>% 
  group_by(model) %>% summarize(median_min = median(min_rh),
                                median_max = median(max_rh))

median_hist <- avg5_historical %>% drop_na(min_rh) %>% filter(land == TRUE) %>% 
  summarize(median_min = median(min_rh),
            median_max = median(max_rh))

median_cc$model <- factor(median_cc$model, levels = mod_df$model)

max_rh_pd <- pd_rh %>% filter(outcome == 'present', param =="max_rh", lag == 35, training == "1985-2025") %>% 
  ggplot() +
  geom_line(aes(x = value, y = yhat), color = '#e31a1c', size = 1.2) + 
  theme_classic(base_size = 15) + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10)) +
  ylab("Predicted presence") +
  scale_x_continuous(limits = c(0,100)) + 
  geom_vline(data =median_cc,  aes(xintercept = median_max, color = model), linetype = 'dashed')+ 
  geom_vline(data =median_hist,  aes(xintercept = median_max), color = 'black', size = 1.5, linetype = 'dashed')+
  scale_color_scico_d("Model", palette = 'roma', direction = -1) 

min_rh_pd <- pd_rh %>% filter(outcome == 'present', param =="min_rh", lag == 35, training == "1985-2025") %>% 
  ggplot() +
  geom_line(aes(x = value, y = yhat), color = '#e31a1c', size = 1.2) + 
  theme_classic(base_size = 15) + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10)) +
  ylab("Predicted presence") +
  scale_x_continuous(limits = c(0,100)) + 
  geom_vline(data =median_cc,  aes(xintercept = median_min, color = model), linetype = 'dashed')+ 
  geom_vline(data =median_hist,  aes(xintercept = median_min), color = 'black', size = 1.5, linetype = 'dashed')+
  scale_color_scico_d("Model", palette = 'roma', direction = -1) 


min_rh_dens <- avg5_cc %>% drop_na(min_rh) %>% filter(year == 2100) %>% 
  ggplot() + aes(x = min_rh, group = model, color = model) + geom_density(position = 'identity') +
  theme_classic(base_size = 15) + 
  geom_density(data = avg5_historical[!is.na(avg5_historical$min_rh),], aes(x = min_rh), color = 'black', size = 1.5, inherit.aes = FALSE) +
  xlab("Minimum Average % Relative Humidity") +
  theme(legend.position = 'none') +
  scale_x_continuous(limits = c(0,100))+
  scale_color_scico_d("Model", palette = 'roma', direction = -1) 

max_rh_dens <- avg5_cc %>% drop_na(max_rh) %>% filter(year == 2100) %>% 
  ggplot() + aes(x = max_rh, group = model, color = model) + geom_density(position = 'identity') +
  theme_classic(base_size = 15) + 
  geom_density(data = avg5_historical[!is.na(avg5_historical$max_rh),], aes(x = max_rh), color = 'black', size = 1.5, inherit.aes = FALSE) +
  xlab("Maximum Average % Relative Humidity")+
  theme(legend.position = 'none') +
  scale_x_continuous(limits = c(0,100)) +
  scale_color_scico_d("Model", palette = 'roma', direction = -1) 


pdf("figures/relative_humidity_bias.pdf",height = 6, width = 10)
plot_grid(max_rh_pd, min_rh_pd, 
          max_rh_dens, min_rh_dens, nrow = 2, align = "hv")
dev.off()





