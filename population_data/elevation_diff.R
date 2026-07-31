library(tidyverse)
library(geosphere)
library(cowplot)
library(gridExtra)

train_data <- read_csv("../range_modeling/data/training_update_0_lag000_periods.csv")
train_data %>% group_by(period) %>% count(present)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

p2 <- 'elev2'
train_data %>% filter(present == 'present') %>%
  select(all_of(c('period','present',p2))) %>% rename(new_name := !!sym(p2)) %>% 
  ggplot() + aes(x = new_name, y = after_stat(ncount), color = period, fill = period) +
  geom_histogram(position = 'identity', alpha = 0.6) + 
  theme_classic(base_size = 10)+
  #xlim(c(lims$min_val-range,lims$max_val+range)) + 
  ylab("Distribution") + xlab("Parameter value") +
  scale_color_manual("", values = c('1985-2010' = 'black', '2011-2025' = '#fdbb84'))+
  scale_fill_manual("", values = c('1985-2010' = 'black', '2011-2025' = '#fdbb84')) +
  theme(legend.position = 'none')


d1 <- train_data %>% filter(present == 'present', period == '1985-2010') %>%
  select(all_of(c('period','present','elev2'))) 
h1 <- hist(d1$elev2)

in1 <- c('Pheromone trapping',
         'Defoliation surveys', "Lab Collections")
in2 <- c('Pheromone trapping',
         'Defoliation surveys','iNaturalist', "Lab Collections")

bin <- 2

means_df <- train_data %>% filter(present == 'present', source %in% in2) %>%
  select(all_of(c('period','present','elev2','lat'))) %>% 
  mutate(bin_lat = round(lat/bin)*bin,
         bin_elev = round(elev2/100)*100) %>% 
  mutate(n = 1) %>% 
  group_by(bin_lat,period) %>% 
  summarize(mean_elev = mean(elev2),
            sd_elev = sd(elev2),
            mean_lat = mean(lat),
            sd_lat = sd(lat),
            sum_n = sum(n),
            max_elev = max(elev2),
            min_elev = min(elev2),
            med_elev = median(elev2)) %>% 
  mutate(period2 = ifelse(period == '1985-2010','p1','p2')) %>% 
  select(-period) 

means_df_p1 <- means_df %>% filter(period2 == 'p1') %>% rename(mean_elev_p1 = mean_elev,
                                                               mean_lat_p1 = mean_lat,
                                                               max_elev_p1 = max_elev,
                                                               min_elev_p1 = min_elev,
                                                               med_elev_p1 = med_elev,
                                                               n_p1 = sum_n) %>% select(-c(period2,sd_elev,sd_lat))

means_df_p2 <- means_df %>% filter(period2 == 'p2') %>% rename(mean_elev_p2 = mean_elev,
                                                               mean_lat_p2 = mean_lat,
                                                               max_elev_p2 = max_elev,
                                                               med_elev_p2 = med_elev,
                                                               min_elev_p2 = min_elev,
                                                               n_p2 = sum_n) %>% select(-c(period2,sd_elev,sd_lat))

means_df2 <- merge(means_df_p1,means_df_p2)

means_df2 <- means_df2 %>% group_by(bin_lat) %>% mutate(max_both = max(max_elev_p1,max_elev_p2))

means_df2 %>% mutate(diff_mean = mean_elev_p2 - mean_elev_p1,
                     diff_max = max_elev_p2 - max_elev_p1,
                     diff_min = min_elev_p2 - min_elev_p1) %>%
  summarize(mean_diff = mean(diff_mean),
            sd_diff = sd(diff_mean),
            max_diff = mean(diff_max),
            min_diff = mean(diff_min))


means_df2 %>% arrange(desc(max_elev_p1)) %>% head(1) %>% pull(max_elev_p1)
means_df2 %>% arrange(desc(max_elev_p2)) %>% head(1) %>% pull(max_elev_p2)

means_df2 %>% arrange(min_elev_p1) %>% select(bin_lat,min_elev_p1) %>% head(10)
means_df2 %>% arrange(min_elev_p2) %>% select(bin_lat,min_elev_p2) %>% head(10)

means_df2 %>% mutate(diff_max_elev = max_elev_p2 - max_elev_p1,
                     diff_min_elev = min_elev_p2 - min_elev_p1) %>% select(bin_lat,max_elev_p1, max_elev_p2,diff_max_elev, 
                                                                           min_elev_p1, min_elev_p2,diff_min_elev)

max_latitudes <- train_data %>% filter(present =='present') %>%  group_by(period) %>% filter(lat == max(lat)) %>% 
  select(source,period,lat,lon,elev2)

distm(x = rbind(c(max_latitudes$lon[1], max_latitudes$lat[1]),c(max_latitudes$lon[1], max_latitudes$lat[6])))/1000

min_latitudes <- train_data %>% filter(present =='present') %>%  group_by(period) %>% filter(lat == min(lat)) %>% 
  select(source,period,lat,lon)

distm(x = rbind(c(min_latitudes$lon[1], min_latitudes$lat[1]),c(min_latitudes$lon[1], min_latitudes$lat[2])))/1000

quants <- train_data %>% filter(present == 'present', source %in% in2) %>% mutate(bin_lat = round(lat/2)*2,
                                                                             bin_elev = round(elev2/100)*100) %>% 
  group_by(period, bin_lat) %>% 
  summarize(q1 = quantile(elev2,probs = c(0.1)),
            q5 = quantile(elev2,probs = c(0.5)),
            q9 = quantile(elev2,probs = c(0.9)))

obs_per <- train_data %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                             lon_coord = round(lon/0.25)*0.25,
                                             n = 1) %>% 
  filter(present == 'present', source %in% in2) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_n = sum(n))

obs_per2 <- obs_per %>% mutate(period = ifelse(period == '1985-2010', 'p1','p2')) %>% 
  pivot_wider(names_from = period, values_from = sum_n) %>%  mutate_at(c('p1','p2'), ~replace_na(.,0)) %>% 
  mutate(bin = case_when(p1 >=1 & p2 >=1 ~'Both time periods',
                         p1 <1 & p2 >=1 ~ "2011-2025 only",
                         p1 >=1 & p2 <=1 ~ "1985-2010 only"))

obs_per2 <- obs_per2 %>% mutate(bin = factor(bin, levels = c('Both time periods',"1985-2010 only","2011-2025 only")))

diff_df2 <- means_df2 %>% mutate(diff = round(med_elev_p2 - med_elev_p1,0)) %>% select(bin_lat, diff) %>% 
  mutate(isneg = ifelse(diff  <= 0, TRUE, FALSE)) %>% mutate(label = ifelse(isneg == TRUE, paste0(diff,"m"), paste0("+",diff,"m")))

train_data %>% filter(present == 'present', source %in% in2) %>% group_by(period) %>%  count(present)
plt1 <- train_data %>% filter(present == 'present', source %in% in2) %>% mutate(bin_lat = round(lat/bin)*bin,
                                                                                bin_elev = round(elev2/100)*100) %>%  
  ggplot() + aes(x = bin_lat, y = elev2, group = interaction(period,bin_lat), color = period, fill = period) +
  geom_boxplot(alpha = 0.4) + theme_classic(base_size = 15)  +
  scale_color_brewer("", palette = "Dark2") + 
  scale_fill_brewer("", palette = "Dark2") + 
  ylab("Elevation (m)") + xlab(expression("Latitude")) +
  geom_segment(data = means_df2, aes(x = bin_lat, y = med_elev_p1, xend = bin_lat, yend = med_elev_p2),
               arrow = arrow(length = unit(0.2,'cm')), color = 'black', size = 1, alpha = 1, inherit.aes =FALSE) +
  geom_text(data = diff_df2, aes(x = bin_lat, y = 3900, label = label),
            color = 'black', size = 5, alpha = 1, inherit.aes = FALSE, hjust = 0) +
  theme(legend.position = 'top') +
  # geom_text(data = means_df2, aes(x = bin_lat - 0.45, y = 4300, label = n_p1),
  #           color = '#1b9e77', size = 3.5, alpha = 1, inherit.aes = FALSE, hjust = 0.5) +
  # geom_text(data = means_df2, aes(x = bin_lat + 0.45, y = 4300, label = n_p2),
  #           color = '#d95f02', size = 3.5, alpha = 1, inherit.aes = FALSE, hjust = 0.5) + 
  scale_x_continuous(breaks = c(seq(32,52,2)), labels = function(x) paste0(x, "\u00b0N"), limits = c(31.25,52.75)) + coord_flip() + 
  scale_y_continuous(limits = c(0,4150))

plt2 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey90', color = 'grey35')  + 
  geom_tile(data = obs_per2, aes(x = lon_coord, y = lat_coord, fill = bin)) + 
  theme_classic(base_size = 15) +
  scale_fill_manual("", values = c("1985-2010 only" = "#1b9e77", "2011-2025 only" = '#d95f02', 'Both time periods' = '#FFCC33'))+
  coord_sf(ylim = c(31.25,52.75), xlim = c(-128.125,-103.875))  + 
  theme(legend.position = 'top') + 
  xlab("Longitude") + ylab("Latitude") + 
  scale_y_continuous(breaks = c(seq(32,52,2)))

pdf("figures/option4_iNat2.pdf",height = 8.5, width = 14)
grid.arrange(plt2, plt1,nrow = 1, widths = c(1,1))
dev.off()

##################
##################

means_df_noiNat <- train_data %>% filter(present == 'present', source %in% in1) %>%
  select(all_of(c('period','present','elev2','lat'))) %>% 
  mutate(bin_lat = round(lat/bin)*bin,
         bin_elev = round(elev2/100)*100) %>% 
  mutate(n = 1) %>% 
  group_by(bin_lat,period) %>% 
  summarize(mean_elev = mean(elev2),
            sd_elev = sd(elev2),
            mean_lat = mean(lat),
            sd_lat = sd(lat),
            sum_n = sum(n),
            max_elev = max(elev2),
            min_elev = min(elev2),
            med_elev = median(elev2)) %>% 
  mutate(period2 = ifelse(period == '1985-2010','p1','p2')) %>% 
  select(-period) 

means_df_p1_noiNat <- means_df_noiNat %>% filter(period2 == 'p1') %>% rename(mean_elev_p1 = mean_elev,
                                                               mean_lat_p1 = mean_lat,
                                                               max_elev_p1 = max_elev,
                                                               min_elev_p1 = min_elev,
                                                               med_elev_p1 = med_elev,
                                                               n_p1 = sum_n) %>% select(-c(period2,sd_elev,sd_lat))

means_df_p2_noiNat <- means_df_noiNat %>% filter(period2 == 'p2') %>% rename(mean_elev_p2 = mean_elev,
                                                               mean_lat_p2 = mean_lat,
                                                               max_elev_p2 = max_elev,
                                                               med_elev_p2 = med_elev,
                                                               min_elev_p2 = min_elev,
                                                               n_p2 = sum_n) %>% select(-c(period2,sd_elev,sd_lat))

means_df2_noiNat <- merge(means_df_p1_noiNat,means_df_p2_noiNat)

means_df2_noiNat <- means_df2_noiNat %>% group_by(bin_lat) %>% mutate(max_both = max(max_elev_p1,max_elev_p2))

sum(means_df2$n_p1)
sum(means_df2$n_p2)

means_df2_noiNat %>% mutate(diff_mean = mean_elev_p2 - mean_elev_p1,
                     diff_max = max_elev_p2 - max_elev_p1,
                     diff_min = min_elev_p2 - min_elev_p1) %>%
  summarize(mean_diff = mean(diff_mean),
            sd_diff = sd(diff_mean),
            max_diff = mean(diff_max),
            min_diff = mean(diff_min))


means_df2_noiNat %>% arrange(desc(max_elev_p1)) %>% head(1) %>% pull(max_elev_p1)
means_df2_noiNat %>% arrange(desc(max_elev_p2)) %>% head(1) %>% pull(max_elev_p2)

means_df2_noiNat %>% arrange(min_elev_p1) %>% select(bin_lat,min_elev_p1) %>% head(10)
means_df2_noiNat %>% arrange(min_elev_p2) %>% select(bin_lat,min_elev_p2) %>% head(10)

means_df2_noiNat %>% mutate(diff_max_elev = max_elev_p2 - max_elev_p1,
                     diff_min_elev = min_elev_p2 - min_elev_p1) %>% select(bin_lat,max_elev_p1, max_elev_p2,diff_max_elev, 
                                                                           min_elev_p1, min_elev_p2,diff_min_elev)

max_latitudes_noiNat <- train_data %>% filter(present =='present', source %in% in1) %>%  group_by(period) %>% filter(lat == max(lat)) %>% 
  select(source,period,lat,lon,elev2)

distm(x = rbind(c(max_latitudes_noiNat$lon[1], max_latitudes_noiNat$lat[1]),c(max_latitudes_noiNat$lon[1], max_latitudes_noiNat$lat[6])))/1000

min_latitudes_noiNat <- train_data %>% filter(present =='present', source %in% in1) %>%  group_by(period) %>% filter(lat == min(lat)) %>% 
  select(source,period,lat,lon)

distm(x = rbind(c(min_latitudes_noiNat$lon[1], min_latitudes_noiNat$lat[1]),c(min_latitudes_noiNat$lon[1], min_latitudes_noiNat$lat[4])))/1000

obs_per <- train_data %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                 lon_coord = round(lon/0.25)*0.25,
                                 n = 1) %>% 
  filter(present == 'present', source %in% in1) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_n = sum(n))

obs_per2 <- obs_per %>% mutate(period = ifelse(period == '1985-2010', 'p1','p2')) %>% 
  pivot_wider(names_from = period, values_from = sum_n) %>%  mutate_at(c('p1','p2'), ~replace_na(.,0)) %>% 
  mutate(bin = case_when(p1 >=1 & p2 >=1 ~'Both time periods',
                         p1 <1 & p2 >=1 ~ "2011-2025 only",
                         p1 >=1 & p2 <=1 ~ "1985-2010 only"))

obs_per2 <- obs_per2 %>% mutate(bin = factor(bin, levels = c('Both time periods',"1985-2010 only","2011-2025 only")))

obs_per2 %>% ungroup() %>% count(bin) %>% mutate(p = n/length(obs_per2$lat_coord))


diff_df2_noiNat <- means_df2_noiNat %>% mutate(diff = round(med_elev_p2 - med_elev_p1,0)) %>% select(bin_lat, diff) %>% 
  mutate(isneg = ifelse(diff  <= 0, TRUE, FALSE)) %>% mutate(label = ifelse(isneg == TRUE, paste0(diff,"m"), paste0("+",diff,"m")))

train_data %>% filter(present == 'present', source %in% in1) %>% group_by(period) %>%  count(present)
plt1 <- train_data %>% filter(present == 'present', source %in% in1) %>% mutate(bin_lat = round(lat/bin)*bin,
                                                                                bin_elev = round(elev2/100)*100) %>%  
  ggplot() + aes(x = bin_lat, y = elev2, group = interaction(period,bin_lat), color = period, fill = period) +
  geom_boxplot(alpha = 0.4) + theme_classic(base_size = 15)  +
  scale_color_brewer("", palette = "Dark2") + 
  scale_fill_brewer("", palette = "Dark2") + 
  ylab("Elevation (m)") + xlab(expression("Latitude")) +
  geom_segment(data = means_df2_noiNat, aes(x = bin_lat, y = med_elev_p1, xend = bin_lat, yend = med_elev_p2),
               arrow = arrow(length = unit(0.2,'cm')), color = 'black', size = 1, alpha = 1, inherit.aes =FALSE) +
  geom_text(data = diff_df2_noiNat, aes(x = bin_lat, y = 3900, label = label),
            color = 'black', size = 5, alpha = 1, inherit.aes = FALSE, hjust = 0) +
  theme(legend.position = 'top') +
  scale_x_continuous(breaks = c(seq(32,52,2)), labels = function(x) paste0(x, "\u00b0N"), limits = c(31.25,52.75)) + coord_flip() + 
  scale_y_continuous(limits = c(0,4150))

plt2 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey90', color = 'grey35')  + 
  geom_tile(data = obs_per2, aes(x = lon_coord, y = lat_coord, fill = bin)) + 
  theme_classic(base_size = 15) +
  scale_fill_manual("", values = c("1985-2010 only" = "#1b9e77", "2011-2025 only" = '#d95f02', 'Both time periods' = '#FFCC33'))+
  coord_sf(ylim = c(31.25,52.75), xlim = c(-128.125,-103.875))  + 
  theme(legend.position = 'top') + 
  xlab("Longitude") + ylab("Latitude") + 
  scale_y_continuous(breaks = c(seq(32,52,2)))

pdf("figures/compare_four/option4_noiNat2.pdf", height = 8.5, width = 14)
grid.arrange(plt2, plt1,nrow = 1, widths = c(1,1))
dev.off()
