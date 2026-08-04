library(tidyverse)
library(geosphere)
library(cowplot)
library(gridExtra)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

habitat_features <- read_csv("../landscape/data/all_habitat_features.csv")
population_records <- read_csv("data/population_records_1947-2025.csv")

habitat_features <- habitat_features %>% select(lat,lon, elev2)

habitat_features <- habitat_features %>% mutate(lat = round(lat,5),
                            lon = round(lon,5))
population_records <- population_records %>% mutate(lat = round(lat,5),
                                                lon = round(lon,5))

population_records2 <- merge(population_records, habitat_features, all.x = TRUE)

population_records2[population_records2$manual_id == 50223 & population_records2$source == "Defoliation surveys",]$elev2 <- 2604

population_records2 <- population_records2 %>% mutate(period = case_when(year <= 1984 ~ "Pre-1985",
                                               year >= 1985 & year <= 2010 ~ "1985-2010",
                                               year >= 2011 & year <= 2025 ~ "2011-2025"))


p2 <- 'elev2'
population_records2 %>% 
  select(all_of(c('period',p2))) %>% rename(new_name := !!sym(p2)) %>% 
  ggplot() + aes(x = new_name, y = after_stat(ncount), color = period, fill = period) +
  geom_histogram(position = 'identity', alpha = 0.6) + 
  theme_classic(base_size = 10)+
  #xlim(c(lims$min_val-range,lims$max_val+range)) + 
  ylab("Distribution") + xlab("Parameter value") +
  scale_color_manual("", values = c('1985-2010' = 'black', '2011-2025' = '#fdbb84'))+
  scale_fill_manual("", values = c('1985-2010' = 'black', '2011-2025' = '#fdbb84')) +
  theme(legend.position = 'none')

population_records2 <- population_records2 %>% filter(period != "Pre-1985")

population_records2 %>% count(period)

d1 <- population_records2 %>% filter(period == '1985-2010') %>%
  select(all_of(c('period','elev2'))) 
h1 <- hist(d1$elev2)

in1 <- c('Pheromone trapping',
         'Defoliation surveys', "Lab Collections")
in2 <- c('Pheromone trapping',
         'Defoliation surveys','iNaturalist', "Lab Collections")

bin <- 2

means_df <- population_records2 %>% filter(source %in% in2) %>%
  select(all_of(c('period','elev2','lat'))) %>% 
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

max_latitudes <- population_records2 %>%  group_by(period) %>% filter(lat == max(lat)) %>% 
  select(source,period,lat,lon,elev2)

# keeping the longitude the same, to only get northward expansion
distm(x = rbind(c(max_latitudes$lon[1], max_latitudes$lat[1]),c(max_latitudes$lon[1], max_latitudes$lat[11])))/1000

min_latitudes <- population_records2 %>%  group_by(period) %>% filter(lat == min(lat)) %>% 
  select(source,period,lat,lon)

distm(x = rbind(c(min_latitudes$lon[1], min_latitudes$lat[1]),c(min_latitudes$lon[1], min_latitudes$lat[2])))/1000

quants <- population_records2 %>% filter(source %in% in2) %>% mutate(bin_lat = round(lat/2)*2,
                                                                     bin_elev = round(elev2/100)*100) %>% 
  group_by(period, bin_lat) %>% 
  summarize(q1 = quantile(elev2,probs = c(0.1)),
            q5 = quantile(elev2,probs = c(0.5)),
            q9 = quantile(elev2,probs = c(0.9)))

obs_per <- population_records2 %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                             lon_coord = round(lon/0.25)*0.25,
                                             n = 1) %>% 
  filter(source %in% in2, manual_id %ni% c(50445, 50381)) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_n = sum(n))

obs_per2 <- obs_per %>% mutate(period = ifelse(period == '1985-2010', 'p1','p2')) %>% 
  pivot_wider(names_from = period, values_from = sum_n) %>%  mutate_at(c('p1','p2'), ~replace_na(.,0)) %>% 
  mutate(bin = case_when(p1 >=1 & p2 >=1 ~'Both time periods',
                         p1 <1 & p2 >=1 ~ "2011-2025 only",
                         p1 >=1 & p2 <=1 ~ "1985-2010 only"))

obs_per2 <- obs_per2 %>% mutate(bin = factor(bin, levels = c('Both time periods',"1985-2010 only","2011-2025 only")))

diff_df2 <- means_df2 %>% mutate(diff = round(med_elev_p2 - med_elev_p1,0)) %>% select(bin_lat, diff) %>% 
  mutate(isneg = ifelse(diff  <= 0, TRUE, FALSE)) %>% mutate(label = ifelse(isneg == TRUE, paste0(diff,"m"), paste0("+",diff,"m")))

plt1 <- population_records2 %>% filter(source %in% in2, manual_id %ni% c(50445, 50381)) %>% mutate(bin_lat = round(lat/bin)*bin,
                                                                   bin_elev = round(elev2/100)*100) %>%  
  ggplot() + aes(x = bin_lat, y = elev2, group = interaction(period,bin_lat), color = period, fill = period) +
  geom_boxplot(alpha = 0.4, width = 1.2, position = position_dodge(width = 2.0)) + theme_classic(base_size = 15)  +
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
  scale_x_continuous(breaks = c(seq(32,52,2)), labels = function(x) paste0(x, "\u00b0N"), limits = c(31,53)) + coord_flip() + 
  scale_y_continuous(limits = c(0,4150))

plt2 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey90', color = 'grey35')  + 
  geom_tile(data = obs_per2, aes(x = lon_coord, y = lat_coord, fill = bin)) + 
  theme_classic(base_size = 15) +
  scale_fill_manual("", values = c("1985-2010 only" = "#1b9e77", "2011-2025 only" = '#d95f02', 'Both time periods' = '#FFCC33'))+
  coord_sf(ylim = c(31,53), xlim = c(-128.125,-103.875))  + 
  theme(legend.position = 'top') + 
  xlab("Longitude") + ylab("Latitude") + 
  scale_y_continuous(breaks = c(seq(32,52,2)))

pdf("figures/option4_iNat4.pdf",height = 8.5, width = 14)
grid.arrange(plt2, plt1,nrow = 1, widths = c(1,1))
dev.off()

##################
##################

means_df_noiNat <- population_records2 %>% filter(source %in% in1) %>%
  select(all_of(c('period','elev2','lat'))) %>% 
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

max_latitudes_noiNat <- population_records2 %>% filter(source %in% in1) %>%  group_by(period) %>% filter(lat == max(lat)) %>% 
  select(source,period,lat,lon,elev2)

distm(x = rbind(c(max_latitudes_noiNat$lon[1], max_latitudes_noiNat$lat[1]),c(max_latitudes_noiNat$lon[1], max_latitudes_noiNat$lat[11])))/1000

min_latitudes_noiNat <- population_records2 %>% filter(source %in% in1) %>%  group_by(period) %>% filter(lat == min(lat)) %>% 
  select(source,period,lat,lon)

distm(x = rbind(c(min_latitudes_noiNat$lon[1], min_latitudes_noiNat$lat[1]),c(min_latitudes_noiNat$lon[1], min_latitudes_noiNat$lat[4])))/1000

obs_per <- population_records2 %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                 lon_coord = round(lon/0.25)*0.25,
                                 n = 1) %>% 
  filter(source %in% in1) %>% 
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

plt1 <- population_records2 %>% filter(source %in% in1) %>% mutate(bin_lat = round(lat/bin)*bin,
                                                                                bin_elev = round(elev2/100)*100) %>%  
  ggplot() + aes(x = bin_lat, y = elev2, group = interaction(period,bin_lat), color = period, fill = period) +
  geom_boxplot(alpha = 0.4, width = 1.2, position = position_dodge(width = 2.0)) + theme_classic(base_size = 15)  +
  scale_color_brewer("", palette = "Dark2") + 
  scale_fill_brewer("", palette = "Dark2") + 
  ylab("Elevation (m)") + xlab(expression("Latitude")) +
  geom_segment(data = means_df2_noiNat, aes(x = bin_lat, y = med_elev_p1, xend = bin_lat, yend = med_elev_p2),
               arrow = arrow(length = unit(0.2,'cm')), color = 'black', size = 1, alpha = 1, inherit.aes =FALSE) +
  geom_text(data = diff_df2_noiNat, aes(x = bin_lat, y = 3900, label = label),
            color = 'black', size = 5, alpha = 1, inherit.aes = FALSE, hjust = 0) +
  theme(legend.position = 'top') +
  scale_x_continuous(breaks = c(seq(32,52,2)), labels = function(x) paste0(x, "\u00b0N"), limits = c(31,53)) + coord_flip() + 
  scale_y_continuous(limits = c(0,4150))

plt2 <- ggplot() + geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey90', color = 'grey35')  + 
  geom_tile(data = obs_per2, aes(x = lon_coord, y = lat_coord, fill = bin)) + 
  theme_classic(base_size = 15) +
  scale_fill_manual("", values = c("1985-2010 only" = "#1b9e77", "2011-2025 only" = '#d95f02', 'Both time periods' = '#FFCC33'))+
  coord_sf(ylim = c(31,53), xlim = c(-128.125,-103.875))  + 
  theme(legend.position = 'top') + 
  xlab("Longitude") + ylab("Latitude") + 
  scale_y_continuous(breaks = c(seq(32,52,2)))

pdf("figures/option4_noiNat2.pdf", height = 8.5, width = 14)
grid.arrange(plt2, plt1,nrow = 1, widths = c(1,1))
dev.off()


#################
#################

pdf("figures/survey_methods.pdf", height = 6, width = 12)
population_records2 %>% filter(manual_id %ni% c(50445, 50381)) %>%
  mutate(type = ifelse(source == "iNaturalist", "iNaturalist", "Surveys")) %>%
  group_by(lat_coord,lon_coord, period)  %>% count(type) %>% pivot_wider(values_from = n, names_from = type) %>% 
  mutate(iNaturalist = ifelse(is.na(iNaturalist), 0, iNaturalist),
         Surveys = ifelse(is.na(Surveys), 0, Surveys)) %>% 
  mutate(iNat_pres = ifelse(iNaturalist>0,1,0),
         Survey_pres = ifelse(Surveys>0,1,0)) %>% 
  mutate(type2 = case_when(iNat_pres == 0 & Survey_pres == 1~ "Survey methods only",
                           iNat_pres == 1 & Survey_pres == 0 ~ "iNaturalist only",
                           iNat_pres == 1 & Survey_pres == 1 ~ "Both methods",
                           iNat_pres == 0 & Survey_pres == 0 ~ "Neither method")) %>% 
  ggplot() +
  geom_sf(data = all_geo2, aes(geometry = geometry), fill = 'grey94', color = 'grey35', inherit.aes = FALSE) +
  geom_tile(aes(x = lon_coord, y= lat_coord, color = type2, fill = type2)) +
  theme_classic(base_size = 15) + 
  facet_wrap(~period) +
  scale_color_brewer("", palette = "Set1") +
  scale_fill_brewer("", palette = "Set1") +
  coord_sf(ylim = c(31,53), xlim = c(-128.125,-103.875)) +
  ylab("Latitude") + xlab("Longitude")
dev.off()
