library(tidyverse)
library(gridExtra)
library(scico)
library(sf)
library(cowplot)

all_geo2 <- st_read("../landscape/gadm/all_geo2.shp")

forested_agb <- read_csv('../projection/data/biomass_by_genus_grid.csv')

forested_agb_genus <- forested_agb %>% mutate(lat = top - 0.125, lon = left + 0.125) %>% 
  group_by(lat,lon, genus) %>% summarize(total_agb = sum(sum2, na.rm=TRUE))

forested_agb_wide <- forested_agb_genus %>% pivot_wider(names_from = genus, values_from = total_agb) %>% 
  mutate(Pseudotsuga = ifelse(is.na(Pseudotsuga),0,Pseudotsuga)) %>% 
  mutate(total_agb = Abies + Pseudotsuga)

#############
#
# Graph for conceptual figure
#
#############

states <- read_csv("../anthropogenic/data/gridded_states.csv")

forested_states <- merge(forested_agb_wide, states)

test_states <- forested_states %>% group_by(state) %>% summarize(sum_Abies = sum(Abies),
                                                                 sum_Pseudotsuga = sum(Pseudotsuga))

pdf("../conceptual/figures/agb_host.pdf",height = 5.2, width = 4.3)
forested_states %>% filter(country %in% c("United States of America", "Canada")) %>% 
  ggplot() + aes(x = lon, y = lat, color = total_agb/1000000, fill = total_agb/1000000,) + geom_tile() +
  theme_classic(base_size = 12) + 
  scale_color_gradient2("", low = 'cornsilk', mid = "darkgreen", high = 'midnightblue', midpoint = 8, breaks = c(0,5,10,15), limits = c(0,16.1)) +
  scale_fill_gradient2("", low = 'cornsilk', mid = "darkgreen", high = 'midnightblue', midpoint = 8, breaks = c(0,5,10,15), limits = c(0,16.1)) +
  geom_sf(data = all_geo2, aes(geometry = geometry), color = 'grey35', fill = NA, inherit.aes = FALSE) +
  coord_sf(ylim = c(32,53.325), xlim = c(-128.125,-103.875))+ 
  theme(legend.position = 'top',
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm")) + 
  xlab("Longitude") + ylab("Latitude")
dev.off()

plot_grid(plt_map, plot_grid(biomass_plt,acres_plt,nrow = 2, align = "v"), align = 'h')

#############
#
# Graph for climate projections
#
#############

binned_all <- read_csv("/Volumes/My Book/Synchrony/projections/summary_all_synth.csv")

binned_all <- binned_all %>% filter(proj == "Psuedo-absence only") 

proj_acres <- merge(binned_all, forested_agb_wide,by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'))

cc_summary <- proj_acres %>% group_by(year,model,trees_change,bin,lag,fitting,proj) %>%
  summarize(sum_agb = sum(total_agb)) %>% rename(pres = bin)

population_records <- read_csv("../population_data/data/population_records_1947-2025.csv")

pop_counts <- population_records %>% mutate(lat_coord = round(lat/0.25)*0.25,
                                            lon_coord = round(lon/0.25)*0.25) %>% filter(year >= 1985) %>% 
  mutate(period = ifelse(year <= 2010,'1985-2010','2011-2025')) %>% 
  group_by(lat_coord,lon_coord,period) %>% summarize(sum_count = sum(count))

hist_period <- merge(pop_counts, forested_agb_wide,by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'))

hist_period %>% mutate(n = ifelse(sum_count>0,1,0)) %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = total_agb, fill = total_agb) + geom_tile() + 
  theme_classic() + facet_wrap(~period) +
  scale_color_gradient("Acres of forest", low = 'white', high = 'darkgreen') +
  scale_fill_gradient("Acres of forest", low = 'white', high = 'darkgreen') 

hist_summary2 <- hist_period %>% group_by(period) %>% summarize(sum_agb = sum(total_agb)) %>%
  mutate(model = 'Historical',trees_change = 'No forest change',pres = 1) %>% 
  mutate(year = ifelse(period == '1985-2010', 1997.5,2018), lag = NA, fitting = NA, proj = NA) %>% select(-period)

summary_all2 <- rbind(hist_summary2,cc_summary)

cc_mod <- cc_summary %>% filter(pres == 1) %>% group_by(year,trees_change,lag,fitting,proj) %>%
  summarize(avg_agb = mean(sum_agb),
            sd_agb = sd(sum_agb))

hist_summary3 <- hist_period %>% group_by(period) %>% summarize(avg_agb = sum(total_agb)) %>%
  mutate(trees_change = 'No forest change',pres = 1) %>% 
  mutate(year = ifelse(period == '1985-2010', 1997.5,2018), lag = 0, fitting = "Historical", sd = NA, proj = "Historical") %>% select(-period)

summary_all3 <- rbind(cc_mod,hist_summary3)

summary_all4 <- summary_all3 %>% mutate(trees_change = ifelse(year %in% c(1997.5,2018), "Historical data",trees_change),
                        lag = ifelse(year %in% c(1997.5,2018), -1,lag)) %>% 
  mutate(name2 = ifelse(lag == 0, "Current weather variables only", "Current + 35-year lagged variables")) %>% 
  mutate(name3 = paste0(name2, " (",fitting,")"))  %>% 
  mutate(name3 = ifelse(year %in% c(1997.5,2018), "Historical data",name3))

pdf("figures/agb/aboveground_biomass_4mod.pdf",height = 6, width = 14)
plt1 <- summary_all4 %>% filter(trees_change %in% c("No forest change", "Historical data")) %>% 
  ggplot() + aes(x = year, y = avg_agb, group = interaction(lag,fitting), 
                 color = name3) + 
  geom_point(size = 2) + 
  geom_line(size = 1)+
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Aboveground Biomass at Risk (Mg)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Current weather variables only (1985-2010)' = '#a6cee3',
                                        'Current weather variables only (1985-2025)' = '#1f78b4',
                                        'Current + 35-year lagged variables (1985-2010)' = '#fb9a99',
                                        'Current + 35-year lagged variables (1985-2025)' = '#e31a1c')) +
  scale_y_continuous(limits = c(0,3e9), breaks = c(0, 1e9, 2e9, 3e9)) + 
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_linetype_manual("Forest projection", values = c("Including forest change" = 'dashed', "No forest change" = 'solid')) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) + 
  ggtitle("Without forest change")

plt2 <- summary_all4 %>% filter(trees_change != "No forest change") %>% 
  mutate(name2 = ifelse(lag == 0, "Current weather variables only", "Current + 35-year lagged variables")) %>% 
  mutate(name3 = paste0(name2, " (",fitting,")"))  %>% 
  mutate(name3 = ifelse(year %in% c(1997.5,2018), "Historical data",name3))  %>% 
  #filter(name3 %in% c('Projection with 35 year lag (1985-2025)','Projection with no (1985-2025)','Historical data')) %>%
  ggplot() + aes(x = year, y = avg_agb, group = interaction(lag,fitting), 
                 color = name3, linetype = trees_change) + 
  geom_point(size = 2) + 
  geom_line(size = 1)+
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Aboveground Biomass at Risk (Mg)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Current weather variables only (1985-2010)' = '#a6cee3',
                                        'Current weather variables only (1985-2025)' = '#1f78b4',
                                        'Current + 35-year lagged variables (1985-2010)' = '#fb9a99',
                                        'Current + 35-year lagged variables (1985-2025)' = '#e31a1c')) +
  scale_y_continuous(limits = c(0,3e9), breaks = c(0, 1e9, 2e9, 3e9)) + 
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_linetype_manual("Forest projection",
                        values = c("Including forest change" = 'dashed', "Historical data" = 'solid')) +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.65,0.15),
        legend.title = element_blank(),
        legend.key.height = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) + 
  ggtitle("With forest change") + 
  guides(linetype = 'none')

grid.arrange(plt1,plt2,nrow = 1)
dev.off()

cc_summary_best_mod <- cc_summary %>% filter(pres == 1, lag == 35, fitting == "1985-2025", trees_change == "No forest change") %>% 
  mutate(proj2 = ifelse(proj == "Historical", "Historical data", "Climate model projection"))

  
means_renamed <- summary_all3 %>% filter(trees_change != c("Including forest change"),
                                         fitting %in% c("1985-2025","Historical")) %>% 
  mutate(lag = ifelse(proj == "Historical", -1, lag)) %>% filter(lag %in% c(-1, 35)) %>% 
  mutate(proj2 = ifelse(proj == "Historical", "Historical data", "Climate model projection"))

biomass_plt <- means_renamed %>% 
  ggplot() + 
  geom_line(aes(x = year, y = avg_agb, group = proj2, 
                color = proj2), alpha = 0.75, size = 1) + 
  geom_point(data = cc_summary_best_mod,
             aes(x = year, y = sum_agb, color = proj2),
             size = 1.5, alpha = 0.5) +
  geom_point(aes(x = year, y = avg_agb, color = proj2), size = 3, alpha = 1) + 
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Aboveground Biomass at Risk (Mg)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Climate model projection' = 'violetred4')) +
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_y_continuous(limits = c(0,3020211180)) + 
  scale_shape_manual("Forest projection", values = c('Historical data' = 15,
                                                     'No forest change' = 16), guide = 'none') +
  theme(legend.position = 'top',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

biomass_plt

means_renamed %>% mutate(ref = means_renamed[means_renamed$year == 2018,]$avg_agb) %>% 
  mutate(delta = avg_agb/ref) %>% select(year,trees_change,lag,avg_agb, ref, delta)

cc_summary_best_mod %>% filter(year == 2100) %>% arrange(desc(sum_agb))

#####################
#####################
#####################
#####################

forested_acres <- read_csv("data/acres_by_genus_10.csv")

forested_acres %>% 
  ggplot() +
  aes(x = lon, y = lat, color = abies_acres, fill =  abies_acres) +
  geom_tile() +
  theme_classic() +
  scale_color_gradient(high = 'darkgreen', low = 'grey95')+
  scale_fill_gradient(high = 'darkgreen', low = 'grey95') 

forested_acres <- forested_acres %>% mutate(combo_acres = pseu_acres + abies_acres) %>% 
  mutate(overlap = combo_acres - total_acres) %>% 
  mutate(DF_alone = pseu_acres - overlap, 
         AB_alone = abies_acres - overlap)


hist_acres <- merge(pop_counts, forested_acres,by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'))

hist_acres %>% mutate(n = ifelse(sum_count>0,1,0)) %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = total_acres, fill = total_acres) + geom_tile() + 
  theme_classic() + facet_wrap(~period) +
  scale_color_gradient("Acres of forest", low = 'white', high = 'darkgreen') +
  scale_fill_gradient("Acres of forest", low = 'white', high = 'darkgreen') 

hist_acres2 <- hist_acres %>% group_by(period) %>% summarize(sum_acres = sum(total_acres),
                                                             pseu_acres = sum(pseu_acres),
                                                             abies_acres = sum(abies_acres)) %>%
  mutate(model = 'Historical',trees_change = 'No forest change',pres = 1) %>% 
  mutate(year = ifelse(period == '1985-2010', 1997.5,2018), lag = NA, fitting = NA, proj = NA) %>% select(-period)

proj_acres2 <- merge(binned_all, forested_acres,by.x = c('lat_coord','lon_coord'), by.y = c('lat','lon'))

write_csv(proj_acres2, 'output/agb/projected_acres_synth_all_10.csv')

proj_acres2 %>% 
  filter(trees_change == 'Including forest change',model == 'ACCESS-ESM1-5', lag == 35, fitting == '1985-2025') %>%
  ggplot() + aes(x = lon_coord, y = lat_coord, color = total_acres, fill = total_acres) + geom_tile() + 
  theme_classic() + facet_wrap(~pres) +
  scale_color_gradient("Acres of forest", low = 'white', high = 'darkgreen') +
  scale_fill_gradient("Acres of forest", low = 'white', high = 'darkgreen') 

cc_summary_acres <- proj_acres2 %>% group_by(year,model,trees_change,bin,lag,fitting,proj) %>%
  summarize(sum_acres = sum(total_acres),
            pseu_acres = sum(pseu_acres),
            abies_acres = sum(abies_acres)) %>% rename(pres = bin)

summary_acres2 <- rbind(hist_acres2,cc_summary_acres)
summary_acres2 %>% filter(pres == 1) %>%
  ggplot() + aes(x = year, y = abies_acres/1000000, group = trees_change, color = model, shape = trees_change) + 
  geom_point(size = 2) + theme_classic()  +
  xlab("Year") + ylab("Million Acres of Forested Area at Risk")

cc_mod_acres <- cc_summary_acres %>% filter(pres == 1) %>% group_by(year,trees_change,lag,fitting,proj) %>%
  summarize(avg_acres = mean(sum_acres),
            sd_acres = sd(sum_acres))

hist_acres3 <- hist_acres %>% group_by(period) %>% summarize(avg_acres = sum(total_acres)) %>%
  mutate(trees_change = 'No forest change',pres = 1) %>% 
  mutate(year = ifelse(period == '1985-2010', 1997.5,2018), lag = 0, fitting = "Historical", sd = NA, proj = "Historical") %>% select(-period)

summary_acres3 <- rbind(cc_mod_acres,hist_acres3)

summary_acres4 <- summary_acres3 %>% mutate(trees_change = ifelse(year %in% c(1997.5,2018), "Historical data",trees_change),
                                        lag = ifelse(year %in% c(1997.5,2018), -1,lag)) %>% 
  mutate(name2 = ifelse(lag == 0, "Current weather variables only", "Current + 35-year lagged variables")) %>% 
  mutate(name3 = paste0(name2, " (",fitting,")"))  %>% 
  mutate(name3 = ifelse(year %in% c(1997.5,2018), "Historical data",name3))

pdf("figures/agb/host_trees_acres_4mod_10.pdf",height = 4, width = 8)
summary_acres3 %>% filter(trees_change == "No forest change") %>% 
  mutate(name2 = ifelse(lag == 0, "Projection with no lag", "Projection with 35 year lag")) %>% 
  mutate(name3 = paste0(name2, " (",fitting,")"))  %>% 
  mutate(name3 = ifelse(year %in% c(1997.5,2018), "Historical data",name3))  %>% 
  #filter(name3 %in% c('Projection with 35 year lag (1985-2025)','Projection with no (1985-2025)','Historical data')) %>%
  ggplot() + aes(x = year, y = avg_acres/1000000, group = interaction(lag,fitting), 
                 color = name3) + 
  geom_point(size = 2) + 
  geom_line(size = 1)+
  theme_classic(base_size = 10)  +
  xlab("Year") + ylab("Host Tree Forests at Risk (Million Acres)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Projection with no lag (1985-2010)' = '#a6cee3',
                                        'Projection with no lag (1985-2025)' = '#1f78b4',
                                        'Projection with 35 year lag (1985-2010)' = '#fb9a99',
                                        'Projection with 35 year lag (1985-2025)' = '#e31a1c')) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,80)) + 
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_linetype_manual("Forest projection", values = c("Including forest change" = 'dashed', "No forest change" = 'solid')) +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.75,0.25),
        legend.title = element_blank(),
        legend.key.height = unit(0.75, "lines"))
dev.off()

pdf("figures/agb/host_trees_acres_4mod_10.pdf",height = 6, width = 14)
plt1 <- summary_acres4 %>% filter(trees_change %in% c("No forest change", "Historical data")) %>% 
  ggplot() + aes(x = year, y = avg_acres/1000000, group = interaction(lag,fitting), 
                 color = name3) + 
  geom_point(size = 2) + 
  geom_line(size = 1)+
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Host Tree Forests at Risk (Million Acres)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Current weather variables only (1985-2010)' = '#a6cee3',
                                        'Current weather variables only (1985-2025)' = '#1f78b4',
                                        'Current + 35-year lagged variables (1985-2010)' = '#fb9a99',
                                        'Current + 35-year lagged variables (1985-2025)' = '#e31a1c')) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,80)) + 
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_linetype_manual("Forest projection", values = c("Including forest change" = 'dashed', "No forest change" = 'solid')) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) + 
  ggtitle("Without forest change")

plt2 <- summary_acres4 %>% filter(trees_change != "No forest change") %>% 
  mutate(name2 = ifelse(lag == 0, "Current weather variables only", "Current + 35-year lagged variables")) %>% 
  mutate(name3 = paste0(name2, " (",fitting,")"))  %>% 
  mutate(name3 = ifelse(year %in% c(1997.5,2018), "Historical data",name3))  %>% 
  #filter(name3 %in% c('Projection with 35 year lag (1985-2025)','Projection with no (1985-2025)','Historical data')) %>%
  ggplot() + aes(x = year, y = avg_acres/1000000, group = interaction(lag,fitting), 
                 color = name3, linetype = trees_change) + 
  geom_point(size = 2) + 
  geom_line(size = 1)+
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Host Tree Forests at Risk (Million Acres)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Current weather variables only (1985-2010)' = '#a6cee3',
                                        'Current weather variables only (1985-2025)' = '#1f78b4',
                                        'Current + 35-year lagged variables (1985-2010)' = '#fb9a99',
                                        'Current + 35-year lagged variables (1985-2025)' = '#e31a1c')) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,80)) + 
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_linetype_manual("Forest projection",
                        values = c("Including forest change" = 'dashed', "Historical data" = 'solid')) +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.65,0.15),
        legend.title = element_blank(),
        legend.key.height = unit(0.75, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) + 
  ggtitle("With forest change") + 
  guides(linetype = 'none')

grid.arrange(plt1,plt2,nrow = 1)
dev.off()

cc_acres_best_mod <- cc_summary_acres %>% filter(pres == 1, lag == 35, fitting == "1985-2025", trees_change == "No forest change") %>% 
  mutate(proj2 = ifelse(proj == "Historical", "Historical data", "Climate model projection"))


acres_renamed <- summary_acres3 %>% filter(trees_change != c("Including forest change"),
                                         fitting %in% c("1985-2025","Historical")) %>% 
  mutate(lag = ifelse(proj == "Historical", -1, lag)) %>% filter(lag %in% c(-1, 35)) %>% 
  mutate(proj2 = ifelse(proj == "Historical", "Historical data", "Climate model projection"))

acres_plt <- acres_renamed %>% 
  ggplot() + 
  geom_line(aes(x = year, y = avg_acres/1000000, group = proj2, 
                color = proj2), alpha = 0.75, size = 1) + 
  geom_point(data = cc_acres_best_mod,
             aes(x = year, y = sum_acres/1000000, color = proj2),
             size = 1.5, alpha = 0.5) +
  geom_point(aes(x = year, y = avg_acres/1000000, color = proj2), size = 3, alpha = 1) + 
  theme_classic(base_size = 15)  +
  xlab("Year") + ylab("Host Forests at Risk (Million Acres)") +
  scale_color_manual("Data", values = c('Historical data' = 'grey55',
                                        'Climate model projection' = 'violetred4')) +
  scale_x_continuous(breaks = c(1997.5,2018,2040,2050,2060,2070,2080,2090,2100),
                     labels = c('1985-2010', '2011-2025',2040,2050,2060,2070,2080,2090,2100)) +
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,92)) + 
  scale_shape_manual("Forest projection", values = c('Historical data' = 15,
                                                     'No forest change' = 16), guide = 'none') +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

acres_plt

acres_renamed %>% mutate(ref = acres_renamed[acres_renamed$year == 2018,]$avg_acres) %>% 
  mutate(delta = avg_acres/ref) %>% select(year,trees_change,lag,avg_acres, ref, delta)

cc_acres_best_mod %>% filter(year == 2100) %>% arrange(desc(sum_acres))

pdf("figures/agb/biomass_and_acres2.pdf",height = 10, width = 4.5)
plot_grid(biomass_plt,acres_plt,nrow = 2, align = "hv", rel_heights = c(1,1))
dev.off()


