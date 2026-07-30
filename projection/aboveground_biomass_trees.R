library(tidyverse)

pseu_can <- read_csv("data/AGB_Pseudotsuga_grid.csv")
abies_can <- read_csv("data/AGB_Abies_grid.csv")
pseu_can$species <- "Pseudotsuga menziesii"
abies_can$species <- "Abies amabilis, A. grandis, and A. lasiocarpa"

pseu_can$genus <- "Pseudotsuga"
abies_can$genus <- "Abies"

canada <- rbind(pseu_can, abies_can)

# dataset units are metric tonnes per hectare
# dataset is 250 * 250 meters, which is 6.25 hectares
# output is metric tonnes
# 1 metric tonne = 1 megagram
canada <- canada %>% drop_na(`_sum`) %>% mutate(country = "Canada") %>% mutate(sum2 = `_sum`*6.25) 

pseu_usa <- read_csv("data/USA_Douglas-fir.csv")
pseu_usa$species <- "Pseudotsuga menziesii"
pseu_usa$genus <- "Pseudotsuga"

grandfir_usa <- read_csv("data/USA_Grandfir.csv")
grandfir_usa$species <- "Abies grandis"
grandfir_usa$genus <- "Abies"

subalpinefir <- read_csv("data/USA_Subalpinefir.csv")
subalpinefir$species <- "Abies lasiocarpa"
subalpinefir$genus <- "Abies"

whitefir <- read_csv("data/USA_Whitefir.csv")
whitefir$species <- "Abies concolor"
whitefir$genus <- "Abies"

pacificsilver <- read_csv("data/USA_PacificSilverfir.csv")
pacificsilver$species <- "Abies concolor"
pacificsilver$genus <- "Abies"

corkbarkfir <- read_csv("data/USA_Corkbarkfir.csv")
corkbarkfir$species <- "Abies lasiocarpa arizonica"
corkbarkfir$genus <- "Abies"

calired <- read_csv("data/USA_CaliRed.csv")
calired$species <- "Abies magnifica"
calired$genus <- "Abies"

shastared <- read_csv("data/USA_ShastaRed.csv")
shastared$species <- "Abies shastensis"
shastared$genus <- "Abies"

noblefir <- read_csv("data/USA_Noblefir.csv")
noblefir$species <- "Abies procera"
noblefir$genus <- "Abies"

usa <- rbind(pseu_usa, grandfir_usa, subalpinefir, whitefir, pacificsilver, corkbarkfir, calired, shastared, noblefir)

# dataset units are US tons per acre
# dataset is 30 * 30 meters, which is 0.222395 acres hectares
# 1 US ton is 0.907185 metric tonnes
# output is metric tonnes
# 1 metric tonne = 1 megagram
usa <- usa %>% drop_na(`_sum`) %>% mutate(country = "United States") %>% mutate(sum2 = `_sum`*0.222395*0.907185)

both <- rbind(canada, usa)

both %>% group_by(left,top,right,bottom, genus) %>% summarize(sum3 = sum(sum2, na.rm=TRUE)) %>% 
  ggplot() + aes(x = left + 0.125, y = top - 0.125, fill = `sum3`) + geom_tile() +
  theme_classic() +
  scale_fill_gradient(low= "white", high = "darkgreen") + 
  facet_wrap(~genus)

both %>% filter(sum2 >0) %>%
  group_by(left,top,right,bottom, species, genus) %>% summarize(sum3 = sum(sum2, na.rm=TRUE)) %>% 
  ggplot() + aes(x = left + 0.125, y = top - 0.125, fill = `sum3`) + geom_tile() +
  theme_classic() +
  scale_fill_viridis_c(option = "turbo") + 
  #scale_fill_gradient(low= "white", high = "darkgreen") + 
  facet_wrap(~species)

write_csv(both, "../projection/data/biomass_by_genus_grid.csv")

###################
###################
###################

pseu_us_acres <- read_csv("data/DouglasFir_pres_USA_grid_10.csv")
abies_us_acres <- read_csv("data/Abies_pres_USA_grid_10.csv")
host_us_acres <- read_csv("data/HostTrees_pres_USA_grid_10.csv")

pseu_can_acres <- read_csv("data/DouglasFir_pres_CAN_grid_22.csv")
abies_can_acres <- read_csv("data/Abies_pres_CAN_grid_22.csv")
host_can_acres <- read_csv("data/HostTrees_pres_CAN_grid_22.csv")

pseu_us_acres %>% drop_na(`_mean`) %>% 
  ggplot() +
  aes(x = left + 0.125, y = top - 0.125, color = `_mean`*acres2, fill = `_mean`*acres2) + geom_tile() +
  theme_classic() +
    scale_color_gradient(high = 'darkgreen', low = 'grey95')+
    scale_fill_gradient(high = 'darkgreen', low = 'grey95')

pseu_us_acres <- pseu_us_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_DF = `_count`, sum_DF = `_sum`, mean_DF = `_mean`) %>% 
  select(id,lat,lon,acres2, count_DF, sum_DF, mean_DF)

abies_us_acres <- abies_us_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_AB = `_count`, sum_AB = `_sum`, mean_AB = `_mean`) %>% 
  select(id,lat,lon,acres2, count_AB, sum_AB, mean_AB)

host_us_acres <- host_us_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_T = `_count`, sum_T = `_sum`, mean_T = `_mean`) %>% 
  select(id,lat,lon,acres2, count_T, sum_T, mean_T)

host_us_acres2 <- merge(host_us_acres, pseu_us_acres)
host_us_acres2 <- merge(host_us_acres2, abies_us_acres)
  
host_us_acres2 <- host_us_acres2 %>% drop_na(count_T) %>% 
  mutate(mean_TPA = mean_DF + mean_AB)

host_us_acres2 %>%
  ggplot() +
  aes(x = lon, y = lat, color = mean_TPA - mean_T, fill =  mean_TPA - mean_T) +
  geom_tile() +
  theme_classic() +
  scale_color_gradient(high = 'darkgreen', low = 'grey95')+
  scale_fill_gradient(high = 'darkgreen', low = 'grey95')

host_us_acres2 %>%
  ggplot() +
  aes(x = mean_DF, y = mean_AB) +
  geom_point() +
  theme_classic() 

pseu_can_acres <- pseu_can_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_DF = `_count`, sum_DF = `_sum`, mean_DF = `_mean`) %>% 
  select(id,lat,lon,acres2, count_DF, sum_DF, mean_DF)

abies_can_acres <- abies_can_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_AB = `_count`, sum_AB = `_sum`, mean_AB = `_mean`) %>% 
  select(id,lat,lon,acres2, count_AB, sum_AB, mean_AB)

host_can_acres <- host_can_acres %>% mutate(lon = left + 0.125, lat = top - 0.125) %>% 
  rename(count_T = `_count`, sum_T = `_sum`, mean_T = `_mean`) %>% 
  select(id,lat,lon,acres2, count_T, sum_T, mean_T)

host_can_acres2 <- merge(host_can_acres, pseu_can_acres)
host_can_acres2 <- merge(host_can_acres2, abies_can_acres)

host_can_acres2 <- host_can_acres2 %>% drop_na(count_T) %>% 
  mutate(mean_TPA = mean_DF + mean_AB)

host_can_acres2 %>% drop_na(mean_T) %>% 
  ggplot() +
  aes(x = lon, y = lat, color = mean_TPA - mean_T, fill =  mean_TPA - mean_T) +
  geom_tile() +
  theme_classic() +
  scale_color_gradient(high = 'darkgreen', low = 'grey95')+
  scale_fill_gradient(high = 'darkgreen', low = 'grey95')

host_can_acres2$ds <- "Canada"
host_us_acres2$ds <- "United States"

host_all_acres <- rbind(host_can_acres2,host_us_acres2)

host_all_acres <- host_all_acres %>% drop_na(mean_T)

non_overlapping <- host_all_acres %>% mutate(n = 1) %>% group_by(lat,lon) %>% mutate(sum_n = sum(n)) %>% 
  filter(sum_n == 1) %>% mutate(total_acres = acres2*mean_T, pseu_acres = mean_DF*acres2, 
                            abies_acres = acres2*mean_AB) %>% 
  select(lat,lon,total_acres,pseu_acres,abies_acres)

overlapping <- host_all_acres %>% mutate(n = 1) %>% group_by(lat,lon) %>% mutate(sum_n = sum(n))%>% 
  filter(sum_n > 1) %>% mutate(total_acres = acres2*mean_T, pseu_acres = mean_DF*acres2, 
                           abies_acres = acres2*mean_AB) %>% 
  filter(ds != "United States") %>% 
  select(lat,lon,total_acres,pseu_acres,abies_acres)

host_all_acres2 <- rbind(non_overlapping, overlapping)

host_all_acres %>% mutate(n = 1) %>% group_by(lat,lon) %>% mutate(sum_n = sum(n))%>% 
  filter(sum_n > 1) %>% mutate(total_acres = acres2*mean_T, pseu_acres = mean_DF*acres2, 
                               abies_acres = acres2*mean_AB) %>% 
  ggplot() +
  aes(x = lon, y = lat, color = total_acres, fill =  total_acres) +
  geom_tile() +
  theme_classic() +
  scale_color_gradient(high = 'darkgreen', low = 'grey95')+
  scale_fill_gradient(high = 'darkgreen', low = 'grey95') +
  facet_wrap(~ds)

host_all_acres2 %>% 
  ggplot() +
  aes(x = lon, y = lat, color = total_acres, fill =  total_acres) +
  geom_tile() +
  theme_classic() +
  scale_color_gradient(high = 'darkgreen', low = 'grey95')+
  scale_fill_gradient(high = 'darkgreen', low = 'grey95') 

write_csv(host_all_acres2, "data/acres_by_genus_10.csv")