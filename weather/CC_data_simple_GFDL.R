library(tidyverse)
library(gridExtra)
library(lubridate)

year1 <- 2090
year2 <- year1 + 10
mod_dir <- paste0('/Volumes/My Book/Climate/CMIP6/GFDL-ESM4-ssp585_',year1,'/')

gdd_all <- read_csv(paste0(mod_dir, "downsampled_int/ALL_season_gdd_",year1, "-", year2, "_GFDL-ESM4.csv"))
#gdd <- read_csv(paste0(mod_dir, "summary_stats_int/gdd_",year1, "-", year2, "_GFDL-ESM4.csv"))

rh_means <- read_csv(paste0(mod_dir, "downsampled_int/means_hurs_",year1, "-", year2, "_GFDL-ESM4.csv"))
pr_means <- read_csv(paste0(mod_dir, "downsampled_int/means_pr_",year1, "-", year2, "_GFDL-ESM4.csv"))
tavg_means <- read_csv(paste0(mod_dir, "downsampled_int/means_tas_",year1, "-", year2, "_GFDL-ESM4.csv"))
tmin_means <- read_csv(paste0(mod_dir, "downsampled_int/means_tmin_",year1, "-", year2, "_GFDL-ESM4.csv"))
tmax_means <- read_csv(paste0(mod_dir, "downsampled_int/means_tmax_",year1, "-", year2, "_GFDL-ESM4.csv"))

average_temps <- tavg_means %>% group_by(lat,lon,year) %>% summarize(mean_temp = mean(tas_mean,na.rm=TRUE))

write_csv(average_temps, paste0("/Volumes/My Book/Synchrony/_climatedata/summary_stats/mean_temp_",year1,'-',year2,".csv"))

# rh <- read_csv(paste0(mod_dir, "downsampled_int/ALL_hum_",year1, "-", year2, "_GFDL-ESM4.csv"))
# pr <- read_csv(paste0(mod_dir, "downsampled_int/ALL_pr_",year1, "-", year2, "_GFDL-ESM4.csv"))
# tavg <- read_csv(paste0(mod_dir, "downsampled_int/ALL_tas_",year1, "-", year2, "_GFDL-ESM4.csv"))
# tmin <- read_csv(paste0(mod_dir, "downsampled_int/ALL_tasmin_",year1, "-", year2, "_GFDL-ESM4.csv"))
# tmax <- read_csv(paste0(mod_dir, "downsampled_int/ALL_tasmax_",year1, "-", year2, "_GFDL-ESM4.csv"))

unique(tavg_means$lat)
unique(tavg_means$lon)

all_means <- merge(rh_means,pr_means)
all_means <- merge(all_means,tavg_means)
all_means <- merge(all_means,tmin_means)
all_means <- merge(all_means,tmax_means)

all_means <- all_means %>% mutate(tas_mean = tas_mean - 273.15,
                                  tmin_mean = tmin_mean - 273.15,
                                  tmax_mean = tmax_mean - 273.15,
                                  sum_pr = sum_pr*86.4) %>% select(-`...1`) %>% mutate(lon = lon - 360)


all_means %>% filter(month == 'July', year == year1) %>% ggplot() +
  aes(x = lon, y = lat, color = tas_mean, fill = tas_mean) + geom_tile() + theme_classic()


gdd_season <- gdd_all %>% select(lat,lon,year,julian,gdd_season) %>% mutate(lon = lon -360)

all_means_tm <- all_means #%>% mutate(year = ifelse(month %in% c("October","November",'December'),year +1,year))

simple_mod <- all_means_tm %>% group_by(lat,lon,year) %>% summarize(sum_tp = sum(sum_pr),
                                                                    min_t2m = min(tas_mean),
                                                                    max_t2m = max(tas_mean),
                                                                    min_rh = min(hurs_mean),
                                                                    max_rh = max(hurs_mean),
                                                                    max_tp = max(sum_pr),
                                                                    min_tp = min(sum_pr))

write_csv(simple_mod, paste0("/Volumes/My Book/Synchrony/presence/annual_simple_stats_",year1,'-',year2,".csv"))


simple_mod %>% ggplot() + aes(x = lon, y = lat, color = sum_tp, fill = sum_tp) + geom_tile() + theme_classic() +
  facet_wrap(~year)

simple_mod <- merge(simple_mod,gdd_season)

simple_mod <- simple_mod %>% group_by(lat,lon) %>%
  summarize(sum_tp_all = mean(sum_tp),
         min_t2m_all = mean(min_t2m),
         max_t2m_all = mean(max_t2m),
         min_rh_all = mean(min_rh),
         max_rh_all = mean(max_rh),
         min_tp_all = mean(min_tp),
         max_tp_all = mean(max_tp),
         gdd_season_all = mean(gdd_season)) %>% mutate(period = paste0(year1,'-',year2))


write_csv(simple_mod,paste0('/Volumes/My Book/Synchrony/presence/cc_simple_mod_',year1, "-", year2, "_GFDL-ESM4.csv"))

