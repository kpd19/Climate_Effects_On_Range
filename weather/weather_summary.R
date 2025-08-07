library(tidyverse)

gdd <- read_csv("data/gdd_example.csv")
pr <- read_csv("data/pr_example.csv")
temp <- read_csv("data/t2m_example.csv")
rh <- read_csv("data/rh_example.csv")
cold <- read_csv("data/cold_example.csv")

temps_extremes <- temp %>% mutate(year_tm = ifelse(month %in% c("October","November",'December'),year+1,year)) %>% 
  group_by(latitude,longitude,year_tm) %>% summarize(min_t2m = min(mean_t2m),
                                                     max_t2m = max(mean_t2m))

rh_extremes <- rh %>% mutate(year_tm = ifelse(month %in% c("October","November",'December'),year+1,year)) %>% 
  group_by(latitude,longitude,year_tm) %>% summarize(min_rh = min(mean_rh),
                                                     max_rh = max(mean_rh))

pr_extremes <- pr %>% mutate(year_tm = ifelse(month %in% c("October","November",'December'),year+1,year)) %>% 
  group_by(latitude,longitude,year_tm) %>% summarize(min_tp = min(sum_tp),
                                                     max_tp = max(sum_tp),
                                                     sum_tp = sum(sum_tp))

cold_extremes <- cold %>% mutate(year_tm = ifelse(month %in% c("October","November",'December'),year+1,year)) %>% 
  group_by(latitude,longitude,year_tm) %>% summarize(coldest = min(min_t2m))

gdd <- gdd %>% rename(year_tm = year)

all_annual <- merge(temps_extremes,rh_extremes)
all_annual <- merge(all_annual,pr_extremes)
all_annual <- merge(all_annual,gdd, all = TRUE)
all_annual <- merge(all_annual,cold_extremes)

all_annual <- all_annual %>% rename(lat = latitude, lon = longitude)

all_annual <- all_annual %>% filter(year_tm != 2025)

write_csv(all_annual, "data/weather_summary_1960-2024_example.csv")
