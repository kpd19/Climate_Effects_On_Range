library(tidyverse)
library(gridExtra)
library(lubridate)

models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4","HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")
years <- c(2030,2041,2050,2070,2090)

for (j in 1:length(models)){
  avg_dfs <- c()
  simple_mod_df <- c()
  means_df <- c()
  for(i in 1:length(years)){
    
    year1 <- years[i]
    
    if (year1 == 2041){
      year2 <- 2089    
    } else {
      year2 <- year1 + 10
    }
    
    mod_dir <- paste0('/Volumes/My Book/Climate/CMIP6/',models[j], '_ssp585_',year1,'/')
    
    gdd_all <- read_csv(paste0(mod_dir, "downsampled/ALL_season_gdd_",year1, "-", year2, "_", models[j], ".csv"))
    rh_means <- read_csv(paste0(mod_dir, "downsampled/means_hurs_",year1, "-", year2, "_", models[j], ".csv"))
    pr_means <- read_csv(paste0(mod_dir, "downsampled/means_pr_",year1, "-", year2, "_", models[j], ".csv"))
    tavg_means <- read_csv(paste0(mod_dir, "downsampled/means_tas_",year1, "-", year2, "_", models[j], ".csv"))
    tmin_means <- read_csv(paste0(mod_dir, "downsampled/means_tmin_",year1, "-", year2, "_", models[j], ".csv"))
    tmax_means <- read_csv(paste0(mod_dir, "downsampled/means_tmax_",year1, "-", year2, "_", models[j], ".csv"))
    
    average_temps <- tavg_means %>% group_by(lat,lon,year) %>% summarize(mean_temp = mean(tas_mean,na.rm=TRUE))
    average_temps <- average_temps %>% mutate(mean_temp = mean_temp - 273.15)
    
    total_pr <- pr_means %>% group_by(lat,lon,year) %>% summarize(total_pr = sum(sum_pr,na.rm=TRUE)*86.4)
    
    average_rh <- rh_means %>% group_by(lat,lon,year) %>% summarize(mean_rh = mean(hurs_mean,na.rm=TRUE))
    
    sum_stats <- merge(average_temps,total_pr)
    sum_stats <- merge(average_temps,average_rh)
    sum_stats <- sum_stats %>% mutate(lon = lon -360)
    
    all_means <- merge(rh_means,pr_means)
    all_means <- merge(all_means,tavg_means)
    all_means <- merge(all_means,tmin_means)
    all_means <- merge(all_means,tmax_means)
    
    all_means <- all_means %>% mutate(tas_mean = tas_mean - 273.15,
                                      tmin_mean = tmin_mean - 273.15,
                                      tmax_mean = tmax_mean - 273.15,
                                      sum_pr = sum_pr*86.4) %>% dplyr::select(-`...1`) %>% mutate(lon = lon - 360)
    
    gdd_season <- gdd_all %>% dplyr::select(lat,lon,year,julian,gdd_season) %>% mutate(lon = lon -360)
    
    all_means_tm <- all_means #%>% mutate(year = ifelse(month %in% c("October","November",'December'),year +1,year))
    
    simple_mod <- all_means_tm %>% group_by(lat,lon,year) %>% summarize(sum_tp = sum(sum_pr),
                                                                        min_t2m = min(tas_mean),
                                                                        max_t2m = max(tas_mean),
                                                                        min_rh = min(hurs_mean),
                                                                        max_rh = max(hurs_mean),
                                                                        max_tp = max(sum_pr),
                                                                        min_tp = min(sum_pr))
    
    
    simple_mod <- merge(simple_mod,gdd_season)   
    
    simple_mod_df <- rbind(simple_mod_df,simple_mod)
    means_df <- rbind(means_df,all_means)
    avg_dfs <- rbind(avg_dfs,sum_stats)
    
  }
  
  length(unique(means_df$year))
  
  write_csv(means_df, paste0("/Volumes/My Book/Synchrony/cc_downsampled/annual_stats_",models[j],"_update.csv"))
  write_csv(simple_mod_df,paste0('/Volumes/My Book/Synchrony/cc_downsampled/simple_avg_',models[j], "_update.csv"))
  write_csv(avg_dfs, paste0("/Volumes/My Book/Synchrony/cc_downsampled/annual_summary_",models[j],"_update.csv"))
}
