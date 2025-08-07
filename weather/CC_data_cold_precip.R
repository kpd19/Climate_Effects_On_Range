library(tidyverse)
library(gridExtra)
library(lubridate)

models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4","HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")
years <- c(2030,2041,2050,2070,2090)

for (j in 1:length(models)){
  mod_df <- c()
  for(i in 1:length(years)){
    
    year1 <- years[i]
    
    if (year1 == 2041){
      year2 <- 2089    
    } else {
      year2 <- year1 + 10
    }
    
    mod_dir <- paste0('/Volumes/My Book/Climate/CMIP6/',models[j], '_ssp585_',year1,'/')
    
    cold_monthly <- read_csv(paste0(mod_dir, "downsampled/cold_tolerance_",year1, "-", year2, "_", models[j], ".csv"), show_col_types = FALSE)
    pr_monthly <- read_csv(paste0(mod_dir, "downsampled/precip_days_",year1, "-", year2, "_", models[j], ".csv"), show_col_types = FALSE)

    cold_monthly <- cold_monthly %>% select(-`...1`)
    pr_monthly <- pr_monthly %>% select(-`...1`)
    
    sum_stats <- merge(cold_monthly,pr_monthly)
   
    sum_stats <- sum_stats %>% mutate(lon = lon -360)
    
    mod_df <- rbind(mod_df,sum_stats)
    
  }
  
  write_csv(mod_df, paste0("/Volumes/My Book/Synchrony/cc_downsampled/cold_tol_precip_days_",models[j],"_update.csv"))
  
  print(paste0("Finishing model: ", models[j]))
  print(Sys.time())
}


models <- c("ACCESS-ESM1-5","CanESM5-p1","EC-Earth3-Veg-LR","CNRM-ESM2-f2","GFDL-ESM4","HadGEM3-GC31-MM","INM-CM5-0","KACE-1-0-G","MIROC-ES2L-f2","NorESM2-MM")

cc_cold <- c()
for(i in 1:length(models)){
  
  temp <- read_csv(paste0('/Volumes/My Book/Synchrony/cc_downsampled/cold_tol_precip_days_',models[i], "_update.csv"))

  temp$model <- models[i]
  
  cc_cold <- rbind(cc_cold,temp)

}

write_csv(cc_cold,"data/annual_cold_stats_all_models.csv")

