library(tidyverse)
library(geonames)

options(geonamesUsername="your_username")

`%ni%` <- Negate(`%in%`)

loc_data <- read_csv("data/range_locations_example.csv")

num_locs <- length(loc_data$lat)

bins <- 5

loc_data$elev_id <- rep(c(1:bins), time = ceiling(num_locs/bins))[1:num_locs]

for(j in 1:bins){
  loc_data2 <- loc_data %>% filter(elev_id == j)
  
  elev <- c()
  for(i in 1:dim(loc_data2)[1]){
    temp <- GNsrtm3(loc_data2$lat[i],loc_data2$lon[i])
    temp$lat <- loc_data2$lat[i]
    temp$lon <- loc_data2$lon[i]
    elev <- rbind(elev,temp)
    
  }
  
  elev <- elev %>% rename(elev2 = srtm3) %>% mutate(elev2 = ifelse(elev2 <= -1000, NA,elev2))
  
  write_csv(elev, paste0("data/temp_elev2_",j,".csv"))
  
  print(paste0("Finishing bin: ",j))
}

elev_df <- c()
for(j in 1:bins){
  file_name <- paste0("data/temp_elev2_",j,".csv")
  file <- read_csv(file_name)
  elev_df <- rbind(elev_df, file)
  
}

write_csv(elev_df, "data/elevation_example.R")

