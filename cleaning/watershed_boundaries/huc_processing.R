library(tidyverse)
library(gridExtra)

latlong <- read_csv("_gendata/all_trap_max_edits.csv")

unique(latlong$State)

huc2_info <- read_csv("/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc2.csv")
huc4_info <- read_csv("/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc4.csv")
huc6_info <- read_csv("/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc6.csv")
huc8_info <- read_csv("/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_huc8.csv")


huc4_info <- huc4_info %>% rename(name4 = name)
huc6_info <- huc6_info %>% rename(name6 = name)
huc8_info <- huc8_info %>% rename(name8 = name)

huc_all <- merge(huc2_info,huc4_info)
huc_all <- merge(huc_all,huc6_info)
huc_all <- merge(huc_all,huc8_info)

latlong_huc <- merge(latlong,huc_all, all = TRUE)

latlong_huc %>% ggplot() +
  aes(x = name4, y = max_trap, group = name4, color = name4) + geom_boxplot() + theme_classic(base_size = 15) + 
  facet_wrap(~region, drop = TRUE, scales = 'free_x')


write_csv(latlong_huc,"/Volumes/My Book/Synchrony/_watershed/_trap_hucs/trap_locations_allhuc.csv")
