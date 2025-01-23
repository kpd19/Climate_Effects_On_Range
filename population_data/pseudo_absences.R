library(tidyverse)

lats <- seq(31.5,54,0.25)
lons <- seq(-128,-104,0.25)

ll_grid <- data.frame(expand.grid(lats,lons))
colnames(ll_grid) <- c("lat_coord","lon_coord")

ll_grid <- merge(ll_grid, data.frame(plt = 1:5))

ll_grid$n <- 0

ll_grid <- ll_grid %>% mutate(n = ifelse(lat_coord < 48 & lon_coord < -124.5 ,-1,n))
ll_grid <- ll_grid %>% mutate(n = ifelse(lat_coord < 32.25 & lon_coord < -115.75 ,-1,n))

ll_grid %>% ggplot() + aes(x = lon_coord, y = lat_coord, fill = as.factor(n)) + geom_tile() + 
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 48, color = 'red') + 
  geom_vline(xintercept = -124.5, color = 'red') +
  geom_hline(yintercept = 32.25, color = 'red') + 
  geom_vline(xintercept = -115.75, color = 'red')

ll_grid <- ll_grid %>% filter(n == 0)

ll_grid$lat_push <- runif(n = dim(ll_grid)[1], min = -0.125, max = 0.125)
ll_grid$lon_push <- runif(n = dim(ll_grid)[1], min = -0.125, max = 0.125)

ll_grid <- ll_grid %>% mutate(lat = lat_coord + lat_push, 
                              lon = lon_coord + lon_push) %>% mutate(j = 1:length(lat_push))
ll_grid$manual_id <- ll_grid$j + 10000

write_csv(ll_grid,"data/nearby_coords_all.csv")
