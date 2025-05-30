library(tidyverse)
library(gridExtra)
library(scico)

trap_all <- read_csv("outbreak_size/data/north_america_trap_data_051225.csv")
locations <- read_csv("outbreak_size/data/north_america_trap_locations_051225.csv")
clust_avg <- read_csv('outbreak_size/clusters/clusters_140002.csv')
clust_dist <- read_csv('outbreak_size/clusters/distances_140002.csv')

clust_avg2 <- clust_avg %>% select(clust,manual_id) %>% rename(clust_n = clust)

trap_all2 <- merge(trap_all,clust_avg2)

tdf <- trap_all2 %>% group_by(State,clust_n) %>% count(clust_n) %>% select(-n)

tdf <- merge(tdf,data.frame(breaks = c('breaks1','breaks2','breaks3','breaks4','breaks5')))

tdf <- tdf %>% arrange(State,clust_n)

write_csv(tdf,"outbreak_size/data/indiv_breaks_clust_140002.csv")

st <- "WA"
start <- 1983

trap_all2 %>% filter(State %in% c(st), clust_n >= 290, clust_n <800) %>% ggplot() +
  aes(x= year, y = trap_mean, group = manual_id) + geom_line() + geom_point() +
  theme_classic(base_size = 15) + 
  facet_wrap(~clust_n, scales = 'free_y') + 
  geom_vline(xintercept = c(start,start + 9,start + 18,start + 27,start + 36), color = 'red', linetype = 'dashed') +
  ylab("# Males Trapped")

tdf_edits <- read_csv("outbreak_size/data/indiv_breaks_clust_140002_edits.csv")

tdf_edits %>% filter(clust_n == 336)

tdf2 <- tdf_edits %>% drop_na(break_end)

uni_clust <- unique(tdf2$clust_n)

clust_break_max <- c()
for(i in 1:length(uni_clust)){
  temp <- tdf2 %>% filter(clust_n == uni_clust[i])
  for(j in 1:length(temp$breaks)){
    
    temp_break <- trap_all2 %>% filter(clust_n == uni_clust[i]) %>% filter(year <= temp$break_end[j], year > temp$break_end[j]-9) %>% 
      drop_na(trap_mean)
    
    if (length(temp_break$manual_id) >0){
      clust_ts <- temp_break %>% group_by(clust_n,year) %>%
        summarize(max_year = max(trap_mean,na.rm=TRUE)) %>% mutate(n = 1)
      
      max_break <- data.frame(max_trap = max(clust_ts$max_year), 
                              sum_trap = sum(clust_ts$max_year),
                              num_y = sum(clust_ts$n),
                              max_year = clust_ts$year[which.max(clust_ts$max_year)],
                              ob = temp$breaks[j],
                              break_end = temp$break_end[j],
                              clust_n = uni_clust[i])
      
      clust_break_max <- rbind(clust_break_max,max_break)
    }
  }
}

clust_break_max %>% count(num_y)

clust_avg3 <- clust_avg %>% mutate(n = 1) %>%
  select(clust,X,Y) %>% rename(clust_n = clust, clust_lon = X, clust_lat = Y)

clust_avg3 <- clust_avg3[!duplicated(clust_avg3),]

clust_break_max <- merge(clust_break_max,clust_avg3)

clust_break_max %>% filter(num_y %in% c(6,7,8,9)) %>% 
  ggplot() + aes(x = max_trap, y = sum_trap, color = as.factor(num_y), group = num_y) +
  geom_point() + theme_classic(base_size = 15) + 
  geom_smooth(method = 'lm') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = 'red')

clust_break_max %>% filter(num_y %in% c(7,8,9)) %>% 
  ggplot() + aes(x = ob, y = sum_trap, group = clust_n) +
  geom_point() + geom_line() + theme_classic(base_size = 15) 

write_csv(clust_break_max,'outbreak_size/data/cluster_max_sum_clustn2.csv')
