library(tidyverse)

train_seen <- 2003

lag_df <- data.frame(lag = c(5,10,15,20,25,30,35,40),
                     height = c(8,7,6,5,4,3,2,1),
                     start = c(train_seen - 5,train_seen - 10,train_seen - 15,train_seen - 20,
                               train_seen - 25,train_seen - 30, train_seen - 35, train_seen - 40),
                     info = c(rep('lag',8)))

lag_df3 <- data.frame(lag = c(0,5,10,15,20,25,30,35,40),
                      height = c(9,8,7,6,5,4,3,2,1),
                      start = rep(train_seen,9),
                      info = c(rep('lag3',9)))

pdf("figures/lag_concept_indiv_both4.pdf",height = 7, width = 6.5)
lag_df %>%
  ggplot() + geom_rect(aes(ymin = height, ymax = height + 0.5, xmin = start + 0.5, xmax = start - 5+ 0.5, fill = info, color = info),
                       alpha = 0.7, size = 1) +
  geom_rect(data = lag_df3, aes(ymin = height, ymax = height + 0.5, xmin = start+ 0.5, xmax = start - 5+ 0.5, fill = info, color = info),
            alpha = 0.7, size = 1) + 
  theme_classic(base_size = 15) + 
  scale_fill_manual(values = c('lag' = 'dodgerblue4','lag3' = 'coral')) + 
  scale_color_manual(values = c('lag' = 'dodgerblue4','lag3' = 'coral')) + 
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none') + 
  scale_x_continuous(breaks = seq(1963,2003,5)) +
  annotate(geom = 'point', x = 2003, y = 10, color = 'black', size = 5) + 
  xlab("Year") +
  geom_vline(xintercept = 2003, linetype = 'dashed')
dev.off()
