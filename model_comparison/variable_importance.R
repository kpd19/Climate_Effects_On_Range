library(tidyverse)
library(randomForest)
library(gridExtra)
library(gtable)
library(grid)
library(scales)
library(ggrepel)

`%ni%` <- Negate(`%in%`)

###################
# Reading in variable importance
###################

var_names <- read_csv("../range_modeling/data/var_names_pa2.csv")
vi_update <- read_csv("data/vi_update.csv")
vi_all <- read_csv("data/vi_0-40.csv")

vi_update <- vi_update %>% select(-c('name','category','type','Use'))

head(vi_update)
head(vi_all)

vi_update <- merge(vi_update,var_names)
vi_all <- merge(vi_all,var_names)
vi_all <- vi_all %>% select(-filenum)
vi_update$training <- '1985-2025'
vi_all$training <- '1985-2010'

vi_all <- rbind(vi_all,vi_update)

vi_all %>% group_by(lag) %>% count(training)

###################
# Comparing variable importance
###################

vi_gini <- vi_all %>% filter(lag %in% c(0,35)) %>% mutate(n = 1) %>%
  arrange(desc(MeanDecreaseGini)) %>% group_by(lag,training) %>% mutate(rank = cumsum(n))

gini_rank <- vi_gini %>% group_by(variables,name) %>% summarize(mean_rank = mean(rank)) %>%
  arrange(desc(mean_rank))

vi_gini <- vi_gini %>% mutate(variables = factor(variables, levels = gini_rank$variables)) %>% 
  mutate(name = factor(name, levels = gini_rank$name))

cols <- c('#332288','#117733','#44AA99','#88CCEE','#DDCC77','#CC6677','#AA4499','#882255')
cols2 <- c('#332288','#117733','#44AA99','#88CCEE','#DDCC77','#CC6677','#882255')

pdf("figures/Gini.pdf",height = 10, width=12)
vi_gini %>% filter(MeanDecreaseGini >0) %>%
  mutate(name2 = ifelse(lag == 0, "No lag", "With 35 year lag")) %>% 
  ggplot() + aes(x = name, y = MeanDecreaseGini, color = category, fill = category) + 
  geom_bar(stat = 'identity') + 
  #geom_point() + 
  theme_classic(base_size =15) + 
  coord_flip() + 
  facet_wrap(training~name2,nrow = 1, scales = 'free_x') +
  scale_color_manual("", values = c('biomass' = cols2[1],'climatic variable' = cols2[6],'climatic variable- lag' = cols2[7],
                                    'land cover type' = cols2[4],'geographic' = cols2[5],'tree species' = cols2[2]))+
  scale_fill_manual("", values = c('biomass' = cols2[1],'climatic variable' = cols2[6],'climatic variable- lag' = cols2[7],
                                   'land cover type' = cols2[4],'geographic' = cols2[5],'tree species' = cols2[2])) +
  ylab('Mean Decrease in Gini') + xlab("Variables")  +
  theme(legend.position = 'top')
dev.off() 

vi_gini %>% filter(MeanDecreaseGini ==0)  %>% pull(name) %>% unique()

vi_acc <- vi_all %>% filter(lag %in% c(0,35)) %>% mutate(n = 1) %>%
  arrange(desc(MeanDecreaseAccuracy)) %>% group_by(lag,training) %>% mutate(rank = cumsum(n))
acc_rank <- vi_acc %>% group_by(variables,name) %>% summarize(mean_rank = mean(rank)) %>% arrange(desc(mean_rank))

vi_acc <- vi_acc %>% mutate(variables = factor(variables, levels = acc_rank$variables)) %>% 
  mutate(name = factor(name, levels = acc_rank$name))

pdf("figures/compare_four/Accuracy.pdf",height = 10, width=12)
vi_acc %>% filter(MeanDecreaseAccuracy >0) %>%
  mutate(name2 = ifelse(lag == 0, "No lag", "With 35 year lag")) %>% 
  ggplot() + aes(x = name, y = MeanDecreaseAccuracy, color = category, fill = category) + 
  geom_bar(stat = 'identity') + 
  #geom_point() + 
  theme_classic(base_size =15) + 
  coord_flip() + 
  facet_wrap(training~name2,nrow = 1, scales = 'free_x') +
  scale_color_manual("", values = c('biomass' = cols2[1],'climatic variable' = cols2[6],'climatic variable- lag' = cols2[7],
                                    'land cover type' = cols2[4],'geographic' = cols2[5],'tree species' = cols2[2]))+
  scale_fill_manual("", values = c('biomass' = cols2[1],'climatic variable' = cols2[6],'climatic variable- lag' = cols2[7],
                                   'land cover type' = cols2[4],'geographic' = cols2[5],'tree species' = cols2[2])) +
  ylab('Mean Decrease in Accuracy') + xlab("Variables")  +
  theme(legend.position = 'top')
dev.off() 

vi_gini %>% filter(MeanDecreaseAccuracy ==0)  %>% pull(name) %>% unique()

acc_rank$score <- "Accuracy"
gini_rank$score <- "Gini"
acc_rank$score2 <- 56:1
gini_rank$score2 <- 56:1

acc_rank2 <- merge(acc_rank,var_names)
gini_rank2 <- merge(gini_rank,var_names)

both <- rbind(acc_rank2,gini_rank2)

both_avg <- both %>% group_by(variables,name,category,type,Use) %>% 
  summarize(mean_rank = mean(mean_rank)) %>% 
  mutate(score = 'Averaged') %>% arrange(desc(mean_rank))
both_avg$score2 <-56:1

write_csv(both_avg,"data/compare_four_avg_rank.csv")

both <- rbind(both_avg,both)

pdf("figures/compare_four/all_rankings.pdf",height = 10, width = 8)
both %>% ggplot() + aes(x = score, y = score2, group = name, color = category) +
  geom_line() + theme_classic() + 
  geom_point() + 
  scale_y_reverse("Ranking", breaks = c(1,10,20,30,40,50)) + 
  geom_text(data = acc_rank2, aes(x = 1, y = score2, label = mean_rank, color = category), nudge_x = -0.05, hjust = 1) +
  geom_text(data = gini_rank2, aes(x = 3, y = score2, label = mean_rank, color = category), nudge_x = 0.05, hjust = 0) +
  geom_label(data = both_avg, aes(x = 2, y = score2, label = paste0(score2,". ",name), color = category), size = 3) +
  scale_color_manual("", values = c('biomass' = cols2[1],'climatic variable' = cols2[6],'climatic variable- lag' = cols2[7],
                                    'land cover type' = cols2[4],'geographic' = cols2[5],'tree species' = cols2[2]))+
  theme(legend.position = 'none')+
  scale_x_discrete("Metric", expand = expansion(add = c(0.25, 0.25))) 
dev.off()  

write_csv(both, 'data/all_rankings.csv')
