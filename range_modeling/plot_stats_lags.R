library(tidyverse)
library(randomForest)

setwd("/Users/katherinedixon/Documents/StuffINeed/_Research/Climate_Range/range_modeling/")

`%ni%` <- Negate(`%in%`)

all_stats <- read_csv('data/stats_0-40.csv')
all_thresh <- read_csv('data/thresh_0-40.csv')
all_pds <- read_csv('data/pds_0-40.csv')
all_nums <- read_csv('data/nums_0-40.csv')
all_roc <- read_csv('data/roc_0-40.csv')
all_vi <- read_csv('data/vi_0-40.csv')

all_thresh <- all_thresh %>% mutate(lag = lag_rh)
all_nums <- all_nums %>% mutate(lag = lag_rh)

#pdf("figures/indiv/all_scores.pdf",height = 10, width = 12)
all_stats %>% mutate(metric = ifelse(metric == 'brier', "1 - Brier", metric)) %>% 
  mutate(estimate = ifelse(metric == "1 - Brier", 1-estimate, estimate)) %>% 
  filter(metric %in% c('1 - Brier', 'AUC','log-likelihood')) %>% 
  ggplot() + aes(x = lag, y = estimate) + geom_line() + 
  geom_point() + theme_classic() + 
  facet_wrap(~metric, scales = 'free')
#dev.off()

all_thresh %>% #filter(thresh <= 0.375,thresh >= 0.20) %>%
  mutate(metric = ifelse(metric == 'brier', "1 - Brier", metric)) %>% 
  mutate(estimate = ifelse(metric == "1 - Brier", 1-estimate, estimate)) %>% 
  ggplot() + aes(x = lag, y = estimate, group = interaction(thresh), color = thresh) +
  geom_point() + geom_line() + theme_classic() + 
  facet_wrap(~metric, scales = 'free') +
  scale_color_viridis_c(option = 'turbo')


all_stats %>% filter(metric == 'AUC') %>% arrange(estimate) %>% pull(estimate)
all_stats %>% filter(metric == 'AUC') %>% arrange(estimate) %>% pull(ci1)

all_thresh %>% filter(metric == 'accuracy') %>% arrange(desc(estimate))
all_thresh %>% filter(metric == 'F1') %>% arrange(desc(estimate))

df1 <- all_thresh %>% filter(thresh == 0.325)

df2 <- all_stats %>% mutate(metric = ifelse(metric == 'brier', "1 - Brier", metric)) %>% 
  mutate(estimate = ifelse(metric == "1 - Brier", 1-estimate, estimate)) %>% 
  filter(metric %in% c('AUC')) 

df3 <- rbind(df1,df2)

df3 <- df3 %>% mutate(metric = recode(metric, "AUC" = 'AUC-ROC','accuracy' = 'Accuracy',
                               'precision' = 'Precision', 'recall' = 'Recall', 'specificity' = 'Specificity')) %>%
  mutate(metric = factor(metric, levels = c("AUC-ROC","F1",'Accuracy','Precision','Recall','Specificity')))

pdf("figures/all_scores.pdf",height = 4, width = 8)
df3 %>% 
  ggplot() + aes(x = lag, y = estimate) + geom_line() + 
  geom_ribbon(aes(x = lag, ymin = ci1, ymax = ci2), fill = 'blue', alpha = 0.2) +
  geom_point() + theme_classic() + 
  facet_wrap(~metric, scales = 'free',nrow = 2) + 
  theme(legend.position = 'top') + 
  scale_color_brewer("", palette = "Set2") +
  xlab("Weather Lag (years)") +ylab("Score") +
  geom_point(data = df3[df3$lag == 35,], aes(x = lag, y = estimate), color = 'red')
dev.off()

pdf("figures/all_roc.pdf",height = 4, width = 6)
all_roc %>% filter(thresh %ni% c(0,1)) %>%
  ggplot() + aes(x = FPR, y = TPR, color = lag_pr, group = lag_pr) +
  #geom_point() + 
  geom_line() +
  theme_classic() +
  scale_color_viridis_c("Weather\nlag (years)", option = 'turbo') +
  xlab("False Positive Rate (FPR)") + ylab("True positive rate (TPR)")
dev.off()
