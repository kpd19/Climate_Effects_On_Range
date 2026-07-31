library(tidyverse)
library(cowplot)

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

small <- all_roc %>% filter(thresh %ni% c(0,1), lag_rh %in% c(0,35)) %>%
  mutate(name2 = ifelse(lag_rh == 0, "Current weather only", "Current + 35-year weather lag"))

small2 <- all_roc %>% filter(thresh %ni% c(0,1), lag_rh %in% c(0,35,34)) %>% 
  mutate(name2 = case_when(lag_rh == 0 ~ "Current weather only", 
                           lag_rh == 35 ~ "Current + 35-year weather lag",
                           lag_rh == 34 ~ "Current + weather lag")) %>% 
  mutate(name2 = factor(name2, levels = c("Current weather only", "Current + 35-year weather lag", "Current + weather lag")))

plt1 <- all_roc %>% filter(thresh %ni% c(0,1)) %>%
  ggplot() + aes(x = FPR, y = TPR, group = lag_pr) +
  #geom_point() + 
  geom_line(color = 'grey65') +
  theme_classic() +
  scale_color_manual("Weather variables", values = c("Current weather only" = "#377eb8",
                                                     "Current + 35-year weather lag" = "#e41a1c")) +
  xlab("False Positive Rate (FPR)") + ylab("True positive rate (TPR)") + 
  geom_line(data = small, aes(x = FPR, y = TPR, color = name2, group = name2), alpha = 0.8) +
  theme(legend.position = "none") + 
  annotate("rect", xmin = 0, xmax = 0.1, ymin = 0.8, ymax = 0.95,
           fill = NA, color = 'blue')

plt2 <- all_roc %>% filter(thresh %ni% c(0,1)) %>%
  ggplot() + aes(x = FPR, y = TPR, group = lag_pr) +
  #geom_point() + 
  geom_line(color = 'grey65', size = 0.5) +
  theme_classic() +
  scale_color_manual("Weather variables", values = c("Current weather only" = "#377eb8",
                                                      "Current + 35-year weather lag" = "#e41a1c")) +
  xlab("False Positive Rate (FPR)") + ylab("True positive rate (TPR)") + 
  geom_line(data = small, aes(x = FPR, y = TPR, color = name2, group = name2), alpha = 1, linewidth = 1) +
  geom_point(data = small[small$thresh == 0.325,], aes(x = FPR, y = TPR, color = name2, group = name2), size = 2)+
  coord_cartesian(xlim = c(0,0.1), ylim = c(0.8,0.95)) +
  annotate("rect", xmin = 0, xmax = 0.1, ymin = 0.8, ymax = 0.95,
           fill = NA, color = 'blue') +
  theme(legend.position = 'none')

leg_plt <- small2 %>% 
  ggplot() + aes(x = FPR, y = TPR, color = name2, group = name2) +
  geom_point() + 
  geom_line() + 
  scale_color_manual("Weather variables", values = c("Current weather only" = "#377eb8",
                                          "Current + 35-year weather lag" = "#e41a1c",
                                          "Current + weather lag" = "grey65")) +
  xlab("False Positive Rate (FPR)") + ylab("True positive rate (TPR)") + 
  theme_classic() +
  theme(legend.position = 'top') +
  scale_linewidth_manual("Weather variables",
                         values = c("Current weather only" = 1,
                                    "Current + 35-year weather lag" = 1,
                                    "Current + weather lag" = 0.5)) 


shared_legend <- get_plot_component(leg_plt, 'guide-box-top', return_all = TRUE)

pdf("figures/all_roc.pdf",height = 4, width = 12)
plot_grid(shared_legend, plot_grid(plt1,plt2, nrow = 1, align = "h"),nrow = 2,  rel_heights= c(0.1,1))
dev.off()
