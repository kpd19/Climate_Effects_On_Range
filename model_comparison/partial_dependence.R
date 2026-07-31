library(tidyverse)
library(randomForest)
library(gridExtra)
library(gtable)
library(grid)
library(scales)
library(ggrepel)

`%ni%` <- Negate(`%in%`)

# ###################
# # Reading in partial dependence
# ###################

split_35 <- read_csv("data/split_pds_0_lag353535.csv")
split_0 <- read_csv("data/split_pds_0_lag000.csv")
split_35_update <- read_csv("data/split_pds_update_0_lag353535.csv")
split_0_update <- read_csv("data/split_pds_update_0_lag000.csv")

split_35$lag <- 35
split_35$training <- '1985-2010'
split_0$lag <- 0
split_0$training <- '1985-2010'
split_35_update$lag <- 35
split_35_update$training <- '1985-2025'
split_0_update$lag <- 0
split_0_update$training <- '1985-2025'

split_all <- rbind(split_35,split_0,split_35_update, split_0_update)

split_all2 <- merge(split_all,var_names,by.x = c('param'), by.y = c('variables'))

split_all2_num <- split_all2 %>% filter(type %in% c('numeric','binary')) %>% mutate(value = as.numeric(value))
split_all2_cat <- split_all2 %>% filter(type %in% c('categorical')) 

split_all2_num2 <- split_all2_num %>%  filter(name %in% c(acc_rank[acc_rank$mean_rank <= 22.5,]$name)) %>%
  mutate(param = factor(param, levels = rev(acc_rank$variables))) %>%
  mutate(name = factor(name, levels = rev(acc_rank$name)))

pdf("figures/split_pds_present.pdf",height = 10, width = 12)
split_all2_num2 %>% filter(outcome == 'present') %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(name2 = ifelse(lag == 0, "Current only", "Current + 35-year lag")) %>% 
  mutate(name3 = paste0(name2, " (",training,")"))  %>% 
  mutate(name3 = factor(name3, levels = c('Current only (1985-2010)', 'Current only (1985-2025)',
                                          'Current + 35-year lag (1985-2010)', 'Current + 35-year lag (1985-2025)'))) %>% 
  ggplot() + aes(x = value, y = yhat, color = name3, group = interaction(name3,outcome)) +
  geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = name3, group = name3), alpha = 0.1, color = NA) +
  geom_line(size = 1.2) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~name, scales = 'free') +
  scale_color_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                    'Current only (1985-2025)' = '#1f78b4',
                                    'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                    'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  scale_fill_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                    'Current only (1985-2025)' = '#1f78b4',
                                    'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                    'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  theme(legend.position = 'top') +
  xlab("Parameter value") + ylab("Predicted probability of presence")
dev.off()


pdf("figures/split_pds_absent.pdf",height = 10, width = 12)
split_all2_num2 %>% filter(outcome == 'absent') %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(name2 = ifelse(lag == 0, "Current only", "Current + 35-year lag")) %>% 
  mutate(name3 = paste0(name2, " (",training,")"))  %>% 
  mutate(name3 = factor(name3, levels = c('Current only (1985-2010)', 'Current only (1985-2025)',
                                          'Current + 35-year lag (1985-2010)', 'Current + 35-year lag (1985-2025)'))) %>% 
  ggplot() + aes(x = value, y = yhat, color = name3, group = interaction(name3,outcome)) +
  geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = name3, group = name3), alpha = 0.1, color = NA) +
  geom_line(size = 1.2) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~name, scales = 'free') +
  scale_color_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                    'Current only (1985-2025)' = '#1f78b4',
                                    'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                    'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  scale_fill_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                   'Current only (1985-2025)' = '#1f78b4',
                                   'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                   'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  theme(legend.position = 'top') +
  xlab("Parameter value") + ylab("Predicted probability of presence")
dev.off()

###################
# Comparing histograms of outcomes
###################

train_35_update <- read_csv("data/training_update_0_lag353535_periods.csv")
both_avg <- read_csv("data/compare_four_avg_rank.csv")

both_avg <- both_avg %>% arrange(mean_rank)

p2 <- 'elev2'
p1_col <- 'black'
p2_col <- '#fdbb84'

lims <- train_35_update %>% rename(new_name := !!sym(p2)) %>% 
  summarize(min_val = min(new_name),
            max_val = max(new_name))

range <- (lims$max_val-lims$min_val)*0.05
pdf("figures/legend.pdf")
train_35_update %>% filter(present == 'present') %>%
  select(all_of(c('period','present',p2))) %>% rename(new_name := !!sym(p2)) %>% 
  ggplot() + aes(x = new_name, y = after_stat(ncount), color = period, fill = period) +
  geom_histogram(position = 'identity', alpha = 0.6) + 
  theme_classic(base_size = 10)+
  xlim(c(lims$min_val-range,lims$max_val+range)) + 
  ylab("Distribution") + xlab("Parameter value") +
  scale_color_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col))+
  scale_fill_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col)) +
  theme(legend.position = 'top')
dev.off()

# the ranges are messed up

lims <- train_35_update %>% 
  summarize(min_val = min(`Temperate or sub-polar needleleaf forest`),
            max_val = max(`Temperate or sub-polar needleleaf forest`))

range <- (lims$max_val-lims$min_val)*0.035

train_35_update %>% filter(present == 'present') %>%
  select(all_of(c('period','present','Temperate or sub-polar needleleaf forest'))) %>% 
  ggplot() + aes(x = `Temperate or sub-polar needleleaf forest`, 
                 y = after_stat(ncount), color = period, fill = period) +
  geom_histogram(position = 'identity', alpha = 0.6) + 
  theme_classic(base_size = 10)+
  #xlim(c(lims$min_val-range,lims$max_val+range)) + 
  ylab("Distribution") + xlab("Parameter value") +
  scale_color_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col))+
  scale_fill_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col)) +
  theme(legend.position = 'top')

p2 <- 'elev2'

split_all2_num <- split_all2_num %>%
  mutate(value = ifelse(param == 'Temperate or sub-polar needleleaf forest', value*100,value)) %>% 
  mutate(value = ifelse(param == 'Temperate or sub-polar shrubland', value*100,value)) %>% 
  mutate(value = ifelse(param == 'Temperate or sub-polar broadleaf deciduous forest', value*100,value)) %>% 
  mutate(value = ifelse(param == 'Temperate or sub-polar grassland', value*100,value))

train_35_update <- train_35_update %>%
  mutate(`Temperate or sub-polar needleleaf forest` = `Temperate or sub-polar needleleaf forest`*100,
         `Temperate or sub-polar shrubland` = `Temperate or sub-polar shrubland`*100,
         `Temperate or sub-polar broadleaf deciduous forest` = `Temperate or sub-polar broadleaf deciduous forest`*100,
         `Temperate or sub-polar grassland` = `Temperate or sub-polar grassland`*100)

plot_split <- function(p2){
  lims <- train_35_update %>% rename(new_name := !!sym(p2)) %>% 
    summarize(min_val = min(new_name),
              max_val = max(new_name))
  p1_col <- 'black'
  p2_col <- '#fdbb84'
  
  range <- (lims$max_val-lims$min_val)*0.035
  
  fac1 <- split_all2_num %>% filter(outcome == 'present', param ==p2) %>% 
  mutate(value = as.numeric(value)) %>% 
    mutate(name2 = ifelse(lag == 0, "Current only", "Current + 35-year lag")) %>% 
    mutate(name3 = paste0(name2, " (",training,")"))  %>% 
  ggplot() + aes(x = value, y = yhat, color = name3, group = interaction(name3,outcome)) +
  geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = name3, group = name3), alpha = 0.1, color = NA) +
  geom_line(size = 1.2) + 
  theme_classic(base_size = 10) + 
  scale_color_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                    'Current only (1985-2025)' = '#1f78b4',
                                    'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                    'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  scale_fill_manual("", values = c('Current only (1985-2010)' = '#a6cee3',
                                   'Current only (1985-2025)' = '#1f78b4',
                                   'Current + 35-year lag (1985-2010)' = '#fb9a99',
                                   'Current + 35-year lag (1985-2025)' = '#e31a1c')) +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10)) +
  ylab("Estimated presence") +
    xlim(c(lims$min_val-range,lims$max_val+range)) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    ggtitle("Present observations")

  fac2 <- train_35_update %>% filter(present == 'present') %>%
    select(all_of(c('period','present',p2))) %>% rename(new_name := !!sym(p2)) %>% 
    ggplot() + aes(x = new_name, y = after_stat(ncount), color = period, fill = period) +
    geom_histogram(position = 'identity', alpha = 0.6) + 
    theme_classic(base_size = 10)+
    xlim(c(lims$min_val-range,lims$max_val+range)) + 
    ylab("Distribution") + xlab("Parameter value") +
    scale_color_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col))+
    scale_fill_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col)) +
    theme(legend.position = 'none') + 
    scale_y_continuous(breaks = c(0,0.5,1))
  
  fac3 <- split_all2_num %>% filter(outcome == 'absent', param ==p2) %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(name2 = ifelse(lag == 0, "No lag", "With 35 year lag")) %>% 
    mutate(name3 = paste0(name2, " (",training,")"))  %>% 
    ggplot() + aes(x = value, y = yhat, color = name3, group = interaction(name3,outcome)) +
    geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = name3, group = name3), alpha = 0.1, color = NA) +
    geom_line(size = 1.2) + 
    theme_classic(base_size = 10) + 
    scale_color_manual("", values = c('No lag (1985-2010)' = '#a6cee3',
                                      'No lag (1985-2025)' = '#1f78b4',
                                      'With 35 year lag (1985-2010)' = '#fb9a99',
                                      'With 35 year lag (1985-2025)' = '#e31a1c')) +
    scale_fill_manual("", values = c('No lag (1985-2010)' = '#a6cee3',
                                     'No lag (1985-2025)' = '#1f78b4',
                                     'With 35 year lag (1985-2010)' = '#fb9a99',
                                     'With 35 year lag (1985-2025)' = '#e31a1c')) +
    theme(legend.position = 'none',
          axis.title.x = element_blank(),
          plot.title = element_text(size = 10)) +
    ylab("Estimated presence") +
    xlim(c(lims$min_val-range,lims$max_val+range)) + 
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    ggtitle("Absent observations")
  
  fac4 <- train_35_update %>% filter(present == 'absent') %>%
    select(all_of(c('period','present',p2))) %>% rename(new_name := !!sym(p2)) %>% 
    ggplot() + aes(x = new_name, y = after_stat(ncount), color = period, fill = period) +
    geom_histogram(position = 'identity', alpha = 0.6) + 
    theme_classic(base_size = 10)+
    xlim(c(lims$min_val-range,lims$max_val+range)) + 
    ylab("Distribution") + xlab("Parameter value") +
    scale_color_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col))+
    scale_fill_manual("", values = c('1985-2010' = p1_col, '2011-2025' = p2_col)) +
    theme(legend.position = 'none')+ 
    scale_y_continuous(breaks = c(0,0.5,1))
  
  title <- split_all2_num %>% filter(outcome == 'present', param ==p2) %>% head(1) %>% pull(name)
  
  p1 <- grid.arrange(fac1,fac2,fac3,fac4,heights = c(1,0.4,1,0.4), top = textGrob(as.character(title),
                                                                                  gp=gpar(fontsize=12,font=8)))
  return(p1)
  
}

plot_split('near_needle')   
# plot_split('Temperate or sub-polar needleleaf forest')   
# plot_split('Temperate or sub-polar grassland')   

imp_vars <- both_avg %>% filter(type %in% c('numeric','binary')) %>%
  arrange(score2) %>% head(30) %>% pull(variables)
p <- list()
for(i in 1:length(imp_vars)){
  p[[i]] <- plot_split(imp_vars[i])   
}

for(i in 1:6){
  r <- (i-1)*5
  p1 <- list(p[[r + 1]],p[[r + 2]],p[[r + 3]],p[[r + 4]],p[[r + 5]]) #,p[[r + 6]]
  
  plt_list <- c(p1,list(nrow = 1))
  pdf(paste0("figures/compare_four/pd_plt",i,".pdf"),height = 7.5, width = 15)
  do.call(grid.arrange,plt_list)
  dev.off()
}

peak_pres <- split_all2_num %>% filter(outcome == "present") %>% 
  group_by(lag,training, outcome, name) %>% summarize(max_yhat = max(yhat))%>% 
  mutate(name2 = ifelse(lag == 0, "Current only", "Current + 35-year lag")) %>% 
  mutate(name3 = paste0(name2, " (",training,")")) %>% ungroup() %>% select(name,max_yhat, name3) %>% 
  pivot_wider(names_from =name3, values_from = max_yhat) %>% 
  mutate(name = factor(name, levels = both_avg$name))

peak_pres <- peak_pres %>% arrange(name)

split_all2_num %>% group_by(lag,training, outcome, name) %>% filter(yhat == max(yhat)) %>% 
  filter(param %in% imp_vars) %>% 
  select(param,name,lag,training,value,yhat) %>% ungroup() %>% 
  group_by(param,name) %>% 
  mutate(yhat_max = max(yhat)) %>% filter(yhat == yhat_max) %>% 
  mutate(mod = paste0("Model: ", lag, "-",training)) %>% ungroup() %>% count(mod)

split_all2_num %>% group_by(lag,training, outcome, name) %>% filter(yhat == max(yhat)) %>% 
  filter(param %in% imp_vars) %>%
  filter(outcome =='present') %>% select(param,name,lag,training,value,yhat) %>% ungroup() %>% 
  group_by(param,name) %>% 
  mutate(yhat_min = min(yhat)) %>% filter(yhat == yhat_min) %>% 
  mutate(mod = paste0("Model: ", lag, "-",training)) %>% ungroup() %>% count(mod)


split_all2_num %>% filter(param %in% imp_vars) %>%
  filter(outcome =='present') %>% select(param,name,lag,training,value,yhat) %>% ungroup() %>% 
  group_by(param,name,lag,training) %>% 
  summarize(yhat_mean = mean(yhat)) %>% group_by(param,name) %>% filter(yhat_mean == max(yhat_mean)) %>% 
  mutate(mod = paste0("Model: ", lag, "-",training)) %>% ungroup() %>% count(mod)


sensitivity <- split_all2_num %>% group_by(lag,training, outcome, name,param) %>% summarize(min_yhat = min(yhat),
                                                                       max_yhat = max(yhat)) %>% 
  mutate(range = max_yhat - min_yhat)
sensitivity %>% filter(param %in% imp_vars) %>% filter(outcome == 'present') %>% arrange(desc(range))
sensitivity %>% filter(param %in% imp_vars) %>% filter(outcome == 'absent') %>% arrange(desc(range))

peak_mod <- split_all2_num %>% group_by(lag,training, outcome, name) %>% filter(yhat == max(yhat)) %>%
  filter(param %in% imp_vars) %>%
  filter(outcome =='present') %>% filter(lag == 35, training == '1985-2025')

split_all2_num %>% filter(param %in% imp_vars) %>% 
  filter(lag == 35, training == '1985-2025', outcome == 'present') %>% filter(yhat >= 0.9) %>% 
  ggplot() + aes(x = value, y = yhat) + 
  geom_line() + theme_classic() + 
  facet_wrap(~param, scales = 'free')

peak_mod_small <- peak_mod %>% ungroup() %>% select(name,value)

range_vals <- split_all2_num %>% filter(param %in% imp_vars) %>% 
  filter(lag == 35, training == '1985-2025', outcome == 'present') %>% filter(yhat >= 0.9) %>% group_by(param,name) %>% 
  summarize(min_val = min(value),
            max_val = max(value)) 

range_vals <- merge(range_vals,peak_mod_small)

range_vals <- range_vals %>% mutate(dig = floor(log10(abs(value)))+1) %>% 
  mutate(param = factor(param, levels = imp_vars)) %>% arrange(param)

write_csv(range_vals, 'data/range_parameters.csv')
