library(tidyverse)
library(ggbiplot)
library(gridExtra)
library(mgcv)
library(randomForest)

`%ni%` <- Negate(`%in%`)

rsq <- function(actual,predicted) 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

stats <- read_csv("outbreak_size/data/clustn_outbreak_size_features.csv")
var_names <- read_csv('range_modeling/data/var_names_pa.csv')

# ob_cutoff <- 25
# small_max = 55
# medium_max = 175

stats3 <- stats %>% filter(sum_trap > ob_cutoff) %>%  
  mutate(preferred = Pseudotsuga + Abies) %>%
  rename(max_biomass = max_cover, mean_biomass = mean_cover) %>% 
  filter(num_y >= 7) %>%
  arrange(clust_n,year_ob) %>% mutate(n = 1) %>% group_by(clust_n) %>%
  mutate(csum = cumsum(n)) %>%
  mutate(num_ob = max(csum)) %>%
  mutate(set = ifelse(num_ob >=3 & num_ob == csum,'test','train')) # %>% 
  #mutate(total_size = case_when(sum_trap <= small_max ~ 'small',
  #                              sum_trap >small_max & sum_trap <= medium_max ~'medium',
  #                              sum_trap >medium_max ~'large'))
  
#stats3$total_size <- factor(stats3$total_size,levels = c('small','medium','large'))

stats3 %>% ungroup() %>% count(set) #%>% filter(set == 'train') %>% pull(total_size)

stats4 <- stats3
stats6 <- stats3

stats5 <- stats4[,c(12:63,67,5)]

training_set <- stats5 %>% filter(set == 'train') %>% select(-set)

ob_sum_rf <- randomForest(x = training_set[-dim(training_set)[2]],
             y = training_set$sum_trap,
             ntree = 2000)

stats6$forest_pred <- predict(object=ob_sum_rf, newdata=stats6)

pdf("outbreak_size/figures/results_regrf.pdf")
stats6 %>% 
  filter(set == 'test') %>% 
  ggplot() + aes(x = sum_trap, y = forest_pred) + 
  geom_point() + theme_classic(base_size = 15) + 
  xlab("Data") + ylab("Prediction") +
  theme(legend.position = 'none')
dev.off()

metrics <- stats6 %>% ungroup() %>% 
  filter(set == 'test') %>% summarize(mse = mean((sum_trap - forest_pred)^2),
                                      rmse = sqrt(mse),
                                      r2 = rsq(sum_trap,forest_pred))


var_imp <- data.frame(importance(ob_sum_rf))
var_imp$variables <- row.names(var_imp)

var_imp <- merge(var_imp,var_names, all.x = TRUE)

## Create a plot of variable importance
pdf("outbreak_size/figures/feat_importance.pdf",height = 7, width = 6)
var_imp %>%
  arrange(IncNodePurity) %>% filter(IncNodePurity>0) %>% 
  ggplot(aes(x=reorder(name, IncNodePurity), y=IncNodePurity, color = category, fill = category)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  ylab('Increase in Node Purity') + xlab("Variables") +
  theme_minimal(base_size = 15) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20)) + 
  scale_color_brewer("", palette = "Set1")+
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = 'top')
dev.off()

get_conf_int <- function(m, x, xname, type, mod){
  
  if (type == 'numeric'){
    pvals <- seq(min(x[,xname]), max(x[,xname]),length.out = 51)
  } else if (type == 'categorical'){
    pvals <- unique(x[,xname])    
  }
  
  y.hat.mean <- vector()
  y.hat.lb1 <- vector()
  y.hat.ub1 <- vector()
  y.hat.lb2 <- vector()
  y.hat.ub2 <- vector()
  y.hat.lb3 <- vector()
  y.hat.ub3 <- vector()
  for (i in 1:length(pvals)){
    xtemp <- x
    xtemp[, xname] <- pvals[i]
    
    if(mod == "rf"){
      y.hat <- stats::predict(m, xtemp)
    } else {
      y.hat <- NA
    }
    
    y.hat.mean[i] <- stats::weighted.mean(y.hat)
    y.hat.lb1[i] <- stats::quantile(y.hat, 0.025)
    y.hat.lb2[i] <- stats::quantile(y.hat, 0.1)
    y.hat.lb3[i] <- stats::quantile(y.hat, 0.25)
    y.hat.ub3[i] <- stats::quantile(y.hat, 0.75)
    y.hat.ub2[i] <- stats::quantile(y.hat, 0.9)
    y.hat.ub1[i] <- stats::quantile(y.hat, 0.975)
    
  }
  m.ci <- as.data.frame(cbind(pvals, y.hat.mean, y.hat.lb1, y.hat.lb2, y.hat.lb3, y.hat.ub1,y.hat.ub2,y.hat.ub3))
  colnames(m.ci) <- c('value','yhat','lb1','lb2','lb3','ub1','ub2','ub3')
  
  return(m.ci)
  
}

imp_vars <- var_imp %>% arrange(desc(IncNodePurity)) %>% head(5)

all_pds <- c()
for(i in 1:length(imp_vars$variables)){
  
  temp <- get_conf_int(ob_sum_rf, training_set,imp_vars$variables[i],imp_vars$type[i],mod = 'rf')
  temp$param <- imp_vars$variables[i]
  
  all_pds <- rbind(all_pds,temp)
  print(i)
}

write_csv(all_pds,"outbreak_size/output/partial_dependence.csv")

all_rugs <- c()
for(i in 1:length(imp_vars$variables)){
  
  temp_vals <- training_set %>% pull(imp_vars$variables[i])
  
  probs <- data.frame(q = seq(0,1,0.1), value = as.numeric(quantile(temp_vals, prob = seq(0,1,0.1))))
  probs$param <- imp_vars$variables[i]
  
  all_rugs <- rbind(all_rugs,probs)
  print(i)
}

all_rugs2 <- merge(all_rugs,var_names,by.x = c('param'),by.y = c('variables'))
all_rugs2 <- all_rugs2 %>% mutate(name = factor(name, levels = imp_vars$name)) 

all_pds2 <- merge(all_pds,var_names, by.x = c('param'),by.y = c('variables'))
all_pds2 <- all_pds2 %>% mutate(name = factor(name, levels = imp_vars$name)) 

pdf("outbreak_size/figures/partial_dependence.pdf", height = 5, width = 16)
all_pds2 %>% 
  ggplot() + geom_ribbon(aes(x = value, ymin = lb3, ymax = ub3), linetype = 'dashed',
                         fill = 'skyblue1', linetype = 'dashed', color = 'grey55', alpha = 0.5) +
  geom_line(aes(x = value, y = ub3), linetype = 'dashed') +
  geom_line(aes(x = value, y = yhat), color = 'navyblue',size = 1.5) +
  theme_classic(base_size = 15) + 
  facet_wrap(~name, scales = 'free_x',nrow = 1) + 
  ylab("Partial dependence") + 
  geom_segment(data = all_rugs2,
               aes(x = value, y = -0.1, yend = 0), color = 'black') +
  scale_color_brewer("", palette = "Set1")+
  theme(legend.position = 'top') 
dev.off()

