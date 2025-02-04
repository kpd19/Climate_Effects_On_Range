library(tidyverse)
#library(ggbiplot)
library(gridExtra)
#library(mgcv)
library(geodata)
library(sf)
library(randomForest)
library(MESS)
library(pROC)
library(pdp)

`%ni%` <- Negate(`%in%`)

find_nas <- function(dataset){
  
  probs <- c()
  for(i in 1:dim(dataset)[2]){
    
    test <- sum(is.na(dataset[,i]))
    test2 <- sum(is.logical(dataset[,i]))
    
    test3 <- sum(test,test2)
    
    if (test3 > 0){
      probs <- c(probs,colnames(dataset)[i])
    }
  }
  
  if (length(probs)>0){
    print(paste0("List of problems: ", probs))
  } else{
    print("No problems in dataset found")
  }
}

get_roc <- function(df){
  df <- merge(df, data.frame(thresh = seq(0.0,1.0,0.025)))
  
  roc <- df %>% 
    mutate(present_name = ifelse(present == 1, 'present','absent'),
           pred_name = ifelse(PA_pred >=thresh,'present','absent')) %>%
    group_by(present_name,lag,thresh) %>% count(pred_name) %>% 
    group_by(present_name,lag,thresh) %>% mutate(tot = sum(n), 
                                                 p = n/tot) %>% 
    mutate(type = case_when(present_name == 'absent' & pred_name == 'absent' ~'true_negative', 
                            present_name == 'absent' & pred_name == 'present' ~'false_positive', 
                            present_name == 'present' & pred_name == 'absent' ~'false_negative', 
                            present_name == 'present' & pred_name == 'present' ~'true_positive')) %>%  
    ungroup() %>% 
    select(lag,thresh,p,type) %>% pivot_wider(names_from = 'type', values_from = 'p') %>% 
    mutate_at(c('false_positive','false_negative','true_positive','true_negative'), ~replace_na(.,0)) %>% 
    mutate(TPR = true_positive/(true_positive + false_negative),
           FPR = false_positive/(false_positive + true_negative))
  
  maximized <- roc %>% mutate(diff = abs(TPR-FPR)) %>% group_by(lag) %>%
    filter(diff == max(diff,na.rm=TRUE)) %>% arrange(desc(diff)) %>% pull(thresh)
  
  roc$maximized <- 0
  roc[roc$thresh == maximized,]$maximized <- 1
  
  return(roc)
  
}

get_stats <- function(df, thresh, lag){
  ci_val <- ci.auc(df$present, df$PA_pred)
  auc_val <- auc(df$present, df$PA_pred)
  
  auc_df <- data.frame(metric = "AUC", estimate = as.numeric(auc_val),
                       ci1 = as.numeric(ci_val)[1],ci2 = as.numeric(ci_val)[3], b1 = NA, b2 = NA)

  scores <- df %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
                              present_name = ifelse(present == 1, 'present','absent')) %>% 
    group_by(present_name) %>% count(pred_class) %>% 
    mutate(type = case_when(present_name == 'absent' & pred_class == 'absent' ~'true_negative', 
                            present_name == 'absent' & pred_class == 'present' ~'false_positive', 
                            present_name == 'present' & pred_class == 'absent' ~'false_negative', 
                            present_name == 'present' & pred_class == 'present' ~'true_positive')) %>% ungroup() %>% 
    mutate(tot = sum(n)) %>% 
    select(tot,n,type) %>% pivot_wider(names_from = 'type', values_from = 'n')

  if ("false_negative" %ni% colnames(scores)){
    scores$false_negative <- 0
  }
    
  beta_precision <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + 1), shape2 = (scores$false_positive + 1))
  beta_recall <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + 1), shape2 = (scores$false_negative + 1))
  beta_acc <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + scores$true_negative+ 1), shape2 = (scores$false_positive + scores$false_negative + 1))
  
  recall_beta <- data.frame(b1 = beta_recall[1],b2 = beta_recall[2])
  precision_beta <- data.frame(b1 = beta_precision[1],b2 = beta_precision[2])
  acc_beta <- data.frame(b1 = beta_acc[1],b2 = beta_acc[2])
  
  recall_df <- scores %>% summarize(recall = true_positive/(true_positive + false_negative),
                                    tot = (true_positive + false_negative)) %>% 
    mutate(ci1 = recall - (1.96*sqrt(recall*(1-recall)/tot)),
           ci2 = recall + (1.96*sqrt(recall*(1-recall)/tot)))
  
  precision_df <- scores %>% summarize(precision = true_positive/(true_positive + false_positive),
                                       tot = (true_positive + false_positive)) %>% 
    mutate(ci1 = precision - (1.96*sqrt(precision*(1-precision)/tot)),
           ci2 = precision + (1.96*sqrt(precision*(1-precision)/tot)))
  
  accuracy_df <- scores %>% mutate(acc = (true_positive + true_negative)/(tot)) %>% 
    mutate(ci1 = acc - (1.96*sqrt(acc*(1-acc)/tot)),
           ci2 = acc + (1.96*sqrt(acc*(1-acc)/tot))) %>% 
    select(acc,ci1,ci2)

  precision_all <- merge(precision_df,precision_beta)
  recall_all <- merge(recall_df,recall_beta)
  acc_all <- merge(accuracy_df,acc_beta)
  
  precision_all <- precision_all %>% select(precision,b1,b2,ci1,ci2) %>% rename(estimate = precision) %>% 
    mutate(metric = "precision")
  recall_all <- recall_all %>% select(recall,b1,b2,ci1,ci2) %>% rename(estimate = recall) %>% 
    mutate(metric = "recall")
  acc_all <- acc_all %>% select(acc,b1,b2,ci1,ci2)%>% rename(estimate = acc) %>% 
    mutate(metric = "accuracy")
  
  all_metrics <- rbind(auc_df,recall_all, acc_all, precision_all)
  all_metrics$thresh <- thresh
  all_metrics$lag <- lag
  
  return(list(all_metrics,scores))
  
  }

get_conf_int <- function(m, x, xname, type){
  
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
    y.hat <- stats::predict(m, xtemp, type = 'prob')[,1]
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

