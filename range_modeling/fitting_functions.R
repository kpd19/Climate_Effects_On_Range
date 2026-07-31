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
    group_by(present_name,thresh) %>% count(pred_name) %>% 
    group_by(present_name,thresh) %>% mutate(tot = sum(n), 
                                                 p = n/tot) %>% 
    mutate(type = case_when(present_name == 'absent' & pred_name == 'absent' ~'true_negative', 
                            present_name == 'absent' & pred_name == 'present' ~'false_positive', 
                            present_name == 'present' & pred_name == 'absent' ~'false_negative', 
                            present_name == 'present' & pred_name == 'present' ~'true_positive')) %>%  
    ungroup() %>% 
    select(thresh,p,type) %>% pivot_wider(names_from = 'type', values_from = 'p') %>% 
    mutate_at(c('false_positive','false_negative','true_positive','true_negative'), ~replace_na(.,0)) %>% 
    mutate(TPR = true_positive/(true_positive + false_negative),
           FPR = false_positive/(false_positive + true_negative))
  
  maximized <- roc %>% mutate(diff = abs(TPR-FPR)) %>% 
    filter(diff == max(diff,na.rm=TRUE)) %>% select(diff) %>% mutate(maximized = 1)
  
  roc <- roc %>% mutate(diff = abs(TPR-FPR)) %>% 
    mutate(maximized = ifelse(diff == max(diff,na.rm=TRUE),1,0))
  
  return(roc)
  
}

get_stats <- function(df, thresh){
  
  ci_val <- ci.auc(df$present, df$PA_pred)
  auc_val <- auc(df$present, df$PA_pred)
  
  auc_df <- data.frame(metric = "AUC", estimate = as.numeric(auc_val),
                       ci1 = as.numeric(ci_val)[1],ci2 = as.numeric(ci_val)[3], b1 = NA, b2 = NA)

  scores <- df %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
                          present_name = ifelse(present == 1, 'present','absent')) %>%
    select(pred_class,present_name) %>% 
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
  beta_spec <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_negative + 1), shape2 = (scores$false_positive + 1))
  
    
  recall_beta <- data.frame(b1 = beta_recall[1],b2 = beta_recall[2])
  precision_beta <- data.frame(b1 = beta_precision[1],b2 = beta_precision[2])
  acc_beta <- data.frame(b1 = beta_acc[1],b2 = beta_acc[2])
  spec_beta <- data.frame(b1 = beta_spec[1],b2 = beta_spec[2])
  
    
  recall_df <- scores %>% summarize(recall = true_positive/(true_positive + false_negative),
                                    tot = (true_positive + false_negative)) %>% 
    mutate(ci1 = recall - (1.96*sqrt(recall*(1-recall)/tot)),
           ci2 = recall + (1.96*sqrt(recall*(1-recall)/tot)))
  
  specificity_df <- scores %>% summarize(specificity = true_negative/(true_negative + false_positive),
                                         tot = (true_negative + false_positive)) %>% 
    mutate(ci1 = specificity - (1.96*sqrt(specificity*(1-specificity)/tot)),
           ci2 = specificity + (1.96*sqrt(specificity*(1-specificity)/tot)))
  
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
  specificity_all <- merge(specificity_df,spec_beta)
  
  F1 <- (2*precision_all$precision*recall_all$recall)/(precision_all$precision + recall_all$recall)
  
  precision_all <- precision_all %>% select(precision,b1,b2,ci1,ci2) %>% rename(estimate = precision) %>% 
    mutate(metric = "precision")
  recall_all <- recall_all %>% select(recall,b1,b2,ci1,ci2) %>% rename(estimate = recall) %>% 
    mutate(metric = "recall")
  acc_all <- acc_all %>% select(acc,b1,b2,ci1,ci2)%>% rename(estimate = acc) %>% 
    mutate(metric = "accuracy")
  specificity_all <- specificity_all %>% select(specificity,b1,b2,ci1,ci2) %>%
    rename(estimate = specificity) %>% 
    mutate(metric = "specificity")
  
  FI_all <- data.frame(metric = "F1", estimate = F1, ci1 = NA, ci2 = NA, b1 = NA, b2 = NA)
  
  brier_all <- df %>% 
    select(present,PA_pred) %>%
    summarize(estimate = mean((PA_pred - present)^2)) %>% select(estimate) %>% 
    mutate(metric = 'brier', ci1 = NA, ci2 = NA, b1 = NA, b2 = NA)
  
  
  ll_all <- df %>% 
    select(present,PA_pred) %>% mutate(p_true_class = ifelse(present == 1, PA_pred,1-PA_pred)) %>% 
    mutate(p_true_class = ifelse(p_true_class == 0, 0.0003333333, p_true_class)) %>% 
    summarize(estimate = sum(log(p_true_class))) %>% select(estimate) %>% 
    mutate(metric = 'log-likelihood', ci1 = NA, ci2 = NA, b1 = NA, b2 = NA)
  
  all_metrics <- rbind(auc_df,recall_all, acc_all, precision_all,specificity_all,FI_all,brier_all,
                       ll_all)
  all_metrics$thresh <- thresh
  #all_metrics$lag <- lag
  
  return(list(all_metrics,scores))
  
  }
  
  get_stats_thresh <- function(df, thresh){
  
  	scores <- df %>% mutate(pred_class = ifelse(PA_pred >= thresh,'present','absent'),
             	             present_name = ifelse(present == 1, 'present','absent')) %>%
    select(pred_class,present_name) %>% 
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
  	
  	if ("true_negative" %ni% colnames(scores)){
    	scores$true_negative <- 0
  	}
  	  	
  	if ("false_positive" %ni% colnames(scores)){
    	scores$false_positive <- 0
  	}	
  	  	
  	if ("true_positive" %ni% colnames(scores)){
    	scores$true_positive <- 0
  	}
  	
  	
  	beta_precision <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + 1), shape2 = (scores$false_positive + 1))
  	beta_recall <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + 1), shape2 = (scores$false_negative + 1))
  	beta_acc <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_positive + scores$true_negative+ 1), shape2 = (scores$false_positive + scores$false_negative + 1))
  	beta_spec <- qbeta(p = c(0.025,0.975),shape1 = (scores$true_negative + 1), shape2 = (scores$false_positive + 1))
  
    recall_beta <- data.frame(b1 = beta_recall[1],b2 = beta_recall[2])
    precision_beta <- data.frame(b1 = beta_precision[1],b2 = beta_precision[2])
    acc_beta <- data.frame(b1 = beta_acc[1],b2 = beta_acc[2])
    spec_beta <- data.frame(b1 = beta_spec[1],b2 = beta_spec[2])
  
    recall_df <- scores %>% summarize(recall = true_positive/(true_positive + false_negative),
                                      tot = (true_positive + false_negative)) %>% 
      mutate(ci1 = recall - (1.96*sqrt(recall*(1-recall)/tot)),
             ci2 = recall + (1.96*sqrt(recall*(1-recall)/tot)))
  
    specificity_df <- scores %>% summarize(specificity = true_negative/(true_negative + false_positive),
                                           tot = (true_negative + false_positive)) %>% 
      mutate(ci1 = specificity - (1.96*sqrt(specificity*(1-specificity)/tot)),
             ci2 = specificity + (1.96*sqrt(specificity*(1-specificity)/tot)))
  
    precision_df <- scores %>% summarize(precision = true_positive/(true_positive + false_positive),
                                         tot = (true_positive + false_positive)) %>% 
      mutate(ci1 = precision - (1.96*sqrt(precision*(1-precision)/tot)),
             ci2 = precision + (1.96*sqrt(precision*(1-precision)/tot)))
  
    accuracy_df <- scores %>% mutate(acc = (true_positive + true_negative)/(tot)) %>% 
      mutate(ci1 = acc - (1.96*sqrt(acc*(1-acc)/tot)),
             ci2 = acc + (1.96*sqrt(acc*(1-acc)/tot))) %>% 
      select(acc,ci1,ci2)
    
    F1_df <- scores %>% summarize(F1 = 2*true_positive/(2*true_positive + false_positive + false_negative),
                                  tot = 2*true_positive + false_positive + false_negative) %>% 
      mutate(ci1 = F1 - (1.96*sqrt(F1*(1-F1)/tot)),
             ci2 = F1 + (1.96*sqrt(F1*(1-F1)/tot))) %>% 
      select(F1,ci1,ci2) %>% mutate(b1 = NA, b2 = NA)
  
    precision_all <- merge(precision_df,precision_beta)
    recall_all <- merge(recall_df,recall_beta)
    acc_all <- merge(accuracy_df,acc_beta)
    specificity_all <- merge(specificity_df,spec_beta)
    
    precision_all <- precision_all %>% select(precision,b1,b2,ci1,ci2) %>% rename(estimate = precision) %>% 
      mutate(metric = "precision")
    recall_all <- recall_all %>% select(recall,b1,b2,ci1,ci2) %>% rename(estimate = recall) %>% 
      mutate(metric = "recall")
    acc_all <- acc_all %>% select(acc,b1,b2,ci1,ci2)%>% rename(estimate = acc) %>% 
      mutate(metric = "accuracy")
    specificity_all <- specificity_all %>% select(specificity,b1,b2,ci1,ci2) %>%
      rename(estimate = specificity) %>% 
      mutate(metric = "specificity")
  
    FI_all <- F1_df %>% select(F1,b1,b2,ci1,ci2) %>%
      rename(estimate = F1) %>% 
      mutate(metric = "F1")
  
    all_metrics <- rbind(recall_all, acc_all, precision_all,specificity_all,FI_all)
    all_metrics$thresh <- thresh
  
    return(list(all_metrics,scores))
  
}

get_conf_int <- function(m, x, xname, type, mod){
  
  if (type == 'numeric'){
    pvals <- seq(min(x[,xname]), max(x[,xname]),length.out = 51)
  } else if (type %in% c('categorical','binary')){
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
      y.hat <- stats::predict(m, xtemp, type = 'prob')[,1]
    } else if (mod == "xgb"){
      y.hat <- stats::predict(m, xtemp, type = 'prob')
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
  
  m.ci$param <- xname
  
  return(m.ci)
  
}

get_conf_int_split <- function(m, x, xname, type, mod){
  
  if (type == 'numeric'){
    pvals <- seq(min(x[,xname]), max(x[,xname]),length.out = 51)
  } else if (type %in% c('categorical','binary')){
    pvals <- unlist(array(unique(x[,xname])))
  }
  
  abs.mean <- vector()
  abs.lb1 <- vector()
  abs.ub1 <- vector()
  abs.lb2 <- vector()
  abs.ub2 <- vector()
  abs.lb3 <- vector()
  abs.ub3 <- vector()
  
  prs.mean <- vector()
  prs.lb1 <- vector()
  prs.ub1 <- vector()
  prs.lb2 <- vector()
  prs.ub2 <- vector()
  prs.lb3 <- vector()
  prs.ub3 <- vector()
  for (i in 1:length(pvals)){
    xtemp <- x
    xtemp[, xname] <- pvals[i]
    
    if(mod == "rf"){
      y.hat <- stats::predict(m, xtemp, type = 'prob')[,1]
    } else if (mod == "xgb"){
      y.hat <- stats::predict(m, xtemp, type = 'prob')
    } else {
      y.hat <- NA
    }
    
    split_df <- data.frame(x = x$present,y = y.hat)
    
    abs.mean[i] <- stats::weighted.mean(split_df[split_df$x == 'absent',]$y)
    abs.lb1[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.025)
    abs.lb2[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.1)
    abs.lb3[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.25)
    abs.ub3[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.75)
    abs.ub2[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.9)
    abs.ub1[i] <- stats::quantile(split_df[split_df$x == 'absent',]$y, 0.975)
    
    prs.mean[i] <- stats::weighted.mean(split_df[split_df$x == 'present',]$y)
    prs.lb1[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.025)
    prs.lb2[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.1)
    prs.lb3[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.25)
    prs.ub3[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.75)
    prs.ub2[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.9)
    prs.ub1[i] <- stats::quantile(split_df[split_df$x == 'present',]$y, 0.975)
    
  }
  a.ci <- data.frame(value = pvals, yhat = abs.mean, lb1 = abs.lb1, lb2 = abs.lb2, lb3 = abs.lb3,
                     ub1 = abs.ub1, ub2 = abs.ub2,ub3 = abs.ub3, outcome ='absent')
  p.ci <- data.frame(value = pvals, yhat = prs.mean, lb1 = prs.lb1, lb2 = prs.lb2, lb3 = prs.lb3,
                     ub1 = prs.ub1, ub2 = prs.ub2,ub3 = prs.ub3, outcome ='present')
  m.ci <- rbind(a.ci,p.ci)

  m.ci$param <- xname
  
  return(m.ci)
  
}
