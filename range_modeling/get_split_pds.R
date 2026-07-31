library(tidyverse)
library(randomForest)
#library(MESS)
library(pROC)
library(zoo)
library(foreach)
library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

t0 <- Sys.time()

print(paste0("Universe size = ", mpi.universe.size()))
print(paste0("Number of workers = ", mpi.comm.size(0) - 1))

num_trees_per_worker <- 200
num_workers <- mpi.comm.size(0) - 1
print(num_workers)

source("fitting_functions.R")

lag_temp <- 35
lag_pr <- 35
lag_rh <- 35

var_names_long <- read_csv("data/var_names_pa2.csv")

#load(paste0('rf_output/rf_update_0_lag', lag_temp,lag_pr,lag_rh,".RData"))
#rf_train_lag0 <- read_csv(paste0("rf_output/training_update_0_lag",lag_temp,lag_pr,lag_rh,".csv"))

load(paste0('rf_output4/rf_models1_0_lag', lag_temp,lag_pr,lag_rh,".RData"))
rf_train_lag0 <- read_csv(paste0("rf_output4/training_0_lag",lag_temp,lag_pr,lag_rh,".csv"))

var_imp_all <- data.frame(importance(rf_model))
var_imp_all$variables <- rownames(var_imp_all)

var_imp_all <- merge(var_names_long,var_imp_all)

imp_vars <- var_imp_all %>% filter(MeanDecreaseGini >0) %>% arrange(desc(MeanDecreaseGini)) #%>% head(num_workers*2)

uni_vars <- length(imp_vars$variables)

steps <- (uni_vars - (uni_vars %% num_workers))/num_workers
extra <- uni_vars %% num_workers 

all_pds <- c()
for(i in 1:steps){
  
  temp <- foreach(i = (num_workers*(i-1) + 1):(num_workers*i), .combine = 'rbind') %dopar% 
    get_conf_int_split(rf_model, rf_train_lag0,imp_vars$variables[i],imp_vars$type[i], mod = 'rf')
  
  all_pds <- rbind(all_pds,temp)
}

if(extra >0){
  temp <- foreach(i = (num_workers*(i) + 1):(num_workers*i + extra), .combine = 'rbind') %dopar% 
    get_conf_int_split(rf_model, rf_train_lag0,imp_vars$variables[i],imp_vars$type[i], mod = 'rf')
  all_pds <- rbind(all_pds,temp)
}

write_csv(all_pds, paste0("rf_output4/split_pds_update_0_lag",lag_temp,lag_pr,lag_rh,".csv"))

paste0("Finished script in ", round(difftime(Sys.time(), t0, units = 'mins'),2), " minutes")

closeCluster(cl)
mpi.quit()