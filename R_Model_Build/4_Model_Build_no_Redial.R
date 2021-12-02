library(ellen)
library(phones)
library(xgboost)
library(xgboosttools)
library(ROCR)
library(timbr)
library(ezxl)
library(mkivtools) 
library(haven)
library(tidyverse)
library(data.table)
library(LNRSPhones)
library(GenPack)

options(scipen = 999)

file_dir_1 <- "E:/Useful Code/Phones/phone_mkiv"

register_mkiv(paste0(file_dir_1, "/phone_mk_iv_3_1_3.sas"))
mkiv <- parse_mkiv(paste0(file_dir_1, "/phone_mk_iv_3_1_3.sas"))$info[, 1:7]
View(mkiv)


df <- readRDS("./files/may_eqp1_inq1_mkiv.RDS") %>% 
  select(-md_good_recent_act, -mpp_src_ldate_bad_mth) # no longer in mkiv

may_redial <- readRDS("./files/may_scored_redial.RDS")

may_dial <- may_redial %>% 
  mutate(rpc_status = ifelse(rpc_status %in% c("FAX", "LEAD PHONE", "DECEASED"), "OTHER", rpc_status)) %>% 
  filter(notin(rpc_status, c("NOT DIALED", "NOT ENOUGH INFO"))) %>% 
  select(account, phone, rpc_status)

df <- df %>% 
  left_join(may_dial, by = c("acctno" = "account",
                             "gathered_ph" = "phone")) %>% 
  filter(!is.na(rpc_status)) %>% 
  filter(rpc_status != "DECEASED")


## Score Disc_score ##

onyx_disc_mod <- readRDS("./models/disc_score_mod.RDS")

onyx_score <- onyx_disc_mod$predict(newdata = df, scaled = FALSE) #score w/ onyx
df$disc_score <- round(onyx_score, digits = 6)

## Read in Accounts ##

dev_accounts <- readRDS("./account_splits/dev_accounts.rds")
holdout_accounts <- readRDS("./account_splits/holdout_accounts.rds")
train_accounts <- readRDS("./account_splits/train_accounts.rds")
test_accounts <- readRDS("./account_splits/test_accounts.rds")

development <- which(df$acctno %in% dev_accounts)
holdout <- which(df$acctno %in% holdout_accounts)
train <- which(df$acctno %in% train_accounts)
test <- which(df$acctno %in% test_accounts)

## Keep New Vars ##

keep_vars <- readRDS("./vars/keep_vars.RDS")

## Create Weights ##

df <- df %>% 
  mutate(dv_VERIFIED = ifelse(rpc_status == "VERIFIED", 1, 0),
         dv_DISC = ifelse(rpc_status == "DISC.", 1, 0),
         dv_WRONG = ifelse(rpc_status == "WRONG PARTY", 1, 0)) %>% 
  mutate(weights = case_when(dv_VERIFIED == 1 ~ 1,
                             dv_DISC == 1 ~ 1,
                             dv_WRONG == 1 ~ 0.25,
                             TRUE ~ 0)) %>% 
  rename(status = rpc_status)

saveRDS(df,
        file = "./files/may21_modeling_df_no_redial.RDS")

MISSING <- -99999

#### Baseline Model ####

df_train <- df[train,c("dv_VERIFIED", "dv_DISC","dv_WRONG","status","acctno", "weights")]
df_test <- df[test,c("dv_VERIFIED",  "dv_DISC","dv_WRONG","status","acctno", "weights")]

X_train <- as.matrix(df[train,keep_vars])
X_train[is.na(X_train)] <- MISSING
X_train_sub <- X_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),]
X_train_weight <- df_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),"weights"]
X_test <- as.matrix(df[test,keep_vars])
X_test[is.na(X_test)] <- MISSING
X_test_sub <- X_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),]
X_test_weight <- df_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),"weights"]
X <- as.matrix(df[,keep_vars])
X[is.na(X)] <- MISSING

y_train <- df$dv_VERIFIED[train]
y_test <- df$dv_VERIFIED[test]

y_train_sub <- as.factor(y_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1)])
y_test_sub <- as.factor(y_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1)])
y <- df$dv_VERIFIED


# RPC v DISC+WPC

dtrain <- xgb.DMatrix(data = X_train_sub, 
                      label = as.numeric(y_train_sub)-1,
                      weight = X_train_weight$weights,
                      missing = MISSING)

dtest <- xgb.DMatrix(data = X_test_sub, 
                     label = as.numeric(y_test_sub)-1,
                     weight = X_test_weight$weights, 
                     missing = MISSING)

feature_names <- readRDS("./vars/feature_names_challenger.RDS")

KS_func <- function(preds,dtrain){
  labels <- getinfo(dtrain, "label")
  p   <- ROCR::prediction(as.numeric(preds),labels)
  perf <- ROCR::performance(p, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]]))
  return(list(metric = "KS", value = ks))
}

#KS_func(pred_mod_temp, dtrain)

KS_func_general <- function(preds,labels){
  require(ROCR)
  p   <- prediction(as.numeric(preds),labels)
  perf <- performance(p, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]]))
  return(list(metric = "KS", value = ks))
}


# Build final model
params <- list(
  booster = "gbtree",
  tree_method = "hist",
  max_bin = 256,
  grow_policy = "lossguide",
  max_depth = 0,
  eta = 0.05, 
  max_leaves = 14,
  min_child_weight = 30,
  subsample = .9,
  gamma = 0,
  lambda = 1,
  objective = "binary:logitraw",
  eval_metric = KS_func
)

mod_1 <- xgb.train(
  data = dtrain, # training dataset - can't be a data.frame.
  params = params,
  maximize = TRUE, # if eval_metric should be maximized to determine early stopping
  missing = NA,
  watchlist = list(dev = dtrain, val = dtest), # datasets to calculate evaluation metrics on.
  early_stopping_rounds = 150, # use watchlist and early_stopping_rounds if don't do xgb.cv to find best_nrounds
  nrounds = 1000 # increase if it doesn't stop before nrounds is reached
)

# dir.create("./models")
saveRDS(mod_1, "./models/challenger_mod_no_redial.RDS")
mod_1 <- readRDS("./models/challenger_mod_no_redial.RDS")

imp <- xgb.importance(feature_names = mod_1$feature_names, model = mod_1)
imp[1:50,]

pred_mod_temp1 <- predict(mod_1, newdata = X, missing = MISSING)
ks_df_mod_temp1 <- data.frame(
  score = pred_mod_temp1,
  R_WEIGHT = rep(1,nrow(X)),
  R_GOOD = df$dv_VERIFIED,
  key_acct = df$key_acct,
  key_acct_phone = df$key_acct_phone,
  status = df$status,
  party = substr(df$acctno, start = 6, stop = 6)
)

phone_rank_fun <- function(df){
  
  df <- df %>% 
    group_by_at("key_acct") %>% 
    mutate(challenger_rank = order(order(score, decreasing = TRUE))) %>%  # rank new scores
    ungroup() %>% 
    mutate(across(ends_with("rank"),
                  function(x) ifelse(x > 5, 6, x)))
  
  var <- "challenger_rank"
  
  dist_tot <- df %>% 
    group_by_at(var) %>% 
    dplyr::summarize(Count = n()) %>% 
    ungroup() %>% 
    mutate(Pct_Accts = percent(Count/nrow(df))) %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_ver <- df %>% 
    filter(status == 'VERIFIED') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Verified = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_wp <- df %>% 
    filter(status == 'WRONG PARTY') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_WrongParty = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_disc <- df %>% 
    filter(status == 'DISC.') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Disc = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_mach <- df %>% 
    filter(status == 'MACHINE') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Machine = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_noans <- df %>% 
    filter(status == 'NO ANSWER') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_NoAnswer = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_oth <- df %>% 
    filter(status %in% c('OTHER')) %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Other = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_fin <- dist_tot %>% 
    left_join(dist_ver, by = var) %>% 
    left_join(dist_wp, by = var) %>% 
    left_join(dist_disc, by = var) %>% 
    left_join(dist_mach, by = var) %>% 
    left_join(dist_noans, by = var) %>% 
    left_join(dist_oth, by = var) %>% 
    mutate(across(.cols = starts_with('N_'),
                  .fns = function(x) ifelse(is.na(x), 0, x))) %>% 
    mutate(N_Contact = N_Verified + N_Machine + N_NoAnswer)  %>%  
    mutate(Contact_CumRate = percent(cumsum(N_Contact)/sum(N_Contact)),
           Verified_CumRate = percent(cumsum(N_Verified)/sum(N_Verified)),
           WrongParty_CumRate = percent(cumsum(N_WrongParty)/sum(N_WrongParty)),
           Disc_CumRate = percent(cumsum(N_Disc)/sum(N_Disc)),
           Machine_CumRate = percent(cumsum(N_Machine)/sum(N_Machine)),
           NoAnswer_CumRate = percent(cumsum(N_NoAnswer)/sum(N_NoAnswer)),
           Other_CumRate = percent(cumsum(N_Other)/sum(N_Other))) %>% 
    mutate(across(ends_with("CumRate"), 
                  .fns = function(x) as.character(x))) %>% 
    adorn_totals('row', fill = "100.00%") %>%
    mutate(Verified_Rate = percent(N_Verified/Count),
           WrongParty_Rate = percent(N_WrongParty/Count),
           Disc_Rate = percent(N_Disc/Count),
           Machine_Rate = percent(N_Machine/Count),
           NoAnswer_Rate = percent(N_NoAnswer/Count),
           Other_Rate = percent(N_Other/Count),
           Contact_Rate = percent(N_Contact/Count)) %>%
    select(starts_with(var), Count, Pct_Accts, N_Contact, Contact_Rate, Contact_CumRate, 
           N_Verified, Verified_Rate, Verified_CumRate, N_WrongParty, WrongParty_Rate, WrongParty_CumRate,
           N_Disc, Disc_Rate, Disc_CumRate, N_Machine, Machine_Rate, Machine_CumRate,
           N_NoAnswer, NoAnswer_Rate, NoAnswer_CumRate, N_Other, Other_Rate, Other_CumRate)
  
  
  return(dist_fin)
}

assess_model <- function(df){
  if(nrow(df)!=(length(train)+length(test)+length(holdout))){
    warning("length of dataframe does not match correct length")
  }
  df_holdout <- df[holdout,]
  print("Train KS")
  print(KS_func_general(df$score[train], df$R_GOOD[train])$value)
  print("Test KS")
  print(KS_func_general(df$score[test], df$R_GOOD[test])$value)
  print("Holdout KS")
  print(KS_func_general(df_holdout$score, df_holdout$R_GOOD)$value)
  
  ks_over <- KS_func_general(df_holdout$score, df_holdout$R_GOOD)$value
  ks1 <- KS_func_general(df_holdout$score[df_holdout$party==1], df_holdout$R_GOOD[df_holdout$party==1])$value
  ks3 <- KS_func_general(df_holdout$score[df_holdout$party==3], df_holdout$R_GOOD[df_holdout$party==3])$value
  ks4 <- KS_func_general(df_holdout$score[df_holdout$party==4], df_holdout$R_GOOD[df_holdout$party==4])$value
  ks5 <- KS_func_general(df_holdout$score[df_holdout$party==5], df_holdout$R_GOOD[df_holdout$party==5])$value
  
  
  rank_all <- phone_rank_fun(df_holdout)
  rank1 <- phone_rank_fun(df_holdout[df_holdout$party==1,])
  rank3 <- phone_rank_fun(df_holdout[df_holdout$party==3,])
  rank4 <- phone_rank_fun(df_holdout[df_holdout$party==4,])
  rank5 <- phone_rank_fun(df_holdout[df_holdout$party==5,])
  
  return(list("All Phones",paste0("KS: ",round(ks_over,digits = 4)),rank_all,
              "First Party Contact",paste0("KS: ",round(ks1,digits = 4)),rank1, 
              "Third Party Contact",paste0("KS: ", round(ks3,digits = 4)),rank3, 
              "Fraud",paste0("KS: ", round(ks4,digits = 4)),rank4,
              "New Accts",paste0("KS: ", round(ks5,digits = 4)),rank5
  )
  )
}

temp1 <- assess_model(ks_df_mod_temp1)

# dir.create("./model_assessments")

saveRDS(temp1,
        file = "./model_assessments/base_no_redial_holdout_assessment.RDS")

## Create Workbook (Mod 1) ##

library(openxlsx)
library(ezxl)

wb_fun <- function(mod, mod_assess, mkiv){
  
  wb <- createWorkbook()
  wb <- ezxl(mod_assess, wb = wb, sheet = "Holdout Performance")
  
  pct_col_style <- createStyle(halign = 'right')
  addStyle(wb = wb, sheet = "Holdout Performance", pct_col_style, rows = 1:100, cols = c(3,5,6,8,9,11,12,14,15,17,18,20,21,23,24), gridExpand = TRUE)
  
  
  # get a data frame with all model variables
  imp <- xgb.importance(feature_names = mod$feature_names, mod)
  
  mod_summary <- as_tibble(imp[,c(1,2)]) # change summary to data frame
  mod_summary
  
  mod_summary <- mod_summary %>%
    left_join(mkiv[, c("var", "description", "group")], by = c("Feature" = "var")) %>% 
    rename(Description = description,
           Group = group) %>% 
    mutate(Group = ifelse(substr(Feature, start = 1, stop = 3) == "mpp", "PhonesPlus", Group)) %>% 
    mutate(Group = ifelse(substr(Feature, start = 1, stop = 4) == "minq", "Inquiries", Group))
  
  
  # summarize model by group tag
  
  mod_group_summary <- mod_summary %>%
    group_by(Group) %>%
    dplyr::summarise(
      variable_count = n(),
      group_rel_inf = sum(Gain)
    ) %>%
    arrange(desc(group_rel_inf))
  
  # create a summary of the groups that are not present in the model
  groups_in_mod <- unique(mod_summary$Group)
  all_groups <- unique(mkiv$group)
  
  groups_not_in_mod <- tibble(Groups_Not_In_Model = sort(setdiff(all_groups, groups_in_mod)))
  
  # output xlsx workbook
  
  addWorksheet(wb, "Model Vars Summary")
  writeDataTable(wb, "Model Vars Summary", mod_summary)
  
  writeDataTable(wb, "Model Vars Summary", mod_group_summary, startCol = 6, startRow = 1)
  
  writeDataTable(wb, "Model Vars Summary", groups_not_in_mod, startCol = 10, startRow = 1)
  
  setColWidths(wb, "Holdout Performance", cols = 1:24, widths = 'auto')
  setColWidths(wb, "Model Vars Summary", cols = 1:24, widths = 'auto')
  
  return(wb)
}

wb_1 <- wb_fun(mod = mod_1, mod_assess = temp1, mkiv = mkiv)

openXL(wb_1)


#### Var Reduction ####

keep_vars <- imp$Feature # take variables that come into the first model

library(mlr3)
library(mlr3filters)
library(mlr3learners)
library(mlr3fselect)


## Only keep variables that come into the first model ##

X_train <- as.matrix(df[train,keep_vars])
X_train[is.na(X_train)] <- MISSING
X_train_sub <- X_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),]
X_train_weight <- df_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),"weights"]
X_test <- as.matrix(df[test,keep_vars])
X_test[is.na(X_test)] <- MISSING
X_test_sub <- X_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),]
X_test_weight <- df_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),"weights"]
X <- as.matrix(df[,keep_vars])
X[is.na(X)] <- MISSING


## Create training task ## 

train_df <- as.data.frame(X_train_sub)
test_df <- as.data.frame(X_test_sub)
all_df <- rbind.data.frame(train_df, test_df)

train_target <- c(y_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1)],
                  y_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1)])

all_df$target <- as.factor(train_target)


train_weight <- c(df_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),"weights"]$weights,
                  df_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),"weights"]$weights)

all_df$weights <- train_weight # assign weights to the dataframe

train_task <- TaskClassif$new(id = "Feat_Sel",  #Create Task
                              backend = all_df,
                              target = "target",
                              positive = as.character(1))

train_task$set_col_roles("weights", roles = "weight")  # adds weights for WP


set.seed(10) # make reproducible

mlr_learners$get("classif.xgboost")
lrn <- lrn("classif.xgboost") # use xgb for learning

lrn$param_set$values <- list(booster = "gbtree",
                             tree_method = "hist",
                             max_bin = 256,
                             grow_policy = "lossguide",
                             max_depth = 0,
                             eta = 0.1, 
                             max_leaves = 14,
                             min_child_weight = 30,
                             subsample = .9,
                             gamma = 0,
                             lambda = 1,
                             objective = "binary:logitraw",
                             eval_metric = "error",
                             nrounds = 500L)   # Set parameters for learning on feature selection

# filter$calculate(train_task)
# head(as.data.table(filter), 100)

hout <- rsmp("holdout") # use holdout re-sampling
measure <- msr("classif.ce") # classification error for performance measure
evals10 <- trm("evals", n_evals = 12) # 12 different evaluations, change if more feature number specifications should be made

mlr_fselectors$get("rfe")
fselector <- fs("rfe") #recursive feature elimination

keep_var_subset <- c(length(keep_vars)-1, 150, 125, 110, 100, 90, 85, 80, 75, 70, 65, 60)

fselector$param_set$values <- list(min_features = 50,
                                   subset_sizes = keep_var_subset,
                                   recursive = TRUE) # need to specify recursive argument, this creates new models everytime features are subset


instance <- FSelectInstanceSingleCrit$new(
  task = train_task,
  learner = lrn,
  resampling = hout,
  measure = measure,
  terminator = evals10,
  store_models = TRUE # needs to be true for recursive, or it will throw error
)

fselector$optimize(instance)

instance$result_feature_set # best feature set

result_df <- as.data.table(instance$archive) # all feature sets

plot(result_df$batch_nr, result_df$classif.ce)

best_feat_set <- 7 # pick which feature set to keep

keep_vars_df <- data.frame(Feature = names(result_df)[1:length(keep_vars)],
                           Include = as.numeric(result_df[best_feat_set,])[1:length(keep_vars)]) %>% 
  filter(Include == 1)

keep_vars_2 <- keep_vars_df$Feature

# keep_vars_2 <- feature_names

### All good, no character variables found

df_train <- df[train,c("dv_VERIFIED", "dv_DISC","dv_WRONG","status","key_acct", "weights")]
df_test <- df[test,c("dv_VERIFIED",  "dv_DISC","dv_WRONG","status","key_acct", "weights")]

X_train <- as.matrix(df[train,keep_vars_2])
X_train[is.na(X_train)] <- MISSING
X_train_sub <- X_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),]
X_train_weight <- df_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),"weights"]
X_test <- as.matrix(df[test,keep_vars_2])
X_test[is.na(X_test)] <- MISSING
X_test_sub <- X_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),]
X_test_weight <- df_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),"weights"]
X <- as.matrix(df[,keep_vars_2])
X[is.na(X)] <- MISSING

y_train <- df$dv_VERIFIED[train]
y_test <- df$dv_VERIFIED[test]

y_train_sub <- as.factor(y_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1)])
y_test_sub <- as.factor(y_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1)])
y <- df$dv_VERIFIED


# RPC v DISC+WPC

dtrain <- xgb.DMatrix(data = X_train_sub, 
                      label = as.numeric(y_train_sub)-1,
                      weight = X_train_weight$weights,
                      missing=MISSING)

dtest <- xgb.DMatrix(data = X_test_sub, 
                     label = as.numeric(y_test_sub)-1,
                     weight = X_test_weight$weights, 
                     missing=MISSING)


feature_names <- colnames(dtrain)
saveRDS(feature_names, file = "./vars/feature_names_challenger_no_redial_var_reduct.RDS")
feature_names <- readRDS("./vars/feature_names_challenger_no_redial_var_reduct.RDS")


# Build model with less variables
params <- list(
  booster = "gbtree",
  tree_method = "hist",
  max_bin = 256,
  grow_policy = "lossguide",
  max_depth = 0,
  eta = 0.1, 
  max_leaves = 14,
  min_child_weight = 30,
  subsample = .9,
  gamma = 0,
  lambda = 1,
  objective = "binary:logitraw",
  eval_metric = KS_func
)

mod_2 <- xgb.train(
  data = dtrain, # training dataset - can't be a data.frame.
  params = params,
  maximize = TRUE, # if eval_metric should be maximized to determine early stopping
  missing = NA,
  watchlist = list(dev = dtrain, val = dtest), # datasets to calculate evaluation metrics on.
  early_stopping_rounds = 100, # use watchlist and early_stopping_rounds if don't do xgb.cv to find best_nrounds
  nrounds = 1000 # increase if it doesn't stop before nrounds is reached
)


saveRDS(mod_2, "./models/challenger_mod_no_redial_reduct.RDS")
mod_2 <- readRDS("./models/challenger_mod_no_redial_reduct.RDS")

imp <- xgb.importance(feature_names = mod_2$feature_names, model = mod_2)
imp[1:50,]

pred_mod_temp2 <- predict(mod_2, newdata = X, missing = MISSING)
ks_df_mod_temp2 <- data.frame(
  score = pred_mod_temp2,
  R_WEIGHT = rep(1,nrow(X)),
  R_GOOD = df$dv_VERIFIED,
  key_acct = df$key_acct,
  key_acct_phone = df$key_acct_phone,
  status = df$status,
  party = substr(df$acctno, start = 6, stop = 6)
)

temp2 <- assess_model(ks_df_mod_temp2)

saveRDS(temp2,
        file = "./model_assessments/var_reduct_no_redial_holdout_assessment.RDS")

## Create Workbook ##

wb_2 <- wb_fun(mod = mod_2, mod_assess = temp2, mkiv = mkiv)

openXL(wb_2)


#### Tune Final Model ####

library(mlr3)
library(mlr3verse)
library(mlr3tuning)
library(data.table)
library(R6)

## Create Task ##

train_df <- as.data.frame(X_train_sub)
test_df <- as.data.frame(X_test_sub)
all_df <- rbind.data.frame(train_df, test_df)

train_target <- c(y_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1)],
                  y_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1)])

all_df$target <- as.factor(train_target)


train_weight <- c(df_train[which(df_train$dv_VERIFIED+df_train$dv_DISC+df_train$dv_WRONG==1),"weights"]$weights,
                  df_test[which(df_test$dv_VERIFIED+df_test$dv_DISC+df_test$dv_WRONG==1),"weights"]$weights)

all_df$weights <- train_weight

train_task <- TaskClassif$new(id = "Tuned",
                              backend = all_df,
                              target = "target",
                              positive = as.character(1))

train_task$set_col_roles("weights", roles = "weight")  # adds weights for WP

print(train_task)


## Create XGB Learner ##

learner <- mlr_learners$get("classif.xgboost")

learner$param_set$values <- list(booster = "gbtree",
                                 tree_method = "hist",
                                 max_bin = 256,
                                 max_depth = 0,
                                 grow_policy = "lossguide",
                                 objective = "binary:logitraw",
                                 eval_metric = "error",
                                 eta = 0.1,
                                 nrounds = 500L)

## Create Parameter Search Space ##

library(paradox)

search_space = ps(
  max_leaves = p_int(lower = 12L, upper = 16L),
  min_child_weight = p_dbl(lower = 10, upper = 35), # extra parameters can be added here as well
  subsample = p_dbl(lower = 0.9, upper = 1)
)

## Re-sampling & Performance Measure ##

rsamp <- rsmp("holdout") # resampling based on holdout sample

measure <- msr("classif.ce") # classification error as measurement of accuracy

## Termination ##

evals <- trm("evals",
             n_evals = 25)

## Tuning Instance ##

instance <- TuningInstanceSingleCrit$new(
  task = train_task,
  learner = learner,
  resampling = rsamp,
  measure = measure,
  search_space = search_space,
  terminator = evals
)

## Search Method ##

tuner = tnr("random_search") # random search

## Tune #

tuner$optimize(instance)

param_tuning_df <- as.data.table(instance$archive) # data table of each result

oth_msr <- as.data.table(instance$archive$benchmark_result$score(msrs(c("classif.acc", 
                                                                        "classif.auc", 
                                                                        "classif.fpr", 
                                                                        "classif.tpr")))) %>% # score on a different measure if wanted
  select_at(.vars = c("uhash","classif.acc", "classif.auc", "classif.fpr", "classif.tpr" ))
  

param_tuning_df_final <- param_tuning_df %>% 
  left_join(oth_msr, by = c("uhash" = "uhash"))

plot(param_tuning_df$max_leaves, param_tuning_df$classif.ce)  # Want lowest classification error
plot(param_tuning_df$min_child_weight, param_tuning_df$classif.ce)
plot(param_tuning_df$subsample, param_tuning_df$classif.ce)


# Build final model
params <- list(
  booster = "gbtree",
  tree_method = "hist",
  max_bin = 256,
  grow_policy = "lossguide",
  max_depth = 0,
  eta = 0.1,
  max_leaves = 13,
  min_child_weight = 21.65,
  subsample = 0.91,
  colsample_bytree = 1,
  gamma = 0,
  lambda = 1,
  objective = "binary:logitraw",
  eval_metric = KS_func
)

mod_3 <- xgb.train(
  data = dtrain, # training dataset - can't be a data.frame.
  params = params,
  maximize = TRUE, # if eval_metric should be maximized to determine early stopping
  missing = NA,
  watchlist = list(dev = dtrain, val = dtest), # datasets to calculate evaluation metrics on.
  early_stopping_rounds = 150, # use watchlist and early_stopping_rounds if don't do xgb.cv to find best_nrounds
  nrounds = 1000 # increase if it doesn't stop before nrounds is reached
)

saveRDS(mod_3, "./models/challenger_mod_no_redial_reduct_tuned.rds")
mod_3 <- readRDS("./models/challenger_mod_no_redial_reduct_tuned.rds")

imp <- xgb.importance(feature_names = mod_3$feature_names, model = mod_3)
imp[1:50,]

pred_mod_temp3 <- predict(mod_3, newdata = X, missing = MISSING)
ks_df_mod_temp3 <- data.frame(
  score = pred_mod_temp3,
  R_WEIGHT = rep(1,nrow(X)),
  R_GOOD = df$dv_VERIFIED,
  key_acct = df$key_acct,
  key_acct_phone = df$key_acct_phone,
  status = df$status,
  party = substr(df$acctno, start = 6, stop = 6)
)

temp3 <- assess_model(ks_df_mod_temp3)

saveRDS(temp3,
        file = "./model_assessments/var_reduct_tuned_no_redial_holdout_assessment.RDS")

## Create Workbook ##

wb_3 <- wb_fun(mod = mod_3, mod_assess = temp3, mkiv = mkiv)

openXL(wb_3)
