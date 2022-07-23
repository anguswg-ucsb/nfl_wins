
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tidymodels)
library(glmnet)
library(doParallel)
library(workflowsets)
library(finetune)
library(themis)

source("utils/utils.R")

data_path  <-  here::here("data")
save_path <- "D:/nfl/classification/"

# *******************
# ---- Load Data ----
# *******************
na_df <- nfl_df[rowSums(is.na(nfl_df)) > 0, ]

# # Load Lag NFL win data
# nfl_df <- readRDS(here::here("data", "football_wins_lag.rds")) %>% 
#   dplyr::select(-contains("qtr_pts_6")) %>% 
#   dplyr::mutate(
#     div_game  = factor(div_game, levels = c(1, 0)),
#     rest_days = dplyr::case_when(
#       rest_days < 7   ~  "short_rest",
#       rest_days == 7  ~  "normal_rest",
#       rest_days > 7   ~  "long_rest"
#       ),
#     opp_rest_days = dplyr::case_when(
#       opp_rest_days < 7   ~  "short_rest",
#       opp_rest_days == 7  ~  "normal_rest",
#       opp_rest_days > 7   ~  "long_rest"
#     ),
#     win = factor(win, levels = c(1, 0))
#     ) %>% 
#   dplyr::mutate(
#     across((contains("dscore")), ~replace(., is.na(.), 0))
#   ) %>% 
#   na.omit() %>%
#   dplyr::select(-week, -season, -score_drives, -ndrives,-def_score_drives, -def_ndrives,
#                 -opp_score_drives, -opp_ndrives, -opp_def_score_drives, -opp_def_ndrives,
#                 -drive_time_of_possession_sec,
#                 -opp_drive_time_of_possession_sec)
# game_spreads <- readRDS(here::here("data", "football_closing_spread_lag.rds")) %>% 
#   dplyr::select(game_id, team, opponent, spread_line)

   # dplyr::select(-spread_line)
# win_cor <-
#   model_dat %>%
#   dplyr::filter(home == 1) %>%
#   dplyr::mutate(win = as.numeric(win)) %>%
#   # dplyr::select(win, names(win_lag)[c(15:18, 20:34)]) %>%
#   dplyr::select(where(is.numeric)) %>%
#   dplyr::select(-contains("opp")) %>% 
#   cor(use =  "pairwise.complete.obs") %>%
#   round(1) %>%
#   reshape2::melt()
# win_cor %>%
#   ggplot(aes(x=Var1, y=Var2,
#                            fill=value)) +
#   geom_tile() +
#   geom_text(aes(Var2, Var1, label = value),
#             color = "black", size = 4) +
#   scale_fill_gradient2(low="darkred", high="darkgreen", guide="colorbar") +
#   theme(axis.text.x = element_text(angle = -45))
# win_vars <- 
#   win_cor %>% 
#   dplyr::filter(Var1 == "win") %>% 
#   dplyr::mutate(abs_val = abs(value)) %>% 
#   arrange(-abs_val) %>% 
#   dplyr::filter(abs_val != 0) %>% 
#   tibble() %>% 
#   dplyr::mutate(Var2 = as.character(Var2)) %>% 
#   .$Var2

# Load entire dataset
model_dat <- readRDS(here::here("data", "football_wins_lag_elo.rds")) 

# Modeling subset
nfl_df <-
  model_dat  %>% 
  # dplyr::filter(season != 2019) %>% 
  dplyr::filter(season != 2021, home == 1) %>% 
  dplyr::select(-abs_spread_line, -home, -home_fav, -fav, -spread_line) %>% 
  dplyr::select(season, week, game_id, team, opponent, win, div_game, rest_days, 
                opp_rest_days, elo, opp_elo, score_diff, 
                opp_score_diff, turnovers, opp_turnovers,
                # score_diff_qtr_1, opp_score_diff_qtr_1,
                win_pct, away_win_pct, home_win_pct, 
                opp_win_pct, opp_away_win_pct, opp_home_win_pct, qb_epa, opp_qb_epa, 
                third_down_pct, opp_third_down_pct)
  # dplyr::select(-abs_spread_line, -home, -home_fav)
  # dplyr::select(-spread_line, -home)

# mod_vars <- names(nfl_df)
# saveRDS(names(nfl_df), here::here("data", "model_features.rds"))

# [1] "season"             "week"               "game_id"           
# [4] "team"               "opponent"           "win"               
# [7] "div_game"           "rest_days"          "opp_rest_days"     
# [10] "elo"                "opp_elo"            "score_diff"        
# [13] "opp_score_diff"     "turnovers"          "opp_turnovers"     
# [16] "win_pct"            "away_win_pct"       "home_win_pct"      
# [19] "opp_win_pct"        "opp_away_win_pct"   "opp_home_win_pct"  
# [22] "qb_epa"             "opp_qb_epa"         "third_down_pct"    
# [25] "opp_third_down_pct"
#   
# prep(juice(xgboost_recipe))

# Set random seed
set.seed(234)

# split data for train/test, stratify by quantiles of fantasy points 
# nfl_split <- initial_split(nfl_df, strata = season)
nfl_split <- rsample::initial_split(nfl_df, strata = win)

# nfl_split <- initial_split(nfl_df)
# training data split
nfl_train <- rsample::training(nfl_split)

# testinng data split
nfl_test  <- rsample::testing(nfl_split)

win_count_train <- 
  nfl_train %>% 
  count(win) %>% 
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games),
    split = "Train"
  ) 

win_count_test <- 
  nfl_test %>% 
  count(win) %>% 
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games),
    split = "Test"
  ) 
win_count <- dplyr::bind_rows(win_count_train, win_count_test) %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  dplyr::mutate(
    pct_lab = paste0(pct_total, "%"), 
    split = factor(split, levels = c("Train", "Test"))
    )

win_count_plot <- 
  win_count %>% 
  ggplot() +
  geom_col(aes(x = win, y = pct_total, fill = split)) +
  geom_text(aes(x = win, y = pct_total, label = pct_lab), nudge_y = -5) +
  facet_wrap(~split) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Mild Class Imbalance",
    x = "Win (1) / Loss (0)",
    y = "Percentage of dataset"
  ) +
  apatheme
win_count_plot

# Save plot
ggsave(
  "D:/nfl/eda/win_class_split.png",
  win_count_plot,
  width  = 10,
  height = 8
)

# *****************
# ---- Recipes ----
# *****************
# usemodels::use_kernlab_svm_rbf(win~., data = nfl_train)
# usemodels::use_kknn(win~., data = nfl_train)
# usemodels::use_xgboost(win~., data = nfl_train)
# usemodels::use_glmnet(win~., data = nfl_train)
# usemodels::use_earth(win~., data = nfl_train)


# Data preprocessing 
logger::log_info("Data preprocessing...")

# # K nearest neighbors
# kknn_recipe <- 
#   recipes::recipe(
#     formula = win ~ ., 
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   # step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
#   step_novel(all_nominal_predictors()) %>% 
#   step_dummy(all_nominal_predictors()) %>% 
#   # themis::step_smote(win) %>%
#   # themis::step_smote(win, over_ratio = 0.8,  skip = T) %>%      
#   step_zv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) %>% 
#   themis::step_smote(win)

# Base recipe
base_recipe <- 
  recipes::recipe(
  formula = win ~ ., 
  data    = nfl_train
  ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, week, new_role = "ID"
  ) 

# normalized recipe
norm_recipe <- 
  base_recipe %>% 
  recipes::step_novel(recipes::all_nominal_predictors()) %>% 
  recipes::step_dummy(recipes::all_nominal_predictors()) %>% 
  recipes::step_zv(recipes::all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors())

# norm_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# norm_smote_recipe <- 
#   norm_recipe %>% 
#   themis::step_up(win, over_ratio = 0.9) 

norm_smote_recipe <- 
  norm_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)


# norm_smote_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# themis::step_smote(win, over_ratio = 0.8,  skip = T)
# recipe(~., nfl_train) %>%
#   step_smote(win, over_ratio = .9) %>%
#   prep() %>%
#   bake(new_data = NULL) %>%
#   ggplot(aes(win)) +
#   geom_bar()


# One hot encoding recipe
oh_recipe <-
  base_recipe %>% 
  recipes::step_novel(all_nominal_predictors()) %>% 
  recipes::step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  recipes::step_zv(all_predictors()) 

# One hot encoding, up sampling recipe
oh_smote_recipe <- 
  oh_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)
                        
  # themis::step_up(win) 
# themis::step_smote(win, over_ratio = 0.8,  skip = T)

# nnet recipe
nnet_recipe <- 
  base_recipe %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) 

# nnet up sampling
nnet_smote_recipe <- 
  nnet_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)
  # themis::step_smote(win) 


# SVM recipe
svm_recipe <- 
  base_recipe %>% 
  recipes::step_zv(all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors()) 
  
# SVM recipe
svm_smote_recipe <- 
  svm_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)
  # themis::step_smote(win) 
  
# kknn_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# knn_juice <- juice(prep(kknn_recipe))
# count(knn_juice, win)

# glmnet linear regression
# glmnet_recipe <- 
#   recipe(
#     formula = win ~ .,
#     data    = nfl_train
#     ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   step_novel(all_nominal_predictors()) %>% 
#   step_dummy(all_nominal_predictors()) %>% 
#   # themis::step_smote(win, over_ratio = 0.8, skip = T) %>%
#   # themis::step_smote(win, over_ratio = 0.9) %>%
#   step_zv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) 

# glmnet linear regression
# glmnet_recipe <- 
#   recipe(
#     formula = win ~ .,
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   # step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
#   step_novel(all_nominal_predictors()) %>% 
#   step_dummy(all_nominal_predictors()) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) %>%
#   # themis::step_smote(win,  over_ratio = 0.8, skip       = T ) %>%
#   step_zv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) 

# glmnet_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# XGBoost trees
# xgboost_recipe <- 
#   recipes::recipe(
#     formula = win ~ ., 
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   step_novel(all_nominal_predictors()) %>% 
#   step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) %>%
#   step_zv(all_predictors()) 



# xgboost_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# Random forest model
# ranger_recipe <- 
#   recipes::recipe(
#     formula = win ~ ., 
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) 

# MARS model
# earth_recipe <- 
#   recipes::recipe(
#     formula = win ~ .,
#     data = nfl_train
#     ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   # step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
#   step_novel(all_nominal_predictors()) %>% 
#   step_dummy(all_nominal_predictors()) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) %>% 
#   step_zv(all_predictors()) 

# earth_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)

# nnet_recipe <- 
#   recipe(
#     formula = win ~ ., 
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#     # game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) %>% 
#   step_normalize(all_numeric_predictors()) 

# nnet_recipe %>% 
#   prep() %>% 
#   juice()
# 
# nnet_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)
# 
# kernlab_recipe <- 
#   recipe(
#     formula = win ~ ., 
#     data    = nfl_train
#   ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, week, new_role = "ID"
#   ) %>% 
#   themis::step_smote(win, over_ratio = 0.8,  skip = T) %>% 
#   step_zv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) 
# 
# kernlab_recipe %>% 
#   prep() %>% 
#   juice() %>% 
#   count(win)


# ********************
# ---- Model Spec ----
# ********************

knn_spec <- 
  nearest_neighbor(
    neighbors   = tune(),
    weight_func = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 

glmnet_spec <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

ranger_spec <-
  rand_forest(
    mtry  = tune(),
    min_n = tune(),
    trees = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

xgboost_spec <- 
  boost_tree(
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(), 
    learn_rate     = tune(), 
    loss_reduction = tune(), 
    sample_size    = tune()
    ) %>% 
  set_mode("classification") %>% 
  parsnip::set_engine("xgboost", importance = "permutation") 

# earth_spec <-
#   mars(
#     num_terms    = tune(),
#     prod_degree  = tune(),
#     prune_method = "none"
#     ) %>%
#   set_mode("classification") %>%
#   set_engine("earth")

nnet_spec <- 
  mlp(
    hidden_units = tune(), 
    penalty      = tune(),
    epochs       = tune()
    # dropout      = 0.5
  ) %>% 
  set_engine("nnet") %>% 
  set_mode("classification")

svm_spec <- 
  svm_rbf(
    cost      = tune(), 
    rbf_sigma = tune()
  ) %>% 
  set_mode("classification") 

svm_poly_spec <- 
  svm_poly(
    cost         = tune(),
    degree       = tune(), 
    scale_factor = tune()
    ) %>% 
  set_mode("classification") 

# ********************************
# ---- Cross Validation folds ----
# ********************************

# Set seed for resampling 
set.seed(432)

# CV folds
nfl_folds <- rsample::vfold_cv(nfl_train, v = 10, strata = win)
# nfl_folds <- rsample::bootstraps(nfl_train, strata = win)

# ---- Workflow set of models ----
nfl_wfs <-
  workflow_set(
    preproc = list(
      kknn_rec        = norm_smote_recipe,
      glmnet_rec      = norm_smote_recipe,
      xgboost_rec     = oh_smote_recipe,
      nnet_rec        = nnet_smote_recipe,
      svm_poly_rec    = svm_smote_recipe,
      svm_rbf_rec     = svm_smote_recipe
    ),
    models  = list(
      knn            = knn_spec,
      glmnet         = glmnet_spec,
      xgboost        = xgboost_spec,
      nnet           = nnet_spec,
      svm_poly       = svm_poly_spec,
      svm_rbf        = svm_spec
    ),
    cross = F
  )
# nfl_wfs <-
#   workflow_set(
#     preproc = list(
#       kknn_rec        = norm_recipe,
#       kknn_rec_up     = norm_smote_recipe,
#       glmnet_rec      = norm_recipe,
#       glmnet_rec_up   = norm_smote_recipe,
#       xgboost_rec     = oh_recipe,
#       xgboost_smote_rec  = oh_smote_recipe,
#       nnet_rec        = nnet_recipe,
#       nnet_smote_rec     = nnet_smote_recipe,
#       svm_poly_rec    = svm_recipe,
#       svm_poly_smote_rec = svm_smote_recipe,
#       svm_rbf_rec     = svm_recipe,
#       svm_rbf_smote_rec  = svm_smote_recipe
#     ),
#     models  = list(
#       knn            = knn_spec,
#       knn_up         = knn_spec,
#       glmnet         = glmnet_spec,
#       glmnet_up      = glmnet_spec,
#       xgboost        = xgboost_spec,
#       xgboost_up     = xgboost_spec,
#       nnet           = nnet_spec,
#       nnet_up        = nnet_spec,
#       svm_poly       = svm_poly_spec,
#       svm_poly_up    = svm_poly_spec,
#       svm_rbf        = svm_spec,
#       svm_rbf_up     = svm_spec
#     ),
#     cross = F
#   )

# nfl_wfs <-
#   workflow_set(
#     preproc = list(
#       kknn_rec         = kknn_recipe,
#       glmnet_rec       = glmnet_recipe,
#       # ranger_rec      = ranger_recipe,
#       xgboost_rec      = xgboost_recipe,
#       # earth_rec        = earth_recipe,
#       nnet_rec         = nnet_recipe,
#       kernlab_poly_rec = kernlab_recipe,
#       kernlab_rec      = kernlab_recipe
#     ),
#     models  = list(
#       kknn       = knn_spec,
#       glmnet     = glmnet_spec,
#       xgboost    = xgboost_spec,
#       # earth      = earth_spec,
#       nnet       = nnet_spec,
#       svm_poly   = svm_poly_spec,
#       svm        = svm_spec
#     ),
#     cross = F
#   )

# Choose metrics
met_set <- yardstick::metric_set(roc_auc, accuracy, mn_log_loss, 
                                 sensitivity, specificity, j_index)

# Set up parallelization, using computer's other cores
parallel::detectCores(logical = FALSE)
modeltime::parallel_start(6, .method = "parallel")

# Set Random seed
set.seed(589)

# Tune models in workflowset
nfl_wfs <-
  nfl_wfs %>%
  workflow_map(
    "tune_grid",
    resamples = nfl_folds ,
    grid      = 20,
    metrics   = met_set,
    control   = control_grid(
      verbose   = TRUE,
      save_pred = TRUE),
    verbose   = TRUE
  )

nfl_wfs[4,]$result[[1]]$.notes[[1]]$note

# Efficient Tuning of models in workflowset
nfl_wfs <-
  nfl_wfs %>%
  workflowsets::workflow_map(
    "tune_race_anova",
    resamples = nfl_folds,
    grid      = 20,
    metrics   = met_set,
    control = finetune::control_race(
      verbose       = TRUE,
      save_pred     = TRUE,
      verbose_elim  = TRUE,
      save_workflow = TRUE
    ),
    verbose   = TRUE
  )

  # Stop parrallelization
modeltime::parallel_stop()

nfl_wfs$result[[1]]$.notes[[1]]$note
# nfl_wfs3   <- readRDS(paste0(save_path, "wfs/win_classification_wfs2.rds"))
rank_results(nfl_wfs)
wfs_ranks <- rank_results(nfl_wfs)
wfs_ranks
# nfl_wfs$result[[2]]$.metrics
# nfl_wfs2 <- nfl_wfs
# k <- 2
# for (k in 1:length(nfl_wfs2$result)) {
#   
#   logger::log_info("{k} of {length(nfl_wfs2$result)}")
#   
#   r_len <- nfl_wfs2$result[[k]]$.metrics
#   
#   
#   for (z in 1:length(r_len)) {
#     
#     logger::log_info("{z} of {length(r_len)}")
#     
#     nfl_wfs2$result[[k]]$.metrics[[z]]  <- nfl_wfs2$result[[k]]$.metrics[[z]] %>% 
#       dplyr::filter(!.metric %in% c("spec", "sens"))
#   }
# }
# rank_results(nfl_wfs2)
# rank_results(nfl_wfs[c(1:4),])
# Comparing Accuracy and ROC AUC of 7 models
class_mod_comp_plot <-
  # nfl_wfs2 %>%
  nfl_wfs %>%
  # nfl_wfs[c(1:4),] %>%
  autoplot() + 
  labs(
    col = "",
    title    = "Classification Model comparisons",
    subtitle = "Predicting wins"
  ) +
  apatheme

class_mod_comp_plot 
  # scale_y_continuous(limits = c(0, 1))
plotly::ggplotly(class_mod_comp_plot)
save_path <- "D:/nfl/classification/"

# Save plot
ggsave(
  paste0(save_path, "plots/class_model_ranks.png"),
  class_mod_comp_plot,
  width  = 12,
  height = 10
)


saveRDS(nfl_wfs, paste0(save_path, "wfs/win_classification_wfs2.rds"))

saveRDS(
  wfs_ranks,
  paste0(save_path, "metrics/class_wfs_ranks.rds")
)

saveRDS(
  nfl_split,
  paste0(save_path, "splits/class_split_data.rds")
)
saveRDS(
  nfl_train,
  paste0(save_path, "splits/class_train_data.rds")
)

saveRDS(
  nfl_test,
  paste0(save_path, "splits/class_test_data.rds")
)

# ****************************
# ---- Select best models ----
# ****************************

metrics_lst    <- list()
conf_mat_lst   <- list()
roc_lst        <- list()
vip_lst        <- list()

# i <- 4


# rm(i, model, model_name, mod_workflow, mod_final_fit, mod_last_fit, mod_workflow_fit, resample_roc_plot, vip_plot,
#    mod_test,mod_train, train_acc, test_acc, overall_aucroc, test_metrics, train_metrics, mod_metrics, conf_mat, conf_mat_plot,conf_mat_lst, vip_lst, metrics_lst,
# roc_auc_curve_plot, mod_results)

# Extract wf set results
for (i in 1:length(nfl_wfs$wflow_id)) {
  
  logger::log_info("\n\nModel: {i} of {length(nfl_wfs$wflow_id)}")
  
  save_path <- "D:/nfl/classification/"
  
  model       <- nfl_wfs$wflow_id[i]
  model_name  <- nfl_wfs$info[[i]]$model
  
  # model
  
  mod_results <- 
    nfl_wfs %>% 
    workflowsets::extract_workflow_set_result(model)
  
  # Extract workflows
  mod_workflow <- 
    nfl_wfs %>%  
    extract_workflow(model)
  
  # Model Engine text
  model_engine <- mod_workflow$fit$actions$model$spec$engine
  
  # Model Engine text
  model_mode <- mod_workflow$fit$actions$model$spec$mode
  
  logger::log_info("\n\nExtracting workflow & finalizing model fit...\n  ---  {model_name} - {model_mode} --- ")
  # mod_results$.metrics
  # select_best(mod_results, metric = "roc_auc")
  # select_best(mod_results, metric = "mn_log_loss")
  # *************
  # ---- Fit ----
  # *************
  # mod_results$.notes[[11]]$note
  # Finalize workflow fit
  mod_workflow_fit <- 
    mod_workflow %>% 
    finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>%
    # finalize_workflow(select_best(mod_results, metric = "mn_log_loss")) %>% 
    fit(data = nfl_train)
  
  # Fit model to split train/test data
  mod_last_fit <- tune::last_fit(mod_workflow_fit, nfl_split)
  
  # print(tune::collect_metrics(mod_results)$mean)
  print(tune::collect_metrics(mod_last_fit))
  
  # Extract & save final fit to use for predictions
  mod_final_fit <- mod_last_fit$.workflow[[1]]
  
  # Resampled CV Fold AUC ROC Curve
  resample_roc_plot <-
    mod_results %>%
    collect_predictions() %>%
    group_by(id) %>% 
    roc_curve(win, .pred_1) %>%
    ggplot(aes(1 - specificity, sensitivity, color = id)) +
    geom_abline(lty = 2, color = "gray80", size = 1.5) +
    geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
    coord_equal() +
    labs(
      title    = paste0("ROC Curve - ", model_name),
      subtitle = "Resample results from 10 Fold Cross Validation",
      x        = "False Positive Rate (1 - Specificity)",
      y        = "True Positive Rate (Sensitivity)"
    ) + 
    apatheme
  # resample_roc_plot
  # *************
  # ---- VIP ----
  # *************
  
  # Plot variable importance if avaliable for model
  tryCatch( 
    {
      vip_plot <-
        mod_last_fit %>% 
        pluck(".workflow", 1) %>%   
        extract_fit_parsnip() %>% 
        vip::vip(num_features = 65) +
        # vip::vip(num_features = 65,   metric = "mn_log_loss") +
        labs(
          title    = paste0("Variable Importance Scores - ", model_name),
          subtitle = "Win classification",
          y        = "Importance",
          x        = "Variables"
        ) + 
        apatheme
      
      
      # Save VIP plot
      ggsave(
        paste0(save_path, "plots/vip_", model_name, ".png"),
        plot   = vip_plot
      )
      
      vip_df <-
        mod_last_fit %>% 
        pluck(".workflow", 1) %>%   
        extract_fit_parsnip() %>% 
        vip::vi() %>% 
        dplyr::mutate(
          model  = model_name
          )
      
      vip_lst[[i]] <- vip_df
      
    },
    error = function(e) {
      logger::log_error('Variable Importance is not avalaible for {model_name} model')
      logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
      logger::log_error(message(e))
      # stop()
    }
  )
  
  # training set predictions
  mod_train <- 
    predict(mod_final_fit, nfl_train) %>% 
    bind_cols(predict(mod_final_fit, nfl_train, type = "prob")) %>%
    bind_cols(dplyr::select(nfl_train, win)) # Add the true outcome data back in

  # testing set predictions
  mod_test <-
    predict(mod_final_fit, nfl_test) %>% 
    bind_cols(predict(mod_final_fit, nfl_test, type = "prob")) %>%
    bind_cols(dplyr::select(nfl_test, win)) 
  
  # Train metrics
  train_metrics <- 
    mod_train %>% 
    yardstick::metrics(truth = win, estimate = .pred_class, .pred_1) %>% 
    dplyr::mutate(
      data   = "train",
      model  = model_name
    )
  
  # Test metrics
  test_metrics <- 
     mod_test %>% 
     yardstick::metrics(truth = win, estimate = .pred_class, .pred_1) %>% 
     dplyr::mutate(
      data   = "test",
      model  = model_name
      )
  

  # print(train_metrics)
  # print(test_metrics)
  
  # Model train/test metrics
  mod_metrics <- dplyr::bind_rows(train_metrics, test_metrics)
  
  print(mod_metrics)
  
  # logger::log_info("sensitivity")
  # 
  # mod_train %>% 
  #   yardstick::sens(truth = win, estimate = .pred_class)
  # mod_test %>% 
  #   yardstick::sens(truth = win, estimate = .pred_class)
  # 
  # logger::log_info("Specifficty")
  
  # mod_train %>% 
  #   yardstick::spec(truth = win, estimate = .pred_class)
  # mod_test %>% 
    # yardstick::spec(truth = win, estimate = .pred_class)
  
  
  # Keep metrics
  metrics_lst[[i]] <- mod_metrics
  
  # *********************
  # ---- Conf Matrix ----
  # *********************
  
  # confusion matrix
  conf_mat <-
      mod_test %>%
      conf_mat(truth = win, .pred_class) %>% 
      .$table %>% 
      as.data.frame() %>%
      dplyr::mutate(
        goodbad = ifelse(Prediction == Truth, "good", "bad"),
        prop    = Freq/sum(Freq),
        Prediction = dplyr::case_when(
          Prediction == 1 ~ "Model Predicts win",
          Prediction == 0 ~ "Model Predicts loss"
        ),
        Truth = dplyr::case_when(
          Truth == 1 ~ "Actual win",
          Truth == 0 ~ "Actual loss"
        )
      ) 
  
  # Plot confusion matrix
  conf_mat_plot <-
      conf_mat %>% 
      ggplot(aes(x = Prediction,
                 y = Truth, 
                 fill = goodbad)) +
      geom_tile(aes(alpha = prop)) +
      scale_fill_manual(values = c(good = "#71CD5F", bad = "red")) + 
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      # scale_fill_gradient( low = "pink", high  = "dodgerblue1", trans = "log" ) +
      labs(
        title    = paste0("Confusion matrix - ", model_name),
        # subtitle = "Number of correct/incorrect predictions",
        x        = "Prediction",
        y        = "Truth"
      ) +
      apatheme +
      theme(
        legend.position = "none",
        axis.title =  element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)
      )
  
  conf_mat_df <-
    conf_mat %>% 
    dplyr::tibble() %>%
    dplyr::mutate(
      model  = model_name
      )
  
  conf_mat_lst[[i]] <- conf_mat_df
  
  # AUC-ROC One vs All Curve
  roc_auc_curve_plot <-
    mod_test %>% 
    yardstick::roc_curve(
      truth = win,
      c(names(mod_test)[2])
    )  %>% 
    ggplot2::autoplot() +
    labs(
      title    = paste0("ROC Curve - ", model_name),
      subtitle = "Final fitted model with tuned hyperparameters",
      x        = "False Positive Rate (1 - Specificity)",
      y        = "True Positive Rate (Sensitivity)"
    ) + 
    apatheme
  
  logger::log_info("\n\nSaving model and plots...\n{model_name} - {model_mode}")
    
  roc_curve_df <- 
    mod_test %>% 
    yardstick::roc_curve(
      truth = win,
      c(names(mod_test)[2])
      ) %>% 
    dplyr::mutate(model = model_name)
  
  roc_lst[[i]] <- roc_curve_df
  
  # save resample ROC AUC plot
  ggsave(
    paste0(save_path, "plots/resample_roc_curve_", model_name, ".png"),
    plot   = resample_roc_plot
  )
  
  # Export plot
  ggsave(
    paste0(save_path, "plots/roc_curve_", model_name, ".png"),
    plot   = roc_auc_curve_plot
  )
  
  # Export plot
  ggsave(
    paste0(save_path, "plots/conf_matrix_", model_name, ".png"),
    plot   = conf_mat_plot
  )
  
  # Save Workflows/Resample results/Final fitted model
  saveRDS(
    mod_workflow_fit, 
    paste0(save_path, "wf/win_workflow_", model_name, ".rds")
  )
  
  saveRDS(
    mod_last_fit,    
    paste0(save_path, "resamples/win_resamples_", model_name, ".rds")
  )
  
  saveRDS(
    mod_final_fit, 
    paste0(save_path, "fit/win_model_", model_name, ".rds")
  )
  
  
  
}

 # Variable importance values
vip_df       <- dplyr::bind_rows(vip_lst)

# Metric set per model
metrics_df   <- dplyr::bind_rows(metrics_lst)

# Confusion matrix table per model
conf_mat_df  <- dplyr::bind_rows(conf_mat_lst)

# Confusion matrix table per model
roc_df  <- dplyr::bind_rows(roc_lst)

# save 
saveRDS(vip_df,  paste0(save_path, "metrics/win_variable_importance.rds"))
saveRDS(metrics_df, paste0(save_path, "metrics/win_metrics.rds"))
saveRDS(conf_mat_df,  paste0(save_path, "metrics/win_conf_matrix.rds"))
saveRDS(roc_df,  paste0(save_path, "metrics/win_roc.rds"))

# ***************************************************************************************
# ***************************************************************************************

metrics_df %>% 
  dplyr::filter(.metric != "kap") %>% 
  # tidyr::pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  ggplot() +
  geom_point(aes(x = model, y = .estimate, col = .metric)) +
  theme(
    axis.text.x = element_text(angle = -45)
  )+
  facet_grid(data~.metric)

# ********************
# ---- Model Eval ----
# ********************
library(flextable)
library(DT)

# Logisitc regression model
log_reg_model <- readRDS("D:/nfl/classification/fit/win_model_logistic_reg.rds")

# XGBoost model
xgb_model <- readRDS("D:/nfl/classification/fit/win_model_boost_tree.rds")

# SVM RBF model
svm_model <- readRDS("D:/nfl/classification/fit/win_model_svm_poly.rds")

# ---- Holdout year ----
holdout_year <-
  model_dat %>% 
  dplyr::filter(season == 2021, home == 1) %>% 
  dplyr::select(-abs_spread_line, -home, -home_fav, -spread_line) %>% 
  dplyr::select(season, week, game_id, team, opponent, fav, win, 
                div_game, rest_days, opp_rest_days, elo, opp_elo,
                score_diff, opp_score_diff, turnovers, opp_turnovers,
                # score_diff_qtr_1, opp_score_diff_qtr_1,
                win_pct, away_win_pct,
                home_win_pct,  opp_win_pct, opp_away_win_pct,
                opp_home_win_pct, qb_epa, opp_qb_epa,
                third_down_pct, opp_third_down_pct, 
                score_drives, opp_score_drives) 

pred_holdout_year <- 
  mod_final_fit %>% 
  augment(holdout_year) %>% 
  # bind_cols(predict(xgb_model, holdout_df, type = "prob")) %>%
  dplyr::select(season, week, game_id, team, opponent, win, fav, .pred_class, .pred_1, .pred_0)

correct_year <- 
  pred_holdout_year %>% 
  dplyr::mutate(
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    ),
    # vegas_colors = dplyr::case_when(
    #   actual_outcome == vegas_pred ~ "#85E088",
    #   actual_outcome != vegas_pred ~ "red"
    # ),
    # model_colors = dplyr::case_when(
    #   actual_outcome == model_pred ~ "#85E088",
    #   actual_outcome != model_pred ~ "red"
    #   ),
    pred_prob = round(.pred_1, 2)
  ) %>% 
  dplyr::arrange(week) %>% 
  # dplyr::select(week, home_team = team, away_team = opponent, actual_outcome, vegas_pred, model_pred, pred_prob, vegas_colors, model_colors)
# %>%
  dplyr::select(week, home_team = team, away_team = opponent, actual_outcome, vegas_pred, model_pred, pred_prob)
# datatable(correct_year,
#           options = list(columnDefs = list(list(
#             visible = FALSE, targets = 9
#           )))) %>% 
#   formatStyle(
#             c("vegas_pred", "model_pred"),
#             valueColumns = c("vegas_colors","model_colors"),
#             backgroundColor = JS("value")
          # )
colormatrix  <- ifelse(correct_year[, c(5, 6)] == correct_year[, c(4, 4)], "#85E088", "indianred2")

correct_year %>% 
  flextable() %>% 
  bg(j = 5:6, bg=colormatrix)
# saveRDS(correct_year, here::here("data", "holdout_year_predictions.rds"))

# Set custom colors
green <- "#71CA97"
red <- "#ff7f7f"

# Make example table
col1 <- c(1.2, 4.2, 5.6, 7.1)
col2 <- c(5.0, 1.3, 10.3, 6.0)
col3 <- c(4.7, 6.3, 1.5, 6.3)
mydata <- data.frame(col1, col2, col3)
library(formattable)
correct_year
# Define color_tile_mean function
color_tile_mean <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = ifelse(x < mean(x) , red, green)) # Remember to change the colors!
  })}

# Use it just like color_tile but without colors
formattable(mydata, align=c("c", "c", "c"),list(
  col1=color_tile_mean(),
  col2=color_tile_mean(),
  col3=color_tile_mean()
)
)
# DT::datatable(correct)
# rm(fav_dogs)

vegas_picks <-
  pred_holdout %>% 
  dplyr::select(-season) %>% 
  dplyr::mutate(
    favorite = dplyr::case_when(
      fav == 1 ~ team,
      fav == 0 ~ opponent
    ),
    underdog = dplyr::case_when(
      fav == 1 ~ opponent,
      fav == 0 ~ team
    ),
    fav_dog = dplyr::case_when(
      fav == 1 ~ "favorite",
      fav == 0 ~ "underdog"
    )
  ) %>% 
  dplyr::mutate(    
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    )
  ) %>% 
  dplyr::select(week, favorite, underdog, actual_outcome, vegas_pred, model_pred, predict_prob = .pred_1) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) 

# Holdout year
holdout_df <-
  model_dat %>% 
  dplyr::filter(season != 2021, home == 1) %>% 
  # dplyr::filter(season == 2020, home == 1) %>% 
  # dplyr::select(-spread_line, -home)
  # dplyr::filter(season != 2019, home == 1) %>%
  dplyr::select(-abs_spread_line, -home, -home_fav, -spread_line) %>% 
  dplyr::select(season, week, game_id, team, opponent, fav, win, 
                div_game, rest_days, opp_rest_days, elo, opp_elo,
                score_diff, opp_score_diff, turnovers, opp_turnovers,
                # score_diff_qtr_1, opp_score_diff_qtr_1,
                win_pct, away_win_pct,
                home_win_pct,  opp_win_pct, opp_away_win_pct,
                opp_home_win_pct, qb_epa, opp_qb_epa,
                third_down_pct, opp_third_down_pct, 
                score_drives, opp_score_drives) %>% 
  dplyr::filter(game_id %in% nfl_test$game_id)

pred_holdout <- 
  mod_final_fit %>% 
  augment(holdout_df) %>% 
  # bind_cols(predict(xgb_model, holdout_df, type = "prob")) %>%
  dplyr::select(season, week, game_id, team, opponent, win, fav, .pred_class, .pred_1, .pred_0)

correct <- 
  pred_holdout %>% 
  dplyr::mutate(
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    )
    # vegas_correct = dplyr::case_when(
    #   fav == 1 & win == 1 ~ "TP",
    #   fav == 0 & win == 0 ~ "TN",
    #   fav == 1 & win == 0 ~ "FP",
    #   fav == 0 & win == 1 ~ "FN"
    # ), 
    # mod_correct = dplyr::case_when(
    #   .pred_class == 1 & win == 1 ~ "TP",
    #   .pred_class == 0 & win == 0 ~ "TN",
    #   .pred_class == 1 & win == 0 ~ "FP",
    #   .pred_class == 0 & win == 1 ~ "FN"
    # )
  ) %>% 
  dplyr::arrange(week) %>% 
  dplyr::select(week, home_team = team, away_team = opponent, actual_outcome, vegas_pred, model_pred)

# DT::datatable(correct)
# rm(fav_dogs)

vegas_picks <-
  pred_holdout %>% 
  dplyr::select(-season) %>% 
    dplyr::mutate(
      favorite = dplyr::case_when(
        fav == 1 ~ team,
        fav == 0 ~ opponent
      ),
      underdog = dplyr::case_when(
        fav == 1 ~ opponent,
        fav == 0 ~ team
      ),
      fav_dog = dplyr::case_when(
        fav == 1 ~ "favorite",
        fav == 0 ~ "underdog"
      )
    ) %>% 
  dplyr::mutate(    
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    )
  ) %>% 
  dplyr::select(week, favorite, underdog, actual_outcome, vegas_pred, model_pred, predict_prob = .pred_1) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) 
  
    # tidyr::pivot_wider(names_from = fav_dog, values_from = fav_team)
  # tidyr::pivot_longer(cols = c(team, opponent)) %>% 

vegas_picks
# vegas_picks_wide <- 
  # vegas_picks %>% 
  #   dplyr::group_by(game_id)
  # tidyr::pivot_wider(id_cols = c(-name), names_from = value, values_from = fav_dog)
pred_season_outcomes <-
  pred_holdout %>% 
  dplyr::select(-game_id) %>% 
  dplyr::mutate(
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    )
  ) %>% 
  dplyr::arrange(week) %>% 
  dplyr::mutate(
    vegas_pct = dplyr::case_when(
      actual_outcome == vegas_pred ~ 1,
      actual_outcome != vegas_pred ~ 0
    ),
    model_pct = dplyr::case_when(
      actual_outcome == model_pred ~ 1,
      actual_outcome != model_pred ~ 0
    )
  ) %>% 
  # dplyr::group_by(week) %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarize(
    games = n(),
    .pred_1 = mean(.pred_1, na.rm = T),
    .pred_0 = mean(.pred_0, na.rm = T),
    vegas_sum   = sum(vegas_pct, na.rm = T),
    model_sum   = sum(model_pct, na.rm = T)
  ) %>% 
  dplyr::mutate(
    vegas_pct = vegas_sum/games,
    model_pct = model_sum/games
  ) %>% 
  round(2)
# linetype="dotted"
unique(pred_outcomes$week)


pct_season_correct_plot <- 
  pred_outcomes %>%
  # dplyr::filter(week <= 22) %>% 
  tidyr::pivot_longer(cols = c(vegas_pct, model_pct)) %>%
  # tidyr::pivot_longer(cols = c(model_pct)) %>%
  dplyr::mutate(
    name = dplyr::case_when(
      name == "model_pct" ~ "Correct Model Predictions",
      name == "vegas_pct" ~ "Correct Vegas Predictions",
    ),
    name = factor(name, levels = c("Correct Vegas Predictions", "Correct Model Predictions"))
  ) %>% 
  ggplot() +
  geom_hline(yintercept = 0.5, size = 1, alpha = 0.7, linetype = "dashed") +
  geom_line(aes(x = season, y = value, col = name,
                alpha = name, size = name)) +
  scale_y_continuous(limits = c(0, 1),
                     labels = function(x) paste0(x*100, "%")) +
  scale_x_continuous(breaks = seq(1999, 2020, 2), limits = c(1999, 2020)) +
  # scale_color_manual(values = c("red3", "black")) +
  scale_color_manual(values = c("midnightblue", "red3")) +
  scale_size_manual(values = c(1, 2.5)) +
  scale_alpha_manual(values=c(0.8, 0.5),guide=F) +
  labs(
    title = "Picking Game winners",
    subtitle = "Model predictions vs. Vegas favorites (season averages)",
    x = "Season",
    y = "% correct picks"
  ) + 
  apatheme +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = -45)
  )
pct_season_correct_plot

# Save plot
ggsave(
  "D:/nfl/classification/plots/pct_season_correct_picks.png",
  pct_season_correct_plot
)

# ---- % correct week ----
pred_outcomes <-
  # pred_holdout %>% 
  pred_holdout_year %>% 
  dplyr::select(-season, -game_id) %>%
  # dplyr::select(-game_id) %>% 
  dplyr::mutate(
    # vegas_outcome = dplyr::case_when(
    #   fav == 1 & win == 1 ~ "TP",
    #   fav == 0 & win == 0 ~ "TN",
    #   fav == 1 & win == 0 ~ "FP",
    #   fav == 0 & win == 1 ~ "FN"
    # ),
    # mod_outcome = dplyr::case_when(
    #   .pred_class == 1 & win == 1 ~ "TP",
    #   .pred_class == 0 & win == 0 ~ "TN",
    #   .pred_class == 1 & win == 0 ~ "FP",
    #   .pred_class == 0 & win == 1 ~ "FN"
    # )
    actual_outcome = dplyr::case_when(
      win == 1 ~ "win",
      win == 0 ~ "loss"
    ),
    vegas_pred = dplyr::case_when(
      fav == 1 ~ "win",
      fav == 0 ~ "loss"
    ),
    model_pred = dplyr::case_when(
      .pred_class == 1 ~ "win",
      .pred_class == 0 ~ "loss"
    )
  ) %>% 
  dplyr::arrange(week) %>% 
    dplyr::mutate(
      vegas_pct = dplyr::case_when(
        actual_outcome == vegas_pred ~ 1,
        actual_outcome != vegas_pred ~ 0
      ),
      model_pct = dplyr::case_when(
        actual_outcome == model_pred ~ 1,
        actual_outcome != model_pred ~ 0
      )
    ) %>% 
    dplyr::group_by(week) %>%
    dplyr::summarize(
      games       = n(),
      .pred_1     = mean(.pred_1, na.rm = T),
      .pred_0     = mean(.pred_0, na.rm = T),
      vegas_sum   = sum(vegas_pct, na.rm = T),
      model_sum   = sum(model_pct, na.rm = T)
    ) %>% 
  dplyr::mutate(
    vegas_pct = vegas_sum/games,
    model_pct = model_sum/games
  ) %>% 
  round(2)
# linetype="dotted"
unique(pred_outcomes$week)

  
pct_correct_plot <- 
    pred_outcomes %>%
    # dplyr::filter(week <= 22) %>% 
    tidyr::pivot_longer(cols = c(vegas_pct, model_pct)) %>%
    # tidyr::pivot_longer(cols = c(model_pct)) %>%
    dplyr::mutate(
      name = dplyr::case_when(
        name == "model_pct" ~ "Correct Model Predictions",
        name == "vegas_pct" ~ "Correct Vegas Predictions",
      ),
      name = factor(name, levels = c("Correct Vegas Predictions", "Correct Model Predictions"))
    ) %>% 
    ggplot() +
    geom_hline(yintercept = 0.5, size = 1, alpha = 0.7, linetype = "dashed") +
    geom_line(aes(x = week, y = value, col = name,
                  alpha = name, size = name)) +
    scale_y_continuous(limits = c(0, 1),
    labels = function(x) paste0(x*100, "%")) +
    scale_x_continuous(breaks = seq(1, 22, 1), limits = c(1, 22)) +
    # scale_color_manual(values = c("red3", "black")) +
    scale_color_manual(values = c("midnightblue", "red3")) +
    scale_size_manual(values = c(1, 2.5)) +
    scale_alpha_manual(values=c(0.8, 0.5),guide=F) +
    labs(
      title = "Picking Game winners",
      subtitle = "Model predictions vs. Vegas favorites - (weekly averages)",
      x = "Week",
      y = "% correct picks"
    ) + 
    apatheme +
    theme(
      panel.grid.minor.x = element_blank() ,
    )
  pct_correct_plot
  
  # Save plot
  ggsave(
    "D:/nfl/classification/plots/pct_correct_picks.png",
    pct_correct_plot
  )
  
pct_correct_box_plot <- 
  pred_outcomes %>%
  dplyr::filter(week <= 18) %>%
  arrange(week) %>% 
  dplyr::mutate(week = as.character(week)) %>% 
  tidyr::pivot_longer(cols = c(vegas_pct, model_pct)) %>% 
  dplyr::mutate(
    name = dplyr::case_when(
      name == "model_pct" ~ "Correct Model Predictions",
      name == "vegas_pct" ~ "Correct Vegas Predictions",
    ),
    name = factor(name, levels = c("Correct Vegas Predictions", "Correct Model Predictions"))
  ) %>% 
  ggplot() +
  geom_hline(yintercept = 0.5, size = 1, alpha = 0.7, linetype = "dashed") +
  geom_boxplot(aes(x = name, y = value, fill = name, alpha = name)) +
  # geom_line(aes(x = week, y = value, col = name), size = 1.5) +
  scale_y_continuous(limits = c(0, 1),
                     labels = function(x) paste0(x*100, "%")) +
  # scale_fill_manual(values = c("red3", "midnightblue")) +
  scale_fill_manual(values = c("midnightblue", "red3")) +
  # scale_size_manual(values = c(1, 2.5)) +
  scale_alpha_manual(values=c(0.5, 0.5),guide=F) +
  labs(
    title = "Picking Game winners - (2021)",
    subtitle = "Model predictions vs. Vegas favorites",
    x = "",
    y = "% correct picks"
  ) + 
  apatheme
pct_correct_box_plot

# Save plot
ggsave(
    "D:/nfl/classification/plots/pct_correct_picks_boxplot.png",
    pct_correct_box_plot
  )

  # dplyr::group_by(week) %>% 
    # count(vegas_outcome)
    # count(actual_outcome, vegas_pred, model_pred)
  # count(vegas_outcome, mod_outcome)
  # dplyr::summarise(
  #  n = n(),
  #  
  # )

pred_outcomes %>%
    dplyr::filter(week <= 17) %>% 
    arrange(week)

vegas_pred_pct <- 
  model_dat %>% 
  dplyr::filter(season == 2021, home == 1) %>% 
  dplyr::select(season, week, game_id, team, opponent, win, fav) %>% 
  conf_mat(truth = win, fav)  %>%
  .$table %>% 
  as.data.frame() %>%
  dplyr::mutate(
    goodbad = ifelse(Prediction == Truth, "good", "bad"),
    prop    = Freq/sum(Freq),
    Prediction = dplyr::case_when(
      Prediction == 1 ~ "Model Predicts win",
      Prediction == 0 ~ "Model Predicts loss"
    ),
    Truth = dplyr::case_when(
      Truth == 1 ~ "Actual win",
      Truth == 0 ~ "Actual loss"
    )
  ) %>% 
  dplyr::mutate(
    games = sum(Freq)
  ) %>% 
  dplyr::group_by(goodbad) %>% 
  dplyr::summarise(pct_correct = sum(Freq)/games) %>% 
  dplyr::ungroup()

mod_pred_pct <- 
  pred_holdout %>%
  conf_mat(truth = win, .pred_class)  %>%
  # conf_mat(truth = win, fav)  %>% 
  .$table %>% 
  as.data.frame() %>%
  dplyr::mutate(
    goodbad = ifelse(Prediction == Truth, "good", "bad"),
    prop    = Freq/sum(Freq),
    Prediction = dplyr::case_when(
      Prediction == 1 ~ "Model Predicts win",
      Prediction == 0 ~ "Model Predicts loss"
    ),
    Truth = dplyr::case_when(
      Truth == 1 ~ "Actual win",
      Truth == 0 ~ "Actual loss"
    )
  ) %>% 
  dplyr::mutate(
    games = sum(Freq)
  ) %>% 
  dplyr::group_by(goodbad) %>% 
  dplyr::summarise(pct_correct = sum(Freq)/games) %>% 
  dplyr::ungroup()
vegas_pred_pct
mod_pred_pct

vegas_conf_mat <- 
  pred_holdout %>%
  # conf_mat(truth = win, .pred_class) %>%
  conf_mat(truth = win, fav) %>%
  # autoplot(type = "heatmap")
  .$table %>% 
  as.data.frame() %>%
  dplyr::mutate(
    goodbad = ifelse(Prediction == Truth, "good", "bad"),
    prop    = Freq/sum(Freq),
    Prediction = dplyr::case_when(
      Prediction == 1 ~ "Model Predicts win",
      Prediction == 0 ~ "Model Predicts loss"
    ),
    Truth = dplyr::case_when(
      Truth == 1 ~ "Actual win",
      Truth == 0 ~ "Actual loss"
    )
  ) 
vegas_conf_mat %>% 
  dplyr::mutate(
    games = sum(Freq)
  ) %>% 
  dplyr::group_by(goodbad) %>% 
  dplyr::summarise(pct_correct = sum(Freq)/games) %>% 
  dplyr::ungroup()

# Plot confusion matrix
vegas_conf_mat_plot <-
  vegas_conf_mat %>% 
  ggplot(aes(x = Prediction,
             y = Truth, 
             fill = goodbad)) +
  geom_tile(aes(alpha = prop)) +
  scale_fill_manual(values = c(good = "#71CD5F", bad = "red")) + 
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  # scale_fill_gradient( low = "pink", high  = "dodgerblue1", trans = "log" ) +
  labs(
    title    = paste0("Confusion matrix - ", model_name),
    subtitle = "Vegas predictions",
    x        = "Prediction",
    y        = "Truth"
  ) +
  apatheme +
  theme(
    legend.position = "none",
    axis.title =  element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

mod_conf_mat <- 
  pred_holdout %>%
  conf_mat(truth = win, .pred_class) %>%
  # autoplot(type = "heatmap")
  .$table %>% 
  as.data.frame() %>%
  dplyr::mutate(
    goodbad = ifelse(Prediction == Truth, "good", "bad"),
    prop    = Freq/sum(Freq),
    Prediction = dplyr::case_when(
      Prediction == 1 ~ "Model Predicts win",
      Prediction == 0 ~ "Model Predicts loss"
    ),
    Truth = dplyr::case_when(
      Truth == 1 ~ "Actual win",
      Truth == 0 ~ "Actual loss"
    )
  ) 

mod_conf_mat %>% 
  dplyr::mutate(
    games = sum(Freq)
  ) %>% 
  dplyr::group_by(goodbad) %>% 
  dplyr::summarise(pct_correct = sum(Freq)/games) %>% 
  dplyr::ungroup()

# Plot confusion matrix
mod_conf_mat_plot <-
  mod_conf_mat %>% 
  ggplot(aes(x = Prediction,
             y = Truth, 
             fill = goodbad)) +
  geom_tile(aes(alpha = prop)) +
  scale_fill_manual(values = c(good = "#71CD5F", bad = "red")) + 
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  # scale_fill_gradient( low = "pink", high  = "dodgerblue1", trans = "log" ) +
  labs(
    title    = paste0("Confusion matrix - ", model_name),
    subtitle = "Model predictions",
    x        = "Prediction",
    y        = "Truth"
  ) +
  apatheme +
  theme(
    legend.position = "none",
    axis.title =  element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

vegas_conf_mat_plot
mod_conf_mat_plot
  
tmp <- 
  correct %>% 
  dplyr::mutate(
    diff = dplyr::case_when(
      vegas_pred != model_pred ~ 1,
      TRUE ~ 0
    ),
    diff_correct = dplyr::case_when(
      model_pred == actual_outcome & vegas_pred != actual_outcome ~ 1,
      model_pred != actual_outcome & vegas_pred == actual_outcome ~ 0
    )
  ) 
tmp %>% 
  dplyr::filter(diff_correct == 1)

tmp %>% 
  dplyr::filter(diff_correct == 0)

tmp %>% 
  dplyr::filter(diff == 1)

# ***************************************************************************************
# ***************************************************************************************






game_spreads <- readRDS(here::here("data", "football_closing_spread_lag.rds")) %>% 
  dplyr::select(game_id, team, opponent, home, spread_line)

holdout_df <- readRDS(here::here("data", "football_wins_lag.rds")) %>% 
  dplyr::select(-contains("qtr_pts_6")) %>% 
  dplyr::left_join(
    dplyr::select(game_spreads, -home),
    by = c("game_id", "team", "opponent")
  ) %>%
  dplyr::filter(season == 2021) %>% 
  dplyr::select(-home, -week, -score_drives, -ndrives, -def_ndrives, -def_score_drives,
                -opp_score_drives, -opp_ndrives, -opp_def_score_drives, -opp_def_ndrives,
                -drive_time_of_possession_sec,  -opp_drive_time_of_possession_sec, 
                # -pts_scored,
                -score_diff_qtr_1, -score_diff_qtr_2, -score_diff_qtr_3,
                -opp_score_diff_qtr_1, -opp_score_diff_qtr_2, -opp_score_diff_qtr_3,
                -def_score_drives_pct, -opp_def_score_drives_pct, -def_third_down_pct,
                -opp_def_third_down_pct, -def_qb_epa, -opp_def_qb_epa,
                -qtr_pts_1, -qtr_pts_2, -qtr_pts_3,
                -opp_qtr_pts_1, -opp_qtr_pts_2, -opp_qtr_pts_3,
                -def_turnovers, -opp_def_turnovers, -score_diff, -opp_score_diff
  ) %>%
  dplyr::select(-contains("dscore_diff"), -contains("def_qtr_pts")) %>%
  dplyr::mutate(
    div_game  = factor(div_game, levels = c(1, 0)),
    rest_days = dplyr::case_when(
      rest_days < 7   ~  "short_rest",
      rest_days == 7  ~  "normal_rest",
      rest_days > 7   ~  "long_rest"
    ),
    opp_rest_days = dplyr::case_when(
      opp_rest_days < 7   ~  "short_rest",
      opp_rest_days == 7  ~  "normal_rest",
      opp_rest_days > 7   ~  "long_rest"
    ),
    win = factor(win, levels = c(1, 0))
  ) %>% 
  dplyr::mutate(
    across((contains("dscore")), ~replace(., is.na(.), 0))
  ) %>% 
  na.omit() %>%
  dplyr::mutate(across(where(is.numeric), round, 4))
  
 vegas_wins <- readRDS(here::here("data", "football_wins_lag.rds")) 

vegas_wins <- 
  vegas_wins %>% 
  dplyr::select(-contains("qtr_pts_6")) %>% 
  dplyr::left_join(
    dplyr::select(game_spreads, -home),
    by = c("game_id", "team", "opponent")
  ) %>%
  dplyr::select(season, week, game_id, home, win, spread = spread_line) %>% 
  dplyr::filter(home == 1) %>% 
  dplyr::mutate(
  vegas_outcome = dplyr::case_when(
    win == 1 & spread < 0 ~ 1,
    win == 0 &spread >= 0 ~ 1,
    win == 1 & spread >= 0 ~ 0,
    win == 0 & spread < 0 ~ 0
    # spread < 0 ~ 1,
    # spread >= 0 ~ 0
    # home == 1 & win == 1 & spread <= 0 ~ 1,
    # home == 1 & win == 0 & spread <= 0 ~ 0,
    # home == 1 & win == 1 & spread > 0 ~ 0,
    # home == 1 & win == 0 & spread > 0 ~ 1,
    # home == 0 & win == 1 & spread <= 0 ~ 0,
    # home == 0 & win == 0 & spread <= 0 ~ 1
    # home == 0 & win == 1 & spread > 0 ~ 0,
    # home == 0 & win == 0 & spread > 0 ~ 1
  ))
  # actual_outcome = dplyr::case_when(
  #   win == 1 & vegas_outcome == 1 ~ 1,
  #   win == 0 & vegas_outcome == 0 ~ 1,
  #   win == 1 & vegas_outcome == 0 ~ 0, 
  #   win == 0 & vegas_outcome == 1 ~ 0
  #    )
  # ) 

vegas_wins %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(
    n = n(),
    vegas_outcome = sum(vegas_outcome, na.rm = T)
  )







