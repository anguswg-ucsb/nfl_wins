
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
# save_path <- "D:/nfl/classification/"

# *******************
# ---- Load Data ----
# *******************

# Load entire dataset
model_dat <- readRDS(here::here("data", "football_wins_lag_elo.rds")) 

# Modeling subset
nfl_df <-
  model_dat  %>% 
  dplyr::filter(home == 1) %>% 
  # dplyr::filter(season != 2021, home == 1) %>% 
  dplyr::select(-abs_spread_line, -home, -home_fav, -fav, -spread_line, -div_game) %>% 
  dplyr::select(season, week, game_id, team, opponent, win, rest_days, 
                opp_rest_days, elo, opp_elo, score_diff, 
                opp_score_diff, turnovers, opp_turnovers,
                win_pct, away_win_pct, home_win_pct, 
                opp_win_pct, opp_away_win_pct, opp_home_win_pct)
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
  "D:/nfl/eda/api_win_class_split.png",
  win_count_plot,
  width  = 10,
  height = 8
)

# *****************
# ---- Recipes ----
# *****************
usemodels::use_kernlab_svm_rbf(win~., data = nfl_train)
usemodels::use_kknn(win~., data = nfl_train)
# usemodels::use_xgboost(win~., data = nfl_train)
# usemodels::use_glmnet(win~., data = nfl_train)
# usemodels::use_earth(win~., data = nfl_train)


# Data preprocessing 
logger::log_info("Data preprocessing...")

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

norm_recipe %>%
  prep() %>%
  juice() %>%
  count(win)

# norm_smote_recipe <- 
#   norm_recipe %>% 
#   themis::step_up(win, over_ratio = 0.9) 

norm_smote_recipe <- 
  norm_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)


norm_smote_recipe %>%
  prep() %>%
  juice() %>%
  count(win)

norm_smote_recipe %>% 
  prep() %>%
  bake(new_data = NULL) %>%
  ggplot(aes(win)) +
  geom_bar()


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

# Neural network recipe 
nnet_recipe <- 
  base_recipe %>%
  recipes::step_zv(all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors()) 

# Neural network recipe w/ SMOTE upsampling
nnet_smote_recipe <- 
  nnet_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)

nnet_smote_recipe %>%
  prep() %>%
  juice() %>%
  count(win)

# SVM recipe
svm_recipe <- 
  base_recipe %>%
  recipes::step_zv(all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors()) 

# SVM recipe w/ SMOTE upsampling
svm_smote_recipe <- 
  svm_recipe %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)

svm_smote_recipe %>%
  prep() %>%
  juice() %>%
  count(win)

# Normalized recipe w/ SMOTE upsampling
std_smote_recipe <-  
  base_recipe %>%
  recipes::step_zv(all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric_predictors()) %>% 
  themis::step_smote(win, over_ratio = 0.9,  skip = T)

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

# ranger_spec <-
#   rand_forest(
#     mtry  = tune(),
#     min_n = tune(),
#     trees = tune()
#   ) %>%
#   set_mode("classification") %>%
#   set_engine("ranger", importance = "permutation")

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

svm_lin_spec <- 
  svm_linear(
    cost      = tune()
  ) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") 

svm_spec <- 
  svm_rbf(
    cost      = tune(), 
    rbf_sigma = tune()
  ) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") 

svm_poly_spec <- 
  svm_poly(
    cost         = tune(),
    degree       = tune(), 
    scale_factor = tune()
  ) %>% 
  set_engine("kernlab") %>% 
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
      nnet_rec        = std_smote_recipe,
      svm_lin_rec     = std_smote_recipe,
      svm_poly_rec    = std_smote_recipe,
      svm_rbf_rec     = std_smote_recipe
    ),
    models  = list(
      knn            = knn_spec,
      glmnet         = glmnet_spec,
      xgboost        = xgboost_spec,
      nnet           = nnet_spec,
      svm_lin        = svm_lin_spec,
      svm_poly       = svm_poly_spec,
      svm_rbf        = svm_spec
    ),
    cross = F
  )

# Choose metrics
met_set <- yardstick::metric_set(
  roc_auc, accuracy, mn_log_loss, 
  sensitivity, specificity, j_index
  )


# Set up parallelization, using computer's other cores
parallel::detectCores(logical = FALSE)
modeltime::parallel_start(6, .method = "parallel")

# Set Random seed
set.seed(589)

# # Tune models in workflowset
# nfl_wfs <-
#   nfl_wfs %>%
#   workflow_map(
#     "tune_grid",
#     resamples = nfl_folds ,
#     grid      = 20,
#     metrics   = met_set,
#     control   = control_grid(
#       verbose   = TRUE,
#       save_pred = TRUE),
#     verbose   = TRUE
#   )
# 
# nfl_wfs[4,]$result[[1]]$.notes[[1]]$note

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
save_path <- "D:/nfl/api_classification/"


# nfl_wfs   <- readRDS(paste0(save_path, "wfs/win_classification_wfs2.rds"))

# print ranks table
wfs_ranks <- rank_results(nfl_wfs)
wfs_ranks

# Comparing Accuracy and ROC AUC of 7 models
class_mod_comp_plot <-
  # nfl_wfs2 %>%
  nfl_wfs %>%
  # nfl_wfs[c(1:4),] %>%
  autoplot() + 
  labs(
    col = "",
    title    = "Classification Model comparisons (API)",
    subtitle = "Predicting wins"
  ) +
  apatheme

class_mod_comp_plot 

# plotly::ggplotly(class_mod_comp_plot)
save_path <- "D:/nfl/api_classification/"

# Save plot
ggsave(
  paste0(save_path, "plots/class_model_ranks.png"),
  class_mod_comp_plot,
  width  = 16,
  height = 10
)


saveRDS(nfl_wfs,
        paste0(save_path, "wfs/win_classification_wfs.rds")
        )

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

# rm(i, model, model_name, mod_workflow, mod_final_fit, mod_last_fit, mod_workflow_fit, resample_roc_plot, vip_plot,
#    mod_test,mod_train, train_acc, test_acc, overall_aucroc, test_metrics, train_metrics, mod_metrics, conf_mat, conf_mat_plot,conf_mat_lst, vip_lst, metrics_lst,
# roc_auc_curve_plot, mod_results)

# i= 2

# Extract wf set results
for (i in 1:length(nfl_wfs$wflow_id)) {
  
  logger::log_info("\n\nModel: {i} of {length(nfl_wfs$wflow_id)}")
  
  save_path <- "D:/nfl/api_classification/"
  
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
  
  
  
  # logger::log_info("sensitivity")
  # # 
  sens_train <- 
    mod_train %>%
    yardstick::sens(truth = win, estimate = .pred_class) %>% 
    dplyr::mutate(
      data   = "train",
      model  = model_name
    )
  sens_test <-
    mod_test %>%
    yardstick::sens(truth = win, estimate = .pred_class) %>% 
    dplyr::mutate(
      data   = "test",
      model  = model_name
    )
  # 
  spec_train <- 
    mod_train %>%
    yardstick::spec(truth = win, estimate = .pred_class)  %>% 
    dplyr::mutate(
      data   = "train",
      model  = model_name
    )
  
  spec_test <- 
    mod_test %>%
    yardstick::spec(truth = win, estimate = .pred_class) %>% 
    dplyr::mutate(
      data   = "test",
      model  = model_name
    )
  
  # Model train/test metrics
  mod_metrics <- dplyr::bind_rows(train_metrics, sens_train, spec_train, test_metrics, sens_test, spec_test)
  
  print(mod_metrics)
  
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