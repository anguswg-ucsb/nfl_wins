
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

# *******************
# ---- Load Data ----
# *******************

# Load Lag NFL win data
nfl_df <- readRDS(here::here("data", "football_wins_lag.rds")) %>% 
  dplyr::select(-contains("qtr_pts_6")) %>% 
  dplyr::mutate(
    div_game  = factor(div_game, levels = c(1, 0)),
    rest_days = dplyr::case_when(
      rest_days < 7   ~  "short_rest",
      rest_days == 7  ~  "normal_rest",
      rest_days > 7   ~  "long_rest"
      ),
    win = factor(win, levels = c(1, 0))
    ) %>% 
  dplyr::select(-game_id, -team, -opponent, -week)

holdout <-
  nfl_df %>% 
  dplyr::filter(season == 2021)

    # home2 = case_when( home == 1 ~ "home", home == 0 ~ "away"),
    # home2 = factor(home2, levels = c("home", "away")) ) 

# count(nfl_df, rest_days)
# tibble::glimpse(nfl_df)

# Remove NAs from data
nfl_df <- 
  nfl_df %>% 
  na.omit() %>% 
  dplyr::filter(season != 2021) %>% 
  dplyr::select(-season)
# all_na <- nfl_df[rowSums(is.na(nfl_df)) > 0,]

# hist(nfl_df$win)
# count(nfl_df, win)
# count(nfl_df, div_game)

# Set random seed
set.seed(234)

# split data for train/test, stratify by quantiles of fantasy points 
# nfl_split <- initial_split(nfl_df, strata = season)
nfl_split <- initial_split(nfl_df, strata = win)

# training data split
nfl_train <- training(nfl_split)

# testinng data split
nfl_test  <- testing(nfl_split)

# *****************
# ---- Recipes ----
# *****************

usemodels::use_kknn(win~., data = nfl_train)
usemodels::use_xgboost(win~., data = nfl_train)
usemodels::use_glmnet(win~., data = nfl_train)
usemodels::use_earth(win~., data = nfl_train)
# usemodels::use_

# Data preprocessing 
logger::log_info("Data preprocessing...")

# K nearest neighbors
kknn_recipe <- 
  recipes::recipe(
    formula = win ~ ., 
    data    = nfl_train
  ) %>% 
  step_string2factor(one_of("rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(nfl_finish) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

knn_juice <- juice(prep(kknn_recipe))
count(knn_juice, win)

# glmnet linear regression
glmnet_recipe <- 
  recipe(
    formula = win ~ .,
    data    = nfl_train
    ) %>% 
  step_string2factor(one_of("rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 


# XGBoost trees
xgboost_recipe <- 
  recipes::recipe(
    formula = win ~ ., 
    data    = nfl_train
  ) %>% 
  step_string2factor(one_of("rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  # themis::step_smote(nfl_finish) %>%
  step_zv(all_predictors()) 

earth_recipe <- 
  recipes::recipe(
    formula = win ~ .,
    data = nfl_train) %>% 
  step_string2factor(one_of("rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) 

# ********************
# ---- Model Spec ----
# ********************

kknn_spec <- 
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
#     trees = 1000
#   ) %>% 
#   set_mode("regression") %>% 
#   set_engine("ranger", importance = "permutation") 

xgboost_spec <- 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(), 
    learn_rate = tune(), 
    loss_reduction = tune(), 
    sample_size = tune()) %>% 
  set_mode("classification") %>% 
  parsnip::set_engine("xgboost", importance = "permutation") 

earth_spec <-
  mars(
    num_terms    = tune(),
    prod_degree  = tune(),
    prune_method = "none") %>%
  set_mode("classification") %>%
  set_engine("earth")

# ********************************
# ---- Cross Validation folds ----
# ********************************

# Set seed for resampling 
set.seed(589)

# CV folds
nfl_folds <- rsample::vfold_cv(nfl_train, v = 10, strata = win)

# ---- Workflow set of models ----
nfl_wfs <- 
  workflow_set(
    preproc = list(
      kknn_rec        = kknn_recipe,
      glmnet_rec      = glmnet_recipe,
      # glmnet_ups_rec  = glmnet_ups_recipe,
      # ranger_rec   = ranger_recipe,
      xgboost_rec     = xgboost_recipe,
      earth_rec       = earth_recipe
      # smote_norm_rec = smote_norm_recipe,
      # smote_oh_rec   = smote_oh_recipe,
      # upsample_norm_rec = upsample_norm_recipe, 
      # upsample_oh_rec = upsample_oh_recipe
    ),
    models  = list(
      kknn       = kknn_spec,
      glmnet     = glmnet_spec,
      # glmnet_ups = glmnet_spec,
      # ranger    = ranger_spec,
      xgboost    = xgboost_spec,
      earth      = earth_spec
    ),
    cross = F
  )

# Choose metrics
# my_metrics <- yardstick::metric_set(rsq, rmse, mae)
my_metrics <- yardstick::metric_set(roc_auc, pr_auc, accuracy, mn_log_loss)

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
    metrics   = my_metrics,
    control   = control_grid(
      verbose   = TRUE,
      save_pred = TRUE),
    verbose   = TRUE
  )

# Efficient Tuning of models in workflowset
nfl_wfs <-
  nfl_wfs %>%
  workflowsets::workflow_map(
    "tune_race_anova",
    resamples = nfl_folds,
    # resamples = flow_roll_splits,
    grid      = 20,
    metrics   = my_metrics,
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

rank_results(nfl_wfs)

# Comparing Accuracy and ROC AUC of 7 models
reg_mod_comp_plot <-
  nfl_wfs %>%
  autoplot() + 
  labs(
    col = "",
    title    = "Regression Model comparisons",
    subtitle = "Predicting wins"
  ) 

reg_mod_comp_plot

 # ****************************
# ---- Select best models ----
# ****************************

metrics_lst <- list()

# rm(acc)
i <- 3
# rm(i, model, model_name, mod_workflow, mod_final_fit, mod_last_fit, mod_workflow_fit, resample_roc_plot, vip_plot,
#    mod_test,mod_train, train_acc, test_acc, overall_aucroc,
# roc_auc_curve_plot, mod_results)

# Extract wf set results
for (i in 1:length(nfl_wfs$wflow_id)) {
  
  model       <- nfl_wfs$wflow_id[i]
  model_name  <- nfl_wfs$info[[i]]$model
  
  model
  
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
  
  logger::log_info("\n\nExtracting workflow & finalizing model fit:\n  --->  {model_name} - {model_mode}")
  # mod_results$.metrics[[1]]
  
  # print(select_best(mod_results, metric = "rmse"))
  # print(select_best(mod_results, metric = "rsq"))
  select_best(mod_results, metric = "roc_auc")
  # rm(mod_workflow_fit)
  # Finalize workflow fit
  mod_workflow_fit <- 
    mod_workflow %>% 
    finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>% 
    # finalize_workflow(select_best(mod_results, metric = "rsq")) %>% 
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
      title    = paste0("AUC-ROC Curve - ", model_name),
      subtitle = "Resample results from 10 Fold Cross Validation",
      x        = "1 - Specificity",
      y        = "Sensitivity"
    ) 
  # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
  resample_plot_path  <-   paste0(ml_data_path, "plots/win_class_resample_aucroc_", model_name, ".png")
  logger::log_info("\n\nSaving Resamples AUC-ROC curve: \n{resample_plot_path}")
  
  # Export plot
  ggsave(
    resample_plot_path,
    plot   = resample_roc_plot
  )
  
  # Plot variable importance if avaliable for model
  tryCatch( 
    {
      # vip_plot <-
      mod_last_fit %>% 
        pluck(".workflow", 1) %>%   
        extract_fit_parsnip() %>% 
        # vip::vip() + 
        vip::vip(num_features = 50) +
        labs(
          title    = paste0("Variable Importance Scores - ", model_name),
          subtitle = "Regression",
          y        = "Importance",
          x        = "Variables"
        )
      
      # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
      vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
      logger::log_info("\n\nSaving Variable Importance plot:\n{vip_path}")
      
      # Export plot
      ggsave(
        vip_path,
        plot   = vip_plot
      )
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
  
  # Train accuracy
  train_acc <- 
    mod_train %>% 
    accuracy(truth = win, .pred_class) %>% 
    mutate(
      data   = "train",
      model  = model_name
    )
  print(train_acc)
  # Test accuracy
  test_acc <- 
    mod_test %>% 
    accuracy(truth = win, .pred_class) %>% 
    mutate(
      data   = "test",
      model  = model_name
    )
  print(test_acc)
  # class(mod_results)
  conf_mat(mod_train, truth = win, .pred_class)
  # overall AUC ROC
  overall_aucroc <-
    mod_test %>% 
    yardstick::roc_auc(
      truth = win,
      c(names(mod_test)[2:3]),
      estimator = "hand_till"
    ) %>%
    mutate(
      data   = "test",
      model  = model_name
    )
  # data(hpc_cv)
  
  # Accuracy
  acc <- bind_rows(train_acc, test_acc, overall_aucroc)
  
  class_metrics_lst[[i]] <- acc
  
  # AUC-ROC One vs All Curve
  roc_auc_curve_plot <-
    mod_test %>% 
    yardstick::roc_curve(
      truth = win,
      c(names(mod_test)[2])
    )  %>% 
    ggplot2::autoplot() +
    labs(
      title    = paste0("AUC-ROC - ", model_name),
      subtitle = "Final fitted model using tuned hyperparameters",
      x        = "1 - Specificity",
      y        = "Sensitivity"
    ) 
  # th
  
  
  # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
  rocauc_plot_path  <-   paste0(ml_data_path, "plots/win_class_aucroc_", model_name, ".png")
  logger::log_info("\n\nSaving final fitted AUC ROC curve:\n{rocauc_plot_path}")
  
  # Export plot
  ggsave(
    rocauc_plot_path,
    plot   = roc_auc_curve_plot
  )
  
  # Save Workflows/Resample results/Final fitted model
  saveRDS(
    mod_workflow_fit, 
    paste0(ml_data_path, "wf/win_class_workflow_", model_name, ".rds")
  )
  
  saveRDS(
    mod_last_fit,    
    paste0(ml_data_path, "resamples/win_class_resamples_", model_name, ".rds")
  )
  
  saveRDS(
    mod_final_fit, 
    paste0(ml_data_path, "fit/win_class_", model_name, ".rds")
  )
  
  
  
}








