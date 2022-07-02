
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tidymodels)
library(stringr)
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

# # Team records/schedule
# team_records      <-  readRDS(here::here("data", "team_records.rds"))
# 
# # Weekly Team defense
# team_defense      <- readRDS(here::here("data", "team_defense.rds"))
# 
# # Weekly Team Fantasy points allowed by position
# fp_against        <- readRDS(here::here("data", "team_fp_against.rds"))
# 
# # Weekly player stats
# player_stats      <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Cumaltive fantasy points average per player and opponent defense
# fantasy             <- readRDS(here::here("data", "fp_model_data.rds"))
fantasy             <- readRDS(here::here("data", "fp_rollmean_data.rds"))
# names(fp_df)
# Subset data to RBs and desired columns
fp_df  <- 
  fantasy %>% 
  dplyr::filter(position == "RB", season >= 2006) %>% 
  dplyr::select(-fp_rank) %>%
  # dplyr::select(1:11, 104:398) %>% 
  dplyr::select(player_id:fp_finish, contains("roll_4")) %>%
  setNames(c(gsub("_lag1|_roll_4", "", names(.)))) %>%
  dplyr::mutate(
    fp_finish = case_when(
      fp_finish == "top_5"     ~ 1,
      fp_finish == "not_top_5" ~ 0 
      ),
    home = case_when(
      home_away == "home_team"  ~ 1,
      home_away == "away_team"  ~ 0 
    )
  ) %>% 
  dplyr::mutate(
    fp_finish = factor(fp_finish, levels = c(1, 0)),
    home      = factor(home, levels = c(1, 0))
    ) %>% 
  dplyr::select(-home_away) %>% 
  # dplyr::mutate_if(is.character, factor) %>% 
  # dplyr::group_by(season, player_id) %>%
  # dplyr::mutate(across(where(is.numeric), ~mean_na(.x))) %>%
#   dplyr::select(-contains("qb"), -contains("passing"), -contains("attempts"), -contains("completions"), 
#                 -contains("sacks"), -contains("interceptions"), -contains("pass"),
#                 -contains("sack"), -contains("pass"), -contains("dakota"), -contains("comp_pct")
#                 ) %>% 
  dplyr::select(
    fp_hppr,
    # fp_finish, 
    home,
    team,
    # player_id,
    # home_away,
    win_pct_rmean,# names(.)[12:53], names(.)[56:97]
    fp_rank, touches:opp_qb_total_yards,fp_hppr_wr
    )
                # touches:catch_rate, receiving_yards:receiving_fumbles_lost,
                # receiving_yards_after_catch:receiving_ypt,
    # opp_passing_yards:opp_rushing_yards,
                # opp_passing_tds:opp_rushing_epa, opp_comp_pct:opp_ypt, fp_hppr_rb)

  # replace inf with NA
  fp_df <- do.call(data.frame, lapply(fp_df, function(value) {
    replace(value, is.infinite(value),NA)
  })
    )
  # convert NaN to 0
  # fp_df[is_nan_data_frame(fp_df)] <- 0
  # tmp <- fp_df[rowSums(is.na(fp_df)) > 0, ]
  
  # Omit NAs, first rows of each player due to cumaltive lags not starting till 2nd record of player data per season
  fp_df <- na.omit(fp_df)
  
  # replace(is.na(.), 0) %>% 
  # na.omit()
# rb 
hist(fp_df$fp_hppr, breaks = 50)


# quantile(fp_df$fp_hppr)
set.seed(234)

# split data for train/test, stratify by quantiles of fantasy points 
fp_split <- initial_split(fp_df, strata = fp_hppr)

# training data split
fp_train <- training(fp_split)

# testinng data split
fp_test  <- testing(fp_split)
count(fp_df, fp_finish)

# model_data <- 
#   fp_df %>% 
#   tidyr::nest(data = everything()) %>% 
#   dplyr::mutate(
#     data_splits = purrr::map(data, initial_split, strata = fp_hppr),
#     train_data  = purrr::map(data_splits, training),
#     test_data   = purrr::map(data_splits, testing)
#   )

# *****************
# ---- Recipes ----
# *****************

usemodels::use_kknn(fp_hppr~., data = fp_train)
usemodels::use_xgboost(fp_hppr~., data = fp_train)
usemodels::use_glmnet(fp_hppr~., data = fp_train)
usemodels::use_earth(fp_hppr~., data = fp_train)
# Data preprocessing 
logger::log_info("Data preprocessing...")

# K nearest neighbors
kknn_recipe <- 
  recipes::recipe(
    formula = fp_hppr ~ ., 
    data    = fp_train
  ) %>% 
  step_string2factor(one_of("team")) %>% 
  # themis::step_smote(fp_finish) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(fp_finish) %>%
  # themis::step_upsample(fp_finish) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

knn_juice <- juice(prep(kknn_recipe))
count(knn_juice, fp_finish)
  # recipes::update_role(
  #   player_id, 
  #   new_role = "id"
  # ) %>% 
  # recipes::step_zv(all_predictors()) %>% 
  # recipes::step_normalize(all_numeric_predictors()) 

# glmnet linear regression
glmnet_recipe <- 
  recipes::recipe(
    formula = fp_hppr ~ ., 
    data    = fp_train
  ) %>% 
  step_string2factor(one_of("team")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  # themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

glmnet_ups_recipe <- 
  recipes::recipe(
    formula = fp_hppr ~ ., 
    data    = fp_train
  ) %>% 
  step_string2factor(one_of("team")) %>%  
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  # themis::step_upsample(fp_finish) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

# Random forest
ranger_recipe  <- 
  recipes::recipe(
    formula = fp_hppr ~ ., 
    data    = fp_train
  ) %>% 
  step_string2factor(one_of("team")) 
  # step_string2factor(one_of("team", "home_away",'fp_finish')) %>% 
  # themis::step_smote(fp_finish) 
  # recipes::update_role(
  #   player_id, 
  #   new_role = "id"
  # ) 

# XGBoost trees
xgboost_recipe <- 
  recipes::recipe(
    formula = fp_hppr ~ ., 
    data    = fp_train
  ) %>% 
  step_string2factor(one_of("team")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  # themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) 
  # recipes::update_role(
  #   player_id, 
  #   new_role = "id"
  #   ) %>% 
  # recipes::step_zv(all_predictors()) 

earth_recipe <- 
  recipes::recipe(
    formula = fp_hppr ~ .,
    data = fp_train) %>% 
  step_string2factor(one_of("team")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) 
  # recipes::step_zv(all_predictors()) 

# XGBoost trees
smote_norm_recipe <- 
  recipes::recipe(
    formula = fp_finish ~ ., 
    data    = fp_train
  ) %>% 
  # step_string2factor(one_of("team", "home_away",'fp_finish')) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

# XGBoost trees
smote_oh_recipe <- 
  recipes::recipe(
    formula = fp_finish ~ ., 
    data    = fp_train
  ) %>% 
  # step_string2factor(one_of("team", "home_away",'fp_finish')) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) 

# XGBoost trees
upsample_norm_recipe<- 
  recipes::recipe(
    formula = fp_finish ~ ., 
    data    = fp_train
  ) %>% 
  # step_string2factor(one_of("team", "home_away",'fp_finish')) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_upsample(fp_finish) %>%
  step_zv(all_predictors())  %>% 
  step_normalize(all_numeric_predictors()) 

# XGBoost trees
upsample_oh_recipe <- 
  recipes::recipe(
    formula = fp_finish ~ ., 
    data    = fp_train
  ) %>% 
  # step_string2factor(one_of("team", "home_away",'fp_finish')) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  themis::step_upsample(fp_finish) %>%
  step_zv(all_predictors()) 

juice(prep(upsample_rec))

# ********************
# ---- Model Spec ----
# ********************

kknn_spec <- 
  nearest_neighbor(
    neighbors   = tune(),
    weight_func = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("kknn") 

glmnet_spec <- 
  linear_reg(
    penalty = tune(),
    mixture = tune()
    ) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet") 

ranger_spec <- 
  rand_forest(
    mtry  = tune(),
    min_n = tune(), 
    trees = 1000
  ) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "permutation") 

xgboost_spec <- 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(), 
    learn_rate = tune(), 
    loss_reduction = tune(), 
    sample_size = tune()) %>% 
  set_mode("regression") %>% 
  parsnip::set_engine("xgboost", importance = "permutation") 

earth_spec <-
  mars(
    num_terms    = tune(),
    prod_degree  = tune(),
    prune_method = "none") %>%
  set_mode("regression") %>%
  set_engine("earth")

# ********************************
# ---- Cross Validation folds ----
# ********************************

# Set seed for resampling 
set.seed(589)

# CV folds
fp_folds <- rsample::vfold_cv(fp_train, v = 10, strata = fp_hppr)

# ---- Workflow set of models ----
fp_wfs <- 
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
my_metrics <- yardstick::metric_set(rsq, rmse, mae)
# my_metrics <- yardstick::metric_set(roc_auc, pr_auc, accuracy)

# Set up parallelization, using computer's other cores
parallel::detectCores(logical = FALSE)
modeltime::parallel_start(6, .method = "parallel")

# Set Random seed
set.seed(589)

# Tune models in workflowset
# fp_wfs <-
#   fp_wfs %>%
#   workflow_map(
#     "tune_grid",
#     resamples = fp_folds ,
#     grid      = 20,
#     metrics   = my_metrics,
#     control   = control_grid(
#       verbose   = TRUE,
#       save_pred = TRUE),
#     verbose   = TRUE
#   )

    # Efficient Tuning of models in workflowset
fp_wfs <-
  fp_wfs %>%
  workflowsets::workflow_map(
    "tune_race_anova",
    resamples = fp_folds,
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

 # **************************
# ---- View WFS results ----
# **************************
ml_data_path <- "D:/ml/nfl/data/"

# Save workflowset
# saveRDS(fp_wfs, paste0(ml_data_path, "wfs/fp_regression_wfs.rds"))
# saveRDS(fp_split, paste0(ml_data_path, "split/fp_split_data.rds"))
# saveRDS(fp_train, paste0(ml_data_path, "split/fp_train_data.rds"))
# saveRDS(fp_test, paste0(ml_data_path, "split/fp_test_data.rds"))

# fp_wfs   <- readRDS( paste0(ml_data_path, "wfs/fp_regression_wfs.rds"))
# fp_split <- readRDS( paste0(ml_data_path, "split/fp_split_data.rds"))
# fp_train <- readRDS( paste0(ml_data_path, "split/fp_train_data.rds"))
# fp_test  <- readRDS( paste0(ml_data_path, "split/fp_test_data.rds"))
# saveRDS(fp_split, paste0(ml_data_path, "split/fp_split_data.rds"))
# saveRDS(fp_train, paste0(ml_data_path, "split/fp_train_data.rds"))
# saveRDS(fp_test, paste0(ml_data_path, "split/fp_test_data.rds"))
# # flow_split <- readRDS("dflow/models/validation/dflow_model_data.rds")
# rm(flow_split)
# flow_train <- readRDS("dflow/models/validation/dflow_train_data.rds")
# flow_test  <- readRDS("dflow/models/validation/dflow_test_data.rds")
# Table of model ranks
rank_results(fp_wfs)

# Comparing Accuracy and ROC AUC of 7 models
reg_mod_comp_plot <-
  fp_wfs %>%
  autoplot() + 
  labs(
    col = "",
    title    = "Regression Model comparisons",
    subtitle = "Predicting wins"
  ) 

reg_mod_comp_plot

# Save plot
ggsave(
  paste0(ml_data_path, "plots/regression_model_rank.png"),
  plot   = reg_mod_comp_plot
)
# ****************************
# ---- Select best models ----
# ****************************

metrics_lst <- list()

# rm(acc)
i <- 2
# rm(i, model, model_name, mod_workflow, mod_final_fit, mod_last_fit, mod_workflow_fit, resample_roc_plot, vip_plot,
#    mod_test,mod_train, train_acc, test_acc, overall_aucroc,
# roc_auc_curve_plot, mod_results)

# Extract wf set results
for (i in 1:length(fp_wfs$wflow_id)) {
  
  model       <- fp_wfs$wflow_id[i]
  model_name  <- fp_wfs$info[[i]]$model
  
  model
  
  mod_results <- 
    fp_wfs %>% 
    workflowsets::extract_workflow_set_result(model)
  
  # Extract workflows
  mod_workflow <- 
    fp_wfs %>%  
    extract_workflow(model)
  
  # Model Engine text
  model_engine <- mod_workflow$fit$actions$model$spec$engine
  
  # Model Engine text
  model_mode <- mod_workflow$fit$actions$model$spec$mode
  
  logger::log_info("\n\nExtracting workflow & finalizing model fit:\n  --->  {model_name} - {model_mode}")
  # mod_results$.metrics[[1]]
  
  # print(select_best(mod_results, metric = "rmse"))
  print(select_best(mod_results, metric = "rsq"))
  # select_best(mod_results, metric = "roc_auc")
  # rm(mod_workflow_fit)
  # Finalize workflow fit
  mod_workflow_fit <- 
    mod_workflow %>% 
    finalize_workflow(select_best(mod_results, metric = "rsq")) %>% 
    fit(data = fp_train)
  
   # Fit model to split train/test data
  mod_last_fit <- tune::last_fit(mod_workflow_fit, fp_split)
  
  # print(tune::collect_metrics(mod_results)$mean)
  print(tune::collect_metrics(mod_last_fit))
  
  # Extract & save final fit to use for predictions
  mod_final_fit <- mod_last_fit$.workflow[[1]]
  
  # Resampled CV Fold AUC ROC Curve
  resample_roc_plot <-
    mod_results %>%
    collect_predictions() %>%
    group_by(id) %>% 
    roc_curve(fp_finish, .pred_1) %>%
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
    predict(mod_final_fit, fp_train) %>% 
    bind_cols(predict(mod_final_fit, fp_train, type = "prob")) %>%
    bind_cols(dplyr::select(fp_train, win)) # Add the true outcome data back in
  
  # testing set predictions
  mod_test <-
    predict(mod_final_fit, fp_test) %>% 
    bind_cols(predict(mod_final_fit, fp_test, type = "prob")) %>%
    bind_cols(dplyr::select(fp_test, win)) 
  
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

