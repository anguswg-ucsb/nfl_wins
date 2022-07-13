# Model for predicting NFL spread (end of game score difference)

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
# na_df <- nfl_df[rowSums(is.na(nfl_df)) > 0,]

# Load Lag NFL win data
# nfl_df <- readRDS(here::here("data", "football_spread_lag.rds")) %>% 
#   na.omit() %>%
#   dplyr::filter(home == 1) %>%
#   # dplyr::filter(week <= 18) %>%
#   # dplyr::mutate(home = as.factor(home)) %>%
#   dplyr::select(-home, -week, -score_drives, -ndrives, -def_ndrives,
#                 -opp_spread, -opp_score_drives, -opp_ndrives, -opp_def_score_drives, -opp_def_ndrives,
#                 -drive_time_of_possession_sec,  -opp_drive_time_of_possession_sec) %>% 
#   dplyr::mutate(
#     pts_scored      = qtr_pts_1 + qtr_pts_2 + qtr_pts_3 + qtr_pts_4,
#     pts_against     = def_qtr_pts_1 + def_qtr_pts_2 + def_qtr_pts_3 + def_qtr_pts_4,
#     point_diff      = pts_scored - pts_against,
#     opp_pts_scored  = opp_qtr_pts_1 + opp_qtr_pts_2 + opp_qtr_pts_3 + opp_qtr_pts_4,
#     opp_pts_against = opp_def_qtr_pts_1 + opp_def_qtr_pts_2 + opp_def_qtr_pts_3 + opp_def_qtr_pts_4,
#     opp_point_diff  = opp_pts_scored - opp_pts_against
#   ) %>% 
#   dplyr::select(-qtr_pts_1, -qtr_pts_2,  -qtr_pts_3 ,   -qtr_pts_4, 
#                 -def_qtr_pts_1, def_qtr_pts_2, def_qtr_pts_3, def_qtr_pts_4,
#                 -opp_qtr_pts_1, -opp_qtr_pts_2,  -opp_qtr_pts_3 ,   -opp_qtr_pts_4,
#                 -opp_def_qtr_pts_1, opp_def_qtr_pts_2, opp_def_qtr_pts_3, opp_def_qtr_pts_4
#                 )
  # dplyr::select(-week, -season, -score_drives, -ndrives, -def_ndrives,
  #               -opp_spread, -opp_score_drives, -opp_ndrives, -opp_def_score_drives, -opp_def_ndrives,
  #               -drive_time_of_possession_sec,  -opp_drive_time_of_possession_sec)
nfl_df <- readRDS(here::here("data", "football_closing_spread_lag.rds")) %>% 
  na.omit() %>% 
  dplyr::filter(home == 1) %>%
  dplyr::select(-home, -week, -spread, -score_drives, -ndrives, -def_ndrives,
                -opp_spread, -opp_score_drives, -opp_ndrives, -opp_def_score_drives, -opp_def_ndrives,
                -drive_time_of_possession_sec,  -opp_drive_time_of_possession_sec, -pts_scored) %>% 
  dplyr::select(-contains("dscore_diff")) %>%
  # dplyr::select(-contains("dscore_diff"), -contains("score_diff"), -contains("def")) %>%
  dplyr::rename(spread = spread_line) 
  # dplyr::mutate(
  #   diff_qtr_pts_1 = qtr_pts_1 - def_qtr_pts_1
  # ) %>% 
  # dplyr::relocate(qtr_pts_1, def_qtr_pts_1, diff_qtr_pts_1)
spread_cor <-
  nfl_df %>%
  # dplyr::filter(home == 1) %>%
  # dplyr::filter(week >= 5, week <= 17) %>%
  # na.omit() %>%
  dplyr::select(spread,pts_for, pts_against, point_diff, opp_pts_for,
                opp_pts_against, opp_point_diff,
                qb_epa, turnovers, top_pct, score_drives_pct) %>%
  cor(use =  "pairwise.complete.obs") %>%
  round(2) %>%
  reshape2::melt()

# spread_cor_plot <-
  ggplot(
    data = spread_cor, aes(
      x    = Var1,
      y    = Var2,
      fill = value
    )
  ) +
  geom_tile() +
  geom_text(
    aes(Var2, Var1, label = value),
    color = "black",
    size  = 4
  ) +
  scale_fill_gradient2(low="darkred", high="midnightblue", guide="colorbar")
# nfl_long <- 
#   nfl_df %>% 
#   dplyr::mutate(
#     pts_scored  = qtr_pts_1 + qtr_pts_2 + qtr_pts_3 + qtr_pts_4,
#     pts_against = def_qtr_pts_1 + def_qtr_pts_2 + def_qtr_pts_3 + def_qtr_pts_4,
#     point_diff  = pts_scored - pts_against
#   ) %>% 
#   dplyr::relocate( qtr_pts_1, qtr_pts_2,  qtr_pts_3 , qtr_pts_4, pts_scored, pts_against, point_diff)
hist(nfl_df$pts_scored)
hist(nfl_df$spread)
sd((nfl_df$spread))
mean(scale(nfl_df$spread))

# Set random seed
set.seed(234)

# split data for train/test, stratify by quantiles of fantasy points 
# nfl_split <- initial_split(nfl_df, strata = season)
nfl_split <- initial_split(nfl_df, strata = season)

# training data split
nfl_train <- training(nfl_split)

# testinng data split
nfl_test  <- testing(nfl_split)

nfl_train %>% 
  count(season) %>% 
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games)
    ) %>% 
  ggplot() +
  geom_col(aes(x = season, y = pct_total)) +
  scale_y_continuous(limits = c(0, 10))

nfl_test %>% 
  count(season) %>% 
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games)
  ) %>% 
  ggplot() +
  geom_col(aes(x = season, y = pct_total)) +
  scale_y_continuous(limits = c(0, 10))

# *****************
# ---- Recipes ----
# *****************

usemodels::use_ranger(spread~., data = nfl_train)
usemodels::use_kknn(spread~., data = nfl_train)
usemodels::use_xgboost(spread~., data = nfl_train)
usemodels::use_glmnet(spread~., data = nfl_train)
usemodels::use_earth(spread~., data = nfl_train)
usemodels::use_kernlab_svm_rbf(spread~., data = nfl_train)
usemodels::use_kernlab_svm_poly(spread~., data = nfl_train)

# Data preprocessing 
logger::log_info("Data preprocessing...")

# K nearest neighbors
kknn_recipe <- 
  recipes::recipe(
    formula = spread ~ ., 
    data    = nfl_train
  ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, new_role = "ID"
  ) %>% 
  step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(nfl_finish) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

kknn_recipe

# kknn_recipe %>% 
#   prep() %>% 
#   juice() %>%
#   glimpse()

# Linear regression
glmnet_recipe <- 
  recipes::recipe(
    formula = spread ~ ., 
    data    = nfl_train
  ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, new_role = "ID"
  ) %>% 
  step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

# XGBoost trees
xgboost_recipe <- 
  recipes::recipe(
    formula = spread ~ ., 
    data    = nfl_train
  ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, new_role = "ID"
  ) %>% 
  step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  # themis::step_smote(nfl_finish) %>%
  step_zv(all_predictors()) 

# MARS model
earth_recipe <- 
  recipes::recipe(
    formula = spread ~ ., 
    data    = nfl_train
  ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, new_role = "ID"
  ) %>% 
  step_string2factor(one_of("rest_days", "opp_rest_days")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # themis::step_smote(fp_finish) %>%
  step_zv(all_predictors()) 

kernlab_recipe <- 
  recipe(
    formula = spread ~ ., 
    data    = nfl_train
    ) %>% 
  recipes::update_role(
    game_id, team, opponent, season, new_role = "ID"
  ) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

# kernlab_poly_recipe <- 
#   recipe(
#     formula = spread ~ ., 
#     data    = nfl_train
#     ) %>% 
#   recipes::update_role(
#     game_id, team, opponent, season, new_role = "ID"
#   ) %>% 
#   step_zv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) 

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

# ranger_spec <-
#   rand_forest(
#     mtry  = tune(),
#     min_n = tune(),
#     trees = tune()
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
  set_mode("regression") %>% 
  parsnip::set_engine("xgboost", importance = "permutation") 

earth_spec <-
  mars(
    num_terms    = tune(),
    prod_degree  = tune(),
    prune_method = "none") %>%
  set_mode("regression") %>%
  set_engine("earth")

kernlab_spec <- 
  svm_rbf(
    cost      = tune(), 
    rbf_sigma = tune()
    ) %>% 
  set_mode("regression") 

# kernlab_poly_spec <- 
#   svm_poly(
#     cost         = tune(), 
#     degree       = tune(), 
#     scale_factor = tune()
#     ) %>% 
#   set_mode("regression") 
# ********************************
# ---- Cross Validation folds ----
# ********************************

# Set seed for resampling 
set.seed(432)

# CV folds
nfl_folds <- rsample::vfold_cv(nfl_train, v = 10, strata = season)
# nfl_folds <- rsample::bootstraps(nfl_train, strata = win)

# ---- Workflow set of models ----
nfl_wfs <- 
  workflow_set(
    preproc = list(
      kknn_rec         = kknn_recipe,
      glmnet_rec       = glmnet_recipe,
      # ranger_rec      = ranger_recipe,
      xgboost_rec      = xgboost_recipe,
      earth_rec        = earth_recipe,
      kernlab_rec      = kernlab_recipe
      # kernlab_poly_rec = kernlab_poly_recipe
    ),
    models  = list(
      kknn       = kknn_spec,
      glmnet     = glmnet_spec,
      # ranger     = ranger_spec,
      xgboost    = xgboost_spec,
      earth      = earth_spec,
      svm        = kernlab_spec
      # svm_poly   = kernlab_poly_spec
    ),
    cross = F
  )

# Choose metrics
my_metrics <- yardstick::metric_set(rsq, rmse, mae)
# my_metrics <- yardstick::metric_set(roc_auc, pr_auc, accuracy, mn_log_loss)

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
 # nfl_wfs[c(1:5),] %>% 
  # rank_results()

# Comparing Accuracy and ROC AUC of 7 models
reg_mod_comp_plot <-
  nfl_wfs %>%
  # nfl_wfs[c(1:5),] %>% 
  autoplot() + 
  labs(
    col = "",
    title    = "Regression Model comparisons",
    subtitle = "Predicting spread"
  ) 

reg_mod_comp_plot

save_path <- "D:/nfl/classification/"

saveRDS(nfl_wfs, paste0(save_path, "wfs/win_classification_wfs.rds"))

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
  print(select_best(mod_results, metric = "rsq"))
  # select_best(mod_results, metric = "roc_auc")
  # rm(mod_workflow_fit)
  # Finalize workflow fit
  mod_workflow_fit <- 
    mod_workflow %>% 
    # finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>% 
    finalize_workflow(select_best(mod_results, metric = "rsq")) %>%
    fit(data = nfl_train)
  
  # Fit model to split train/test data
  mod_last_fit <- tune::last_fit(mod_workflow_fit, nfl_split)
  
  # print(tune::collect_metrics(mod_results)$mean)
  print(tune::collect_metrics(mod_last_fit))
  
  # Extract & save final fit to use for predictions
  mod_final_fit <- mod_last_fit$.workflow[[1]]
  
  # Resampled CV Fold AUC ROC Curve
  # resample_roc_plot <-
  #   mod_results %>%
  #   collect_predictions() %>%
  #   group_by(id) %>% 
  #   roc_curve(win, .pred_1) %>%
  #   ggplot(aes(1 - specificity, sensitivity, color = id)) +
  #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
  #   geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  #   coord_equal() +
  #   labs(
  #     title    = paste0("AUC-ROC Curve - ", model_name),
  #     subtitle = "Resample results from 10 Fold Cross Validation",
  #     x        = "1 - Specificity",
  #     y        = "Sensitivity"
  #   ) 
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
        vip::vip(num_features = 65) +
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
    # bind_cols(predict(mod_final_fit, nfl_train)) %>%
    bind_cols(dplyr::select(nfl_train, spread)) # Add the true outcome data back in
  
  yardstick::rmse(mod_train, truth = spread, .pred)
  yardstick::rsq(mod_train, truth = spread, .pred)
  
  # testing set predictions
  mod_test <-
    predict(mod_final_fit, nfl_test) %>% 
    bind_cols(dplyr::select(nfl_test, spread)) %>% 
    # bind_cols(predict(mod_final_fit, nfl_test, type = "prob")) %>%
    # bind_cols(dplyr::select(nfl_test, spread)) %>% 
    dplyr::mutate(across(where(is.numeric), round, 1))
  
  yardstick::rmse(mod_test, truth = spread, .pred)
  yardstick::rsq(mod_test, truth = spread, .pred)
  
  ##Plot the predicted and true values 

  mod_last_fit %>%
    collect_predictions() %>%
    # ggplot(aes(spread, .pred, color = id)) +
    ggplot(aes(spread, .pred)) +
    geom_abline(lty = 2, color = "gray80", size = 1.5) +
    # geom_point(alpha = 0.3) +
    geom_point(color = "midnightblue", alpha = 0.3) +
    labs(
      x = "Truth",
      y = "Predicted year",
      color = NULL,
      title = "Predicted and True Years for Frequency",
      subtitle = "Each Cross-validation Fold is Shown in a Different Color"
    )
  
    # dplyr::mutate(spread_diff = round(spread_diff, 2)) %>% 
    # dplyr::mutate(across(where(is.numeric), round, 2)) %>% 
  # # Train accuracy
  # train_acc <- 
  #   mod_train %>% 
  #   accuracy(truth = win, .pred_class) %>% 
  #   mutate(
  #     data   = "train",
  #     model  = model_name
  #   )
  # print(train_acc)
  # # Test accuracy
  # test_acc <- 
  #   mod_test %>% 
  #   accuracy(truth = win, .pred_class) %>% 
  #   mutate(
  #     data   = "test",
  #     model  = model_name
  #   )
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

game_outcomes <- readRDS(here::here("data", "football_closing_spread_lag.rds")) %>% 
  dplyr::select(game_id, season, week, team, actual_spread = spread)

pred_outcomes <- 
  mod_final_fit %>% 
  predict(nfl_test) %>% 
  bind_cols(dplyr::select(nfl_test, game_id, team, spread)) %>% 
  dplyr::left_join(
    game_outcomes,
    by = c("game_id", "team")
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  dplyr::relocate(season, week, game_id, team, spread, .pred, actual_spread) %>%
  dplyr::arrange(season, week) %>% 
  dplyr::mutate(
    n = 1:n()
  ) 

pred_eval %>% 
  dplyr::group_by(season, week, name) %>% 
  dplyr::summarise(
    spread_diff = mean(spread_diff, na.rm = T)
  )

pred_eval <- 
  pred_outcomes %>% 
  dplyr::group_by(game_id) %>% 
  tidyr::pivot_longer(cols = c(spread, .pred)) %>% 
  dplyr::mutate(
    spread_diff = abs(actual_spread - value)
    # spread_diff = actual_spread - value
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(game_id) %>%
  dplyr::mutate(n = cur_group_id()) %>% 
  # dplyr::mutate(
  #   n = n()
  # ) %>%
  dplyr::ungroup()
# dplyr::filter(n < 200)
# dplyr::group_by(game_id) %>% 
# tidyr::pivot_wider(names_from = name, values_from = c(spread_diff, value))

abs_pred_diff <- 
  pred_eval %>% 
  dplyr::group_by(season, week, name) %>% 
  dplyr::summarise(
    spread_diff = mean(spread_diff, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  # tidyr::pivot_wider(
  #   names_from  = name, 
  #   values_from = spread_diff
  #   ) %>% 
  dplyr::mutate(spread_diff = round(spread_diff, 2)) %>% 
  # dplyr::mutate(across(where(is.numeric), round, 2)) %>% 
  dplyr::arrange(season, week) %>% 
  dplyr::mutate(week = factor(week))
# dplyr::mutate(
#   n = 1:n()
# )

abs_pred_diff %>% 
  ggplot() +
  geom_hline(yintercept = 0, size = 2, alpha = 0.7, col = "black") +
  geom_boxplot(aes(x = sort(week), y = spread_diff, fill = name)) +
  scale_y_continuous(limits = c(0, 30))
# geom_line(aes(x = n, y = .pred))
# geom_line(aes(x = reorder(season, week), y = .pred))
pred_outcomes %>% 
  tidyr::pivot_longer(cols = c(spread, .pred)) %>% 
  ggplot() +
  geom_point(aes(x = actual_spread , y = value, color = name)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_grid(~name)

pred_outcomes %>% 
  tidyr::pivot_longer(cols = c(spread, .pred, actual_spread)) %>% 
  dplyr::mutate(
    name = factor(name, levels = c("actual_spread", "spread", ".pred"))
    # name = factor(name, levels = c("spread", ".pred"))
  ) %>% 
  ggplot() +
  geom_point(aes(x = n , y = value, color = name, alpha = 0.9),  size = 1) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~season)

pred_outcomes %>% 
  tidyr::pivot_longer(cols = c(spread, .pred)) %>% 
  dplyr::mutate(
    name = factor(name, levels = c("spread", ".pred"))
    # name = factor(name, levels = c("spread", ".pred"))
  ) %>% 
  ggplot() +
  geom_point(aes(x = n , y = value, color = name, alpha = 0.9),  size = 1) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~season)

pred_eval %>% 
  ggplot() +
  geom_line(aes(x = n , y = spread_diff, color = name)) +
facet_wrap(~season)

pred_correct <- 
  pred_eval %>% 
  tidyr::pivot_wider(
    names_from  = name,
    values_from = c(spread_diff, value)
  ) %>% 
  dplyr::rename(spread = value_spread, .pred = value_.pred) %>% 
  dplyr::mutate(
    closer = dplyr::case_when(
      spread_diff_spread >= spread_diff_.pred  ~ 1,
      spread_diff_spread < spread_diff_.pred  ~ 0
      # spread_diff_spread == spread_diff_.pred ~ 1,
      # TRUE                                   ~ NA
    )
  ) %>% 
  dplyr::group_by(season, week) %>% 
  dplyr::mutate(
    games = n(),
    pct_correct = sum(closer, na.rm = T)/games
  ) %>% 
  dplyr::summarise(pct_correct = mean(pct_correct, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(season, week) %>% 
  dplyr::mutate(
    season = factor(season),
    week = factor(week)
  )

pred_correct %>% 
  ggplot() +
  # geom_hline(yintercept = 0, size = 2, alpha = 0.7, col = "black") +
  geom_boxplot(aes(x = sort(week), y = pct_correct)) +
  scale_y_continuous(limits = c(0, 1))
