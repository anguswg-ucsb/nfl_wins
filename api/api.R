# Angus Watters 
# Plumber API to take new data from web scraping function and input into model to generate predictions

rm(list = ls())

# Data manipulation packages
library(tidyverse)
library(tidymodels)
library(janitor)
library(stringr)
library(httr)
library(jsonlite)
library(rvest)
library(elo)

# API packages
library(plumber)
library(vetiver)

source("utils/utils.R")

# *******************
# ---- Load Data ----
# *******************

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

# Modeling data
nfl_df <-
  readRDS(here::here("data", "api_model_data.rds")) %>% 
  dplyr::filter(home == 1) %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, 
                opp_rest_days, elo, opp_elo, score_diff, 
                opp_score_diff, turnovers, opp_turnovers,
                win_pct, away_win_pct, home_win_pct, 
                opp_win_pct, opp_away_win_pct, opp_home_win_pct) 
  # dplyr::select(
  #   order(colnames(.))
  #   )

new_data <- scrape_games(
  year      = 2021, 
  pred_week = 14
  ) 

# Paths to models
log_reg_path <- paste("D:", "nfl", "api_classification", "fit", "win_model_logistic_reg.rds", sep = "/")
svm_path     <- paste("D:", "nfl", "api_classification", "fit", "win_model_svm_linear.rds", sep = "/")

# read in models
log_reg_model <- readRDS(log_reg_path)
svm_model     <- readRDS(svm_path)

pred <- augment(log_reg_model, new_data)
tmp <- 
  nfl_df %>% 
  dplyr::filter(season == 2021, week == 18) %>% 
  dplyr::select(season, week, game_id, team, opponent, score_diff, opp_score_diff, elo, opp_elo) %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))

tmp2 <- new_data %>% 
  dplyr::select(season, week, game_id, team, opponent, score_diff, opp_score_diff, elo, opp_elo) %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))
names(new_data) %in% names(nfl_df)
names(nfl_df) %in% names(new_data)
glimpse(nfl_df)
glimpse(new_data)

# Paths to models
log_reg_path <- paste("D:", "nfl", "api_classification", "fit", "win_model_logistic_reg.rds", sep = "/")
svm_path     <- paste("D:", "nfl", "api_classification", "fit", "win_model_svm_linear.rds", sep = "/")

# read in models
log_reg_model <- readRDS(log_reg_path)
svm_model     <- readRDS(svm_path)

here::here("D:")

























