# Angus Watters 
# Aggregate and clean data from using NFL play-by-play data from nflFastR

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(timetk)
library(elo)

source("utils/utils.R")

data_path  <-  here::here("data")

# ********************
# ---- NFL Fast R ----
# ********************

# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR

# Question 1: what stats relate to teams winning?
# Question 2: Can we predict whether or not a home team will win their game?

desc <- nflfastR::field_descriptions

# unique seasons
seasons_lst <- 1999:2021

# **********************
# ---- Player stats ----
# **********************


# ******************
# ---- Team pbp ----
# ******************

# Clear cache if needed
# nflreadr::.clear_cache()

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
pbp_stats <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  pbp <- nflfastR::load_pbp(x)
}
) 

# pbp_df <- dplyr::bind_rows(pbp_stats)

# saveRDS(pbp_stats, here::here("data", "pbp.rds"))

# ******************************
# ---- Team offensive stats ----
# ******************************

# calculate overall team offensive statistics from NFL play-by-play data
off_stats <- lapply(pbp_stats, FUN = function(x) {
  
  offense <- get_offense(x)
  }
) 

off_df <- dplyr::bind_rows(off_stats)

saveRDS(off_df, here::here("data", "offensive.rds"))

# ******************************
# ---- Team defensive stats ----
# ******************************

# calculate overall team defensive statistics from NFL play-by-play data
def_stats <- lapply(pbp_stats, FUN = function(x) {
  
  defense <- get_defense(x)
  
}
) 
def_df <- dplyr::bind_rows(def_stats) 

saveRDS(def_df, here::here("data", "defensive.rds"))

# **********************
# ---- Team records ----
# **********************

# pull rosters for every year
team_records <- lapply(seasons_lst, FUN = function(x) {

  logger::log_info("Season PBP: {x}")

  schedule <- nflfastR::fast_scraper_schedules(x) %>%
    get_schedule()
  }
) %>%
  dplyr::bind_rows()

saveRDS(team_records, here::here("data", "wins.rds"))

# ********************
# ---- ELO Rating ----
# ********************
# Team records
team_records <- readRDS(here::here("data", "wins.rds"))

nfl_split <- 
  team_records %>% 
  dplyr::filter(home_away == "home_team") %>%
  dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
  dplyr::group_by(season) %>% 
  dplyr::group_split()

# pull rosters for every year
nfl_elo <- lapply(nfl_split, FUN = function(x) {
  
  elo_rating <- get_nfl_elo(x)
  
}
) %>%
  dplyr::bind_rows()

saveRDS(nfl_elo, here::here("data", "elo_ratings.rds"))

# off_df %>% 
#   dplyr::left_join(nfl_elo, 
#                    by = "game_id")

# ******************************
# ---- Join Offense/Defense ----
# ******************************

# rm(off_df)
# Offense
off_df       <- readRDS(here::here("data", "offensive.rds"))

# Defense
def_df       <- readRDS(here::here("data", "defensive.rds"))

# Team records
team_records <- readRDS(here::here("data", "wins.rds"))

# Vegas game spreads
game_spreads <- readRDS(here::here("data", "closing_spread_lines.rds"))

# Elo ratings
nfl_elo      <- readRDS(here::here("data", "elo_ratings.rds"))

# Join home team offense, defense, records
off_def_data <- 
  team_records %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days,
                win_pct, home_win_pct, away_win_pct, win) %>%
  dplyr::left_join(
    dplyr::select(game_spreads, -home_away),
    by = c("season", "week", "game_id", "team")
  ) %>% 
  dplyr::left_join(
    off_df,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    def_df,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(nfl_elo, -win),
    by = c("season", "week", "game_id", "team")
  ) 

# Join away team offense, defense, records
opp_off_def_data <- 
  team_records %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days,
                win_pct, home_win_pct, away_win_pct) %>%
  dplyr::left_join(
    dplyr::select(game_spreads, -home_away),
    by = c("season", "week", "game_id", "team")
  ) %>% 
  dplyr::left_join(
    off_df,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    def_df,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(nfl_elo, -win),
    by = c("season", "week", "game_id", "team")
  ) %>% 
  setNames(c("season", "week", "game_id", "team", "opponent", paste0("opp_", names(.)[6:45]))) %>%
  dplyr::select(-team)


team_off_def <- 
  off_def_data %>%
  dplyr::left_join(
    dplyr::select(opp_off_def_data, game_id, opponent, opp_rest_days:opp_elo),
    by = c("game_id", "opponent")
  ) %>% 
  dplyr::mutate(
    pts_scored      = qtr_pts_1 + qtr_pts_2 + qtr_pts_3 + qtr_pts_4,
    pts_against     = def_qtr_pts_1 + def_qtr_pts_2 + def_qtr_pts_3 + def_qtr_pts_4,
    point_diff      = pts_scored - pts_against,
    opp_pts_scored  = opp_qtr_pts_1 + opp_qtr_pts_2 + opp_qtr_pts_3 + opp_qtr_pts_4,
    opp_pts_against = opp_def_qtr_pts_1 + opp_def_qtr_pts_2 + opp_def_qtr_pts_3 + opp_def_qtr_pts_4,
    opp_point_diff  = opp_pts_scored - opp_pts_against
  ) %>%  
  dplyr::mutate(across(where(is.numeric), round, 4)) %>% 
  dplyr::select(-score_diff, -opp_score_diff) %>% 
  dplyr::rename(
    "score_diff"     = "point_diff", 
    "opp_score_diff" = "opp_point_diff"
  ) %>% 
  dplyr::mutate(
    home_fav = dplyr::case_when(
      home == 1 & spread_line > 0 ~ 1,
      spread_line == 0            ~ 0,
      TRUE                        ~ 0 
    ),
    fav = dplyr::case_when(
      home == 1 & spread_line > 0 ~ 1, 
      home == 1 & spread_line < 0 ~ 0, 
      home == 0 & spread_line < 0 ~ 1, 
      home == 0 & spread_line > 0 ~ 0,
      spread_line == 0            ~ 0,
      TRUE                        ~ 0 
    ),
    abs_spread_line = abs(spread_line)
  ) %>% 
  dplyr::mutate(
    home_fav         = factor(home_fav, levels = c(1, 0)),
    fav              = factor(fav, levels = c(1, 0))
    # rest_days        = factor(rest_days, levels = c("short_rest", "normal_rest", "long_rest")),
    # opp_rest_days    = factor(opp_rest_days, levels = c("short_rest", "normal_rest", "long_rest"))
  )

saveRDS(team_off_def, here::here("data", "football_wins.rds"))

# *********************************
# ---- Season to date averages ----
# *********************************

# cumalative offense
lag_off      <- rolling_offense(off_df)

# cumalative defense
lag_def      <- rolling_defense(def_df)

# cumalative win %
lag_record   <- rolling_record(team_records)

# cumalative win %
lag_elo   <- rolling_elo(nfl_elo)

# lagged data from home team POV
lag_data <- 
  lag_record %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    game_spreads,
    by = c("season", "week", "game_id", "team")
  ) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    lag_def,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(lag_elo, game_id, team, elo),
    by = c("game_id", "team")
  )

# lagged data from away team POV
opp_lag_data <- 
  lag_record %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    lag_def,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>%
  dplyr::left_join(
    dplyr::select(lag_elo, game_id, team, elo),
    by = c("game_id", "team")
  ) %>% 
  setNames(c("season", "week", "game_id", "team", "opponent", paste0("opp_", names(.)[6:44]))) %>%
  dplyr::select(-team)

final_lag_data <- 
  lag_data %>% 
  dplyr::left_join(
    # dplyr::select(opp_lag_data, -opponent),
    opp_lag_data,
    by = c("season", "week", "game_id", "team" = "opponent")
  ) %>% 
  dplyr::select(-opp_win, -opp_home, -opp_div_game) %>% 
  dplyr::relocate(season, week, game_id, team, opponent, win, win_pct, home_win_pct, away_win_pct, home, div_game) %>% 
  dplyr::mutate(
    pts_scored      = qtr_pts_1 + qtr_pts_2 + qtr_pts_3 + qtr_pts_4,
    pts_against     = def_qtr_pts_1 + def_qtr_pts_2 + def_qtr_pts_3 + def_qtr_pts_4,
    point_diff      = pts_scored - pts_against,
    opp_pts_scored  = opp_qtr_pts_1 + opp_qtr_pts_2 + opp_qtr_pts_3 + opp_qtr_pts_4,
    opp_pts_against = opp_def_qtr_pts_1 + opp_def_qtr_pts_2 + opp_def_qtr_pts_3 + opp_def_qtr_pts_4,
    opp_point_diff  = opp_pts_scored - opp_pts_against
  ) %>% 
  dplyr::mutate(
    div_game  = factor(div_game, levels = c(1, 0)),
    # rest_days = dplyr::case_when(
    #   rest_days < 7   ~  "short_rest",
    #   rest_days == 7  ~  "normal_rest",
    #   rest_days > 7   ~  "long_rest"
    # ),
    # opp_rest_days = dplyr::case_when(
    #   opp_rest_days < 7   ~  "short_rest",
    #   opp_rest_days == 7  ~  "normal_rest",
    #   opp_rest_days > 7   ~  "long_rest"
    # ),
    win  = factor(win, levels = c(1, 0)),
    home = factor(home, levels = c(1, 0))
  ) %>% 
  dplyr::mutate(
    across((contains("dscore")), ~replace(., is.na(.), 0))
  ) %>% 
  na.omit() %>%
  # dplyr::select(win, contains("qtr_pts"))%>% 
  dplyr::select(-contains("dscore_diff"), -contains("def_qtr_pts"),
                -contains("ndrives"), -contains("drive_time_of_possession_sec"),
                -contains("score_drive_pct"), -contains("qtr_pts"), -opp_score_diff,
                -score_diff, -contains("score_drives_pct"), -home_away, -contains("score_diff_qtr_2"),
                -contains("score_diff_qtr_3")
                ) %>%
  dplyr::mutate(across(where(is.numeric), round, 4)) %>% 
  dplyr::rename(
    "score_diff"     = "point_diff", 
    "opp_score_diff" = "opp_point_diff"
    ) %>% 
  dplyr::mutate(
    home_fav = dplyr::case_when(
      home == 1 & spread_line > 0 ~ 1,
      spread_line == 0            ~ 0,
      TRUE                        ~ 0 
    ),
    fav = dplyr::case_when(
      home == 1 & spread_line > 0 ~ 1, 
      home == 1 & spread_line < 0 ~ 0, 
      home == 0 & spread_line < 0 ~ 1, 
      home == 0 & spread_line > 0 ~ 0,
      spread_line == 0            ~ 0,
      TRUE                        ~ 0 
    ),
    abs_spread_line = abs(spread_line)
  ) %>% 
  dplyr::mutate(
    home_fav         = factor(home_fav, levels = c(1, 0)),
    fav              = factor(fav, levels = c(1, 0))
    # rest_days        = factor(rest_days, levels = c("short_rest", "normal_rest", "long_rest")),
    # opp_rest_days    = factor(opp_rest_days, levels = c("short_rest", "normal_rest", "long_rest"))
  )

# The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference)
final_lag_data %>%
  count(rest_days) %>%
  dplyr::mutate(
    n_sum = sum(n, na.rm = T),
    pct   = round(100*(n/n_sum), 3)
  )
hist(final_lag_data$rest_days, breaks = 20)

final_lag_data %>%
  count(div_game) %>%
  dplyr::mutate(
    n_sum = sum(n, na.rm = T),
    pct   = round(100*(n/n_sum), 3)
  )

hist(final_lag_data$rest_days, breaks = 20)

saveRDS(final_lag_data, here::here("data", "football_wins_lag_elo.rds"))

# ********************************
# ---- Get Data for API model ----
# ********************************

# Data for model to be used in API (less variables used than first modeling approach)

# Check if api_offense.rds file exists, if not, go get it 
if(file.exists(here::here("data", "api_offense.rds"))) {
  logger::log_info("\n\nReading: api_offense.rds")
  off_df <- readRDS(here::here("data", "api_offense.rds"))
  
} else {
  
  logger::log_info("\n\nCould not find api_offense.rds\nRetreiving offense data")
  
  # calculate overall team offensive statistics from NFL play-by-play data
  off_stats <- lapply(pbp_stats, FUN = function(x) {
    
    offense <- get_offense2(x)
  }
  ) 
  
  # select desired columns
  off_df <-
    off_stats %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(season, week, game_id, posteam, home, div_game, turnovers, score_diff)
  
  # Save
  saveRDS(off_df, here::here("data", "api_offense.rds"))
}

# Check if api_offense.rds file exists, if not, go get it 
if(file.exists(here::here("data", "wins.rds"))) {
 
  logger::log_info("\n\nReading: wins.rds")
  team_records <- readRDS(here::here("data", "wins.rds"))
  
  if(file.exists(here::here("data", "elo_ratings.rds"))) {
    
    logger::log_info("\n\nReading: elo_ratings.rds")
    
    nfl_elo <- readRDS(here::here("data", "elo_ratings.rds"))
    
  } else {
    logger::log_info("\n\nCould not find elo_ratings.rds\nCalculating Elo Ratings...")
    
    # Split team records data by season
    nfl_split <- 
      team_records %>% 
      dplyr::filter(home_away == "home_team") %>%
      dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
      dplyr::group_by(season) %>% 
      dplyr::group_split()
    
    # calculate Elo ratings for all years
    nfl_elo <- lapply(nfl_split, FUN = function(x) {
      
      elo_rating <- get_nfl_elo(x)
      
    }
    ) %>%
      dplyr::bind_rows()
    
    logger::log_info("\n\nSaving: data/elo_ratings.rds")
    
    # save Elo
    saveRDS(nfl_elo, here::here("data", "elo_ratings.rds"))
  }

} else {
  
  logger::log_info("\n\nCould not find team_records.rds\nRetrieving records...")
  
  # pull team schedule/records for all seasons
  team_records <- lapply(seasons_lst, FUN = function(x) {
    
    logger::log_info("Season PBP: {x}")
    
    schedule <- nflfastR::fast_scraper_schedules(x) %>%
      get_schedule()
  }
  ) %>%
    dplyr::bind_rows()
  
  logger::log_info("\n\nSaving: data/wins.rds")
  
  # save records
  saveRDS(team_records, here::here("data", "wins.rds"))
  
  logger::log_info("\n\nCalculating Elo Ratings...")
  
  # Split team records data by season
  nfl_split <- 
    team_records %>% 
    dplyr::filter(home_away == "home_team") %>%
    dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
    dplyr::group_by(season) %>% 
    dplyr::group_split()
  
  # calculate Elo ratings for all years
  nfl_elo <- lapply(nfl_split, FUN = function(x) {
    
    elo_rating <- get_nfl_elo(x)
    
  }
  ) %>%
    dplyr::bind_rows()
  
  logger::log_info("\n\nSaving: data/elo_ratings.rds")
  
  # save Elo
  saveRDS(nfl_elo, here::here("data", "elo_ratings.rds"))
}

# **********************
# ---- API Lag data ----
# **********************

# cumalative offense
lag_off      <- rolling_score(off_df)

# cumalative win %
lag_record   <- rolling_record(team_records)

# cumalative win %
lag_elo   <- rolling_elo(nfl_elo)

# ******************************
# ---- API Join lagged data ----
# ******************************

# lagged data from home team POV
lag_data <- 
  lag_record %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(lag_elo, game_id, team, elo),
    by = c("game_id", "team")
  )

# lagged data from away team POV
opp_lag_data <- 
  lag_record %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(lag_elo, game_id, team, elo),
    by = c("game_id", "team")
  ) %>% 
  setNames(c("season", "week", "game_id", "team", "opponent", paste0("opp_", names(.)[6:44]))) %>%
  dplyr::select(-team)

# final join of data for modeling (API model)
final_lag_data <- 
  lag_data %>% 
  dplyr::left_join(
    opp_lag_data,
    by = c("season", "week", "game_id", "team" = "opponent")
  ) %>% 
  dplyr::select(-opp_win, -opp_home, -opp_div_game, -div_game) %>% 
  dplyr::mutate(
    win  = factor(win, levels = c(1, 0)),
    home = factor(home, levels = c(1, 0))
  ) %>% 
  dplyr::relocate(season, week, game_id, team, opponent, win, win_pct, home_win_pct, away_win_pct, home) %>% 
  na.omit() 

# save
saveRDS(final_lag_data, here::here("data", "api_model_data.rds"))
# ***********************
# ---- Clean rosters ----
# ***********************

# pull rosters for every year
rosters <- lapply(seasons_lst, FUN = function(x) {
  fsr <- fast_scraper_roster(x)  %>% 
    clean_rosters()
}
) %>% 
  dplyr::bind_rows() 


saveRDS(rosters, here::here("data", "rosters.rds"))
rosters <- readRDS(here::here("data", "rosters.rds"))