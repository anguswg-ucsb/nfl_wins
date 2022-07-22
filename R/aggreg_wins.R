# Angus Watters 
# Aggregate and clean data from nflFastR

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

# Question 1: what QB stats most influence W/L
# Question 2: Factors that influence whether a RB will finish in top 5 on a week?
# Team Win %
# Height
# Weight
# Snap % # Data dictionary
desc <- nflfastR::field_descriptions

# unique seasons
seasons_lst <- 1999:2021
# % RB carries
# Yards per route run 
# Yard per carry
# Number of top 5 finishes so far

# **********************
# ---- Player stats ----
# **********************


# ******************
# ---- Team pbp ----
# ******************

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
# rm(offense)
# season_pbp <- pbp_stats[[1]]
# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
off_stats <- lapply(pbp_stats, FUN = function(x) {
  
  offense <- get_offense(x)
  }
) 

# rm(season_df, season_pbp, offense, first_qtr, off_df2, off_qtr_score_diff, off_drive_stats, off_game, off_qtr_scores, off_time_of_poss, off_stats, qtr_scores, off_dscore_diff)
off_df <- dplyr::bind_rows(off_stats)
# off_df2 <- readRDS(here::here("data", "offensive.rds"))

saveRDS(off_df, here::here("data", "offensive.rds"))

tmp <- 
  off_df %>% 
  dplyr::filter(season == 2020) %>% 
  dplyr::group_by(season, week) %>% 
  dplyr::select(season:div_game, qb_epa) %>% 
  dplyr::mutate(
    qb_epa_mean = mean(qb_epa, na.rm = T),
    qb_epa_normal = qb_epa - qb_epa_mean
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, posteam) %>% 
  dplyr::mutate(
    season_qb_epa = mean(qb_epa, na.rm = T),
    season_qb_epa_normal = mean(qb_epa_normal, na.rm = T)
  )
ggplot() +
  geom_boxplot(data = tmp, aes(x = reorder(posteam, season_qb_epa_normal), y = qb_epa_normal))
  # geom_boxplot(data = tmp, aes(x = reorder(posteam, season_qb_epa), y = qb_epa))
tmp
# ******************************
# ---- Team defensive stats ----
# ******************************
# season_pbp <- pbp_stats[[14]]
# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
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

# elo_r <- nfl_split[[3]] %>% get_nfl_elo()

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

# Join offense, defense, records
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

sort(unique(final_lag_data$rest_days))
sort(unique(final_lag_data$opp_rest_days))
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

# bits_to_dec <- function(bitty) {
#   bitty <- 15
#   # Step 1 divide integer by 2, track quotient and remainder
#   quot  <- 15/2 
#   rem   <- 15 %% 2
#   
#   quot2 <- quot/2
#   rem2  <- quot %% 2
# }
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