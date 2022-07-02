# Angus Watters 
# Aggregate and clean data from nflFastR

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(timetk)

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


# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
off_stats <- lapply(pbp_stats, FUN = function(x) {
  
  offense <- get_offense(x)

}
) 
off_df <- dplyr::bind_rows(off_stats)

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

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
def_stats <- lapply(pbp_stats, FUN = function(x) {
  
  defense <- get_defense(x)
  
}
) 
def_df <- dplyr::bind_rows(def_stats) %>% 
  dplyr::select(-contains("def_qtr_pts_6"))

saveRDS(def_df, here::here("data", "defensive.rds"))

 # **********************
# ---- Team records ----
# **********************
rm(pbp)

# pull rosters for every year
team_records <- lapply(seasons_lst, FUN = function(x) {

  logger::log_info("Season PBP: {x}")

  schedule <- nflfastR::fast_scraper_schedules(x) %>%
    get_schedule()
  }
) %>%
  dplyr::bind_rows()

saveRDS(team_records, here::here("data", "wins.rds"))

# *********************************
# ---- Season to date averages ----
# *********************************
 
# Offense
off_df       <- readRDS(here::here("data", "offensive.rds"))

# Defense
def_df       <- readRDS(here::here("data", "defensive.rds"))

# Team records
team_records <- readRDS(here::here("data", "wins.rds"))

# Join offense, defense, records
off_def_data <- 
  team_records %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    off_df,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    def_df,
    by = c("season", "week", "game_id", "team" = "defteam")
  )

saveRDS(off_def_data, here::here("data", "football_wins.rds"))

# cumalative offense
lag_off      <- rolling_offense(off_df)

# cumalative defense
lag_def      <- rolling_defense(def_df)

# cumalative win %
lag_record   <- rolling_record(team_records)


lag_data <- 
  lag_record %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    lag_def,
    by = c("season", "week", "game_id", "team" = "defteam")
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
  setNames(c("season", "week", "game_id", "team", "opponent", paste0("opp_", names(.)[6:36]))) %>% 
  dplyr::select(-team)

final_lag_data <- 
  lag_data %>% 
  dplyr::left_join(
    # dplyr::select(opp_lag_data, -opponent),
    opp_lag_data,
    by = c("season", "week", "game_id", "team" = "opponent")
  ) %>% 
  dplyr::select(-opp_win, -opp_home, -opp_div_game) %>% 
  dplyr::relocate(season, week, game_id, team, opponent, win, win_pct, home_win_pct, away_win_pct, home, div_game)

saveRDS(final_lag_data, here::here("data", "football_wins_lag.rds"))

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