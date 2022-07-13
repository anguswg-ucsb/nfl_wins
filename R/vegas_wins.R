
rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(timetk)

source("utils/utils.R")

data_path  <-  here::here("data")

# Snap % # Data dictionary
desc <- nflfastR::field_descriptions

# unique seasons
seasons_lst <- 1999:2021

# ******************
# ---- Team pbp ----
# ******************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
pbp_stats <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  pbp <- nflfastR::load_pbp(x)
}
) 
pbp_df <- dplyr::bind_rows(pbp_stats)

spread <-
  pbp_df %>% 
  dplyr::select(game_id, home_team, away_team, week, season, home_score, away_score, spread_line) %>% 
  tidyr::pivot_longer(
    cols      = c(home_team, away_team),
    names_to  = "home_away",
    values_to = "team"
  ) %>% 
  dplyr::group_by(season, week, game_id, spread_line) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

# **********************
# ---- Team records ----
# **********************

team_records <- readRDS(here::here("data", "wins.rds")) %>% 
  dplyr::select(season, week, game_id, team, home_away, win)

results <- 
  team_records %>% 
  dplyr::left_join(
    dplyr::select(spread, game_id, spread_line),
    by = "game_id"
  ) %>% 
  dplyr::mutate(
    spread_wins = dplyr::case_when(
      home_away == "away_team" & spread_line <= 0 & win == 1 ~ "TP",
      home_away == "away_team" & spread_line <= 0 & win == 0 ~ "FP",
      home_away == "away_team" & spread_line > 0 & win == 1 ~ "FN",
      home_away == "away_team" & spread_line > 0 & win == 0 ~ "TN",
      home_away == "home_team" & spread_line >= 0 & win == 1 ~ "TP",
      home_away == "home_team" & spread_line >= 0 & win == 0 ~ "FP",
      home_away == "home_team" & spread_line < 0 & win == 1 ~ "FN",
      home_away == "home_team" & spread_line < 0 & win == 0 ~ "TN"
    )
  ) %>% 
  dplyr::group_by(season, spread_wins) %>%
  dplyr::summarise(
    id = n()
  ) %>% 
  na.omit() %>% 
  tidyr::pivot_wider(
    names_from  = spread_wins, 
    values_from = id
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    season = as.character(season),
    accuracy = (TP + TN)/(TP+TN+FP+FN)
  ) %>% 
  dplyr::arrange(season, week) %>% 
  dplyr::mutate(rownum = 1:n()) %>% 
  dplyr::filter(season >= 2011, season <= 2014)

ggplot() +
  geom_line(data = results, aes(x = week, y = accuracy, color = season))

results
results %>% 
  count(spread_wins) %>% 
  na.omit() %>% 
  tidyr::pivot_wider(
    names_from  = spread_wins, 
    values_from = n
    ) %>% 
  dplyr::mutate(
    accuracy = (TP + TN)/(TP+TN+FP+FN)
  )
# *****************************
# ---- Wins vs. spreadline ----
# *****************************

team_records %>% 
  dplyr::select(season, week, game_id, team, home_away, win)


# pull rosters for every year
team_records <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Season PBP: {x}")
  
  schedule <- nflfastR::fast_scraper_schedules(2020) %>%
    get_schedule()
}
) %>%
  dplyr::bind_rows()

saveRDS(team_records, here::here("data", "wins.rds"))
# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
pbp_stats <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  pbp <- nflfastR::load_pbp(x)
}
) 

# pbp_df <- dplyr::bind_rows(pbp_stats)









