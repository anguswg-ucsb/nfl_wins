# Angus Watters 
# Scrape new data to use in real time predictions

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(httr)
library(jsonlite)
library(rvest)

source("utils/utils.R")

data_path  <-  here::here("data")

# *****************************
# ---- Get Season schedule ----
# *****************************

# scrapes empty schedule for upcoming season (2021)
scrape_empty_schedule <- function(year) {
  
  # Construct URL
  url  <- paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm")
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  
  # Extract HTML nodes for table
  page_nodes <- 
    page %>%  
    rvest::html_nodes("table")
  
  
  # Extract season games 
  page_table <- rvest::html_table(
    page_nodes[[1]],
    header  = T,
    fill    = T, 
    convert = F
  ) %>% 
    janitor::clean_names() %>% 
    stats::setNames(c("week", "day", "date", "away_team", "win_pts", "at", "home_team", "lose_pts", "time")) %>% 
    dplyr::select(-win_pts, -lose_pts, -time, -at) %>% 
    dplyr::mutate(
      date = lubridate::mdy(paste0(gsub(" ", "/", date), "/", year))
    ) 
  
  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]
  
  # Join team abbreviation w/ long names
  page_table <- 
    page_table %>%
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, home_team_abb = team_abb),
      by = c("home_team" = "team_name")
      ) %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, away_team_abb = team_abb),
      by = c("away_team" = "team_name")
    ) %>% 
    dplyr::select(week, day, date, home_team = home_team_abb, away_team = away_team_abb)
  
  return(page_table)
  
}
scr <- scrape_empty_schedule(2022)

saveRDS(scr, here::here("data", "empty_schedule."))

# **************************
# ---- Get model inputs ----
# **************************
# scrapes empty schedule for upcoming season (2021)
scrape_recent_games <- function(year, pred_week) {
  
  year <- 2021
  pred_week <- 6
  
  # Construct URL
  url  <- paste0("https://www.pro-football-reference.com/years/", year ,"/games.htm")
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  
  # Extract HTML nodes for table
  page_nodes <- 
    page %>%  
    rvest::html_nodes("table")
  
  
  # Extract season games 
  page_table <- rvest::html_table(
    page_nodes[[1]],
    header  = T,
    fill    = T, 
    convert = F
  ) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(
      home = case_when(
        x == ""  ~ 1,
        x == "@" ~ 0,
        x == "N" ~ 0
        )
      ) 
  
  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]

  # remove headers break for playoff start 
  page_table <- page_table[!grepl("Playoffs", page_table$date), ]
  
  page_table <- 
    page_table %>% 
    dplyr::mutate(
      week = dplyr::case_when(
        week == "WildCard"  ~ "19",
        week == "Division"  ~ "20",
        week == "ConfChamp" ~ "21",
        week == "SuperBowl" ~ "22",
        TRUE                ~ week
        ),
      week     = as.numeric(week)
      ) %>% 
    dplyr::filter(week < pred_week)
  
  outcomes <- 
    page_table %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, win_team_abb = team_abb),
      by = c("winner_tie" = "team_name")
    ) %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, lose_team_abb = team_abb),
      by = c("loser_tie" = "team_name")
    ) %>% 
    dplyr::select(week, date,
                  win_team  = win_team_abb,
                  x, 
                  lose_team = lose_team_abb,
                  pts_win   = pts,
                  pts_lose  = pts_2, 
                  tow, tol)  %>% 
    dplyr::mutate(
      home_team = dplyr::case_when(
        x == ""  ~ win_team,
        x == "@" ~ lose_team,
        week == 22 ~ win_team
        # week == "SuperBowl" ~ win_team
      ),
      away_team = dplyr::case_when(
        x == ""  ~ lose_team,
        x == "@" ~ win_team,
        week == 22 ~ lose_team
        # week == "SuperBowl" ~ lose_team
      ),
      pts_win  = as.numeric(pts_win),
      pts_lose = as.numeric(pts_lose),
      # week = dplyr::case_when(
      #   week == "WildCard"  ~ "19",
      #   week == "Division"  ~ "20",
      #   week == "ConfChamp" ~ "21",
      #   week == "SuperBowl" ~ "22",
      #   TRUE                ~ week
      # ),
      # week     = as.numeric(week),
      game_id  = dplyr::case_when(
        week < 10 ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
        week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
      )
    ) %>% 
    dplyr::select(-x) %>% 
    dplyr::group_by(game_id) %>% 
    dplyr::mutate(
      home_pts = dplyr::case_when(
        home_team == win_team ~ max(pts_win, pts_lose),
        home_team != win_team ~ min(pts_win, pts_lose)
      ),
      away_pts = dplyr::case_when(
        home_team == win_team ~ min(pts_win, pts_lose),
        home_team != win_team ~ max(pts_win, pts_lose)
      ),
      home_turnovers = dplyr::case_when(
        home_team == win_team ~ tow,
        home_team != win_team ~ tol
      ),
      away_turnovers = dplyr::case_when(
        home_team == win_team ~ tol,
        home_team != win_team ~ tow
      ),
      season = year
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(season, week, game_id, gameday = date, home_team, away_team, home_score = home_pts, away_score = away_pts, home_turnovers, away_turnovers) 

  # calculate win loss percentages
  record <- get_schedule(outcomes)
  
  outcomes <- 
    outcomes %>% 
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "home",
      values_to = "team"
    ) %>% 
    dplyr::mutate(
      home = dplyr::case_when(
        home == "home_team" ~ 1, 
        home == "away_team" ~ 0
      ),
      score_diff = dplyr::case_when(
        home == 1 ~ home_score - away_score, 
        home == 0 ~ away_score - home_score
      ),
      home_away  = case_when(
        home == 1 ~ "home_team",
        home == 0 ~ "away_team"
        )
      ) %>% 
    dplyr::mutate(
      split_game_id = substr(game_id, 9, 20)
    ) %>% 
    dplyr::mutate(
      home_team     =  str_split_fixed(split_game_id, "_", 2)[,2],
      away_team     =  str_split_fixed(split_game_id, "_", 2)[,1],
      opponent      =  dplyr::case_when(
        home_team == team ~ away_team,
        away_team == team ~ home_team
      )
    ) %>% 
    dplyr::select(-split_game_id, -home_team, -away_team) %>% 
    dplyr::group_by(team) %>% 
    dplyr::arrange(gameday, .by_group = T) %>% 
    dplyr::mutate(
      lag_date    = lag(gameday), 
      rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-lag_date, -gameday, -home_score, -away_score) %>% 
    replace(is.na(.), 7) %>% 
    dplyr::relocate(season, week, game_id, team, opponent, home_away, home, rest_days, score_diff) %>% 
    dplyr::left_join(
      dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
      by = c("week", "game_id", "team")
    )

  
  # outcomes <- 
  #   page_table %>% 
  #   dplyr::left_join(
  #     dplyr::select(nfl_teams(), team_name, win_team_abb = team_abb),
  #     by = c("winner_tie" = "team_name")
  #   ) %>% 
  #   dplyr::left_join(
  #     dplyr::select(nfl_teams(), team_name, lose_team_abb = team_abb),
  #     by = c("loser_tie" = "team_name")
  #   ) %>% 
  #   dplyr::select(week, date,
  #                 win_team  = win_team_abb,
  #                 x, 
  #                 lose_team = lose_team_abb,
  #                 pts_win   = pts,
  #                 pts_lose  = pts_2, 
  #                 tow, tol)  %>% 
  #   dplyr::mutate(
  #     home_team = dplyr::case_when(
  #       x == ""  ~ win_team,
  #       x == "@" ~ lose_team,
  #       week == "SuperBowl" ~ win_team
  #     ),
  #     away_team = dplyr::case_when(
  #       x == ""  ~ lose_team,
  #       x == "@" ~ win_team,
  #       week == "SuperBowl" ~ lose_team
  #     ),
  #     pts_win  = as.numeric(pts_win),
  #     pts_lose = as.numeric(pts_lose),
  #     game_id  = paste0(substr(date, 1, 4), "_", week, "_", home_team, "_",  away_team)
  #   ) %>% 
  #   dplyr::select(-x) %>% 
  #   dplyr::group_by(game_id) %>% 
  #   dplyr::mutate(
  #     home_pts = dplyr::case_when(
  #       home_team == win_team ~ max(pts_win, pts_lose),
  #       home_team != win_team ~ min(pts_win, pts_lose)
  #     ),
  #     away_pts = dplyr::case_when(
  #       home_team == win_team ~ min(pts_win, pts_lose),
  #       home_team != win_team ~ max(pts_win, pts_lose)
  #     ),
  #     turnovers      = dplyr::case_when(
  #       home_team == win_team ~ tow,
  #       home_team != win_team ~ tol
  #     ),
  #     away_turnovers = dplyr::case_when(
  #       home_team == win_team ~ tol,
  #       home_team != win_team ~ tow
  #     )
  #   ) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::select(game_id, week, date, team = home_team, opponent = away_team, home_pts, away_pts, turnovers, away_turnovers)  %>% 
    tidyr::pivot_longer(
      cols      = c(team, opponent),
      names_to  = "home",
      values_to = "team"
    ) %>% 
    dplyr::mutate(
      home = dplyr::case_when(
        home == "team"     ~ 1, 
        home == "opponent" ~ 0
      ),
      win = dplyr::case_when(
        home == 1 & home_pts > away_pts ~ 1, 
        home == 1 & home_pts < away_pts ~ 0, 
        home == 0 & home_pts < away_pts ~ 1, 
        home == 0 & home_pts > away_pts ~ 0
      )
    )
  
  # calculate number of rest days between games
  rest_days_df <- 
    outcomes2 %>% 
    dplyr::group_by(team) %>% 
    dplyr::arrange(gameday, .by_group = T) %>% 
    dplyr::mutate(
      lag_date    = lag(gameday), 
      rest_days   = as.numeric(round(difftime(gameday, lag_date), 0))
      ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-lag_date) %>% 
    replace(is.na(.), 7)
  
  # rest_days_df %>% 
  #   dplyr::mutate(rest_days = )
  
  # length(unique(rest_days_df$team))
  rest_days_df
  losers <- 
    page_table %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, lose_team_abb = team_abb),
      by = c("loser_tie" = "team_name")
    ) %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, win_team_abb = team_abb),
      by = c("winner_tie" = "team_name")
    ) %>% 
    dplyr::select(week, date, 
                  win_team  = win_team_abb,
                  lose_team = lose_team_abb,
                  x,
                  pts_win    = pts,
                  pts_lose   = pts_2, 
                  tol
                  )  %>% 
    dplyr::mutate(
      # home_away = "home"
      home = dplyr::case_when(
        x == ""  ~ 0,
        x == "@" ~ 1
      )
    ) %>% 
    dplyr::select(-x) 

  losers2 <- 
    losers %>% 
    dplyr::filter(home == 1) %>% 
    dplyr::mutate(
      team = dplyr::case_when(
        home == 1 ~ lose_team,
        TRUE ~ win_team
      )
    )

  
  tmp <- 
    winners %>% 
    dplyr::group_by(week, win_team) %>% 
    dplyr::mutate(
      pts_win  = as.numeric(pts_win),
      pts_lose = as.numeric(pts_lose),
      # home_pts = max(pts_win, pts_lose),
      # away_pts = min(pts_win, pts_lose)
      home_pts = dplyr::case_when(
        home == 1 ~ max(pts_win, pts_lose),
        home == 0 ~ min(pts_win, pts_lose)
      ),
      away_pts = dplyr::case_when(
        home == 1 ~ min(pts_win, pts_lose),
        home == 0 ~ max(pts_win, pts_lose)
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(home == 1) 
  
  tmp2 <- 
    tmp %>% 
    dplyr::left_join(
      dplyr::select(losers, week, date, win_team, lose_team, home2 = home),
      by = c("week", "date", "win_team")
    )
 
    # dplyr::filter(home == 1)
  
  tbl_long <- 
    page_table %>% 
    tidyr::pivot_longer(cols = c(winner_tie, loser_tie)) %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, team_abb),
      by = c("value" = "team_name")
    ) %>% 
    dplyr::mutate(
      home_away = dplyr::case_when(
        home == 1 & name == "winner_tie"           ~ "home",
        home == 1 & name == "loser_tie"            ~ "away",
        home == 0 & name == "winner_tie"           ~ "away",
        home == 0 & name == "loser_tie"            ~ "home",
        week == "SuperBowl" & name == "loser_tie"  ~ "home",
        week == "SuperBowl" & name == "winner_tie" ~ "away"
      )
    )
  
  # home team stats
  home_teams <- 
    tbl_long %>% 
    dplyr::filter(home_away == "home")
  
  # Away team stats
  away_teams <- 
    tbl_long %>% 
    dplyr::filter(home_away == "away")
  
  # tbl_wide <- 
  #   tbl_long %>% 
  #   tidyr::pivot_wider(
  #     id_cols     = c(-date, -value),
  #     names_from  = "home_away", 
  #     values_from = c(team_abb, name)
  #     ) %>% 
  #   dplyr::rename(
  #     home_team   = team_abb_home_team,
  #     away_team   = team_abb_away_team,
  #     home_result = name_home_team,
  #     away_result = name_away_team
  #     )
  names(tbl_wide)
  tbl_wide %>% 
    dplyr::select(week, date)
  
  page_table %>% 
    janitor::clean_names() %>% 
    stats::setNames(c("week", "day", "date", "away_team", "win_pts", "at", "home_team", "lose_pts", "time")) %>% 
    dplyr::select(-win_pts, -lose_pts, -time, -at) %>% 
    dplyr::mutate(
      date = lubridate::mdy(paste0(gsub(" ", "/", date), "/", year))
    ) 
  # page_table %>% 
  #   janitor::clean_names() %>% 
  #   dplyr::mutate(
  #     home = case_when(
  #       x == ""  ~ 1,
  #       x == "@" ~ 0
  #     )
  #   )
  # remove headers for each week
  page_table <- page_table[!grepl("Week", page_table$week), ]
  
  # Join team abbreviation w/ long names
  page_table <- 
    page_table %>%
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, home_team_abb = team_abb),
      by = c("home_team" = "team_name")
    ) %>% 
    dplyr::left_join(
      dplyr::select(nfl_teams(), team_name, away_team_abb = team_abb),
      by = c("away_team" = "team_name")
    ) %>% 
    dplyr::select(week, day, date, home_team = home_team_abb, away_team = away_team_abb)
  
  return(page_table)
  
}

# *****************************
# *****************************
scrape_pfr <- function(year) {
  
  logger::log_info("\n\nScraping Pro Football Reference...\n{year} Rushing Defense ")
  year <- 2020
  url  <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F", year, "%2Fopp.htm&div=div_rushing")
  
  page <- rvest::read_html(url)
  
  page_nodes <- 
    page %>%  
    rvest::html_nodes("table")
  
  page_table <- rvest::html_table(
    page_nodes[[1]],
    header  = T,
    fill    = T, 
    convert = T
  ) %>% 
    setNames(c("rank", "team", "games", "attempts", "rush_yards", 
               "rush_tds", "rush_yards_per_attempt", "rush_yards_per_game",
               "epa_rush_def")) %>% 
    dplyr::mutate(season = year) %>% 
    dplyr::filter(!team %in% c("League Total", "Avg Tm/G", "Avg Team")) %>% 
    dplyr::left_join(
      nfl_teams(),
      by = c("team" = "team_name")
    ) %>% 
    dplyr::select(team_abb, season, rank, rush_yards_per_attempt, rush_yards_per_game)
  
  return(page_table)
  
}