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
library(elo)

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

# **************************
# ---- Get model inputs ----
# **************************

# scrapes empty schedule for upcoming season (2021)
scrape_schedule <- function(year, pred_week) {
 # pred_week <- 8
 
 # week <- 18
 # year <- 2015
 
 if (year >= 2021) {

   
   weeks <- 1:18
   
 } else if(year >= 2002 & year <= 2020) {

   
   weeks <- 1:17
   
 } else {

   year  <- 2002
   
   weeks <- 1:17
   
 }

  urls <- sapply(weeks, USE.NAMES = TRUE, simplify = TRUE, function(x) {
    paste0("https://www.espn.com/nfl/schedule/_/week/", weeks[x],"/year/", year, "/seasontype/2")
  }) %>% 
    tibble::tibble() %>% 
    dplyr::mutate(week = dplyr::row_number()) %>% 
    stats::setNames(c("url", "week"))
    
 week_lst <- list()
 
 for (z in 1:nrow(urls)) {
   

   # Read HTML page using URL
   page <- rvest::read_html(urls$url[z])
    
   # Extract HTML nodes for table
   page_nodes <- 
     page %>%  
     rvest::html_nodes("table")
  
    tbl_lst <- list()
    
    for (i in 1:length(page_nodes)) {
      
      logger::log_info("table {i} of {length(page_nodes)}")
      
      # Extract season games 
      page_table <- rvest::html_table(
        page_nodes[[i]],
        header  = F,
        fill    = T, 
        convert = T
      ) 
      
      if (page_table[1, 1] == "BYE") {
        next
      } else{
        page_table <- 
          page_table %>% 
          dplyr::select(X1, X2) %>% 
          dplyr::filter(X1 != "matchup") %>% 
          stats::setNames(c("away_team", "home_team")) %>% 
          dplyr::mutate(
            id = 1:n()
          ) %>% 
          dplyr::group_by(id) %>% 
          dplyr::mutate(
            away_team = tail(strsplit(away_team, split = " ")[[1]], 1),
            home_team = tail(strsplit(home_team, split = " ")[[1]], 1),
            week      = urls$week[z]
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(-id)
        
        
        tbl_lst[[i]] <- page_table
      }
    }
    
   week_lst[[z]] <- dplyr::bind_rows(tbl_lst)
   
 }
 season_schedule <- dplyr::bind_rows(week_lst)
 
 return(season_schedule)
 
}

# **************************
# ---- Get model inputs ----
# **************************

# scrapes empty schedule for upcoming season (2021)
get_upcoming_game <- function(year, pred_week, post_season = FALSE) {
  
  if (year < 2021 & pred_week > 17) {
    
    pred_week <- 17
    
  } 
  

  
  # If game is a post season game
  if (post_season == TRUE) {
    
    url <- paste0("https://www.espn.com/nfl/schedule/_/week/", pred_week,"/year/", year, "/seasontype/3")
    
  } else {
    
    url <- paste0("https://www.espn.com/nfl/schedule/_/week/", pred_week,"/year/", year, "/seasontype/2")
  }
  
  logger::log_info("\n\nRetrieving matchups:\nSeason: {year}\nWeek: {pred_week}")
  
  # Read HTML page using URL
  page <- rvest::read_html(url)
  
  # Extract HTML nodes for table
  page_nodes <- 
      page %>%  
      rvest::html_nodes("table")
  
  # empty list to add to in loop
  tbl_lst <- list()

  for (i in 1:length(page_nodes)) {
    
    # logger::log_info("table {i} of {length(page_nodes)}")
    
    # Extract season games 
    page_table <- rvest::html_table(
        page_nodes[[i]],
        header  = F,
        fill    = T, 
        convert = T
      ) 
    
    # if a BYE week table, skip iteration
    if (page_table[1, 1] == "BYE") {
      
      next
      
      } else {
        
        page_table <- 
          page_table %>% 
          dplyr::select(X1, X2) %>% 
          dplyr::filter(X1 != "matchup") %>% 
          stats::setNames(c("away_team", "home_team")) %>% 
          dplyr::mutate(
            id = 1:n()
          ) %>% 
          dplyr::group_by(id) %>% 
          dplyr::mutate(
            season    = year,
            week      = pred_week,
            away_team = tail(strsplit(away_team, split = " ")[[1]], 1),
            home_team = tail(strsplit(home_team, split = " ")[[1]], 1),
            game_id   = dplyr::case_when(
              week < 10 ~ paste0(year, "_0", week, "_", away_team, "_",  home_team),
              week >= 10 ~ paste0(year, "_", week, "_", away_team, "_",  home_team)
            )
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(season, week, game_id, home_team, away_team)
        
        
        tbl_lst[[i]] <- page_table
      }
    }
    
  # Bind rows of list
    upcoming_games <- dplyr::bind_rows(tbl_lst)

    # Replace some team names to match names in  other data
    upcoming_games$home_team <- gsub("LAR", "LA", upcoming_games$home_team)
    upcoming_games$away_team <- gsub("LAR", "LA", upcoming_games$away_team)
    upcoming_games$home_team <- gsub("WSH", "WAS", upcoming_games$home_team)
    upcoming_games$away_team <- gsub("WSH", "WAS", upcoming_games$away_team)
    
    return(upcoming_games)
  
}

# **************************
# ---- Get model inputs ----
# **************************

# scrapes empty schedule for upcoming season (2021)
scrape_games <- function(year, pred_week) {

  # Check and make sure pred_week is a valid week of season
  if(pred_week < 2) {
    
    # setting week to 2 if pred_week is less than 2
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - upcoming week\nSetting pred_week = 2")
    
    pred_week = 2
    
  } else if(year >= 2021 & pred_week > 18) {
    
    # setting week to max week after 2020 season (18)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - 18\nSetting pred_week = 18")
    
    pred_week = 18
    
  } else if(year < 2021 & pred_week > 17) {
    
    # setting week to max week before 2021 season (17)
    logger::log_info("\n\nWeek {pred_week} invalid\nWeek must be within valid week range: 2 - 17\nSetting pred_week = 17")
    
    pred_week = 17
    
  }
  # if(pred_week < 2) {
  #   
  #   logger::log_info("Please enter a week between week 2 and the Superbowl")
  #   
  #   pred_week = 2
  #   
  # } else if(year < 2021 & pred_week > 21) {
  #   
  #   logger::log_info("Please enter a week between 2-21")
  #   
  #   pred_week = 21
  #   
  # } else if(year >= 2021 & pred_week > 22) {
  #   
  #   logger::log_info("Please enter a week between 2-22")
  #   
  #   pred_week = 22
  #   
  # }
  
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
  
  # Rename playoff columns as weeks, accounting for added game after 2020 season
  if (year >= 2021) {
    
    # If season is after 2020
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
    
    # Rename playoff columns as weeks, accounting for fewer games before 2021
  } else {
    
    # if season is before 2021
    page_table <- 
      page_table %>% 
      dplyr::mutate(
        week = dplyr::case_when(
          week == "WildCard"  ~ "18",
          week == "Division"  ~ "19",
          week == "ConfChamp" ~ "20",
          week == "SuperBowl" ~ "21",
          TRUE                ~ week
        ),
        week     = as.numeric(week)
      ) %>% 
      dplyr::filter(week < pred_week)
  }

  # parse data tables from Pro Football Reference
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
  record <- get_schedule(outcomes, verbose = FALSE)
  
  # Create Score differential, home or away team ID, # of rest days
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
      home_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,2],
      away_team     =  stringr::str_split_fixed(split_game_id, "_", 2)[,1],
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
    dplyr::select(-lag_date, -gameday, -home) %>% 
    replace(is.na(.), 7) %>% 
    dplyr::relocate(season, week, game_id, team, opponent, home_away, rest_days, score_diff, home_score, away_score) %>% 
    dplyr::left_join(
      dplyr::select(record, week, game_id, team, win, win_pct, home_win_pct, away_win_pct),
      by = c("week", "game_id", "team")
    )
  
  # Calculate ELO ratings
  elo <-  
    outcomes %>% 
    dplyr::filter(home_away == "home_team") %>%
    dplyr::select(season, week, game_id, team, opponent, win, home_score, away_score) %>% 
    get_nfl_elo()
  
  # Join ELO ratings back w/ outcomes
  outcomes <- 
    outcomes %>% 
    dplyr::left_join(
      dplyr::select(elo, week, game_id, team, elo),
      by = c("week", "game_id", "team")
    ) %>%
    dplyr::group_by(team) %>% 
    dplyr::arrange(week, .by_group = TRUE) %>% 
    dplyr::mutate(
      turnovers = dplyr::case_when(
        home_away == "home_team" ~ as.numeric(home_turnovers),
        home_away == "away_team" ~ as.numeric(away_turnovers)
      ),
      turnovers    = mean(turnovers, na.rm = T),
      score_diff   = mean(score_diff, na.rm = T),
      elo          = mean(elo, na.rm = T)
    ) %>% 
    dplyr::slice(which.max(week)) %>% 
    dplyr::select(season, week, game_id, team, opponent, home_away, win, win_pct, home_win_pct, away_win_pct, rest_days, score_diff, turnovers, elo)  %>% 
    dplyr::mutate(across(c(win_pct:away_win_pct), round, 4)) %>% 
    dplyr::ungroup()
  
  
  # Get schedule of upcoming games
  next_game <- get_upcoming_game(
    year        = year,
    pred_week   = pred_week,
    post_season = FALSE
  ) 
  
  # Upcoming home team stats
  home <- 
    next_game %>% 
    dplyr::left_join(
      dplyr::select(outcomes, -season, -week, -game_id, -opponent, -home_away, -win),
      by = c("home_team" = "team")
    ) 
  
  # Upcoming away team stats
  away <-
    outcomes %>% 
    dplyr::filter(team %in% home$away_team) %>% 
    dplyr::select(-season, -week, -game_id, -opponent, -home_away, -win) %>% 
    stats::setNames(
      c("away_team", 
        paste0("opp_", names(.)[names(.) != "team"])
        )
      )
  
  # Join Home team and away team stats leading up to upcoming game, used as input into models
  matchups <-
    home %>% 
    dplyr::left_join(
      away,
      by = c("away_team")
    ) %>% 
    dplyr::relocate(season, week, game_id, team = home_team, opponent = away_team)
  
  return(matchups)
  
}

game_data <- scrape_games(
  year      = 2021, 
  pred_week = 8
  )

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