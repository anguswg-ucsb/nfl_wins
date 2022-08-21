# Angus Watters 
# Aggregate and clean data for predicting spread 

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(timetk)

source("utils/utils.R")
raw_url <- "https://site.api.espn.com/apis/v2/sports/football/nfl/standings"

request <- httr::GET(
  raw_url,
  query = list(
    season = 2022
  )
)
library(tidyverse)
resp <- httr::content(request, as = "text", encoding = "UTF-8")
raw_standings <- jsonlite::parse_json(resp)

names_fix <- tibble(
  type = c(
    "playoffseed",
    "wins",
    "losses",
    "winpercent",
    "gamesbehind",
    "ties",
    "pointsfor",
    "pointsagainst",
    "differential",
    "streak",
    "clincher",
    "divisionrecord",
    "divisionwins",
    "divisionties",
    "divisionlosses",
    "total",
    "home",
    "road",
    "vsdiv",
    "vsconf"
  ),
  abb = c(
    "seed",
    "wins",
    "losses",
    "win_pct",
    "g_behind",
    "ties",
    "pts_for",
    "pts_against",
    "pts_diff",
    "streak",
    "div_clincher",
    "record_div",
    "div_wins",
    "div_ties",
    "div_losses",
    "record",
    "record_home",
    "record_away",
    "record_div",
    "record_conf"
  )
)

full_stand <- raw_standings[["children"]] %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  select(conf = abbreviation, standings) %>%
  unnest_wider(standings) %>%
  unnest_longer(entries) %>%
  unnest_wider(entries) %>%
  select(conf, season, team, stats) %>%
  unnest_wider(team) %>%
  hoist(logos, team_logo = list(1, "href")) %>%
  select(
    conf,
    season,
    team_id = id,
    team_location = location,
    team_name = name,
    team_abb = abbreviation,
    team_full = displayName,
    team_logo,
    stats
  ) %>%
  unnest_longer(stats) %>%
  unnest_wider(stats) %>%
  mutate(value = as.character(value)) %>%
  mutate(value = if_else(is.na(value), displayValue, value)) %>%
  select(conf:team_logo, type, value) %>%
  left_join(names_fix, by = "type") %>%
  filter(!(abb == "record_div" & str_length(value) < 2)) %>%
  filter(!is.na(abb), abb != "NA") %>%
  filter(abb != "div_clincher") %>%
  select(-type) %>%
  pivot_wider(
    names_from = abb,
    values_from = value,
    id_cols = conf:team_logo
  ) %>%
  separate(record_home, c("home_wins", "home_losses"), convert = TRUE, extra = "drop") %>%
  separate(record_away, c("away_wins", "away_losses"), convert = TRUE, extra = "drop") %>%
  separate(record_div, c("div_wins", "div_losses"), convert = TRUE, extra = "drop") %>%
  separate(record_conf, c("conf_wins", "conf_losses"), convert = TRUE, extra = "drop") %>%
  mutate(across(c(seed:div_losses, -record), as.double)) %>%
  group_by(conf) %>%
  arrange(conf, desc(win_pct), g_behind) %>%
  mutate(seed = row_number()) %>%
  ungroup()

full_stand
full_stand <- raw_standings[["children"]] %>%
  tibble::tibble(data = .)
stop(
  sprintf(
    "ESPN API request failed [%s]\n%s\n<%s>",
    httr::status_code(resp),
    parsed$message,
    parsed$documentation_url
  ),
  call. = FALSE
)
data_path  <-  here::here("data")
remotes::install_github("jthomasmock/espnscrapeR")
nflreadr::load_pbp(2022)
library(espnscrapeR)
sched <- get_nfl_standings(2021)
sched <- espnscrapeR::get_nfl_schedule(2021)
sched2 <- sched %>% 
  dplyr::filter(season == 2021)
bs_lst <- list()
for (i in 1:nrow(sched2)) {
  logger::log_info("{i}")
  bs <- espnscrapeR::get_nfl_boxscore(sched2$game_id[i])
  bs_lst[[i]] <- bs
}
tm <- espnscrapeR::get_nfl_boxscore(sched2$game_id)
dict <- nflreadr::dictionary_pbp()
# ********************
# ---- NFL Fast R ----
# ********************

# Snap % # Data dictionary
desc <- nflfastR::field_descriptions

# unique seasons
seasons_lst <- 1999:2021

# ***************************
# ---- Play by play data ----
# ***************************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
pbp_stats <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  pbp <- nflfastR::load_pbp(x)
}
) 

# pbp_df <- dplyr::bind_rows(pbp_stats)

# **********************
# ---- Team records ----
# **********************

# pull rosters for every year
game_spreads <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Season PBP: {x}")
  
  schedule <- nflfastR::fast_scraper_schedules(2020) %>%
    get_spread()
}
) %>%
  dplyr::bind_rows()

saveRDS(game_spreads, here::here("data", "game_spreads.rds"))

# *****************************
# ---- Closing vegas lines ----
# *****************************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
closing_lines <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  
  pbp <- nflfastR::load_pbp(2020) %>% 
    get_closing_line()
}
) %>%
  dplyr::bind_rows()

saveRDS(closing_lines, here::here("data", "closing_spread_lines.rds"))

# ******************************
# ---- Join Offense/Defense ----
# ******************************

# rm(off_df)
# Offense
off_df       <- readRDS(here::here("data", "offensive.rds"))

# Defense
def_df       <- readRDS(here::here("data", "defensive.rds"))

# Team records
game_spreads <- readRDS(here::here("data", "game_spreads.rds")) %>% 
  dplyr::mutate(
    pts_scored = dplyr::case_when(
      home_away == "home_team" ~ home_score,
      home_away == "away_team" ~ away_score
      )
    ) 

# Join offense, defense, records
off_def_data <- 
  game_spreads %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win_pct, home_win_pct, away_win_pct, win, spread, pts_scored) %>% 
  dplyr::left_join(
    off_df,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    def_df,
    by = c("season", "week", "game_id", "team" = "defteam")
  )

saveRDS(off_def_data, here::here("data", "football_spread.rds"))

# *********************************
# ---- Season to date averages ----
# *********************************

# cumalative offense
lag_off      <- rolling_offense(off_df)

# cumalative defense
lag_def      <- rolling_defense(def_df)

# cumalative win %
lag_spread   <- rolling_spread(game_spreads)

closing_lines <- readRDS(here::here("data", "closing_spread_lines.rds"))

lag_data <- 
  lag_spread %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win, win_pct, home_win_pct, away_win_pct, spread, spread_lag, pts_scored) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    lag_def,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>% 
  dplyr::left_join(
    dplyr::select(closing_lines, -home_away),
    by = c("season", "week", "game_id", "team")
  )

names(opp_lag_data)

opp_lag_data <- 
  lag_spread %>% 
  dplyr::select(season, week, game_id, team, opponent, rest_days, win, win_pct, home_win_pct, away_win_pct, spread, spread_lag) %>% 
  dplyr::left_join(
    lag_off,
    by = c("season", "week", "game_id", "team" = "posteam")
  ) %>% 
  dplyr::left_join(
    lag_def,
    by = c("season", "week", "game_id", "team" = "defteam")
  ) %>%
  setNames(c("season", "week", "game_id", "team", "opponent", paste0("opp_", names(.)[6:45]))) %>% 
  dplyr::select(-team)

final_spread_lag <- 
  lag_data %>% 
  dplyr::left_join(
    # dplyr::select(opp_lag_data, -opponent),
    opp_lag_data,
    by = c("season", "week", "game_id", "team" = "opponent")
  ) %>% 
  dplyr::select(-opp_win, -opp_home, -opp_div_game) %>% 
  dplyr::mutate(
    div_game  = factor(div_game, levels = c(1, 0)),
    rest_days = dplyr::case_when(
      rest_days < 7   ~  "short_rest",
      rest_days == 7  ~  "normal_rest",
      rest_days > 7   ~  "long_rest"
    ),
    opp_rest_days = dplyr::case_when(
      opp_rest_days < 7   ~  "short_rest",
      opp_rest_days == 7  ~  "normal_rest",
      opp_rest_days > 7   ~  "long_rest"
    ),
    win = factor(win, levels = c(1, 0))
  ) %>% 
  dplyr::mutate(
    across((contains("dscore")), ~replace(., is.na(.), 0))
  ) %>% 
  dplyr::relocate(season, week, game_id, team, opponent, spread_line, spread, spread_lag, win_pct, home_win_pct, away_win_pct, home, div_game) %>% 
  dplyr::select(-win) %>% 
  dplyr::mutate(
    pts_for         = qtr_pts_1 + qtr_pts_2 + qtr_pts_3 + qtr_pts_4,
    pts_against     = def_qtr_pts_1 + def_qtr_pts_2 + def_qtr_pts_3 + def_qtr_pts_4,
    point_diff      = pts_for - pts_against,
    opp_pts_for     = opp_qtr_pts_1 + opp_qtr_pts_2 + opp_qtr_pts_3 + opp_qtr_pts_4,
    opp_pts_against = opp_def_qtr_pts_1 + opp_def_qtr_pts_2 + opp_def_qtr_pts_3 + opp_def_qtr_pts_4,
    opp_point_diff  = opp_pts_for - opp_pts_against
  ) 

saveRDS(final_spread_lag, here::here("data", "football_closing_spread_lag.rds"))

# bits_to_dec <- function(bitty) {
#   bitty <- 15
#   # Step 1 divide integer by 2, track quotient and remainder
#   quot  <- 15/2 
#   rem   <- 15 %% 2
#   
#   quot2 <- quot/2
#   rem2  <- quot %% 2
# }