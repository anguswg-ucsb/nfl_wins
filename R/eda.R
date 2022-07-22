# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(httr)
library(jsonlite)
library(rvest)
library(corrplot)
library(plotly)
library(viridis)

source("utils/utils.R")

data_path  <-  here::here("data")

# *******************
# ---- Load Data ----
# *******************

# Team records/schedule
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
# 
# # Cumaltive fantasy points average per player and opponent defense
# fp_df             <- readRDS(here::here("data", "fp_model_data.rds"))

# Read in joined offense defense data
# football     <- readRDS(here::here("data", "football_wins.rds"))
# football     <- readRDS(here::here("data", "football_wins2.rds"))
football     <- readRDS(here::here("data", "football_wins.rds"))
win_lag      <- readRDS(here::here("data", "football_wins_lag.rds"))
game_spreads <- readRDS(here::here("data", "football_spread.rds"))
spreads_lag  <- readRDS(here::here("data", "football_spread_lag.rds"))
offense      <- readRDS(here::here("data", "offensive.rds"))
defense      <- readRDS(here::here("data", "defensive.rds"))

# model_dat <- readRDS(here::here("data", "football_wins_lag_elo.rds"))
model_dat_lag <- readRDS(here::here("data", "football_wins_lag_elo.rds")) %>% 
  # dplyr::filter(season != 2021, home == 1) %>% 
  # dplyr::filter(home == 1) %>% 
  dplyr::select(-abs_spread_line, -home_fav, -fav, -spread_line) %>% 
  dplyr::select(season, week, game_id, team, opponent, win, home, div_game, rest_days, 
                opp_rest_days, elo, opp_elo, score_diff, 
                opp_score_diff, turnovers, opp_turnovers,
                # score_diff_qtr_1, opp_score_diff_qtr_1,
                win_pct, away_win_pct, home_win_pct, 
                opp_win_pct, opp_away_win_pct, opp_home_win_pct, qb_epa, opp_qb_epa, 
                third_down_pct, opp_third_down_pct,
                score_drives, opp_score_drives) 

model_dat <- 
  football %>%
  dplyr::select(names(model_dat_lag)) %>% 
  # dplyr::filter(home == 1) %>% 
  dplyr::select(-home)

# Add team colors for plotting
football <- 
  football %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  )

team_colors       <- nflfastR::teams_colors_logos


# ***************************
# ---- Season win totals ----
# ***************************

win_total <- 
  football %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    total_wins = sum(win, na.rm = T)
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(
    mean_wins = mean(total_wins, na.rm = T)
  )  %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) %>% 
  dplyr::arrange(mean_wins)

# ggplot() +
  # geom_point(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins, color = team_color3))
team_win_totals_plot <- 
  ggplot() +
  geom_boxplot(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins, fill = team_nick, color = team_nick)) +
  scale_fill_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color)
    ) +
  scale_color_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color2)
  ) +
  labs(
    title = "Which teams win the most games?", 
    subtitle = "Season Win Totals (1999 - 2021)",
    x = "Team", 
    y = "Season Win Totals"
    # fill = ""
  ) + 
  apatheme +
  # theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = -45)
  )
team_win_totals_plot
ggsave(
  here::here("img", "team_win_totals.png"),
  team_win_totals_plot,
  width = 12,
  height = 8
  )
# ***************************
# ---- Season win totals ----
# ***************************

win_total <- 
  football %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    total_wins = sum(win, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(
    mean_wins = mean(total_wins, na.rm = T)
  )  %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) %>% 
  dplyr::arrange(mean_wins)

# ggplot() +
# geom_point(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins, color = team_color3))
ggplot() +
  geom_boxplot(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins, fill = team_nick, color = team_nick)) +
  scale_fill_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color)
  ) +
  scale_color_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color2)
  )

# ************************
# ---- scoring drives ----
# ************************


scoring_drives <- 
  football %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    score_drives_pct =  mean(score_drives_pct, na.rm = T)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(
    mean_score_drives_pct = mean(score_drives_pct, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>%  
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) %>% 
  dplyr::arrange(mean_score_drives_pct) 

ggplot() +
  geom_boxplot(data = scoring_drives, aes(x = reorder(team,mean_score_drives_pct ), y = score_drives_pct,
                                          fill = team_nick, color = team_nick)) +
  labs(
    title = "What percentage of drives does an NFL team score points ?",
    x = "Team",
    y = "Percent of Drives resulting in points"
  ) + 
  scale_fill_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color)
  ) +
  scale_color_manual(
    breaks = win_total$team_nick,
    values = c(win_total$team_color2)
  ) +
  theme_bw()
nflfastR::teams_colors_logos

# *******************
# ---- 3rd Downs ----
# *******************


third_downs <- 
  football %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise(
    third_down_pct = mean(third_down_pct, na.rm = T)
  ) %>%
  dplyr::ungroup() 

ggplot() +
  geom_boxplot(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins))

# ****************
# ---- QB EPA ----
# ****************
rm(qb_epa)
epa <- 
  football %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    qb_epa = mean(qb_epa, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(
    mean_qb_epa = mean(qb_epa, na.rm = T)
  )  %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) %>% 
  dplyr::arrange(mean_qb_epa) %>% 
  dplyr::ungroup()

# ggplot() +
# geom_point(data = win_total, aes(x = reorder(team, mean_wins), y = total_wins, color = team_color3))
qb_epa_plot <-
  ggplot() +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_boxplot(data = epa,
               aes(x = reorder(team, mean_qb_epa), 
                   y = qb_epa, 
                   fill = team_nick, color = team_nick)) +
  scale_fill_manual(
    breaks = epa$team_nick,
    values = c(epa$team_color)
  ) +
  scale_color_manual(
    breaks = epa$team_nick,
    values = c(epa$team_color2)
  ) +
  labs(
    title    = "Which teams have the best quarterbacks?", 
    subtitle = "Average QB EPA per season (1999 - 2021)",
    x        = "Team", 
    y        = "Quarterback EPA"
    # fill = ""
  ) + 
  apatheme +
  # theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = -45)
  )
qb_epa_plot
ggsave(
  here::here("img", "qb_epa.png"),
  qb_epa_plot,
  width = 12,
  height = 8
)
# ************************************
# ---- Win Correlation w/ Offense ----
# ************************************
# library(corrr)
# x <-   
#   football %>% 
#   dplyr::select(win, home, div_game,qtr_pts_1:turnovers,top_pct, score_drives_pct) %>%
#   correlate() %>%    # Create correlation data frame (cor_df)
#   focus(-win, mirror = F) %>%  # Focus on cor_df without 'cyl' and 'vs'
#   rearrange() %>%  # rearrange by correlations
#   shave() 
# rplot(x)
names(win_lag)[c(15:18, 20:34)]
off_cor <-
  # football %>%
  win_lag %>%
  dplyr::mutate(win = as.numeric(win)) %>%
  # dplyr::select(win, names(win_lag)[c(15:18, 20:34)]) %>% 
  dplyr::select(where(is.numeric)) %>%
  # dplyr::select(pts_for, pts_against, point_diff) %>% 
  dplyr::select( -season, -contains("opp")) %>%
  # na.omit() %>%
  # dplyr::select(win, home, div_game,qtr_pts_1:turnovers,top_pct, score_drives, score_drives_pct) %>% 
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt()

off_cor_plot <- 
  ggplot(data = off_cor, aes(x=Var1, y=Var2,
                           fill=value)) +
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value),
          color = "black", size = 4) +
  scale_fill_gradient2(low="darkred", high="darkgreen", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "F") +
  labs(
    title = "Correlation matrix for NFL Offenses",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme + 
  theme(axis.text.x = element_text(angle = -45))
off_cor_plot
ggsave(
  here::here("img", "correlation_offense.png"),
  off_cor_plot,
  width = 12,
  height = 8
)
  # viridis::scale_fill_viridis(direction = -1, option = "H")

# ************************************
# ---- Win Correlation w/ Defense ----
# ************************************
def_cor <-
  football %>% 
  # na.omit() %>%
  dplyr::select(win, home, div_game,def_qtr_pts_1:def_turnovers, def_score_drives_pct) %>% 
  # dplyr::select(win, home, div_game, contains("def")) %>% 
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt()

def_cor_plot <- 
  ggplot(
    data = def_cor, aes(
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
  scale_fill_gradient2(low="darkred", high="darkgreen", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "D") +
  labs(
    title = "Correlation matrix for NFL Defenses",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme
def_cor_plot

ggsave(
  here::here("img", "correlation_defense.png"),
  def_cor_plot,
  width = 12,
  height = 8
)
# viridis::scale_fill_viridis(direction = -1, option = "H")

# ***************************************
# ---- Spread Correlation w/ offense ----
# ***************************************

spread_cor <-
  game_spreads %>% 
  # dplyr::filter(home == 1) %>%
  # dplyr::filter(week >= 5, week <= 17) %>%
  # na.omit() %>%
  dplyr::select(spread, div_game, qtr_pts_1:qtr_pts_4, qb_epa, turnovers, top_pct, score_drives_pct) %>%
  # dplyr::select(spread, home, div_game, qtr_pts_1:qtr_pts_4, qb_epa, turnovers, top_pct, score_drives_pct) %>%
  # dplyr::select(win, home, div_game, contains("def")) %>% 
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt()

spread_cor_plot <-
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
  scale_fill_gradient2(low="darkred", high="midnightblue", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "D") +
  labs(
    # title = "Relationship between Spread and Offensive performance",
    title = "Correlations with End-of-Game score differences",
    # subtitle = "Spread and Offensive performance",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme +
  theme(axis.text.x = element_text(angle = -45))
  
spread_cor_plot

ggsave(
  here::here("img", "correlation_offense_spread.png"),
  spread_cor_plot,
  width = 12,
  height = 8
)
# *********************************
# ---- Spread lag Correlations ----
# *********************************

spread_lag_cor <-
  spreads_lag %>% 
  # na.omit() %>%
  dplyr::select(spread, spread_lag, win_pct, home, qtr_pts_1:qb_epa, turnovers, top_pct, score_drives_pct) %>% 
  # dplyr::select(win, home, div_game, contains("def")) %>% 
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt()

# spread_lag_cor_plot <-
  ggplot(
    data = spread_lag_cor, aes(
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
  scale_fill_gradient2(low="darkred", high="midnightblue", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "D") +
  labs(
    # title = "Relationship between Spread and Offensive performance",
    title = "Correlations with End-of-Game score differences",
    # subtitle = "Spread and Offensive performance",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme +
  theme(axis.text.x = element_text(angle = -45))

# spread_cor_plot

# ggsave(
#   here::here("img", "correlation_offense_spread.png"),
#   spread_cor_plot,
#   width = 12,
#   height = 8
# )
# *********************
# ---- ELO Ratings ----
# *********************
  
nfl_elo <-  
  football %>% 
  dplyr::select(season, week, team, elo) %>% 
  dplyr::group_by(team) %>% 
  dplyr::mutate(
    mean_elo = mean(elo, na.rm = T)
  ) %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) %>% 
    dplyr::arrange(mean_elo) %>%
    dplyr::ungroup()

  nfl_elo_plot <- 
    nfl_elo %>% 
    ggplot() +
    geom_boxplot(aes(x = reorder(team, mean_elo), 
                     y = elo, 
                     fill = team_nick, color = team_nick)) +
    scale_fill_manual(
      breaks = nfl_elo$team_nick,
      values = c(nfl_elo$team_color)
    ) +
    scale_color_manual(
      breaks = nfl_elo$team_nick,
      values = c(nfl_elo$team_color2)
    ) +
    labs(
      title    = "Which teams are consistently ranked at the top of the league?", 
      subtitle = "Team Elo Ratings (1999 - 2021)",
      x        = "Team", 
      y        = "Weekly ELO rating"
      # fill = ""
    ) + 
    apatheme +
    # theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -45, hjust = .1)
    )
  
ggsave(
    here::here("img", "team_elo_ratings.png"),
    nfl_elo_plot,
    width = 12,
    height = 8
  )

top_season_elo <-
  football %>% 
  dplyr::select(season, week, team, elo) %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarize(
    elo = mean(elo, na.rm = T)
  ) %>% 
    dplyr::group_by(season, team) %>%
    dplyr::arrange(-elo, .by_group = F) %>% 
    dplyr::ungroup() %>% 
    dplyr::slice(1:20) %>% 
    dplyr::mutate(
      team_season_full = paste0(season, " ", team),
      team_season = paste0(substr(season, 3, 4), " ", team)
      # team_season = paste0(team, " ", substr(season, 3, 4))
      # team_season = paste0(team, " ", season)
    ) %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) 

top_season_elo

top_season_elo_plot <- 
  top_season_elo %>% 
  ggplot(aes(x = reorder(team_season_full, elo),  y = elo, label = team_season)) +
  geom_point(aes(color = team_nick, label = team_season),  size = 4) +
  coord_flip() +
  # geom_text() +
  ggrepel::geom_text_repel(
    min.segment.length = unit(0, 'lines')
    ) +
  scale_color_manual(
    breaks = top_season_elo$team_nick,
    values = c(top_season_elo$team_color2)
  ) +
  scale_y_continuous(limits = c(min(top_season_elo$elo), max(top_season_elo$elo))) +
  labs(
    title    = "Top 20 highest ranked seasons", 
    subtitle = "Season Average Team Elo Ratings (1999 - 2021)",
    x        = "Team", 
    y        = "Weekly ELO rating",
  ) + 
  apatheme 

top_season_elo_plot

ggsave(
  here::here("img", "top_team_elo_ratings.png"),
  top_season_elo_plot,
  width = 12,
  height = 8
)

low_season_elo <-
  football %>% 
  dplyr::select(season, week, team, elo) %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarize(
    elo = mean(elo, na.rm = T)
  ) %>% 
  dplyr::group_by(season, team) %>%
  dplyr::arrange(elo, .by_group = F) %>% 
  dplyr::ungroup() %>% 
  dplyr::slice(1:20) %>% 
  dplyr::mutate(
    team_season_full = paste0(season, " ", team),
    team_season = paste0(substr(season, 3, 4), " ", team)
    # team_season = paste0(team, " ", substr(season, 3, 4))
    # team_season = paste0(team, " ", season)
  ) %>% 
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("team" = "team_abbr")
  ) 

low_season_elo

low_season_elo_plot <-
  low_season_elo %>% 
  ggplot(aes(x = reorder(team_season_full, elo),  y = elo, label = team_season)) +
  geom_point(aes(color = team_nick, label = team_season),  size = 4) +
  coord_flip() +
  # geom_text() +
  ggrepel::geom_text_repel(
    min.segment.length = unit(0, 'lines')
  ) +
  scale_color_manual(
    breaks = low_season_elo$team_nick,
    values = c(low_season_elo$team_color2)
  ) +
  scale_y_continuous(limits = c(min(low_season_elo$elo), max(low_season_elo$elo))) +
  labs(
    title    = "Bottom 20 lowest ranked seasons", 
    subtitle = "Season Average Team Elo Ratings (1999 - 2021)",
    x        = "Team", 
    y        = "Weekly ELO rating",
  ) + 
  apatheme 

low_season_elo_plot

ggsave(
  here::here("img", "bottom_team_elo_ratings.png"),
  low_season_elo_plot,
  width = 12,
  height = 8
)

team_elo <-
  nfl_elo %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise(elo = mean(elo, na.rm = T)) %>% 
  dplyr::arrange(-elo) %>% 
  dplyr::ungroup()

# *********************
# ---- Correlation ----
# ********************* 

corr_df <-
  # football %>%
  model_dat %>%
  dplyr::mutate(win = as.numeric(win)) %>%
  dplyr::select(where(is.numeric)) %>%
  # dplyr::select( -season, -week, -div_game) %>%
  dplyr::select( -season, -week, -div_game, -contains("opp")) %>%
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt() 

corr_plot <-
  corr_df %>% 
  ggplot(
    aes(x    = Var1,
        y    = Var2,
        fill = value)) +
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4) +
  scale_fill_gradient2(low="darkred", high="darkgreen", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "F") +
  labs(
    # title = "Home team correlation matrix",
    # title = "Home team - Correlation matrix",
    title = "Correlation matrix",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme + 
  theme(
    axis.text.x = element_text(angle = -45, 
                                   vjust = 0.5,
                                   hjust = 0.1)
    )
corr_plot

ggsave(
  here::here("img", "correlation_home_team.png"),
  corr_plot,
  width = 12,
  height = 8
)

opp_corr_df <-
  # football %>%
  model_dat %>%
  dplyr::mutate(win = as.numeric(win)) %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select( -season, -week, -div_game) %>%
  dplyr::select(win, contains("opp")) %>%
  # dplyr::select( -season, -week,  -contains("opp")) %>%
  cor(use =  "pairwise.complete.obs") %>% 
  round(2) %>% 
  reshape2::melt() 

opp_corr_plot <-
  opp_corr_df %>% 
  ggplot(
    aes(x    = Var1,
        y    = Var2,
        fill = value)) +
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4) +
  scale_fill_gradient2(low="darkred", high="darkgreen", guide="colorbar") +
  # viridis::scale_fill_viridis(direction = -1, option = "F") +
  labs(
    title = "Correlation matrix - Away team",
    x = "",
    y = "", 
    fill = "Coefficient"
  ) +
  apatheme + 
  theme(axis.text.x = element_text(angle = -45, 
                                   vjust = 0.5,
                                   hjust = 0.1
                                   ))

opp_corr_plot

ggsave(
  here::here("img", "correlation_away_team.png"),
  opp_corr_plot,
  width = 12,
  height = 8
)

library(patchwork)
all_corr_plot <- corr_plot + opp_corr_plot
all_corr_plot
ggsave(
  here::here("img", "correlation.png"),
  all_corr_plot,
  width = 14,
  height = 6
)
# ***************************
# ---- Season win totals ----
# ***************************

# season total wins
win_total  <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T))) %>% 
  dplyr::ungroup() 

# Average wins per season per team
team_wins <- 
  win_total %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise((across(where(is.numeric), mean, na.rm = T))) %>% 
  dplyr::mutate(
    win_pct            = (win+ tie)/(win+loss + tie),
    score_differential = max_score_diff - min_score_diff
    ) %>% 
  dplyr::ungroup() 


ggplot() +
  geom_point(data = team_wins, aes(x = reorder(team, win), y = win)) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 17)) +
  theme(
    axis.text = element_text(angle = -45)
  )

# Season averages
season_stats <- 
  player_stats %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(across(where(is.numeric), mean))

ggplot() +
  geom_boxplot(data = season_stats, aes(x = factor(season), y = passing_epa, outlier.shape = NA))

# ***********************************
# ---- Top teams @ each position ----
# ***********************************

fp_positions <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::group_by(season, team, position) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T)))

ggplot() +
  geom_boxplot(data = fp_positions, aes(x = reorder(team, fp_hppr), y = fp_hppr, fill = position)) +
  facet_wrap(~position) +
  labs(
    title = "How many fantasy points does each team produce?",
    x = "Teams",
    y = "Total QB Fantasy Points (0.5 PPR)"
  ) + 
  # scale_y_continuous(limits = c(0, 650)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45),
    axis.title  = element_text(face = "bold")
  )

# ***********************
# ---- Top teams EPA ----
# ***********************

epa_df <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), mean, na.rm = T))) %>% 
  dplyr::mutate(
    # total_epa = passing_epa + rushing_epa + receiving_epa
    total_epa =  rushing_epa + receiving_epa
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, team, week, win, total_epa, 
                # passing_epa,
                receiving_epa,
                rushing_epa) %>% 
  tidyr::pivot_longer(
    # cols      = c(passing_epa, rushing_epa, receiving_epa),
    cols      = c(rushing_epa, receiving_epa),
    names_to  = "epa_category",
    values_to = "epa"
    ) %>% 
  dplyr::mutate(
    epa_category = factor(epa_category, levels = c("rushing_epa", "receiving_epa"))
    # epa_category = factor(epa_category, levels = c("passing_epa", "rushing_epa", "receiving_epa"))
  )

ggplot() +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_boxplot(data = epa_df, aes(x = reorder(team, total_epa), y = epa, fill = epa_category)) +
  facet_wrap(~epa_category) +

  # coord_flip() +
  labs(
    title = "Who is best at running and catching the football?",
    x = "Teams",
    y = "Average EPA"
  ) + 
  scale_y_continuous(limits = c(-4, 4)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45),
    axis.title  = element_text(face = "bold")
  )


# ********************
# ---- RB FP Corr ----
# ********************

fantasy             <- readRDS(here::here("data", "fp_rollmean_data.rds"))

# Subset data to RBs and desired columns
fp_df  <- 
  fantasy %>% 
  dplyr::filter(position == "RB") %>% 
  dplyr::select(-fp_rank) %>% 
  dplyr::select(player_id:fp_finish, contains("roll_4")) %>% 
  setNames(c(gsub("_lag1|_roll_4", "", names(.)))) %>% 
  dplyr::select(player_id:fp_rank, touches:catch_rate, receiving_yards:receiving_fumbles_lost,
                receiving_yards_after_catch:receiving_ypt, opp_passing_yards:opp_rushing_yards, 
                opp_passing_tds:opp_rushing_epa, opp_comp_pct:opp_ypt, fp_hppr_rb)
# Corelation matrix
mcor <- cor(na.omit(fp_df[sapply(na.omit(fp_df),is.numeric)])) %>% 
  reshape2::melt() %>% 
  tibble::tibble() %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))

# Correlation heatmap
cor_plot <- 
  ggplot(mcor, aes(x = Var1,
                    y = Var2,
                    fill = value)) + 
  geom_tile() +
  theme(axis.text = element_text(angle = 45, vjust = 0.1, hjust = 1)) +
  viridis::scale_fill_viridis(discrete = FALSE, direction = 1)

cor_plot

plotly::ggplotly(cor_plot)


# **********************
# ---- Top RB teams ----
# **********************

rb_df <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(position == "RB") %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T)))

ggplot() +
  geom_boxplot(data = rb_df, aes(x = reorder(team, fp_hppr), y = fp_hppr), fill = "salmon") +
  labs(
    title = "How many RB fantasy points does each team produce?",
    x = "Teams",
    y = "Total RB Fantasy Points (0.5 PPR)"
  ) + 
  scale_y_continuous(limits = c(0, 650)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45),
    axis.title  = element_text(face = "bold")
  )

# **********************
# ---- Top QB teams ----
# **********************

qb_df <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(position == "QB") %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T)))

ggplot() +
  geom_boxplot(data = qb_df, aes(x = reorder(team, fp_hppr), y = fp_hppr), fill = "salmon") +
  labs(
    title = "How many QB fantasy points does each team produce?",
    x = "Teams",
    y = "Total QB Fantasy Points (0.5 PPR)"
  ) + 
  scale_y_continuous(limits = c(0, 650)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45),
    axis.title  = element_text(face = "bold")
  )

# *********************
# ---- Correlation ----
# *********************

# Corelation matrix
mcor <- cor(play_stats[sapply(play_stats,is.numeric)]) %>% 
  reshape2::melt() %>% 
  tibble::tibble() %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))

# Correlation heatmap
cor_plot <- 
  ggplot(mcor1, aes(x = Var1,
                    y = Var2,
                    fill = value)) + 
  geom_tile() +
  theme(axis.text = element_text(angle = 45, vjust = 0.1, hjust = 1)) +
  scale_fill_viridis(discrete = FALSE, direction = 1)

cor_plot

plotly::ggplotly(cor_plot)

# ******************************
# ---- Correlated variables ----
# ******************************

# Select most correlated variables
feat_select <- 
  cor(play_stats[sapply(play_stats,is.numeric)])  %>% 
  reshape2::melt() %>% 
  tibble::tibble() %>% 
  dplyr::mutate(across(where(is.numeric), round, 4)) %>% 
  dplyr::filter(Var2 == "win") %>% 
  dplyr::mutate(
    abs_corr = abs(value)
  ) 
# dplyr::filter(abs_corr >= 0.1)

saveRDS(feat_select, here::here("data", "nfl_feature_selection.rds"))

# ***************
# ---- Plots ----
# ***************

play_stats    <- readRDS(here::here("data", "nfl_wins.rds")) 
team_records  <-  readRDS(here::here("data", "team_records2.rds")) %>% 
  dplyr::filter(team != "")


# season total wins
win_total  <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T))) %>% 
  dplyr::ungroup() 

# Average wins per season per team
team_wins <- 
  win_total %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise((across(where(is.numeric), mean, na.rm = T))) %>% 
  dplyr::ungroup() 

ggplot() +
  geom_point(data = team_wins, aes(x = reorder(team, win), y = win))
# Season averages
season_stats <- 
  play_stats %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(across(where(is.numeric), mean))

ggplot() +
  geom_boxplot(data = play_stats, aes(x = factor(season), y = passing_epa, outlier.shape = NA))
# geom_point(data = season_stats, aes(x = season, y = passing_epa))
# *****************************
# ---- Stepwise regression ----
# *****************************

nfl_trim <- 
  play_stats %>% 
  dplyr::select(-week, -team, -season)

lm_fit <- lm(win~., data = nfl_trim)
broom::glance(lm_fit)
summary(lm_fit)
lm_ols <- olsrr::ols_step_forward_p(lm_fit)
lm_ols$model



head(data1)
palette = colorRampPalette(c("green", "white", "red"))(20)
corrplot(mcor)
ggplotly(heatmap(x = mcor, col = palette, symm = T))
# dplyr::left_join(
#   win_total, 
#   by = c("season", "team")
# )

plot(play_stats$turnovers~play_stats$win)

ggplot() +
  # geom_point(data = play_stats, aes(x = week, y = runs)) 
  geom_point(data = play_stats, aes(x = season, y = value, color = name))  +
  facet_wrap(~team)
# coord_flip()
# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(httr)
library(jsonlite)
library(rvest)
library(corrplot)
library(plotly)
library(viridis)
library(olsrr)

source("utils/utils.R")

data_path  <-  here::here("data")

# ********************
# ---- NFL Fast R ----
# ********************

# play_stats <- readRDS(here::here("data", "nfl_wins.rds"))

week_stats     <- readRDS(here::here("data", "weekly_player_team_record.rds"))
# QB stats
qb <- 
  football %>% 
  dplyr::filter(position == "QB") 

qb_stats <- 
  qb %>% 
  dplyr::group_by(team, season) %>% 
  dplyr::summarise(
    pass = sum(passing_yards, na.rm = T),
    run  = sum(rushing_yards, na.rm = T)
  ) %>% 
  dplyr::arrange(season) %>% 
  tidyr::pivot_longer(cols = c(run, pass)) %>% 
  dplyr::ungroup()

tmp <- 
  wl_stats %>% 
  dplyr::filter(position == "QB") %>% 
  dplyr::select(-team, -player_id, -player_name, -position) 
# dplyr::mutate(
#   turnover_ratio = (rushing_tds + passing_tds + receiving_tds)/(interceptions + rushing_fumbles_lost),
#   turnover_ratio2 = case_when(
#     is.nan(turnover_ratio ) ~ 1,
#     is.infinite(turnover_ratio) ~ 2,
#     TRUE ~ turnover_ratio
#   )
# )

lm <- lm(win~passing_epa                  , data = tmp)
summary(lm)
tmp_plot <- 
  tmp %>% 
  correlate() %>%    # Create correlation data frame (cor_df)
  # focus(-cyl, -vs, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  # rearrange() %>%  # rearrange by correlations
  # shave() %>% 
  rplot() + 
  theme(axis.text.x = element_text(angle = 270, vjust = .5))
plotly::ggplotly(tmp_plot)
library(corrr)
x <- datasets::mtcars %>%
  correlate() %>%    # Create correlation data frame (cor_df)
  focus(-cyl, -vs, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  rearrange() %>%  # rearrange by correlations
  shave()
rplot(x)