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

# *******************
# ---- Load Data ----
# *******************

# Team records/schedule
team_records      <-  readRDS(here::here("data", "team_records.rds"))

# Weekly Team defense
team_defense      <- readRDS(here::here("data", "team_defense.rds"))

# Weekly Team Fantasy points allowed by position
fp_against        <- readRDS(here::here("data", "team_fp_against.rds"))

# Weekly player stats
player_stats      <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Cumaltive fantasy points average per player and opponent defense
fp_df             <- readRDS(here::here("data", "fp_model_data.rds"))

#
football          <- readRDS(here::here("data", "football_wins.rds"))

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