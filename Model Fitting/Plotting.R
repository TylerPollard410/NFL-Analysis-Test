# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# library(tidytext)
# library(MASS)
library(plotly)
library(patchwork)
# library(doParallel)
# library(rBayesianOptimization)
# library(xgboost)
# library(caret)
library(cmdstanr)
library(rstan)
library(brms)
library(posterior)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
#library(vip)
library(broom.mixed)
library(tidybayes)
#library(discrim)
#library(bayesian)
#library(timetk)
#library(modeltime)
#library(tidymodels)

library(nflverse)
library(tidyverse)

library(nflendzonePipeline)
library(nflendzone)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 1. DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Global Variables ----
all_seasons <- 2006:get_current_season()
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
github_data_repo <- "TylerPollard410/nflendzoneData"

## nflverse ----
teams_data <- load_teams(current = TRUE)
teams <- teams_data$team_abbr

### games ----
game_data <- load_game_data()
game_data_long <- game_data |> clean_homeaway(invert = c("result", "spread_line"))

game_id_keys <- game_data |> select(
  game_id, season, game_type, season_type, week, home_team, away_team, location
)
game_long_id_keys <- game_data_long |> select(
  game_id, season, game_type, season_type, week, team, opponent, location
)

### release data ----
tag <- "game_features"
game_features_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "game_model"
game_model_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "team_features"
team_features_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "team_model"
team_model_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "nfl_stats_week_team_regpost"
nfl_stats_week_team_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "nfl_stats_week_player_regpost"
nfl_stats_week_player_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "srs"
srs_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

## Set up modeling data ----
#   - Use seasons 2007–2023 for training/validation
#   - Hold out seasons 2024 for out of sample weekly forecats
game_model_data <- game_model_data |>
  mutate(
    hfa = ifelse(location == "Home", 1, 0)
  )

team_model_data <- team_model_data |>
  mutate(
    hfa = case_when(
      location == "home" ~ 1,
      location == "away" ~ -1,
      location == "neutral" ~ 0,
      TRUE ~ NA
    )
  )


game_fit_data_all <- game_model_data |>
  mutate(
    home_id = match(home_team, teams),
    away_id = match(away_team, teams),
    .after = away_team
  ) |>
  mutate(
    season_idx = as.integer(as.factor(season)),
    .after = season
  ) |>
  select(
    game_id, season, season_idx, week, week_idx = week_seq, game_type, season_type,
    home_team, away_team, home_id, away_id,
    location, hfa,
    home_score, away_score, 
    result, spread_line,
    total, total_line
  )

team_fit_data_all <- game_fit_data_all |>
  clean_homeaway(invert = c("result", "spread_line", "hfa"))

game_fit_data <- game_fit_data_all |>
  filter(!is.na(result))


# Unique week table
week_tbl <- game_fit_data_all |>
  select(season, season_idx, week, week_idx) |>
  distinct() |>
  arrange(week_idx)

season_breaks <- game_model_data |>
  rename(week_idx = week_seq) |>
  group_by(season) |>
  slice_min(week_idx, with_ties = FALSE) |>
  arrange(week_idx) |>
  select(season, week_idx)

team_colors <- teams_data$team_color
names(team_colors) <- teams_data$team_abbr
team_colors

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. PLOT TEAM PRIOR ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

team_plot_data <- game_fit_data |>
  clean_homeaway(invert = c("result", "spread_line", "hfa")) |>
  left_join(
    teams_data |> select(-team_id),
    by = join_by(team == team_abbr)
  ) |>
  mutate(
    spread_residual = result - spread_line,
    .after = spread_line
  )

## Result ----
p <- ggplot(
  data = team_plot_data,
  aes(x = week_idx, y = result, 
      colour = team)
) +
  #geom_line(alpha = 0.1) +
  # geom_smooth(
  #   method = "gam",
  #   formula = y ~ s(x, bs = "tp", k = 20),
  #   se = FALSE) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    #n = 100,
    span = 0.5,
    se = FALSE) +
  scale_color_manual(values = team_colors) +
  # ggrepel::geom_text_repel(
  #   data = season_breaks |> filt
  #                            ) +
  gghighlight::gghighlight(
    team_division == "AFC North",line_label_type = "ggrepel_text",
    #team == "BAL",
    use_group_by = FALSE,
    use_direct_label = FALSE) +
  #facet_wrap(vars(team_division), ncol = 1, dir = "v") +
  #scale_color_nfl()
  scale_x_continuous(
    breaks = season_breaks$week_idx,
    minor_breaks = seq(1, max(team_plot_data$week_idx),1),
    labels = season_breaks$season
  ) +
  #coord_cartesian(ylim = c(-20, 20)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom"
  )
p
ggplotly(p)


## Spread_line ----
p <- ggplot(
  data = team_plot_data,
  aes(x = season, y = spread_line)#, colour = team)
) +
  #geom_line(alpha = 0.1) +
  # geom_smooth(
  #   method = "gam",
  #   formula = y ~ s(x, bs = "tp", k = 20),
  #   se = FALSE) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    #n = 100,
    #span = 0.5,
    se = FALSE) +
  #scale_color_manual(values = team_colors) +
  # ggrepel::geom_text_repel(
  #   data = season_breaks |> filt
  #                            ) +
  # gghighlight::gghighlight(
  #   team_division == "AFC North",line_label_type = "ggrepel_text",
  #   #team == "BAL",
  #   use_group_by = FALSE,
  #   use_direct_label = FALSE) +
  #facet_wrap(vars(team_division), ncol = 1, dir = "v") +
  #scale_color_nfl()
  scale_x_continuous(
    breaks = season_breaks$season,
    #minor_breaks = seq(1, max(team_plot_data$week_idx),1),
    labels = season_breaks$season
  ) +
  #coord_cartesian(ylim = c(-20, 20)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom"
  )
p
ggplotly(p)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. PLOT GAME PRIOR ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

game_plot_data <- game_fit_data |>
  mutate(
    spread_residual = result - spread_line,
    .after = spread_line
  )

