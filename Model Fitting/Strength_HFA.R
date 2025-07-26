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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. MODEL 1 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2023, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 18) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_oos_week, last_oos_week))
train_data


## 2.1 Compile Model ----
hfa_mod1 <- cmdstan_model("~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/strength_hfa_weekly.stan")
hfa_mod1_variables <- hfa_mod1$variables()

## 2.2 Stan Data ----
# Determine unique teams and weeks
team_id_lookup <- setNames(seq_along(teams), teams)

# Map team names to indices (1-based)
home_id <- as.integer(factor(train_data$home_team, levels = teams))
away_id <- as.integer(factor(train_data$away_team, levels = teams))

# Map week_idx to a contiguous sequence starting from 1
weeks <- sort(unique(train_data$week_idx))
week_lookup <- setNames(seq_along(weeks), weeks)
week_idx <- as.integer(factor(train_data$week_idx, levels = weeks))

hfa_mod1_stan_data <- list(
  N_games   = nrow(train_data),
  N_teams   = length(teams),
  N_weeks   = length(weeks),
  home_id   = home_id,
  away_id   = away_id,
  week_idx  = week_idx,
  hfa       = as.integer(train_data$hfa),     # 1 = home, 0 = neutral
  result    = as.numeric(train_data$result)   # margin (home - away)
)

mod_output_dir <- "Model Fitting/stan_models"

hfa_mod1_iters <- 1000
hfa_mod1_warmup <- 500
hfa_mod1_chains <- 2
hfa_mod1_thin <- 1
hfa_mod1_sims <- ((hfa_mod1_iters)/hfa_mod1_thin)*hfa_mod1_chains
hfa_mod1_parallel_chains <- parallel::detectCores()
hfa_mod1_adapt_delta <- 0.95
hfa_mod1_max_treedeepth <- 15
hfa_mod1_seed <- 52

## 2.3 Fit hfa_model ----
### MCMC ----
fit_mcmc <- hfa_mod1$sample(
  data = hfa_mod1_stan_data,
  #output_dir = hfa_mod1_output_dir,
  chains = hfa_mod1_chains,
  parallel_chains = hfa_mod1_parallel_chains,
  iter_sampling = hfa_mod1_iters, 
  iter_warmup = hfa_mod1_warmup,
  thin = hfa_mod1_thin,
  adapt_delta = hfa_mod1_adapt_delta, 
  max_treedepth = hfa_mod1_max_treedeepth,
  seed = hfa_mod1_seed
)

### Diagnostics ----
fit_mcmc_sum <- fit_mcmc$summary()
fit_mcmc$diagnostic_summary()

fit_mcmc |>
  recover_types(train_data)

### Posterior ----
hfa1_team_strength_post <- fit_mcmc |>
  spread_draws(team_strength_gq[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  median_hdci()
hfa1_team_strength_post

hfa1_team_hfa_post <- fit_mcmc |>
  spread_draws(hfa_team_gq[team]) |>
  mutate(
    team = teams[team]
  ) |>
  median_hdci()
hfa1_team_hfa_post

### Plots ----
p <- ggplot(hfa1_team_strength_post, aes(x = week_idx, y = team_strength_gq, color = team)) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)")
ggplotly(p)

# Join home/away strength
hfa1_game_strengths <- train_data |>
  left_join(
    hfa1_team_strength_post |>
      select(home_team = team, week_idx, home_strength = team_strength_gq),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa1_team_strength_post |>
      select(away_team = team, week_idx, away_strength = team_strength_gq),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa1_team_hfa_post |>
      select(home_team = team, home_hfa = hfa_team_gq),
    by = "home_team"
  ) |>
  mutate(
    pred_result = home_strength - away_strength + home_hfa*hfa,
    pred_error = pred_result - result,
    spread_error = spread_line - result
  )

ggplot(hfa1_game_strengths, aes(x = pred_result, y = result)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa1_game_strengths, aes(x = pred_result, y = pred_error)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 0, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa1_game_strengths, aes(x = pred_result, y = spread_line)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dotted") +
  labs(
    x = "Model Expected Spread",
    y = "Vegas Spread",
    title = "Model vs Vegas Spread"
  )

ggplot(hfa1_game_strengths, aes(x = pred_error)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Model Spread Error (Model - Actual)",
    y = "Count",
    title = "Distribution of Spread Errors"
  )


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL 2 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2023, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 18) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_oos_week, last_oos_week))
train_data


## 3.1 Compile Model ----
hfa2_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/strength_hfa_week_season.stan"
  )
hfa2_mod_variables <- hfa2_mod$variables()

## 3.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
# For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
week_tbl_season_map <- setNames(seq_along(seasons), seasons)

week_tbl_hfa2 <- week_tbl |>
  mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
  filter(week_idx %in% weeks)

week_season <- week_tbl_hfa2$season_id[match(weeks, week_tbl_hfa2$week_idx)] # length N_weeks, 1-based

# 2. first/last week of each season (values are week_idx, NOT positions)
first_week_of_season <- week_tbl_hfa2 |>
  group_by(season_id) |>
  summarise(first = min(week_idx), .groups = "drop") |>
  pull(first) |>
  match(weeks)  # Get position in weeks

last_week_of_season <- week_tbl_hfa2 |>
  group_by(season_id) |>
  summarise(last = max(week_idx), .groups = "drop") |>
  pull(last) |>
  match(weeks)  # Get position in weeks

# 3. Stan data
hfa2_stan_data <- list(
  N_games    = nrow(train_data),
  N_teams    = length(teams),
  N_seasons  = length(seasons),
  N_weeks    = length(weeks),
  home_id    = as.integer(factor(train_data$home_team, levels = teams)),
  away_id    = as.integer(factor(train_data$away_team, levels = teams)),
  week_id    = as.integer(factor(train_data$week_idx, levels = weeks)),
  season_id  = as.integer(factor(train_data$season, levels = seasons)),
  week_season = week_season,  # length N_weeks, value in 1:N_seasons
  first_week_of_season = first_week_of_season, # N_seasons, value is week_idx
  last_week_of_season  = last_week_of_season,  # N_seasons, value is week_idx
  hfa        = as.integer(train_data$hfa), # 1 = home, 0 = neutral
  result     = as.numeric(train_data$result) # home - away margin
)



#mod_output_dir <- "Model Fitting/stan_models"

hfa2_iters <- 1000
hfa2_warmup <- 500
hfa2_chains <- 2
hfa2_thin <- 1
hfa2_sims <- ((hfa2_iters)/hfa2_thin)*hfa2_chains
hfa2_parallel_chains <- parallel::detectCores()
hfa2_adapt_delta <- 0.95
hfa2_max_treedeepth <- 15
hfa2_seed <- 52

## 3.3 Fit hfa_model ----
### MCMC ----
hfa2_fit_mcmc <- hfa2_mod$sample(
  data = hfa2_stan_data,
  #output_dir = hfa2_output_dir,
  chains = hfa2_chains,
  parallel_chains = hfa2_parallel_chains,
  iter_sampling = hfa2_iters, 
  iter_warmup = hfa2_warmup,
  thin = hfa2_thin,
  adapt_delta = hfa2_adapt_delta, 
  max_treedepth = hfa2_max_treedeepth,
  seed = hfa2_seed
)

### MLE ----
hfa2_fit_mle <- hfa2_mod$optimize(
  data = hfa2_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa2_seed
)

hfa2_fit_mle$output()
hfa2_mle_sum <- hfa2_fit_mle$summary(
  variables = c("lp__", 
                "beta_s", "beta_w",
                "sigma_team_hfa", "sigma_s", "sigma_w", "sigma_y",
                "team_hfa", "league_hfa",
                "team_strength")
) |>
  mutate(estimate = round(estimate, 4))

### MAP ----
hfa2_fit_map <- hfa2_mod$optimize(
  data = hfa2_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa2_seed
)
hfa2_fit_map$output()
round(hfa2_fit_map$mle(variables = c("team_hfa", "league_hfa", "team_strength")), 4)
hfa2_map_sum <- hfa2_fit_map$summary(
  variables = c("lp__", 
                "beta_s", "beta_w",
                "sigma_team_hfa", "sigma_s", "sigma_w", "sigma_y",
                "team_hfa", "league_hfa",
                #"team_strength",
                "team_strength_raw")
) |>
  mutate(estimate = round(estimate, 4))


### Diagnostics ----
fit_mcmc_sum <- fit_mcmc$summary()
fit_mcmc$diagnostic_summary()
