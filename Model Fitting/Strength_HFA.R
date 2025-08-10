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
    home_spread_odds, away_spread_odds, 
    home_spread_prob, away_spread_prob,
    total, total_line,
    over_odds, under_odds,
    over_prob, under_prob,
    winner,
    home_moneyline, away_moneyline,
    home_moneyline_prob, away_moneyline_prob
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
# 2. MODEL hfa1 ----
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
hfa1_mod <- cmdstan_model("~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/strength_hfa_weekly.stan")
hfa1_mod_variables <- hfa1_mod$variables()

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

hfa1_stan_data <- list(
  N_games   = nrow(train_data),
  N_teams   = length(teams),
  N_weeks   = length(weeks),
  home_id   = home_id,
  away_id   = away_id,
  week_idx  = week_idx,
  hfa       = as.integer(train_data$hfa),     # 1 = home, 0 = neutral
  result    = as.numeric(train_data$result)   # margin (home - away)
)

#mod_output_dir <- "Model Fitting/stan_models"

hfa1_iters <- 2000
hfa1_warmup <- 500
hfa1_chains <- 2
hfa1_thin <- 1
hfa1_sims <- ((hfa1_iters)/hfa1_thin)*hfa1_chains
hfa1_parallel_chains <- parallel::detectCores()
hfa1_adapt_delta <- 0.95
hfa1_max_treedeepth <- 15
hfa1_seed <- 52

## 2.3 MCMC ----
### 2.3.1 Fit Model ----
hfa1_mcmc_fit <- hfa1_mod$sample(
  data = hfa1_stan_data,
  #output_dir = hfa1_output_dir,
  chains = hfa1_chains,
  parallel_chains = hfa1_parallel_chains,
  iter_sampling = hfa1_iters, 
  iter_warmup = hfa1_warmup,
  thin = hfa1_thin,
  adapt_delta = hfa1_adapt_delta, 
  max_treedepth = hfa1_max_treedeepth,
  seed = hfa1_seed
)

### 2.3.2 Diagnostics ----
hfa1_mcmc_fit$
  hfa1_mcmc_fit$cmdstan_diagnose()
hfa1_mcmc_fit$print()
hfa1_mcmc_sum <- hfa1_mcmc_fit$summary()
hfa1_mcmc_fit$diagnostic_summary()

### 2.3.3 Posterior ----
hfa1_team_strength <- hfa1_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  median_hdci()
hfa1_team_strength

hfa1_team_hfa <- hfa1_mcmc_fit |>
  spread_draws(hfa_team[team]) |>
  mutate(
    team = teams[team]
  ) |>
  median_hdci()
hfa1_team_hfa

hfa1_league_hfa <- hfa1_mcmc_fit |>
  spread_draws(hfa_mean) |>
  median_hdci()
hfa1_league_hfa

### 2.3.4 Plots ----
p <- ggplot(hfa1_team_strength, aes(x = week_idx, y = team_strength, color = team)) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)")
ggplotly(p)

# Join home/away strength
hfa1_game_strengths <- train_data |>
  left_join(
    hfa1_team_strength |>
      select(home_team = team, week_idx, home_strength = team_strength),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa1_team_strength |>
      select(away_team = team, week_idx, away_strength = team_strength),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa1_team_hfa |>
      select(home_team = team, home_hfa = hfa_team),
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

## 2.4 MLE ----
hfa1_mle_fit <- hfa1_mod$optimize(
  data = hfa1_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa1_seed
)

hfa1_mle_fit$output()
hfa1_mle_sum <- hfa1_mle_fit$summary(
  variables = c("lp__", 
                "hfa_mean", "hfa_team", "team_strength",
                "tau_team", "sigma")
) |>
  mutate(estimate = round(estimate, 4))

hfa1_mle_team_strength <- hfa1_mle_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  mean_hdi() |>
  distinct(team, team_strength)
hfa1_mle_team_strength

hfa1_mle_team_hfa <- hfa1_mle_fit |>
  spread_draws(hfa_team[team]) |>
  mutate(
    team = teams[team]
  ) |>
  mean_hdi() |>
  distinct(team, hfa_team)
hfa1_mle_team_hfa

## 2.5 MAP ----
hfa1_map_fit <- hfa1_mod$optimize(
  data = hfa1_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa1_seed
)

hfa1_map_fit$output()
hfa1_map_sum <- hfa1_map_fit$summary(
  variables = c("lp__", 
                "hfa_mean", "hfa_team", "team_strength",
                "tau_team", "sigma")
) |>
  mutate(estimate = round(estimate, 4))

hfa1_map_team_strength <- hfa1_map_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  mean_hdi() |>
  distinct(team, team_strength)
hfa1_map_team_strength

hfa1_map_team_hfa <- hfa1_map_fit |>
  spread_draws(hfa_team[team]) |>
  mutate(
    team = teams[team]
  ) |>
  mean_hdi() |>
  distinct(team, hfa_team)
hfa1_map_team_hfa

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL hfa2 ----
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

## 3.3 MCMC ----
### 3.3.1 Fit Model ----
hfa2_mcmc_fit <- hfa2_mod$sample(
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

### 3.3.2 Diagnostics ----
hfa2_mod_variables
hfa2_mcmc_fit$sampler_diagnostics()
hfa2_mcmc_fit$cmdstan_diagnose()
hfa2_mcmc_fit$diagnostic_summary()
hfa2_mcmc_sum <- hfa2_mcmc_fit$summary()

### 3.3.3 Posterior ----
hfa2_team_strength <- hfa2_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

# hfa2_team_hfa <- hfa2_mcmc_fit |>
#   spread_draws(team_hfa[team, season]) |>
#   mutate(
#     team = teams[team],
#     season = seasons[season]
#   ) |>
#   summarise_draws()
# hfa2_team_hfa
# 
# hfa2_league_hfa <- hfa2_mcmc_fit |>
#   spread_draws(league_hfa[season]) |>
#   mutate(
#     season = seasons[season]
#   ) |>
#   summarise_draws()
# hfa2_league_hfa

hfa2_team_hfa_total <- hfa2_mcmc_fit |>
  spread_draws(league_hfa[season], team_hfa[team, season]) |>
  mutate(team_hfa_total = team_hfa + league_hfa) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  summarise_draws()
hfa2_team_hfa_total


# hfa2_team_strength_end <- week_tbl |>
#   right_join(hfa2_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa2_team_strength_end <- hfa2_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa2_team_hfa_end <- hfa2_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa2_team_total <- bind_rows(
  hfa2_team_hfa_end,
  hfa2_team_strength_end
) |>
  fill(season, week_idx, .direction = "downup") |>
  select(season, week_idx, team, everything()) |>
  arrange(team) |>
  filter(variable %in% c("team_hfa_total", "team_strength")) |>
  mutate(
    variable = case_when(
      variable == "team_hfa_total" ~ "sp_hfa",
      variable == "team_strength" ~ "sp_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )

### 3.3.4 Plots ----
p <- ggplot(hfa2_team_strength, aes(x = week_idx, y = team_strength, color = team)) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)")
ggplotly(p)

# Join home/away strength
hfa2_game_strengths <- train_data |>
  left_join(
    hfa2_team_strength |>
      select(home_team = team, week_idx, home_strength = team_strength),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa2_team_strength |>
      select(away_team = team, week_idx, away_strength = team_strength),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa2_team_hfa |>
      select(home_team = team, home_hfa = hfa_team),
    by = "home_team"
  ) |>
  mutate(
    pred_result = home_strength - away_strength + home_hfa*hfa,
    pred_error = pred_result - result,
    spread_error = spread_line - result
  )

ggplot(hfa2_game_strengths, aes(x = pred_result, y = result)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa2_game_strengths, aes(x = pred_result, y = pred_error)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 0, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa2_game_strengths, aes(x = pred_result, y = spread_line)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dotted") +
  labs(
    x = "Model Expected Spread",
    y = "Vegas Spread",
    title = "Model vs Vegas Spread"
  )

ggplot(hfa2_game_strengths, aes(x = pred_error)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Model Spread Error (Model - Actual)",
    y = "Count",
    title = "Distribution of Spread Errors"
  )

## 3.4 MLE ----
hfa2_mle_fit <- hfa2_mod$optimize(
  data = hfa2_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa2_seed
)

hfa2_mle_fit$output()
hfa2_mle_sum <- hfa2_mle_fit$summary(
  variables = c("lp__", 
                "beta_s", "beta_w",
                "sigma_team_hfa", "sigma_s", "sigma_w", "sigma_y",
                "team_hfa", "league_hfa",
                "team_strength")
) |>
  mutate(estimate = round(estimate, 4))

hfa1_mle_team_strength <- hfa1_mle_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  mean_hdi() |>
  distinct(team, team_strength)
hfa1_mle_team_strength

hfa1_mle_team_hfa <- hfa1_mle_fit |>
  spread_draws(hfa_team[team]) |>
  mutate(
    team = teams[team]
  ) |>
  mean_hdi() |>
  distinct(team, hfa_team)
hfa1_mle_team_hfa

## 3.5 MAP ----
hfa2_map_fit <- hfa2_mod$optimize(
  data = hfa2_stan_data,
  #output_dir = hfa2_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa2_seed
)
hfa2_map_fit$output()
round(hfa2_map_fit$mle(variables = c("team_hfa", "league_hfa", "team_strength")), 4)
hfa2_map_sum <- hfa2_map_fit$summary(
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. MODEL hfa3 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2020, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))
train_data


## 4.1 Compile Model ----
hfa3_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa3.stan"
)
hfa3_mod_variables <- hfa3_mod$variables()

## 4.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
# For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
week_tbl_season_map <- setNames(seq_along(seasons), seasons)

week_tbl_hfa3 <- week_tbl |>
  mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
  filter(week_idx %in% weeks)

week_season <- week_tbl_hfa3$season_id[match(weeks, week_tbl_hfa3$week_idx)] # length N_weeks, 1-based

# 2. first/last week of each season (values are week_idx, NOT positions)
first_week_of_season <- week_tbl_hfa3 |>
  group_by(season_id) |>
  summarise(first = min(week_idx), .groups = "drop") |>
  pull(first) |>
  match(weeks)  # Get position in weeks

last_week_of_season <- week_tbl_hfa3 |>
  group_by(season_id) |>
  summarise(last = max(week_idx), .groups = "drop") |>
  pull(last) |>
  match(weeks)  # Get position in weeks

# 4. Stan data
hfa3_stan_data <- list(
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

hfa3_iters <- 2000
hfa3_warmup <- 1000
hfa3_chains <- 4
hfa3_thin <- 1
hfa3_sims <- ((hfa3_iters)/hfa3_thin)*hfa3_chains
hfa3_parallel_chains <- parallel::detectCores()
hfa3_adapt_delta <- 0.95
hfa3_max_treedeepth <- 15
hfa3_seed <- 52

## 4.3 MCMC ----
### 4.4.1 Fit Model ----
hfa3_mcmc_fit <- hfa3_mod$sample(
  data = hfa3_stan_data,
  #output_dir = hfa3_output_dir,
  chains = hfa3_chains,
  parallel_chains = hfa3_parallel_chains,
  iter_sampling = hfa3_iters, 
  iter_warmup = hfa3_warmup,
  thin = hfa3_thin,
  adapt_delta = hfa3_adapt_delta, 
  max_treedepth = hfa3_max_treedeepth,
  seed = hfa3_seed
)

### Save
hfa3_mcmc_fit$save_object(file = "Model Fitting/stan_models/hfa3_mcmc_fit_recent.rds")

### 4.4.2 Diagnostics ----
hfa3_mod_variables
hfa3_mcmc_fit$sampler_diagnostics()
hfa3_mcmc_fit$cmdstan_diagnose()
hfa3_mcmc_fit$diagnostic_summary()

hfa3_mcmc_fit_vars <- hfa3_mcmc_fit$metadata()$stan_variable_sizes
hfa3_mcmc_fit_vars
hfa3_mcmc_fit$output()
hfa3_mcmc_sum <- hfa3_mcmc_fit$summary(
  variables = subset(names(hfa3_mcmc_fit_vars),
                     hfa3_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa3_mcmc_sum, n = Inf)

hfa3_mcmc_fit |>
  spread_draws(lp__) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

### 4.4.3 Posterior ----
hfa3_team_strength <- hfa3_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

# hfa3_team_hfa <- hfa3_mcmc_fit |>
#   spread_draws(team_hfa[team, season]) |>
#   mutate(
#     team = teams[team],
#     season = seasons[season]
#   ) |>
#   summarise_draws()
# hfa3_team_hfa
# 
hfa3_league_hfa <- hfa3_mcmc_fit |>
  spread_draws(league_hfa[season]) |>
  mutate(
    season = seasons[season]
  ) |>
  summarise_draws()
hfa3_league_hfa

hfa3_team_hfa_total <- hfa3_mcmc_fit |>
  spread_draws(league_hfa[season], team_hfa[team, season]) |>
  mutate(team_hfa_total = team_hfa + league_hfa) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  summarise_draws()
hfa3_team_hfa_total


# hfa3_team_strength_end <- week_tbl |>
#   right_join(hfa3_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa3_team_strength_end <- hfa3_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa3_team_hfa_end <- hfa3_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa3_team_total <- bind_rows(
  hfa3_team_hfa_end,
  hfa3_team_strength_end
) |>
  fill(season, week_idx, .direction = "downup") |>
  select(season, week_idx, team, everything()) |>
  arrange(team) |>
  filter(variable %in% c("team_hfa_total", "team_strength")) |>
  mutate(
    variable = case_when(
      variable == "team_hfa_total" ~ "sp_hfa",
      variable == "team_strength" ~ "sp_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )

### 4.4.4 Plots ----
p <- hfa3_team_strength |>
  left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
p
ggplotly(p, tooltip = "text")

pb <- ggplot_build(p)

# Join home/away strength
hfa3_game_strengths <- train_data |>
  left_join(
    hfa3_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, week_idx, home_strength = team_strength_mean),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa3_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(away_team = team, week_idx, away_strength = team_strength_mean),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa3_team_hfa_total |>
      filter(variable == "team_hfa_total") |>
      pivot_wider(
        id_cols = c(team, season),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, season, home_hfa = team_hfa_total_mean),
    by = c("home_team", "season")
  ) |>
  mutate(
    pred_result = home_strength - away_strength + home_hfa*hfa,
    pred_error = pred_result - result,
    spread_error = spread_line - result
  )

hfa3_game_strengths <- hfa3_team_hfa_total |>
  filter(variable %in% c("team_hfa_total", "team_strength"))

ggplot(hfa3_game_strengths, aes(x = pred_result, y = result)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa3_game_strengths, aes(x = pred_result, y = pred_error)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 0, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Model Expected Spread - Actual Result (Home Margin)",
    title = "Model Spread vs Residuals"
  )

ggplot(hfa3_game_strengths, aes(x = pred_result, y = spread_line)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Vegas Spread",
    title = "Model vs Vegas Spread"
  )

ggplot(hfa3_game_strengths, aes(x = pred_error)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Model Spread Error (Model - Actual)",
    y = "Count",
    title = "Distribution of Spread Errors"
  )

hfa3_metrics <- tibble(
  MAE   = mae(hfa3_game_strengths$result, hfa3_game_strengths$pred_result),
  RMSE  = rmse(hfa3_game_strengths$result, hfa3_game_strengths$pred_result),
  Bias  = mean(hfa3_game_strengths$pred_error),
  SD_Error = sd(hfa3_game_strengths$pred_error)
)
hfa3_metrics_vegas <- tibble(
  MAE   = mae(hfa3_game_strengths$result, hfa3_game_strengths$spread_line),
  RMSE  = rmse(hfa3_game_strengths$result, hfa3_game_strengths$spread_line),
  Bias  = mean(hfa3_game_strengths$spread_error),
  SD_Error = sd(hfa3_game_strengths$spread_error)
)
hfa3_metrics <- hfa3_metrics |>
  mutate(Metric = "Model", .before = 1) |>
  bind_rows(
    hfa3_metrics_vegas |>
      mutate(Metric = "Vegas", .before = 1)
  ) |>
  mutate(Model = "hfa3", .before = 1)
print(hfa3_metrics)

hfa3_league_hfa |>
  ggplot(aes(x = season, y = mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "blue", alpha = 0.2) +
  scale_x_continuous(
    name = "Season",
    breaks = seasons,  # Show season boundaries on x-axis
    minor_breaks = FALSE,
    labels = seasons
  ) +
  labs(title = "League-wide Home Field Advantage by Season",
       subtitle = "Model hfa3",
       x = "Season",
       y = "League HFA (mean ± 90% interval)") +
  theme_minimal()

## 4.4 MLE ----
hfa3_mle_fit <- hfa3_mod$optimize(
  data = hfa3_stan_data,
  #output_dir = hfa3_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa3_seed
)

hfa3_mle_fit_vars <- hfa3_mle_fit$metadata()$stan_variable_sizes
hfa3_mle_fit_vars
hfa3_mle_fit$output()
hfa3_mle_sum <- hfa3_mle_fit$summary(
  variables = subset(names(hfa3_mle_fit_vars),
                     hfa3_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa3_mle_sum, n = Inf)

hfa3_mle_team_strength <- hfa3_mle_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_mle",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa3_mle_league_hfa <- hfa3_mle_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_mle",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa3_mle_team_hfa <- hfa3_mle_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa3_mle_team_total_hfa <- hfa3_mle_fit$draws(
  variables = c("total_team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa3_mle_team_estimates <- hfa3_mle_team_strength |>
  left_join(week_tbl_hfa3) |>
  left_join(hfa3_mle_league_hfa) |>
  left_join(hfa3_mle_team_hfa) |>
  left_join(hfa3_mle_team_total_hfa) |>
  relocate(contains("mle"), .after = last_col())

## 4.5 MAP ----
hfa3_map_fit <- hfa3_mod$optimize(
  data = hfa3_stan_data,
  #output_dir = hfa3_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa3_seed
)
hfa3_map_fit_vars <- hfa3_map_fit$metadata()$stan_variable_sizes
hfa3_map_fit_vars
hfa3_map_fit$output()
hfa3_map_sum <- hfa3_map_fit$summary(
  variables = subset(names(hfa3_map_fit_vars),
                     hfa3_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa3_map_sum, n = Inf)

hfa3_map_team_strength <- hfa3_map_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_map",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa3_map_league_hfa <- hfa3_map_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_map",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa3_map_team_hfa <- hfa3_map_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa3_map_team_total_hfa <- hfa3_map_fit$draws(
  variables = c("total_team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa3_map_team_estimates <- hfa3_map_team_strength |>
  left_join(week_tbl_hfa3) |>
  left_join(hfa3_map_league_hfa) |>
  left_join(hfa3_map_team_hfa) |>
  left_join(hfa3_map_team_total_hfa) |>
  relocate(contains("map"), .after = last_col())

## 4.6 Variational ----
hfa3_vi_fit <- hfa3_mod$variational(
  data = hfa3_stan_data,
  init = 1,
  iter = 20000,
  draws = hfa3_iters,
  seed = hfa3_seed
)

hfa3_vi_fit_vars <- hfa3_vi_fit$metadata()$stan_variable_sizes
hfa3_vi_fit_vars
hfa3_vi_fit$output()
hfa3_vi_sum <- hfa3_vi_fit$summary(
  variables = subset(names(hfa3_vi_fit_vars),
                     hfa3_vi_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa3_vi_sum, n = Inf)

hfa3_vi_team_strength <- hfa3_vi_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_vi",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa3_vi_league_hfa <- hfa3_vi_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_vi",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa3_vi_team_hfa_dev <- hfa3_vi_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_vi",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa3_vi_team_total_hfa <- hfa3_vi_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa3_vi_team_estimates <- hfa3_vi_team_strength |>
  left_join(week_tbl_hfa3) |>
  left_join(hfa3_vi_league_hfa) |>
  left_join(hfa3_vi_team_hfa_dev) |>
  left_join(hfa3_vi_team_total_hfa) |>
  relocate(contains("vi"), .after = last_col())

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 5. MODEL hfa4 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2020, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))
train_data


## 5.1 Compile Model ----
hfa4_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa4.stan"
)
hfa4_mod_variables <- hfa4_mod$variables()

hfa4_mod_stan <- stan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/strength_week_hfa_season.stan"
)
hfa4_mod_variables_stan <- hfa4_mod_stan$variables()

## 5.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
# For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
week_tbl_season_map <- setNames(seq_along(seasons), seasons)

week_tbl_hfa4 <- week_tbl |>
  mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
  filter(week_idx %in% weeks)

week_season <- week_tbl_hfa4$season_id[match(weeks, week_tbl_hfa4$week_idx)] # length N_weeks, 1-based

# 2. first/last week of each season (values are week_idx, NOT positions)
first_week_of_season <- week_tbl_hfa4 |>
  group_by(season_id) |>
  summarise(first = min(week_idx), .groups = "drop") |>
  pull(first) |>
  match(weeks)  # Get position in weeks

last_week_of_season <- week_tbl_hfa4 |>
  group_by(season_id) |>
  summarise(last = max(week_idx), .groups = "drop") |>
  pull(last) |>
  match(weeks)  # Get position in weeks

# 5. Stan data
hfa4_stan_data <- list(
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

hfa4_iters <- 2000
hfa4_warmup <- 1000
hfa4_chains <- 4
hfa4_thin <- 1
hfa4_sims <- ((hfa4_iters)/hfa4_thin)*hfa4_chains
hfa4_parallel_chains <- parallel::detectCores()
hfa4_adapt_delta <- 0.95
hfa4_max_treedeepth <- 15
hfa4_seed <- 52

## 5.3 MCMC ----
### 5.5.1 Fit Model ----
system.time(
  hfa4_mcmc_fit <- hfa4_mod$sample(
    data = hfa4_stan_data,
    #output_dir = hfa4_output_dir,
    chains = hfa4_chains,
    parallel_chains = hfa4_parallel_chains,
    iter_sampling = hfa4_iters, 
    iter_warmup = hfa4_warmup,
    thin = hfa4_thin,
    adapt_delta = hfa4_adapt_delta, 
    max_treedepth = hfa4_max_treedeepth,
    seed = hfa4_seed
  )
)

### Save
hfa4_mcmc_fit$save_object(file = "Model Fitting/stan_models/hfa4_mcmc_fit_recent.rds")

### 5.5.2 Diagnostics ----
hfa4_mod_variables
hfa4_mcmc_fit$sampler_diagnostics(format = "df")
hfa4_mcmc_fit$cmdstan_diagnose()
hfa4_mcmc_fit$diagnostic_summary()

hfa4_mcmc_fit_vars <- hfa4_mcmc_fit$metadata()$stan_variable_sizes
hfa4_mcmc_fit_vars
hfa4_mcmc_fit$output()
hfa4_mcmc_sum <- hfa4_mcmc_fit$summary(
  variables = subset(names(hfa4_mcmc_fit_vars),
                     hfa4_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa4_mcmc_sum, n = Inf)

scalar_draws <- hfa4_mcmc_fit$draws(
  variables = c(
    "lp__",
    "league_hfa", "sigma_team_hfa_dev",
    "beta_w", "sigma_w",
    "beta_hfa", "sigma_hfa",
    "beta_s", "sigma_s",
    "sigma_y"
  ),
  format = "matrix"
) 
scalar_draws_df <- hfa4_mcmc_fit$draws(
  variables = c(
    "lp__",
    "league_hfa", "sigma_team_hfa_dev",
    "beta_w", "sigma_w",
    "beta_hfa", "sigma_hfa",
    "beta_s", "sigma_s",
    "sigma_y"
  ),
  format = "df"
) 

mcmc_trace(scalar_draws)
mcmc_pairs(
  scalar_draws_df |> dplyr::select(sigma_team_hfa_dev, sigma_w, sigma_s, "league_hfa[1]"),
  diag_fun = "dens"
)
mcmc_dens_overlay(
  scalar_draws,
  pars = c("sigma_team_hfa_dev", "sigma_w", "sigma_s")
)

hfa4_mcmc_fit |>
  spread_draws(lp__) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

### 5.5.3 Posterior ----
fit_draws <- hfa4_mcmc_fit$draws(
  variables = c("team_strength", "team_total_hfa"),
  format = "df"
)

team_params <- paste0("team_strength[", 1:5, ",", 
                      rep(c(1, 10, 19), each = 5), "]")

# Traceplots for those teams
bayesplot::mcmc_trace(
  fit_draws, pars = team_params,
  facet_args = list(nrow = 5, dir = "v")
)

# Overlayed density plots for those teams
bayesplot::mcmc_dens_overlay(
  fit_draws, pars = team_params,
  facet_args = list(nrow = 5, dir = "v")
)


hfa4_team_strength <- hfa4_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

# hfa4_team_hfa <- hfa4_mcmc_fit |>
#   spread_draws(team_hfa_dev[team]) |>
#   mutate(
#     team = teams[team]
#   ) |>
#   summarise_draws()
# hfa4_team_hfa
# 
hfa4_league_hfa <- hfa4_mcmc_fit |>
  spread_draws(league_hfa[season]) |>
  mutate(
    season = seasons[season]
  ) |>
  summarise_draws()
hfa4_league_hfa

hfa4_team_hfa_total <- hfa4_mcmc_fit |>
  spread_draws(league_hfa[season], 
               team_hfa_dev[team],
               team_total_hfa[team, season]) |>
  #mutate(team_hfa_total2 = team_hfa_dev + league_hfa) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  rename(team_hfa_total = team_total_hfa) |>
  summarise_draws()
hfa4_team_hfa_total



# hfa4_team_strength_end <- week_tbl |>
#   right_join(hfa4_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa4_team_strength_end <- hfa4_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa4_team_hfa_end <- hfa4_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa4_team_total <- bind_rows(
  hfa4_team_hfa_end,
  hfa4_team_strength_end
) |>
  fill(season, week_idx, .direction = "downup") |>
  select(season, week_idx, team, everything()) |>
  arrange(team) |>
  filter(variable %in% c("team_hfa_total", "team_strength")) |>
  mutate(
    variable = case_when(
      variable == "team_hfa_total" ~ "sp_hfa",
      variable == "team_strength" ~ "sp_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )

### 5.5.4 Plots ----
p <- hfa4_team_strength |>
  left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
p
ggplotly(p, tooltip = "text")

pb <- ggplot_build(p)

# Join home/away strength
hfa4_game_strengths <- train_data |>
  left_join(
    hfa4_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, week_idx, home_strength = team_strength_mean),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa4_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(away_team = team, week_idx, away_strength = team_strength_mean),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa4_team_hfa_total |>
      filter(variable == "team_hfa_total") |>
      pivot_wider(
        id_cols = c(team, season),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, season, home_hfa = team_hfa_total_mean),
    by = c("home_team", "season")
  ) |>
  mutate(
    pred_result = home_strength - away_strength + home_hfa*hfa,
    pred_error = pred_result - result,
    spread_error = spread_line - result
  )

# hfa4_game_strengths <- hfa4_team_hfa_total |>
#   filter(variable %in% c("team_hfa_total", "team_strength"))

ggplot(hfa4_game_strengths, aes(x = pred_result, y = result)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa4_game_strengths, aes(x = pred_result, y = pred_error)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 0, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Model Expected Spread - Actual Result (Home Margin)",
    title = "Model Spread vs Residuals"
  )

ggplot(hfa4_game_strengths, aes(x = pred_result, y = spread_line)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Vegas Spread",
    title = "Model vs Vegas Spread"
  )

ggplot(hfa4_game_strengths, aes(x = pred_error)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Model Spread Error (Model - Actual)",
    y = "Count",
    title = "Distribution of Spread Errors"
  )

hfa4_metrics <- tibble(
  MAE   = mae(hfa4_game_strengths$result, hfa4_game_strengths$pred_result),
  RMSE  = rmse(hfa4_game_strengths$result, hfa4_game_strengths$pred_result),
  Bias  = mean(hfa4_game_strengths$pred_error),
  SD_Error = sd(hfa4_game_strengths$pred_error)
)
hfa4_metrics_vegas <- tibble(
  MAE   = mae(hfa4_game_strengths$result, hfa4_game_strengths$spread_line),
  RMSE  = rmse(hfa4_game_strengths$result, hfa4_game_strengths$spread_line),
  Bias  = mean(hfa4_game_strengths$spread_error),
  SD_Error = sd(hfa4_game_strengths$spread_error)
)
hfa4_metrics <- hfa4_metrics |>
  mutate(Metric = "Model4", .before = 1) |>
  bind_rows(
    hfa4_metrics_vegas |>
      mutate(Metric = "Vegas", .before = 1)
  ) |>
  mutate(Model = "hfa4", .before = 1)

hfa_metrics <- bind_rows(
  hfa3_metrics,
  hfa4_metrics
)
print(hfa_metrics)

hfa4_league_hfa |>
  ggplot(aes(x = season, y = mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "blue", alpha = 0.2) +
  scale_x_continuous(
    name = "Season",
    breaks = seasons,  # Show season boundaries on x-axis
    minor_breaks = FALSE,
    labels = seasons
  ) +
  labs(title = "League-wide Home Field Advantage by Season",
       subtitle = "Model hfa4",
       x = "Season",
       y = "League HFA (mean ± 90% interval)") +
  theme_minimal()

## 5.4 MLE ----
hfa4_mle_fit <- hfa4_mod$optimize(
  data = hfa4_stan_data,
  #output_dir = hfa4_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa4_seed
)

hfa4_mle_fit_vars <- hfa4_mle_fit$metadata()$stan_variable_sizes
hfa4_mle_fit_vars
hfa4_mle_fit$output()
hfa4_mle_sum <- hfa4_mle_fit$summary(
  variables = subset(names(hfa4_mle_fit_vars),
                     hfa4_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa4_mle_sum, n = Inf)

hfa4_mle_team_strength <- hfa4_mle_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_mle",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa4_mle_league_hfa <- hfa4_mle_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_mle",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa4_mle_team_hfa_dev <- hfa4_mle_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_mle",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa4_mle_team_total_hfa <- hfa4_mle_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa4_mle_team_estimates <- hfa4_mle_team_strength |>
  left_join(week_tbl_hfa4) |>
  left_join(hfa4_mle_league_hfa) |>
  left_join(hfa4_mle_team_hfa_dev) |>
  left_join(hfa4_mle_team_total_hfa) |>
  relocate(contains("mle"), .after = last_col())

## 5.5 MAP ----
hfa4_map_fit <- hfa4_mod$optimize(
  data = hfa4_stan_data,
  #output_dir = hfa4_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa4_seed
)
hfa4_map_fit_vars <- hfa4_map_fit$metadata()$stan_variable_sizes
hfa4_map_fit_vars
hfa4_map_fit$output()
hfa4_map_sum <- hfa4_map_fit$summary(
  variables = subset(names(hfa4_map_fit_vars),
                     hfa4_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa4_map_sum, n = Inf)

hfa4_map_team_strength <- hfa4_map_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_map",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa4_map_league_hfa <- hfa4_map_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_map",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa4_map_team_hfa_dev <- hfa4_map_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_map",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa4_map_team_total_hfa <- hfa4_map_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa4_map_team_estimates <- hfa4_map_team_strength |>
  left_join(week_tbl_hfa4) |>
  left_join(hfa4_map_league_hfa) |>
  left_join(hfa4_map_team_hfa_dev) |>
  left_join(hfa4_map_team_total_hfa) |>
  relocate(contains("map"), .after = last_col())


## 5.6 Variational ----
hfa4_vi_fit <- hfa4_mod$variational(
  data = hfa4_stan_data,
  init = 0,
  iter = 20000,
  draws = hfa4_iters,
  seed = hfa4_seed
)

hfa4_vi_fit_vars <- hfa4_vi_fit$metadata()$stan_variable_sizes
hfa4_vi_fit_vars
hfa4_vi_fit$output()
hfa4_vi_sum <- hfa4_vi_fit$summary(
  variables = subset(names(hfa4_vi_fit_vars),
                     hfa4_vi_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa4_vi_sum, n = Inf)

hfa4_vi_team_strength <- hfa4_vi_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_vi",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa4_vi_league_hfa <- hfa4_vi_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_vi",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa4_vi_team_hfa_dev <- hfa4_vi_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_vi",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa4_vi_team_total_hfa <- hfa4_vi_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa4_vi_team_estimates <- hfa4_vi_team_strength |>
  left_join(week_tbl_hfa4) |>
  left_join(hfa4_vi_league_hfa) |>
  left_join(hfa4_vi_team_hfa_dev) |>
  left_join(hfa4_vi_team_total_hfa) |>
  relocate(contains("vi"), .after = last_col())

## 5.7 Laplace ----
hfa4_lap_fit <- hfa4_mod$laplace(
  data = hfa4_stan_data,
  init = 0,
  jacobian = TRUE,
  draws = hfa4_iters,
  seed = hfa4_seed
)

hfa4_lap_fit_vars <- hfa4_lap_fit$metadata()$stan_variable_sizes
hfa4_lap_fit_vars
hfa4_lap_fit$output()
hfa4_lap_sum <- hfa4_lap_fit$summary(
  variables = subset(names(hfa4_lap_fit_vars),
                     hfa4_lap_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa4_lap_sum, n = Inf)

hfa4_lap_team_strength <- hfa4_lap_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_lap",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa4_lap_league_hfa <- hfa4_lap_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_lap",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa4_lap_team_hfa_dev <- hfa4_lap_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_lap",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa4_lap_team_total_hfa <- hfa4_lap_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_lap",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa4_lap_team_estimates <- hfa4_lap_team_strength |>
  left_join(week_tbl_hfa4) |>
  left_join(hfa4_lap_league_hfa) |>
  left_join(hfa4_lap_team_hfa_dev) |>
  left_join(hfa4_lap_team_total_hfa) |>
  relocate(contains("lap"), .after = last_col())



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 6. MODEL hfa5 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2020, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))
train_data


## 6.1 Compile Model ----
hfa5_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa5.stan"
)
hfa5_mod_variables <- hfa5_mod$variables()

## 6.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
# For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
week_tbl_season_map <- setNames(seq_along(seasons), seasons)

week_tbl_hfa5 <- week_tbl |>
  mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
  filter(week_idx %in% weeks)

week_season <- week_tbl_hfa5$season_id[match(weeks, week_tbl_hfa5$week_idx)] # length N_weeks, 1-based

# 2. first/last week of each season (values are week_idx, NOT positions)
first_week_of_season <- week_tbl_hfa5 |>
  group_by(season_id) |>
  summarise(first = min(week_idx), .groups = "drop") |>
  pull(first) |>
  match(weeks)  # Get position in weeks

last_week_of_season <- week_tbl_hfa5 |>
  group_by(season_id) |>
  summarise(last = max(week_idx), .groups = "drop") |>
  pull(last) |>
  match(weeks)  # Get position in weeks

# 6. Stan data
hfa5_stan_data <- list(
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

hfa5_iters <- 2000
hfa5_warmup <- 1000
hfa5_chains <- 4
hfa5_thin <- 1
hfa5_sims <- ((hfa5_iters)/hfa5_thin)*hfa5_chains
hfa5_parallel_chains <- parallel::detectCores()
hfa5_adapt_delta <- 0.95
hfa5_max_treedeepth <- 15
hfa5_seed <- 52

## 6.3 MCMC ----
### 6.3.1 Fit Model ----
system.time(
  hfa5_mcmc_fit <- hfa5_mod$sample(
    data = hfa5_stan_data,
    #output_dir = hfa5_output_dir,
    chains = hfa5_chains,
    parallel_chains = hfa5_parallel_chains,
    iter_sampling = hfa5_iters, 
    iter_warmup = hfa5_warmup,
    thin = hfa5_thin,
    adapt_delta = hfa5_adapt_delta, 
    max_treedepth = hfa5_max_treedeepth,
    seed = hfa5_seed
  )
)

### Save
hfa5_mcmc_fit$save_object(file = "Model Fitting/stan_models/hfa5_mcmc_fit_recent.rds")

### 6.3.2 Diagnostics ----
hfa5_mod_variables
hfa5_mcmc_fit$sampler_diagnostics(format = "df")
hfa5_mcmc_fit$cmdstan_diagnose()
hfa5_mcmc_fit$diagnostic_summary()

hfa5_mcmc_fit_vars <- hfa5_mcmc_fit$metadata()$stan_variable_sizes
hfa5_mcmc_fit_vars
hfa5_mcmc_fit$output()
hfa5_mcmc_sum <- hfa5_mcmc_fit$summary(
  variables = subset(names(hfa5_mcmc_fit_vars),
                     hfa5_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5_mcmc_sum, n = Inf)


### 6.3.3 Posterior ----
hfa5_mcmc_team_strength2 <- hfa5_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()
hfa5_mcmc_team_strength2 <- hfa5_team_strength |>
  left_join(week)

hfa5_mcmc_team_hfa <- hfa5_mcmc_fit |>
  spread_draws(team_hfa[team, season]) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  summarise_draws()
hfa5_mcmc_team_hfa

hfa5_mcmc_league_hfa <- hfa5_mcmc_fit |>
  spread_draws(league_hfa[season]) |>
  mutate(
    season = seasons[season]
  ) |>
  summarise_draws()
hfa5_mcmc_league_hfa

hfa5_team_hfa_total <- hfa5_mcmc_fit |>
  spread_draws(league_hfa[season], 
               team_hfa_dev[team],
               team_total_hfa[team, season]) |>
  #mutate(team_hfa_total2 = team_hfa_dev + league_hfa) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  rename(team_hfa_total = team_total_hfa) |>
  summarise_draws()
hfa5_team_hfa_total



# hfa5_team_strength_end <- week_tbl |>
#   right_join(hfa5_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa5_team_strength_end <- hfa5_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa5_team_hfa_end <- hfa5_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa5_team_total <- bind_rows(
  hfa5_team_hfa_end,
  hfa5_team_strength_end
) |>
  fill(season, week_idx, .direction = "downup") |>
  select(season, week_idx, team, everything()) |>
  arrange(team) |>
  filter(variable %in% c("team_hfa_total", "team_strength")) |>
  mutate(
    variable = case_when(
      variable == "team_hfa_total" ~ "sp_hfa",
      variable == "team_strength" ~ "sp_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )

### 6.3.4 Plots ----
p <- hfa5_mcmc_team_strength |>
  left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
p
ggplotly(p, tooltip = "text")

pb <- ggplot_build(p)

# Join home/away strength
hfa5_game_strengths <- train_data |>
  left_join(
    hfa5_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, week_idx, home_strength = team_strength_mean),
    by = c("home_team", "week_idx")
  ) |>
  left_join(
    hfa5_team_strength |>
      pivot_wider(
        id_cols = c(team, week_idx),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(away_team = team, week_idx, away_strength = team_strength_mean),
    by = c("away_team", "week_idx")
  ) |>
  left_join(
    hfa5_team_hfa_total |>
      filter(variable == "team_hfa_total") |>
      pivot_wider(
        id_cols = c(team, season),
        names_from = variable,
        values_from = c(mean, median),
        names_glue = "{variable}_{.value}"
      ) |>
      select(home_team = team, season, home_hfa = team_hfa_total_mean),
    by = c("home_team", "season")
  ) |>
  mutate(
    pred_result = home_strength - away_strength + home_hfa*hfa,
    pred_error = pred_result - result,
    spread_error = spread_line - result
  )

# hfa5_game_strengths <- hfa5_team_hfa_total |>
#   filter(variable %in% c("team_hfa_total", "team_strength"))

ggplot(hfa5_game_strengths, aes(x = pred_result, y = result)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Actual Result (Home Margin)",
    title = "Model Spread vs Actual Result"
  )

ggplot(hfa5_game_strengths, aes(x = pred_result, y = pred_error)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 0, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Model Expected Spread - Actual Result (Home Margin)",
    title = "Model Spread vs Residuals"
  )

ggplot(hfa5_game_strengths, aes(x = pred_result, y = spread_line)) +
  geom_point(alpha = 0.4) +
  smplot2::sm_statCorr(r2 = TRUE, color = "blue", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 0.75) +
  labs(
    x = "Model Expected Spread",
    y = "Vegas Spread",
    title = "Model vs Vegas Spread"
  )

ggplot(hfa5_game_strengths, aes(x = pred_error)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    x = "Model Spread Error (Model - Actual)",
    y = "Count",
    title = "Distribution of Spread Errors"
  )

hfa5_metrics <- tibble(
  MAE   = mae(hfa5_game_strengths$result, hfa5_game_strengths$pred_result),
  RMSE  = rmse(hfa5_game_strengths$result, hfa5_game_strengths$pred_result),
  Bias  = mean(hfa5_game_strengths$pred_error),
  SD_Error = sd(hfa5_game_strengths$pred_error)
)
hfa5_metrics_vegas <- tibble(
  MAE   = mae(hfa5_game_strengths$result, hfa5_game_strengths$spread_line),
  RMSE  = rmse(hfa5_game_strengths$result, hfa5_game_strengths$spread_line),
  Bias  = mean(hfa5_game_strengths$spread_error),
  SD_Error = sd(hfa5_game_strengths$spread_error)
)
hfa5_metrics <- hfa5_metrics |>
  mutate(Metric = "Model4", .before = 1) |>
  bind_rows(
    hfa5_metrics_vegas |>
      mutate(Metric = "Vegas", .before = 1)
  ) |>
  mutate(Model = "hfa5", .before = 1)

hfa_metrics <- bind_rows(
  hfa3_metrics,
  hfa5_metrics
)
print(hfa_metrics)

hfa5_league_hfa |>
  ggplot(aes(x = season, y = mean)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "blue", alpha = 0.2) +
  scale_x_continuous(
    name = "Season",
    breaks = seasons,  # Show season boundaries on x-axis
    minor_breaks = FALSE,
    labels = seasons
  ) +
  labs(title = "League-wide Home Field Advantage by Season",
       subtitle = "Model hfa5",
       x = "Season",
       y = "League HFA (mean ± 90% interval)") +
  theme_minimal()

## 6.4 MLE ----
hfa5_mle_fit <- hfa5_mod$optimize(
  data = hfa5_stan_data,
  #output_dir = hfa5_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa5_seed
)

hfa5_mle_fit_vars <- hfa5_mle_fit$metadata()$stan_variable_sizes
hfa5_mle_fit_vars
hfa5_mle_fit$output()
hfa5_mle_sum <- hfa5_mle_fit$summary(
  variables = subset(names(hfa5_mle_fit_vars),
                     hfa5_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa5_mle_sum, n = Inf)

hfa5_mle_team_strength <- hfa5_mle_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_mle",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5_mle_league_hfa <- hfa5_mle_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_mle",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5_mle_team_hfa <- hfa5_mle_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_mle_team_total_hfa <- hfa5_mle_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_mle_team_estimates <- hfa5_mle_team_strength |>
  left_join(week_tbl_hfa5) |>
  left_join(hfa5_mle_league_hfa) |>
  left_join(hfa5_mle_team_hfa_dev) |>
  left_join(hfa5_mle_team_total_hfa) |>
  relocate(contains("mle"), .after = last_col())

## 6.5 MAP ----
hfa5_map_fit <- hfa5_mod$optimize(
  data = hfa5_stan_data,
  #output_dir = hfa5_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa5_seed
)
hfa5_map_fit_vars <- hfa5_map_fit$metadata()$stan_variable_sizes
hfa5_map_fit_vars
hfa5_map_fit$output()
hfa5_map_sum <- hfa5_map_fit$summary(
  variables = subset(names(hfa5_map_fit_vars),
                     hfa5_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa5_map_sum, n = Inf)

hfa5_map_team_strength <- hfa5_map_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_map",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5_map_league_hfa <- hfa5_map_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_map",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5_map_team_hfa <- hfa5_map_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_map_team_total_hfa <- hfa5_map_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_map_team_estimates <- hfa5_map_team_strength |>
  left_join(week_tbl_hfa5) |>
  left_join(hfa5_map_league_hfa) |>
  left_join(hfa5_map_team_hfa_dev) |>
  left_join(hfa5_map_team_total_hfa) |>
  relocate(contains("map"), .after = last_col())


## 6.6 Variational ----
hfa5_vi_fit <- hfa5_mod$variational(
  data = hfa5_stan_data,
  #init = 0,
  iter = 20000,
  draws = hfa5_iters*hfa5_chains,
  seed = hfa5_seed
)

hfa5_vi_fit_vars <- hfa5_vi_fit$metadata()$stan_variable_sizes
hfa5_vi_fit_vars
hfa5_vi_fit$output()
hfa5_vi_sum <- hfa5_vi_fit$summary(
  variables = subset(names(hfa5_vi_fit_vars),
                     hfa5_vi_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5_vi_sum, n = Inf)

hfa5_vi_team_strength <- hfa5_vi_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_vi",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5_vi_league_hfa <- hfa5_vi_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_vi",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5_vi_team_hfa <- hfa5_vi_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = season[season]
  )

hfa5_vi_team_total_hfa <- hfa5_vi_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_vi_team_estimates <- hfa5_vi_team_strength |>
  left_join(week_tbl_hfa5) |>
  left_join(hfa5_vi_league_hfa) |>
  left_join(hfa5_vi_team_hfa_dev) |>
  left_join(hfa5_vi_team_total_hfa) |>
  relocate(contains("vi"), .after = last_col())

## 6.7 Laplace ----
hfa5_lap_fit <- hfa5_mod$laplace(
  data = hfa5_stan_data,
  init = 0,
  jacobian = TRUE,
  draws = hfa5_iters,
  seed = hfa5_seed
)

hfa5_lap_fit_vars <- hfa5_lap_fit$metadata()$stan_variable_sizes
hfa5_lap_fit_vars
hfa5_lap_fit$output()
hfa5_lap_sum <- hfa5_lap_fit$summary(
  variables = subset(names(hfa5_lap_fit_vars),
                     hfa5_lap_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5_lap_sum, n = Inf)

hfa5_lap_team_strength <- hfa5_lap_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_lap",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5_lap_league_hfa <- hfa5_lap_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_lap",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5_lap_team_hfa_dev <- hfa5_lap_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_lap",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa5_lap_team_total_hfa <- hfa5_lap_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_lap",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5_lap_team_estimates <- hfa5_lap_team_strength |>
  left_join(week_tbl_hfa5) |>
  left_join(hfa5_lap_league_hfa) |>
  left_join(hfa5_lap_team_hfa_dev) |>
  left_join(hfa5_lap_team_total_hfa) |>
  relocate(contains("lap"), .after = last_col())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 7. MODEL hfa5A ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2006, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))
train_data


## 7.1 Compile Model ----
hfa5A_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa5A.stan"
)
hfa5A_mod_variables <- hfa5A_mod$variables()

## 7.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
# For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
week_tbl_season_map <- setNames(seq_along(seasons), seasons)

week_tbl_hfa5A <- week_tbl |>
  mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
  filter(week_idx %in% weeks)

week_season <- week_tbl_hfa5A$season_id[match(weeks, week_tbl_hfa5A$week_idx)] # length N_weeks, 1-based

# 2. first/last week of each season (values are week_idx, NOT positions)
first_week_of_season <- week_tbl_hfa5A |>
  group_by(season_id) |>
  summarise(first = min(week_idx), .groups = "drop") |>
  pull(first) |>
  match(weeks)  # Get position in weeks

last_week_of_season <- week_tbl_hfa5A |>
  group_by(season_id) |>
  summarise(last = max(week_idx), .groups = "drop") |>
  pull(last) |>
  match(weeks)  # Get position in weeks

# 7. Stan data
hfa5A_stan_data <- list(
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

hfa5A_iters <- 2000
hfa5A_warmup <- 1000
hfa5A_chains <- 4
hfa5A_thin <- 1
hfa5A_sims <- ((hfa5A_iters)/hfa5A_thin)*hfa5A_chains
hfa5A_parallel_chains <- parallel::detectCores()
hfa5A_adapt_delta <- 0.95
hfa5A_max_treedeepth <- 10
hfa5A_seed <- 52

## 7.3 MCMC ----
### 7.3.1 Fit Model ----
system.time(
  hfa5A_mcmc_fit <- hfa5A_mod$sample(
    data = hfa5A_stan_data,
    #output_dir = hfa5A_output_dir,
    chains = hfa5A_chains,
    parallel_chains = hfa5A_parallel_chains,
    iter_sampling = hfa5A_iters, 
    iter_warmup = hfa5A_warmup,
    thin = hfa5A_thin,
    adapt_delta = hfa5A_adapt_delta, 
    max_treedepth = hfa5A_max_treedeepth,
    seed = hfa5A_seed
  )
)

### Save
hfa5A_mcmc_fit$save_object(file = "Model Fitting/stan_models/hfa5A_mcmc_fit_all.rds")

### 7.3.2 Diagnostics ----
hfa5A_mod_variables
hfa5A_mcmc_fit$sampler_diagnostics(format = "df")
hfa5A_mcmc_fit$cmdstan_diagnose()
hfa5A_mcmc_fit$diagnostic_summary()

hfa5A_mcmc_fit_vars <- hfa5A_mcmc_fit$metadata()$stan_variable_sizes
hfa5A_mcmc_fit_vars
hfa5A_mcmc_fit$output()
hfa5A_mcmc_sum <- hfa5A_mcmc_fit$summary(
  variables = subset(names(hfa5A_mcmc_fit_vars),
                     hfa5A_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5A_mcmc_sum, n = Inf)

### 6.3.3 Posterior ----
# hfa5A_mcmc_team_strength2 <- hfa5A_mcmc_fit |>
#   spread_draws(team_strength[team, week_idx]) |>
#   mutate(
#     team = teams[team],
#     week_idx = weeks[week_idx]
#   ) |>
#   summarise_draws()

hfa5A_mcmc_team_strength <- hfa5A_mcmc_fit$summary(
 variables = "team_strength"
)

hfa5A_mcmc_team_strength <- hfa5A_mcmc_team_strength |>
  mutate(
    variable = "team_strength",
    team = rep(teams, times = length(weeks)),
    week_idx = rep(weeks, each = length(teams)),
    .before = 1
  )

# hfa5A_mcmc_team_hfa <- hfa5A_mcmc_fit |>
#   spread_draws(team_hfa[team, season], ndraws = 1000) |>
#   mutate(
#     team = teams[team],
#     season = seasons[season]
#   ) |>
#   summarise_draws()
# hfa5A_mcmc_team_hfa

hfa5A_mcmc_team_hfa <- hfa5A_mcmc_fit$summary(
  variables = "team_hfa"
) |>
  mutate(
    variable = "team_hfa",
    team = rep(teams, times = length(seasons)),
    season = rep(seasons, each = length(teams)),
    .before = 1
  )

# hfa5A_mcmc_league_hfa <- hfa5A_mcmc_fit |>
#   spread_draws(league_hfa[season]) |>
#   mutate(
#     season = seasons[season]
#   ) |>
#   summarise_draws()
# hfa5A_mcmc_league_hfa

hfa5A_mcmc_league_hfa <- hfa5A_mcmc_fit$summary(
  variables = "league_hfa"
) |>
  mutate(
    variable = "league_hfa",
    season = seasons,
    .before = 1
  )

hfa5A_team_hfa_total <- hfa5A_mcmc_fit |>
  spread_draws(league_hfa[season], 
               team_hfa_dev[team],
               team_total_hfa[team, season]) |>
  #mutate(team_hfa_total2 = team_hfa_dev + league_hfa) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  rename(team_hfa_total = team_total_hfa) |>
  summarise_draws()
hfa5A_team_hfa_total



# hfa5A_team_strength_end <- week_tbl |>
#   right_join(hfa5A_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa5A_team_strength_end <- hfa5A_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa5A_team_hfa_end <- hfa5A_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa5A_team_total <- bind_rows(
  hfa5A_team_hfa_end,
  hfa5A_team_strength_end
) |>
  fill(season, week_idx, .direction = "downup") |>
  select(season, week_idx, team, everything()) |>
  arrange(team) |>
  filter(variable %in% c("team_hfa_total", "team_strength")) |>
  mutate(
    variable = case_when(
      variable == "team_hfa_total" ~ "sp_hfa",
      variable == "team_strength" ~ "sp_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )

### 6.3.4 Plots ----
p <- hfa5A_mcmc_team_strength |>
  left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
p
ggplotly(p, tooltip = "text")

p <- hfa5A_mcmc_team_hfa |>
  #left_join(week_tbl) |>
  ggplot(
    aes(x = season, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", mean), "\n",
          "season: ", season, "\n"
          #"week: ", week, "\n",
          #"week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength (SRS)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
p
ggplotly(p, tooltip = "text")

## 7.4 MLE ----
hfa5A_mle_fit <- hfa5A_mod$optimize(
  data = hfa5A_stan_data,
  #output_dir = hfa5A_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa5A_seed
)

hfa5A_mle_fit_vars <- hfa5A_mle_fit$metadata()$stan_variable_sizes
hfa5A_mle_fit_vars
hfa5A_mle_fit$output()
hfa5A_mle_sum <- hfa5A_mle_fit$summary(
  variables = subset(names(hfa5A_mle_fit_vars),
                     hfa5A_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa5A_mle_sum, n = Inf)

hfa5A_mle_team_strength <- hfa5A_mle_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_mle",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5A_mle_league_hfa <- hfa5A_mle_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_mle",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5A_mle_team_hfa <- hfa5A_mle_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_mle_team_total_hfa <- hfa5A_mle_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_mle",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_mle_team_estimates <- hfa5A_mle_team_strength |>
  left_join(week_tbl_hfa5A) |>
  left_join(hfa5A_mle_league_hfa) |>
  left_join(hfa5A_mle_team_hfa_dev) |>
  left_join(hfa5A_mle_team_total_hfa) |>
  relocate(contains("mle"), .after = last_col())

## 7.5 MAP ----
hfa5A_map_fit <- hfa5A_mod$optimize(
  data = hfa5A_stan_data,
  #output_dir = hfa5A_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa5A_seed
)
hfa5A_map_fit_vars <- hfa5A_map_fit$metadata()$stan_variable_sizes
hfa5A_map_fit_vars
hfa5A_map_fit$output()
hfa5A_map_sum <- hfa5A_map_fit$summary(
  variables = subset(names(hfa5A_map_fit_vars),
                     hfa5A_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa5A_map_sum, n = Inf)

hfa5A_map_team_strength <- hfa5A_map_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_map",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5A_map_league_hfa <- hfa5A_map_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_map",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5A_map_team_hfa <- hfa5A_map_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_map_team_total_hfa <- hfa5A_map_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_map",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_map_team_estimates <- hfa5A_map_team_strength |>
  left_join(week_tbl_hfa5A) |>
  left_join(hfa5A_map_league_hfa) |>
  left_join(hfa5A_map_team_hfa_dev) |>
  left_join(hfa5A_map_team_total_hfa) |>
  relocate(contains("map"), .after = last_col())


## 7.6 Variational ----
hfa5A_vi_fit <- hfa5A_mod$variational(
  data = hfa5A_stan_data,
  #init = 0,
  iter = 20000,
  draws = hfa5A_iters*hfa5A_chains,
  seed = hfa5A_seed
)

hfa5A_vi_fit_vars <- hfa5A_vi_fit$metadata()$stan_variable_sizes
hfa5A_vi_fit_vars
hfa5A_vi_fit$output()
hfa5A_vi_sum <- hfa5A_vi_fit$summary(
  variables = subset(names(hfa5A_vi_fit_vars),
                     hfa5A_vi_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5A_vi_sum, n = Inf)

hfa5A_vi_team_strength <- hfa5A_vi_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_vi",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5A_vi_league_hfa <- hfa5A_vi_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_vi",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5A_vi_team_hfa <- hfa5A_vi_fit$draws(
  variables = c("team_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = season[season]
  )

hfa5A_vi_team_total_hfa <- hfa5A_vi_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_vi",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_vi_team_estimates <- hfa5A_vi_team_strength |>
  left_join(week_tbl_hfa5A) |>
  left_join(hfa5A_vi_league_hfa) |>
  left_join(hfa5A_vi_team_hfa_dev) |>
  left_join(hfa5A_vi_team_total_hfa) |>
  relocate(contains("vi"), .after = last_col())

## 7.7 Laplace ----
hfa5A_lap_fit <- hfa5A_mod$laplace(
  data = hfa5A_stan_data,
  init = 0,
  jacobian = TRUE,
  draws = hfa5A_iters,
  seed = hfa5A_seed
)

hfa5A_lap_fit_vars <- hfa5A_lap_fit$metadata()$stan_variable_sizes
hfa5A_lap_fit_vars
hfa5A_lap_fit$output()
hfa5A_lap_sum <- hfa5A_lap_fit$summary(
  variables = subset(names(hfa5A_lap_fit_vars),
                     hfa5A_lap_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa5A_lap_sum, n = Inf)

hfa5A_lap_team_strength <- hfa5A_lap_fit$draws(
  variables = c("team_strength"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "week_idx"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_strength_lap",
    names_transform = list(team = as.integer,
                           week_idx = as.integer)
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  )

hfa5A_lap_league_hfa <- hfa5A_lap_fit$draws(
  variables = c("league_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "season"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "league_hfa_lap",
    names_transform = list(season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    season = seasons[season]
  )

hfa5A_lap_team_hfa_dev <- hfa5A_lap_fit$draws(
  variables = c("team_hfa_dev"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team"),
    names_pattern = "(.*)\\[(\\d+)\\]",
    values_to = "team_hfa_lap",
    names_transform = list(team = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team]
  )

hfa5A_lap_team_total_hfa <- hfa5A_lap_fit$draws(
  variables = c("team_total_hfa"),
  format = "df"
) |>
  pivot_longer(
    everything(),
    names_to = c("param", "team", "season"),
    names_pattern = "(.*)\\[(\\d+),(\\d+)\\]",
    values_to = "team_total_hfa_lap",
    names_transform = list(team = as.integer,
                           season = as.integer),
    values_drop_na = TRUE
  ) |>
  filter(!is.na(param)) |>
  select(-param) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  )

hfa5A_lap_team_estimates <- hfa5A_lap_team_strength |>
  left_join(week_tbl_hfa5A) |>
  left_join(hfa5A_lap_league_hfa) |>
  left_join(hfa5A_lap_team_hfa_dev) |>
  left_join(hfa5A_lap_team_total_hfa) |>
  relocate(contains("lap"), .after = last_col())