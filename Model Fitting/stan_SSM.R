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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. PLOT ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

season_breaks <- game_model_data |>
  rename(week_idx = week_seq) |>
  group_by(season) |>
  slice_min(week_idx, with_ties = FALSE) |>
  arrange(week_idx) |>
  select(season, week_idx)

team_plot_data <- game_fit_data |>
  clean_homeaway(invert = c("result", "spread_line", "hfa")) |>
  left_join(
    teams_data,
    by = join_by(team == team_abbr)
  )

team_colors <- teams_data$team_color
names(team_colors) <- teams_data$team_abbr
team_colors

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
    span = 0.1,
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
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Model Dictionary ----
# Keep running list of model description for saved .stan files
stan_model_dictionary <- tribble(
  ~model_name, ~data_format, ~description,
  "stan_model1", "game", "State-Space with varying team strengths and hfa by week for each team"
)

# These values can be set to any range you want for any experiment/backtest
first_train_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

## Compile Stan model ----
stan_model <- cmdstan_model("Model Fitting/stan_models/stan_model1.stan")
stan_variables <- stan_model$variables()

## Stan method variables ----

mod_seed = 52
mod_output_dir <- "Model Fitting/stan_models"

### MCMC ----
mod_iters <- 1000
mod_warmup <- 500
mod_chains <- 2
mod_thin <- 1
mod_sims <- ((mod_iters)/mod_thin)*mod_chains
mod_parallel_chains <- parallel::detectCores()
mod_adapt_delta <- 0.95
mod_max_treedeepth <- 15

### Optimize ----


## Stan Data ----
### Train data ----
train_data <- game_fit_data_all |>
  filter(week_idx >= first_train_week, week_idx <= current_train_max)

### Prediction Data ----
predict_data <- game_fit_data_all |>
  filter(week_idx == pred_week)

### Stan Indexing ----
fit_seasons    <- sort(unique(train_data$season))
fit_season_idx <- sort(unique(train_data$season_idx))
fit_weeks      <- sort(unique(train_data$week_idx))

week_tbl_rolling <- game_fit_data_all |>
  filter(season %in% fit_seasons) |>
  select(season, season_idx, week, week_idx) |>
  distinct() |>
  arrange(week_idx)

season_start_week <- week_tbl_rolling |>
  group_by(season_idx) |>
  summarise(start = min(week_idx), .groups = "drop") |>
  arrange(season_idx) |>
  pull(start)
season_end_week <- week_tbl_rolling |>
  group_by(season_idx) |>
  summarise(end = max(week_idx), .groups = "drop") |>
  arrange(season_idx) |>
  pull(end)

### Stan Data List ----
stan_data <- list(
  N_games = nrow(train_data),
  N_teams = length(teams),
  N_weeks = max(week_tbl_rolling$week_idx),
  N_seasons = length(fit_season_idx),
  game_week = train_data$week_idx,
  home_id = train_data$home_id,
  away_id = train_data$away_id,
  result = train_data$result,
  game_season = train_data$season_idx,
  week_season = week_tbl_rolling$season_idx,
  season_start_week = season_start_week,
  season_end_week   = season_end_week
)


## Run Models ----
### MCMC ----
fit_mcmc <- stan_model$sample(
  data = stan_data,
  output_dir = mod_output_dir,
  chains = mod_chains,
  parallel_chains = mod_parallel_chains,
  iter_sampling = mod_iters, 
  iter_warmup = mod_warmup,
  thin = mod_thin,
  adapt_delta = mod_adapt_delta, 
  max_treedepth = mod_max_treedeepth,
  seed = mod_seed
)

### Optimize ----
#### MLE -----
fit_mle <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  jacobian = FALSE,
  seed = mod_seed
)

#### MAP -----
fit_map <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  jacobian = TRUE,
  seed = mod_seed
)








tictoc::tic("Full OOS rolling fit")
#for (pred_week in seq(first_oos_week, last_oos_week)) {
  # --- Training data: all games up to the current max training week
  train_data <- game_fit_data_all |>
    filter(week_idx >= first_train_week, week_idx <= current_train_max)
  
  # --- Prediction data: all games in the prediction week (should only be 2024)
  predict_data <- game_fit_data_all |>
    filter(week_idx == pred_week)
  
  if (nrow(predict_data) == 0) {
    current_train_max <- pred_week
    next
  }
  
  # ----- Stan index construction -----
  fit_seasons    <- sort(unique(train_data$season))
  fit_season_idx <- sort(unique(train_data$season_idx))
  fit_weeks      <- sort(unique(train_data$week_idx))
  
  week_tbl_rolling <- game_fit_data_all |>
    filter(season %in% fit_seasons) |>
    select(season, season_idx, week, week_idx) |>
    distinct() |>
    arrange(week_idx)
  
  season_start_week <- week_tbl_rolling |>
    group_by(season_idx) |>
    summarise(start = min(week_idx), .groups = "drop") |>
    arrange(season_idx) |>
    pull(start)
  season_end_week <- week_tbl_rolling |>
    group_by(season_idx) |>
    summarise(end = max(week_idx), .groups = "drop") |>
    arrange(season_idx) |>
    pull(end)
  
  stan_data <- list(
    N_games = nrow(train_data),
    N_teams = length(teams),
    N_weeks = max(week_tbl_rolling$week_idx),
    N_seasons = length(fit_season_idx),
    game_week = train_data$week_idx,
    home_id = train_data$home_id,
    away_id = train_data$away_id,
    result = train_data$result,
    game_season = train_data$season_idx,
    week_season = week_tbl_rolling$season_idx,
    season_start_week = season_start_week,
    season_end_week   = season_end_week
  )
  
  # --- Stan fit
  fit <- stan_model$sample(
    data = stan_data,
    output_dir = "Model Fitting/stan_models",
    chains = mod_chains,
    parallel_chains = parallel::detectCores(),
    iter_sampling = mod_iters, 
    iter_warmup = mod_warmup,
    thin = mod_thin,
    adapt_delta = 0.95, 
    max_treedepth = 15,
    seed = 52
  )
  
  # --- Extract posterior draws using tidybayes for all OOS prediction games ---
  pred_week_num   <- unique(predict_data$week_idx)[1]
  pred_season_num <- unique(predict_data$season_idx)[1]
  draws <- fit$draws(c("strength", "hfa"))
  
  pred_long <- predict_data |>
    mutate(game_row = row_number()) |>
    select(game_row, game_id, home_id, away_id, season_idx) |>
    expand_grid(.draw = 1:posterior::ndraws(draws)) |>
    left_join(
      tidybayes::spread_draws(draws, strength[week, team]) |>
        filter(week == pred_week_num) |>
        rename(strength_home = .value),
      by = c(".draw", "home_id" = "team")
    ) |>
    left_join(
      tidybayes::spread_draws(draws, strength[week, team]) |>
        filter(week == pred_week_num) |>
        rename(strength_away = .value),
      by = c(".draw", "away_id" = "team")
    ) |>
    left_join(
      tidybayes::spread_draws(draws, hfa[team, season]) |>
        filter(season == pred_season_num) |>
        rename(hfa_home = .value),
      by = c(".draw", "home_id" = "team")
    ) |>
    mutate(
      pred_margin = strength_home - strength_away + hfa_home
    ) |>
    select(game_id, .draw, pred_margin)
  
  pred_summary <- pred_long |>
    group_by(game_id) |>
    summarise(
      pred_draws = list(pred_margin),
      .groups = "drop"
    ) |>
    left_join(predict_data, by = "game_id") |>
    mutate(
      pred_mean   = map_dbl(pred_draws, mean),
      pred_median = map_dbl(pred_draws, median),
      pred_p95    = map_dbl(pred_draws, ~quantile(.x, 0.95)),
      pred_p05    = map_dbl(pred_draws, ~quantile(.x, 0.05)),
      prob_cover  = map2_dbl(pred_draws, spread_line, ~mean(.x > .y))
    )
  
  oos_predictions[[as.character(pred_week)]] <- pred_summary
  message(glue::glue("OOS posterior draws saved for week {pred_week}"))
  
  # Expand window for next week
  current_train_max <- pred_week
#}
tictoc::toc()


fit$diagnostic_summary()
fit$save_object(file = "Model Fitting/stan_models/mod1.rds")

fit_mle <- stan_model$optimize(
  data = stan_data,
  output_dir = "Model Fitting/stan_models",
  chains = mod_chains,
  parallel_chains = parallel::detectCores(),
  iter_sampling = mod_iters, 
  iter_warmup = mod_warmup,
  thin = mod_thin,
  adapt_delta = 0.95, 
  max_treedepth = 15,
  seed = 52
)


# --- Extract posterior draws using tidybayes for all OOS prediction games ---
pred_week_num   <- unique(predict_data$week_idx)[1]
pred_season_num <- unique(predict_data$season_idx)[1]
draws <- fit$draws(c("strength", "hfa"))

pred_long <- predict_data |>
  mutate(game_row = row_number()) |>
  select(game_row, game_id, home_id, away_id, season_idx) |>
  expand_grid(.draw = 1:posterior::ndraws(draws)) |>
  left_join(
    tidybayes::spread_draws(draws, strength[week, team]) |>
      filter(week == pred_week_num) |>
      rename(strength_home = .value),
    by = c(".draw", "home_id" = "team")
  ) |>
  left_join(
    tidybayes::spread_draws(draws, strength[week, team]) |>
      filter(week == pred_week_num) |>
      rename(strength_away = .value),
    by = c(".draw", "away_id" = "team")
  ) |>
  left_join(
    tidybayes::spread_draws(draws, hfa[team, season]) |>
      filter(season == pred_season_num) |>
      rename(hfa_home = .value),
    by = c(".draw", "home_id" = "team")
  ) |>
  mutate(
    pred_margin = strength_home - strength_away + hfa_home
  ) |>
  select(game_id, .draw, pred_margin)

pred_summary <- pred_long |>
  group_by(game_id) |>
  summarise(
    pred_draws = list(pred_margin),
    .groups = "drop"
  ) |>
  left_join(predict_data, by = "game_id") |>
  mutate(
    pred_mean   = map_dbl(pred_draws, mean),
    pred_median = map_dbl(pred_draws, median),
    pred_p95    = map_dbl(pred_draws, ~quantile(.x, 0.95)),
    pred_p05    = map_dbl(pred_draws, ~quantile(.x, 0.05)),
    prob_cover  = map2_dbl(pred_draws, spread_line, ~mean(.x > .y))
  )

oos_predictions[[as.character(pred_week)]] <- pred_summary









# Combine all weeks for downstream analysis
oos_predictions_all <- bind_rows(oos_predictions, .id = "week_idx_pred")

# Now oos_predictions_all has:
#   - all posterior draws per game
#   - means/intervals/probabilities for each OOS forecast
#   - all original columns (game_id, teams, spread_line, result, etc.)
  
  # --- Extract draws for prediction games using tidybayes
  draws <- fit$draws(c("strength", "hfa"))
  pred_week_num <- unique(predict_data$week_idx)[1]
  pred_season_num <- unique(predict_data$season_idx)[1]
  
  # For each game in this week, get all posterior draws for the forecasted result
  pred_long <- predict_data |>
    mutate(game_row = row_number()) |>
    select(game_row, game_id, home_id, away_id, season_idx) |>
    tidybayes::expand_grid(.draw = 1:posterior::ndraws(draws)) |>
    left_join(
      tidybayes::spread_draws(draws, strength[week, team]) |>
        filter(week == pred_week_num) |>
        rename(strength_home = .value),
      by = c(".draw", "home_id" = "team")
    ) |>
    left_join(
      tidybayes::spread_draws(draws, strength[week, team]) |>
        filter(week == pred_week_num) |>
        rename(strength_away = .value),
      by = c(".draw", "away_id" = "team")
    ) |>
    left_join(
      tidybayes::spread_draws(draws, hfa[team, season]) |>
        filter(season == pred_season_num) |>
        rename(hfa_home = .value),
      by = c(".draw", "home_id" = "team")
    ) |>
    mutate(
      pred_margin = strength_home - strength_away + hfa_home
    ) |>
    select(game_id, .draw, pred_margin)
  
  pred_summary <- pred_long |>
    group_by(game_id) |>
    summarise(
      pred_draws = list(pred_margin),
      .groups = "drop"
    ) |>
    left_join(predict_data, by = "game_id")
  
  oos_predictions[[as.character(pred_week)]] <- pred_summary
  
  message(glue::glue("OOS posterior draws saved for week {pred_week}"))
  current_train_max <- pred_week
}

oos_predictions_all <- bind_rows(oos_predictions, .id = "week_idx_pred")

oos_predictions_all <- oos_predictions_all |>
  mutate(
    pred_mean = map_dbl(pred_draws, mean),
    pred_median = map_dbl(pred_draws, median),
    pred_p95 = map_dbl(pred_draws, ~quantile(.x, 0.95)),
    pred_p05 = map_dbl(pred_draws, ~quantile(.x, 0.05)),
    prob_cover = map2_dbl(pred_draws, spread_line, ~mean(.x > .y))
  )

# oos_predictions_all is now your full OOS results table



## Define Stan Data Variables ----
N_teams   <- length(teams)
N_seasons <- n_distinct(week_tbl$season)
N_weeks   <- nrow(week_tbl)
week_season <- week_tbl$season_idx
season_start_week <- week_tbl |>
  group_by(season_idx) |>
  summarize(start = min(week_idx), .groups = "drop") |>
  pull(start)
season_end_week <- week_tbl |>
  group_by(season_idx) |>
  summarize(end = max(week_idx), .groups = "drop") |>
  pull(end)

## Sequential Out-of-Sample Forecasting for 2024 ----

# Compile Stan model (only needs to be done once)
mod <- cmdstan_model("Model Fitting/stan_models/stan_model1.stan")

# Identify 2024 season and week indices for OOS forecasting
forecast_year <- 2024
forecast_season_idx <- unique(game_fit_data$season_idx[game_fit_data$season == forecast_year])
forecast_weeks <- week_tbl |> filter(season == forecast_year) |> pull(week_idx)

# Initialize list to store predictions for each week
results_list <- vector("list", length(forecast_weeks))
names(results_list) <- forecast_weeks

### Set sampling parameters ----
mod_iters <- 1000
mod_warmup <- 500
mod_chains <- 2
mod_thin <- 1
mod_sims <- ((mod_iters - mod_warmup)/mod_thin)*mod_chains

tictoc::tic()
for (i in seq_along(forecast_weeks)) {
  wk_idx <- forecast_weeks[i]
  last_week <- wk_idx - 1
  
  # Training data: all games up to (but not including) this week
  train_data <- game_fit_data |> filter(week_idx < wk_idx)
  
  # Games to forecast: current week of 2024
  forecast_games <- game_fit_data |> filter(week_idx == wk_idx, season == forecast_year)
  
  # Defensive: only fit if at least 1 row of training data, and last_week >= 1
  if (nrow(train_data) > 0 && last_week >= 1) {
    
    # Prepare Stan data for this fit
    stan_data <- list(
      N_games = nrow(train_data),
      N_teams = N_teams,
      N_weeks = N_weeks,
      N_seasons = N_seasons,
      game_week = train_data$week_idx,
      home_id = train_data$home_id,
      away_id = train_data$away_id,
      result = train_data$result,
      game_season = train_data$season_idx,
      week_season = week_season,
      season_start_week = season_start_week,
      season_end_week = season_end_week
    )
    
    # Fit Stan model (adjust iter/warmup for dev vs. final use)
    fit <- mod$sample(
      data = stan_data,
      output_dir = "Model Fitting/stan_models",
      chains = mod_chains,
      parallel_chains = parallel::detectCores(),
      iter_sampling = mod_iters, 
      iter_warmup = mod_warmup,
      thin = mod_thin,
      adapt_delta = 0.95, 
      max_treedepth = 15,
      init = 0.1,
      seed = 52
    )
    
    draws <- fit$draws()
    pred_strength <- posterior::as_draws_array(draws, variable = "strength") |> 
      apply(c(2,3), mean)
    pred_hfa <- posterior::as_draws_array(draws, variable = "hfa") |> 
      apply(c(2,3), mean)
    
    preds <- forecast_games |> mutate(
      pred_mu = pred_strength[home_id, last_week] - pred_strength[away_id, last_week] + pred_hfa[home_id, season_idx]
    )
    results_list[[i]] <- preds
    cat(sprintf("Week_idx %d forecast complete (%d games)\n", wk_idx, nrow(preds)))
  } else {
    cat(sprintf("Skipping week_idx %d: insufficient training data (n = %d) or invalid last_week (%d)\n", wk_idx, nrow(train_data), last_week))
    results_list[[i]] <- NULL
  }
}
tictoc::toc()

# Combine all OOS forecasts for 2024 into a single data frame
all_forecasts_2024 <- bind_rows(results_list)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL DIAGNOSTICS, POSTERIOR CHECKS, & PERFORMANCE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## --- 3.1: Summarize and check MCMC convergence ----
fit$summary("mu")
fit$summary("sigma")
fit$summary(c("sigma", 
              "tau_week", "tau_season",
              "rho_week", "rho_season", 
              "hfa_league", "tau_hfa"))

## Summarize last fit (useful if you want the most recent fit's diagnostics)
# Extract all team strengths for the final week
last_week <- N_weeks
strength_vars <- paste0("strength[", 1:32, ",", last_week, "]")
strength_summary <- fit$summary(variables = "strength") 
strength_summary <- strength_summary |> 
  mutate(team = rep(teams, N_weeks))  |>
  relocate(team, .before = 1)

# Compute posterior means for each team in the last week
strength_means <- colMeans(strength_mat)

# Center strengths so mean is zero (identifiability)
strength_means_centered <- strength_means - mean(strength_means)
names(strength_means_centered) <- teams

# Final season HFA
N_seasons <- max(week_tbl$season_idx)
hfa_vars <- paste0("hfa[", 1:32, ",", N_seasons, "]")
hfa_summary <- fit$summary(variables = "hfa")
hfa_summary <- hfa_summary |>
  mutate(team = rep(teams, N_seasons)) |>
  relocate(team, .before = 1)
print(hfa_summary, n = 32)

## Check convergence for all parameters
# Quick Rhat and ESS checks
bad_rhat <- last_fit_summary |> filter(rhat > 1.05)
bad_ess  <- last_fit_summary |> filter(ess_bulk < 400 | ess_tail < 400)

if (nrow(bad_rhat) > 0) cat("Parameters with high Rhat (>1.05):\n", bad_rhat$variable, "\n")
if (nrow(bad_ess) > 0) cat("Parameters with low effective sample size (<400):\n", bad_ess$variable, "\n")

## --- 3.2: Traceplots and marginal distributions (bayesplot) ----

# Example: plot traces for a few key parameters (last fit)
draws_array <- fit$draws(format = "draws_array")

# Example for sigma, tau_week, tau_season, hfa_league
bayesplot::mcmc_trace(draws_array, pars = c("sigma", "tau_week", "tau_season", "hfa_league"))
bayesplot::mcmc_dens_overlay(draws_array, pars = c("sigma", "tau_week", "tau_season", "hfa_league"))

# Example: traceplots for first team's strength in the last week
n_teams <- dim(draws_array)$variable[["strength[1,1]"]]
bayesplot::mcmc_trace(draws_array, pars = c("strength[1,1]", "strength[1,10]", "strength[1,20]"))

## --- 3.3: Posterior Predictive Checks ----

# You saved y_rep in Stan (generated quantities)
# Extract from the last fit, plot predictive checks
y_rep <- fit$draws("y_rep") |> posterior::as_draws_matrix()
y_rep <- as.matrix(y_rep)

# For actual OOS games: (if you want PPC for all training games, use train_data$result)
# Let's use the last week's forecasted games:
y_actual <- train_data$result

# PPC: overlay posterior predictive for these games
samps <- samp
bayesplot::ppc_dens_overlay(y_actual, y_rep[, 1:ncol(as.matrix(y_rep))]) # adjust columns to match y_actual length

## --- 3.4: Summarize Posterior for Key Parameters (posterior, tidybayes) ----

# Posterior mean, sd, intervals for core parameters
fit$summary(variables = c("sigma", "tau_week", "tau_season", "rho_week", "rho_season", "hfa_league"))

# Example: posterior draws for first 3 teams' strength, last week
as_draws_df(fit$draws("strength")) |>
  pivot_longer(everything(), names_to = "param", values_to = "value") |>
  filter(str_detect(param, "strength\\[(1|2|3),")) |>
  ggplot(aes(x = value, fill = param)) +
  geom_density(alpha = 0.6) +
  labs(title = "Posterior distributions for first 3 teams, final week strength") +
  theme_minimal()

## --- 3.5: Performance metrics for all 2024 OOS games ----

# Already calculated in prior code, but here's a summary again:
all_forecasts_2024 |>
  summarize(
    MAE = Metrics::mae(result, pred_mu),
    RMSE = Metrics::rmse(result, pred_mu),
    Coverage80 = mean(result >= pred_lower_80 & result <= pred_upper_80, na.rm = TRUE),
    Coverage95 = mean(result >= pred_lower_95 & result <= pred_upper_95, na.rm = TRUE),
    n = n()
  ) |> print()

## --- 3.6: Interactive exploration (optional) ----
# Use bayesplot's MCMC interactive for trace and pair plots:
# mcmc_intervals(draws_array, pars = c("sigma", "tau_week", "tau_season", "hfa_league"))
# mcmc_pairs(draws_array, pars = c("sigma", "tau_week", "tau_season"))

## --- 3.7: Save outputs for later analysis ----
# saveRDS(fit, "stan_fit_last_week.rds")
# saveRDS(all_forecasts_2024, "all_forecasts_2024.rds")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# END OF DIAGNOSTICS AND EVALUATION
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

