# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(tictoc)
# library(tidytext)
# library(MASS)
library(plotly)
library(smplot2)
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
all_seasons <- 2002:get_current_season()
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
github_data_repo <- "TylerPollard410/nflendzoneData"

## nflverse ----
teams_data <- load_teams(current = TRUE)
teams <- teams_data$team_abbr

### games ----
game_data <- load_game_data(seasons = all_seasons)
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

# tag <- "nfl_stats_week_team_regpost"
# nfl_stats_week_team_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))
# 
# tag <- "nfl_stats_week_player_regpost"
# nfl_stats_week_player_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))
# 
# tag <- "srs"
# srs_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

## Set up modeling data ----
#   - Use seasons 2007–2023 for training/validation
#   - Hold out seasons 2024 for out of sample weekly forecats
game_model_data <- game_data |> #game_model_data |>
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
    game_id, season, season_idx, week, week_idx = week_seq, 
    game_type, season_type,
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. HELPERS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Build Stan data list
make_stan_data <- function(df, start_season, start_week, end_season, end_week) {
  df_sub <- df |>
    filter(
      (season > start_season | (season == start_season & week >= start_week)) &
        (season < end_season | (season == end_season & week <= end_week))
    ) |>
    arrange(season, week)
  
  list(
    N_games = nrow(df_sub),
    N_obs = nrow(df_sub),
    N_teams = length(unique(df$home_id)),
    N_seasons = length(unique(df$season_idx)),
    N_weeks = max(df$week_idx, na.rm = TRUE),
    home_id = df_sub$home_id,
    away_id = df_sub$away_id,
    week_id = df_sub$week_idx,
    season_id = df_sub$season_idx,
    first_week_of_season = df |> 
      #filter(season_idx <= max(df_sub$season_idx)) |>
      group_by(season_idx) |> 
      summarise(min(week_idx)) |> pull(),
    last_week_of_season = df |> 
      #filter(season_idx <= max(df_sub$season_idx)) |>
      group_by(season_idx) |> 
      summarise(max(week_idx)) |> pull(),
    hfa = df_sub$hfa,
    result = df_sub$result,
    N_oos = 0,
    oos_idx = array(0, 0)
  )
}

# Build per‑season first/last week arrays aligned to season_idx
get_first_last_week <- function(df) {
  fw <- df |> group_by(season_idx) |> summarise(fw = min(week_idx), .groups = "drop")
  lw <- df |> group_by(season_idx) |> summarise(lw = max(week_idx), .groups = "drop")
  tibble(season_idx = sort(unique(df$season_idx))) |>
    left_join(fw, by = "season_idx") |>
    left_join(lw, by = "season_idx") |>
    arrange(season_idx)
}

# Week table (absolute season/week and their indices) to drive iteration
build_week_table <- function(df) {
  df |>
    distinct(season, week, season_idx, week_idx) |>
    arrange(season, week)
}

# Return the "next" (season, week) after an end boundary
next_week_after <- function(week_tbl, end_season, end_week) {
  idx <- which(week_tbl$season == end_season & week_tbl$week == end_week)
  if (length(idx) == 0 || idx == nrow(week_tbl)) return(NULL)
  week_tbl[idx + 1, c("season", "week", "season_idx", "week_idx")]
}

# Schedule builder for a specific (season, week)
schedule_for <- function(df, season, week) {
  df |>
    filter(season == !!season, week == !!week) |>
    select(home_id, away_id, week_id = week_idx, season_id = season_idx, hfa)
}

# Extract snapshot (kept as-is, but add last observed season/week indices)
extract_snapshot <- function(fit, last_season_idx, last_week_idx) {
  # var_sizes <- fit$metadata()$stan_variable_sizes
  
  param_vars <-c(
    "league_hfa_z",
    "league_hfa_init",
    "team_hfa_z",
    "z_start",
    "z_w"
  )
  
  snapshot_vars <-c(
    "beta_w","sigma_w","beta_s","sigma_s",
    "beta_hfa","sigma_hfa","sigma_team_hfa","sigma_y",
    "team_strength_last","team_hfa_last","league_hfa_last"
  )
  draws <- fit$draws(
    format = "df",
    variables = snapshot_vars
  )
  list(
    beta_w   = draws$beta_w,
    sigma_w  = draws$sigma_w,
    beta_s   = draws$beta_s,
    sigma_s  = draws$sigma_s,
    beta_hfa = draws$beta_hfa,
    sigma_hfa = draws$sigma_hfa,
    sigma_team_hfa = draws$sigma_team_hfa,
    sigma_y  = draws$sigma_y,
    team_strength_last = draws |> dplyr::select(starts_with("team_strength_last")),
    team_hfa_last      = draws |> dplyr::select(starts_with("team_hfa_last")),
    league_hfa_last    = draws$league_hfa_last,
    last_season_idx    = last_season_idx,
    last_week_idx      = last_week_idx
  )
}

# # Inits builder (seed key scalars with previous posterior means)
# make_inits <- function(fit, snapshot) {
#   var_sizes <- fit$metadata()$stan_variable_sizes
#   last_s <- snapshot[["last_season_idx"]]
#   last_w <- snapshot[["last_week_idx"]]
#   
#   param_vars <-c(
#     "league_hfa_z",
#     "league_hfa_init",
#     "team_hfa_z",
#     "z_start",
#     "z_w"
#   )
#   draws <- fit$draws(
#     format = "df",
#     variables = param_vars
#   )
#   
#   league_hfa_z <- draws |> select(contains("league_hfa_z")) |>
#     colMeans() |> 
#     unname() #|> 
#     # head(n = last_s) <--- removed this
#   league_hfa_init <- draws |> select(contains("league_hfa_init")) |>
#     colMeans() |> 
#     unname()
#   
#   team_hfa_z <- draws |> select(contains("team_hfa_z")) |> 
#     colMeans() |>
#     matrix(nrow = var_sizes[["team_hfa_z"]][1], 
#            ncol = var_sizes[["team_hfa_z"]][2], 
#            byrow = FALSE) |>
#     t() |> scale(scale = FALSE) |> t() #|> 
#   # head(n = last_s) <--- removed this
#   
#   z_start <- draws |> select(contains("z_start")) |> 
#     colMeans() |>
#     matrix(nrow = var_sizes[["z_start"]][1],
#            ncol = var_sizes[["z_start"]][2],
#            byrow = FALSE) |>
#     t() |> scale(scale = FALSE) |> t() #|> 
#   # head(n = last_s) <--- removed this
#   
#   z_w <- draws |>
#     select(contains("z_w")) |>
#     colMeans() |>
#     matrix(nrow = var_sizes[["z_w"]][1],
#            ncol = var_sizes[["z_w"]][2],
#            byrow = FALSE) |>
#     t() |> scale(scale = FALSE) |> t() #|> 
#   # head(n = last_w) <--- removed this
#   
#   list(
#     league_hfa_z = league_hfa_z,
#     league_hfa_init = league_hfa_init,
#     
#     beta_hfa = mean(snapshot$beta_hfa),
#     sigma_hfa = abs(mean(snapshot$sigma_hfa)),
#     
#     team_hfa_z = team_hfa_z,
#     sigma_team_hfa = abs(mean(snapshot$sigma_team_hfa)),
#     
#     z_start = z_start,
#     z_w = z_w,
#     
#     beta_w = mean(snapshot$beta_w),
#     sigma_w = mean(snapshot$sigma_w),
#     
#     beta_s = mean(snapshot$beta_s),
#     sigma_s = mean(snapshot$sigma_s),
#     
#     sigma_y = mean(snapshot$sigma_y)
#   )
# }

make_rolling_inits <- function(fit, snapshot, stan_data_next) {
  var_sizes <- fit$metadata()$stan_variable_sizes
  
  # Get fitted dimensions from snapshot
  last_s <- snapshot[["last_season_idx"]]  # Number of previously-fitted seasons
  last_w <- snapshot[["last_week_idx"]]    # Number of previously-fitted weeks
  
  # New dimensions from next fit (from stan_data)
  N_seasons <- stan_data_next$N_seasons
  N_weeks   <- stan_data_next$N_weeks
  N_teams   <- stan_data_next$N_teams
  
  # Helper for sum-to-zero normal
  rnorm_sumzero <- function(n) {
    x <- rnorm(n)
    x - mean(x)
  }
  
  # Get previous draws
  param_vars <- c("league_hfa_z", "league_hfa_init", "team_hfa_z", "z_start", "z_w")
  draws <- fit$draws(format = "df", variables = param_vars)
  
  # league_hfa_z: [N_seasons]
  league_hfa_z_prev <- suppressWarnings(
    draws |> dplyr::select(contains("league_hfa_z")) |> 
      colMeans() |> 
      unname()
  )
  league_hfa_z <- c(
    league_hfa_z_prev[1:last_s],
    if (N_seasons > last_s) rnorm(N_seasons - last_s) else NULL
  )
  
  # league_hfa_init: [scalar]
  league_hfa_init <- suppressWarnings(
    draws |> dplyr::select(contains("league_hfa_init")) |>
      colMeans() |>
      unname()
  )
  
  # team_hfa_z: [N_seasons, N_teams]
  team_hfa_z_prev <- suppressWarnings(
    draws |> select(contains("team_hfa_z")) |> 
      colMeans() |>
      matrix(nrow = var_sizes[["team_hfa_z"]][1], 
             ncol = var_sizes[["team_hfa_z"]][2], 
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t()
  )
  team_hfa_z <- matrix(NA, nrow = N_seasons, ncol = N_teams)
  if (last_s > 0) team_hfa_z[1:last_s, ] <- team_hfa_z_prev[1:last_s, , drop = FALSE]
  if (N_seasons > last_s) {
    for (s in (last_s+1):N_seasons) team_hfa_z[s, ] <- rnorm_sumzero(N_teams)
  }
  
  # z_start: [N_seasons, N_teams]
  z_start_prev <- suppressWarnings(
    draws |> select(contains("z_start")) |> 
      colMeans() |>
      matrix(nrow = var_sizes[["z_start"]][1], 
             ncol = var_sizes[["z_start"]][2], 
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t()
  )
  z_start <- matrix(NA, nrow = N_seasons, ncol = N_teams)
  if (last_s > 0) z_start[1:last_s, ] <- z_start_prev[1:last_s, , drop = FALSE]
  if (N_seasons > last_s) {
    for (s in (last_s+1):N_seasons) z_start[s, ] <- rnorm_sumzero(N_teams)
  }
  
  # z_w: [N_weeks, N_teams]
  z_w_prev <- suppressWarnings(
    draws |> select(contains("z_w")) |> 
      colMeans() |>
      matrix(nrow = var_sizes[["z_w"]][1], 
             ncol = var_sizes[["z_w"]][2], 
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t()
  )
  z_w <- matrix(NA, nrow = N_weeks, ncol = N_teams)
  if (last_w > 0) z_w[1:last_w, ] <- z_w_prev[1:last_w, , drop = FALSE]
  if (N_weeks > last_w) {
    for (w in (last_w+1):N_weeks) z_w[w, ] <- rnorm_sumzero(N_teams)
  }
  
  # Scalar params as before
  list(
    league_hfa_z = league_hfa_z,
    league_hfa_init = league_hfa_init,
    beta_hfa = mean(snapshot$beta_hfa),
    sigma_hfa = abs(mean(snapshot$sigma_hfa)),
    team_hfa_z = team_hfa_z,
    sigma_team_hfa = abs(mean(snapshot$sigma_team_hfa)),
    z_start = z_start,
    z_w = z_w,
    beta_w = mean(snapshot$beta_w),
    sigma_w = abs(mean(snapshot$sigma_w)),
    beta_s = mean(snapshot$beta_s),
    sigma_s = abs(mean(snapshot$sigma_s)),
    sigma_y = abs(mean(snapshot$sigma_y))
  )
}


# Forecast with ssm1_gq (now passes correct season/week bookkeeping)
forecast_ssm1 <- function(mod_gq, snapshot, schedule, n_draws,
                          first_week_of_season, last_week_of_season,
                          N_teams, N_seasons, N_weeks) {
  stopifnot(nrow(schedule) > 0)
  
  stan_data <- list(
    N_draws   = n_draws,
    N_teams   = N_teams,
    N_seasons = N_seasons,
    N_weeks   = N_weeks,
    current_season = snapshot$last_season_idx,
    current_week   = snapshot$last_week_idx,
    first_week_of_season = first_week_of_season,
    last_week_of_season  = last_week_of_season,
    
    beta_w   = snapshot$beta_w[1:n_draws],
    sigma_w  = snapshot$sigma_w[1:n_draws],
    beta_s   = snapshot$beta_s[1:n_draws],
    sigma_s  = snapshot$sigma_s[1:n_draws],
    beta_hfa = snapshot$beta_hfa[1:n_draws],
    sigma_hfa = snapshot$sigma_hfa[1:n_draws],
    sigma_team_hfa = snapshot$sigma_team_hfa[1:n_draws],
    sigma_y  = snapshot$sigma_y[1:n_draws],
    
    team_strength_T = as.matrix(snapshot$team_strength_last)[1:n_draws, , drop = FALSE],
    team_hfa_cur    = as.matrix(snapshot$team_hfa_last)[1:n_draws, , drop = FALSE],
    league_hfa_cur  = snapshot$league_hfa_last[1:n_draws],
    
    N_oos   = nrow(schedule),
    home_id = schedule$home_id,
    away_id = schedule$away_id,
    week_id = schedule$week_id,
    season_id = schedule$season_id,
    hfa     = schedule$hfa
  )
  
  mod_gq$sample(
    data = stan_data,
    fixed_param = TRUE,
    iter_sampling = 1,
    chains = 1,
    seed = 123
  )
}

# Tidy GQ output (mu_oos & y_pred) into long tibble keyed by draw & game index
tidy_gq_output <- function(gq_fit, schedule, add_cols = TRUE) {
  raw <- gq_fit$draws(variables = c("mu_oos","y_pred"), format = "df")
  long <- raw |>
    pivot_longer(
      cols = matches("^(mu_oos|y_pred)\\["),
      names_to = c("var","draw","game"),
      names_pattern = "^(mu_oos|y_pred)\\[(\\d+),(\\d+)\\]$",
      values_to = "value"
    ) |>
    mutate(
      draw = as.integer(draw),
      game = as.integer(game)
    ) |>
    pivot_wider(names_from = var, values_from = value) |>
    arrange(game, draw)
  if (add_cols) {
    sched_key <- schedule |>
      mutate(game = row_number()) |>
      select(game, season_id, week_id, home_id, away_id, hfa)
    long <- long |>
      left_join(sched_key, by = "game")
  }
  long
}

# Save helpers (paths like artifacts/ssm1_seq/forecast_YYYY_WW.rds)
.ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)
.artifact_path <- function(root, what, season, week) {
  file.path(root, sprintf("%s_%d_w%02d.rds", what, season, week))
}

save_snapshot <- function(snapshot, root, season, week) {
  .ensure_dir(root)
  saveRDS(snapshot, .artifact_path(root, "snapshot", season, week))
}

save_forecast <- function(forecast_tbl, root, season, week) {
  .ensure_dir(root)
  saveRDS(forecast_tbl, .artifact_path(root, "forecast", season, week))
}

# Fit wrapper (minor fix: require mod explicitly)
fit_ssm1 <- function(mod, stan_data, inits = NULL,
                     iter_warmup = 500, iter_sampling = 1000, chains = 4,
                     adapt_delta = 0.9, max_treedepth = 10) {
  mod$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    seed = 123,
    init = if (!is.null(inits)) inits else 0,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )
}

# One sequential step: (fit up-through end), forecast next week, save
sequential_step <- function(df, mod, mod_gq,
                            start_train, end_train,
                            n_draws_gq = 200,
                            save_root = NULL,
                            iter_warmup = 500, iter_sampling = 1000, chains = 4,
                            adapt_delta = 0.9, max_treedepth = 10,
                            prev_inits = NULL) {
  # Stan data for current training window
  stan_data <- make_stan_data(df,
                              start_train$season, start_train$week,
                              end_train$season,   end_train$week)
  
  # Fit
  fit <- fit_ssm1(
    mod, stan_data, inits = prev_inits,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth
  )
  
  # Bookkeeping
  week_tbl <- build_week_table(df)
  end_row  <- week_tbl |> filter(season == end_train$season, week == end_train$week)
  last_season_idx <- end_row$season_idx[[1]]
  last_week_idx   <- end_row$week_idx[[1]]
  
  # Snapshot
  snapshot <- extract_snapshot(fit, last_season_idx, last_week_idx)
  
  # Forecast the very next week
  nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
  if (is.null(nxt)) {
    forecast_tbl <- tibble()  # nothing to forecast
  } else {
    sched <- schedule_for(df, nxt$season, nxt$week)
    if (nrow(sched) == 0) {
      forecast_tbl <- tibble()
    } else {
      # Season/week arrays and sizes for gq
      fl <- get_first_last_week(df)
      N_teams   <- length(unique(df$home_id))
      N_seasons <- max(df$season_idx)
      N_weeks   <- max(df$week_idx)
      gq_fit <- forecast_ssm1(
        mod_gq, snapshot, sched, n_draws = n_draws_gq,
        first_week_of_season = fl$fw, last_week_of_season = fl$lw,
        N_teams = N_teams, N_seasons = N_seasons, N_weeks = N_weeks
      )
      forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE) |>
        mutate(forecast_season = nxt$season, forecast_week = nxt$week)
    }
  }
  
  # Save artifacts if requested
  if (!is.null(save_root)) {
    save_snapshot(snapshot, save_root, end_train$season, end_train$week)
    if (nrow(forecast_tbl) > 0) {
      save_forecast(forecast_tbl, save_root, nxt$season, nxt$week)
    }
  }
  
  list(
    fit = fit,
    snapshot = snapshot,
    forecast = forecast_tbl,
    next_pointer = if (is.null(nxt)) NULL else list(season = nxt$season, week = nxt$week),
    next_inits = make_inits(snapshot)
  )
}

# Full sequential runner from an initial window to a final end window (inclusive)
sequential_run <- function(df, mod, mod_gq,
                           start_train, end_train_initial, end_train_final,
                           n_draws_gq = 200,
                           save_root = NULL,
                           iter_warmup = 500, iter_sampling = 1000, chains = 4,
                           adapt_delta = 0.9, max_treedepth = 10) {
  
  week_tbl <- build_week_table(df)
  
  # Build list of (season, week) we will use as training endpoints
  start_row <- week_tbl |> filter(season == start_train$season, week == start_train$week)
  end0_row  <- week_tbl |> filter(season == end_train_initial$season, week == end_train_initial$week)
  endF_row  <- week_tbl |> filter(season == end_train_final$season, week == end_train_final$week)
  
  stopifnot(nrow(start_row) == 1, nrow(end0_row) == 1, nrow(endF_row) == 1)
  
  # Sequence of endpoints: start at end0, then step weekly until endF (inclusive)
  endpoints <- week_tbl |>
    filter(
      (season >  end_train_initial$season) |
        (season == end_train_initial$season & week >= end_train_initial$week)
    ) |>
    filter(
      (season <  end_train_final$season) |
        (season == end_train_final$season & week <= end_train_final$week)
    ) |>
    arrange(season, week)
  
  out_forecasts <- list()
  inits <- NULL
  last_fit <- NULL
  last_snapshot <- NULL
  
  # First fit exactly at end0 (this will also forecast the next week)
  step0 <- sequential_step(
    df, mod, mod_gq,
    start_train = start_train,
    end_train   = list(season = end_train_initial$season, week = end_train_initial$week),
    n_draws_gq = n_draws_gq,
    save_root = save_root,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth,
    prev_inits = inits
  )
  last_fit <- step0$fit
  last_snapshot <- step0$snapshot
  inits <- step0$next_inits
  if (nrow(step0$forecast) > 0) out_forecasts[[length(out_forecasts)+1]] <- step0$forecast
  
  # Now march forward: for each subsequent week in endpoints AFTER the first row
  if (nrow(endpoints) > 1) {
    for (r in 2:nrow(endpoints)) {
      endr <- endpoints[r, ]
      stepr <- sequential_step(
        df, mod, mod_gq,
        start_train = start_train,
        end_train   = list(season = endr$season, week = endr$week),
        n_draws_gq  = n_draws_gq,
        save_root   = save_root,
        iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
        adapt_delta = adapt_delta, max_treedepth = max_treedepth,
        prev_inits  = inits
      )
      last_fit <- stepr$fit
      last_snapshot <- stepr$snapshot
      inits <- stepr$next_inits
      if (nrow(stepr$forecast) > 0) out_forecasts[[length(out_forecasts)+1]] <- stepr$forecast
    }
  }
  
  list(
    last_fit = last_fit,
    last_snapshot = last_snapshot,
    all_forecasts = if (length(out_forecasts)) bind_rows(out_forecasts) else tibble()
  )
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. STEP-BY-STEP TEST: Sequential State-Space Model Fitting ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 3.1 Compile Stan Models ----

save_root <- "Model Fitting/stan_models"  # update if needed

mod_ssm1    <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1_gq.stan"))

## 3.2 Set initial fit window ----

start_train <- list(season = 2002, week = 1)
end_train   <- list(season = 2023, week = 10)

# Build week table for pointer logic (useful later)
week_tbl <- build_week_table(game_fit_data_all)
glimpse(week_tbl)
tail.matrix(week_tbl)

## 3.3 Prepare Stan Data for Initial Fit ----

stan_data_init <- make_stan_data(
  df = game_fit_data_all,
  start_season = start_train$season,
  start_week   = start_train$week,
  end_season   = end_train$season,
  end_week     = end_train$week
)
glimpse(stan_data_init)

## 3.4 Fit Initial Model ----

fit_init <- mod_ssm1$sample(
  data = stan_data_init,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = min(4, parallel::detectCores()),
  seed = 52,
  adapt_delta = 0.9,
  max_treedepth = 10,
  init = 0  # Let Stan initialize the first fit!
)
fit_init$time()

scalar_params <- mod_ssm1$variables()$parameters |>
  bind_rows(.id = "parameter") |>
  filter(dimensions == 0) |>
  pull(parameter)
print(scalar_params)

fit_init$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init$draws(variables = scalar_params))


### 3.4.1 Alternate Algorithms ----
# Fit same data using other cmdstanr model to help diagnose ssm1.stan

#### MLE 
fit_init_mle <- mod_ssm1$optimize(
  data = stan_data_init, seed = 52, init = 0,
  jacobian = FALSE
)

fit_init_mle$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_mle$draws(variables = scalar_params))

#### MAP 
fit_init_map <- mod_ssm1$optimize(
  data = stan_data_init, seed = 52, init = 0,
  jacobian = TRUE
)

fit_init_map$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_map$draws(variables = scalar_params))

#### Laplace MLE
fit_init_laplace_mle <- mod_ssm1$laplace(
  data = stan_data_init, seed = 52, init = 0,
  #mode = fit_init_mle, 
  jacobian = FALSE,
  draws = 4000
)

fit_init_laplace_mle$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_laplace_mle$draws(variables = scalar_params))

#### Laplace MAP
fit_init_laplace_map <- mod_ssm1$laplace(
  data = stan_data_init, seed = 52, init = 0,
  #mode = fit_init_map,
  jacobian = TRUE,
  draws = 4000
)

fit_init_laplace_map$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_laplace_map$draws(variables = scalar_params))

#### Automatic Differentiation Variational Inference (ADVI)
fit_init_variational <- mod_ssm1$variational(
  data = stan_data_init, seed = 52, init = 0,
  draws = 4000
)

fit_init_variational$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_variational$draws(variables = scalar_params))

#### Variational (Pathfinder)
fit_init_pathfinder <- mod_ssm1$pathfinder(
  data = stan_data_init, seed = 52, init = 0,
  draws = 4000
)

fit_init_pathfinder$summary(variables = scalar_params, .cores = 4)
mcmc_trace(fit_init_pathfinder$draws(variables = scalar_params))

# Save fit for future use
fit_init$save_object(file = file.path(save_root, "ssm1_initial_fit.rds"))

# Remove fit from environment to mimic real-life restart
rm(fit_init); gc()

## 3.5 Load Previous Fit ----

fit_prev <- readRDS(file.path(save_root, "ssm1_initial_fit.rds"))

## 3.6 Extract Final Snapshot From Fit ----

# Get the index of the last fitted season/week (matches end_train above)
end_row <- week_tbl |>
  filter(season == end_train$season, week == end_train$week)

last_season_idx <- end_row$season_idx[[1]]
last_week_idx   <- end_row$week_idx[[1]]
print(last_season_idx)
print(last_week_idx)


snapshot_prev <- extract_snapshot(
  fit = fit_prev,
  last_season_idx = last_season_idx,
  last_week_idx = last_week_idx
)
glimpse(snapshot_prev)

## 3.7 Build Inits For Next Fit Step (OLD) ---- 
# 
# # If running multi-chain, replicate the list for each chain!
# inits_next <- make_inits(fit_prev, snapshot_prev)
# glimpse(inits_next)
# inits_next_list <- rep(list(inits_next), 4)
# glimpse(inits_next_list)
# 
# # Should be TRUE:
# all(abs(rowSums(inits_next$team_hfa_z)) < 1e-8)
# all(abs(rowSums(inits_next$z_start)) < 1e-8)
# all(abs(rowSums(inits_next$z_w)) < 1e-8)
# 
# any(is.na(unlist(inits_next)))
# any(is.nan(unlist(inits_next)))
# any(is.infinite(unlist(inits_next)))

## 3.8 Identify Next Week To Fit ----

next_pointer <- next_week_after(
  week_tbl,
  end_train$season,
  end_train$week
)
print(next_pointer)
# (Check: if NULL, you’re at the very end of your data!)

# Build new training window
start_train_next <- start_train  # For rolling fits, may change; here keep same
end_train_next   <- list(season = next_pointer$season, week = next_pointer$week)
print(start_train_next)
print(end_train_next)

## 3.9 Forecast OOS for Next Week BEFORE Refitting ----

# You already have: next_pointer (week 11 after week 10), snapshot_prev from fit up through week 10.

# Get schedule for week 11 (OOS forecast)
sched_oos <- schedule_for(
  game_fit_data_all,
  season = next_pointer$season,
  week = next_pointer$week
)
print(sched_oos, n = Inf)

# Build first/last week arrays and model sizes
fl <- get_first_last_week(game_fit_data_all)
N_teams   <- length(unique(game_fit_data_all$home_id))
N_seasons <- max(game_fit_data_all$season_idx)
N_weeks   <- max(game_fit_data_all$week_idx)

print(fl, n = Inf)
print(N_teams)
print(N_seasons)
print(N_weeks)

stan_data_gq <- list(
  N_draws   = 200,
  N_teams   = N_teams,
  N_seasons = N_seasons,
  N_weeks   = N_weeks,
  current_season = snapshot_prev$last_season_idx,
  current_week   = snapshot_prev$last_week_idx,
  first_week_of_season = fl$fw,
  last_week_of_season  = fl$lw,
  beta_w   = snapshot_prev$beta_w[1:200],
  sigma_w  = snapshot_prev$sigma_w[1:200],
  beta_s   = snapshot_prev$beta_s[1:200],
  sigma_s  = snapshot_prev$sigma_s[1:200],
  beta_hfa = snapshot_prev$beta_hfa[1:200],
  sigma_hfa = snapshot_prev$sigma_hfa[1:200],
  sigma_team_hfa = snapshot_prev$sigma_team_hfa[1:200],
  sigma_y  = snapshot_prev$sigma_y[1:200],
  team_strength_T = as.matrix(snapshot_prev$team_strength_last)[1:200, , drop = FALSE],
  team_hfa_cur    = as.matrix(snapshot_prev$team_hfa_last)[1:200, , drop = FALSE],
  league_hfa_cur  = snapshot_prev$league_hfa_last[1:200],
  N_oos   = nrow(sched_oos),
  home_id = sched_oos$home_id,
  away_id = sched_oos$away_id,
  week_id = sched_oos$week_id,
  season_id = sched_oos$season_id,
  hfa     = sched_oos$hfa
)
glimpse(stan_data_gq)

# Run GQ forecast for week 11
if (nrow(sched_oos) > 0) {
  fit_gq <- mod_ssm1_gq$sample(
    data = stan_data_gq,
    fixed_param = TRUE,
    iter_sampling = 1,
    chains = 1,
    seed = 123
  )
  forecast_oos_tbl <- tidy_gq_output(fit_gq, sched_oos, add_cols = TRUE)
  print(head(forecast_oos_tbl))
} else {
  forecast_oos_tbl <- tibble()
  print("No games to forecast for week 11.")
}

## 3.10 Build Stan Data For Next Fit ----

stan_data_next <- make_stan_data(
  df = game_fit_data_all,
  start_season = start_train_next$season,
  start_week   = start_train_next$week,
  end_season   = end_train_next$season,
  end_week     = end_train_next$week
)
glimpse(stan_data_next)

## Build Inits For Next Fit Step (from snapshot) ---- #

# If running multi-chain, replicate the list for each chain!
inits_next <- make_rolling_inits(fit_prev, snapshot_prev, stan_data_next)
glimpse(inits_next)
inits_next_list <- rep(list(inits_next), 4)

# Should be TRUE:
all(abs(rowSums(inits_next$team_hfa_z)) < 1e-8)
all(abs(rowSums(inits_next$z_start)) < 1e-8)
all(abs(rowSums(inits_next$z_w)) < 1e-8)

any(is.na(unlist(inits_next)))
any(is.nan(unlist(inits_next)))
any(is.infinite(unlist(inits_next)))
init_fn <- function() make_rolling_inits(fit_prev, snapshot_prev, stan_data_next)
## 3.11 Fit Model For Next Window Using Custom Inits ----

fit_next <- mod_ssm1$sample(
  data = stan_data_next,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = min(4, parallel::detectCores()),
  seed = 52,
  adapt_delta = 0.9,
  max_treedepth = 10,
  init = init_fn
  #init = 0
  #init = inits_next_list
)
fit_next$time()
fit_next_inits <- fit_next$init()
fit_next$cmdstan_summary()
fit_next_inits_method <- fit_next$init_model_methods()
fit_next$save_object(file = file.path(save_root, "ssm1_next_fit.rds"))

## 3.12 Extract New Snapshot and Prepare for Next Step ----

# Indices for the new fit
end_row_next <- week_tbl |>
  filter(season == end_train_next$season, week == end_train_next$week)
last_season_idx_next <- end_row_next$season_idx[[1]]
last_week_idx_next   <- end_row_next$week_idx[[1]]

snapshot_next_init_list <- extract_snapshot(
  fit = fit_next,
  last_season_idx = last_season_idx_next,
  last_week_idx = last_week_idx_next
)


# What is the following week to forecast?
next_forecast_pointer <- next_week_after(
  week_tbl,
  end_train_next$season,
  end_train_next$week
)
if (!is.null(next_forecast_pointer)) {
  sched_next <- schedule_for(
    game_fit_data_all,
    season = next_forecast_pointer$season,
    week = next_forecast_pointer$week
  )
  if (nrow(sched_next) > 0) {
    # Build first/last week arrays
    fl <- get_first_last_week(game_fit_data_all)
    N_teams   <- length(unique(game_fit_data_all$home_id))
    N_seasons <- max(game_fit_data_all$season_idx)
    N_weeks   <- max(game_fit_data_all$week_idx)
    
    stan_data_gq <- list(
      N_draws   = 200,
      N_teams   = N_teams,
      N_seasons = N_seasons,
      N_weeks   = N_weeks,
      current_season = snapshot_next$last_season_idx,
      current_week   = snapshot_next$last_week_idx,
      first_week_of_season = fl$fw,
      last_week_of_season  = fl$lw,
      beta_w   = snapshot_next$beta_w[1:200],
      sigma_w  = snapshot_next$sigma_w[1:200],
      beta_s   = snapshot_next$beta_s[1:200],
      sigma_s  = snapshot_next$sigma_s[1:200],
      beta_hfa = snapshot_next$beta_hfa[1:200],
      sigma_hfa = snapshot_next$sigma_hfa[1:200],
      sigma_team_hfa = snapshot_next$sigma_team_hfa[1:200],
      sigma_y  = snapshot_next$sigma_y[1:200],
      team_strength_T = as.matrix(snapshot_next$team_strength_last)[1:200, , drop = FALSE],
      team_hfa_cur    = as.matrix(snapshot_next$team_hfa_last)[1:200, , drop = FALSE],
      league_hfa_cur  = snapshot_next$league_hfa_last[1:200],
      N_oos   = nrow(sched_next),
      home_id = sched_next$home_id,
      away_id = sched_next$away_id,
      week_id = sched_next$week_id,
      season_id = sched_next$season_id,
      hfa     = sched_next$hfa
    )
    
    # Run generated quantities forecast
    fit_gq <- mod_ssm1_gq$sample(
      data = stan_data_gq,
      fixed_param = TRUE,
      iter_sampling = 1,
      chains = 1,
      seed = 123
    )
    
    # Tidy GQ output for downstream evaluation
    forecast_tbl <- tidy_gq_output(fit_gq, sched_next, add_cols = TRUE)
  } else {
    forecast_tbl <- tibble()
  }
} else {
  forecast_tbl <- tibble()
}

## 3.13 (Optional) Inspect/Print Results ----

# View posterior summaries from new fit
print(fit_next$summary())

# View first few rows of forecast table
print(head(forecast_tbl))

# Inspect objects: check shapes, types, any warnings, etc.

# At this point you have manually run the entire sequential step as would be handled
# by `sequential_step()`, with **all helper function logic unwrapped**.



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. TEST RUN w FUNCTIONS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Define save directory for sequential artifacts
save_root <- "Model Fitting/stan_models"

# Load previous fit (ssm1.rds)
last_fit <- readRDS(file.path(save_root, "ssm1.rds"))

# Extract final snapshot from the existing fit
last_snapshot <- extract_snapshot(
  fit = last_fit,
  last_season_idx = max(game_fit_data_all$season_idx[game_fit_data_all$season == 2023 & game_fit_data_all$week == 10]),
  last_week_idx = max(game_fit_data_all$week_idx[game_fit_data_all$season == 2023 & game_fit_data_all$week == 10])
)

# Extract dimensions from full data for complete inits
# N_teams <- length(unique(game_fit_data_all$home_id))
# N_seasons <- max(game_fit_data_all$season_idx)
# N_weeks <- max(game_fit_data_all$week_idx)

# Create inits object for next fit step
last_inits <- make_inits(last_fit, last_snapshot)
init_list <- rep(list(last_inits), 4)

# Define start and end pointers for sequential update
start_train <- list(season = 2002, week = 1) # (keep original start)
end_train_initial <- list(season = 2023, week = 10) # already fit
end_train_final <- list(season = 2024, week = 22) # up to current season end

# Compile Stan models
mod_ssm1 <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1_gq.stan"))

# Run first continuation step from last snapshot and inits
week_tbl <- build_week_table(game_fit_data_all)
end_row <- week_tbl |> filter(season == end_train_initial$season, week == end_train_initial$week)
next_pointer <- next_week_after(week_tbl, end_train_initial$season, end_train_initial$week)

first0 <- sequential_step(
  df = game_fit_data_all,
  mod = mod_ssm1,
  mod_gq = mod_ssm1_gq,
  start_train = start_train,
  end_train = list(season = next_pointer$season, week = next_pointer$week),
  n_draws_gq = 200,
  save_root = save_root,
  iter_warmup = 500, iter_sampling = 1000,
  chains = 4,
  adapt_delta = 0.9, max_treedepth = 10,
  prev_inits = init_list
)


# Save final model, snapshot, and forecasts
saveRDS(first_step$fit, file.path(save_root, "ssm1_latest_fit.rds"))
saveRDS(first_step$snapshot, file.path(save_root, "ssm1_latest_snapshot.rds"))
saveRDS(first_step$forecast, file.path(save_root, "ssm1_all_forecasts.rds"))


df = game_fit_data_all
mod = mod_ssm1
mod_gq = mod_ssm1_gq
start_train = start_train
end_train = list(season = next_pointer$season, week = next_pointer$week)
n_draws_gq = 200
save_root = save_root
iter_warmup = 500
iter_sampling = 1000
chains = 4
adapt_delta = 0.9
max_treedepth = 10
prev_inits = NULL

df, start_season, start_week, end_season, end_week
df
start_season = start_train$season
start_week = start_train$week
end_season = end_train_initial$season
end_week =end_train_initial$week

stan_data <- make_stan_data(df,
                            start_train$season, start_train$week,
                            end_train_initial$season,   end_train_initial$week)

# Fit
fit <- fit_ssm1(
  mod, stan_data, inits = NULL,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth
)

# Bookkeeping
week_tbl <- build_week_table(df)
end_row  <- week_tbl |> filter(season == end_train$season, week == end_train$week)
last_season_idx <- end_row$season_idx[[1]]
last_week_idx   <- end_row$week_idx[[1]]

# Snapshot
snapshot <- extract_snapshot(fit, last_season_idx, last_week_idx)

# Forecast the very next week
nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
if (is.null(nxt)) {
  forecast_tbl <- tibble()  # nothing to forecast
} else {
  sched <- schedule_for(df, nxt$season, nxt$week)
  if (nrow(sched) == 0) {
    forecast_tbl <- tibble()
  } else {
    # Season/week arrays and sizes for gq
    fl <- get_first_last_week(df)
    N_teams   <- length(unique(df$home_id))
    N_seasons <- max(df$season_idx)
    N_weeks   <- max(df$week_idx)
    gq_fit <- forecast_ssm1(
      mod_gq, snapshot, sched, n_draws = n_draws_gq,
      first_week_of_season = fl$fw, last_week_of_season = fl$lw,
      N_teams = N_teams, N_seasons = N_seasons, N_weeks = N_weeks
    )
    forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE) |>
      mutate(forecast_season = nxt$season, forecast_week = nxt$week)
  }
}

# Save artifacts if requested
if (!is.null(save_root)) {
  save_snapshot(snapshot, save_root, end_train$season, end_train$week)
  if (nrow(forecast_tbl) > 0) {
    save_forecast(forecast_tbl, save_root, nxt$season, nxt$week)
  }
}

list(
  fit = fit,
  snapshot = snapshot,
  forecast = forecast_tbl,
  next_pointer = if (is.null(nxt)) NULL else list(season = nxt$season, week = nxt$week),
  next_inits = make_inits(snapshot)
)

mod
stan_data
inits = init_list
iter_warmup = 500
iter_sampling = 1000
chains = 4
adapt_delta = 0.9
max_treedepth = 10

fit2 <- mod$sample(
  data = stan_data,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  chains = chains,
  parallel_chains = min(chains, parallel::detectCores()),
  seed = 123,
  init = if (!is.null(inits)) inits else 0,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth
)

mle <- mod_ssm1$optimize(
  data = stan_data, seed = 52, iter = 10000, jacobian = FALSE
)

mle_sum <- mle$summary(variables = pars)

map <- mod_ssm1$optimize(
  data = stan_data, seed = 52, iter = 10000, jacobian = TRUE
)
map_sum <- map$summary(variables = pars)

vi <- mod_ssm1$variational(
  data = stan_data, seed = 52, iter = 10000, draws = 1000
)
vi_sum <- vi$summary(variables = pars)

lap_mle <- mod_ssm1$laplace(
  data = stan_data, seed = 52, jacobian = FALSE, draws = 1000
)
lap_mle_sum <- lap_mle$summary(variables = pars)

lap_map <- mod_ssm1$laplace(
  data = stan_data, seed = 52, jacobian = FALSE, draws = 1000
)
lap_map_sum <- lap_map$summary(variables = pars)

path <- mod_ssm1$pathfinder(
  data = stan_data, seed = 52
)
path_sum <- path$summary(variables = pars)


print(mle_sum, n = Inf)
print(map_sum, n = Inf)
print(vi_sum, n = Inf)
print(path_sum, n = Inf)
print(lap_mle_sum, n = Inf)
print(lap_map_sum, n = Inf)

mcmc_sum <- last_fit$summary(variables = pars)

comb_sum <- mcmc_sum |> rename_with(~paste0("mcmc_", .x), -variable) |>
  left_join(mle_sum |> rename_with(~paste0("mle_", .x), -variable)) |>
  left_join(map_sum |> rename_with(~paste0("map_", .x), -variable)) |>
  left_join(vi_sum |> rename_with(~paste0("vi_", .x), -variable)) |>
  left_join(path_sum |> rename_with(~paste0("path_", .x), -variable)) |>
  relocate(contains("estimate"), contains("mean"), .after = variable) |>
  mutate(across(-variable, ~round(.x, 4)))
  bind_cols(
  mle_sum |> rename_with(~paste0("mle_", .x), -variable),
  map_sum |> rename_with(~paste0("map_", .x), -variable),
  vi_sum |> rename_with(~paste0("vi_", .x), -variable),
  path_sum |> rename_with(~paste0("path_", .x), -variable)
)



