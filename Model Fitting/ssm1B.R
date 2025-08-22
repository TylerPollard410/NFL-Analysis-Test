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

# Inits builder (seed key scalars with previous posterior means)
make_inits <- function(fit, snapshot) {
  var_sizes <- fit$metadata()$stan_variable_sizes
  last_s <- snapshot[["last_season_idx"]]
  last_w <- snapshot[["last_week_idx"]]
  
  param_vars <-c(
    "league_hfa_z",
    "league_hfa_init",
    "team_hfa_z",
    "z_start",
    "z_w"
  )
  draws <- fit$draws(
    format = "df",
    variables = param_vars
  )
  
  list(
    # beta_w = mean(snapshot$beta_w),
    # sigma_w = mean(snapshot$sigma_w),
    # beta_s = mean(snapshot$beta_s),
    # sigma_s = mean(snapshot$sigma_s),
    # beta_hfa = mean(snapshot$beta_hfa),
    # sigma_hfa = mean(snapshot$sigma_hfa),
    # sigma_team_hfa = mean(snapshot$sigma_team_hfa),
    # sigma_y = mean(snapshot$sigma_y),
    
    league_hfa_z = draws |>
      select(contains("league_hfa_z")) |>
      colMeans() |>
      unname() |> 
      head(n = last_s),
    #league_hfa_z = rep(0, var_sizes[["league_hfa_z"]][1]),
    league_hfa_init = draws |>
      select(contains("league_hfa_init")) |>
      colMeans() |>
      unname(),
    #league_hfa_init = rep(0, var_sizes[["league_hfa_init"]][1]),
    
    beta_hfa = mean(snapshot$beta_hfa),
    sigma_hfa = mean(snapshot$sigma_hfa),
    
    team_hfa_z = draws |>
      select(contains("team_hfa_z")) |>
      colMeans() |>
      matrix(nrow = var_sizes[["team_hfa_z"]][1],
             ncol = var_sizes[["team_hfa_z"]][2],
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t() |> 
      head(n = last_s),
    # team_hfa_z = matrix(0.0, nrow = var_sizes[["team_hfa_z"]][1], 
    #                     ncol = var_sizes[["team_hfa_z"]][2],
    #                     byrow = FALSE),
    sigma_team_hfa = mean(snapshot$sigma_team_hfa),
    
    z_start = draws |>
      select(contains("z_start")) |>
      colMeans() |>
      matrix(nrow = var_sizes[["z_start"]][1],
             ncol = var_sizes[["z_start"]][2],
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t() |> 
      head(n = last_s),
    # z_start = matrix(0.0, nrow = var_sizes[["z_start"]][1],
    #                  ncol = var_sizes[["z_start"]][2],
    #                  byrow = FALSE),
    z_w = draws |>
      select(contains("z_w")) |>
      colMeans() |>
      matrix(nrow = var_sizes[["z_w"]][1],
             ncol = var_sizes[["z_w"]][2],
             byrow = FALSE) |>
      t() |> scale(scale = FALSE) |> t() |> 
      head(n = last_w),
    # z_w = matrix(0.0, nrow = var_sizes[["z_w"]][1], 
    #              ncol = var_sizes[["z_w"]][2],
    #              byrow = FALSE),
    
    beta_w = mean(snapshot$beta_w),
    sigma_w = mean(snapshot$sigma_w),
    
    beta_s = mean(snapshot$beta_s),
    sigma_s = mean(snapshot$sigma_s),
    
    sigma_y = mean(snapshot$sigma_y)
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
# 3. TEST RUN ----
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
N_teams <- length(unique(game_fit_data_all$home_id))
N_seasons <- max(game_fit_data_all$season_idx)
N_weeks <- max(game_fit_data_all$week_idx)

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
