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
# 2. MODEL hfa6 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2006, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
# first_oos_week <- 
#   game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
# last_oos_week <- 
#   game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))
#train_data


## 2.1 Compile Model ----
hfa6_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa6.stan"
)
#hfa6_mod_variables <- hfa6_mod$variables()

## 2.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# 1. week_season: length N_weeks, maps each week (in 'weeks') to a season index (1-based)
#For each week_idx in 'weeks', find season_idx in week_tbl, then remap to 1:N_seasons
# week_tbl_season_map <- setNames(seq_along(seasons), seasons)
# 
# week_tbl_hfa6 <- week_tbl |>
#   mutate(season_id = week_tbl_season_map[as.character(season)]) |> # 1-based season index
#   filter(week_idx %in% weeks)
# 
# week_season <- week_tbl_hfa6$season_id[match(weeks, week_tbl_hfa6$week_idx)] # length N_weeks, 1-based
# 
# # 2. first/last week of each season (values are week_idx, NOT positions)
# first_week_of_season <- week_tbl_hfa6 |>
#   group_by(season_id) |>
#   summarise(first = min(week_idx), .groups = "drop") |>
#   pull(first) |>
#   match(weeks)  # Get position in weeks
# 
# last_week_of_season <- week_tbl_hfa6 |>
#   group_by(season_id) |>
#   summarise(last = max(week_idx), .groups = "drop") |>
#   pull(last) |>
#   match(weeks)  # Get position in weeks

# 2. Stan data
# hfa6_stan_data <- list(
#   N_games    = nrow(train_data),
#   N_obs      = nrow(train_data),
#   N_teams    = length(teams),
#   N_seasons  = length(seasons),
#   N_weeks    = length(weeks),
#   home_id    = as.integer(factor(train_data$home_team, levels = teams)),
#   away_id    = as.integer(factor(train_data$away_team, levels = teams)),
#   week_id    = as.integer(factor(train_data$week_idx, levels = weeks)),
#   season_id  = as.integer(factor(train_data$season, levels = seasons)),
#   week_season = week_season,  # length N_weeks, value in 1:N_seasons
#   first_week_of_season = first_week_of_season, # N_seasons, value is week_idx
#   last_week_of_season  = last_week_of_season,  # N_seasons, value is week_idx
#   hfa        = as.integer(train_data$hfa), # 1 = home, 0 = neutral
#   result     = as.numeric(train_data$result) # home - away margin
# )

# #mod_output_dir <- "Model Fitting/stan_models"

# hfa6_iters <- 2000
# hfa6_warmup <- 1000
# hfa6_chains <- 4
# hfa6_thin <- 1
# hfa6_sims <- ((hfa6_iters)/hfa6_thin)*hfa6_chains
# hfa6_parallel_chains <- parallel::detectCores()
# hfa6_adapt_delta <- 0.95
# hfa6_max_treedeepth <- 10
# hfa6_seed <- 52


# Build a vector of cutoff weeks (end-of-week indices) for filtering
cutoff_weeks <- week_tbl |>
  arrange(week_idx) |>
  filter(season >= 2006) |>
  pull(week_idx) |>
  unique()

# Helper: games observed up to a cutoff
games_through <- function(cutoff_idx) {
  which(train_data$week_idx <= cutoff_idx)
}

# Build a sliced Stan data list up to and including cutoff week W
build_stan_data_upto <- function(cutoff_wk, base_df, teams, week_tbl) {
  dfW <- base_df |> filter(week_idx <= cutoff_wk)
  
  weeks_W   <- sort(unique(dfW$week_idx))
  seasons_W <- sort(unique(dfW$season))
  
  week_tbl_W <- week_tbl |>
    filter(week_idx %in% weeks_W) |>
    mutate(season_id = as.integer(factor(season, levels = seasons_W)))
  
  first_week_of_season_W <- week_tbl_W |>
    group_by(season_id) |>
    summarise(first = min(week_idx), .groups = "drop") |>
    pull(first) |>
    match(weeks_W)
  
  last_week_of_season_W <- week_tbl_W |>
    group_by(season_id) |>
    summarise(last = max(week_idx), .groups = "drop") |>
    pull(last) |>
    match(weeks_W)
  
  week_season_W <- week_tbl_W$season_id[match(weeks_W, week_tbl_W$week_idx)]
  
  list(
    data = list(
      N_games   = nrow(dfW),
      N_teams   = length(teams),
      N_seasons = length(seasons_W),
      N_weeks   = length(weeks_W),
      
      home_id   = as.integer(factor(dfW$home_team, levels = teams)),
      away_id   = as.integer(factor(dfW$away_team, levels = teams)),
      week_id   = as.integer(factor(dfW$week_idx,  levels = weeks_W)),
      season_id = as.integer(factor(dfW$season,    levels = seasons_W)),
      
      week_season = week_season_W,
      first_week_of_season = first_week_of_season_W,
      last_week_of_season  = last_week_of_season_W,
      
      hfa    = as.integer(dfW$hfa),
      result = as.numeric(dfW$result)   # full vector; N_obs set outside
    ),
    index = list(
      dfW = dfW,
      weeks_W = weeks_W,
      seasons_W = seasons_W
    )
  )
}

# Truncate previous fit’s posterior means to current dims (teams fixed)
make_inits_from_prev_fit <- function(fit, teams, seasons_W, weeks_W) {
  Tn <- length(teams)
  S <- length(seasons_W)
  W <- length(weeks_W)
  
  sizes <- fit$metadata()$stan_variable_sizes
  
  # helpers
  get_scalar_mean <- function(v) {
    #s <- posterior::summarise_draws(fit$draws(v))
    s <- fit$summary(variables = v)
    if (nrow(s) == 0) return(NA_real_) else s$mean[1]
  }
  get_means_matrix <- function(v, nrow_prev, ncol_prev) {
    if (is.null(sizes[[v]])) return(matrix(0, nrow_prev, ncol_prev))
    dm <- fit$draws(v, format = "draws_matrix")  # draws x (nrow_prev*ncol_prev)
    cm <- colMeans(dm)
    matrix(cm, nrow = nrow_prev, ncol = ncol_prev)  # Stan indexes column-wise
  }
  get_means_vector <- function(v, n_prev) {
    if (is.null(sizes[[v]])) return(rep(0, n_prev))
    dm <- fit$draws(v, format = "draws_matrix")
    colMeans(dm)
  }
  
  # previous dims (if missing, assume 0)
  dim_team_hfa_z <- sizes$team_hfa_z %||% integer()
  T_prev <- if (length(dim_team_hfa_z) >= 1) dim_team_hfa_z[1] else Tn
  S_prev <- if (length(dim_team_hfa_z) >= 2) dim_team_hfa_z[2] else 0
  
  dim_z_start <- sizes$z_start %||% integer()
  S_prev_start <- if (length(dim_z_start) >= 2) dim_z_start[2] else S_prev
  
  dim_z_w <- sizes$z_w %||% integer()
  W_prev <- if (length(dim_z_w) >= 2) dim_z_w[2] else 0
  
  # means from previous fit (then pad/truncate to current dims)
  league_hfa_z_prev <- get_means_vector("league_hfa_z", max(S_prev, 0))
  league_hfa_z_now  <- if (S <= length(league_hfa_z_prev)) {
    c(league_hfa_z_prev[seq_len(S)], use.names = FALSE)
  } else {
    c(league_hfa_z_prev, rep(0, S - length(league_hfa_z_prev)), use.names = FALSE)
  }
  
  team_hfa_z_prev <- if (S_prev > 0) get_means_matrix("team_hfa_z", T_prev, S_prev) else matrix(0, Tn, 0)
  team_hfa_z_now  <- matrix(0, nrow = Tn, ncol = S)
  if (S_prev > 0) {
    # match team dimension (should already be same ordering)
    take_T <- min(Tn, nrow(team_hfa_z_prev))
    take_S <- min(S,  ncol(team_hfa_z_prev))
    team_hfa_z_now[seq_len(take_T), seq_len(take_S)] <- team_hfa_z_prev[seq_len(take_T), seq_len(take_S)]
  }
  
  z_start_prev <- if (S_prev_start > 0) get_means_matrix("z_start", T_prev, S_prev_start) else matrix(0, Tn, 0)
  z_start_now  <- matrix(0, nrow = Tn, ncol = S)
  if (S_prev_start > 0) {
    take_T <- min(Tn, nrow(z_start_prev))
    take_S <- min(S,  ncol(z_start_prev))
    z_start_now[seq_len(take_T), seq_len(take_S)] <- z_start_prev[seq_len(take_T), seq_len(take_S)]
  }
  
  z_w_prev <- if (W_prev > 0) get_means_matrix("z_w", T_prev, W_prev) else matrix(0, Tn, 0)
  z_w_now  <- matrix(0, nrow = Tn, ncol = W)
  if (W_prev > 0) {
    take_T <- min(Tn, nrow(z_w_prev))
    take_W <- min(W,  ncol(z_w_prev))
    z_w_now[seq_len(take_T), seq_len(take_W)] <- z_w_prev[seq_len(take_T), seq_len(take_W)]
  }
  
  list(
    league_hfa_z    = league_hfa_z_now,
    league_hfa_init = get_scalar_mean("league_hfa_init"),
    beta_hfa        = get_scalar_mean("beta_hfa"),
    sigma_hfa       = abs(get_scalar_mean("sigma_hfa")),
    
    team_hfa_z      = team_hfa_z_now,
    sigma_team_hfa  = abs(get_scalar_mean("sigma_team_hfa")),
    
    z_start         = z_start_now,
    z_w             = z_w_now,
    
    beta_w          = min(max(get_scalar_mean("beta_w"), 0.01), 0.99),
    sigma_w         = abs(get_scalar_mean("sigma_w")),
    beta_s          = min(max(get_scalar_mean("beta_s"), 0.01), 0.99),
    sigma_s         = abs(get_scalar_mean("sigma_s")),
    sigma_y         = abs(get_scalar_mean("sigma_y"))
  )
}

## 2.3 Initial Fit (2006) ----
end_2006 <- week_tbl |>
  filter(season == 2006) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

built0 <- build_stan_data_upto(end_2006, train_data, teams, week_tbl)
stan_data0 <- built0$data
stan_data0$N_obs <- nrow(built0$index$dfW)   # fit all of 2006

message("Fitting through 2006 (sliced)...")
fit0 <- hfa6_mod$sample(
  data = stan_data0,
  chains = 4, 
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 10,
  init = 0.2, 
  seed = 52
)

## 2.4 Backtesting ----
future_weeks <- cutoff_weeks[cutoff_weeks > end_2006]
results <- vector("list", length(future_weeks))
prev_fit <- fit0  # warm-start source; may be NULL on very first run

{tic(msg = "🎉 Total Backtest Time")
for (i in seq_along(future_weeks)) {
  W   <- future_weeks[i]
  Wm1 <- if (i == 1) end_2006 else future_weeks[i - 1]
  
  cutoff_season <- unique(game_fit_data_all$season[game_fit_data_all$week_idx == W])
  cutoff_week   <- unique(game_fit_data_all$week[game_fit_data_all$week_idx == W])
  message(paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ",
    "Season ", cutoff_season, ", Week ", cutoff_week, 
    " %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  ))
  
  # --- Build sliced data up to and including week W ---
  builtW <- build_stan_data_upto(W, train_data, teams, week_tbl)
  stan_data_W <- builtW$data
  
  # --- If any results through Wm1 or W are NA, stop fitting ---
  train_na <- anyNA(builtW$index$dfW$result[builtW$index$dfW$week_idx <= Wm1])
  pred_na  <- anyNA(builtW$index$dfW$result[builtW$index$dfW$week_idx == W])
  
  # Get rows for prediction week
  rows_W <- which(builtW$index$dfW$week_idx == W)
  if (length(rows_W) == 0) {
    message("No games found for week ", W, " — stopping backtest.")
    break
  }
  
  if (train_na || pred_na) {
    msg <- if (train_na) "in training set" else "in prediction week"
    message("Reached first week with incomplete results (", msg, "): stopping refits.")
    
    vars_W <- paste0("mu[", rows_W, "]")
    mu_summ <- prev_fit$summary(variables = vars_W)
    
    mu_next <- mu_summ |>
      mutate(idx = as.integer(gsub("mu\\[|\\]", "", variable)),
             game_id = builtW$index$dfW$game_id[idx]) |>
      select(game_id, pred_mean = mean, pred_q05 = q5, pred_q95 = q95) |>
      left_join(train_data |> select(game_id, season, week, week_idx, result, spread_line),
                by = "game_id") |>
      mutate(error = pred_mean - result,
             spread_error = spread_line - result)
    
    results[[i]] <- mu_next
    break
  }
  
  # --- Fit with N_obs = through W-1 (no leakage) ---
  stan_data_W$N_obs <- sum(builtW$index$dfW$week_idx <= Wm1)
  
  inits_list <- if (is.null(prev_fit)) 0.2 else {
    make_inits_from_prev_fit(prev_fit, teams, builtW$index$seasons_W, builtW$index$weeks_W)
  }
  
  tic(paste0("Fitting ≤ Week ", W - 1, " (N_obs = ", stan_data_W$N_obs, ")"))
  fit_W <- hfa6_mod$sample(
    data = stan_data_W,
    chains = 4, 
    parallel_chains = min(4, parallel::detectCores()),
    iter_warmup = 200, 
    iter_sampling = 500,
    adapt_delta = 0.95, 
    max_treedepth = 10, 
    init = function() inits_list,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    seed = 52
  )
  toc()
  
  # --- OOS Prediction for week W from THIS fit ---
  tic("OOS Prediction Time")
  vars_W <- paste0("mu[", rows_W, "]")
  
  mu_summ <- fit_W$summary(variables = vars_W)
  
  mu_next <- mu_summ |>
    mutate(idx = as.integer(gsub("mu\\[|\\]", "", variable)),
           game_id = builtW$index$dfW$game_id[idx]) |>
    select(game_id, pred_mean = mean, pred_q05 = q5, pred_q95 = q95) |>
    left_join(train_data |> select(game_id, season, week, week_idx, result, spread_line),
              by = "game_id") |>
    mutate(error = pred_mean - result,
           spread_error = spread_line - result)
  
  results[[i]] <- mu_next
  toc()
  
  # --- Warm-start for next week ---
  prev_fit <- fit_W
}
toc()}

### 2.4.1 OOS Inference ----
backtest_preds <- bind_rows(results) |>
  left_join(train_data) |>
  relocate(season, week, week_idx, .after = game_id)

backtest_metrics <- backtest_preds |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= pred_q05 & result <= pred_q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(backtest_metrics)

backtest_metrics_season <- backtest_preds |>
  group_by(season) |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= pred_q05 & result <= pred_q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(backtest_metrics_season)

backtest_metrics_week <- backtest_preds |>
  group_by(week) |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= pred_q05 & result <= pred_q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(backtest_metrics_week)

backtest_metrics_week_idx <- backtest_preds |>
  group_by(season, week, week_idx) |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= pred_q05 & result <= pred_q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(backtest_metrics_week_idx)

print(backtest_metrics)
print(backtest_metrics_season)
print(backtest_metrics_week)
print(backtest_metrics_week_idx)

ggplot(backtest_preds, aes(x = pred_mean, y = result)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  sm_statCorr(color = "blue", r2 = TRUE, label_y = 55) +
  sm_statCorr(color = "blue", r2 = FALSE, label_y = 50) +
  labs(
    title = "Predicted vs Actual Score Differential",
    x = "Predicted Mean",
    y = "Actual Result"
  ) +
  theme_bw()

ggplot(backtest_preds, aes(x = week_idx, y = error)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Prediction Error Over Time",
    x = "Week Index",
    y = "Prediction - Result"
  ) +
  theme_bw()

ggplot(backtest_preds, aes(x = spread_line, y = spread_error)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residual vs Vegas Spread",
    x = "Market Spread",
    y = "Spread - Result"
  ) +
  theme_bw()



### 2.2.1 Mid Fit ----
end_mid <- week_tbl |>
  filter(season == 2016, week == 10) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

built_mid <- build_stan_data_upto(end_mid, train_data, teams, week_tbl)
stan_data_mid <- built_mid$data
stan_data_mid$N_obs <- nrow(built_mid$index$dfW)   # fit all of 2006

message("Fitting through Season 2016 Week 10 (sliced)...")
fit_mid <- hfa6_mod$sample(
  data = stan_data_mid,
  chains = 2, 
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 10,
  init = 0.2, 
  seed = 52
)


### 2.2.2 Full Fit ----
end_full <- week_tbl |>
  filter(season == 2024, week == 22) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

built_full <- build_stan_data_upto(end_full, train_data, teams, week_tbl)
stan_data_full <- built_full$data
stan_data_full$N_obs <- nrow(built_full$index$dfW)   # fit all of 2006

message("Fitting through Season 2024 Week 22 (sliced)...")
fit_full <- hfa6_mod$sample(
  data = stan_data_full,
  chains = 2, 
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 10,
  init = 0.2, 
  seed = 52
)


## 2.3 MCMC ----
### 2.3.1 Fit Model ----
system.time(
  hfa6_mcmc_fit <- hfa6_mod$sample(
    data = hfa6_stan_data,
    #output_dir = hfa6_output_dir,
    chains = hfa6_chains,
    parallel_chains = hfa6_parallel_chains,
    iter_sampling = hfa6_iters, 
    iter_warmup = hfa6_warmup,
    thin = hfa6_thin,
    adapt_delta = hfa6_adapt_delta, 
    max_treedepth = hfa6_max_treedeepth,
    seed = hfa6_seed
  )
)

### Save
hfa6_mcmc_fit$save_object(file = "Model Fitting/stan_models/hfa6_mcmc_fit_all.rds")

### 2.3.2 Diagnostics ----
hfa6_mod_variables
hfa6_mcmc_fit$sampler_diagnostics(format = "df")
hfa6_mcmc_fit$cmdstan_diagnose()
hfa6_mcmc_fit$diagnostic_summary()

hfa6_mcmc_fit_vars <- hfa6_mcmc_fit$metadata()$stan_variable_sizes
hfa6_mcmc_fit_vars
hfa6_mcmc_fit$output()
hfa6_mcmc_sum <- hfa6_mcmc_fit$summary(
  variables = subset(names(hfa6_mcmc_fit_vars),
                     hfa6_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa6_mcmc_sum, n = Inf)

### 2.3.3 Posterior ----
hfa6_mcmc_team_strength <- hfa6_mcmc_fit |>
  spread_draws(team_strength[team, week_idx]) |>
  mutate(
    team = teams[team],
    week_idx = weeks[week_idx]
  ) |>
  summarise_draws()

hfa6_mcmc_team_hfa <- hfa6_mcmc_fit |>
  spread_draws(team_hfa[team, season]) |>
  mutate(
    team = teams[team],
    season = seasons[season]
  ) |>
  summarise_draws()
hfa6_mcmc_team_hfa

hfa6_mcmc_league_hfa <- hfa6_mcmc_fit |>
  spread_draws(league_hfa[season]) |>
  mutate(
    season = seasons[season]
  ) |>
  summarise_draws()
hfa6_mcmc_league_hfa

hfa6_team_hfa_total <- hfa6_mcmc_fit |>
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
hfa6_team_hfa_total



# hfa6_team_strength_end <- week_tbl |>
#   right_join(hfa6_team_strength) |>
#   slice_tail(n = 1, by = team)

hfa6_team_strength_end <- hfa6_team_strength |>
  ungroup() |>
  arrange(week_idx, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa6_team_hfa_end <- hfa6_team_hfa_total |>
  ungroup() |>
  arrange(season, team) |>
  slice_tail(n = 1, by = c(team, variable))

hfa6_team_total <- bind_rows(
  hfa6_team_hfa_end,
  hfa6_team_strength_end
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

### 2.3.4 Plots ----
p <- hfa6_mcmc_team_strength |>
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

p <- hfa6_mcmc_team_hfa |>
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

## 2.4 MLE ----
hfa6_mle_fit <- hfa6_mod$optimize(
  data = hfa6_stan_data,
  #output_dir = hfa6_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = hfa6_seed
)

hfa6_mle_fit_vars <- hfa6_mle_fit$metadata()$stan_variable_sizes
hfa6_mle_fit_vars
hfa6_mle_fit$output()
hfa6_mle_sum <- hfa6_mle_fit$summary(
  variables = subset(names(hfa6_mle_fit_vars),
                     hfa6_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa6_mle_sum, n = Inf)

hfa6_mle_team_strength <- hfa6_mle_fit$draws(
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

hfa6_mle_league_hfa <- hfa6_mle_fit$draws(
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

hfa6_mle_team_hfa <- hfa6_mle_fit$draws(
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

hfa6_mle_team_total_hfa <- hfa6_mle_fit$draws(
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

hfa6_mle_team_estimates <- hfa6_mle_team_strength |>
  left_join(week_tbl_hfa6) |>
  left_join(hfa6_mle_league_hfa) |>
  left_join(hfa6_mle_team_hfa_dev) |>
  left_join(hfa6_mle_team_total_hfa) |>
  relocate(contains("mle"), .after = last_col())

## 2.5 MAP ----
hfa6_map_fit <- hfa6_mod$optimize(
  data = hfa6_stan_data,
  #output_dir = hfa6_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = hfa6_seed
)
hfa6_map_fit_vars <- hfa6_map_fit$metadata()$stan_variable_sizes
hfa6_map_fit_vars
hfa6_map_fit$output()
hfa6_map_sum <- hfa6_map_fit$summary(
  variables = subset(names(hfa6_map_fit_vars),
                     hfa6_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfa6_map_sum, n = Inf)

hfa6_map_team_strength <- hfa6_map_fit$draws(
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

hfa6_map_league_hfa <- hfa6_map_fit$draws(
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

hfa6_map_team_hfa <- hfa6_map_fit$draws(
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

hfa6_map_team_total_hfa <- hfa6_map_fit$draws(
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

hfa6_map_team_estimates <- hfa6_map_team_strength |>
  left_join(week_tbl_hfa6) |>
  left_join(hfa6_map_league_hfa) |>
  left_join(hfa6_map_team_hfa_dev) |>
  left_join(hfa6_map_team_total_hfa) |>
  relocate(contains("map"), .after = last_col())


## 2.6 Variational ----
hfa6_vi_fit <- hfa6_mod$variational(
  data = hfa6_stan_data,
  #init = 0,
  iter = 20000,
  draws = hfa6_iters*hfa6_chains,
  seed = hfa6_seed
)

hfa6_vi_fit_vars <- hfa6_vi_fit$metadata()$stan_variable_sizes
hfa6_vi_fit_vars
hfa6_vi_fit$output()
hfa6_vi_sum <- hfa6_vi_fit$summary(
  variables = subset(names(hfa6_vi_fit_vars),
                     hfa6_vi_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa6_vi_sum, n = Inf)

hfa6_vi_team_strength <- hfa6_vi_fit$draws(
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

hfa6_vi_league_hfa <- hfa6_vi_fit$draws(
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

hfa6_vi_team_hfa <- hfa6_vi_fit$draws(
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

hfa6_vi_team_total_hfa <- hfa6_vi_fit$draws(
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

hfa6_vi_team_estimates <- hfa6_vi_team_strength |>
  left_join(week_tbl_hfa6) |>
  left_join(hfa6_vi_league_hfa) |>
  left_join(hfa6_vi_team_hfa_dev) |>
  left_join(hfa6_vi_team_total_hfa) |>
  relocate(contains("vi"), .after = last_col())

## 2.7 Laplace ----
hfa6_lap_fit <- hfa6_mod$laplace(
  data = hfa6_stan_data,
  init = 0,
  jacobian = TRUE,
  draws = hfa6_iters,
  seed = hfa6_seed
)

hfa6_lap_fit_vars <- hfa6_lap_fit$metadata()$stan_variable_sizes
hfa6_lap_fit_vars
hfa6_lap_fit$output()
hfa6_lap_sum <- hfa6_lap_fit$summary(
  variables = subset(names(hfa6_lap_fit_vars),
                     hfa6_lap_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa6_lap_sum, n = Inf)

hfa6_lap_team_strength <- hfa6_lap_fit$draws(
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

hfa6_lap_league_hfa <- hfa6_lap_fit$draws(
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

hfa6_lap_team_hfa_dev <- hfa6_lap_fit$draws(
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

hfa6_lap_team_total_hfa <- hfa6_lap_fit$draws(
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

hfa6_lap_team_estimates <- hfa6_lap_team_strength |>
  left_join(week_tbl_hfa6) |>
  left_join(hfa6_lap_league_hfa) |>
  left_join(hfa6_lap_team_hfa_dev) |>
  left_join(hfa6_lap_team_total_hfa) |>
  relocate(contains("lap"), .after = last_col())