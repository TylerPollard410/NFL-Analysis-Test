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
all_seasons <- 1999:get_current_season()
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
game_model_data <- game_data |>#game_model_data |>
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
# 2. MODEL hfa7 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 1999, week == 1)  |> pull(week_idx) |> unique()
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
hfa7_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa7.stan"
)

## 2.2 Stan Data ----

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

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
      N_obs     = 0L,                       # set outside
      N_teams   = length(teams),
      N_seasons = length(seasons_W),
      N_weeks   = length(weeks_W),
      home_id   = as.integer(dfW$home_id),
      away_id   = as.integer(dfW$away_id),
      week_id   = as.integer(dfW$week_idx),
      season_id = as.integer(dfW$season_idx),
      first_week_of_season = as.integer(first_week_of_season_W),
      last_week_of_season  = as.integer(last_week_of_season_W),
      hfa       = as.integer(dfW$hfa),
      result    = as.numeric(dfW$result),   # full vector; N_obs set outside
      N_oos     = 0L,                       # <— NEW
      oos_idx   = integer(0)                # <— NEW
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
stan_data0$N_oos   <- 0L
stan_data0$oos_idx <- integer(0)

message("Fitting through 2006 (sliced)...")
fit0 <- hfa7_mod$sample(
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
oos_draws <- vector("list", length(future_weeks))
team_strength_last <- c()
team_hfa_last <- c()
league_hfa_last <- c()
beta_hfa <- c()
sigma_hfa <- c()
sigma_team_hfa <- c()
beta_w <- c()
sigma_w <- c()
beta_s <- c()
sigma_s <- c()
sigma_y <- c()
prev_fit <- fit0  # warm-start source; may be NULL on very first run

#future_weeks2 <- future_weeks[future_weeks > 218]

fit_list <- list()

{tic(msg = "🎉 Total Backtest Time")
for (i in seq_along(future_weeks)) {
#for (i in seq_along(future_weeks2)) {
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
    break
  }
  
  # if (train_na || pred_na) {
  #   msg <- if (train_na) "in training set" else "in prediction week"
  #   message("Reached first week with incomplete results (", msg, "): stopping refits.")
  #   
  #   vars_W <- paste0("mu[", rows_W, "]")
  #   mu_summ <- prev_fit$summary(variables = vars_W)
  #   
  #   mu_next <- mu_summ |>
  #     mutate(idx = as.integer(gsub("mu\\[|\\]", "", variable)),
  #            game_id = builtW$index$dfW$game_id[idx]) |>
  #     select(game_id, pred_mean = mean, pred_q05 = q5, pred_q95 = q95) |>
  #     left_join(train_data |> select(game_id, season, week, week_idx, result, spread_line),
  #               by = "game_id") |>
  #     mutate(error = pred_mean - result,
  #            spread_error = spread_line - result)
  #   
  #   results[[i]] <- mu_next
  #   break
  # }
  
  # --- Fit with N_obs = through W-1 (no leakage) ---
  stan_data_W$N_obs <- sum(builtW$index$dfW$week_idx <= Wm1)
  stan_data_W$N_oos   <- length(rows_W)
  stan_data_W$oos_idx <- as.integer(rows_W)
  
  inits_list <- if (is.null(prev_fit)) 0 else {
    make_inits_from_prev_fit(prev_fit, teams, builtW$index$seasons_W, builtW$index$weeks_W)
  }
  
  tic(paste0("Fitting ≤ Week ", W - 1, " (N_obs = ", stan_data_W$N_obs, ")"))
  fit_W <- hfa7_mod$sample(
    data = stan_data_W,
    chains = 4, 
    parallel_chains = min(4, parallel::detectCores()),
    iter_warmup = 200, 
    iter_sampling = 500,
    adapt_delta = 0.95, 
    max_treedepth = 15, 
    init = function() inits_list,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    seed = 52
  )
  toc()
  
  # --- Fitted Estimates for week W-1 from THIS fit ---
  tic("Fitted Estimates Time")
  team_strength_last  <- bind_rows(team_strength_last,fit_W$summary(variables = "team_strength_last"))
  team_hfa_last <- bind_rows(team_hfa_last,fit_W$summary(variables = "team_hfa_last"))
  league_hfa_last <- bind_rows(league_hfa_last,fit_W$summary(variables = "league_hfa_last"))
  beta_hfa <- bind_rows(beta_hfa,fit_W$summary(variables = "beta_hfa"))
  sigma_hfa <- bind_rows(sigma_hfa,fit_W$summary(variables = "sigma_hfa"))
  sigma_team_hfa <- bind_rows(sigma_team_hfa,fit_W$summary(variables = "sigma_team_hfa"))
  beta_w <- bind_rows(beta_w,fit_W$summary(variables = "beta_w"))
  sigma_w <- bind_rows(sigma_w,fit_W$summary(variables = "sigma_w"))
  beta_s <- bind_rows(beta_s,fit_W$summary(variables = "beta_s"))
  sigma_s <- bind_rows(sigma_s,fit_W$summary(variables = "sigma_s"))
  sigma_y <- bind_rows(sigma_y,fit_W$summary(variables = "sigma_y"))
  toc()
  
  # --- OOS Expected Prediction for week W from THIS fit ---
  tic("OOS Expected Prediction Time")
  
  mu_oos <- fit_W$summary(variables = "mu_oos")
  y_oos <- fit_W$summary(variables = "y_oos")
  mu_next <- mu_oos |>
    mutate(
      k = row_number(),
      game_id = builtW$index$dfW$game_id[rows_W[k]]
    ) |>
    select(game_id, 
           pred_mean = mean, 
           pred_q05 = q5,
           pred_q95 = q95) |>
    left_join(
      train_data |>
        select(game_id, season, week, week_idx, result, spread_line),
      by = "game_id"
    ) |>
    mutate(
      post_mean = y_oos$mean,
      .before = pred_mean
    ) |>
    mutate(
      error = pred_mean - result,
      spread_error = spread_line - result
    )
  
  results[[W]] <- mu_next
  toc()
  
  # --- OOS Posterior Prediction for week W from THIS fit ---
  tic("OOS Posterior Prediction Time")
  y_oos_draws <- fit_W$draws(variables = "y_oos", format = "df") 
  suppressWarnings(y_oos_draws <- y_oos_draws |> select(-c(.chain, .iteration, .draw)))
  oos_draws[[W]] <- y_oos_draws
  toc()
  
  # --- Warm-start for next week ---
  prev_fit <- fit_W
  fit_list[[paste0("fitW_", Wm1)]] <- fit_W
}
toc()}

saveRDS(fit_list, "fit_list.rds")

future_weeks
future_weeks2
future_weeks3 <- future_weeks[!(future_weeks %in% future_weeks2)]

total_weeks <- nrow(team_strength_last)/32
team_strength_last3 <- team_strength_last |>
  mutate(
    team = rep(teams, times = total_weeks),
    week_idx = rep(1:total_weeks, each = 32),
    week_idx = (future_weeks-1)[week_idx],
    .after = variable
  ) |>
  left_join(
    week_tbl |> select(season, week, week_idx)
  ) |>
  relocate(season, week, .after = team)
team_hfa_last3 <- team_hfa_last |>
  mutate(
    team = rep(teams, times = total_weeks),
    week_idx = rep(1:total_weeks, each = 32),
    week_idx = (future_weeks-1)[week_idx],
    .after = variable
  ) |>
  left_join(
    week_tbl |> select(season, week, week_idx)
  ) |>
  relocate(season, week, .after = team)
league_hfa_last3 <- league_hfa_last |>
  mutate(
    #team = rep(teams, times = total_weeks),
    week_idx = 1:total_weeks,
    week_idx = (future_weeks-1)[week_idx],
    .after = variable
  ) |>
  left_join(
    week_tbl |> select(season, week, week_idx)
  ) |>
  relocate(season, week, .after = variable)

oos_draws3 <- bind_cols(
  oos_draws
)

oos_names <- train_data |>
  #filter(week_idx %in% future_weeks3) |>
  group_by(week_idx) |>
  mutate(game_idx = row_number(), .after = week_idx) |>
  ungroup() |>
  mutate(game_idx = paste0(week_idx, ",", game_idx),
         row_id = row_number()) |>
  select(game_id, game_idx, season, week, week_idx, row_id)

oos_names2 <- oos_names |> filter(week_idx %in% future_weeks3) |> pull(game_idx)
oos_names3 <- oos_names |> filter(week_idx %in% future_weeks2) |> pull(game_idx)

oos_draws4 <- bind_cols(
  oos_draws2,
  oos_draws3 |> select(1:length(oos_names3))
)
oos_names4 <- c(oos_names2, oos_names3)
oos_draws4_names <- paste0("y_oos[", oos_names4, "]")
colnames(oos_draws4) <- oos_draws4_names
oos_draws4_df <- oos_draws4 |>
  mutate(.chain = NA, .iteration = NA, .draw = 1:2000) |>
  spread_draws(y_oos[week_idx, game_idx])
oos_preds <- oos_draws4_df |>
  mutate(.chain = rep(1:4, each = 500),
         .iteration = rep(1:500, times = 4)) |>
  summarise_draws()


saveRDS(team_strength_last3, "Model Fitting/stan_models/team_strength.rds")
saveRDS(team_hfa_last3, "Model Fitting/stan_models/team_hfa.rds")
saveRDS(league_hfa_last3, "Model Fitting/stan_models/league_hfa.rds")
saveRDS(oos_draws4, "Model Fitting/stan_models/y_oos.rds")
saveRDS(oos_draws4_names, "Model Fitting/stan_models/y_oos_names.rds")

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

expected_preds <- game_fit_data_all |>
  group_by(week_idx) |>
  mutate(game_idx = row_number(), .after = week_idx) |>
  left_join(
    team_strength_last3 |> 
      select(
        team, season, week, week_idx, 
        home_team_strength = mean
      ),
    by = join_by(home_team == team, season, week, week_idx)
  ) |>
  left_join(
    team_strength_last3 |> 
      select(
        team, season, week, week_idx, 
        away_team_strength = mean
      ),
    by = join_by(away_team == team, season, week, week_idx)
  ) |>
  left_join(
    team_hfa_last3 |> 
      select(
        team, season, week, week_idx, 
        home_team_hfa = mean
      ),
    by = join_by(home_team == team, season, week, week_idx)
  ) |>
  left_join(
    league_hfa_last3 |> 
      select(
        season, week, week_idx, 
        league_hfa = mean
      ),
    by = join_by(season, week, week_idx)
  ) |>
  mutate(
    pred_mean = home_team_strength - away_team_strength + (home_team_hfa)*hfa
  )

oos_exp_preds <- expected_preds |>
  left_join(oos_preds |> select(-variable) |> rename(post_mean = mean)) |>
  filter(!is.na(post_mean)) |>
  mutate(
    #error = pred_mean - result,
    error = post_mean - result,
    spread_error = spread_line - result
  )

oos_metrics <- oos_exp_preds |>
  ungroup() |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= q5 & result <= q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(oos_metrics)

oos_exp_preds |>
  ungroup() |>
  group_by(season) |>
  summarise(
    MAE  = mean(abs(error), na.rm = TRUE),
    MAE_spread  = mean(abs(spread_error), na.rm = TRUE),
    COV_90 = mean(result >= q5 & result <= q95, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    RMSE_spread = sqrt(mean(spread_error^2, na.rm = TRUE)),
    Bias = mean(error, na.rm = TRUE),
    Bias_spread = mean(spread_error, na.rm = TRUE),
    SD_Error = sd(error, na.rm = TRUE),
    SD_Error_spread = sd(spread_error, na.rm = TRUE)
  )
print(oos_metrics)

### 2.4.2 Plots ----
first_week_of_season <- stan_data_W$first_week_of_season
last_week_of_season <- stan_data_W$last_week_of_season

p <- team_strength_last3 |>
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


p <- league_hfa_last3 |>
  #left_join(week_tbl |> select(season, week, week_idx)) |>
  ggplot(
    aes(x = week_idx, y = mean, group = 1,
        text = paste0(
          #"team: ", team, "\n",
          sprintf("hfa: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_line(aes(color = estimate)) +
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
  #scale_color_nfl(guide = guide_legend()) +
  labs(title = "League HFA Over Time",
       x = "Week", y = "League HFA") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) 
# guides(
#   color = guide_legend(
#     nrow = 3,  # Use 2, 4, or 8 depending on what fits
#     byrow = TRUE,
#     title.position = "top"
#   )
# )

p
ggplotly(p, tooltip = "text")

p <- team_hfa_last3 |>
  #left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = mean, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("hfa: %.2f", mean), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team HFA Over Time", x = "Week", y = "Estimated Strength") +
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

## 2.5 Diagnostics ----
hfa_mcmc_fit <- fit0
hfa_mcmc_fit$sampler_diagnostics(format = "df")
hfa_mcmc_fit$cmdstan_diagnose()
hfa_mcmc_fit$diagnostic_summary()

hfa_mcmc_fit_vars <- hfa_mcmc_fit$metadata()$stan_variable_sizes
hfa_mcmc_fit_vars
hfa_mcmc_fit$output()
hfa7_mcmc_sum <- hfa_mcmc_fit$summary(
  variables = subset(names(hfa_mcmc_fit_vars),
                     hfa_mcmc_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(hfa7_mcmc_sum, n = Inf)

## 2.5 Final Posterior Values ----


built1 <- build_stan_data_upto(last_train_week, train_data, teams, week_tbl)
stan_data1 <- built1$data
stan_data1$N_obs <- nrow(built1$index$dfW)   # fit all of 2006
stan_data1$N_oos   <- 0L
stan_data1$oos_idx <- integer(0)

message("Fitting through 2006 (sliced)...")
fit1 <- hfa7_mod$sample(
  data = stan_data1,
  chains = 4, 
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 10,
  init = 0.2, 
  seed = 52
)
