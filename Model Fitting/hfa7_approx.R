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

## 2.0 Helpers ----
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

## 2.1 Compile Model ----
hfa7_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa7.stan"
)

## 2.2
## 2.2 Stan Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 1999, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2022, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# Build a vector of cutoff weeks (end-of-week indices) for filtering
end_week <- week_tbl |>
  filter(season == 2022) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

builtA <- build_stan_data_upto(end_week, train_data, teams, week_tbl)
stan_dataA <- builtA$data
stan_dataA$N_obs <- nrow(builtA$index$dfW) 
stan_dataA$N_oos <- 0L
stan_dataA$oos_idx <- integer(0)
#stan_dataA$N_obs <- sum(builtA$index$dfW$week_idx <= end_week)
# stan_dataA$N_oos   <- sum(builtA$index$dfW$week_idx > end_week)
# stan_dataA$oos_idx <- as.integer(which(builtA$index$dfW$week_idx > end_week))

prev_fitA <- NULL
inits_listA <- if (is.null(prev_fitA)) 0.1 else {
  make_inits_from_prev_fit(prev_fitA, teams, builtA$index$seasons_W, builtA$index$weeks_W)
}

fit_mcmc_A <- hfa7_mod$sample(
  data = stan_dataA,
  chains = 4, 
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 1000, 
  iter_sampling = 2000,
  adapt_delta = 0.95, 
  max_treedepth = 15, 
  init = 0.2,
  # refresh = 0,
  # show_messages = FALSE,
  # show_exceptions = FALSE,
  seed = 52
)

## 2.3 Fit Models ----
## 2.3 Initial Fit (2006) ----
message("Fitting through 2006 (sliced)...")
fit_mcmc <- hfa7_mod$sample(
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2.3 Helpers (build data windows, fit methods, summarize OOS) ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 2.3.1 Build Stan data for Train≤season_T, Predict season_P ----
# Uses your week_tbl and base df. Includes all games ≤ season_P in arrays,
# sets N_obs to games ≤ season_T, and OOS is (season_T, season_P].
build_stan_data_window <- function(base_df, week_tbl, teams,
                                   train_season, predict_season) {
  # Map to end-of-season week_idx for train & predict windows
  wk_train_end <- week_tbl |>
    filter(season == train_season) |>
    summarise(max_week = max(week_idx), .groups = "drop") |>
    pull(max_week)
  
  wk_pred_end <- week_tbl |>
    filter(season == predict_season) |>
    summarise(max_week = max(week_idx), .groups = "drop") |>
    pull(max_week)
  
  # Slice arrays to include everything up to end of predict season
  dfW <- base_df |> filter(week_idx <= wk_pred_end)
  
  # Local week/season domains for this window
  weeks_W   <- sort(unique(dfW$week_idx))
  seasons_W <- sort(unique(dfW$season))
  
  # Local remaps (important: Stan requires 1..N indexing)
  week_id_local   <- match(dfW$week_idx, weeks_W)
  season_id_local <- match(dfW$season,   seasons_W)
  
  # First/last week-of-season as local week indices
  week_tbl_W <- week_tbl |>
    filter(week_idx %in% weeks_W) |>
    mutate(season_id = match(season, seasons_W))
  
  first_week_of_season_W <- week_tbl_W |>
    group_by(season_id) |>
    summarise(first = min(week_idx), .groups = "drop") |>
    mutate(first = match(first, weeks_W)) |>
    arrange(season_id) |>
    pull(first)
  
  last_week_of_season_W <- week_tbl_W |>
    group_by(season_id) |>
    summarise(last = max(week_idx), .groups = "drop") |>
    mutate(last = match(last, weeks_W)) |>
    arrange(season_id) |>
    pull(last)
  
  # Training vs OOS partition (indices local to dfW / arrays)
  train_rows <- which(dfW$week_idx <= wk_train_end)
  oos_rows   <- which(dfW$week_idx >  wk_train_end)
  
  list(
    data = list(
      N_games   = nrow(dfW),
      N_obs     = length(train_rows),
      N_teams   = length(teams),
      N_seasons = length(seasons_W),
      N_weeks   = length(weeks_W),
      
      home_id   = as.integer(dfW$home_id),
      away_id   = as.integer(dfW$away_id),
      week_id   = as.integer(week_id_local),    # local 1..N_weeks
      season_id = as.integer(season_id_local),  # local 1..N_seasons
      
      first_week_of_season = as.integer(first_week_of_season_W),
      last_week_of_season  = as.integer(last_week_of_season_W),
      
      hfa       = as.integer(dfW$hfa),
      result    = as.numeric(dfW$result),
      
      N_oos     = length(oos_rows),
      oos_idx   = as.integer(oos_rows)          # local to arrays above
    ),
    index = list(
      dfW       = dfW,
      weeks_W   = weeks_W,
      seasons_W = seasons_W,
      train_rows = train_rows,
      oos_rows   = oos_rows,
      wk_train_end = wk_train_end,
      wk_pred_end  = wk_pred_end
    )
  )
}

## 2.3.2 Safe GQ runner (works for HMC/Laplace/Pathfinder/ADVI) ----
run_gq_safe <- function(mod, stan_data, fitted) {
  tryCatch(mod$generate_quantities(data = stan_data, fitted_params = fitted),
           error = function(e) NULL)
}

## 2.3.3 Summarize OOS draws to per-game stats ----
summarize_oos_draws <- function(draws_gq, var = c("y_oos","mu_oos"), oos_len) {
  var <- match.arg(var)
  idx_vars <- paste0(var, "[", seq_len(oos_len), "]")
  M <- posterior::as_draws_matrix(draws_gq, variables = idx_vars)
  tibble::tibble(
    game_idx_rel = seq_len(oos_len),
    mean = apply(M, 2, mean),
    sd   = apply(M, 2, sd),
    q5   = apply(M, 2, stats::quantile, probs = 0.05),
    q95  = apply(M, 2, stats::quantile, probs = 0.95)
  )
}

## 2.3.4 OOS metric calculator ----
oos_metrics <- function(pred_tbl) {
  pred_tbl |>
    dplyr::summarise(
      N = dplyr::n(),
      RMSE = sqrt(mean((mean - truth)^2)),
      MAE  = mean(abs(mean - truth)),
      Cover90 = mean(truth >= q5 & truth <= q95),
      Sharpness = mean(sd)
    )
}

## 2.3.5 Fit all methods for a given data window ----
#fit_all_methods <- function(mod, stan_data, init = NULL, seed = 20250813) {
fits <- list()
times <- tibble::tibble()

# HMC (reference)
t <- system.time({
  fits_mcmc <- mod$sample(
    data = stan_data,
    chains = 4, 
    parallel_chains = min(4, parallel::detectCores()),
    iter_warmup = 750, iter_sampling = 750,
    adapt_delta = 0.95,
    seed = seed,
    init = if (is.null(init)) 0.1 else replicate(2, init, simplify = FALSE)
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="MCMC", Elapsed_sec=as.numeric(t["elapsed"])))

# MAP (posterior mode)
t <- system.time({
  fits$MAP <- mod$optimize(
    data = stan_data,
    algorithm = "lbfgs",
    jacobian = TRUE,
    iter = 10000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="MAP", Elapsed_sec=as.numeric(t["elapsed"])))

# Laplace @ MAP
t <- system.time({
  fits$Laplace <- mod$laplace(
    data = stan_data,
    mode = fits$MAP,
    jacobian = TRUE,
    draws = 2000,
    seed = seed
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="Laplace@MAP", Elapsed_sec=as.numeric(t["elapsed"])))

# Pathfinder
t <- system.time({
  fits$Pathfinder <- mod$pathfinder(
    data = stan_data,
    num_paths = 8, num_draws = 2000,
    max_lbfgs_iters = 2000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="Pathfinder", Elapsed_sec=as.numeric(t["elapsed"])))

# ADVI (fullrank)
t <- system.time({
  fits$ADVI <- mod$variational(
    data = stan_data,
    algorithm = "fullrank",
    output_samples = 2000,
    iter = 10000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="ADVI-fullrank", Elapsed_sec=as.numeric(t["elapsed"])))

list(fits = fits, timing = times)
#}

## 2.3.6 Extract OOS predictions (where available) and score ----
collect_oos_results <- function(mod, stan_data, built_idx, fits_list, label) {
  methods_with_draws <- c("HMC","Laplace","Pathfinder","ADVI")
  out <- list(preds = list(), metrics = NULL)
  
  for (nm in names(fits_list)) {
    f <- fits_list[[nm]]
    has_draws <- any(methods_with_draws == nm)
    if (!has_draws) next
    
    gq <- run_gq_safe(mod, stan_data, f)
    if (is.null(gq)) next
    
    draws_gq <- gq$draws()
    oos_len  <- stan_data$N_oos
    if (oos_len <= 0) next
    
    summ <- summarize_oos_draws(draws_gq, var = "y_oos", oos_len = oos_len) |>
      dplyr::mutate(
        Method = dplyr::case_when(
          nm == "HMC" ~ "HMC",
          nm == "Laplace" ~ "Laplace@MAP",
          nm == "Pathfinder" ~ "Pathfinder",
          nm == "ADVI" ~ "ADVI-fullrank",
          TRUE ~ nm
        ),
        game_idx = built_idx$oos_rows,
        season = built_idx$dfW$season[built_idx$oos_rows],
        week   = built_idx$dfW$week[built_idx$oos_rows],
        truth  = built_idx$dfW$result[built_idx$oos_rows],
        label  = label
      )
    
    out$preds[[nm]] <- summ
  }
  
  preds_all <- dplyr::bind_rows(out$preds)
  metrics <- preds_all |>
    dplyr::group_by(label, Method) |>
    oos_metrics() |>
    dplyr::ungroup()
  
  list(preds = preds_all, metrics = metrics)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2.4 Stage A — Train ≤ 2022, Predict 2023 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 2.4.1 Build data window ----
built_A <- build_stan_data_window(
  base_df = game_fit_data_all |> dplyr::filter(!is.na(result)),
  week_tbl = week_tbl,
  teams = teams,
  train_season = 2022,
  predict_season = 2023
)
stan_data_A <- built_A$data

## 2.4.2 Compile (reuse your compiled model object if already loaded) ----
# hfa7_mod already compiled above; if not, compile here:
# hfa7_mod <- cmdstan_model(".../hfa7.stan")

## 2.4.3 Fit all methods ----
fits_A <- fit_all_methods(hfa7_mod, stan_data_A)

mod <- hfa7_mod
stan_data <- stan_data_A
seed <- 52
init <- NULL

fits <- list()
times <- tibble::tibble()

# HMC (reference)
t <- system.time({
  fits$MCMC <- mod$sample(
    data = stan_data,
    chains = 4, 
    parallel_chains = min(4, parallel::detectCores()),
    iter_warmup = 750, iter_sampling = 750,
    adapt_delta = 0.95,
    seed = seed,
    init = if (is.null(init)) 0.1 else replicate(2, init, simplify = FALSE)
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="MCMC", Elapsed_sec=as.numeric(t["elapsed"])))

# MAP (posterior mode)
t <- system.time({
  fits$MAP <- mod$optimize(
    data = stan_data,
    algorithm = "lbfgs",
    jacobian = TRUE,
    iter = 10000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="MAP", Elapsed_sec=as.numeric(t["elapsed"])))

# Laplace @ MAP
t <- system.time({
  fits$Laplace <- mod$laplace(
    data = stan_data,
    mode = fits$MAP,
    jacobian = TRUE,
    draws = 2000,
    seed = seed
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="Laplace@MAP", Elapsed_sec=as.numeric(t["elapsed"])))

# Pathfinder
t <- system.time({
  fits$Pathfinder <- mod$pathfinder(
    data = stan_data,
    num_paths = 8, num_draws = 2000,
    max_lbfgs_iters = 2000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="Pathfinder", Elapsed_sec=as.numeric(t["elapsed"])))

# ADVI (fullrank)
t <- system.time({
  fits$ADVI <- mod$variational(
    data = stan_data,
    algorithm = "fullrank",
    output_samples = 2000,
    iter = 10000,
    seed = seed,
    init = if (is.null(init)) 0.1 else init
  )
})
times <- dplyr::bind_rows(times, tibble::tibble(Method="ADVI-fullrank", Elapsed_sec=as.numeric(t["elapsed"])))


## 2.4.4 Score OOS (2023) ----
scored_A <- collect_oos_results(
  mod = hfa7_mod,
  stan_data = stan_data_A,
  built_idx = built_A$index,
  fits_list = fits_A$fits,
  label = "Train≤2022 → Predict 2023"
)

## 2.4.5 Save artifacts ----
dir.create("stan_oos_train_1999_2022__test_2023", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(fits_A$timing, "stan_oos_train_1999_2022__test_2023/timing.csv")
readr::write_csv(scored_A$metrics, "stan_oos_train_1999_2022__test_2023/oos_metrics.csv")
readr::write_csv(scored_A$preds,   "stan_oos_train_1999_2022__test_2023/oos_predictions.csv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2.5 Stage B — Update with 2023, Predict 2024 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 2.5.1 Build data window ----
built_B <- build_stan_data_window(
  base_df = game_fit_data_all |> dplyr::filter(!is.na(result)),
  week_tbl = week_tbl,
  teams = teams,
  train_season = 2023,
  predict_season = 2024
)
stan_data_B <- built_B$data

## 2.5.2 Warm-start from Stage A Laplace (optional but recommended) ----
# Use your existing helper to make inits from a fit with draws
inits_B <- tryCatch(
  make_inits_from_prev_fit(
    fit   = fits_A$fits$Laplace,   # has draws & metadata
    teams = teams,
    seasons_W = built_B$index$seasons_W,
    weeks_W   = built_B$index$weeks_W
  ),
  error = function(e) NULL
)

## 2.5.3 Fit all methods (warm-start if available) ----
fits_B <- fit_all_methods(hfa7_mod, stan_data_B, init = inits_B)

## 2.5.4 Score OOS (2024) ----
scored_B <- collect_oos_results(
  mod = hfa7_mod,
  stan_data = stan_data_B,
  built_idx = built_B$index,
  fits_list = fits_B$fits,
  label = "Train≤2023 → Predict 2024"
)

## 2.5.5 Save artifacts ----
dir.create("stan_oos_train_1999_2023__test_2024", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(fits_B$timing, "stan_oos_train_1999_2023__test_2024/timing.csv")
readr::write_csv(scored_B$metrics, "stan_oos_train_1999_2023__test_2024/oos_metrics.csv")
readr::write_csv(scored_B$preds,   "stan_oos_train_1999_2023__test_2024/oos_predictions.csv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2.6 Quick glance ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

bind_rows(
  scored_A$metrics,
  scored_B$metrics
) |> arrange(label, Method) |> print(n = Inf)




