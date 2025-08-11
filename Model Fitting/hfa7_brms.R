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
# 2. MODEL cmdstanr ----
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
    
    inits_list <- if (is.null(prev_fit)) 0.2 else {
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
      max_treedepth = 10, 
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
    
    results[[i]] <- mu_next
    toc()
    
    # --- OOS Posterior Prediction for week W from THIS fit ---
    tic("OOS Posterior Prediction Time")
    y_oos_draws <- fit_W$draws(variables = "y_oos", format = "df") 
    suppressWarnings(y_oos_draws <- y_oos_draws |> select(-c(.chain, .iteration, .draw)))
    oos_draws[[i]] <- y_oos_draws
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL brms ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 3.1 Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2006, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
# first_oos_week <- 
#   game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
# last_oos_week <- 
#   game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data_brms <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week)) |>
  mutate(homeWeight = 1, awayWeight = -1)

## 3.2 Formula and Fit params ----
hfa7_brms_formula <- bf(
  result ~ 0 + #Intercept +
    hfa +
    (0 + hfa|gr(home_team)) +
    (1 + s(week_idx)|mm(home_team, away_team,
          weights = cbind(homeWeight, awayWeight),
          scale = FALSE, cor = FALSE))
)

## 3.3 Fit ----
system.time(
  hfa7_brms_fit <- brm(
    hfa7_brms_formula,
    data = train_data_brms,
    #prior = priors,
    drop_unused_levels = FALSE,
    #stanvars = srs_stanvars,
    save_pars = save_pars(all = TRUE), 
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = parallel::detectCores(),
    #init = 0,
    #normalize = T,
    control = list(adapt_delta = 0.95, max_treedepth = 10),
    backend = "cmdstanr",
    seed = 52
  )
)



# --- data prep (same idea you used before) ---
dat <- game_fit_data_all |>
  filter(!is.na(result)) |>
  mutate(
    home_team = factor(home_team),
    away_team = factor(away_team),
    home_team_season = factor(paste0(home_team, ":", season)),
    away_team_season = factor(paste0(away_team, ":", season)),
    homeWeight = 1,
    awayWeight = -1
  )

# --- nonlinear formula: mu = str + (hleague + hteam)*hfa ---
nl_form <- bf(
  result ~ str + (hleague + hteam) * hfa,  # mean structure (no intercept)
  str ~ 0 + (1 | mm(home_team_season, away_team_season,
                    weights = cbind(homeWeight, awayWeight),
                    scale = FALSE, cor = FALSE)),
  hleague ~ 0 + (1 | season),
  hteam ~ 0 + (1 | gr(home_team_season)),
  nl = TRUE
)

# --- priors ---
# We only have group-level terms for the nl-parameters, so set sd priors per nlpar.
# Tweak scales to your historical score-diff ranges.
pri <- c(
  prior(student_t(3, 0, 10), class = "sd", nlpar = "str"),
  prior(student_t(3, 0, 5),  class = "sd", nlpar = "hleague"),
  prior(student_t(3, 0, 5),  class = "sd", nlpar = "hteam"),
  prior(student_t(3, 0, 10), class = "sigma")
  # If using student(), you can also set a prior on nu:
  # prior(gamma(2, 0.1), class = "nu")
)

fit_brms_nl <- brm(
  formula = nl_form,
  data = dat,
  family = student(),             # or gaussian()
  prior = pri,
  drop_unused_levels = FALSE,
  save_pars = save_pars(all = TRUE),
  backend = "cmdstanr",
  chains = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 10),
  seed = 52
)

# --- predictions (includes all RE by default) ---
# one-week-ahead newdata example:
# newdata <- game_fit_data_all |> dplyr::filter(week_idx == some_oos_week)
pred_draws <- posterior_predict(
  fit_brms_nl,
  newdata = dat,              # replace with your OOS set
  re_formula = NULL,
  allow_new_levels = TRUE
)








library(splines)  # for bs()

# --- Data prep (one row per game) ---
dat <- game_fit_data_all |>
  filter(!is.na(result)) |>
  mutate(
    home_team        = factor(home_team),
    away_team        = factor(away_team),
    home_team_season = factor(paste0(home_team, ":", season)),
    away_team_season = factor(paste0(away_team, ":", season)),
    homeWeight = 1,
    awayWeight = -1,
    hfa              = as.integer(hfa)  # 1 = true home, 0 otherwise
  )

# --- Build a compact spline basis for in-season week (use your 'week' 1..22) ---
K <- 6
B <- bs(dat$week, df = K, intercept = TRUE)  # includes a constant column
colnames(B) <- paste0("B", seq_len(ncol(B)) - 1)  # B0..B5
dat <- bind_cols(dat, as.data.frame(B))

# --- Programmatically build the mm random-slope term over all basis columns ---
basis_vars <- colnames(B)                         # "B0","B1",...
mm_slopes   <- paste(basis_vars, collapse = " + ")
mm_term     <- paste0("(0 + ", mm_slopes,
                      " | mm(home_team_season, away_team_season, ",
                      "weights = cbind(homeWeight, awayWeight), scale = FALSE, cor = FALSE))")

# --- Full formula: ONE team-season function, reused as + (home) and − (away) ---
# Mean: result = team_function(home, week) - team_function(away, week) + HFA terms
form_signed <- bf(
  as.formula(
    paste(
      "result ~ 0 +",
      "(0 + hfa | season) +",                   # league-season HFA
      "(0 + hfa | gr(home_team_season)) +",     # team-season HFA (only when hfa==1)
      mm_term                                   # time-varying team strength (single function)
    )
  )
) #+ ar(time = week_idx, gr = season, p = 1)      # optional residual AR(1)

# --- Priors (adjust scales to your historical score diffs) ---
pri <- c(
  prior(student_t(3, 0, 5),  class = "sd", group = "season",           coef = "hfa"),
  prior(student_t(3, 0, 5),  class = "sd", group = "home_team_season", coef = "hfa"),
  prior(student_t(3, 0, 10), class = "sd"),   # defaults for the basis slope sds
  prior(student_t(3, 0, 10), class = "sigma")
  # (Optional) put slightly tighter priors on higher-order basis sds for extra smoothness:
  # prior(student_t(3, 0, 6), class="sd", coef="B4"),
  # prior(student_t(3, 0, 4), class="sd", coef="B5")
)

fit_signed <- brm(
  formula  = form_signed,
  data     = dat,
  family   = student(),              # or gaussian()
  prior    = pri,
  backend  = "cmdstanr",
  save_pars = save_pars(all = TRUE),
  drop_unused_levels = FALSE,
  chains = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 52
)

print(fit_signed, digits = 4)
ranef(fit_signed)

pp_check(fit_signed, ndraws = 200)
conds_eff <- conditional_effects(fit_signed, re_formula = NULL)




