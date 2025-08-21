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
# 2. MODEL hfa8 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 2.0 Helpers ----
# Helper: games observed up to a cutoff
games_through <- function(cutoff_idx) {
  which(train_data$week_idx <= cutoff_idx)
}

# Build a sliced Stan data list up to and including cutoff week W
build_stan_data_upto <- function(cutoff_wk, base_df, teams, week_tbl) {
  #   dfW <- base_df |> filter(week_idx <= cutoff_wk)
  #   
  #   weeks_W   <- sort(unique(dfW$week_idx))
  #   seasons_W <- sort(unique(dfW$season))
  #   
  #   week_tbl_W <- week_tbl |>
  #     filter(week_idx %in% weeks_W) |>
  #     mutate(season_id = as.integer(factor(season, levels = seasons_W)))
  #   
  #   first_week_of_season_W <- week_tbl_W |>
  #     group_by(season_id) |>
  #     summarise(first = min(week_idx), .groups = "drop") |>
  #     pull(first) |>
  #     match(weeks_W)
  #   
  #   last_week_of_season_W <- week_tbl_W |>
  #     group_by(season_id) |>
  #     summarise(last = max(week_idx), .groups = "drop") |>
  #     pull(last) |>
  #     match(weeks_W)
  #   
  #   week_season_W <- week_tbl_W$season_id[match(weeks_W, week_tbl_W$week_idx)]
  #   
  #   list(
  #     data = list(
  #       N_games   = nrow(dfW),
  #       N_obs     = 0L,                       # set outside
  #       N_teams   = length(teams),
  #       N_seasons = length(seasons_W),
  #       N_weeks   = length(weeks_W),
  #       home_id   = as.integer(dfW$home_id),
  #       away_id   = as.integer(dfW$away_id),
  #       week_id   = as.integer(dfW$week_idx),
  #       season_id = as.integer(dfW$season_idx),
  #       first_week_of_season = as.integer(first_week_of_season_W),
  #       last_week_of_season  = as.integer(last_week_of_season_W),
  #       hfa       = as.integer(dfW$hfa),
  #       result    = as.numeric(dfW$result),   # full vector; N_obs set outside
  #       N_oos     = 0L,                       # <— NEW
  #       oos_idx   = integer(0)                # <— NEW
  #     ),
  #     index = list(
  #       dfW = dfW,
  #       weeks_W = weeks_W,
  #       seasons_W = seasons_W
  #     )
  #   )
}

build_stan_data_upto <- function(end_train_wk,
                                 #end_oos_wk,
                                 base_df, 
                                 teams,
                                 week_tbl) {
  dfW <- base_df |> dplyr::filter(week_idx <= end_train_wk)
  
  weeks_W   <- sort(unique(dfW$week_idx))      # LOCAL week domain
  seasons_W <- sort(unique(dfW$season))        # LOCAL season domain
  
  # --- local IDs (critical) ---
  week_id_local   <- match(dfW$week_idx, weeks_W)     # 1..N_weeks
  season_id_local <- match(dfW$season,   seasons_W)   # 1..N_seasons
  
  # --- first/last week of season (local week indices) ---
  week_tbl_W <- week_tbl |>
    dplyr::filter(week_idx %in% weeks_W) |>
    dplyr::mutate(season_id = match(season, seasons_W))
  
  first_week_of_season_W <- week_tbl_W |>
    dplyr::group_by(season_id) |>
    dplyr::summarise(first = min(week_idx), .groups = "drop") |>
    dplyr::mutate(first = match(first, weeks_W)) |>
    dplyr::arrange(season_id) |>
    dplyr::pull(first)
  
  last_week_of_season_W <- week_tbl_W |>
    dplyr::group_by(season_id) |>
    dplyr::summarise(last = max(week_idx), .groups = "drop") |>
    dplyr::mutate(last = match(last, weeks_W)) |>
    dplyr::arrange(season_id) |>
    dplyr::pull(last)
  
  # --- season_of_week (local): which season each local week belongs to ---
  season_of_week_W <- week_tbl_W$season_id[match(weeks_W, week_tbl_W$week_idx)]
  
  # --- active_team[t,s] (team existed in that season?) ---
  N_teams_local <- max(c(dfW$home_id, dfW$away_id), na.rm = TRUE)
  act <- matrix(0L, nrow = N_teams_local, ncol = length(seasons_W))
  for (s_i in seq_along(seasons_W)) {
    rows <- which(dfW$season == seasons_W[s_i])
    ids  <- sort(unique(c(dfW$home_id[rows], dfW$away_id[rows])))
    if (length(ids)) act[ids, s_i] <- 1L
  }
  storage.mode(act) <- "integer"
  
  # --- OOS indexer ---
  #stan_dataA$N_obs <- sum(builtA$index$dfW$week_idx <= end_week)
  # stan_dataA$N_oos <- sum(builtA$index$dfW$week_idx > end_week)
  # stan_dataA$oos_idx <- as.integer(which(builtA$index$dfW$week_idx > end_week))
  
  list(
    data = list(
      N_games   = nrow(dfW),
      N_obs     = 0L,                       # set outside
      N_teams   = N_teams_local,
      N_seasons = length(seasons_W),
      N_weeks   = length(weeks_W),
      
      home_id   = as.integer(dfW$home_id),
      away_id   = as.integer(dfW$away_id),
      week_id   = as.integer(week_id_local),    # LOCAL
      season_id = as.integer(season_id_local),  # LOCAL
      
      first_week_of_season = as.integer(first_week_of_season_W),
      last_week_of_season  = as.integer(last_week_of_season_W),
      season_of_week       = as.integer(season_of_week_W),  # <--- NEW
      
      active_team = act,
      
      hfa       = as.integer(dfW$hfa),
      result    = as.numeric(dfW$result),
      
      N_oos     = 0L,
      oos_idx   = integer(0)
    ),
    index = list(
      dfW = dfW,
      weeks_W = weeks_W,
      seasons_W = seasons_W
    )
  )
}
#stan_dataA$N_obs <- sum(builtA$index$dfW$week_idx <= end_week)
# stan_dataA$N_oos <- sum(builtA$index$dfW$week_idx > end_week)
# stan_dataA$oos_idx <- as.integer(which(builtA$index$dfW$week_idx > end_week))

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
hfa8_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa8.stan"
)

hfa8B_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa8B.stan"
)

hfa8C_mod <- cmdstan_model(
  "~/Desktop/NFLAnalysisTest/Model Fitting/stan_models/hfa8C.stan"
)

## 2.2
## 2.2 Stan Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2002, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 10) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# Build a vector of cutoff weeks (end-of-week indices) for filtering
end_week <- week_tbl |>
  filter(week_idx == last_train_week) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

builtA <- build_stan_data_upto(end_week, train_data, teams, week_tbl)
stan_dataA <- builtA$data
stan_dataA$N_obs <- nrow(builtA$index$dfW) 
stan_dataA$N_oos <- 0L
stan_dataA$oos_idx <- integer(0)
#stan_dataA$N_obs <- sum(builtA$index$dfW$week_idx <= end_week)
# stan_dataA$N_oos <- sum(builtA$index$dfW$week_idx > end_week)
# stan_dataA$oos_idx <- as.integer(which(builtA$index$dfW$week_idx > end_week))

# prev_fitA <- NULL
# inits_listA <- if (is.null(prev_fitA)) 0.1 else {
#   make_inits_from_prev_fit(prev_fitA, teams, builtA$index$seasons_W, builtA$index$weeks_W)
# }

## 2.2.1 Sanity check ----
qa_data <- function(d){
  stopifnot(
    d$N_obs <= d$N_games,
    all(is.finite(d$result[seq_len(d$N_obs)])),
    all(d$home_id >= 1 & d$home_id <= d$N_teams),
    all(d$away_id >= 1 & d$away_id <= d$N_teams),
    all(d$week_id >= 1 & d$week_id <= d$N_weeks),
    all(d$season_id >= 1 & d$season_id <= d$N_seasons),
    all(d$first_week_of_season >= 1 & d$first_week_of_season <= d$N_weeks),
    all(d$last_week_of_season  >= 1 & d$last_week_of_season  <= d$N_weeks),
    length(d$season_of_week) == d$N_weeks
  )
  TRUE
}
qa_data(stan_dataA)

## 2.3 Fit Models ----
### MODEL A ----
#### 2.3.1 MCMC ----
tic()
fit_mcmc_A <- hfa8_mod$sample(
  data = stan_dataA,
  chains = 4, 
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 1000, 
  iter_sampling = 2000,
  adapt_delta = 0.9, 
  max_treedepth = 10, 
  init = 0,
  # refresh = 0,
  # show_messages = FALSE,
  # show_exceptions = FALSE,
  seed = 52
)
toc()

tic()
fit_mcmc_AB <- hfa8B_mod$sample(
  data = stan_dataA,
  chains = 4, 
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 1000, 
  iter_sampling = 2000,
  adapt_delta = 0.9, 
  max_treedepth = 10, 
  init = 0,
  # refresh = 0,
  # show_messages = FALSE,
  # show_exceptions = FALSE,
  seed = 52
)
toc()

tic()
fit_mcmc_AC <- hfa8C_mod$sample(
  data = stan_dataA,
  chains = 4, 
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 500, 
  iter_sampling = 1000,
  adapt_delta = 0.8, 
  max_treedepth = 10, 
  init = 0,
  # refresh = 0,
  # show_messages = FALSE,
  # show_exceptions = FALSE,
  seed = 52
)
toc()

fit_mcmc_A$time()
fit_mcmc_AB$time()
fit_mcmc_AC$time()

fit_mcmc_vars <- names(fit_mcmc_A$metadata()$stan_variable_sizes)
fit_mcmc_vars <- hfa8_mod$variables()
fit_mcmc_params <- names(fit_mcmc_vars$parameters)

fit_mcmc_A_param_draws <- fit_mcmc_A$draws()
fit_mcmc_A_param_draws |>
  spread_draws(
    c(lp__,
      beta_hfa, sigma_hfa, sigma_team_hfa,
      beta_w, sigma_w,
      beta_s, sigma_s, sigma_y,
      league_hfa_init, league_hfa,
      league_hfa_last, team_hfa_last, team_strength_last
    ),
    regex = TRUE
  ) |>
  summarise_draws(
    #"lp__"
    # "beta_hfa", "sigma_hfa", "sigma_team_hfa",
    # "beta_w", "sigma_w",
    # "beta_s", "sigma_s", "sigma_y",
    # "league_hfa_init", "league_hfa",
    # "league_hfa_last", "team_hfa_last", "team_strength_last"
    #)
  )

variables(fit_mcmc_A_param_draws)

rename_variables(
  fit_mcmc_A_param_draws,
  set_names(
    sprintf("league_hfa_z[%s]", seq_along(seasons)),
    sprintf("league_hfa_z[%s]", seasons)
  )
)

fit_mcmc_A_param_sum <- fit_mcmc_A_param_draws |>
  summarise_draws()
fit_mcmc_A_param_sum <- fit_mcmc_A$summary(
  variables = c("lp__", 
                "beta_hfa", "sigma_hfa", "sigma_team_hfa",
                "beta_w", "sigma_w",
                "beta_s", "sigma_s", "sigma_y",
                "league_hfa_init", "league_hfa",
                "league_hfa_last", "team_hfa_last", "team_strength_last"
  )
)
fit_mcmc_AB_param_sum <- fit_mcmc_AB$summary(
  variables = c("lp__", 
                "beta_hfa", "sigma_hfa", "sigma_team_hfa",
                "beta_w", "sigma_w",
                "beta_s", "sigma_s", "sigma_y",
                "league_hfa_init", "league_hfa",
                "league_hfa_last", "team_hfa_last", "team_strength_last"
  )
)
fit_mcmc_AC_param_sum <- fit_mcmc_AC$summary(
  variables = c("lp__", 
                "beta_hfa", "sigma_hfa", "sigma_team_hfa",
                "beta_w", "sigma_w",
                "beta_s", "sigma_s", "sigma_y",
                "league_hfa_init", "league_hfa",
                "league_hfa_last", "team_hfa_last", "team_strength_last"
  )
)
print(fit_mcmc_A_param_sum, n = Inf)
print(fit_mcmc_AB_param_sum, n = Inf)
print(fit_mcmc_AC_param_sum, n = Inf)

comb_sum <- bind_rows(
  fit_mcmc_A_param_sum |> mutate(order = row_number(), fit = "A", .before = 1),
  fit_mcmc_AB_param_sum |> mutate(order = row_number(), fit = "AB", .before = 1),
  fit_mcmc_AC_param_sum |> mutate(order = row_number(), fit = "AC", .before = 1)
) |>
  arrange(order)
print(comb_sum, n = Inf, digits = 4)


fit_mcmc_AB_param_sum <- fit_mcmc_AB$summary()

post <- posterior_predict(fit_mcmc_A)

#### 2.3.2 MLE ----
hfaA_mle_fit <- hfa8_mod$optimize(
  data = stan_dataA,
  #output_dir = hfaA_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = 52
)

hfaA_mle_fit_vars <- hfaA_mle_fit$metadata()$stan_variable_sizes
hfaA_mle_fit_vars
hfaA_mle_fit$output()
hfaA_mle_sum <- hfaA_mle_fit$summary(
  variables = subset(names(hfaA_mle_fit_vars),
                     hfaA_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfaA_mle_sum, n = Inf)

#### 2.3.3 MAP ----
hfaA_map_fit <- hfa8_mod$optimize(
  data = stan_dataA,
  #output_dir = hfaA_output_dir,
  iter = 20000, 
  jacobian = TRUE,
  seed = 52
)

hfaA_map_fit_vars <- hfaA_map_fit$metadata()$stan_variable_sizes
hfaA_map_fit_vars
hfaA_map_fit$output()
hfaA_map_sum <- hfaA_map_fit$summary(
  variables = subset(names(hfaA_map_fit_vars),
                     hfaA_map_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfaA_map_sum, n = Inf)

#### 2.3.4 VI ----
hfaA_mle_fit <- hfa8_mod$optimize(
  data = stan_dataA,
  #output_dir = hfaA_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = 52
)

hfaA_mle_fit_vars <- hfaA_mle_fit$metadata()$stan_variable_sizes
hfaA_mle_fit_vars
hfaA_mle_fit$output()
hfaA_mle_sum <- hfaA_mle_fit$summary(
  variables = subset(names(hfaA_mle_fit_vars),
                     hfaA_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfaA_mle_sum, n = Inf)

#### 2.3.5 Laplace ----
hfaA_mle_fit <- hfa8_mod$optimize(
  data = stan_dataA,
  #output_dir = hfaA_output_dir,
  iter = 20000, 
  jacobian = FALSE,
  seed = 52
)

hfaA_mle_fit_vars <- hfaA_mle_fit$metadata()$stan_variable_sizes
hfaA_mle_fit_vars
hfaA_mle_fit$output()
hfaA_mle_sum <- hfaA_mle_fit$summary(
  variables = subset(names(hfaA_mle_fit_vars),
                     hfaA_mle_fit_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(estimate = round(estimate, 4))
print(hfaA_mle_sum, n = Inf)

fit_mcmc_A_draws <- fit_mcmc_A$draws(format = "draws_array")
fit_mcmc_A_vars <- fit_mcmc_A$metadata()$stan_variable_sizes
fit_mcmc_A_sum <- fit_mcmc_A$summary(
  variables = subset(names(fit_mcmc_A_vars),
                     fit_mcmc_A_vars |> map_vec(\(x) length(x)==1))
) |>
  mutate(across(-variable,
                ~round(.x, 4)))
print(fit_mcmc_A_sum, n = Inf)
unconstrained_internal_draws <- fit_mcmc_A$unconstrain_draws()

fit_mcmc_A_meta <- fit_mcmc_A$metadata()
initB <- fit_mcmc_A$variable_skeleton()

fit_mcmc_B_inits <- make_inits_from_prev_fit(
  fit = fit_mcmc_A, teams = teams, seasons_W = seasons
)



### MODEL B ----
first_train_weekB <- 
  game_fit_data_all |> filter(season == 2020, week == 1)  |> pull(week_idx) |> unique()
last_train_weekB <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_dataB <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_weekB))

# Master lookup objects
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))

# Build a vector of cutoff weeks (end-of-week indices) for filtering
start_weekB <- week_tbl |>
  filter(season == 2023) |>
  summarise(min_week = min(week_idx), .groups = "drop") |>
  pull(min_week)

end_weekB <- week_tbl |>
  filter(season == 2024) |>
  summarise(max_week = max(week_idx), .groups = "drop") |>
  pull(max_week)

builtB <- build_stan_data_upto(last_train_weekB,
                               game_fit_data_all |> 
                                 #filter(!is.na(result)) |>
                                 filter(week_idx >= first_train_week), 
                               teams,
                               week_tbl)
stan_dataB <- builtB$data
stan_dataB$N_obs <- nrow(builtB$index$dfW) 
stan_dataB$N_oos   <- sum(builtB$index$dfW$week_idx > end_week)
stan_dataB$oos_idx <- as.integer(which(builtB$index$dfW$week_idx > end_week))
stan_dataB$N_obs <- nrow(builtB$index$dfW) 
stan_dataB$N_oos <- 0L
stan_dataB$oos_idx <- integer(0)

var_skel_A <- fit_mcmc_A$variable_skeleton()
fit_mcmc_A_vars <- names(fit_mcmc_A$metadata()$stan_variable_sizes)
fit_mcmc_vars <- hfa8_mod$variables()
fit_mcmc_params <- names(fit_mcmc_vars$parameters)

fit_mcmc_A_param_draws <- fit_mcmc_A$draws(
  #variables = fit_mcmc_params#, format = "draws_list"
)

fit_mcmc_A_vars <- 
  fit_mcmc_B_inits <- fit_mcmc_A_param_draws |> as_draws_list()

tic()
fit_mcmc_B <- hfa8_mod$sample(
  data = stan_dataB,
  chains = 4, 
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1000, 
  iter_sampling = 2000,
  adapt_delta = 0.95, 
  max_treedepth = 12, 
  init = function() fit_mcmc_B_inits,
  # refresh = 0,
  # show_messages = FALSE,
  # show_exceptions = FALSE,
  seed = 52
)
toc()

gen_qa <- hfa8_mod$generate_quantities(
  fit_mcmc_A,
  data = stan_dataB,
  seed = 52,
  parallel_chains = min(4, parallel::detectCores())
)

gen_qa_A <- gen_qa$print()
initB <- fit_mcmc_A$variable_skeleton()
fit_mcmc_A_vars <- names(fit_mcmc_A$metadata()$stan_variable_sizes)
fit_mcmc_vars <- hfa8_mod$variables()
fit_mcmc_params <- names(fit_mcmc_vars$parameters)

fit_mcmc_A_list <- fit_mcmc_A$draws(
  variables = fit_mcmc_A_vars, 
  format = "draws_list"
)

fit_mcmc_A_aray <- fit_mcmc_A$draws(
  variables = fit_mcmc_A_vars, 
  format = "draws_array"
)

fit_mcmc_A

set.seed(52)
fit_mcmc_B_inits_sub <- fit_mcmc_A$draws() |>
  subset_draws(
    variable = fit_mcmc_params,
    chain = 1:4,
    iteration = sample(1:fit_mcmc_A_meta$iter_sampling, 10, replace = FALSE)
  )
fit_mcmc_B_inits_sub_list <- as_draws_list(fit_mcmc_B_inits_sub)

fit_mcmc_B_inits <- make_inits_from_prev_fit(
  fit = fit_mcmc_A, 
  teams = teams, 
  seasons_W = builtB$index$seasons_W,
  weeks_W = builtB$index$weeks_W
)




