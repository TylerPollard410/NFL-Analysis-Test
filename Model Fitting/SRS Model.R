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
  ) |> 
  mutate(
    game_idx = row_number(),
    .before = 1
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
    game_id, game_idx, season, season_idx, week, week_idx = week_seq, 
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
  select(game_idx, season, season_idx, week, week_idx) |>
  distinct() |>
  arrange(week_idx)

season_breaks <- game_model_data |>
  rename(week_idx = week_seq) |>
  group_by(season) |>
  slice_min(week_idx, with_ties = FALSE) |>
  arrange(week_idx) |>
  select(season, week_idx)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

first_train_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) 
#filter(between(week_idx, first_oos_week, last_oos_week))
train_data


## 2.1 SRS no HFA cmdstanr ----

### Compile ----
stan_model <- cmdstan_model("Model Fitting/stan_models/static_SRS.stan")
mod_vars <- stan_model$variables()

### Stan Data ----
stan_data <- list(
  N_games = nrow(train_data),
  N_teams = length(teams),
  #N_weeks = max(week_tbl_rolling$week_idx),
  #N_seasons = length(fit_season_idx),
  #game_week = train_data$week_idx,
  home_id = train_data$home_id,
  away_id = train_data$away_id,
  result = train_data$result
  #game_season = train_data$season_idx,
  #week_season = week_tbl_rolling$season_idx,
  #season_start_week = season_start_week,
  #season_end_week   = season_end_week,
  #hfa = as.integer(train_data$hfa)
)

mod_seed <- 52
mod_output_dir <- "Model Fitting/stan_models"

### MCMC ----
mod_iters <- 4000
mod_warmup <- 500
mod_chains <- 1
mod_thin <- 1
mod_sims <- ((mod_iters)/mod_thin)*mod_chains
mod_parallel_chains <- parallel::detectCores()
mod_adapt_delta <- 0.95
mod_max_treedeepth <- 15

### Fit ----
#### MCMC ----
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
fit_mcmc$save_object(file = "Model Fitting/stan_models/static_SRS_MCMC.rds")

mcmc_print <- fit_mcmc$summary(variables = c("srs")) |>
  mutate(team = rep(teams, 1), .after = 1) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  )


#### MLE ----
fit_mle <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = FALSE,
  seed = mod_seed
  #init = 0.1
)

mle_print <- fit_mle$summary(variables = c("srs")) |>
  mutate(team = rep(teams, 1), .after = 1) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  )

#### MAP -----
fit_map <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = TRUE,
  seed = mod_seed
  #init = 0.1
)

map_print <- fit_map$summary(variables = c("srs")) |>
  mutate(team = rep(teams, 1), .after = 1) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  )

srs_estimates <- mcmc_print |> 
  left_join(
    mle_print |> select(team, mle_estimate = estimate)
  ) |>
  left_join(
    map_print |> select(team, map_estimate = estimate)
  ) |>
  relocate(mle_estimate, map_estimate, .after = mean) |>
  select(variable, team, mean, median, mle_estimate, map_estimate, MOV, SOS, SRS, OSRS, DSRS)

calc_srs_direct <- function(game_long_df) {
  teams <- sort(unique(game_long_df$team))
  N <- length(teams)
  team_idx <- setNames(seq_along(teams), teams)
  M <- nrow(game_long_df)
  A <- matrix(0, M, N)
  for (i in seq_len(M)) {
    A[i, team_idx[game_long_df$team[i]]] <- 1
    A[i, team_idx[game_long_df$opponent[i]]] <- -1
  }
  # Add sum-to-zero constraint
  A <- rbind(A, rep(1, N))
  b <- c(game_long_df$result, 0)
  srs <- as.vector(solve(t(A) %*% A, t(A) %*% b))
  tibble(team = teams, SRS = srs)
}

hand_calc_srs <- calc_srs_direct(team_fit_data_all |> filter(season == 2024, week %in% 1:18))

srs_estimates <- srs_estimates |>
  left_join(
    hand_calc_srs |> rename(SRS_hand = SRS)
  )

srs_estimates |> 
  summarise(
    across(-c(1:2),
           ~mean)
  )

colMeans(srs_estimates |> select(-c(1:2))) |> round(6)

tibble(
  estimate = c("MCMC", "MLE", "MAP"),
  rmse = c(
    rmse(srs_estimates$SRS, srs_estimates$mean),
    rmse(srs_estimates$SRS, srs_estimates$mle_estimate),
    rmse(srs_estimates$SRS, srs_estimates$map_estimate)
  ),
  mae = c(
    mae(srs_estimates$SRS, srs_estimates$mean),
    mae(srs_estimates$SRS, srs_estimates$mle_estimate),
    mae(srs_estimates$SRS, srs_estimates$map_estimate)
  )
)


## 2.2 SRS ALL no HFA cmdstanr ----

### Compile ----
stan_model2 <- cmdstan_model("Model Fitting/stan_models/static_SRS_full.stan")
mod_vars <- stan_model2$variables()

### Stan Data ----
stan_data <- list(
  N_games = nrow(train_data),
  N_teams = length(teams),
  #N_weeks = max(week_tbl_rolling$week_idx),
  #N_seasons = length(fit_season_idx),
  #game_week = train_data$week_idx,
  home_id = train_data$home_id,
  away_id = train_data$away_id,
  result = train_data$result,
  home_score = train_data$home_score,
  away_score = train_data$away_score
  #game_season = train_data$season_idx,
  #week_season = week_tbl_rolling$season_idx,
  #season_start_week = season_start_week,
  #season_end_week   = season_end_week,
  #hfa = as.integer(train_data$hfa)
)

mod_seed <- 52
mod_output_dir <- "Model Fitting/stan_models"

### MCMC ----
mod_iters <- 4000
mod_warmup <- 500
mod_chains <- 1
mod_thin <- 1
mod_sims <- ((mod_iters)/mod_thin)*mod_chains
mod_parallel_chains <- parallel::detectCores()
mod_adapt_delta <- 0.95
mod_max_treedeepth <- 15

### Fit ----
#### MCMC ----
fit_mcmc2 <- stan_model2$sample(
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
fit_mcmc2$save_object(file = "Model Fitting/stan_models/static_SRS_MCMC_full.rds")

mcmc_print2 <- fit_mcmc2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = mean
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )


#### MLE ----
fit_mle2 <- stan_model2$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = FALSE,
  seed = mod_seed
  #init = 0.1
)

mle_print2 <- fit_mle2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = estimate
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )

#### MAP -----
fit_map2 <- stan_model2$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = TRUE,
  seed = mod_seed
  #init = 0.1
)

map_print2 <- fit_map2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = estimate
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )

srs_estimates2 <- mcmc_print2 |> 
  left_join(
    mle_print2 |> select(team, 
                         mle_sos = sos,
                         mle_srs = srs,
                         mle_sos2 = sos2,
                         mle_srs2 = srs2,
                         mle_osrs = osrs,
                         mle_dsrs = dsrs
    )
  ) |>
  left_join(
    map_print2 |> select(team, 
                         map_sos = sos,
                         map_srs = srs,
                         map_sos2 = sos2,
                         map_srs2 = srs2,
                         map_osrs = osrs,
                         map_dsrs = dsrs
    )
  ) |>
  relocate(MOV, SOS, SRS, OSRS, DSRS, .after = last_col()) |>
  select(team, 
         MOV,
         sos, sos2, mle_sos, mle_sos2, map_sos, map_sos2, SOS,
         srs, srs2, mle_srs, mle_srs2, map_srs, map_srs2, SRS,
         osrs, mle_osrs, map_osrs, OSRS,
         dsrs, mle_dsrs, map_dsrs, DSRS
  )

srs_estimates2 |> 
  summarise(
    across(-c(1:2),
           ~mean)
  )

colMeans(srs_estimates2 |> select(-c(1))) |> round(6)

srs_mod_perf2 <- tibble(
  variable = c(rep("sos", 6), rep("srs", 6), rep("osrs", 3), rep("dsrs", 3)),
  estimate = c(rep(c("MCMC", "MCMC2", "MLE", "MLE2", "MAP", "MAP2"), 2),
               rep(c("MCMC2", "MLE2", "MAP2"), 2)),
  rmse = c(
    rmse(srs_estimates2$SOS, srs_estimates2$sos),
    rmse(srs_estimates2$SOS, srs_estimates2$sos2),
    rmse(srs_estimates2$SOS, srs_estimates2$mle_sos),
    rmse(srs_estimates2$SOS, srs_estimates2$mle_sos2),
    rmse(srs_estimates2$SOS, srs_estimates2$map_sos),
    rmse(srs_estimates2$SOS, srs_estimates2$map_sos2),
    rmse(srs_estimates2$SRS, srs_estimates2$srs),
    rmse(srs_estimates2$SRS, srs_estimates2$srs2),
    rmse(srs_estimates2$SRS, srs_estimates2$mle_srs),
    rmse(srs_estimates2$SRS, srs_estimates2$mle_srs2),
    rmse(srs_estimates2$SRS, srs_estimates2$map_srs),
    rmse(srs_estimates2$SRS, srs_estimates2$map_srs2),
    rmse(srs_estimates2$OSRS, srs_estimates2$osrs),
    rmse(srs_estimates2$OSRS, srs_estimates2$mle_osrs),
    rmse(srs_estimates2$OSRS, srs_estimates2$map_osrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$dsrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$mle_dsrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$map_dsrs)
  ),
  mae = c(
    mae(srs_estimates2$SOS, srs_estimates2$sos),
    mae(srs_estimates2$SOS, srs_estimates2$sos2),
    mae(srs_estimates2$SOS, srs_estimates2$mle_sos),
    mae(srs_estimates2$SOS, srs_estimates2$mle_sos2),
    mae(srs_estimates2$SOS, srs_estimates2$map_sos),
    mae(srs_estimates2$SOS, srs_estimates2$map_sos2),
    mae(srs_estimates2$SRS, srs_estimates2$srs),
    mae(srs_estimates2$SRS, srs_estimates2$srs2),
    mae(srs_estimates2$SRS, srs_estimates2$mle_srs),
    mae(srs_estimates2$SRS, srs_estimates2$mle_srs2),
    mae(srs_estimates2$SRS, srs_estimates2$map_srs),
    mae(srs_estimates2$SRS, srs_estimates2$map_srs2),
    mae(srs_estimates2$OSRS, srs_estimates2$osrs),
    mae(srs_estimates2$OSRS, srs_estimates2$mle_osrs),
    mae(srs_estimates2$OSRS, srs_estimates2$map_osrs),
    mae(srs_estimates2$DSRS, srs_estimates2$dsrs),
    mae(srs_estimates2$DSRS, srs_estimates2$mle_dsrs),
    mae(srs_estimates2$DSRS, srs_estimates2$map_dsrs)
  )
)


## 2.3 SRS OSRS no HFA cmdstanr ----

### Compile ----
stan_model2 <- cmdstan_model("Model Fitting/stan_models/static_SRS_full.stan")
mod_vars <- stan_model2$variables()

### Stan Data ----
stan_data <- list(
  N_games = nrow(train_data),
  N_teams = length(teams),
  #N_weeks = max(week_tbl_rolling$week_idx),
  #N_seasons = length(fit_season_idx),
  #game_week = train_data$week_idx,
  home_id = train_data$home_id,
  away_id = train_data$away_id,
  result = train_data$result,
  home_score = train_data$home_score,
  away_score = train_data$away_score
  #game_season = train_data$season_idx,
  #week_season = week_tbl_rolling$season_idx,
  #season_start_week = season_start_week,
  #season_end_week   = season_end_week,
  #hfa = as.integer(train_data$hfa)
)

mod_seed <- 52
mod_output_dir <- "Model Fitting/stan_models"

### MCMC ----
mod_iters <- 4000
mod_warmup <- 500
mod_chains <- 1
mod_thin <- 1
mod_sims <- ((mod_iters)/mod_thin)*mod_chains
mod_parallel_chains <- parallel::detectCores()
mod_adapt_delta <- 0.95
mod_max_treedeepth <- 15

### Fit ----
#### MCMC ----
fit_mcmc2 <- stan_model2$sample(
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
fit_mcmc2$save_object(file = "Model Fitting/stan_models/static_SRS_MCMC_full.rds")

mcmc_print2 <- fit_mcmc2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = mean
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )


#### MLE ----
fit_mle2 <- stan_model2$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = FALSE,
  seed = mod_seed
  #init = 0.1
)

mle_print2 <- fit_mle2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = estimate
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )

#### MAP -----
fit_map2 <- stan_model2$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = TRUE,
  seed = mod_seed
  #init = 0.1
)

map_print2 <- fit_map2$summary(variables = c("srs", "srs2", "osrs", "dsrs")) |>
  mutate(variable = str_remove(variable, "\\[.*\\]")) |>
  mutate(team = rep(teams, 4), .after = 1) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = estimate
  ) |>
  left_join(
    srs_data |>
      filter(season == 2024, week == 18) |>
      select(team, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  mutate(
    sos = srs - MOV, .before = srs
  ) |>
  mutate(
    sos2 = srs2 - MOV, .before = srs2
  )

srs_estimates2 <- mcmc_print2 |> 
  left_join(
    mle_print2 |> select(team, 
                         mle_sos = sos,
                         mle_srs = srs,
                         mle_sos2 = sos2,
                         mle_srs2 = srs2,
                         mle_osrs = osrs,
                         mle_dsrs = dsrs
    )
  ) |>
  left_join(
    map_print2 |> select(team, 
                         map_sos = sos,
                         map_srs = srs,
                         map_sos2 = sos2,
                         map_srs2 = srs2,
                         map_osrs = osrs,
                         map_dsrs = dsrs
    )
  ) |>
  relocate(MOV, SOS, SRS, OSRS, DSRS, .after = last_col()) |>
  select(team, 
         MOV,
         sos, sos2, mle_sos, mle_sos2, map_sos, map_sos2, SOS,
         srs, srs2, mle_srs, mle_srs2, map_srs, map_srs2, SRS,
         osrs, mle_osrs, map_osrs, OSRS,
         dsrs, mle_dsrs, map_dsrs, DSRS
  )

srs_estimates2 |> 
  summarise(
    across(-c(1:2),
           ~mean)
  )

colMeans(srs_estimates2 |> select(-c(1))) |> round(6)

srs_mod_perf2b <- tibble(
  variable = c(rep("sos", 6), rep("srs", 6), rep("osrs", 3), rep("dsrs", 3)),
  estimate = c(rep(c("MCMC", "MCMC2", "MLE", "MLE2", "MAP", "MAP2"), 2),
               rep(c("MCMC2", "MLE2", "MAP2"), 2)),
  rmse = c(
    rmse(srs_estimates2$SOS, srs_estimates2$sos),
    rmse(srs_estimates2$SOS, srs_estimates2$sos2),
    rmse(srs_estimates2$SOS, srs_estimates2$mle_sos),
    rmse(srs_estimates2$SOS, srs_estimates2$mle_sos2),
    rmse(srs_estimates2$SOS, srs_estimates2$map_sos),
    rmse(srs_estimates2$SOS, srs_estimates2$map_sos2),
    rmse(srs_estimates2$SRS, srs_estimates2$srs),
    rmse(srs_estimates2$SRS, srs_estimates2$srs2),
    rmse(srs_estimates2$SRS, srs_estimates2$mle_srs),
    rmse(srs_estimates2$SRS, srs_estimates2$mle_srs2),
    rmse(srs_estimates2$SRS, srs_estimates2$map_srs),
    rmse(srs_estimates2$SRS, srs_estimates2$map_srs2),
    rmse(srs_estimates2$OSRS, srs_estimates2$osrs),
    rmse(srs_estimates2$OSRS, srs_estimates2$mle_osrs),
    rmse(srs_estimates2$OSRS, srs_estimates2$map_osrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$dsrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$mle_dsrs),
    rmse(srs_estimates2$DSRS, srs_estimates2$map_dsrs)
  ),
  mae = c(
    mae(srs_estimates2$SOS, srs_estimates2$sos),
    mae(srs_estimates2$SOS, srs_estimates2$sos2),
    mae(srs_estimates2$SOS, srs_estimates2$mle_sos),
    mae(srs_estimates2$SOS, srs_estimates2$mle_sos2),
    mae(srs_estimates2$SOS, srs_estimates2$map_sos),
    mae(srs_estimates2$SOS, srs_estimates2$map_sos2),
    mae(srs_estimates2$SRS, srs_estimates2$srs),
    mae(srs_estimates2$SRS, srs_estimates2$srs2),
    mae(srs_estimates2$SRS, srs_estimates2$mle_srs),
    mae(srs_estimates2$SRS, srs_estimates2$mle_srs2),
    mae(srs_estimates2$SRS, srs_estimates2$map_srs),
    mae(srs_estimates2$SRS, srs_estimates2$map_srs2),
    mae(srs_estimates2$OSRS, srs_estimates2$osrs),
    mae(srs_estimates2$OSRS, srs_estimates2$mle_osrs),
    mae(srs_estimates2$OSRS, srs_estimates2$map_osrs),
    mae(srs_estimates2$DSRS, srs_estimates2$dsrs),
    mae(srs_estimates2$DSRS, srs_estimates2$mle_dsrs),
    mae(srs_estimates2$DSRS, srs_estimates2$map_dsrs)
  )
)

## 2.2 brms SRS ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2020, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 18) |> pull(week_idx) |> unique()

train_data_brms <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week)) |>
  mutate(homeWeight = 1, awayWeight = -1)
train_data_brms

srs_formula <- bf(
  result ~ 0 + #Intercept +
    hfa +
    (0 + hfa|gr(home_team)) +
    (1|mm(home_team, away_team,
          weights = cbind(homeWeight, awayWeight),
          scale = FALSE, cor = FALSE))
)


default_prior(srs_formula, train_data_brms)

# Define priors.
priors <- c(
  prior(normal(2, 3), coef = "hfa", class = "b"),
  prior(student_t(3, 0, 5), class = "sd",group = "home_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sd", group = "mmhome_teamaway_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sigma", lb = 0)
)

iters <- 1500
burn <- 500
chains <- 2
sims <- (iters-burn)*chains

## Stancode
srs_stanvars <- stanvar(
  scode = 
    "vector[N_1] team_hfa = rep_vector(b[1], N_1) + r_1_1;",
  block = "tparameters",
  position = "end"
)

srs_stancode <- stancode(
  srs_formula,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE,
  save_pars = save_pars(all = TRUE), 
  #stanvars = srs_stanvars,
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = parallel::detectCores(),
  #init = 0,
  normalize = T,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 52
)
srs_stancode

srs_standata <- standata(
  srs_formula,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE,
  #stanvars = srs_stanvars,
  save_pars = save_pars(all = TRUE), 
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = parallel::detectCores(),
  #init = 0,
  normalize = FALSE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 52
)
srs_standata

### 2.2.1 Fit ----
system.time(
  srs_fit <- brm(
    srs_formula,
    data = train_data_brms,
    #prior = priors,
    drop_unused_levels = FALSE,
    #stanvars = srs_stanvars,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    normalize = T,
    control = list(adapt_delta = 0.95, max_treedepth = 10),
    backend = "cmdstanr",
    seed = 52
  )
)

### Check Fit ----
print(srs_fit, digits = 4)
srs_ranef <- ranef(srs_fit)
srs_ranef

variables(srs_fit)

# brms_hfa_mod_list <- list()
# fixef_list <- list()
# ranef_list <- list()
# 
# fit <- 1
fit <- fit + 1

brms_hfa_mod_list[[paste0("fit", fit)]] <- srs_fit

srs_fixef <- fixef(srs_fit)
srs_fixef
fixef_list[[paste0("fit", fit)]] <- srs_fixef

srs_ranef <- ranef(srs_fit)
srs_ranef
ranef_list[[paste0("fit", fit)]] <- srs_ranef


loo_list <- list()
loo(srs_fit)
loo_list[[paste0("fit", fit)]] <- loo(brms_hfa_mod_list[[paste0("fit", fit)]])
loo_compare(loo_list)



variables(srs_fit)

srs_team_strength <- srs_ranef$mmhome_teamaway_team[,,"Intercept"] |>
  as_tibble(rownames = NA) |>
  rownames_to_column(var = "team")

srs_team_strength <- srs_fit |>
  spread_draws(r_mmhome_teamaway_team[team,]) |>
  rename(team_strength = r_mmhome_teamaway_team) |>
  summarise_draws()

srs_team_hfa <- srs_ranef$home_team[,,"hfa"] |>
  as_tibble(rownames = NA) |>
  rownames_to_column(var = "team")

srs_team_hfa_total <- srs_fit |>
  spread_draws(r_home_team[team, ], b_hfa) |>
  mutate(
    team_hfa = r_home_team + b_hfa
  ) |>
  summarise_draws() |>
  filter(variable == "team_hfa")

srs_team_hfa <- srs_fit |>
  spread_draws(hfa_team[team]) |>
  mutate(
    team = teams[team]
  ) |>
  median_hdci()
srs_team_hfa

srs_league_hfa <- srs_fit |>
  spread_draws(hfa_mean) |>
  median_hdci()
srs_league_hfa

srs_team_final_sum <- bind_rows(
  srs_team_hfa_total,
  srs_team_strength
) |> 
  arrange(team)

srs_team_final <- srs_team_final_sum |>
  mutate(
    variable = case_when(
      variable == "team_hfa" ~ "srs_hfa",
      variable == "team_strength" ~ "srs_strength"
    )
  ) |>
  pivot_wider(
    id_cols = team,
    names_from = variable,
    values_from = c(mean, median),
    names_glue = "{variable}_{.value}"
  )




#posterior_summary(srs_fit)
mean(srs_ranef$home_team[,,"hfa"][,"Estimate"])
mean(srs_ranef$mmhome_teamaway_team[,,"Intercept"][,"Estimate"])

sum(srs_ranef$home_team[,,"hfa"][,"Estimate"])
sum(srs_ranef$mmhome_teamaway_team[,,"Intercept"][,"Estimate"])

srs_ranef$home_team[,,"hfa"][,"Estimate"] + srs_ranef$mmhome_teamaway_team[,,"hfa"][,"Estimate"]

summ_hfa <- posterior_summary(srs_fit, variable = "team_hfa")
rownames(summ_hfa) <- paste0("team_hfa[", teams, "]")
summ_hfa

sum(summ_hfa[,"Estimate"])
mean(summ_hfa[,"Estimate"])

srs_draws_mat <- as_draws_matrix(srs_fit)
srs_draws_df <- as_draws_df(srs_fit)

srs_draws_df |> 
  select(contains("r_home_team")) |>
  slice(1:10) |>
  rowMeans()

srs_draws_df |> 
  select(contains("z_1")) |>
  slice(1:10) |>
  rowMeans()

srs_draws_df |> 
  select(contains("r_mmhome_teamaway_team")) |>
  slice(1:10) |>
  rowMeans()

srs_draws_df |> 
  select(contains("z_2")) |>
  slice(1:10) |>
  rowMeans()


srs_posterior <- posterior_predict(srs_fit) 


colnames(game_model_data)

srs_roll_comp <- game_model_data |>
  filter(!is.na(result)) |>
  #group_by(season) |>
  summarise(
    across(contains("net_SRS"),
           list(
             rmse = ~rmse(result, .x),
             mae = ~mae(result, .x)
           ),
           .unpack = FALSE
    )
  ) |>
  pivot_longer(
    #cols = -season,
    cols = everything(),
    names_to = c("srs", ".value"),
    names_pattern = "(net_SRS(?:_\\d+)?)[_]?(rmse|mae)"
  ) |>
  mutate(
    srs = case_when(
      srs == "net_SRS" ~ "cum",
      str_detect(srs, "^net_SRS_\\d+$") ~ str_replace(srs, "^net_SRS_(\\d+)$", "roll_\\1"),
      TRUE ~ srs
    )
  ) |>
  mutate(
    srs = factor(
      srs,
      levels = c("cum", paste0("roll_", 5:20))
    )
  )

srs_roll_comp |> arrange(rmse)
srs_roll_comp |> arrange(mae)

srs_roll_comp <- srs_roll_comp |>
  pivot_longer(
    cols = c(rmse, mae),
    names_to = "metric",
    values_to = "value"
  ) 

srs_roll_comp

srs_roll_comp |>
  ggplot() +
  geom_line(aes(x = srs, y = value, color = metric, group = metric)) +
  geom_vline(xintercept = srs_roll_comp |> 
               group_by(metric) |> 
               filter(value == min(value)) |>
               pull(srs)
  ) +
  #facet_wrap(~ season) +
  theme_bw()


# 3. OSRS/SRS brms -----
## 3.1 Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 10) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2023, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()

train_data_brms <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week)) |>
  select(
    game_idx, season, season_idx, week, week_idx,
    home_team, away_team, home_id, away_id,
    location, hfa,
    home_score, away_score, 
    result, spread_line,
    winner,
    total, total_line
  ) |>
  mutate(winner = ifelse(home_team == winner, 1, ifelse(away_team == winner, -1, 0))) |>
  mutate(home_weight = 1, away_weight = -1) |>
  clean_homeaway(invert = c("result", "spread_line", "hfa")) |>
  mutate(
    hfa_off = ifelse(location == "home", 1, 0),
    hfa_def = ifelse(location == "away", 1, 0),
    .after = hfa
  )
#mutate(winner = ifelse(team == winner, 1, ifelse(opponent == winner, 0, NA))) 
train_data_brms

head(train_data_brms, n = 6)
tail.matrix(train_data_brms, n = 6)

## 3.2 Splines ----
### 3.2.1 Formula ----
srs_formula <- bf(
  result ~ 0 + #Intercept +
    hfa +
    (0 + hfa|gr(home_team)) +
    (1|mm(home_team, away_team,
          weights = cbind(home_weight, away_weight),
          scale = FALSE, cor = FALSE))
) + brmsfamily(family = "student")


smooth_parts <- c(
  s(week_idx, bs = "cr", k = 5)
)

srs_formula <- bf(
  team_score ~ #0 +
    #s(week_idx, m = 1) + #, label = "leaguescore") +
    
    # League-wide HFA drift (off/def sides)
    #s(week_idx, by = hfa, bs = "tp", m = 1) + #, k = 16) +
    #s(week_idx, by = hfa_def, bs = "tp", m = 1) + #, k = 16) +
    t2(week_idx, team, by = hfa_off, bs = c("tp","re"), m = 1, full=TRUE) + #, k = 16) +
    t2(week_idx, opponent, by = hfa_def, bs = c("tp","re"), m = 1, full=TRUE) +#, k = 16)
    
    # Team offense/defense trajectories
    t2(week_idx, team, bs = c("tp","re"), m = 1, full=TRUE, k = 10) + #, k = 16) +
    t2(week_idx, opponent, bs = c("tp","re"), m = 1, full=TRUE, k = 10) #, k = 16)
)


default_prior(srs_formula, train_data_brms)

# Define priors.
priors <- c(
  prior(normal(2, 3), coef = "hfa", class = "b"),
  prior(student_t(3, 0, 5), class = "sd",group = "home_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sd", group = "mmhome_teamaway_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sigma", lb = 0)
)

iters <- 1500
burn <- 500
chains <- 2
sims <- (iters-burn)*chains

## Stancode
srs_stanvars <- stanvar(
  scode = 
    "vector[N_1] team_hfa = rep_vector(b[1], N_1) + r_1_1;",
  block = "tparameters",
  position = "end"
)

srs_stancode <- stancode(
  srs_formula,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE
  #normalize = FALSE
)
srs_stancode

srs_standata <- standata(
  srs_formula,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE,
  #stanvars = srs_stanvars,
  save_pars = save_pars(all = TRUE), 
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = min(chains, parallel::detectCores()),
  #init = 0,
  #normalize = FALSE,
  control = list(adapt_delta = 0.8,
                 max_treedepth = 10),
  backend = "cmdstanr",
  seed = 52
)
srs_standata

### 3.2.2 Fit ----
system.time(
  srs_fit <- brm(
    srs_formula,
    data = train_data_brms,
    #prior = priors,
    drop_unused_levels = FALSE,
    #stanvars = srs_stanvars,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    #normalize = F,
    control = list(adapt_delta = 0.8, max_treedepth = 10),
    backend = "cmdstanr",
    seed = 52
  )
)

### Check Fit ----
print(srs_fit, digits = 4)
srs_ranef <- ranef(srs_fit)
srs_ranef

variables(srs_fit)

loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo3 <- loo(fit3)
loo4 <- loo(fit4)
loo5 <- loo(fit5)
loo6 <- loo(fit6)

loo(srs_fit)

loo_compare(loo1, loo2, loo3, loo4, loo5, loo6)

# brms_hfa_mod_list <- list()
# fixef_list <- list()
# ranef_list <- list()
# 
# fit <- 1
fit <- fit + 1

brms_hfa_mod_list[[paste0("fit", fit)]] <- srs_fit

srs_fixef <- fixef(srs_fit)
srs_fixef
fixef_list[[paste0("fit", fit)]] <- srs_fixef

srs_ranef <- ranef(srs_fit)
srs_ranef
ranef_list[[paste0("fit", fit)]] <- srs_ranef


loo_list <- list()
loo(srs_fit)
loo_list[[paste0("fit", fit)]] <- loo(brms_hfa_mod_list[[paste0("fit", fit)]])
loo_compare(loo_list)

pp_check(srs_fit, ndraws = 100)
pp_check(srs_fit, ndraws = 100, 
         type = "dens_overlay_grouped",
         group = "team")

smooth_terms <- brmsterms(srs_formula)
smooth_terms$dpars$mu$sm

srs_conds_league <- make_conditions(
  train_data_brms |> expand(week_idx),
  vars = "week_idx",
  resolution = nrow(train_data_brms |> expand(week_idx))
)

srs_conds <- make_conditions(
  srs_fit,
  vars = "week_idx" #= train_data_brms |> expand(week_idx) |> pull(week_idx)
)


srs_smooths <- conditional_smooths(
  srs_fit,
  int_conditions = list(
    week_idx = 466:485,
    #hfa = c(1, 0, -1)
    hfa_off = c(0, 1),
    hfa_def = c(0, 1)
  )
)
plot(srs_smooths, 
     ask = FALSE,
     line_args = list(se = FALSE))


srs_smooths_league <- conditional_smooths(
  srs_fit,
  smooths = 's(week_idx,bs="tp",k=5)',
  int_conditions = list(week_idx = 466:485)
)
srs_smooths_off <- conditional_smooths(
  srs_fit,
  smooths = 't2(week_idx,team,bs=c("tp","re"),m=1,k=16)'
)
srs_smooths_def <- conditional_smooths(
  srs_fit,
  smooths = 't2(week_idx,opponent,bs=c("tp","re"),m=1,k=16)'
)
srs_smooths_off_hfa <- conditional_smooths(
  srs_fit,
  smooths = 't2(week_idx,team,bs=c("tp","re"),m=1,k=16)'
)
srs_smooths_def_hfa <- conditional_smooths(
  srs_fit,
  smooths = 't2(week_idx,opponent,bs=c("tp","re"),m=1,k=16)'
)



srs_cond_league <- srs_smooths$`mu: s(week_idx,bs="tp",k=5`
srs_cond_team_off <- srs_smooths$`mu: t2(week_idx,team,bs=c("tp","re"),m=1,k=16`
srs_cond_team_def <- srs_smooths$`mu: t2(week_idx,opponent,bs=c("tp","re"),m=1,k=16`
srs_cond_off_hfa <- srs_smooths$`mu: s(week_idx,by=hfa_off,bs="tp",m=1,k=16`
srs_cond_def_hfa <- srs_smooths$`mu: s(week_idx,by=hfa_def,bs="tp",m=1,k=16`

srs_cond_league_plot <- srs_cond_league |>
  ggplot(aes(x = week_idx, y = estimate__)) +
  geom_line() +
  labs(title = "League",
       y = "League Estimate") +
  theme_bw()
srs_cond_league_plot

srs_cond_team_off_plot <- srs_cond_team_off |>
  ggplot(aes(x = week_idx, y = estimate__, color = team, group = team)) +
  geom_line() +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Off",
       y = "Team Off Estimate") +
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
srs_cond_team_off_plot
ggplotly(srs_cond_team_off_plot)

srs_cond_team_def_plot <- srs_cond_team_def |>
  rename(team = opponent) |>
  ggplot(aes(x = week_idx, y = estimate__, color = team, group = team)) +
  geom_line() +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Deef",
       y = "Team Def Estimate") +
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
srs_cond_team_def_plot
ggplotly(srs_cond_team_def_plot)

srs_cond_off_hfa_plot <- srs_cond_off_hfa |>
  filter(hfa_off == 1) |>
  #rename(team = opponent) |>
  ggplot(aes(x = week_idx, y = estimate__)) +#, color = team, group = team)) +
  geom_line() +
  #scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Off HFA",
       y = "Team Off HFA Estimate") +
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
srs_cond_off_hfa_plot

srs_cond_def_hfa_plot <- srs_cond_def_hfa |>
  filter(hfa_def == 1) |>
  #rename(team = opponent) |>
  ggplot(aes(x = week_idx, y = estimate__)) + #, color = team, group = team)) +
  geom_line() +
  #scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team def HFA",
       y = "Team def HFA Estimate") +
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
srs_cond_def_hfa_plot

srs_cond_team_plot <- srs_cond_team_off |>
  select(week_idx, team, off = estimate__) |>
  left_join(
    srs_cond_team_def |> 
      select(week_idx, team = opponent, def = estimate__)
  ) |>
  mutate(strength = off + def) |>
  ggplot(aes(x = week_idx, y = strength, color = team, group = team)) +
  geom_line() +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength",
       y = "Team Strength Estimate") +
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
srs_cond_team_plot
ggplotly(srs_cond_team_plot)

srs_grid <- train_data_brms |>
  expand(season, week_idx, team)

srs_smooth_league <- train_data_brms |> 
  expand(week_idx) |>
  bind_cols(
    srs_fit |>
      posterior_smooths(
        's(week_idx,bs="tp",k=5)',
        newdata = train_data_brms |> expand(week_idx)
      ) |>
      summarise_draws()
  )

srs_smooth_team_off <- train_data_brms |> 
  expand(week_idx, team) |>
  bind_cols(
    srs_fit |>
      posterior_smooths(
        't2(week_idx, team, bs = c("tp", "re"), m = 1, k = 16)',
        newdata = train_data_brms |> expand(week_idx, team)
      ) |>
      summarise_draws()
  )

srs_smooth_team_def <- train_data_brms |> 
  expand(week_idx, opponent) |>
  bind_cols(
    srs_fit |>
      posterior_smooths(
        't2(week_idx, opponent, bs = c("tp", "re"), m = 1, k = 16)',
        newdata = train_data_brms |> expand(week_idx, opponent)
      ) |>
      summarise_draws()
  )

srs_smooth_hfa_off <- train_data_brms |> 
  expand(week_idx, opponent) |>
  bind_cols(
    srs_fit |>
      posterior_smooths(
        's(week_idx, by = hfa_off, bs = "tp", m = 1, k = 16)',
        newdata = train_data_brms |> expand(week_idx, opponent)
      ) |>
      summarise_draws()
  )

srs_smooth_team_def <- train_data_brms |> 
  expand(week_idx, opponent) |>
  bind_cols(
    srs_fit |>
      posterior_smooths(
        's(week_idx, by = hfa_def, bs = "tp", m = 1, k = 16)',
        newdata = train_data_brms |> expand(week_idx, opponent)
      ) |>
      summarise_draws()
  )

srs_post <- srs_grid |>
  left_join(
    srs_smooth_league |>
      select(week_idx, league_hfa = mean)
  ) |>
  left_join(
    srs_smooth_team_off |>
      select(week_idx, team, team_off = mean)
  ) |>
  left_join(
    srs_smooth_team_def |>
      select(week_idx, team = opponent, team_def = mean)
  )

srs_post2 <- srs_post |>
  mutate(
    team_hfa = team_off - team_def
  ) |>
  group_by(week_idx) |>
  summarise(
    across(-c(season, team),
           ~mean(.x))
  )


srs_pred_prep <- prepare_predictions(srs_fit)


srs_epreds <- srs_fit |>
  add_epred_draws(
    newdata = train_data_brms |>
      expand(season, week_idx, team) |>
      mutate(opponent = team),
    re_formula = NULL,
    ndraws = 100,
    dpar = "mu"
  )
srs_linpreds <- srs_fit |>
  linpred_draws(
    # newdata = train_data_brms |>
    #   expand(season, week_idx, team) |>
    #   mutate(opponent = team),
    re_formula = NULL,
    ndraws = 100
  )
get_variables(srs_fit)


## 3.3 AR ----
### 3.3.1 Formula ----
srs_formula <- bf(
  result ~ 0 + #Intercept +
    hfa +
    (0 + hfa|gr(home_team)) +
    (1|mm(home_team, away_team,
          weights = cbind(home_weight, away_weight),
          scale = FALSE, cor = FALSE))
) + brmsfamily(family = "student")

srs_formula2 <- bf(
  team_score ~
    ar(week_idx, gr = team, p = 1) + #, label = "leaguescore") +
    
    (1|team) +
    (1|opponent)
    
    # League-wide HFA drift (off/def sides)
    #ar(week_idx, gr = hfa_off, p = 1) #+
    #ar(week_idx, gr = hfa_def, p = 1) +
    
    # Team offense/defense trajectories
    #ar(team, p = 1, cov = FALSE) +
    #ar(opponent, p = 1, cov = FALSE)
)


default_prior(srs_formula2, train_data_brms)

# Define priors.
priors <- c(
  prior(normal(2, 3), coef = "hfa", class = "b"),
  prior(student_t(3, 0, 5), class = "sd",group = "home_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sd", group = "mmhome_teamaway_team", lb = 0),
  prior(student_t(3, 0, 10), class = "sigma", lb = 0)
)

iters <- 1500
burn <- 500
chains <- 2
sims <- (iters-burn)*chains

## Stancode
srs_stanvars <- stanvar(
  scode = 
    "vector[N_1] team_hfa = rep_vector(b[1], N_1) + r_1_1;",
  block = "tparameters",
  position = "end"
)

srs_stancode <- stancode(
  srs_formula2,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE
  #normalize = FALSE
)
srs_stancode

srs_standata <- standata(
  srs_formula2,
  data = train_data_brms,
  #prior = priors,
  drop_unused_levels = FALSE,
  #stanvars = srs_stanvars,
  save_pars = save_pars(all = TRUE), 
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = min(chains, parallel::detectCores()),
  #init = 0,
  #normalize = FALSE,
  control = list(adapt_delta = 0.8,
                 max_treedepth = 10),
  backend = "cmdstanr",
  seed = 52
)
srs_standata

### 3.3.2 Fit ----
system.time(
  srs_fit <- brm(
    srs_formula2,
    data = train_data_brms,
    #prior = priors,
    drop_unused_levels = FALSE,
    #stanvars = srs_stanvars,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    #normalize = F,
    control = list(adapt_delta = 0.8, max_treedepth = 10),
    backend = "cmdstanr",
    seed = 52
  )
)

### Check Fit ----
print(srs_fit, digits = 4)
srs_ranef <- ranef(srs_fit)
srs_ranef

variables(srs_fit)

loo1 <- loo(fit1)
loo2 <- loo(srs_fit)

loo_compare(loo1, loo2)

srs_smooths <- conditional_effects(
  srs_fit,
  int_conditions = list(
    week_idx = 466:485
    #hfa = c(1, 0, -1)
    #hfa_off = c(0, 1),
    #hfa_def = c(0, 1)
  )
)
plot(srs_smooths, 
     ask = FALSE,
     line_args = list(se = FALSE))
