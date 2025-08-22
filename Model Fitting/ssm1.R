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
        (season < end_season   | (season == end_season & week <= end_week))
    ) |>
    arrange(season, week)
  
  list(
    N_games   = nrow(df_sub),
    N_obs     = nrow(df_sub),
    N_teams   = length(unique(df$home_id)),
    N_seasons = length(unique(df$season_idx)),
    N_weeks   = max(df$week_idx, na.rm = TRUE),
    home_id   = df_sub$home_id,
    away_id   = df_sub$away_id,
    week_id   = df_sub$week_idx,
    season_id = df_sub$season_idx,
    first_week_of_season = df |> group_by(season_idx) |> summarise(min(week_idx)) |> pull(),
    last_week_of_season  = df |> group_by(season_idx) |> summarise(max(week_idx)) |> pull(),
    hfa      = df_sub$hfa,
    result   = df_sub$result,
    N_oos    = 0,
    oos_idx  = array(0, 0)
  )
}

# Fit SSM1
fit_ssm1 <- function(mod, stan_data, inits = NULL,
                     iter_warmup = 500, iter_sampling = 1000, chains = 4,
                     adapt_delta = 0.9, max_treedepth = 10) {
  # mod <- cmdstan_model("ssm1.stan")
  mod$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    seed = 123,
    init = if (!is.null(inits)) inits else 0
  )
}

# Extract snapshot for forecasting
extract_snapshot <- function(fit) {
  draws <- fit$draws(
    format = "df",
    variables = c(
      "beta_w",
      "sigma_w",
      "beta_s",
      "sigma_s",
      "beta_hfa",
      "sigma_hfa",
      "sigma_team_hfa",
      "sigma_y",
      "team_strength_last",
      "team_hfa_last",
      "league_hfa_last"
    )
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
    league_hfa_last    = draws$league_hfa_last
  )
}

# Inits builder
make_inits <- function(snapshot) {
  list(
    beta_w   = mean(snapshot$beta_w),
    sigma_w  = mean(snapshot$sigma_w),
    beta_s   = mean(snapshot$beta_s),
    sigma_s  = mean(snapshot$sigma_s),
    beta_hfa = mean(snapshot$beta_hfa),
    sigma_hfa = mean(snapshot$sigma_hfa),
    sigma_team_hfa = mean(snapshot$sigma_team_hfa),
    sigma_y  = mean(snapshot$sigma_y)
  )
}

# Forecast with ssm1_gq
forecast_ssm1 <- function(mod_gq, snapshot, schedule, n_draws) {
  # mod_gq <- cmdstan_model("ssm1_gq.stan")
  stan_data <- list(
    N_draws   = n_draws,
    N_teams   = ncol(snapshot$team_strength_last),
    N_seasons = max(schedule$season_id),
    N_weeks   = max(schedule$week_id),
    current_season = max(schedule$season_id),
    current_week   = max(schedule$week_id) - 1,
    first_week_of_season = rep(1, max(schedule$season_id)),
    last_week_of_season  = rep(max(schedule$week_id), max(schedule$season_id)),
    beta_w   = snapshot$beta_w[1:n_draws],
    sigma_w  = snapshot$sigma_w[1:n_draws],
    beta_s   = snapshot$beta_s[1:n_draws],
    sigma_s  = snapshot$sigma_s[1:n_draws],
    beta_hfa = snapshot$beta_hfa[1:n_draws],
    sigma_hfa = snapshot$sigma_hfa[1:n_draws],
    sigma_team_hfa = snapshot$sigma_team_hfa[1:n_draws],
    sigma_y  = snapshot$sigma_y[1:n_draws],
    team_strength_T = as.matrix(snapshot$team_strength_last)[1:n_draws, ],
    team_hfa_cur    = as.matrix(snapshot$team_hfa_last)[1:n_draws, ],
    league_hfa_cur  = snapshot$league_hfa_last[1:n_draws],
    N_oos    = nrow(schedule),
    home_id  = schedule$home_id,
    away_id  = schedule$away_id,
    week_id  = schedule$week_id,
    season_id = schedule$season_id,
    hfa      = schedule$hfa
  )
  mod_gq$sample(data = stan_data, fixed_param = TRUE, iter_sampling = 1, chains = 1)
}

# Sequential wrapper
run_sequential <- function(df, start_train, end_train, final_forecast) {
  # Fit
  stan_data <- make_stan_data(df, start_train$season, start_train$week,
                              end_train$season, end_train$week)
  fit <- fit_ssm1(stan_data)
  snapshot <- extract_snapshot(fit)
  
  # Forecast (immediate week after end_train)
  forecast_week <- df |>
    filter(season == end_train$season, week == end_train$week + 1) |>
    select(home_id, away_id, week_id, season_id, hfa)
  
  forecast <- forecast_ssm1(snapshot, forecast_week, n_draws = 200)
  
  list(fit = fit, snapshot = snapshot, forecast = forecast)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. TEST RUN ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

mod <- cmdstan_model("Model Fitting/stan_models/ssm1.stan")
mod_gq <- cmdstan_model("Model Fitting/stan_models/ssm1_gq.stan")

# Fit
df = game_fit_data_all
start_train = list(season = 2002, week = 1)
end_train   = list(season = 2023, week = 10)
final_forecast = list(season = 2025, week = 1)

stan_data <- make_stan_data(df, start_train$season, start_train$week,
                            end_train$season, end_train$week)
fit <- fit_ssm1(mod_ssm1, stan_data)
fit2 <- fit_ssm1(mod, stan_data, inits = fit_inits)
snapshot <- extract_snapshot(fit)

# Forecast (immediate week after end_train)
forecast_week <- df |>
  filter(season == end_train$season, week == end_train$week + 1) |>
  select(home_id, away_id, week_id = week_idx, season_id = season_idx, hfa)

forecast <- forecast_ssm1(mod_gq, snapshot, forecast_week, n_draws = 200)

test_results <- list(fit = fit, snapshot = snapshot, forecast = forecast)

print(test_results$fit)
bayesplot::mcmc_trace(test_results$fit$draws(c("sigma_y", "beta_w")))
str(test_results$snapshot, max.level = 1)
fore_sum_y <- test_results$forecast$draws("y_pred", format = "df") |>
  spread_draws(y_pred[draw, game]) 
fore_sum_y2 <- fore_sum_y |>
  group_by(game) |>
  mean_hdci(y_pred) |>
  mutate(game_id = game_fit_data_all |> filter(week_idx == 454) |> pull(game_id)) |>
  left_join(game_fit_data_all)

p <- fore_sum_y2 |>
  #left_join(week_tbl) |>
  ggplot(
    aes(x = game_id, y = y_pred #color = team, group = team,
        # text = paste0(
        #   "team: ", team, "\n",
        #   sprintf("strength: %.2f", mean), "\n",
        #   "season: ", season, "\n",
        #   "week: ", week, "\n",
        #   "week_idx: ", week_idx
        # )
    )
  ) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper)) +
  geom_point(aes(color = "Predictiion")) +
  geom_point(aes(y = result, color = "Result")) +
  geom_point(aes(y = spread_line, color = "Spread")) +
  scale_color_brewer(palette = 1, type = "qual")
p
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

fore_sum_y <- fore_sum |> slice()
fit_inits <- make_inits(snapshot)




test_results <- run_sequential(
  df = game_fit_data_all,
  start_train = list(season = 2002, week = 1),
  end_train   = list(season = 2023, week = 10),
  final_forecast = list(season = 2025, week = 1)
)


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


# ---------- NEW HELPERS FOR A FULLY-SEQUENTIAL PIPELINE ----------

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
  draws <- fit$draws(
    format = "df",
    variables = c(
      "beta_w","sigma_w","beta_s","sigma_s",
      "beta_hfa","sigma_hfa","sigma_team_hfa","sigma_y",
      "team_strength_last","team_hfa_last","league_hfa_last"
    )
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
make_inits <- function(snapshot) {
  list(
    beta_w   = mean(snapshot$beta_w),
    sigma_w  = mean(snapshot$sigma_w),
    beta_s   = mean(snapshot$beta_s),
    sigma_s  = mean(snapshot$sigma_s),
    beta_hfa = mean(snapshot$beta_hfa),
    sigma_hfa = mean(snapshot$sigma_hfa),
    sigma_team_hfa = mean(snapshot$sigma_team_hfa),
    sigma_y  = mean(snapshot$sigma_y)
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


# ---------- TEST RUN: compile & sequential pipeline ----------

# Compile models
mod    <- cmdstan_model("Model Fitting/stan_models/ssm1.stan")
mod_gq <- cmdstan_model("Model Fitting/stan_models/ssm1_gq.stan")

# Shorthand
df <- game_fit_data_all

# Bookkeeping arrays for GQ (also useful for quick checks)
fl <- get_first_last_week(df)

# === Phase A: Fit 2002 W1 through 2023 W10; forecast 2023 W11 ===
start_train <- list(season = 2002, week = 1)
end0_train  <- list(season = 2023, week = 10)

# Sanity: what is the immediate next week?
wk_tbl <- build_week_table(df)
nxt0   <- next_week_after(wk_tbl, end0_train$season, end0_train$week)
message(sprintf("Immediate next week after %d W%02d is %d W%02d",
                end0_train$season, end0_train$week, nxt0$season, nxt0$week))

# One step to fit & forecast the next week
step0 <- sequential_step(
  df, mod, mod_gq,
  start_train = start_train,
  end_train   = end0_train,
  n_draws_gq  = 200,
  save_root   = "artifacts/ssm1_seq",  # set to NULL if you don't want files
  iter_warmup = 500, iter_sampling = 1000, chains = 4,
  adapt_delta = 0.9, max_treedepth = 10,
  prev_inits  = NULL
)

print(step0$fit)
if (nrow(step0$forecast)) {
  step0$forecast |>
    group_by(game) |>
    summarise(mu_mean = mean(mu_oos), mu_sd = sd(mu_oos), .groups = "drop") |>
    print(n = 5)
}

# === Phase B: Continue sequentially until end = 2024 W22; then forecast 2025 W01 ===
endF_train <- list(season = 2024, week = 22)

seq_run <- sequential_run(
  df, mod, mod_gq,
  start_train = start_train,
  end_train_initial = end0_train,
  end_train_final   = endF_train,
  n_draws_gq  = 200,
  save_root   = "artifacts/ssm1_seq",
  iter_warmup = 400, iter_sampling = 800, chains = 4,
  adapt_delta = 0.9, max_treedepth = 10
)

# All forecasts produced along the march (each step forecasts the next week)
cat("Number of forecasted games across the sequential run: ",
    nrow(seq_run$all_forecasts), "\n")

# Optionally summarise last few forecasts
if (nrow(seq_run$all_forecasts)) {
  tail(seq_run$all_forecasts, 10) |>
    select(forecast_season, forecast_week, game, mu_oos, y_pred) |>
    print(n = 10)
}

# === Final: Forecast 2025 Week 1 using the last snapshot ===
target <- list(season = 2025, week = 1)
sched_2025w1 <- schedule_for(df, target$season, target$week)

if (nrow(sched_2025w1) == 0) {
  warning("No schedule rows found for 2025 W01 in df; skipping final forecast.")
} else {
  fl <- get_first_last_week(df)
  final_gq <- forecast_ssm1(
    mod_gq,
    snapshot = seq_run$last_snapshot,
    schedule = sched_2025w1,
    n_draws = 200,
    first_week_of_season = fl$fw,
    last_week_of_season  = fl$lw,
    N_teams   = length(unique(df$home_id)),
    N_seasons = max(df$season_idx),
    N_weeks   = max(df$week_idx)
  )
  final_fc <- tidy_gq_output(final_gq, sched_2025w1, add_cols = TRUE) |>
    mutate(forecast_season = target$season, forecast_week = target$week)
  
  # Save + peek
  save_forecast(final_fc, "artifacts/ssm1_seq", target$season, target$week)
  final_fc |>
    group_by(game) |>
    summarise(mu_mean = mean(mu_oos), mu_sd = sd(mu_oos), .groups = "drop") |>
    print(n = 10)
}



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. HELPER FUNCTIONS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# ----------------------------- #
## I/O + basic prep ----
# ----------------------------- #

# Build integer ID maps (teams, seasons, global weeks) and return mapping tables
build_id_maps <- function(games) {
  teams <- sort(unique(c(games$home_team, games$away_team)))
  seasons <- sort(unique(games$season))
  
  # global week index (1..N_weeks) over all seasons in order
  weeks_df <- games |>
    distinct(season, week) |>
    arrange(season, week) |>
    mutate(week_global = row_number())
  
  team_map   <- tibble(team = teams, team_id = seq_along(teams))
  season_map <- tibble(season = seasons, season_id = seq_along(seasons))
  
  list(team_map = team_map, season_map = season_map, weeks_df = weeks_df)
}

# Build Stan data arrays for the full fit
# Assumes games has columns: home_team, away_team, season, week, result, hfa (0/1)
# If you don't have hfa, compute as (neutral_site == 0) etc.
build_stan_data_fit <- function(games, maps) {
  team_map   <- maps$team_map
  season_map <- maps$season_map
  weeks_df   <- maps$weeks_df
  
  # join IDs
  dat <- games |>
    left_join(team_map, by = c("home_team" = "team")) |>
    rename(home_id = team_id) |>
    left_join(team_map, by = c("away_team" = "team")) |>
    rename(away_id = team_id) |>
    left_join(season_map, by = "season") |>
    rename(season_id = season_id) |>
    left_join(weeks_df, by = c("season", "week")) |>
    rename(week_id = week_global) |>
    arrange(season, week) |>
    mutate(hfa = if ("hfa" %in% names(.)) hfa else 1L)  # default to 1 if not present
  
  # per-season first/last global week
  season_bounds <- weeks_df |>
    group_by(season) |>
    summarize(first_week_of_season = min(week_global),
              last_week_of_season  = max(week_global),
              .groups = "drop") |>
    left_join(season_map, by = "season") |>
    arrange(season_id)
  
  list(
    N_games  = nrow(dat),
    N_obs    = nrow(dat),   # all observed for the fit snapshot
    N_teams  = nrow(team_map),
    N_seasons= nrow(season_map),
    N_weeks  = nrow(weeks_df),
    
    home_id  = dat$home_id,
    away_id  = dat$away_id,
    week_id  = dat$week_id,
    season_id= dat$season_id,
    hfa      = as.integer(dat$hfa),
    result   = dat$result,
    
    first_week_of_season = season_bounds$first_week_of_season,
    last_week_of_season  = season_bounds$last_week_of_season,
    
    # oos placeholders (none during fit)
    N_oos   = 0L,
    oos_idx = integer(0),
    
    # attach convenient lookup tables
    _maps = list(team_map = team_map, season_map = season_map, weeks_df = weeks_df,
                 season_bounds = season_bounds, games_aug = dat)
  )
}

# ----------------------------- #
## Model compile (once) ----
# ----------------------------- #

get_fitted_model <- function(stan_file = "stan/nfl_state_space_model.stan") {
  cmdstan_model(stan_file)
}

get_forecaster_model <- function(stan_file = "stan/nfl_forecast_gq.stan") {
  cmdstan_model(stan_file)
}

# ------------------------------------ #
## Sampling + snapshot extraction ----
# ------------------------------------ #

# Fit the model up to the last available game in 'games'
fit_state_space <- function(model, stan_data,
                            iter_warmup = 1000, iter_sampling = 1000,
                            chains = 4, parallel_chains = 4,
                            seed = 123) {
  model$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    parallel_chains = parallel_chains,
    seed = seed
  )
}

# Extract a compact per-draw object for the current snapshot:
# scalars + final-week states + current-season HFA
# This is what we'll save and feed to the stand-alone forecaster later
pack_draws_for_gq <- function(fit, stan_data) {
  # Figure current season/week (last entries in data)
  current_bounds <- stan_data$last_week_of_season
  current_season <- which(current_bounds == max(current_bounds))[[1]]
  current_week   <- max(stan_data$week_id)
  
  # Pull draws
  dr <- fit$draws()  # posterior::draws_array
  # Scalars (draw-wise)
  pull_vec <- function(name) as.numeric(posterior::as_draws_matrix(dr, variables = name)[,1])
  
  beta_w        <- pull_vec("beta_w")
  sigma_w       <- pull_vec("sigma_w")
  beta_s        <- pull_vec("beta_s")
  sigma_s       <- pull_vec("sigma_s")
  beta_hfa      <- pull_vec("beta_hfa")
  sigma_hfa     <- pull_vec("sigma_hfa")
  sigma_team_hfa<- pull_vec("sigma_team_hfa")
  sigma_y       <- pull_vec("sigma_y")
  
  # League HFA for current season
  league_hfa_cur <- as.matrix(fit$draws("league_hfa"))[, current_season]
  
  # team_hfa for current season (vector[J] per draw)
  team_hfa_array <- fit$draws(sprintf("team_hfa[%d]", current_season)) # draws x (teams)
  team_hfa_cur   <- posterior::as_draws_matrix(team_hfa_array) |> as.matrix()
  
  # team_strength for current week (vector[J] per draw)
  theta_array <- fit$draws(sprintf("team_strength[%d]", current_week))
  team_strength_T <- posterior::as_draws_matrix(theta_array) |> as.matrix()
  
  list(
    # counts
    N_draws   = nrow(team_strength_T),
    N_teams   = stan_data$N_teams,
    N_seasons = stan_data$N_seasons,
    N_weeks   = stan_data$N_weeks,
    
    # "as-of"
    current_season = current_season,
    current_week   = current_week,
    first_week_of_season = stan_data$first_week_of_season,
    last_week_of_season  = stan_data$last_week_of_season,
    
    # scalars (draw-wise)
    beta_w = beta_w,
    sigma_w = sigma_w,
    beta_s = beta_s,
    sigma_s = sigma_s,
    beta_hfa = beta_hfa,
    sigma_hfa = sigma_hfa,
    sigma_team_hfa = sigma_team_hfa,
    sigma_y = sigma_y,
    
    # states
    team_strength_T = split(data.frame(team_strength_T), seq_len(nrow(team_strength_T))) |>
      map(~ as.numeric(.x)),
    team_hfa_cur    = split(data.frame(team_hfa_cur),    seq_len(nrow(team_hfa_cur))) |>
      map(~ as.numeric(.x)),
    league_hfa_cur  = as.numeric(league_hfa_cur),
    
    # maps for external use
    _maps = stan_data$`_maps`
  )
}

# Save compact snapshot
save_snapshot <- function(snapshot, season, week,
                          dir = "snapshots/states") {
  fs::dir_create(dir)
  path <- fs::path(dir, sprintf("snapshot_s%04d_w%02d.rds", season, week))
  saveRDS(snapshot, path)
  path
}

# ----------------------------- #
## Forecast schedule builder ----
# ----------------------------- #

# Prepare an "upcoming games" schedule (home/away/season/week/hfa) to forecast
# If you already have a tibble of upcoming games, just format columns and skip this.
build_forecast_schedule <- function(upcoming_games_tbl, maps) {
  team_map   <- maps$team_map
  season_map <- maps$season_map
  weeks_df   <- maps$weeks_df
  
  upcoming_games_tbl |>
    left_join(team_map, by = c("home_team" = "team")) |>
    rename(home_id = team_id) |>
    left_join(team_map, by = c("away_team" = "team")) |>
    rename(away_id = team_id) |>
    left_join(season_map, by = "season") |>
    rename(season_id = season_id) |>
    left_join(weeks_df, by = c("season", "week")) |>
    rename(week_id = week_global) |>
    mutate(hfa = if ("hfa" %in% names(.)) as.integer(hfa) else 1L) |>
    select(home_id, away_id, week_id, season_id, hfa)
}

# ----------------------------------------------------- #
## Build stan data for forecaster (fixed_param GQ) ----
# ----------------------------------------------------- #

# snapshot: list returned by pack_draws_for_gq (for a single fit week)
# schedule: tibble with columns: home_id, away_id, week_id, season_id, hfa
build_stan_data_forecaster <- function(snapshot, schedule) {
  # Convert per-draw vectors into shapes the forecaster.stan expects
  # team_strength_T and team_hfa_cur are lists of numeric vectors length J, one per draw
  # We'll pass them as an array[N_draws] vector[N_teams] using cmdstanr's data format
  
  # coerce lists into proper array-of-vectors (cmdstanr accepts list->array mapping)
  team_strength_T <- snapshot$team_strength_T
  team_hfa_cur    <- snapshot$team_hfa_cur
  
  list(
    N_draws  = snapshot$N_draws,
    N_teams  = snapshot$N_teams,
    N_seasons= snapshot$N_seasons,
    N_weeks  = snapshot$N_weeks,
    
    current_season = snapshot$current_season,
    current_week   = snapshot$current_week,
    first_week_of_season = snapshot$first_week_of_season,
    last_week_of_season  = snapshot$last_week_of_season,
    
    beta_w        = snapshot$beta_w,
    sigma_w       = snapshot$sigma_w,
    beta_s        = snapshot$beta_s,
    sigma_s       = snapshot$sigma_s,
    beta_hfa      = snapshot$beta_hfa,
    sigma_hfa     = snapshot$sigma_hfa,
    sigma_team_hfa= snapshot$sigma_team_hfa,
    sigma_y       = snapshot$sigma_y,
    
    team_strength_T = team_strength_T,
    team_hfa_cur    = team_hfa_cur,
    league_hfa_cur  = snapshot$league_hfa_cur,
    
    N_oos    = nrow(schedule),
    home_id  = schedule$home_id,
    away_id  = schedule$away_id,
    week_id  = schedule$week_id,
    season_id= schedule$season_id,
    hfa      = schedule$hfa
  )
}

# ---------------------------------- #
## Run forecaster (fixed_param) ----
# ---------------------------------- #

run_forecasts <- function(forecaster_model, forecaster_data, seed = 42) {
  # fixed_param: 1 sample is enough; all randomness inside GQ
  fit <- forecaster_model$sample(
    data = forecaster_data,
    chains = 1,
    parallel_chains = 1,
    iter_sampling = 1,
    iter_warmup = 0,
    fixed_param = TRUE,
    seed = seed
  )
  # Extract arrays: mu_oos[d,k], y_pred[d,k]
  draws <- fit$draws(variables = c("mu_oos", "y_pred"))
  # Shape into tibbles
  arr <- posterior::as_draws_array(draws)
  
  # mu
  mu_mat <- posterior::as_draws_matrix(arr, variables = "mu_oos") |>
    as.matrix()
  y_mat  <- posterior::as_draws_matrix(arr, variables = "y_pred") |>
    as.matrix()
  
  # Column order corresponds to k=1..N_oos
  list(mu = mu_mat, y_pred = y_mat, raw_fit = fit)
}

save_forecasts <- function(forecasts, season, week, dir = "snapshots/forecasts") {
  fs::dir_create(dir)
  path <- fs::path(dir, sprintf("forecast_from_s%04d_w%02d.rds", season, week))
  saveRDS(forecasts, path)
  path
}

# -------------------------------- #
## Summaries for tables/plots ----
# -------------------------------- #

summarize_game_predictions <- function(mu_mat, y_mat, schedule, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  # mu_mat and y_mat have rows = draws, cols = games
  mu_df <- tibble(game_index = seq_len(ncol(mu_mat))) |>
    mutate(
      mu_mean = colMeans(mu_mat),
      mu_sd   = apply(mu_mat, 2, sd)
    )
  
  # predictive summaries
  pred_df <- tibble(game_index = seq_len(ncol(y_mat))) |>
    mutate(
      y_mean = colMeans(y_mat),
      y_sd   = apply(y_mat, 2, sd)
    )
  
  # quantiles
  q_mu  <- apply(mu_mat, 2, quantile, probs = probs)
  q_y   <- apply(y_mat, 2, quantile, probs = probs)
  
  out <- schedule |>
    mutate(game_index = row_number()) |>
    left_join(mu_df, by = "game_index") |>
    left_join(pred_df, by = "game_index") |>
    mutate(
      mu_q = I(split(t(q_mu), rep(1, ncol(q_mu)))),
      y_q  = I(split(t(q_y),  rep(1, ncol(q_y))))
    )
  
  out
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. RUN PIPELINE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

maps  <- build_id_maps(games)
stan_data <- build_stan_data_fit(games, maps)

# 1) compile models once
fit_model <- get_fitted_model("stan/nfl_state_space_model.stan")
gq_model  <- get_forecaster_model("stan/nfl_forecast_gq.stan")

# 2) fit through last available week (e.g., 2024 week 22)
fit <- fit_state_space(
  model = fit_model,
  stan_data = stan_data,
  iter_warmup = 1000, iter_sampling = 1000,
  chains = 4, parallel_chains = 4, seed = 20250820
)

# 3) pack compact snapshot for forecasting + archival
snapshot <- pack_draws_for_gq(fit, stan_data)

# Save snapshot (season/week come from stan_data)
cur_season <- snapshot$current_season
cur_week   <- snapshot$current_week
snap_path  <- save_snapshot(snapshot, cur_season, cur_week)

message("Saved snapshot: ", snap_path)

# 4) Build an upcoming schedule tibble to forecast (example)
# Suppose you have a data.frame upcoming_games with columns:
# home_team, away_team, season, week, hfa (0/1)
# Here we just demonstrate with a placeholder; replace with real schedule.
upcoming_games <- tibble(
  home_team = c("NE", "KC"),
  away_team = c("NYJ", "BUF"),
  season    = c(2025, 2025),
  week      = c(1, 1),
  hfa       = c(1L, 1L)
)

schedule <- build_forecast_schedule(upcoming_games, maps)

# 5) Build forecaster data and run fixed_param GQ
gq_data <- build_stan_data_forecaster(snapshot, schedule)

forecasts <- run_forecasts(gq_model, gq_data, seed = 99)
forecast_path <- save_forecasts(forecasts, cur_season, cur_week)
message("Saved forecasts: ", forecast_path)

# 6) Summarize for tables/plots
summary_tbl <- summarize_game_predictions(
  mu_mat = forecasts$mu,
  y_mat  = forecasts$y_pred,
  schedule = schedule
)

print(summary_tbl)

