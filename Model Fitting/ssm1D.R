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
library(rstan)
library(cmdstanr)
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

.print_function <- function(depth = 3, enabled = TRUE) {
  if (!enabled) return(invisible(NULL))
  calls <- sys.calls()
  stack <- rev(tail(calls, depth + 1))
  fnames <- vapply(stack[-1], function(call) as.character(call[[1]]), character(1))
  fnames <- rev(fnames)
  banner <- paste0("# -------------- ", paste(fnames, collapse=" -> "), " -------------- #")
  cat("\n", banner, "\n")
  invisible(NULL)
}
.print_time <- function(start = TRUE, timer = NULL, enabled = TRUE, msg = NULL) {
  if (!enabled) return(invisible(NULL))
  pretty_time <- function(secs) {
    h <- floor(secs / 3600)
    m <- floor((secs %% 3600) / 60)
    s <- secs %% 60
    sprintf("%.3f hours %.3f minutes %.3f seconds", h, m, s)
  }
  toc_pretty <- function(tic, toc, msg = NULL, ...) {
    elapsed <- toc - tic
    msg_part <- if (!is.null(msg) && !is.na(msg) && length(msg) && nzchar(msg)) paste0(msg, ": ") else ""
    paste0("[Time] Elapsed: ", msg_part, pretty_time(elapsed))
  }
  if (start) {
    if ("tictoc" %in% loadedNamespaces()) {
      tictoc::tic(msg = msg, quiet = TRUE)
      return(invisible("tictoc"))
    } else {
      t0 <- proc.time()
      return(invisible(list(time = t0, msg = msg)))
    }
  } else {
    if (identical(timer, "tictoc")) {
      invisible(tictoc::toc(quiet = FALSE, func.toc = toc_pretty))
      cat("\n")
    } else if (!is.null(timer)) {
      elapsed <- as.numeric((proc.time() - timer$time)["elapsed"])
      msg_part <- if (!is.null(timer$msg) && !is.na(timer$msg) && length(timer$msg) && nzchar(timer$msg)) paste0(timer$msg, ": ") else ""
      cat(sprintf("[Time] Elapsed: %s%s\n", msg_part, pretty_time(elapsed)))
      cat("\n")
    }
    invisible(NULL)
  }
}
.glimpse_return <- function(out, enabled = TRUE, max.level = 1) {
  if (!enabled) return(out)
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::glimpse(out)
  } else {
    str(out, max.level = max.level)
  }
  invisible(out)
}


# ---------- Stan Data List Builder ---------- #
make_stan_data <- function(df, start_season, start_week, end_season, end_week, 
                           debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  df_sub <- df |>
    filter(
      (season > start_season | (season == start_season & week >= start_week)) &
        (season < end_season | (season == end_season & week <= end_week))
    ) |>
    arrange(season, week)
  
  result <- list(
    N_games = nrow(df_sub),
    N_obs = nrow(df_sub),
    N_teams = length(unique(df$home_id)),
    N_seasons = length(unique(df$season_idx)),
    N_weeks = max(df$week_idx, na.rm = TRUE),
    home_id = df_sub$home_id,
    away_id = df_sub$away_id,
    week_id = df_sub$week_idx,
    season_id = df_sub$season_idx,
    first_week_of_season = df |> group_by(season_idx) |> summarise(min(week_idx)) |> pull(),
    last_week_of_season  = df |> group_by(season_idx) |> summarise(max(week_idx)) |> pull(),
    hfa = df_sub$hfa,
    result = df_sub$result,
    N_oos = 0,
    oos_idx = array(0, 0)
  )
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

get_first_last_week <- function(df, 
                                debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  fw <- df |> group_by(season_idx) |> summarise(fw = min(week_idx), .groups = "drop")
  lw <- df |> group_by(season_idx) |> summarise(lw = max(week_idx), .groups = "drop")
  
  result <- tibble(season_idx = sort(unique(df$season_idx))) |>
    left_join(fw, by = "season_idx") |>
    left_join(lw, by = "season_idx") |>
    arrange(season_idx)
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

build_week_table <- function(df, 
                             debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  result <- df |>
    distinct(season, week, season_idx, week_idx) |>
    arrange(season, week)
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

next_week_after <- function(week_tbl, end_season, end_week, 
                            debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  idx <- which(week_tbl$season == end_season & week_tbl$week == end_week)
  if (length(idx) == 0 || idx == nrow(week_tbl)) return(NULL)
  
  result <- week_tbl[idx + 1, c("season", "week", "season_idx", "week_idx")]
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

schedule_for <- function(df, season, week, 
                         debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  result <- df |>
    filter(season == !!season, week == !!week) |>
    select(home_id, away_id, week_id = week_idx, season_id = season_idx, hfa)
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

# ---------- Summaries Saving Logic ---------- #
update_summary_file <- function(new_summary, file, season, week,
                                key_cols = c("season", "week"), 
                                debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  new_summary[[key_cols[1]]] <- season
  new_summary[[key_cols[2]]] <- week
  if (file.exists(file)) {
    prev <- readRDS(file)
    before_idx <- which(prev[[key_cols[1]]] < season | 
                          (prev[[key_cols[1]]] == season & prev[[key_cols[2]]] < week))
    prev <- prev[before_idx, , drop = FALSE]
    prev <- dplyr::bind_rows(prev, new_summary)
  } else {
    prev <- new_summary
  }
  
  saveRDS(prev, file)
  
  result <- prev
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

save_fit_checkpoint <- function(fit, save_root, 
                                debug_fun = TRUE, debug_time = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  fit$save_object(file = file.path(save_root, "ssm1_last_fit.rds"))
  
  .print_time(start = FALSE, timer, enabled = debug_time)
}

save_snapshot_summary <- function(snapshot_summary, save_root, season, week, 
                                  debug_fun = TRUE, debug_time = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  file <- file.path(save_root, "ssm1_snapshot_summaries.rds")
  update_summary_file(snapshot_summary, file, season, week)
  
  .print_time(start = FALSE, timer, enabled = debug_time)
}

save_forecast_summary <- function(forecast_summary, save_root, season, week, 
                                  debug_fun = TRUE, debug_time = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  file <- file.path(save_root, "ssm1_forecast_summaries.rds")
  update_summary_file(forecast_summary, file, season, week)
  
  .print_time(start = FALSE, timer, enabled = debug_time)
}

# ---------- Rolling Inits from Summaries ---------- #
make_summary_inits <- function(snapshot_summary, 
                               debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  get_means <- function(var_prefix, dim = NULL) {
    vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
    if (!is.null(dim)) matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) else vals
  }
  
  result <- list(
    league_hfa_z    = get_means("league_hfa_z"),
    league_hfa_init = get_means("league_hfa_init"),
    beta_hfa        = get_means("beta_hfa"),
    sigma_hfa       = abs(get_means("sigma_hfa")),
    team_hfa_z      = get_means("team_hfa_z"),
    sigma_team_hfa  = abs(get_means("sigma_team_hfa")),
    z_start         = get_means("z_start"),
    z_w             = get_means("z_w"),
    beta_w          = get_means("beta_w"),
    sigma_w         = abs(get_means("sigma_w")),
    beta_s          = get_means("beta_s"),
    sigma_s         = abs(get_means("sigma_s")),
    sigma_y         = abs(get_means("sigma_y"))
  )
  
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}


# ---------- Fit and Forecast Helpers ---------- #
fit_state_space <- function(mod, stan_data, inits = NULL,
                     iter_warmup = 500, iter_sampling = 1000, chains = 4,
                     adapt_delta = 0.9, max_treedepth = 10, sig_figs = 8,
                     debug_fun = TRUE, debug_time = TRUE, debug_glimpse = FALSE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  result <- mod$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    seed = 52,
    init = if (!is.null(inits)) inits else 0,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    sig_figs = sig_figs
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

make_stan_data_forecast <- function(snapshot_summary, schedule, n_draws,
                                    first_week_of_season, last_week_of_season,
                                    N_teams, N_seasons, N_weeks, 
                                    debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  mean_by_var <- function(v) snapshot_summary |> filter(variable == v) |> pull(mean)
  
  result <- list(
    N_draws   = n_draws,
    N_teams   = N_teams,
    N_seasons = N_seasons,
    N_weeks   = N_weeks,
    current_season = mean_by_var("last_season_idx"),
    current_week   = mean_by_var("last_week_idx"),
    first_week_of_season = first_week_of_season,
    last_week_of_season  = last_week_of_season,
    beta_w   = rep(mean_by_var("beta_w"), n_draws),
    sigma_w  = rep(mean_by_var("sigma_w"), n_draws),
    beta_s   = rep(mean_by_var("beta_s"), n_draws),
    sigma_s  = rep(mean_by_var("sigma_s"), n_draws),
    beta_hfa = rep(mean_by_var("beta_hfa"), n_draws),
    sigma_hfa = rep(mean_by_var("sigma_hfa"), n_draws),
    sigma_team_hfa = rep(mean_by_var("sigma_team_hfa"), n_draws),
    sigma_y  = rep(mean_by_var("sigma_y"), n_draws),
    team_strength_T = matrix(
      snapshot_summary |> filter(str_starts(variable, "team_strength_last")) |> pull(mean),
      nrow = n_draws, ncol = N_teams, byrow = TRUE
    ),
    team_hfa_cur = matrix(
      snapshot_summary |> filter(str_starts(variable, "team_hfa_last")) |> pull(mean),
      nrow = n_draws, ncol = N_teams, byrow = TRUE
    ),
    league_hfa_cur = rep(mean_by_var("league_hfa_last"), n_draws),
    N_oos   = nrow(schedule),
    home_id = schedule$home_id,
    away_id = schedule$away_id,
    week_id = schedule$week_id,
    season_id = schedule$season_id,
    hfa     = schedule$hfa
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

append_sched_to_stan_data <- function(stan_data, sched, 
                                      debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  stopifnot(is.data.frame(sched))
  new_n <- nrow(sched)
  cols <- colnames(sched)
  
  # Use list_modify and set_names to update only sched columns
  stan_data <- stan_data |>
    purrr::list_modify(!!!setNames(
      purrr::map(cols, ~ c(stan_data[[.x]], sched[[.x]])), 
      cols
    ))
  
  stan_data$N_games <- length(stan_data[[cols[1]]])
  stan_data$N_oos   <- new_n
  stan_data$oos_idx <- as.array((stan_data$N_obs + 1):(stan_data$N_obs + new_n))
  
  result <- stan_data
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

forecast_ssm1 <- function(mod_gq, stan_data,
                          iter_sampling = 1,
                          chains = 1,
                          seed = 52,
                          debug_fun = TRUE, debug_time = TRUE, debug_glimpse = FALSE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  result <- mod_gq$sample(
    data = stan_data,
    fixed_param = TRUE,
    iter_sampling = iter_sampling,
    chains = chains,
    seed = seed
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}


tidy_gq_output <- function(gq_fit, schedule, add_cols = TRUE, 
                           debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
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
  
  result <- long
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

# ---------- Main Sequential Step (Summary-Only) ---------- #
sequential_step <- function(df, mod, mod_gq,
                            start_train, end_train,
                            prev_snapshot_summary = NULL,
                            n_draws_gq = 200,
                            save_root = NULL,
                            iter_warmup = 500, iter_sampling = 1000, chains = 4,
                            adapt_delta = 0.9, max_treedepth = 10,
                            debug_fun = TRUE, debug_time = TRUE, debug_glimpse = FALSE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  stan_data <- make_stan_data(
    df,
    start_train$season, start_train$week,
    end_train$season,   end_train$week
  )
  inits <- if (!is.null(prev_snapshot_summary)) make_summary_inits(prev_snapshot_summary) else 0
  fit <- fit_state_space(
    mod, stan_data, inits = inits,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth
  )
  # Bookkeeping
  week_tbl <- build_week_table(df)
  end_row  <- week_tbl |> filter(season == end_train$season, week == end_train$week)
  last_season_idx <- end_row$season_idx[[1]]
  last_week_idx   <- end_row$week_idx[[1]]
  # Snapshot summary
  snapshot_vars <- c(
    "team_strength_last", "team_hfa_last", "league_hfa_last",
    "beta_hfa","sigma_hfa","sigma_team_hfa",
    "beta_w", "sigma_w", "beta_s", "sigma_s", "sigma_y"
  )
  snapshot_summary <- fit$summary(variables = snapshot_vars)
  snapshot_summary$last_season_idx <- last_season_idx
  snapshot_summary$last_week_idx <- last_week_idx
  snapshot_summary$season <- end_train$season
  snapshot_summary$week <- end_train$week
  if (!is.null(save_root)) {
    save_snapshot_summary(snapshot_summary, save_root, end_train$season, end_train$week)
    save_fit_checkpoint(fit, save_root)
  }
  # Forecast the very next week (summary only)
  nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
  forecast_summary <- tibble()
  if (!is.null(nxt)) {
    sched <- schedule_for(df, nxt$season, nxt$week)
    if (nrow(sched) > 0) {
      fl <- get_first_last_week(df)
      N_teams   <- length(unique(df$home_id))
      N_seasons <- max(df$season_idx)
      N_weeks   <- max(df$week_idx)
      gq_fit <- forecast_ssm1(
        mod_gq, snapshot, sched, n_draws = n_draws_gq,
        first_week_of_season = fl$fw, last_week_of_season = fl$lw,
        N_teams = N_teams, N_seasons = N_seasons, N_weeks = N_weeks
      )
      # forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE) |>
      #   mutate(forecast_season = nxt$season, forecast_week = nxt$week)
      # stan_data_forecast <- make_stan_data_forecast(
      #   snapshot_summary = snapshot_summary,
      #   schedule = sched,
      #   n_draws = n_draws_gq,
      #   first_week_of_season = fl$fw,
      #   last_week_of_season  = fl$lw,
      #   N_teams   = N_teams,
      #   N_seasons = N_seasons,
      #   N_weeks   = N_weeks
      # )
      # gq_fit <- forecast_ssm1(
      #   mod_gq,
      #   stan_data = stan_data_forecast,
      #   iter_sampling = 1,
      #   chains = 1,
      #   seed = 52
      # )
      forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE)
      forecast_summary <- forecast_tbl |>
        group_by(game, season_id, week_id, home_id, away_id, hfa) |>
        summarise(
          mu_mean = mean(mu_oos),
          mu_sd = sd(mu_oos),
          mu_p05 = quantile(mu_oos, 0.05),
          mu_p95 = quantile(mu_oos, 0.95),
          y_pred_mean = mean(y_pred),
          y_pred_sd = sd(y_pred),
          y_pred_p05 = quantile(y_pred, 0.05),
          y_pred_p95 = quantile(y_pred, 0.95),
          .groups = "drop"
        ) |>
        mutate(forecast_season = nxt$season, forecast_week = nxt$week)
      if (!is.null(save_root)) {
        save_forecast_summary(forecast_summary, save_root, nxt$season, nxt$week)
      }
    }
  }
  result <- list(
    fit = fit,
    snapshot_summary = snapshot_summary,
    forecast_summary = forecast_summary,
    next_pointer = if (is.null(nxt)) NULL else list(season = nxt$season, week = nxt$week)
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

# ---------- Sequential Runner (Summary-Only) ---------- #
sequential_run <- function(df, mod, mod_gq,
                           start_train, end_train_initial, end_train_final,
                           n_draws_gq = 200,
                           save_root = NULL,
                           iter_warmup = 500, iter_sampling = 1000, chains = 4,
                           adapt_delta = 0.9, max_treedepth = 10,
                           debug_fun = TRUE, debug_time = TRUE, debug_glimpse = TRUE) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)
  
  week_tbl <- build_week_table(df)
  start_row <- week_tbl |> filter(season == start_train$season, week == start_train$week)
  end0_row  <- week_tbl |> filter(season == end_train_initial$season, week == end_train_initial$week)
  endF_row  <- week_tbl |> filter(season == end_train_final$season, week == end_train_final$week)
  stopifnot(nrow(start_row) == 1, nrow(end0_row) == 1, nrow(endF_row) == 1)
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
  last_snapshot_summary <- NULL
  # First fit at end0 (use default inits)
  step0 <- sequential_step(
    df, mod, mod_gq,
    start_train = start_train,
    end_train   = list(season = end_train_initial$season, week = end_train_initial$week),
    prev_snapshot_summary = NULL,
    n_draws_gq = n_draws_gq,
    save_root = save_root,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth
  )
  last_snapshot_summary <- step0$snapshot_summary
  # Loop forward with summary-only logic
  if (nrow(endpoints) > 1) {
    for (r in 2:nrow(endpoints)) {
      endr <- endpoints[r, ]
      stepr <- sequential_step(
        df, mod, mod_gq,
        start_train = start_train,
        end_train   = list(season = endr$season, week = endr$week),
        prev_snapshot_summary = last_snapshot_summary,
        n_draws_gq  = n_draws_gq,
        save_root   = save_root,
        iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
        adapt_delta = adapt_delta, max_treedepth = max_treedepth
      )
      last_snapshot_summary <- stepr$snapshot_summary
    }
  }
  result <- list(last_snapshot_summary = last_snapshot_summary)
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. FULL SEQUENTIAL SSM1 WORKFLOW: Rolling Stan Fits + Forecasts ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

start_train      <- list(season = 2002, week = 1)
end_train_init   <- list(season = 2005, week = 14)
end_train_final  <- list(season = 2005, week = 15)
n_draws_gq      <- 200
save_root       <- "Model Fitting/stan_models"
iter_warmup     <- 250
iter_sampling   <- 500
chains          <- 4
adapt_delta     <- 0.9
max_treedepth   <- 10

mod_ssm1    <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1_gq.stan"))

seq_result <- sequential_run(
  df                = game_fit_data_all,
  mod               = mod_ssm1,
  mod_gq            = mod_ssm1_gq,
  start_train       = start_train,
  end_train_initial = end_train_init,
  end_train_final   = end_train_final,
  n_draws_gq        = n_draws_gq,
  save_root         = save_root,
  iter_warmup       = iter_warmup,
  iter_sampling     = iter_sampling,
  chains            = chains,
  adapt_delta       = adapt_delta,
  max_treedepth     = max_treedepth
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. TEST HELPERS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

start_train      <- list(season = 2002, week = 1)
end_train_init   <- list(season = 2005, week = 14)
end_train_final  <- list(season = 2005, week = 15)
n_draws_gq      <- 200
save_root       <- "Model Fitting/stan_models"
iter_warmup     <- 250
iter_sampling   <- 500
chains          <- 4
adapt_delta     <- 0.9
max_treedepth   <- 10

mod_ssm1    <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1D_gq.stan"))


## -------------- sequential_run -------------- 
df                = game_fit_data_all
mod               = mod_ssm1
mod_gq            = mod_ssm1_gq
start_train       = start_train
end_train_initial = end_train_init
end_train_final   = end_train_final
n_draws_gq        = n_draws_gq
save_root         = save_root
iter_warmup       = iter_warmup
iter_sampling     = iter_sampling
chains            = chains
adapt_delta       = adapt_delta
max_treedepth     = max_treedepth

### -------------- sequential_run -> build_week_table -------------- 
week_tbl <- build_week_table(df)
start_row <- week_tbl |> filter(season == start_train$season, week == start_train$week)
end0_row  <- week_tbl |> filter(season == end_train_initial$season, week == end_train_initial$week)
endF_row  <- week_tbl |> filter(season == end_train_final$season, week == end_train_final$week)
stopifnot(nrow(start_row) == 1, nrow(end0_row) == 1, nrow(endF_row) == 1)
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
last_snapshot_summary <- NULL

### -------------- sequential_run -> sequential_step --------------  
#step0 <- sequential_step(
df = df
mod = mod
mod_gq = mod_gq
start_train = start_train
end_train   = list(season = end_train_initial$season, week = end_train_initial$week)
prev_snapshot_summary = NULL
n_draws_gq = n_draws_gq
save_root = save_root
iter_warmup = iter_warmup 
iter_sampling = iter_sampling 
chains = chains
adapt_delta = adapt_delta
max_treedepth = max_treedepth
#)
### -------------- sequential_run -> sequential_step -> make_stan_data -------------- 
stan_data <- make_stan_data(
  df,
  start_train$season, start_train$week,
  end_train$season,   end_train$week
)
inits <- if (!is.null(prev_snapshot_summary)) make_summary_inits(prev_snapshot_summary) else 0

### -------------- sequential_run -> sequential_step -> fit_state_space --------------  
fit <- fit_state_space(
  mod, stan_data, inits = inits,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth
)
# Bookkeeping
### -------------- sequential_run -> sequential_step -> build_week_table --------------  
week_tbl <- build_week_table(df)
end_row  <- week_tbl |> filter(season == end_train$season, week == end_train$week)
last_season_idx <- end_row$season_idx[[1]]
last_week_idx   <- end_row$week_idx[[1]]
# Snapshot summary
snapshot_vars <- c(
  "team_strength_last", "team_hfa_last", "league_hfa_last",
  "beta_hfa","sigma_hfa","sigma_team_hfa",
  "beta_w", "sigma_w", "beta_s", "sigma_s", "sigma_y"
)
snapshot_summary <- fit$summary(variables = snapshot_vars)
snapshot_summary$last_season_idx <- last_season_idx
snapshot_summary$last_week_idx <- last_week_idx
snapshot_summary$season <- end_train$season
snapshot_summary$week <- end_train$week
#if (!is.null(save_root)) {
# if (!is.null(save_root)) {
#   save_snapshot_summary(snapshot_summary, save_root, end_train$season, end_train$week)
#   save_fit_checkpoint(fit, save_root)
# }
!is.null(save_root)
### -------------- sequential_run -> sequential_step -> save_snapshot_summary --------------
#### -------------- sequential_step -> save_snapshot_summary -> update_summary_file --------------
#save_snapshot_summary(snapshot_summary, save_root, end_train$season, end_train$week)
season = end_train$season
week = end_train$week

file <- file.path(save_root, "ssm1_snapshot_summaries.rds")

##update_summary_file(snapshot_summary, file)
new_summary = snapshot_summary
file = file
season = season
week = week
key_cols = c("season", "week")

new_summary[[key_cols[1]]] <- season
new_summary[[key_cols[2]]] <- week
#if (file.exists(file)) {
file.exists(file)
prev <- readRDS(file)
before_idx <- which(prev[[key_cols[1]]] < season | 
                      (prev[[key_cols[1]]] == season & prev[[key_cols[2]]] < week))
prev <- prev[before_idx, , drop = FALSE]
prev <- dplyr::bind_rows(prev, new_summary)
#} else {
prev <- new_summary
#}

saveRDS(prev, file)

### -------------- sequential_run -> sequential_step -> save_fit_checkpoint -------------- 
#save_fit_checkpoint(fit, save_root)
fit$save_object(file = file.path(save_root, "ssm1_last_fit.rds"))
#}

### -------------- sequential_run -> sequential_step -> next_week_after -------------- 
#nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
end_season = end_train$season
end_week = end_train$week
idx <- which(week_tbl$season == end_season & week_tbl$week == end_week)
if (length(idx) == 0 || idx == nrow(week_tbl)) return(NULL)
nxt <- week_tbl[idx + 1, c("season", "week", "season_idx", "week_idx")]

forecast_summary <- tibble()
#if (!is.null(nxt)) {
!is.null(nxt)
### -------------- sequential_run -> sequential_step -> schedule_for -------------- 
# sched <- schedule_for(df, nxt$season, nxt$week)
season = nxt$season
week =  nxt$week
sched <- df |>
  filter(season == !!season, week == !!week) |>
  select(home_id, away_id, week_id = week_idx, season_id = season_idx, hfa)

#if (nrow(sched) > 0) {
nrow(sched) > 0

### -------------- sequential_run -> sequential_step -> get_first_last_week -------------- 
#fl <- get_first_last_week(df)
fw <- df |> group_by(season_idx) |> summarise(fw = min(week_idx), .groups = "drop")
lw <- df |> group_by(season_idx) |> summarise(lw = max(week_idx), .groups = "drop")

fl <- tibble(season_idx = sort(unique(df$season_idx))) |>
  left_join(fw, by = "season_idx") |>
  left_join(lw, by = "season_idx") |>
  arrange(season_idx)

N_teams   <- length(unique(df$home_id))
N_seasons <- max(df$season_idx)
N_weeks   <- max(df$week_idx)

### -------------- sequential_run -> sequential_step -> make_stan_data_forecast -------------- 
# stan_data_forecast <- make_stan_data_forecast(
#   snapshot_summary = snapshot_summary,
#   schedule = sched,
#   n_draws = n_draws_gq,
#   first_week_of_season = fl$fw,
#   last_week_of_season  = fl$lw,
#   N_teams   = N_teams,
#   N_seasons = N_seasons,
#   N_weeks   = N_weeks
# )
snapshot_summary = snapshot_summary
schedule = sched
n_draws = n_draws_gq
first_week_of_season = fl$fw
last_week_of_season  = fl$lw
N_teams   = N_teams
N_seasons = N_seasons
N_weeks   = N_weeks

# mean_by_var <- function(v) snapshot_summary |> filter(variable == v) |> pull(mean)
# stan_data_forecast <- list(
#   N_draws   = n_draws,
#   N_teams   = N_teams,
#   N_seasons = N_seasons,
#   N_weeks   = N_weeks,
#   current_season = mean_by_var("last_season_idx"),
#   current_week   = mean_by_var("last_week_idx"),
#   first_week_of_season = first_week_of_season,
#   last_week_of_season  = last_week_of_season,
#   beta_w   = rep(mean_by_var("beta_w"), n_draws),
#   sigma_w  = rep(mean_by_var("sigma_w"), n_draws),
#   beta_s   = rep(mean_by_var("beta_s"), n_draws),
#   sigma_s  = rep(mean_by_var("sigma_s"), n_draws),
#   beta_hfa = rep(mean_by_var("beta_hfa"), n_draws),
#   sigma_hfa = rep(mean_by_var("sigma_hfa"), n_draws),
#   sigma_team_hfa = rep(mean_by_var("sigma_team_hfa"), n_draws),
#   sigma_y  = rep(mean_by_var("sigma_y"), n_draws),
#   team_strength_T = matrix(
#     snapshot_summary |> filter(str_starts(variable, "team_strength_last")) |> pull(mean),
#     nrow = n_draws, ncol = N_teams, byrow = TRUE
#   ),
#   team_hfa_cur = matrix(
#     snapshot_summary |> filter(str_starts(variable, "team_hfa_last")) |> pull(mean),
#     nrow = n_draws, ncol = N_teams, byrow = TRUE
#   ),
#   league_hfa_cur = rep(mean_by_var("league_hfa_last"), n_draws),
#   N_oos   = nrow(schedule),
#   home_id = schedule$home_id,
#   away_id = schedule$away_id,
#   week_id = schedule$week_id,
#   season_id = schedule$season_id,
#   hfa     = schedule$hfa
# )
# unique(snapshot_summary$variable)
# [1] "team_strength_last[1]"  "team_strength_last[2]"  "team_strength_last[3]"  "team_strength_last[4]"  "team_strength_last[5]" 
# [6] "team_strength_last[6]"  "team_strength_last[7]"  "team_strength_last[8]"  "team_strength_last[9]"  "team_strength_last[10]"
# [11] "team_strength_last[11]" "team_strength_last[12]" "team_strength_last[13]" "team_strength_last[14]" "team_strength_last[15]"
# [16] "team_strength_last[16]" "team_strength_last[17]" "team_strength_last[18]" "team_strength_last[19]" "team_strength_last[20]"
# [21] "team_strength_last[21]" "team_strength_last[22]" "team_strength_last[23]" "team_strength_last[24]" "team_strength_last[25]"
# [26] "team_strength_last[26]" "team_strength_last[27]" "team_strength_last[28]" "team_strength_last[29]" "team_strength_last[30]"
# [31] "team_strength_last[31]" "team_strength_last[32]" "team_hfa_last[1]"       "team_hfa_last[2]"       "team_hfa_last[3]"      
# [36] "team_hfa_last[4]"       "team_hfa_last[5]"       "team_hfa_last[6]"       "team_hfa_last[7]"       "team_hfa_last[8]"      
# [41] "team_hfa_last[9]"       "team_hfa_last[10]"      "team_hfa_last[11]"      "team_hfa_last[12]"      "team_hfa_last[13]"     
# [46] "team_hfa_last[14]"      "team_hfa_last[15]"      "team_hfa_last[16]"      "team_hfa_last[17]"      "team_hfa_last[18]"     
# [51] "team_hfa_last[19]"      "team_hfa_last[20]"      "team_hfa_last[21]"      "team_hfa_last[22]"      "team_hfa_last[23]"     
# [56] "team_hfa_last[24]"      "team_hfa_last[25]"      "team_hfa_last[26]"      "team_hfa_last[27]"      "team_hfa_last[28]"     
# [61] "team_hfa_last[29]"      "team_hfa_last[30]"      "team_hfa_last[31]"      "team_hfa_last[32]"      "league_hfa_last"       
# [66] "beta_hfa"               "sigma_hfa"              "sigma_team_hfa"         "beta_w"                 "sigma_w"               
# [71] "beta_s"                 "sigma_s"                "sigma_y"
gq__data_vars <- mod_gq$variables()$data |> bind_rows(.id = "variable")

N_draws   = n_draws
N_teams   = N_teams
N_seasons = N_seasons
N_weeks   = N_weeks

# current_season = mean_by_var("last_season_idx")
current_season = snapshot_summary |> filter(variable == "last_season_idx") |> pull(mean)
# current_week   = mean_by_var("last_week_idx")


first_week_of_season = first_week_of_season
last_week_of_season  = last_week_of_season

beta_w   = rep(mean_by_var("beta_w"), n_draws)
sigma_w  = rep(mean_by_var("sigma_w"), n_draws)
beta_s   = rep(mean_by_var("beta_s"), n_draws)
sigma_s  = rep(mean_by_var("sigma_s"), n_draws)
beta_hfa = rep(mean_by_var("beta_hfa"), n_draws)
sigma_hfa = rep(mean_by_var("sigma_hfa"), n_draws)
sigma_team_hfa = rep(mean_by_var("sigma_team_hfa"), n_draws)
sigma_y  = rep(mean_by_var("sigma_y"), n_draws)
team_strength_T = matrix(
  snapshot_summary |> filter(str_starts(variable, "team_strength_last")) |> pull(mean),
  nrow = n_draws, ncol = N_teams, byrow = TRUE
)
team_hfa_cur = matrix(
  snapshot_summary |> filter(str_starts(variable, "team_hfa_last")) |> pull(mean),
  nrow = n_draws, ncol = N_teams, byrow = TRUE
)
league_hfa_cur = rep(mean_by_var("league_hfa_last"), n_draws)
N_oos   = nrow(schedule)
home_id = schedule$home_id
away_id = schedule$away_id
week_id = schedule$week_id
season_id = schedule$season_id
hfa     = schedule$hfa




stan_data_forecast <- list(
  N_draws   = n_draws,
  N_teams   = N_teams,
  N_seasons = N_seasons,
  N_weeks   = N_weeks,
  current_season = mean_by_var("last_season_idx"),
  current_week   = mean_by_var("last_week_idx"),
  first_week_of_season = first_week_of_season,
  last_week_of_season  = last_week_of_season,
  beta_w   = rep(mean_by_var("beta_w"), n_draws),
  sigma_w  = rep(mean_by_var("sigma_w"), n_draws),
  beta_s   = rep(mean_by_var("beta_s"), n_draws),
  sigma_s  = rep(mean_by_var("sigma_s"), n_draws),
  beta_hfa = rep(mean_by_var("beta_hfa"), n_draws),
  sigma_hfa = rep(mean_by_var("sigma_hfa"), n_draws),
  sigma_team_hfa = rep(mean_by_var("sigma_team_hfa"), n_draws),
  sigma_y  = rep(mean_by_var("sigma_y"), n_draws),
  team_strength_T = matrix(
    snapshot_summary |> filter(str_starts(variable, "team_strength_last")) |> pull(mean),
    nrow = n_draws, ncol = N_teams, byrow = TRUE
  ),
  team_hfa_cur = matrix(
    snapshot_summary |> filter(str_starts(variable, "team_hfa_last")) |> pull(mean),
    nrow = n_draws, ncol = N_teams, byrow = TRUE
  ),
  league_hfa_cur = rep(mean_by_var("league_hfa_last"), n_draws),
  N_oos   = nrow(schedule),
  home_id = schedule$home_id,
  away_id = schedule$away_id,
  week_id = schedule$week_id,
  season_id = schedule$season_id,
  hfa     = schedule$hfa
)

stopifnot(is.data.frame(sched))
new_n <- nrow(sched)
cols <- colnames(sched)

stan_data_forecast <- stan_data |>
  purrr::list_modify(!!!purrr::set_names(
    purrr::map(cols, ~ c(stan_data[[.x]], sched[[.x]])),
    cols
  ))

stan_data_forecast$N_games <- length(stan_data_forecast[[cols[1]]])
stan_data_forecast$N_oos   <- new_n
stan_data_forecast$oos_idx <- as.array((stan_data_forecast$N_obs + 1):(stan_data_forecast$N_obs + new_n))

stan_data

mod_gq <- 

### -------------- sequential_run -> sequential_step -> forecast_ssm1 -------------- 
gq_fit <- forecast_ssm1(
  mod_gq,
  stan_data = stan_data_forecast,
  iter_sampling = 1,
  chains = 1,
  seed = 52
)

gq_fit <- mod_gq$generate_quantities(
  fit,
  data = stan_data_forecast, 
  seed = 52,
  sig_figs = 10
  )
gq_fit$output()
gq_fit$print()
gq_fit$summary()
forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE)
forecast_summary <- forecast_tbl |>
  group_by(game, season_id, week_id, home_id, away_id, hfa) |>
  summarise(
    mu_mean = mean(mu_oos),
    mu_sd = sd(mu_oos),
    mu_p05 = quantile(mu_oos, 0.05),
    mu_p95 = quantile(mu_oos, 0.95),
    y_pred_mean = mean(y_pred),
    y_pred_sd = sd(y_pred),
    y_pred_p05 = quantile(y_pred, 0.05),
    y_pred_p95 = quantile(y_pred, 0.95),
    .groups = "drop"
  ) |>
  mutate(forecast_season = nxt$season, forecast_week = nxt$week)
#if (!is.null(save_root)) {
save_forecast_summary(forecast_summary, save_root, nxt$season, nxt$week)
#}
#}
#}
result <- list(
  fit = fit,
  snapshot_summary = snapshot_summary,
  forecast_summary = forecast_summary,
  next_pointer = if (is.null(nxt)) NULL else list(season = nxt$season, week = nxt$week)
)


last_snapshot_summary <- step0$snapshot_summary
# Loop forward with summary-only logic
if (nrow(endpoints) > 1) {
  for (r in 2:nrow(endpoints)) {
    endr <- endpoints[r, ]
    stepr <- sequential_step(
      df, mod, mod_gq,
      start_train = start_train,
      end_train   = list(season = endr$season, week = endr$week),
      prev_snapshot_summary = last_snapshot_summary,
      n_draws_gq  = n_draws_gq,
      save_root   = save_root,
      iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
      adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
    last_snapshot_summary <- stepr$snapshot_summary
  }
}
list(last_snapshot_summary = last_snapshot_summary)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. FULL SEQUENTIAL SSM1 WORKFLOW: Rolling Stan Fits + Forecasts ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

start_train      <- list(season = 2002, week = 1)
end_train_init   <- list(season = 2005, week = 14)
end_train_final  <- list(season = 2005, week = 15)
n_draws_gq      <- 200
save_root       <- "Model Fitting/stan_models"
iter_warmup     <- 250
iter_sampling   <- 500
chains          <- 4
adapt_delta     <- 0.9
max_treedepth   <- 10

mod_ssm1    <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1_gq.stan"))

seq_result <- sequential_run(
  df                = game_fit_data_all,
  mod               = mod_ssm1,
  mod_gq            = mod_ssm1_gq,
  start_train       = start_train,
  end_train_initial = end_train_init,
  end_train_final   = end_train_final,
  n_draws_gq        = n_draws_gq,
  save_root         = save_root,
  iter_warmup       = iter_warmup,
  iter_sampling     = iter_sampling,
  chains            = chains,
  adapt_delta       = adapt_delta,
  max_treedepth     = max_treedepth
)

# ---- Load and Analyze Snapshot and Forecast Summaries ----

snapshot_file <- file.path(save_root, "ssm1_snapshot_summaries.rds")
forecast_file <- file.path(save_root, "ssm1_forecast_summaries.rds")
snapshots <- readRDS(snapshot_file)
forecasts <- readRDS(forecast_file)

# ---- Join with True Outcomes and Analyze ----

forecasts <- forecasts |>
  left_join(
    game_fit_data_all |>
      select(season, week, home_id, away_id, result),
    by = c("forecast_season" = "season", "forecast_week" = "week",
           "home_id", "away_id")
  )

library(Metrics)
oos_summary <- forecasts |>
  group_by(forecast_season, forecast_week) |>
  summarise(
    games = n_distinct(game),
    mu_mean = mean(mu_mean, na.rm = TRUE),
    obs_mean  = mean(result, na.rm = TRUE),
    RMSE      = rmse(result, mu_mean)
  ) |>
  arrange(forecast_season, forecast_week)
print(oos_summary)

# ---- Visualization Example ----

library(ggplot2)
ggplot(forecasts, aes(x = mu_mean)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_vline(aes(xintercept = result), color = "red", linetype = "dashed") +
  facet_wrap(~ forecast_week) +
  labs(title = "OOS Forecast Distribution vs Observed",
       x = "Predicted Margin (mu_mean)",
       y = "Count")

# ---- Save Everything ----

saveRDS(seq_result, file = file.path(save_root, "sequential_fit_full.rds"))

# ---- Resume: Continue from last checkpoint as new data arrives ----

# (See earlier: just reload the summaries and last fit, and run sequential_step
#  with the last snapshot and the next window.)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 5 . COMPARE ssm1 versions ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

start_train      <- list(season = 2002, week = 1)
end_train_init   <- list(season = 2005, week = 14)
end_train_final  <- list(season = 2005, week = 21)
n_draws_gq      <- 200
save_root       <- "Model Fitting/stan_models"
iter_warmup     <- 500
iter_sampling   <- 1000
chains          <- 4
adapt_delta     <- 0.9
max_treedepth   <- 10

mod_ssm1    <- cmdstan_model(file.path(save_root, "ssm1.stan"))
mod_ssm1_gq <- cmdstan_model(file.path(save_root, "ssm1_gq.stan"))

## sequential_run ----
### input ----
df                = game_fit_data_all
mod               = mod_ssm1
mod_gq            = mod_ssm1_gq
start_train       = start_train
end_train_initial = end_train_init
end_train_final   = end_train_final
n_draws_gq        = n_draws_gq
save_root         = save_root
iter_warmup       = iter_warmup
iter_sampling     = iter_sampling
chains            = chains
adapt_delta       = adapt_delta
max_treedepth     = max_treedepth

### function ----
#### build_week_table ----
# week_tbl <- build_week_table(df)
week_tbl <- df |>
  distinct(season, week, season_idx, week_idx) |>
  arrange(season, week)

cat("# -------------- glimpse_build_week_table ------------- #\n")
glimpse_build_week_table <- glimpse(week_tbl)
return(out_build_week_table)



start_row <- week_tbl |> filter(season == start_train$season, week == start_train$week)
end0_row  <- week_tbl |> filter(season == end_train_initial$season, week == end_train_initial$week)
endF_row  <- week_tbl |> filter(season == end_train_final$season, week == end_train_final$week)
stopifnot(nrow(start_row) == 1, nrow(end0_row) == 1, nrow(endF_row) == 1)
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
last_snapshot_summary <- NULL
# First fit at end0 (use default inits)
step0 <- sequential_step(
  df, mod, mod_gq,
  start_train = start_train,
  end_train   = list(season = end_train_initial$season, week = end_train_initial$week),
  prev_snapshot_summary = NULL,
  n_draws_gq = n_draws_gq,
  save_root = save_root,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth
)
last_snapshot_summary <- step0$snapshot_summary
# Loop forward with summary-only logic
if (nrow(endpoints) > 1) {
  for (r in 2:nrow(endpoints)) {
    endr <- endpoints[r, ]
    stepr <- sequential_step(
      df, mod, mod_gq,
      start_train = start_train,
      end_train   = list(season = endr$season, week = endr$week),
      prev_snapshot_summary = last_snapshot_summary,
      n_draws_gq  = n_draws_gq,
      save_root   = save_root,
      iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
      adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
    last_snapshot_summary <- stepr$snapshot_summary
  }
}
list(last_snapshot_summary = last_snapshot_summary)





# get_means <- function(var_prefix, dim = NULL) {
#   vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
#   if (!is.null(dim)) matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) else vals
# }
# stan_data_forecast <- list(
#   league_hfa_z    = get_means("league_hfa_z"),
#   league_hfa_init = get_means("league_hfa_init"),
#   beta_hfa        = get_means("beta_hfa"),
#   sigma_hfa       = abs(get_means("sigma_hfa")),
#   team_hfa_z      = get_means("team_hfa_z"),
#   sigma_team_hfa  = abs(get_means("sigma_team_hfa")),
#   z_start         = get_means("z_start"),
#   z_w             = get_means("z_w"),
#   beta_w          = get_means("beta_w"),
#   sigma_w         = abs(get_means("sigma_w")),
#   beta_s          = get_means("beta_s"),
#   sigma_s         = abs(get_means("sigma_s")),
#   sigma_y         = abs(get_means("sigma_y"))
# )

#league_hfa_z    = get_means("league_hfa_z")
var_prefix = "league_hfa_z"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#league_hfa_init = get_means("league_hfa_init")
var_prefix = "league_hfa_init"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#beta_hfa        = get_means("beta_hfa")
var_prefix = "beta_hfa"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#sigma_hfa       = abs(get_means("sigma_hfa"))
var_prefix = "sigma_hfa"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#team_hfa_z      = get_means("team_hfa_z")
var_prefix = "team_hfa_z"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#sigma_team_hfa  = abs(get_means("sigma_team_hfa"))
var_prefix = "sigma_team_hfa"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#z_start         = get_means("z_start")
var_prefix = "z_start"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#z_w             = get_means("z_w")
var_prefix = "z_w"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#beta_w          = get_means("beta_w")
var_prefix = "beta_w"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#sigma_w         = abs(get_means("sigma_w"))
var_prefix = "sigma_w"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#beta_s          = get_means("beta_s")
var_prefix = "beta_s"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#sigma_s         = abs(get_means("sigma_s"))
var_prefix = "sigma_s"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals

#sigma_y         = abs(get_means("sigma_y"))
var_prefix = "sigma_y"
dim = NULL
vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
vals
#if (!is.null(dim)) 
!is.null(dim)
vals <- matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) 
#else 
vals <- vals




stan_data_forecast <- list(
  league_hfa_z    = get_means("league_hfa_z"),
  league_hfa_init = get_means("league_hfa_init"),
  beta_hfa        = get_means("beta_hfa"),
  sigma_hfa       = abs(get_means("sigma_hfa")),
  team_hfa_z      = get_means("team_hfa_z"),
  sigma_team_hfa  = abs(get_means("sigma_team_hfa")),
  z_start         = get_means("z_start"),
  z_w             = get_means("z_w"),
  beta_w          = get_means("beta_w"),
  sigma_w         = abs(get_means("sigma_w")),
  beta_s          = get_means("beta_s"),
  sigma_s         = abs(get_means("sigma_s")),
  sigma_y         = abs(get_means("sigma_y"))
)
