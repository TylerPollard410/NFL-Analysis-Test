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
#library(rstan)
library(cmdstanr)
#library(brms)
library(posterior)
library(bayesplot)
library(Metrics) # for MAE, RMSE
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

# detach("package:nflendzonePipeline",unload = TRUE, force = TRUE)
# install.packages(".", repos = NULL, type = "source")
# pak::pak("TylerPollard410/nflendzone")
library(nflendzonePipeline)
library(nflendzone)

set.seed(52)
options(scipen = 10)

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
game_data_long <- game_data |>
  clean_homeaway(invert = c("result", "spread_line"))

game_id_keys <- game_data |>
  select(
    game_id,
    season,
    game_type,
    season_type,
    week,
    home_team,
    away_team,
    location
  )
game_long_id_keys <- game_data_long |>
  select(
    game_id,
    season,
    game_type,
    season_type,
    week,
    team,
    opponent,
    location
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
game_model_data <- game_data |>
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
    game_id,
    season,
    season_idx,
    week,
    week_idx = week_seq,
    game_type,
    season_type,
    home_team,
    away_team,
    home_id,
    away_id,
    location,
    hfa,
    home_score,
    away_score,
    result,
    spread_line,
    home_spread_odds,
    away_spread_odds,
    home_spread_prob,
    away_spread_prob,
    total,
    total_line,
    over_odds,
    under_odds,
    over_prob,
    under_prob,
    winner,
    home_moneyline,
    away_moneyline,
    home_moneyline_prob,
    away_moneyline_prob
  )

team_fit_data_all <- game_fit_data_all |>
  clean_homeaway(invert = c("result", "spread_line", "hfa"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. HELPERS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## ---- Printing, Timing, Glimpsing ----
.print_function <- function(depth = 3, enabled = TRUE) {
  if (!enabled) {
    return(invisible(NULL))
  }
  calls <- sys.calls()
  stack <- rev(tail(calls, depth + 1))
  fnames <- vapply(
    stack[-1],
    function(call) as.character(call[[1]]),
    character(1)
  )
  fnames <- rev(fnames)
  banner <- paste0(
    "# -------------- ",
    paste(fnames, collapse = " -> "),
    " -------------- #"
  )
  cat("\n", banner, "\n")
  invisible(NULL)
}
.print_time <- function(
  start = TRUE,
  timer = NULL,
  enabled = TRUE,
  msg = NULL
) {
  if (!enabled) {
    return(invisible(NULL))
  }
  pretty_time <- function(secs) {
    h <- floor(secs / 3600)
    m <- floor((secs %% 3600) / 60)
    s <- secs %% 60
    sprintf("%.3f hours %.3f minutes %.3f seconds", h, m, s)
  }
  toc_pretty <- function(tic, toc, msg = NULL, ...) {
    elapsed <- toc - tic
    msg_part <- if (
      !is.null(msg) && !is.na(msg) && length(msg) && nzchar(msg)
    ) {
      paste0(msg, ": ")
    } else {
      ""
    }
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
      msg_part <- if (
        !is.null(timer$msg) &&
          !is.na(timer$msg) &&
          length(timer$msg) &&
          nzchar(timer$msg)
      ) {
        paste0(timer$msg, ": ")
      } else {
        ""
      }
      cat(sprintf("[Time] Elapsed: %s%s\n", msg_part, pretty_time(elapsed)))
      cat("\n")
    }
    invisible(NULL)
  }
}
.glimpse_return <- function(out, enabled = TRUE, max.level = 1) {
  if (!enabled) {
    return(out)
  }
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::glimpse(out)
  } else {
    str(out, max.level = max.level)
  }
  invisible(out)
}


## ---- Stan Data List Builder ----
make_stan_data <- function(
  df,
  start_season,
  start_week,
  end_season,
  end_week,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
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
    fw_season = df |>
      group_by(season_idx) |>
      summarise(min(week_idx)) |>
      pull(),
    lw_season = df |>
      group_by(season_idx) |>
      summarise(max(week_idx)) |>
      pull(),
    hfa = df_sub$hfa,
    result = df_sub$result,
    N_oos = 0,
    oos_idx = array(0, 0)
  )

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

get_first_last_week <- function(
  df,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  fw <- df |>
    group_by(season_idx) |>
    summarise(fw = min(week_idx), .groups = "drop")
  lw <- df |>
    group_by(season_idx) |>
    summarise(lw = max(week_idx), .groups = "drop")

  result <- tibble(season_idx = sort(unique(df$season_idx))) |>
    left_join(fw, by = "season_idx") |>
    left_join(lw, by = "season_idx") |>
    arrange(season_idx)

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

build_week_table <- function(
  df,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  result <- df |>
    distinct(season, week, season_idx, week_idx) |>
    arrange(season, week)

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

next_week_after <- function(
  week_tbl,
  end_season,
  end_week,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  idx <- which(week_tbl$season == end_season & week_tbl$week == end_week)
  if (length(idx) == 0 || idx == nrow(week_tbl)) {
    return(NULL)
  }

  result <- week_tbl[idx + 1, c("season", "week", "season_idx", "week_idx")]

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

schedule_for <- function(
  df,
  season,
  week,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  result <- df |>
    filter(season == !!season, week == !!week) |>
    select(home_id, away_id, week_id = week_idx, season_id = season_idx, hfa)

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

## ---------- Summaries Saving Logic ----------
update_summary_file <- function(
  new_summary,
  file,
  season,
  week,
  key_cols = c("season", "week"),
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  new_summary[[key_cols[1]]] <- season
  new_summary[[key_cols[2]]] <- week
  if (file.exists(file)) {
    prev <- readRDS(file)
    before_idx <- which(
      prev[[key_cols[1]]] < season |
        (prev[[key_cols[1]]] == season & prev[[key_cols[2]]] < week)
    )
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

save_fit_checkpoint <- function(
  fit,
  save_root,
  debug_fun = TRUE,
  debug_time = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  fit$save_object(file = file.path(save_root, "ssm1_last_fit.rds"))

  .print_time(start = FALSE, timer, enabled = debug_time)
}

save_snapshot_summary <- function(
  snapshot_summary,
  save_root,
  season,
  week,
  debug_fun = TRUE,
  debug_time = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  file <- file.path(save_root, "ssm1_snapshot_summaries.rds")
  update_summary_file(snapshot_summary, file, season, week)

  .print_time(start = FALSE, timer, enabled = debug_time)
}

save_forecast_summary <- function(
  forecast_summary,
  save_root,
  season,
  week,
  debug_fun = TRUE,
  debug_time = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  file <- file.path(save_root, "ssm1_forecast_summaries.rds")
  update_summary_file(forecast_summary, file, season, week)

  .print_time(start = FALSE, timer, enabled = debug_time)
}

## ---------- Rolling Inits from Summaries ----------
# Utility to check sum-to-zero property (returns TRUE if sums to near zero)
.check_sum_to_zero <- function(mat, tol = 1e-9) {
  if (is.null(mat)) {
    return(TRUE)
  }
  if (is.vector(mat)) {
    return(abs(sum(mat)) < tol)
  }
  apply(mat, 1, function(x) abs(sum(x)) < tol) %>% all()
}

# make_summary_inits_OLD <- function(
#     prev_fit,
#     snapshot_summary,
#     debug_fun = TRUE,
#     debug_time = TRUE,
#     debug_glimpse = TRUE
# ) {
#   .print_function(enabled = debug_fun)
#   timer <- .print_time(start = TRUE, enabled = debug_time)
#
#   get_means <- function(var_prefix, dim = NULL) {
#     vals <- snapshot_summary |>
#       filter(str_starts(variable, var_prefix)) |>
#       pull(mean)
#     if (!is.null(dim)) {
#       matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE)
#     } else {
#       vals
#     }
#   }
#
#   result <- list(
#     league_hfa_raw = get_means("league_hfa_raw"),
#     #league_hfa_init = get_means("league_hfa_init"),
#     beta_league_hfa = get_means("beta_league_hfa"),
#     sigma_league_hfa = abs(get_means("sigma_league_hfa")),
#     z_team_hfa = get_means("z_team_hfa"),
#     sigma_team_hfa = abs(get_means("sigma_team_hfa")),
#     z_s = get_means("z_s"),
#     z_w = get_means("z_w"),
#     beta_w = get_means("beta_w"),
#     sigma_w = abs(get_means("sigma_w")),
#     beta_s = get_means("beta_s"),
#     sigma_s = abs(get_means("sigma_s")),
#     sigma_y = abs(get_means("sigma_y"))
#   )
#
#   # Sum-to-zero check
#   for (nm in c("z_team_hfa", "z_s", "z_w")) {
#     if (!.check_sum_to_zero(result[[nm]])) {
#       warning(paste("Init for", nm, "does not sum to zero!"))
#     }
#   }
#
#   .print_time(start = FALSE, timer, enabled = debug_time)
#   return(.glimpse_return(result, enabled = debug_glimpse))
# }

make_summary_inits <- function(
  prev_fit,
  snapshot_summary,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  var_skel <- prev_fit$variable_skeleton(
    transformed_parameters = FALSE,
    generated_quantities = FALSE
  )

  # Get the list of variable names
  param_names <- names(var_skel)

  # Function to get posterior mean and reshape to match skeleton
  get_mean_param <- function(var, skel, fit) {
    # Get draws for this var in df format
    df <- fit$draws(variables = var, format = "df")
    # Remove .draw, .chain, .iteration cols
    draws <- suppressWarnings(
      df[, setdiff(names(df), c(".draw", ".chain", ".iteration")), drop = FALSE]
    )
    # Get posterior means
    means <- colMeans(draws)
    # If scalar, just return it as is
    dims <- dim(skel)
    if (length(dims) < 2) {
      return(as.numeric(means))
    }
    # Otherwise, reshape into array with dims matching skel
    array(as.numeric(means), dim = dims)
  }

  # Now apply this to all vars in var_skel
  result <- map2(param_names, var_skel, ~ get_mean_param(.x, .y, prev_fit)) |>
    set_names(names(var_skel))

  # Sum-to-zero check
  for (nm in c("z_team_hfa", "z_s", "z_w")) {
    if (!.check_sum_to_zero(result[[nm]])) {
      warning(paste("Init for", nm, "does not sum to zero!"))
    }
  }

  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

## ---- Stan Data Appender for Forecast ----
append_sched_to_stan_data <- function(
  stan_data,
  sched,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  stopifnot(is.data.frame(sched))
  new_n <- nrow(sched)
  cols <- colnames(sched)

  # Use list_modify and set_names to update only sched columns
  stan_data <- stan_data |>
    purrr::list_modify(
      !!!setNames(
        purrr::map(cols, ~ c(stan_data[[.x]], sched[[.x]])),
        cols
      )
    )

  stan_data$N_games <- length(stan_data[[cols[1]]])
  stan_data$N_oos <- new_n
  stan_data$oos_idx <- as.array((stan_data$N_obs + 1):(stan_data$N_obs + new_n))

  result <- stan_data
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

make_stan_data_forecast <- function(
  snapshot_summary,
  schedule,
  n_draws,
  fw_season,
  lw_season,
  N_teams,
  N_seasons,
  N_weeks,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  mean_by_var <- function(v) {
    snapshot_summary |> filter(variable == v) |> pull(mean)
  }

  result <- list(
    N_draws = n_draws,
    N_teams = N_teams,
    N_seasons = N_seasons,
    N_weeks = N_weeks,
    current_season = mean_by_var("last_season_idx"),
    current_week = mean_by_var("last_week_idx"),
    fw_season = fw_season,
    lw_season = lw_season,
    beta_w = rep(mean_by_var("beta_w"), n_draws),
    sigma_w = rep(mean_by_var("sigma_w"), n_draws),
    beta_s = rep(mean_by_var("beta_s"), n_draws),
    sigma_s = rep(mean_by_var("sigma_s"), n_draws),
    beta_league_hfa = rep(mean_by_var("beta_league_hfa"), n_draws),
    sigma_league_hfa = rep(mean_by_var("sigma_league_hfa"), n_draws),
    sigma_team_hfa = rep(mean_by_var("sigma_team_hfa"), n_draws),
    sigma_y = rep(mean_by_var("sigma_y"), n_draws),
    team_strength_cur = matrix(
      snapshot_summary |>
        filter(str_starts(variable, "team_strength_last")) |>
        pull(mean),
      nrow = n_draws,
      ncol = N_teams,
      byrow = TRUE
    ),
    team_hfa_cur = matrix(
      snapshot_summary |>
        filter(str_starts(variable, "team_hfa_last")) |>
        pull(mean),
      nrow = n_draws,
      ncol = N_teams,
      byrow = TRUE
    ),
    league_hfa_cur = rep(mean_by_var("league_hfa_last"), n_draws),
    N_oos = nrow(schedule),
    home_id = schedule$home_id,
    away_id = schedule$away_id,
    week_id = schedule$week_id,
    season_id = schedule$season_id,
    hfa = schedule$hfa
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

## ---------- Fit and Forecast Helpers ----------
fit_state_space <- function(
  mod,
  stan_data,
  inits = NULL,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  sig_figs = 10,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = FALSE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  result <- mod$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    seed = 52,
    init = if (!is.null(inits)) {
      inits <- replicate(chains, inits, simplify = FALSE)
    } else {
      0
    },
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    sig_figs = sig_figs
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

forecast_ssm1 <- function(
  mod_gq,
  stan_data,
  iter_sampling = 1,
  chains = 1,
  seed = 52,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = FALSE
) {
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


tidy_gq_output <- function(
  gq_fit,
  schedule,
  add_cols = TRUE,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  raw <- gq_fit$draws(variables = c("mu_pred", "y_pred"), format = "df")
  long <- raw |>
    pivot_longer(
      cols = matches("^(mu_pred|y_pred)\\["),
      names_to = c("var", "draw", "game"),
      names_pattern = "^(mu_pred|y_pred)\\[(\\d+),(\\d+)\\]$",
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

## ---------- Main Sequential Step (Summary-Only) ----------
sequential_step <- function(
  df,
  mod,
  mod_gq,
  start_train,
  end_train,
  prev_fit,
  prev_snapshot_summary = NULL,
  n_draws_gq = 200,
  save_root = NULL,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = FALSE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  stan_data <- make_stan_data(
    df,
    start_train$season,
    start_train$week,
    end_train$season,
    end_train$week
  )
  inits <- if (!is.null(prev_snapshot_summary)) {
    make_summary_inits(prev_fit)
  } else {
    0
  }

  fit <- fit_state_space(
    mod,
    stan_data,
    inits = inits,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )
  # Bookkeeping
  week_tbl <- build_week_table(df)
  end_row <- week_tbl |>
    filter(season == end_train$season, week == end_train$week)
  last_season_idx <- end_row$season_idx[[1]]
  last_week_idx <- end_row$week_idx[[1]]
  # Snapshot summary
  snapshot_vars <- c(
    "team_strength_last",
    "team_hfa_last",
    "league_hfa_last",
    "beta_league_hfa",
    "sigma_league_hfa",
    "sigma_team_hfa",
    "beta_w",
    "sigma_w",
    "beta_s",
    "sigma_s",
    "sigma_y",
    "league_hfa_raw",
    "z_team_hfa",
    "z_s",
    "z_w"
  )
  snapshot_summary <- fit$summary(variables = snapshot_vars)
  snapshot_summary$last_season_idx <- last_season_idx
  snapshot_summary$last_week_idx <- last_week_idx
  snapshot_summary$season <- end_train$season
  snapshot_summary$week <- end_train$week
  if (!is.null(save_root)) {
    save_snapshot_summary(
      snapshot_summary,
      save_root,
      end_train$season,
      end_train$week
    )
    save_fit_checkpoint(fit, save_root)
  }
  # Forecast the very next week (summary only)
  nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
  forecast_summary <- tibble()
  if (!is.null(nxt)) {
    sched <- schedule_for(df, nxt$season, nxt$week)
    if (nrow(sched) > 0) {
      fl <- get_first_last_week(df)
      N_teams <- length(unique(df$home_id))
      N_seasons <- max(df$season_idx)
      N_weeks <- max(df$week_idx)
      gq_fit <- forecast_ssm1(
        mod_gq,
        snapshot,
        sched,
        n_draws = n_draws_gq,
        fw_season = fl$fw,
        lw_season = fl$lw,
        N_teams = N_teams,
        N_seasons = N_seasons,
        N_weeks = N_weeks
      )
      # forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE) |>
      #   mutate(forecast_season = nxt$season, forecast_week = nxt$week)
      # stan_data_forecast <- make_stan_data_forecast(
      #   snapshot_summary = snapshot_summary,
      #   schedule = sched,
      #   n_draws = n_draws_gq,
      #   fw_season = fl$fw,
      #   lw_season  = fl$lw,
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
    next_pointer = if (is.null(nxt)) {
      NULL
    } else {
      list(season = nxt$season, week = nxt$week)
    }
  )
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

## ---------- Sequential Runner (Summary-Only) ----------
sequential_run <- function(
  df,
  mod,
  mod_gq,
  start_train,
  end_train_initial,
  end_train_final,
  n_draws_gq = 200,
  save_root = NULL,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  debug_fun = TRUE,
  debug_time = TRUE,
  debug_glimpse = TRUE
) {
  .print_function(enabled = debug_fun)
  timer <- .print_time(start = TRUE, enabled = debug_time)

  week_tbl <- build_week_table(df)
  start_row <- week_tbl |>
    filter(season == start_train$season, week == start_train$week)
  end0_row <- week_tbl |>
    filter(season == end_train_initial$season, week == end_train_initial$week)
  endF_row <- week_tbl |>
    filter(season == end_train_final$season, week == end_train_final$week)
  stopifnot(nrow(start_row) == 1, nrow(end0_row) == 1, nrow(endF_row) == 1)
  endpoints <- week_tbl |>
    filter(
      (season > end_train_initial$season) |
        (season == end_train_initial$season & week >= end_train_initial$week)
    ) |>
    filter(
      (season < end_train_final$season) |
        (season == end_train_final$season & week <= end_train_final$week)
    ) |>
    arrange(season, week)
  last_snapshot_summary <- NULL
  # First fit at end0 (use default inits)
  step0 <- sequential_step(
    df,
    mod,
    mod_gq,
    start_train = start_train,
    end_train = list(
      season = end_train_initial$season,
      week = end_train_initial$week
    ),
    prev_snapshot_summary = NULL,
    n_draws_gq = n_draws_gq,
    save_root = save_root,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )
  last_snapshot_summary <- step0$snapshot_summary
  # Loop forward with summary-only logic
  if (nrow(endpoints) > 1) {
    for (r in 2:nrow(endpoints)) {
      endr <- endpoints[r, ]
      stepr <- sequential_step(
        df,
        mod,
        mod_gq,
        start_train = start_train,
        end_train = list(season = endr$season, week = endr$week),
        prev_snapshot_summary = last_snapshot_summary,
        n_draws_gq = n_draws_gq,
        save_root = save_root,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        chains = chains,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth
      )
      last_snapshot_summary <- stepr$snapshot_summary
    }
  }
  result <- list(last_snapshot_summary = last_snapshot_summary)
  .print_time(start = FALSE, timer, enabled = debug_time)
  return(.glimpse_return(result, enabled = debug_glimpse))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MAIN SEQUENTIAL FITTING AND FORECASTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## ---- Set file locations for Stan models ----
save_root <- "Model Fitting/ssm_check"
loc_ssm1 <- "Model Fitting/ssm_check/ssm1.stan"
loc_ssm1_gq <- "Model Fitting/ssm_check/ssm1_gq.stan"
dir.create(save_root, showWarnings = FALSE, recursive = TRUE)

## ---- Compile Stan models (do this ONCE; force_recompile only if Stan code changes) ----
mod_ssm1 <- cmdstan_model(
  loc_ssm1,
  compile_model_methods = TRUE,
  force_recompile = TRUE,
  #dry_run = TRUE,
  #stanc_options = list("Oexperimental"),
  pedantic = TRUE
)
mod_ssm1$code()

mod_ssm1_gq <- cmdstan_model(
  loc_ssm1_gq,
  compile_model_methods = TRUE,
  force_recompile = TRUE,
  #dry_run = TRUE,
  #stanc_options = list("Oexperimental"),
  pedantic = TRUE
)
mod_ssm1_gq$code()

## ---- Set up training schedule ----
df <- game_fit_data_all

# Initial train: 2002 wk1 to 2005 wk21
start_train <- list(season = 2002, week = 1)
end_train_init <- list(season = 2005, week = 21)

# Find starting index for sequential run (first week to forecast: 2006 wk1)
week_tbl <- build_week_table(df)
n_steps <- 25

# Sequential endpoints: Fit through 2006 wk1, then 2006 wk2, ..., 25 total steps
initial_idx <- which(week_tbl$season == 2006 & week_tbl$week == 1)
endpoints <- week_tbl[initial_idx:(initial_idx + n_steps - 1), ]

stan_data <- make_stan_data(
  df,
  start_train$season,
  start_train$week,
  end_train_init$season,
  end_train_init$week
)

## ---- Warmup fit (init = 0) ----
cat("\n--- WARMUP FIT: 2002 wk1 to 2005 wk21 ---\n")
fit0 <- fit_state_space(
  mod_ssm1,
  stan_data = stan_data,
  inits = 0,
  iter_warmup = 1000, # Larger for first fit
  iter_sampling = 1000,
  chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  sig_figs = 10
)
# Extract initial snapshot summary for rolling inits
snapshot_vars <- c(
  "team_strength_last",
  "team_hfa_last",
  "league_hfa_last",
  "beta_league_hfa",
  "sigma_league_hfa",
  "sigma_team_hfa",
  "beta_w",
  "sigma_w",
  "beta_s",
  "sigma_s",
  "sigma_y",
  "league_hfa_raw",
  "z_team_hfa",
  "z_s",
  "z_w"
)
snapshot_summary0 <- fit0$summary(variables = snapshot_vars)
snapshot_summary0$last_season_idx <- week_tbl$season_idx[initial_idx - 1]
snapshot_summary0$last_week_idx <- week_tbl$week_idx[initial_idx - 1]
snapshot_summary0$season <- end_train_init$season
snapshot_summary0$week <- end_train_init$week

## ---- Initialize storage ----
rolling_snapshot_summaries <- list()
rolling_forecast_summaries <- list()
last_snapshot_summary <- snapshot_summary0
prev_fit <- fit0

## ---- Sequential weekly rolling fits ----
for (step in seq_len(n_steps)) {
  endr <- endpoints[step, ]
  cat(glue::glue(
    "\n--- SEQUENTIAL FIT {step}: Through {endr$season} wk{endr$week} ---\n"
  ))
  out <- sequential_step(
    df,
    mod_ssm1,
    mod_ssm1_gq,
    start_train = start_train,
    end_train = list(season = endr$season, week = endr$week),
    prev_fit = prev_fit,
    prev_snapshot_summary = last_snapshot_summary,
    n_draws_gq = 100, # Number of GQ draws per week
    save_root = save_root,
    iter_warmup = 250, # Smaller after warmup
    iter_sampling = 500,
    chains = 2,
    adapt_delta = 0.9,
    max_treedepth = 10
  )
  # Keep only the most recent summaries to save memory
  rolling_snapshot_summaries[[step]] <- out$snapshot_summary
  rolling_forecast_summaries[[step]] <- out$forecast_summary
  last_snapshot_summary <- out$snapshot_summary
  prev_fit <- out$fit
}

## ---- Save final snapshot and forecast summaries as .rds ----
saveRDS(
  rolling_snapshot_summaries,
  file.path(save_root, "rolling_snapshot_summaries.rds")
)
saveRDS(
  rolling_forecast_summaries,
  file.path(save_root, "rolling_forecast_summaries.rds")
)

cat("\n==== Sequential Fitting Complete ====\n")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. SINGLE FITTING AND FORECASTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## ---- Set file locations for Stan models ----
save_root <- "Model Fitting/ssm_check"
loc_ssm1 <- "Model Fitting/ssm_check/ssm1.stan"
loc_ssm1_gq <- "Model Fitting/ssm_check/ssm1_gq.stan"
dir.create(save_root, showWarnings = FALSE, recursive = TRUE)

## ---- Compile Stan models (do this ONCE; force_recompile only if Stan code changes) ----
mod_ssm1 <- cmdstan_model(
  loc_ssm1,
  compile_model_methods = TRUE,
  force_recompile = TRUE,
  #dry_run = TRUE,
  #stanc_options = list("Oexperimental"),
  pedantic = TRUE
)
mod_ssm1$code()

mod_ssm1_gq <- cmdstan_model(
  loc_ssm1_gq,
  compile_model_methods = TRUE,
  force_recompile = TRUE,
  #dry_run = TRUE,
  #stanc_options = list("Oexperimental"),
  pedantic = TRUE
)
mod_ssm1_gq$code()

## ---- Set up training schedule ----
df <- game_fit_data_all

## week_tbl ----
week_tbl <- build_week_table(df)

## idx ----
teams <- sort(unique(c(df$home_team, df$away_team)))
seasons <- sort(unique(df$season))
weeks <- sort(unique(df$week_idx))

team_idx <- setNames(1:32, teams)
season_idx <- setNames(unique(week_tbl$season_idx), unique(week_tbl$season))
week_idx <- setNames(
  week_tbl$week_idx,
  paste0("[", week_tbl$season_idx, ",", week_tbl$week, "]")
)

N_teams <- length(unique(df$home_id))
N_seasons <- max(df$season_idx)
N_weeks <- max(df$week_idx)

# Initial train: 2002 wk1 to 2005 wk21
start_train <- list(season = 2002, week = 1)
end_train_init <- list(season = 2024, week = 22)

# Find starting index for sequential run (first week to forecast: 2006 wk1)
week_tbl <- build_week_table(df)
n_steps <- 25

# Sequential endpoints: Fit through 2006 wk1, then 2006 wk2, ..., 25 total steps
initial_idx <- which(week_tbl$season == 2006 & week_tbl$week == 1)
endpoints <- week_tbl[initial_idx:(initial_idx + n_steps - 1), ]

stan_data <- make_stan_data(
  df,
  start_train$season,
  start_train$week,
  end_train_init$season,
  end_train_init$week
)


## ---- Warmup fit (init = 0) ----
iter_warm = 250
iter_samp = 500
chains = 4
adapt_delta = 0.9
max_treedepth = 10
sig_figs = 10
debug_fun = TRUE
debug_time = TRUE
debug_glimpse = FALSE

cat("\n--- WARMUP FIT: 2002 wk1 to 2005 wk21 ---\n")
fit0 <- fit_state_space(
  mod_ssm1,
  stan_data = stan_data,
  inits = NULL,
  iter_warmup = iter_warm, # Larger for first fit
  iter_sampling = iter_samp,
  chains = chains,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  sig_figs = sig_figs
)

fit0_vars <- mod_ssm1$variables()
snap_vars <- names(fit0_vars$generated_quantities)

snapshot_draws2 <- fit0 |>
  spread_draws(
    c(team_strength_last, team_hfa_last)[team]
  ) |>
  mutate(
    team = teams[team]
  )
snapshot_sum2 <- snapshot_draws2 |>
  summarise_draws()

next_week <- next_week_after(
  week_tbl,
  end_train_init$season,
  end_train_init$week
)
next_sched <- schedule_for(df, next_week$season, next_week$week)
stan_data_forecast <- append_sched_to_stan_data(stan_data, next_sched)

gq0 <- mod_ssm1_gq$generate_quantities(
  fit0,
  data = stan_data_forecast,
  seed = 52,
  sig_figs = 4,
  parallel_chains = min(4, parallel::detectCores())
)

forecast_draws2 <- gq0 |>
  spread_draws(
    c(
      team_strength_home_pred,
      team_strength_away_pred,
      team_hfa_pred,
      mu_pred,
      y_pred
    )[oos_idx]
  ) |>
  select(
    oos_idx,
    team_strength_home_pred,
    team_strength_away_pred,
    team_hfa_pred,
    mu_pred,
    y_pred,
    everything()
  )

forecast_sum2 <- forecast_draws2 |>
  summarise_draws()

mu_preds_sum2 <- forecast_draws2 |>
  point_interval(mu_pred)

pred_df2 <- df |>
  inner_join(
    next_sched |> bind_cols(mu_preds_sum2),
    by = c(
      "season_idx" = "season_id",
      "week_idx" = "week_id",
      "home_id",
      "away_id",
      "hfa"
    )
  ) |>
  relocate(mu_pred, .lower, .upper, .after = spread_line)

save(
  snapshot_sum,
  forecast_sum,
  pred_df,
  file = file.path(save_root, "single_fit_results.RData")
)


inits0 <- make_summary_inits(fit0)

start_train <- list(season = 2002, week = 1)
end_train_init <- list(season = 2025, week = 1)

stan_data <- make_stan_data(
  df,
  start_train$season,
  start_train$week,
  end_train_init$season,
  end_train_init$week
)

fit1 <- mod_ssm1$sample(
  data = stan_data,
  iter_warmup = iter_warm,
  iter_sampling = iter_samp,
  chains = chains,
  parallel_chains = min(chains, parallel::detectCores()),
  seed = 52,
  init = fit0,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  sig_figs = sig_figs
)

snapshot_draws21 <- fit1 |>
  spread_draws(
    c(team_strength_last, team_hfa_last)[team]
  ) |>
  mutate(
    team = teams[team]
  )
snapshot_sum21 <- snapshot_draws21 |>
  summarise_draws()

next_week <- next_week_after(
  week_tbl,
  end_train_init$season,
  end_train_init$week
)
next_sched <- schedule_for(df, next_week$season, next_week$week)
stan_data_forecast <- append_sched_to_stan_data(stan_data, next_sched)

gq1 <- mod_ssm1_gq$generate_quantities(
  fit1,
  data = stan_data_forecast,
  seed = 52,
  sig_figs = 4,
  parallel_chains = min(4, parallel::detectCores())
)

forecast_draws21 <- gq1 |>
  spread_draws(
    c(
      team_strength_home_pred,
      team_strength_away_pred,
      team_hfa_pred,
      mu_pred,
      y_pred
    )[oos_idx]
  ) |>
  select(
    oos_idx,
    team_strength_home_pred,
    team_strength_away_pred,
    team_hfa_pred,
    mu_pred,
    y_pred,
    everything()
  )

forecast_sum21 <- forecast_draws21 |>
  summarise_draws()

mu_preds_sum21 <- forecast_draws21 |>
  point_interval(mu_pred)

pred_df21 <- df |>
  inner_join(
    next_sched |> bind_cols(mu_preds_sum21),
    by = c(
      "season_idx" = "season_id",
      "week_idx" = "week_id",
      "home_id",
      "away_id",
      "hfa"
    )
  ) |>
  relocate(mu_pred, .lower, .upper, .after = spread_line)
