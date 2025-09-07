


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. HELPERS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# ---- Printing, Timing, Glimpsing ----
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

# ---- Stan Data List Builder ----
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

# ---- Robust Save/Append RDS Utility ----
robust_save_append_rds <- function(obj, file, key_cols = c("season", "week")) {
  # Only keeps the most recent snapshot per season/week
  if (file.exists(file)) {
    prev <- readRDS(file)
    # Remove any duplicate or "future" entries for the same season/week
    prev <- prev[!duplicated(prev[key_cols]), , drop = FALSE]
    prev <- dplyr::bind_rows(prev, obj)
  } else {
    prev <- obj
  }
  saveRDS(prev, file)
  invisible(prev)
}

# ---- Summaries Saving Logic ----
save_snapshot_summary <- function(snapshot_summary, save_root, season, week) {
  file <- file.path(save_root, "ssm1_snapshot_summaries.rds")
  robust_save_append_rds(snapshot_summary, file)
}
save_forecast_summary <- function(forecast_summary, save_root, season, week) {
  file <- file.path(save_root, "ssm1_forecast_summaries.rds")
  robust_save_append_rds(forecast_summary, file)
}
save_fit_checkpoint <- function(fit, save_root) {
  fit$save_object(file = file.path(save_root, "ssm1_last_fit.rds"))
}

# ---- Rolling Inits from Summaries ----
# Utility to check sum-to-zero property (returns TRUE if sums to near zero)
.check_sum_to_zero <- function(mat, tol = 1e-6) {
  if (is.null(mat)) return(TRUE)
  if (is.vector(mat)) return(abs(sum(mat)) < tol)
  apply(mat, 1, function(x) abs(sum(x)) < tol) %>% all()
}

make_summary_inits <- function(snapshot_summary, debug = FALSE) {
  # Extract posterior means for initialization
  get_means <- function(var_prefix, dim = NULL) {
    vals <- snapshot_summary |> filter(str_starts(variable, var_prefix)) |> pull(mean)
    if (!is.null(dim)) matrix(vals, nrow = dim[1], ncol = dim[2], byrow = TRUE) else vals
  }
  # Construct inits (vector or matrix where needed)
  inits <- list(
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
  # Sum-to-zero check
  if (debug) {
    for (nm in c("team_hfa_z", "z_start", "z_w")) {
      if (!.check_sum_to_zero(inits[[nm]])) {
        warning(paste("Init for", nm, "does not sum to zero!"))
      }
    }
  }
  inits
}

# ---- Stan Data Appender for Forecast ----
append_sched_to_stan_data <- function(stan_data, sched) {
  stopifnot(is.data.frame(sched))
  new_n <- nrow(sched)
  cols <- colnames(sched)
  stan_data <- stan_data |>
    purrr::list_modify(!!!setNames(
      purrr::map(cols, ~ c(stan_data[[.x]], sched[[.x]])), 
      cols
    ))
  stan_data$N_games <- length(stan_data[[cols[1]]])
  stan_data$N_oos   <- new_n
  stan_data$oos_idx <- as.array((stan_data$N_obs + 1):(stan_data$N_obs + new_n))
  stan_data
}

# ---- Fit and Forecast Helpers ----
fit_state_space <- function(mod, stan_data, inits = NULL,
                            iter_warmup = 500, iter_sampling = 1000, chains = 4,
                            adapt_delta = 0.9, max_treedepth = 10, sig_figs = 10) {
  mod$sample(
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
}

forecast_ssm1 <- function(mod_gq, stan_data, iter_sampling = 1, chains = 1, seed = 52) {
  mod_gq$sample(
    data = stan_data,
    fixed_param = TRUE,
    iter_sampling = iter_sampling,
    chains = chains,
    seed = seed
  )
}

tidy_gq_output <- function(gq_fit, schedule, add_cols = TRUE) {
  raw <- gq_fit$draws(variables = c("mu_pred","y_pred"), format = "df")
  long <- raw |>
    pivot_longer(
      cols = matches("^(mu_pred|y_pred)\\["),
      names_to = c("var","draw","game"),
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
  long
}

# ---- Main Sequential Step ----
sequential_step <- function(df, mod, mod_gq,
                            start_train, end_train,
                            prev_snapshot_summary = NULL,
                            n_draws_gq = 200,
                            save_root = NULL,
                            iter_warmup = 500, iter_sampling = 1000, chains = 4,
                            adapt_delta = 0.9, max_treedepth = 10) {
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
  week_tbl <- build_week_table(df)
  end_row  <- week_tbl |> filter(season == end_train$season, week == end_train$week)
  last_season_idx <- end_row$season_idx[[1]]
  last_week_idx   <- end_row$week_idx[[1]]
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
  nxt <- next_week_after(week_tbl, end_train$season, end_train$week)
  forecast_summary <- tibble()
  if (!is.null(nxt)) {
    sched <- schedule_for(df, nxt$season, nxt$week)
    if (nrow(sched) > 0) {
      fl <- get_first_last_week(df)
      N_teams   <- length(unique(df$home_id))
      N_seasons <- max(df$season_idx)
      N_weeks   <- max(df$week_idx)
      # For GQ: only last fit's snapshot needed for inits
      stan_data_forecast <- append_sched_to_stan_data(stan_data, sched)
      gq_fit <- forecast_ssm1(
        mod_gq, stan_data_forecast,
        iter_sampling = n_draws_gq, chains = 1, seed = 52
      )
      forecast_tbl <- tidy_gq_output(gq_fit, sched, add_cols = TRUE)
      forecast_summary <- forecast_tbl |>
        group_by(game, season_id, week_id, home_id, away_id, hfa) |>
        summarise(
          mu_mean = mean(mu_pred),
          mu_sd = sd(mu_pred),
          mu_p05 = quantile(mu_pred, 0.05),
          mu_p95 = quantile(mu_pred, 0.95),
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
  list(
    fit = fit,
    snapshot_summary = snapshot_summary,
    forecast_summary = forecast_summary,
    next_pointer = if (is.null(nxt)) NULL else list(season = nxt$season, week = nxt$week)
  )
}

# ---- Sequential Runner ----
sequential_run <- function(df, mod, mod_gq,
                           start_train, end_train_initial, end_train_final,
                           n_draws_gq = 200,
                           save_root = NULL,
                           iter_warmup = 500, iter_sampling = 1000, chains = 4,
                           adapt_delta = 0.9, max_treedepth = 10) {
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
  list(last_snapshot_summary = last_snapshot_summary)
}
