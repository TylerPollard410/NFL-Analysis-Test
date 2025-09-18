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
      cat(sprintf(
        "[Time] Elapsed: %s%s\n",
        msg_part,
        pretty_time(elapsed)
      ))
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

#' Get Season Boundaries from Game Data
#'
#' @description
#' Helper function to determine the actual first and last weeks for any season,
#' using the fw_season_idx and lw_season_idx indicators from the data.
#'
#' @param data Game data (data frame) OR Stan data list from create_stan_data
#' @param season_year Specific season to get boundaries for (optional)
#' @return List with season boundaries information
get_season_boundaries <- function(data, season_year = NULL) {
  # Handle Stan data list input
  if (is.list(data) && "season_idx" %in% names(data)) {
    # Convert Stan data back to data frame format
    game_data_df <- tibble(
      season_idx = data$season_idx,
      week_idx = data$week_idx,
      fw_season_idx = data$fw_season_idx,
      lw_season_idx = data$lw_season_idx
    )

    # Create season lookup (assuming season_idx maps to consecutive seasons starting from min season)
    unique_seasons <- sort(unique(game_data_df$season_idx))
    # We need to reconstruct actual seasons - this assumes data starts from known seasons
    # For now, use season_idx as proxy, but ideally we'd store season info in Stan data
    game_data_df <- game_data_df |>
      mutate(
        season = season_idx + min(all_seasons) - 1, # Map back to actual years
        week = week_idx # Assuming week_idx is actual week number
      )

    data <- game_data_df
  }

  if (!is.null(season_year)) {
    season_data <- data |> filter(season == season_year)

    first_week <- season_data |>
      filter(fw_season_idx == 1) |>
      pull(week) |>
      min()

    last_week <- season_data |>
      filter(lw_season_idx == 1) |>
      pull(week) |>
      max()

    return(list(
      season = season_year,
      first_week = first_week,
      last_week = last_week,
      total_weeks = last_week - first_week + 1
    ))
  }

  # Get boundaries for all seasons
  boundaries <- data |>
    group_by(season) |>
    summarise(
      first_week = min(week[fw_season_idx == 1], na.rm = TRUE),
      last_week = max(week[lw_season_idx == 1], na.rm = TRUE),
      total_weeks = last_week - first_week + 1,
      .groups = "drop"
    )

  return(boundaries)
}

#' Simple Season Filter
#'
#' @description
#' Clean, simple filtering for seasons. Much easier to understand than the
#' previous complex logic.
#'
#' @param data Game data (data frame) OR Stan data list from create_stan_data
#' @param seasons Vector of seasons to include, OR
#' @param before_season Season cutoff (keep seasons < this), OR
#' @param after_season Season cutoff (keep seasons >= this)
#' @param min_week Minimum week to include (optional)
#' @param max_week Maximum week to include (optional)
#' @param verbose Print filtering info
#'
#' @examples
#' # Training data (seasons before 2006)
#' train_data <- filter_seasons(game_data, before_season = 2006)
#'
#' # Specific seasons
#' test_data <- filter_seasons(game_data, seasons = c(2020, 2021))
#'
#' # Season with week limits
#' regular_season <- filter_seasons(game_data, seasons = 2023, max_week = 18)
filter_seasons <- function(
  data,
  seasons = NULL,
  before_season = NULL,
  after_season = NULL,
  min_week = NULL,
  max_week = NULL,
  verbose = FALSE
) {
  # Handle Stan data list input - only trigger if this is actually a Stan data list
  if (is.list(data) && "season_idx" %in% names(data) && !is.data.frame(data)) {
    # Convert Stan data back to data frame format for filtering
    game_data_df <- tibble(
      season_idx = data$season_idx,
      week_idx = data$week_idx,
      fw_season_idx = data$fw_season_idx,
      lw_season_idx = data$lw_season_idx
    )

    # Map season_idx back to actual seasons
    game_data_df <- game_data_df |>
      mutate(
        season = season_idx + min(all_seasons) - 1,
        week = week_idx
      )

    data <- game_data_df
  }

  if (verbose) {
    cat("Filtering game data...\n")
    cat("  Starting with", nrow(data), "games\n")
  }

  filtered <- data

  # Season filtering (mutually exclusive)
  if (!is.null(seasons)) {
    filtered <- filtered |> filter(season %in% seasons)
    if (verbose) {
      cat("  Kept seasons:", paste(seasons, collapse = ", "), "\n")
    }
  } else if (!is.null(before_season)) {
    filtered <- filtered |> filter(season < before_season)
    if (verbose) cat("  Kept seasons before", before_season, "\n")
  } else if (!is.null(after_season)) {
    filtered <- filtered |> filter(season >= after_season)
    if (verbose) cat("  Kept seasons from", after_season, "onward\n")
  }

  # Week filtering
  if (!is.null(min_week)) {
    filtered <- filtered |> filter(week >= min_week)
    if (verbose) cat("  Kept weeks >=", min_week, "\n")
  }
  if (!is.null(max_week)) {
    filtered <- filtered |> filter(week <= max_week)
    if (verbose) cat("  Kept weeks <=", max_week, "\n")
  }

  if (verbose) {
    cat("  Final result:", nrow(filtered), "games\n")
    cat(
      "  Seasons:",
      paste(sort(unique(filtered$season)), collapse = ", "),
      "\n"
    )
    cat("  Week range:", min(filtered$week), "to", max(filtered$week), "\n")
  }

  return(filtered)
}

#' Extend Data for Forecasting
#'
#' @description
#' Intelligently extends existing game data for forecasting by detecting where
#' the current data ends and adding the requested number of forecast weeks.
#' Now uses actual season boundaries instead of hard-coding week 22.
#'
#' @param base_data Base game data (data frame) OR Stan data list from create_stan_data
#' @param forecast_weeks Number of weeks to forecast ahead
#' @param verbose Print extension details
#'
#' @examples
#' # Extend current data by 4 weeks for forecasting
#' extended_data <- extend_for_forecast(game_data, forecast_weeks = 4, verbose = TRUE)
extend_for_forecast <- function(base_data, forecast_weeks, verbose = FALSE) {
  # Handle Stan data list input
  if (is.list(base_data) && "season_idx" %in% names(base_data)) {
    # Convert Stan data back to data frame format
    game_data_df <- tibble(
      season_idx = base_data$season_idx,
      week_idx = base_data$week_idx,
      fw_season_idx = base_data$fw_season_idx,
      lw_season_idx = base_data$lw_season_idx
    )

    # Map season_idx back to actual seasons
    game_data_df <- game_data_df |>
      mutate(
        season = season_idx + min(all_seasons) - 1,
        week = week_idx
      )

    # Need to reload full game data for forecasting
    full_game_data <- load_game_data(seasons = all_seasons) |>
      mutate(
        game_idx = row_number(),
        season_idx = as.integer(as.factor(season)),
        week_idx = week_seq,
        fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
        lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
        home_idx = match(home_team, teams),
        away_idx = match(away_team, teams),
        hfa = as.integer(ifelse(location == "Home", 1, 0))
      )

    base_data <- full_game_data

    # Use the converted data frame to find max season/week
    max_season <- max(game_data_df$season)
    max_week <- max(
      game_data_df |> filter(season == max_season) |> pull(week)
    )
  } else {
    # Original data frame logic
    max_season <- max(base_data$season)
    current_season_data <- base_data |> filter(season == max_season)
    max_week <- max(current_season_data$week)
  }

  # Get season boundaries for current and potentially next season
  current_boundaries <- get_season_boundaries(base_data, max_season)
  season_last_week <- current_boundaries$last_week

  if (verbose) {
    cat("Current data ends at: Season", max_season, "Week", max_week, "\n")
    cat("Season", max_season, "ends at week", season_last_week, "\n")
    cat("Extending by", forecast_weeks, "weeks...\n")
  }

  # Calculate forecast end point
  forecast_end_week <- max_week + forecast_weeks

  # Simple case: forecast stays within current season
  if (forecast_end_week <= season_last_week) {
    if (verbose) {
      cat(
        "Forecast stays within season",
        max_season,
        "(weeks",
        max_week + 1,
        "to",
        forecast_end_week,
        ")\n"
      )
    }

    return(filter_seasons(
      base_data,
      seasons = max_season,
      min_week = max_week + 1,
      max_week = forecast_end_week,
      verbose = verbose
    ))
  }

  # Complex case: forecast spans into next season
  weeks_left_in_season <- season_last_week - max_week
  weeks_in_next_season <- forecast_weeks - weeks_left_in_season

  if (verbose) {
    cat("Forecast spans seasons:\n")
    cat(
      "  Season",
      max_season,
      ": weeks",
      max_week + 1,
      "to",
      season_last_week,
      "(",
      weeks_left_in_season,
      "weeks)\n"
    )
    cat(
      "  Season",
      max_season + 1,
      ": weeks 1 to",
      weeks_in_next_season,
      "(",
      weeks_in_next_season,
      "weeks)\n"
    )
  }

  # Get data from both seasons
  current_season_forecast <- filter_seasons(
    base_data,
    seasons = max_season,
    min_week = max_week + 1,
    max_week = season_last_week
  )

  next_season_forecast <- filter_seasons(
    base_data,
    seasons = max_season + 1,
    max_week = weeks_in_next_season
  )

  # Combine and return
  bind_rows(current_season_forecast, next_season_forecast)
}

#' Create Stan Data - Simplified and Robust
#'
#' @description
#' Much simpler version that's easier to use and understand. Handles the most
#' common use cases with clear, straightforward parameters.
#'
#' @param seasons Seasons to include (default: all available seasons)
#' @param before_season Keep seasons before this cutoff (for training data)
#' @param after_season Keep seasons from this point onward (for test data)
#' @param specific_seasons Vector of specific seasons to include
#' @param min_week Minimum week to include (optional)
#' @param max_week Maximum week to include (optional)
#' @param forecast_weeks If provided, extend data by this many weeks
#' @param exclude_vars Variables to exclude from stan data (optional)
#' @param verbose Print detailed progress information
#'
#' @return Stan data list ready for cmdstanr
#'
#' @examples
#' # Original training data (reproduce fit_stan_data)
#' train_data <- create_stan_data(before_season = 2006)
#'
#' # Test data for specific seasons
#' test_data <- create_stan_data(specific_seasons = c(2020, 2021))
#'
#' # Regular season only
#' regular_data <- create_stan_data(specific_seasons = 2023, max_week = 18)
#'
#' # Extend for forecasting
#' forecast_data <- create_stan_data(
#'   specific_seasons = 2023,
#'   forecast_weeks = 4,
#'   verbose = TRUE
#' )
create_stan_data <- function(
  seasons = all_seasons,
  before_season = NULL,
  after_season = NULL,
  specific_seasons = NULL,
  min_week = NULL,
  max_week = NULL,
  forecast_weeks = NULL,
  exclude_vars = NULL,
  verbose = FALSE
) {
  if (verbose) {
    cat("Creating Stan data...\n")
  }

  # Validate inputs
  filter_count <- sum(
    !is.null(before_season),
    !is.null(after_season),
    !is.null(specific_seasons)
  )
  if (filter_count > 1) {
    stop(
      "Use only one of: before_season, after_season, or specific_seasons"
    )
  }

  # Load and prepare base data
  if (verbose) {
    cat(
      "Loading game data for seasons",
      paste(range(seasons), collapse = "-"),
      "...\n"
    )
  }

  base_data <- load_game_data(seasons = seasons) |>
    mutate(
      game_idx = row_number(),
      season_idx = as.integer(as.factor(season)),
      week_idx = week_seq,
      fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
      lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
      home_idx = match(home_team, teams),
      away_idx = match(away_team, teams),
      hfa = as.integer(ifelse(location == "Home", 1, 0))
    )

  # Apply season filtering
  if (!is.null(specific_seasons)) {
    filtered_data <- filter_seasons(
      base_data,
      seasons = specific_seasons,
      min_week = min_week,
      max_week = max_week,
      verbose = verbose
    )
  } else if (!is.null(before_season)) {
    filtered_data <- filter_seasons(
      base_data,
      before_season = before_season,
      min_week = min_week,
      max_week = max_week,
      verbose = verbose
    )
  } else if (!is.null(after_season)) {
    filtered_data <- filter_seasons(
      base_data,
      after_season = after_season,
      min_week = min_week,
      max_week = max_week,
      verbose = verbose
    )
  } else {
    filtered_data <- filter_seasons(
      base_data,
      min_week = min_week,
      max_week = max_week,
      verbose = verbose
    )
  }

  # Add forecast data if requested
  if (!is.null(forecast_weeks)) {
    if (verbose) {
      cat("Adding forecast weeks...\n")
    }

    forecast_data <- extend_for_forecast(
      base_data,
      forecast_weeks,
      verbose = verbose
    )
    filtered_data <- bind_rows(filtered_data, forecast_data)

    # Re-create indices after combining data
    filtered_data <- filtered_data |>
      arrange(season, week, game_id) |>
      mutate(
        game_idx = row_number(),
        season_idx = as.integer(as.factor(season)),
        week_idx = week_seq
      )
  }

  # Select final variables and map teams to indices like the original
  selected_data <- filtered_data |>
    select(
      season_idx,
      week_idx,
      fw_season_idx,
      lw_season_idx,
      home_team,
      away_team,
      hfa,
      home_score,
      away_score,
      result,
      total
    ) |>
    mutate(
      home_team = match(home_team, teams),
      away_team = match(away_team, teams)
    )

  # Create Stan data using compose_data
  stan_data <- selected_data |>
    compose_data(
      .n_name = n_prefix("N"),
      N_games = nrow(filtered_data),
      N_teams = length(teams),
      N_seasons = length(unique(filtered_data$season_idx)),
      N_weeks = length(unique(filtered_data$week_idx))
    )

  if (verbose) {
    cat("\nStan data summary:\n")
    cat("  Games:", stan_data$N_games, "\n")
    cat("  Teams:", stan_data$N_teams, "\n")
    cat("  Seasons:", stan_data$N_seasons, "\n")
    cat("  Weeks:", stan_data$N_weeks, "\n")
    cat("  Variables:", paste(names(stan_data), collapse = ", "), "\n")
  }

  return(stan_data)
}

# Add these functions after the existing helper functions, around line 300

#' Create Forecast Stan Data from Previous Fit Data
#'
#' @description
#' Creates forecast data that can be used with generated_quantities() by taking
#' a previous stan_data_list and extending it with forecast games. This is
#' specifically designed for Stan's generated_quantities block.
#'
#' @param previous_stan_data Stan data list from a previous create_stan_data() call
#' @param forecast_seasons Vector of seasons to include in forecast
#' @param forecast_weeks Vector of weeks to include (optional, defaults to all available)
#' @param min_week Minimum week to include in forecast
#' @param max_week Maximum week to include in forecast
#' @param exclude_outcome_vars Remove outcome variables (scores, results) for forecasting
#' @param verbose Print detailed information
#'
#' @return Stan data list suitable for generated_quantities
#'
#' @examples
#' # Forecast 2006 season using model trained on 2002-2005
#' forecast_data <- create_forecast_data(
#'   previous_stan_data = fit_stan_data,
#'   forecast_seasons = 2006,
#'   verbose = TRUE
#' )
create_forecast_data <- function(
  previous_stan_data,
  forecast_seasons,
  forecast_weeks = NULL,
  min_week = NULL,
  max_week = NULL,
  exclude_outcome_vars = TRUE,
  verbose = FALSE
) {
  if (verbose) {
    cat("Creating forecast data from previous Stan data...\n")
    cat("Previous data summary:\n")
    cat("  Games:", previous_stan_data$N_games, "\n")
    cat("  Teams:", previous_stan_data$N_teams, "\n")
    cat("  Seasons:", previous_stan_data$N_seasons, "\n")
  }

  # Create forecast data using existing function
  forecast_stan_data <- create_stan_data(
    specific_seasons = forecast_seasons,
    min_week = min_week,
    max_week = max_week,
    verbose = verbose
  )

  # Ensure team and season dimensions match the original fit
  forecast_stan_data$N_teams <- previous_stan_data$N_teams

  # Calculate total seasons (previous + forecast)
  prev_max_season <- max(previous_stan_data$season_idx)
  forecast_season_offset <- prev_max_season

  # Adjust season indices to continue from previous data
  forecast_stan_data$season_idx <- forecast_stan_data$season_idx +
    forecast_season_offset
  forecast_stan_data$N_seasons <- max(forecast_stan_data$season_idx)

  if (verbose) {
    cat("Forecast data summary:\n")
    cat("  Games:", forecast_stan_data$N_games, "\n")
    cat(
      "  Season range:",
      min(forecast_stan_data$season_idx),
      "to",
      max(forecast_stan_data$season_idx),
      "\n"
    )
  }

  # Remove outcome variables if requested (common for generated_quantities)
  if (exclude_outcome_vars) {
    outcome_vars <- c("home_score", "away_score", "result", "total")
    for (var in outcome_vars) {
      if (var %in% names(forecast_stan_data)) {
        forecast_stan_data[[var]] <- NULL
        if (verbose) cat("Removed outcome variable:", var, "\n")
      }
    }
  }

  return(forecast_stan_data)
}

#' Extend Existing Stan Data with Forecast Period
#'
#' @description
#' Alternative approach that extends existing Stan data by appending forecast
#' games. Useful when you want to include both historical and forecast data
#' in the same Stan data structure.
#'
#' @param base_stan_data Existing Stan data list
#' @param forecast_seasons Vector of seasons to add
#' @param forecast_weeks Vector of weeks to add (optional)
#' @param min_week Minimum week to include from forecast seasons
#' @param max_week Maximum week to include from forecast seasons
#' @param exclude_outcome_vars Remove outcome variables from forecast portion
#' @param verbose Print detailed information
#'
#' @return Extended Stan data list with both historical and forecast data
extend_stan_data_with_forecast <- function(
  base_stan_data,
  forecast_seasons,
  forecast_weeks = NULL,
  min_week = NULL,
  max_week = NULL,
  exclude_outcome_vars = TRUE,
  verbose = FALSE
) {
  if (verbose) {
    cat("Extending Stan data with forecast periods...\n")
  }

  # Get forecast data
  forecast_data <- create_forecast_data(
    previous_stan_data = base_stan_data,
    forecast_seasons = forecast_seasons,
    forecast_weeks = forecast_weeks,
    min_week = min_week,
    max_week = max_week,
    exclude_outcome_vars = exclude_outcome_vars,
    verbose = verbose
  )

  # Combine the data
  combined_data <- base_stan_data

  # Append forecast arrays to existing arrays
  array_vars <- c(
    "season_idx",
    "week_idx",
    "fw_season_idx",
    "lw_season_idx",
    "home_team",
    "away_team",
    "hfa"
  )

  # Only include outcome variables if they exist in both datasets
  if (!exclude_outcome_vars) {
    outcome_vars <- c("home_score", "away_score", "result", "total")
    array_vars <- c(array_vars, outcome_vars)
  }

  for (var in array_vars) {
    if (var %in% names(forecast_data) && var %in% names(base_stan_data)) {
      combined_data[[var]] <- c(
        base_stan_data[[var]],
        forecast_data[[var]]
      )
    }
  }

  # Update counts
  combined_data$N_games <- length(combined_data$season_idx)
  combined_data$N_seasons <- max(combined_data$season_idx)
  combined_data$N_weeks <- length(unique(combined_data$week_idx))

  if (verbose) {
    cat("Combined data summary:\n")
    cat("  Total games:", combined_data$N_games, "\n")
    cat(
      "  (",
      base_stan_data$N_games,
      "historical +",
      forecast_data$N_games,
      "forecast )\n"
    )
    cat("  Total seasons:", combined_data$N_seasons, "\n")
  }

  return(combined_data)
}

### USAGE EXAMPLES ----

# Example 1: Reproduce your original fit_stan_data
# fit_stan_data_new <- create_stan_data(before_season = 2006, verbose = TRUE)

# Example 2: Get 2023 regular season only
# regular_2023 <- create_stan_data(specific_seasons = 2023, max_week = 18, verbose = TRUE)

# Example 3: Get multiple specific seasons
# recent_seasons <- create_stan_data(specific_seasons = c(2020, 2021, 2022), verbose = TRUE)

# Example 4: Extend 2023 data with 4 forecast weeks
# forecast_2023 <- create_stan_data(
#   specific_seasons = 2023,
#   forecast_weeks = 4,
#   verbose = TRUE
# )

# Example 5: Get season boundaries for any season
# boundaries_2023 <- get_season_boundaries(game_data, 2023)
# print(boundaries_2023)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. GQ HELPERS (OOS + FUTURE META) ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#' Last fitted global week from Stan data list
#'
#' @param fit_stan_data List created by create_stan_data() used for fitting
#' @return Integer last global week index included in the fit
get_last_fitted_week <- function(fit_stan_data) {
  if (!is.list(fit_stan_data) || is.null(fit_stan_data$week_idx)) {
    stop("fit_stan_data must be the list passed to Stan with week_idx")
  }
  max(as.integer(fit_stan_data$week_idx), na.rm = TRUE)
}

#' Last fitted season index from Stan data list
#'
#' @param fit_stan_data List created by create_stan_data() used for fitting
#' @return Integer last season index included in the fit
get_last_fitted_season <- function(fit_stan_data) {
  if (!is.list(fit_stan_data) || is.null(fit_stan_data$season_idx)) {
    stop("fit_stan_data must be the list passed to Stan with season_idx")
  }
  max(as.integer(fit_stan_data$season_idx), na.rm = TRUE)
}

#' Prepare a schedule with indices consistent with Stan
#'
#' @param games_df Data frame from load_game_data()
#' @param teams Character vector of team abbreviations (same used for create_stan_data)
#' @return Data frame with home_idx, away_idx, season_idx, week_idx, hfa, fw/lw flags
prepare_schedule_indices <- function(seasons = all_seasons, teams) {
  load_game_data(seasons = all_seasons) |>
    mutate(
      game_idx = row_number(),
      season_idx = as.integer(as.factor(season)),
      week_idx = week_seq,
      fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
      lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
      home_idx = match(home_team, teams),
      away_idx = match(away_team, teams),
      hfa = as.integer(ifelse(location == "Home", 1, 0))
    )
}

#' Build OOS arrays from a schedule subset
#'
#' @param schedule_df Data frame with columns home_idx, away_idx, season_idx, week_idx, hfa
#' @return Named list with N_oos and oos_* vectors for Stan GQ
build_oos_from_schedule <- function(schedule_df) {
  if (!nrow(schedule_df)) {
    return(list(
      N_oos = 0L,
      oos_home_team = integer(0),
      oos_away_team = integer(0),
      oos_season_idx = integer(0),
      oos_week_idx = integer(0),
      oos_hfa = integer(0)
    ))
  }
  list(
    N_oos = as.integer(nrow(schedule_df)),
    oos_home_team = as.integer(schedule_df$home_idx),
    oos_away_team = as.integer(schedule_df$away_idx),
    oos_season_idx = as.integer(schedule_df$season_idx),
    oos_week_idx = as.integer(schedule_df$week_idx),
    oos_hfa = as.integer(schedule_df$hfa)
  )
}

#' Build contiguous future-week metadata for multi-step forecasts
#'
#' @description
#' Creates future_is_first_week (0/1) and future_week_to_season arrays for weeks
#' (last_w+1) .. (last_w+horizon). Assumes global week_idx across seasons.
#'
#' @param fit_stan_data List passed to Stan fit (contains week_idx)
#' @param schedule_df Full schedule with week_idx, season_idx, fw_season_idx
#' @param horizon Number of future weeks to cover (0 allowed)
#' @return Named list with N_future_weeks and arrays for Stan GQ
build_future_meta <- function(fit_stan_data, schedule_df, horizon) {
  if (is.null(horizon) || horizon <= 0) {
    return(list(
      N_future_weeks = 0L,
      future_is_first_week = integer(0),
      future_week_to_season = integer(0)
    ))
  }

  last_w <- get_last_fitted_week(fit_stan_data)
  future_weeks <- seq.int(from = last_w + 1L, length.out = horizon)

  fut <- schedule_df |>
    filter(week_idx %in% future_weeks) |>
    group_by(week_idx) |>
    summarise(
      future_is_first_week = as.integer(any(fw_season_idx == 1)),
      future_week_to_season = as.integer(first(season_idx)),
      .groups = "drop"
    ) |>
    arrange(week_idx)

  # Ensure contiguity and fill any missing rows
  if (nrow(fut) != horizon) {
    fut <- tibble(week_idx = future_weeks) |>
      left_join(fut, by = "week_idx") |>
      tidyr::fill(future_week_to_season, .direction = "down") |>
      mutate(
        future_is_first_week = ifelse(
          is.na(future_is_first_week),
          0L,
          future_is_first_week
        ),
        future_week_to_season = as.integer(ifelse(
          is.na(future_week_to_season),
          max(schedule_df$season_idx, na.rm = TRUE),
          future_week_to_season
        ))
      )
  }

  list(
    N_future_weeks = as.integer(horizon),
    future_is_first_week = as.integer(fut$future_is_first_week),
    future_week_to_season = as.integer(fut$future_week_to_season)
  )
}

#' Prepare full GQ data list
#'
#' @param fit_stan_data Training data list used in fit
#' @param schedule_df Full schedule with indices (see prepare_schedule_indices)
#' @param targets Integer vector of global week_idx to score (can exceed N_weeks)
#' @return Stan data list for generate_quantities()
prepare_gq_data <- function(fit_stan_data, schedule_df, targets) {
  if (is.null(targets) || !length(targets)) {
    oos <- build_oos_from_schedule(schedule_df[0, ])
    fut <- build_future_meta(fit_stan_data, schedule_df, horizon = 0L)
    return(c(fit_stan_data, oos, fut))
  }
  last_w <- get_last_fitted_week(fit_stan_data)
  horizon <- max(0L, max(targets) - last_w)

  oos_df <- schedule_df |>
    filter(week_idx %in% targets) |>
    arrange(week_idx)
  oos <- build_oos_from_schedule(oos_df)
  fut <- build_future_meta(fit_stan_data, schedule_df, horizon = horizon)
  c(fit_stan_data, oos, fut)
}

#' Convenience wrapper to run generated quantities
#'
#' @param gq_mod Compiled cmdstanr model for mod_gq.stan
#' @param fit Fitted object from mod_fit.stan (cmdstanr fit)
#' @param gq_data Data list from prepare_gq_data()
#' @param seed RNG seed
#' @param sig_figs Integer
#' @param parallel_chains Integer
#' @return cmdstanr gq fit object
run_gq <- function(
  gq_mod,
  fit,
  gq_data,
  seed = 52,
  sig_figs = 10,
  parallel_chains = min(4L, parallel::detectCores())
) {
  gq_mod$generate_quantities(
    fitted_params = fit,
    data = gq_data,
    seed = seed,
    sig_figs = sig_figs,
    parallel_chains = parallel_chains
  )
}

#' Roll forward fit Stan data by a number of global weeks (no OOS)
#'
#' @description
#' Builds a new Stan fit data list including all games with week_idx <= last_w + weeks_ahead.
#' Uses the same variable names expected by mod_fit.stan. Outcomes must exist for all included games.
#'
#' @param fit_stan_data List used for previous fit
#' @param schedule_df Data frame from prepare_schedule_indices() (must include week_idx, season_idx, home_idx, away_idx, hfa and outcomes if used)
#' @param weeks_ahead Integer number of global weeks to extend (default 1)
#' @return Stan data list suitable for mod_fit.stan
roll_forward_fit_stan_data <- function(
  fit_stan_data,
  schedule_df,
  weeks_ahead = 1L
) {
  last_w <- get_last_fitted_week(fit_stan_data)
  target_w <- last_w + as.integer(weeks_ahead)

  df <- schedule_df |>
    dplyr::filter(week_idx <= target_w) |>
    dplyr::arrange(week_idx)

  # If outcomes exist, ensure no missing values are included
  if (all(c("result", "home_score", "away_score", "total") %in% names(df))) {
    if (any(is.na(df$result))) {
      stop(
        "roll_forward_fit_stan_data: Missing outcomes detected in included games. ",
        "Ensure target window doesn't include future games for fit."
      )
    }
  } else {
    stop(
      "roll_forward_fit_stan_data: schedule_df must include outcome columns for fitting (result, home_score, away_score, total)."
    )
  }

  stan_df <- df |>
    dplyr::transmute(
      season_idx = as.integer(season_idx),
      week_idx = as.integer(week_idx),
      fw_season_idx = as.integer(fw_season_idx),
      lw_season_idx = as.integer(lw_season_idx),
      home_team = as.integer(home_idx),
      away_team = as.integer(away_idx),
      hfa = as.integer(hfa),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      result = as.integer(result),
      total = as.integer(total)
    )

  N_teams <- if (!is.null(fit_stan_data$N_teams)) {
    as.integer(fit_stan_data$N_teams)
  } else {
    length(unique(c(df$home_idx, df$away_idx)))
  }

  stan_df |>
    compose_data(
      .n_name = n_prefix("N"),
      N_games = nrow(stan_df),
      N_teams = N_teams,
      N_seasons = max(stan_df$season_idx),
      N_weeks = length(unique(stan_df$week_idx))
    )
}

#' Run GQ for target weeks (wrapper combining prepare + run)
#'
#' @param gq_mod Compiled mod_gq model
#' @param fit Fitted params from mod_fit
#' @param fit_stan_data Training data list
#' @param schedule_df Full indexed schedule (prepare_schedule_indices)
#' @param targets Integer vector of global week_idx to score (PPC or OOS)
#' @param seed RNG seed
#' @param parallel_chains Integer
#' @return cmdstanr generated quantities object
gq_for_targets <- function(
  gq_mod,
  fit,
  fit_stan_data,
  schedule_df,
  targets,
  seed = 52,
  parallel_chains = min(4L, parallel::detectCores())
) {
  gq_data <- prepare_gq_data(fit_stan_data, schedule_df, targets)
  run_gq(gq_mod, fit, gq_data, seed = seed, parallel_chains = parallel_chains)
}

#' Compute next global week targets automatically (week_idx)
#'
#' @param fit_stan_data Training data list used in fit
#' @param horizon Integer number of weeks ahead (default 1)
#' @return Integer vector of global week_idx targets (last_w + 1 ... + horizon)
next_week_targets <- function(fit_stan_data, horizon = 1L) {
  last_w <- get_last_fitted_week(fit_stan_data)
  if (is.null(horizon) || horizon <= 0) {
    return(integer(0))
  }
  seq.int(from = last_w + 1L, length.out = as.integer(horizon))
}

#' Run GQ for the next horizon weeks (auto-detected by week_idx)
#'
#' @param gq_mod Compiled mod_gq model
#' @param fit Fitted params from mod_fit
#' @param fit_stan_data Training data list
#' @param schedule_df Full indexed schedule
#' @param horizon Integer number of future weeks (default 1)
#' @param seed RNG seed
#' @param parallel_chains Integer
#' @return cmdstanr generated quantities object
gq_next_weeks <- function(
  gq_mod,
  fit,
  fit_stan_data,
  schedule_df,
  horizon = 1L,
  seed = 52,
  parallel_chains = min(4L, parallel::detectCores())
) {
  targets <- next_week_targets(fit_stan_data, horizon = horizon)
  gq_for_targets(
    gq_mod,
    fit,
    fit_stan_data,
    schedule_df,
    targets,
    seed = seed,
    parallel_chains = parallel_chains
  )
}

#' One sequential step: predict next weeks (GQ) then optionally roll fit window and refit
#'
#' @param fit_mod Compiled mod_fit model
#' @param gq_mod Compiled mod_gq model
#' @param fit Current fitted object from mod_fit
#' @param fit_stan_data Current training data list
#' @param schedule_df Full indexed schedule
#' @param horizon Weeks ahead to predict (default 1)
#' @param refit_after If TRUE, roll window by `refit_weeks` and refit
#' @param refit_weeks How many weeks to extend the fit window (default 1)
#' @param seed RNG seed
#' @param parallel_chains Integer
#' @param iter_warmup,iter_sampling,adapt_delta,max_treedepth Fitting controls
#' @return list(fit = fit_new, fit_stan_data = fit_data_new, gq = gq_obj, targets = vector)
sequential_step <- function(
  fit_mod,
  gq_mod,
  fit,
  fit_stan_data,
  schedule_df,
  horizon = 1L,
  refit_after = TRUE,
  refit_weeks = 1L,
  seed = 52,
  sig_figs = 10,
  chains = 4,
  parallel_chains = min(4L, parallel::detectCores()),
  iter_warmup = 500,
  iter_sampling = 1000,
  adapt_delta = 0.9,
  max_treedepth = 10,
  warm_start = TRUE,
  reuse_metric = TRUE
) {
  # 1) Predict next weeks via GQ
  targets <- next_week_targets(fit_stan_data, horizon = horizon)
  gq_obj <- gq_for_targets(
    gq_mod,
    fit,
    fit_stan_data,
    schedule_df,
    targets,
    seed = seed,
    parallel_chains = parallel_chains
  )

  # 2) Optionally roll window and refit
  fit_stan_data_new <- fit_stan_data
  fit_new <- fit
  if (isTRUE(refit_after) && refit_weeks > 0L) {
    fit_stan_data_new <- roll_forward_fit_stan_data(
      fit_stan_data,
      schedule_df,
      weeks_ahead = refit_weeks
    )
    dims_match <- TRUE
    if (!is.null(fit_stan_data$N_teams) && !is.null(fit_stan_data_new$N_teams)) {
      dims_match <- dims_match && identical(
        as.integer(fit_stan_data$N_teams),
        as.integer(fit_stan_data_new$N_teams)
      )
    }
    if (!is.null(fit_stan_data$N_seasons) && !is.null(fit_stan_data_new$N_seasons)) {
      dims_match <- dims_match && identical(
        as.integer(fit_stan_data$N_seasons),
        as.integer(fit_stan_data_new$N_seasons)
      )
    }
    if (!is.null(fit_stan_data$N_weeks) && !is.null(fit_stan_data_new$N_weeks)) {
      dims_match <- dims_match && identical(
        as.integer(fit_stan_data$N_weeks),
        as.integer(fit_stan_data_new$N_weeks)
      )
    }

    init_arg <- 0
    if (isTRUE(warm_start) && !isTRUE(dims_match)) {
      message(
        "sequential_step: warm_start requested but skipped because data dimensions changed."
      )
    }
    if (isTRUE(warm_start) && isTRUE(dims_match)) {
      init_try <- try(
        build_init_lists_from_fit(fit, fit_mod, n_chains = chains, seed = seed),
        silent = TRUE
      )
      if (!inherits(init_try, "try-error")) init_arg <- init_try
    }
    metric_type <- NULL
    inv_metric <- NULL
    if (isTRUE(reuse_metric) && !isTRUE(dims_match)) {
      message(
        "sequential_step: reuse_metric requested but skipped because data dimensions changed."
      )
    }
    if (isTRUE(reuse_metric) && isTRUE(dims_match)) {
      met <- try(reuse_metric_from_fit(fit), silent = TRUE)
      if (!inherits(met, "try-error")) {
        metric_type <- met$metric
        inv_metric <- met$inv_metric
      }
    }
    fit_new <- fit_mod$sample(
      data = fit_stan_data_new,
      seed = seed,
      chains = chains,
      parallel_chains = parallel_chains,
      sig_figs = sig_figs,
      init = init_arg,
      metric = metric_type,
      inv_metric = inv_metric,
      iter_warmup = if (isTRUE(warm_start) || isTRUE(reuse_metric)) {
        max(50L, as.integer(iter_warmup))
      } else {
        iter_warmup
      },
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  }

  list(
    fit = fit_new,
    fit_stan_data = fit_stan_data_new,
    gq = gq_obj,
    targets = targets
  )
}

# ---- Snapshot/save helpers ------------------------------------------------- #

#' Build per-chain init lists from a previous fit's parameter draws and save
#'
#' @param fit CmdStanMCMC fit
#' @param compiled_model compiled cmdstanr model (mod_fit)
#' @param n_chains how many init lists to save (usually = chains)
#' @param seed RNG seed
#' @return list of init lists (length n_chains)
build_init_lists_from_fit <- function(
  fit,
  compiled_model,
  n_chains = 4L,
  seed = 52
) {
  stopifnot(inherits(fit, "CmdStanMCMC"))
  vars <- compiled_model$variables()
  params_info <- vars$parameters
  param_names <- names(params_info)
  if (length(param_names) == 0L) {
    stop("No parameters found in compiled model.")
  }

  draws_df <- fit$draws(variables = param_names, format = "df")
  if (!nrow(draws_df)) {
    stop("No parameter draws found in previous fit.")
  }

  set.seed(seed)
  row_ids <- sample(seq_len(nrow(draws_df)), size = n_chains, replace = TRUE)

  # reconstruct arrays from flat columns like name[1], name[1,2]
  make_value <- function(base, dims, row) {
    # scalar
    if (is.null(dims) || length(dims) == 0L || prod(dims) == 0L) {
      val <- suppressWarnings(as.numeric(row[[base]]))
      if (is.na(val)) {
        stop("Missing scalar init for ", base)
      }
      return(val)
    }
    pat <- paste0('^', gsub("\\[", "\\[", base), "\\[")
    cols <- grep(pat, names(row), value = TRUE)
    if (length(cols) == 0L) {
      if (length(dims) == 1L && dims[1] == 1L && !is.null(row[[base]])) {
        return(array(as.numeric(row[[base]]), dim = dims))
      }
      stop("Could not find init columns for ", base)
    }
    vals <- as.numeric(unlist(row[cols], use.names = FALSE))
    # order by multi-index lexicographically
    idx_str <- sub(paste0('^', base, '\\['), '', sub(']$', '', cols))
    split_idx <- strsplit(idx_str, ",")
    idx_mat <- do.call(rbind, lapply(split_idx, function(v) as.integer(v)))
    ord <- do.call(order, as.data.frame(idx_mat))
    vals <- vals[ord]
    dim_sizes <- if (is.null(dim(idx_mat))) {
      as.integer(max(idx_mat))
    } else {
      as.integer(apply(idx_mat, 2, max))
    }
    array(vals, dim = dim_sizes)
  }

  inits <- vector("list", n_chains)
  for (ch in seq_len(n_chains)) {
    row <- draws_df[row_ids[ch], , drop = FALSE]
    lst <- list()
    for (p in param_names) {
      dims <- params_info[[p]]$dimensions
      lst[[p]] <- make_value(p, dims, row)
    }
    inits[[ch]] <- lst
  }
  inits
}

#' Extract metric info for reuse from previous fit
reuse_metric_from_fit <- function(fit) {
  stopifnot(inherits(fit, "CmdStanMCMC"))
  fit_metric = fit$metadata()$metric
  if (fit_metric == "diag_e") {
    message("reuse_metric_from_fit: using diag_e metric")
    fit_inv_metric = fit$inv_metric(matrix = FALSE)
  } else if (fit_metric == "dense_e") {
    message("reuse_metric_from_fit: using dense_e metric")
    fit_inv_metric = fit$inv_metric(matrix = TRUE)
  } else {
    stop("reuse_metric_from_fit: unsupported metric type: ", fit_metric)
  }
  list(
    metric = fit_metric,
    inv_metric = fit_inv_metric
  )
}

#' Extract tidy state summaries from a GQ object (filtered/predicted by team)
#'
#' @param gq cmdstanr GQ result
#' @param teams optional character vector of team codes to attach
#' @return tibble with team_id, stat, mean, sd
extract_state_summaries_from_gq <- function(gq, teams = NULL) {
  stopifnot(inherits(gq, "CmdStanGQ"))
  as_df <- function(var) posterior::as_draws_matrix(gq$draws(variables = var))
  # filtered last (vector[N_teams])
  f_last <- as_df("filtered_team_strength_last")
  p_next <- as_df("predicted_team_strength_next_mean")
  N_teams <- ncol(f_last)
  tibble::tibble(
    team_id = rep(seq_len(N_teams), times = 2),
    stat = rep(c("filtered_last", "predicted_next_mean"), each = N_teams),
    mean = c(colMeans(f_last), colMeans(p_next)),
    sd = c(apply(f_last, 2, sd), apply(p_next, 2, sd))
  ) |>
    dplyr::mutate(
      team = if (!is.null(teams) && length(teams) >= N_teams) {
        teams[team_id]
      } else {
        as.character(team_id)
      }
    )
}

#' Extract tidy OOS predictions from a GQ object (mu_pred/y_pred)
#'
#' @param gq cmdstanr GQ result
#' @param schedule_df schedule used to build OOS arrays
#' @return tibble with row per OOS game: mu_mean, mu_sd, y_mean, y_sd
extract_oos_predictions_from_gq <- function(gq, schedule_df) {
  stopifnot(inherits(gq, "CmdStanGQ"))
  mu <- posterior::as_draws_matrix(gq$draws(variables = "mu_pred"))
  y <- posterior::as_draws_matrix(gq$draws(variables = "y_pred"))
  k <- ncol(mu)
  tibble::tibble(
    oos_row = seq_len(k),
    game_id = schedule_df$game_id[seq_len(k)],
    week_idx = schedule_df$week_idx[seq_len(k)],
    season = schedule_df$season[seq_len(k)],
    home = schedule_df$home_team[seq_len(k)],
    away = schedule_df$away_team[seq_len(k)],
    mu_mean = colMeans(mu),
    mu_sd = apply(mu, 2, sd),
    y_mean = colMeans(y),
    y_sd = apply(y, 2, sd)
  )
}

#' Save a compact snapshot of the sequential step
#'
#' @param path directory to save files (created if missing)
#' @param fit current fit (CmdStanMCMC)
#' @param fit_mod compiled model (for shapes)
#' @param gq current GQ (CmdStanGQ)
#' @param fit_stan_data training data list
#' @param oos_df schedule subset used for OOS (rows must match N_oos)
#' @param teams optional team codes
#' @param n_init how many per-chain init lists to save
#' @param seed RNG seed for sampling init draws from previous posterior
#' @return invisibly, the snapshot list
save_sequential_snapshot <- function(
  path,
  fit,
  fit_mod,
  gq,
  fit_stan_data,
  oos_df,
  teams = NULL,
  n_init = 4L,
  seed = 52
) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  # Warm-start artifacts
  init_lists <- try(
    build_init_lists_from_fit(fit, fit_mod, n_chains = n_init, seed = seed),
    silent = TRUE
  )
  metric_obj <- try(reuse_metric_from_fit(fit), silent = TRUE)

  # Summaries
  state_summ <- try(
    extract_state_summaries_from_gq(gq, teams = teams),
    silent = TRUE
  )
  pred_summ <- try(extract_oos_predictions_from_gq(gq, oos_df), silent = TRUE)

  snap <- list(
    when = Sys.time(),
    last_week_idx = get_last_fitted_week(fit_stan_data),
    N_weeks = fit_stan_data$N_weeks,
    N_seasons = fit_stan_data$N_seasons,
    N_teams = fit_stan_data$N_teams,
    init_lists = if (!inherits(init_lists, "try-error")) init_lists else NULL,
    metric_type = if (!inherits(metric_obj, "try-error")) {
      metric_obj$metric
    } else {
      NULL
    },
    inv_metric = if (!inherits(metric_obj, "try-error")) {
      metric_obj$inv_metric
    } else {
      NULL
    },
    state_summaries = if (!inherits(state_summ, "try-error")) {
      state_summ
    } else {
      NULL
    },
    oos_predictions = if (!inherits(pred_summ, "try-error")) pred_summ else NULL
  )

  saveRDS(
    snap,
    file = file.path(path, sprintf("snapshot_w%03d.rds", snap$last_week_idx))
  )
  invisible(snap)
}
#' Find the latest snapshot file in a directory
#'
#' @param path Directory containing snapshot_wXXX.rds files
#' @return Full file path of the latest snapshot or NULL if none
find_latest_snapshot <- function(path) {
  if (!dir.exists(path)) {
    return(NULL)
  }
  files <- list.files(
    path,
    pattern = '^snapshot_w[0-9]{3}\\.rds$',
    full.names = TRUE
  )
  if (!length(files)) {
    return(NULL)
  }
  w <- as.integer(sub('^.*snapshot_w([0-9]{3})\\.rds$', '\\1', files))
  files[order(w, decreasing = TRUE)][1]
}

#' Load a snapshot by week or latest if week is NULL
#'
#' @param path Directory of snapshots
#' @param week Optional numeric week_idx to load (global)
#' @return Snapshot list as saved by save_sequential_snapshot()
load_sequential_snapshot <- function(path, week = NULL) {
  if (!dir.exists(path)) {
    stop("Snapshot path does not exist: ", path)
  }
  file <- if (is.null(week)) {
    find_latest_snapshot(path)
  } else {
    fp <- file.path(path, sprintf('snapshot_w%03d.rds', as.integer(week)))
    if (file.exists(fp)) fp else NULL
  }
  if (is.null(file)) {
    stop("No snapshot found in ", path)
  }
  readRDS(file)
}

#' Convert a snapshot to warm-start artifacts
#'
#' @param snapshot Snapshot list from load_sequential_snapshot
#' @param compiled_model Compiled cmdstan model (mod_fit) for shapes
#' @param n_chains Number of chains to initialize
#' @return list(init = init_lists|"random", metric = type|NULL, inv_metric = object|NULL)
snapshot_to_warm_start <- function(snapshot, compiled_model, n_chains = 4L) {
  init <- if (!is.null(snapshot$init_lists)) snapshot$init_lists else "random"
  list(
    init = init,
    metric = if (!is.null(snapshot$metric_type)) snapshot$metric_type else NULL,
    inv_metric = if (!is.null(snapshot$inv_metric)) {
      snapshot$inv_metric
    } else {
      NULL
    }
  )
}

#' Build fit Stan data up to a specific global week from a full indexed schedule
#'
#' @param schedule_df Output of prepare_schedule_indices()
#' @param last_w Global week_idx to include (<= max in schedule)
#' @return Stan data list suitable for mod_fit.stan
build_fit_stan_data_until_week <- function(schedule_df, last_w) {
  df <- schedule_df |>
    dplyr::filter(week_idx <= as.integer(last_w)) |>
    dplyr::arrange(week_idx)
  if (!all(c("result", "home_score", "away_score", "total") %in% names(df))) {
    stop(
      "build_fit_stan_data_until_week: schedule_df must include outcomes for fitting."
    )
  }
  if (any(is.na(df$result))) {
    stop(
      "build_fit_stan_data_until_week: Missing outcomes in requested window."
    )
  }
  stan_df <- df |>
    dplyr::transmute(
      season_idx = as.integer(season_idx),
      week_idx = as.integer(week_idx),
      fw_season_idx = as.integer(fw_season_idx),
      lw_season_idx = as.integer(lw_season_idx),
      home_team = as.integer(home_idx),
      away_team = as.integer(away_idx),
      hfa = as.integer(hfa),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      result = as.integer(result),
      total = as.integer(total)
    )
  N_teams <- length(unique(c(df$home_idx, df$away_idx)))
  stan_df |>
    compose_data(
      .n_name = n_prefix("N"),
      N_games = nrow(stan_df),
      N_teams = N_teams,
      N_seasons = max(stan_df$season_idx),
      N_weeks = length(unique(stan_df$week_idx))
    )
}

#' Resume or start one sequential step from a snapshot directory
#'
#' @param snapshot_dir Directory containing snapshots
#' @param fit_mod Compiled mod_fit
#' @param gq_mod Compiled mod_gq
#' @param schedule_df Full indexed schedule (prepare_schedule_indices)
#' @param horizon Weeks ahead to predict
#' @param chains,parallel_chains,seed,iter_warmup,iter_sampling,adapt_delta,max_treedepth Fitting controls
#' @return list(fit, fit_stan_data, gq, targets, snapshot)
resume_sequential_from_snapshot <- function(
  snapshot_dir,
  fit_mod,
  gq_mod,
  schedule_df,
  horizon = 1L,
  chains = 4L,
  parallel_chains = min(4L, parallel::detectCores()),
  seed = 52,
  iter_warmup = 200,
  iter_sampling = 1000,
  adapt_delta = 0.9,
  max_treedepth = 10
) {
  snap <- load_sequential_snapshot(snapshot_dir)
  fit_stan_data <- build_fit_stan_data_until_week(
    schedule_df,
    snap$last_week_idx
  )
  warm <- snapshot_to_warm_start(snap, fit_mod, n_chains = chains)

  targets <- next_week_targets(fit_stan_data, horizon = horizon)
  gq_obj <- gq_for_targets(
    gq_mod,
    NULL,
    fit_stan_data,
    schedule_df,
    targets,
    seed = seed,
    parallel_chains = parallel_chains
  )

  fit_stan_data_next <- roll_forward_fit_stan_data(
    fit_stan_data,
    schedule_df,
    weeks_ahead = 1L
  )
  fit_new <- fit_mod$sample(
    data = fit_stan_data_next,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    init = warm$init,
    metric = warm$metric,
    inv_metric = warm$inv_metric,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )

  list(
    fit = fit_new,
    fit_stan_data = fit_stan_data_next,
    gq = gq_obj,
    targets = targets,
    snapshot = snap
  )
}
