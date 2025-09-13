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
