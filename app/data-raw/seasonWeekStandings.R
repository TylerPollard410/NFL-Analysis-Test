# seasonWeekStandings.R
# Helper script to compute cumulative weekly SRS standings for each season-week

# Dependencies: dplyr, nflverse (loaded externally in UpdateData.R)

#' Compute weekly SRS standings across all seasons and weeks
#'
#' @param gameData   Data frame of games with columns season, week, team, opponent, result, team_score, opponent_score
#' @param tol        Numeric tolerance for SRS convergence (default 1e-3)
#' @param max_iter   Maximum iterations for convergence (default 100)
#' @param reset      TRUE (by season), FALSE (last 20 weeks rolling), or integer N (rolling window)
#' @return Tibble with all summary fields plus MOV, SOS, SRS, OSRS, DSRS, and week
compute_weekly_standings <- function(gameData, tol = 1e-3, max_iter = 100, reset = TRUE, ...) {
  valid_games <- gameData |> filter(!is.na(result))
  weekGrid <- valid_games |> distinct(season, week) |> arrange(season, week)
  results <- vector("list", nrow(weekGrid))
  
  # Decide reset mode and window
  if (identical(reset, TRUE)) {
    reset_mode <- "season"; window <- NA_integer_
  } else if (identical(reset, FALSE)) {
    reset_mode <- "rolling"; window <- 20L
  } else if (is.numeric(reset) && length(reset) == 1 && reset > 0) {
    reset_mode <- "rolling"; window <- as.integer(reset)
  } else {
    stop("reset must be TRUE, FALSE, or a single positive integer")
  }
  
  for (i in seq_len(nrow(weekGrid))) {
    s <- weekGrid$season[i]; w <- weekGrid$week[i]
    cat(sprintf("Computing Season %s Week %s...\n", s, w))
    
    if (reset_mode == "season") {
      slice_df <- valid_games |> filter(season == s, week <= w)
    } else if (reset_mode == "rolling") {
      week_id <- which(weekGrid$season == s & weekGrid$week == w)
      if (week_id <= window) {
        slice_weeks <- weekGrid[seq_len(week_id), ]
      } else {
        slice_weeks <- weekGrid[(week_id - window + 1):week_id, ]
      }
      slice_df <- valid_games |> semi_join(slice_weeks, by = c("season", "week"))
    }
    
    long_df <- clean_homeaway(slice_df, invert = c("result", "spread_line"))
    # Always group by team only so no duplicates when rolling window crosses seasons
    base_w <- long_df |> 
      group_by(team) |> 
      summarise(
        games   = n(),
        wins    = sum(result > 0),
        true_wins = sum(result > 0),
        losses  = sum(result < 0),
        ties    = sum(result == 0),
        pf      = sum(team_score, na.rm = TRUE),
        pa      = sum(opponent_score, na.rm = TRUE),
        pd      = pf - pa,
        win_pct = ifelse(games > 0, wins / games, NA_real_),
        .groups = "drop"
      ) |> mutate(season = s) |> relocate(season)
    
    ratings <- compute_ratings(
      df           = long_df,
      season_year  = s,
      tol          = tol,
      max_iter     = max_iter,
      print_message = FALSE,
      ...
    )
    
    results[[i]] <- base_w |> left_join(ratings, by = "team") |> mutate(week = w)
  }
  bind_rows(results)
}

#' Wrapper to update weekly standings with caching
update_weekly_standings <- function(
    gameData,
    tol = 1e-3,
    max_iter = 100,
    reset = TRUE,
    recompute_all = FALSE,
    cache_file = "./app/data/seasonWeekStandings.rda",
    ...
) {
  if (!recompute_all && file.exists(cache_file)) {
    cat("Loading cached season-week standings...\n")
    load(cache_file)
    curr_season <- get_current_season()
    cat(sprintf("Updating current season %s...\n", curr_season))
    existing <- seasonWeekStandings |> filter(season != curr_season)
    new_data <- gameData |> filter(season == curr_season)
    weekly_curr <- compute_weekly_standings(
      new_data,
      tol = tol,
      max_iter = max_iter,
      reset = reset,
      ...
    )
    seasonWeekStandings <- bind_rows(existing, weekly_curr)
  } else {
    if (file.exists(cache_file)) {
      cat("Recomputing all seasons...\n")
    } else {
      cat("Computing all seasons for the first time...\n")
    }
    seasonWeekStandings <- compute_weekly_standings(
      gameData,
      tol = tol,
      max_iter = max_iter,
      reset = reset,
      ...
    )
  }
  seasonWeekStandings |>
    select(season, week, team, everything()) |>
    arrange(season, week, team)
}

# Auto-run when sourced in UpdateData.R
# if (exists("gameData")) {
#   seasonWeekStandings <- update_weekly_standings(
#     gameData,
#     reset = TRUE,
#     recompute_all = FALSE,
#     cache_file = "./scripts/UpdateData/PriorData/seasonWeekStandings.rda"
#   )
# }













# seasonWeekStandings.R
# Helper script to compute cumulative weekly SRS standings for each season-week

# Dependencies: dplyr, nflverse (loaded externally in UpdateData.R)

#' #' Compute weekly SRS standings across all seasons and weeks
#' #'
#' #' @param gameData   Data frame of games with columns season, week, team, opponent, result, team_score, opponent_score
#' #' @param tol        Numeric tolerance for SRS convergence (default 1e-3)
#' #' @param max_iter   Maximum iterations for convergence (default 100)
#' #' @param reset      Logical; if TRUE (default), reset ratings each season; if FALSE, cumulative across seasons
#' #' @return Tibble with all summary fields plus MOV, SOS, SRS, OSRS, DSRS, and week
#' compute_weekly_standings <- function(gameData, tol = 1e-3, max_iter = 100, reset = TRUE, ...) {
#'   # Only keep games with non-missing result, once
#'   valid_games <- gameData |> filter(!is.na(result))
#'   
#'   # build the grid of season/weeks
#'   weekGrid <- valid_games |>
#'     distinct(season, week) |>
#'     arrange(season, week)
#'   
#'   results <- vector("list", nrow(weekGrid))
#'   
#'   for (i in seq_len(nrow(weekGrid))) {
#'     s <- weekGrid$season[i]
#'     w <- weekGrid$week[i]
#'     cat(sprintf("Computing Season %s Week %s...\n", s, w))
#'     
#'     # slice up to that week
#'     slice_df <- if (reset) {
#'       valid_games |> filter(season == s, week <= w)
#'     } else {
#'       valid_games |> filter((season < s) | (season == s & week <= w))
#'     }
#'     
#'     # Hand-calculate summary fields for ALL games so far this season
#'     long_df <- clean_homeaway(slice_df, invert = c("result", "spread_line"))
#'     base_w <- long_df |> 
#'       group_by(team) |> 
#'       summarise(
#'         games   = n(),
#'         wins    = sum(result > 0),
#'         true_wins = sum(result > 0), # Can adjust if you want a more advanced wins calculation
#'         losses  = sum(result < 0),
#'         ties    = sum(result == 0),
#'         pf      = sum(team_score, na.rm = TRUE),
#'         pa      = sum(opponent_score, na.rm = TRUE),
#'         pd      = pf - pa,
#'         win_pct = ifelse(games > 0, wins / games, NA_real_),
#'         # You can add division/conference % if you add those fields to long_df
#'         .groups = "drop"
#'       )
#'     
#'     ratings <- compute_ratings(
#'       df           = long_df,
#'       season_year  = s,
#'       tol          = tol,
#'       max_iter     = max_iter,
#'       print_message = FALSE,
#'       ...
#'     )
#'     
#'     results[[i]] <- base_w |>
#'       left_join(ratings, by = "team") |>
#'       mutate(week = w, season = s)
#'   }
#'   bind_rows(results)
#' }
#' 
#' #' Wrapper to update weekly standings with caching
#' update_weekly_standings <- function(
#'     gameData,
#'     tol = 1e-3,
#'     max_iter = 100,
#'     reset = TRUE,
#'     recompute_all = FALSE,
#'     cache_file = "./app/data/seasonWeekStandings.rda",
#'     ...
#' ) {
#'   if (!recompute_all && file.exists(cache_file)) {
#'     cat("Loading cached season-week standings...\n")
#'     load(cache_file)  # loads seasonWeekStandings
#'     curr_season <- get_current_season()
#'     cat(sprintf("Updating current season %s...\n", curr_season))
#'     existing <- seasonWeekStandings |> filter(season != curr_season)
#'     new_data <- gameData |> filter(season == curr_season)
#'     weekly_curr <- compute_weekly_standings(
#'       new_data,
#'       tol = tol,
#'       max_iter = max_iter,
#'       reset = reset,
#'       ...
#'     )
#'     seasonWeekStandings <- bind_rows(existing, weekly_curr)
#'   } else {
#'     if (file.exists(cache_file)) {
#'       cat("Recomputing all seasons...\n")
#'     } else {
#'       cat("Computing all seasons for the first time...\n")
#'     }
#'     seasonWeekStandings <- compute_weekly_standings(
#'       gameData,
#'       tol = tol,
#'       max_iter = max_iter,
#'       reset = reset,
#'       ...
#'     )
#'   }
#'   seasonWeekStandings |>
#'     select(season, week, team, everything()) |>
#'     arrange(season, week, team)
#' }

# Auto-run when sourced in UpdateData.R ----
# if (exists("gameData")) {
#   seasonWeekStandings <- update_weekly_standings(
#     gameData,
#     tol = 1e-3,
#     max_iter = 100,
#     reset = TRUE,
#     recompute_all = TRUE,
#     cache_file = "./app/data/seasonWeekStandings.rda",
#   )
# }
#
# seasonWeekStandings |> 
# filter(season %in% 2023:2024) |> 
#   group_by(season, week) |> 
#   summarise(
#     across(c(MOV, SOS, SRS, OSRS, DSRS), 
#            ~round(mean(.x), digits = 4))
#     ) |> 
#   print(n = 50)
