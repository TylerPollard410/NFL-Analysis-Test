
# seasonWeekStandings.R
# Helper script to compute cumulative weekly SRS standings for each season-week

# Libraries
library(dplyr)
library(nflverse)

#' Compute weekly SRS standings across all seasons and weeks
#'
#' @param gameData   Data frame of games with columns season, week, team, opponent, result, team_score, opponent_score
#' @param tol        Numeric tolerance for SRS convergence (default 1e-3)
#' @param max_iter   Maximum iterations for convergence (default 100)
#' @param reset      Logical; if TRUE (default), reset ratings each season; if FALSE, cumulative across seasons
#' @return Tibble with all nfl_standings fields plus MOV, SOS, SRS, OSRS, DSRS, and week
compute_weekly_standings <- function(gameData, tol = 1e-3, max_iter = 100, reset = TRUE) {
  # 1. build the grid of season/weeks
  weekGrid <- gameData |>
    filter(!is.na(result)) |>
    distinct(season, week) |>
    arrange(season, week)
  
  # 2. pre-allocate list
  results <- vector("list", nrow(weekGrid))
  
  # 3. loop over each season-week
  for (i in seq_len(nrow(weekGrid))) {
    s <- weekGrid$season[i]
    w <- weekGrid$week[i]
    cat(sprintf("Computing Season %s Week %s...\n", s, w))
    
    # a) slice up to that week
    slice_df <- if (reset) {
      gameData |>
        filter(season == s, week <= w, !is.na(result))
    } else {
      gameData |>
        filter((season < s) | (season == s & week <= w), !is.na(result))
    }
    
    # b) get all the base standings columns
    base_w <- suppressMessages(
      nfl_standings(games = slice_df, ranks = "NONE")
    )
    
    # c) compute your ratings on the same slice
    long_df <- clean_homeaway(slice_df, invert = c("result", "spread_line"))
    ratings <- compute_ratings(
      df           = long_df,
      season_year  = s,
      tol          = tol,
      max_iter     = max_iter,
      print_message = FALSE
    )
    
    # d) join them together and add the week
    results[[i]] <- base_w |>
      left_join(ratings, by = "team") |>
      mutate(week = w)
  }
  
  # 4. bind them all into one tibble
  bind_rows(results)
}


#' Wrapper to update weekly standings with caching
#'
#' @param gameData full data
#' @param tol tolerance
#' @param max_iter max iterations
#' @param reset reset per season or cumulative
#' @param recompute_all logical; if TRUE compute all seasons, if FALSE update only current
#' @param cache_file path to cache .rda file
#' @return updated seasonWeekStandings tibble
update_weekly_standings <- function(
    gameData,
    tol = 1e-3,
    max_iter = 100,
    reset = TRUE,
    recompute_all = FALSE,
    cache_file = "./app/data/seasonWeekStandings.rda"
) {
  if (!recompute_all && file.exists(cache_file)) {
    cat("Loading cached season-week standings...\n")
    load(cache_file)  # loads seasonWeekStandings
    curr_season <- get_current_season()
    cat(sprintf("Updating current season %s...\n", curr_season))
    existing <- seasonWeekStandings |>
      filter(season != curr_season)
    new_data <- gameData |>
      filter(season == curr_season, !is.na(result))
    weekly_curr <- compute_weekly_standings(
      new_data,
      tol = tol,
      max_iter = max_iter,
      reset = reset
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
      reset = reset
    )
  }
  seasonWeekStandings |>
    select(season, week, team, everything()) |>
    arrange(season, week, team)
}

# Auto-run when sourced in UpdateData.R
# Assigns seasonWeekStandings by default updating only current season
if (exists("gameData")) {
  seasonWeekStandings <- update_weekly_standings(
    gameData,
    reset = TRUE,
    recompute_all = FALSE,
    cache_file = "./scripts/UpdateData/PriorData/seasonWeekStandings.rda"
  )
}


