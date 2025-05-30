# seasonWeekStandings.R
# Wrapper script to update cumulative weekly SRS standings for each season-week with caching

# Dependencies: dplyr, nflverse (loaded externally in UpdateData.R)

#' Compute weekly SRS standings across all seasons and weeks
#'
#' @param game_df   Data frame of games with columns season, week, team, opponent, result, team_score, opponent_score
#' @param tol        Numeric tolerance for SRS convergence (default 1e-3)
#' @param max_iter   Maximum iterations for convergence (default 100)
#' @param reset      TRUE (by season), FALSE (last 20 weeks rolling), or integer N (rolling window)
#' @return Tibble with all summary fields plus MOV, SOS, SRS, OSRS, DSRS, and week
#' Wrapper to update weeely standings with caching
compute_weekly_standings_data <- function(game_df,
                                         tol = 1e-3,
                                         max_iter = 100,
                                         reset = TRUE,
                                         recompute_all = FALSE,
                                         cache_file = "./app/data/weekly_standings_data.rda",
                                         ...) {
  if (!recompute_all && file.exists(cache_file)) {
    cat("Loading cached season-week standings...\n")
    load(cache_file)
    curr_season <- get_current_season()
    cat(sprintf("Updating current season %s...\n", curr_season))
    existing <- weekly_standings_data |> filter(season != curr_season)
    new_data <- game_df |> filter(season == curr_season)
    weekly_curr <- calc_weekly_standings(
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
    seasonWeekStandings <- calc_weekly_standings(
      game_df,
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
# if (exists("game_df")) {
#   seasonWeekStandings <- update_weekly_standings(
#     game_df,
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
#' #' @param game_df   Data frame of games with columns season, week, team, opponent, result, team_score, opponent_score
#' #' @param tol        Numeric tolerance for SRS convergence (default 1e-3)
#' #' @param max_iter   Maximum iterations for convergence (default 100)
#' #' @param reset      Logical; if TRUE (default), reset ratings each season; if FALSE, cumulative across seasons
#' #' @return Tibble with all summary fields plus MOV, SOS, SRS, OSRS, DSRS, and week
#' compute_weekly_standings <- function(game_df, tol = 1e-3, max_iter = 100, reset = TRUE, ...) {
#'   # Only keep games with non-missing result, once
#'   valid_games <- game_df |> filter(!is.na(result))
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
    #'     game_df,
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
#'     new_data <- game_df |> filter(season == curr_season)
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
#'       game_df,
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
# if (exists("game_df")) {
#   seasonWeekStandings <- update_weekly_standings(
#     game_df,
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
