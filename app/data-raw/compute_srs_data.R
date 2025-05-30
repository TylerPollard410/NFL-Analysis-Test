# compute_srs_data.R
# Helper script to compute weekly SRS data for multiple reset window options
# with caching and incremental updates

# Dependencies: dplyr, purrr

#' Compute SRS data for multiple reset window options, with optional caching and incremental updates
#'
#' @param game_df      Data frame of games with columns: season, week, team, opponent, result, team_score, opponent_score
#' @param resets       List of logical or numeric values specifying reset parameter options (default: list(TRUE, 10, 20))
#' @param tol          Numeric tolerance for SRS convergence (default: 1e-3)
#' @param max_iter     Maximum iterations for convergence (default: 100)
#' @param recompute_all Logical; if FALSE and cache exists, load from cache and update only new weeks (default: FALSE)
#' @param cache_file   Path to .rda file for caching results (default: "app/data/srs_data.rda")
#' @param ...          Additional arguments passed to calc_weekly_standings
#' @return A tibble with season, week, team, summary columns and rating stats (MOV, SOS, SRS, OSRS, DSRS) for each reset window
compute_srs_data <- function(game_df,
                             resets       = list(TRUE, 10, 20),
                             tol          = 1e-3,
                             max_iter     = 100,
                             recompute_all= FALSE,
                             cache_file   = "app/data/srs_data.rda",
                             ...) {
  # Internal helper: compute ratings for a subset of games
  compute_for_df <- function(df_subset) {
    rating_cols <- c("MOV", "SOS", "SRS", "OSRS", "DSRS")
    suffixes    <- map_chr(resets, ~ if (identical(.x, TRUE)) "" else paste0("_", .x))
    srs_list    <- map2(resets, suffixes, function(r, suffix) {
      weekly <- calc_weekly_standings(
        df_subset,
        tol      = tol,
        max_iter = max_iter,
        reset    = r,
        ...
      )
      if (suffix == "") {
        weekly |>
          select(season, week, team,
                 games, wins, losses, ties, pf, pa, pd, win_pct,
                 all_of(rating_cols))
      } else {
        weekly |>
          select(season, week, team, all_of(rating_cols)) |>
          rename_with(~ paste0(.x, suffix), all_of(rating_cols))
      }
    })
    reduce(srs_list, left_join, by = c("season", "week", "team"))
  }
  
  # Full compute vs. incremental update
  if (!recompute_all && file.exists(cache_file)) {
    message("Loading cached SRS data from: ", cache_file)
    load(cache_file)  # this must load an object named `srs_data`
    existing <- srs_data
    
    # Find which season-weeks need computing
    new_games <- game_df |>
      filter(!is.na(result)) |>
      distinct(season, week) |>
      anti_join(existing |> distinct(season, week),
                by = c("season", "week"))
    if (nrow(new_games) == 0) {
      message("No new weeks; returning cached data.")
      return(existing)
    }
    
    message("Computing SRS for new weeks with rolling context…")
    # Build an index of all valid season-weeks
    weekGrid <- game_df |>
      filter(!is.na(result)) |>
      distinct(season, week) |>
      arrange(season, week) |>
      mutate(idx = row_number())
    
    new_idx   <- weekGrid |>
      semi_join(new_games, by = c("season", "week")) |>
      pull(idx)
    max_win   <- max(unlist(Filter(is.numeric, resets)), na.rm = TRUE)
    start_idx <- max(1, min(new_idx) - max_win)
    
    context_weeks <- weekGrid |>
      slice(start_idx:max(new_idx)) |>
      select(season, week)
    
    # Subset games for context and compute
    df_subset <- game_df |>
      semi_join(context_weeks, by = c("season", "week")) |>
      filter(!is.na(result))
    
    full_new <- compute_for_df(df_subset)
    
    # Extract only newly-needed weeks
    new_ratings <- full_new |>
      semi_join(new_games, by = c("season", "week"))
    
    srs_data <- bind_rows(existing, new_ratings)
    
  } else {
    # No valid cache or forced full recompute
    message(ifelse(!file.exists(cache_file),
                   "Cache not found; computing all SRS data…",
                   "Recomputing all SRS data from scratch…"))
    srs_data <- compute_for_df(game_df)
  }
  
  # Return in sorted order
  srs_data |>
    arrange(season, week, team)
}






# 
# # ---------------------------
# # Usage in UpdateData.R:
# # ---------------------------
# # Compute and cache SRS data
# srs_data <- compute_srs_data(
#   game_data |> filter(season %in% 2022:2024),
#   resets = list(TRUE, 10, 20),
#   tol = 1e-3,
#   max_iter = 100,
#   recompute_all = FALSE,
#   cache_file = "app/data/srs_data.rda"
# )
# # Save to disk
# srs_data_temp <- srs_data
# srs_data <- srs_data |> filter(season <= 2023)
# save(srs_data, file = "app/data/srs_data.rda")
# 
# srs_data2 <- game_data_long |>
#   filter(season %in% 2022:2024) |>
#   select(season, week, team) |>
#   left_join(srs_data)
# 
# 
# weekly_standings_data_TRUE <- compute_weekly_standings_data(
#   game_data |> filter(season %in% 2022:2024),
#   tol = 1e-3,
#   max_iter = 200,
#   reset = TRUE,
#   recompute_all = TRUE, #TRUE,
#   #cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings.rda" 
#   cache_file = "./app/data/weekly_standings_data.rda"
# )
# weekly_standings_data_10 <- compute_weekly_standings_data(
#   game_data |> filter(season %in% 2022:2024),
#   tol = 1e-3,
#   max_iter = 200,
#   reset = 10,
#   recompute_all = TRUE, #TRUE,
#   #cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings.rda" 
#   cache_file = "./app/data/weekly_standings_data.rda"
# )
# weekly_standings_data_20 <- compute_weekly_standings_data(
#   game_data |> filter(season %in% 2022:2024),
#   tol = 1e-3,
#   max_iter = 200,
#   reset = 20,
#   recompute_all = TRUE, #TRUE,
#   #cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings.rda" 
#   cache_file = "./app/data/weekly_standings_data.rda"
# )
# 
# 
# srs_data_comp <- game_data_long |>
#   filter(season %in% 2022:2024) |>
#   select(season, week, team) |>
#   left_join(
#     weekly_standings_data_TRUE |>
#       select(season, week, team, MOV, SOS, SRS, OSRS, DSRS)
#   )|>
#   left_join(
#     weekly_standings_data_10 |>
#       select(season, week, team,
#              MOV_10 = MOV,
#              SOS_10 = SOS,
#              SRS_10 = SRS,
#              OSRS_10 = OSRS,
#              DSRS_10 = DSRS)
#   ) |>
#   left_join(
#     weekly_standings_data_20 |>
#       select(season, week, team,
#              MOV_20 = MOV,
#              SOS_20 = SOS,
#              SRS_20 = SRS,
#              OSRS_20 = OSRS,
#              DSRS_20 = DSRS)
#   )


#' # compute_srs_data.R
#' # Helper script to compute weekly SRS data for multiple reset window options with caching
#' 
#' # Dependencies: dplyr, purrr
#' 
#' #' Compute SRS data for multiple reset window options, with optional caching
#' #'
# #' @param game_df      Data frame of games with columns: season, week, team, opponent, result, team_score, opponent_score
# #' @param resets       List of logical or numeric values specifying reset parameter options (default: list(TRUE, 10, 20))
# #' @param tol          Numeric tolerance for SRS convergence (default: 1e-3)
# #' @param max_iter     Maximum iterations for convergence (default: 100)
# #' @param recompute_all Logical; if FALSE and cache exists, load from cache instead of recomputing (default: FALSE)
# #' @param cache_file   Path to .rda file for caching results (default: "app/data/srs_data.rda")
# #' @param ...          Additional arguments passed to calc_weekly_standings
# #' @return A tibble with season, week, team, summary columns and rating stats (MOV, SOS, SRS, OSRS, DSRS) for each reset window
#' compute_srs_data <- function(game_df,
#'                              resets = list(TRUE, 10, 20),
#'                              tol = 1e-3,
#'                              max_iter = 100,
#'                              recompute_all = FALSE,
#'                              cache_file = "app/data/srs_data.rda",
#'                              ...) {
#'   # If cache exists and not forced to recompute, load and return
#'   if (!recompute_all && file.exists(cache_file)) {
#'     message("Loading cached SRS data from: ", cache_file)
#'     load(cache_file)  # expects object 'srs_data'
#'     return(srs_data)
#'   }
#'   
#'   # Otherwise compute all resets
#'   message(ifelse(!file.exists(cache_file),
#'                  "Cache not found; computing all SRS data...",
#'                  "Recomputing all SRS data..."))
#'   
#'   # Define rating columns to extract
#'   rating_cols <- c("MOV", "SOS", "SRS", "OSRS", "DSRS")
#'   
#'   # Generate suffixes for each reset value
#'   suffixes <- map_chr(resets, function(r) {
#'     if (identical(r, TRUE)) "" else paste0("_", r)
#'   })
#'   
#'   # Compute weekly standings for each reset value
#'   srs_list <- map2(resets, suffixes, function(r, suffix) {
#'     df_weekly <- calc_weekly_standings(
#'       game_df,
#'       tol = tol,
#'       max_iter = max_iter,
#'       reset = r,
#'       ...
#'     )
#'     
#'     if (suffix == "") {
#'       # Include summary fields for default reset
#'       df_weekly |>
#'         select(season, week, team,
#'                games, wins, losses, ties, pf, pa, pd, win_pct,
#'                all_of(rating_cols))
#'     } else {
#'       # Only extract and rename rating columns for other resets
#'       df_weekly |>
#'         select(season, week, team,
#'                all_of(rating_cols)) |>
#'         rename_with(~ paste0(.x, suffix), all_of(rating_cols))
#'     }
#'   })
#'   
#'   # Join all results by season, week, team
#'   srs_data <- reduce(srs_list, function(x, y) {
#'     left_join(x, y, by = c("season", "week", "team"))
#'   })
#'   
#'   return(srs_data)
#' }


