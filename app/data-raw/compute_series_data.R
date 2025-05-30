
#' Compute series features for modeling, with optional recompute
#'
#' @param game_long_df Tibble of long-format game-team rows with columns: game_id, season, week, team, opponent
#' @param pbp_df      Play-by-play tibble for series calculation
#' @param series_loc   File path for nflSeriesWeek RDA
#' @param recompute_all Logical, if TRUE forces series stats recompute (default FALSE)
#' @return Tibble with one row per game-team containing series conversion rates
compute_series_data <- function(game_long_df = game_data_long,
                                pbp_df = pbp_data, 
                                series_loc, 
                                recompute_all = FALSE) {
  # uses dplyr
  
  # STEP 1: Load or generate series conversion rates
  seriesData <- calc_weekly_series_stats(pbp_df, series_loc, recompute_all)
  
  # STEP 2: Merge with gameDataLong to enforce ordering
  id_cols <- c("game_id", "season", "week", "team", "opponent")
  seriesFeatures <- game_long_df |>
    filter(!is.na(result)) |>
    select(all_of(id_cols)) |>
    left_join(seriesData, by = join_by(season, week, team))
  
  return(seriesFeatures)
}

# Example usage:
# series_loc      <- "~/Desktop/NFLAnalysisTest/scripts/UpdateData/PriorData/nflSeriesWeek.rda"
# series_features <- compute_series_data(gameDataLong, pbpData, series_loc, recompute_all = TRUE)
# series_cols     <- colnames(select(series_features, contains("off"), contains("def")))
