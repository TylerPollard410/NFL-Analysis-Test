# scoresData.R
# Functions to generate and combine weekly efficiency and scoring stats per team-game

# Dependencies: dplyr, nflverse (loaded externally in UpdateData.R)

#' Update or load weekly team efficiency stats
#'
#' @param seasons    Integer vector of all seasons
#' @param stats_loc     File path to save/load nflStatsWeek (e.g. ".../nflStatsWeek.rda")
#' @param recompute_all Logical, if TRUE forces full recalculation even if saved file exists (default FALSE)
#' @return Tibble of weekly team stats (nflStatsWeek)
calc_weekly_team_stats <- function(seasons = all_seasons, 
                                   sum_level = "week",
                                   stat_level = "team",
                                   season_level = "REG+POST",
                                   stats_loc, 
                                   recompute_all = FALSE) {
  # uses dplyr, nflverse
  
  if (!recompute_all && file.exists(stats_loc)) {
    cat("Updating Weekly Team Stats\n",
        "Calculating", get_current_season(), "season Data")
    load(stats_loc)
    # drop current season to refresh
    nflStatsWeek <- nflStatsWeek |>
      filter(season != get_current_season())
    
    temp <- with_progress({
      calculate_stats(
        seasons       = get_current_season(),
        summary_level = sum_level,
        stat_type     = stat_level,
        season_type   = season_level
      )
    })
    nflStatsWeek <- bind_rows(nflStatsWeek, temp)
  } else {
    cat("Recomputing All Weekly Team Stats\n",
        "Calculating", min(seasons), "-", get_current_season(), "season Data")
    # full recalculation
    nflStatsWeek <- with_progress({
      calculate_stats(
        seasons       = seasons,
        summary_level = sum_level,
        stat_type     = stat_level,
        season_type   = season_level
      )
    })
  }
  
  save(nflStatsWeek, file = stats_loc)
  return(nflStatsWeek)
}



# Example usage:
# stats_loc   <- "~/Desktop/NFLAnalysisTest/scripts/UpdateData/PriorData/nflStatsWeek.rda"
# scores_data <- compute_scores_data(gameDataLong, pbpData, allSeasons, stats_loc, recompute_all=TRUE)
