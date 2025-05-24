# modelData.R
# Functions to assemble all feature datasets into modeling-wide and long formats

# Dependencies: dplyr, tidyr, nflverse (loaded externally in UpdateData.R)

#' Generate long-format modeling dataset by joining multiple feature tables
#'
#' @param gameDataLong          Tibble of long-format game-team rows with columns:
#'                              game_id, season, week, team, opponent,
#'                              location, locationID, home_away,
#'                              team_score, opponent_score
#' @param pbpData               Play-by-play tibble
#' @param allSeasons            Integer vector of all seasons
#' @param stats_loc             File path for nflStatsWeek RDA
#' @param series_loc            File path for nflSeriesWeek RDA
#' @param elo_loc               File path for ELO ratings RDA
#' @param seasonWeekStandings   Tibble of season-week SRS standings loaded externally
#' @param scaled_wp             Logical, whether to use scaled EPA metrics (default FALSE)
#' @return Tibble with one row per team-game and all feature columns
compute_modDataLong <- function(gameDataLong, pbpData,
                                allSeasons, stats_loc, series_loc, elo_loc,
                                seasonWeekStandings, scaled_wp = FALSE) {
  # uses dplyr, nflverse
  
  # 1) compute fresh
  # elo_wide <- compute_elo_data(gameDataLong, elo_loc)
  # eloDataLong  <- clean_homeaway(elo_wide)
  # srsData      <- seasonWeekStandings
  # epaData      <- compute_epa_data(pbpData, scaled_wp = scaled_wp)
  # scoresData   <- compute_scores_data(gameDataLong, pbpData, allSeasons, stats_loc)
  # seriesData   <- compute_series_data(gameDataLong, pbpData, series_loc)
  # turnoverData <- compute_turnover_data(gameDataLong, pbpData)
  # redzoneData  <- compute_redzone_data(gameDataLong, pbpData)
  
  # compute from loaded data previously sourced in UpdataData.R
  eloDataLong  <- clean_homeaway(
    eloData |> select(game_id, season, week, home_team, away_team, contains("elo"))
    ) |> select(-location)
  srsData      <- seasonWeekStandings
  epaData      <- epaData |> select(-any_of(c("home_team", "away_team")))
  scoresData   <- scoresData
  seriesData   <- seriesData
  turnoverData <- turnoverData
  redzoneData  <- redzoneData
  
  # 2) define join keys (team-level)
  id_cols <- c("game_id", "season", "week", "team", "opponent")
  
  # 3) assemble long-format merged table
  modDataLong <- gameDataLong |>
    select(all_of(id_cols), location, locationID, team_score, opponent_score) |>
    left_join(eloDataLong,  by = id_cols) |>
    left_join(srsData,      by = c("season", "week", "team")) |>
    left_join(epaData,      by = c("game_id", "season", "week", "team")) |>
    left_join(scoresData,   by = c("game_id", "season", "week", "team")) |>
    left_join(seriesData,   by = id_cols) |>
    left_join(turnoverData, by = id_cols) |>
    left_join(redzoneData,  by = id_cols)
  
  return(modDataLong)
}

#' Pivot long-format modeling dataset to wide format per game
#'
#' @param modDataLong  Tibble returned by compute_modDataLong()
#' @return Wide tibble keyed by game_id, season, week, with separate home_/away_ columns for each feature
compute_modData <- function(modDataLong) {
  # uses dplyr, tidyr
  
  # pivot to wide per game, splitting home/away via 'location'
  modData <- modDataLong |>
    pivot_wider(
      id_cols     = c(game_id, season, week),
      names_from  = location,
      names_glue  = "{location}_{.value}",
      values_from = setdiff(names(modDataLong), c("game_id", "season", "week", "location", "locationID"))
    )
  
  return(modData)
}

# Example usage:
# modDataLong <- compute_modDataLong(gameDataLong, pbpData,
#                                    allSeasons, stats_loc, series_loc, elo_loc,
#                                    seasonWeekStandings, scaled_wp = FALSE)
# modData     <- compute_modData(modDataLong)
