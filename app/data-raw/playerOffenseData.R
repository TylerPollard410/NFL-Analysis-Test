# playerOffenseData.R
# Function to load and process player offense stats for modeling

# Dependencies: nflverse, tidyverse

#' Load player offense statistics and merge with game-level identifiers
#'
#' @param seasons Integer vector of seasons to load (default: 2006:most_recent_season())
#' @param game_data_long Data frame with game-level identifiers (default: gameDataLong)
#' @return A tibble of player offense stats with game_id, season, week, and opponent fields
compute_player_offense_data <- function(seasons = 2006:most_recent_season(),
                                        game_data_long = gameDataLong) {
  # Load raw player offense stats
  player_stats <- load_player_stats(
    seasons = seasons,
    stat_type = "offense"
  )
  
  # Join with game-level info to get game_id, week, and opponent
  playerOffenseData <- player_stats |>
    left_join(
      game_data_long |> select(game_id, season, week, opponent),
      by = join_by(season, week, opponent_team == opponent)
    ) |>
    relocate(game_id, .before = 1)
  
  return(playerOffenseData)
}

# Auto-run when sourced in UpdateData.R
# if (exists("allSeasons") && exists("gameDataLong")) {
#   playerOffenseData <- compute_player_offense_data(allSeasons, gameDataLong)
# }
