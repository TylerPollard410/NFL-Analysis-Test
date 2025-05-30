# turnoverData.R
# Function to compute and combine turnover statistics per game-team

# Dependencies: dplyr (loaded externally in UpdateData.R)

#' Compute turnover feature dataset for modeling
#'
#' @param game_long_df  Tibble of long-format game-team rows with columns:
#'                      game_id, season, week, team, opponent
#' @param pbp_df       Play-by-play tibble with columns: game_id, season, week,
#'                      posteam, defteam, interception, fumble_forced,
#'                      fumble_lost, fumble
#' @return Tibble with one row per game-team containing turnover features:
#'   turnover_diff, turnover_won, turnover_lost,
#'   interception_won, interception_lost, fumble_won, fumble_lost
compute_turnover_data <- function(game_long_df = game_data_long, 
                                  pbp_df = pbp_data) {
  # uses dplyr
  
  # STEP 1: Summarise raw turnover metrics by team-game
  turnoverData <- pbp_df |>
    select(game_id, season, week,
           posteam, defteam,
           interception, fumble_forced, fumble_lost, fumble) |>
    filter(!is.na(posteam)) |>
    summarise(
      interception_lost = sum(interception,    na.rm = TRUE),
      fumble_lost       = sum(fumble_lost,     na.rm = TRUE),
      turnover_lost     = interception_lost + fumble_lost,
      .by = c(game_id, season, week, posteam, defteam)
    ) |>
    mutate(
      interception_won = rev(interception_lost),
      fumble_won       = rev(fumble_lost),
      turnover_won     = rev(turnover_lost)
    ) |>
    mutate(
      turnover_diff = turnover_won - turnover_lost
    )
  
  # STEP 2: Merge with gameDataLong to enforce ordering
  id_cols <- c("game_id", "season", "week", "team", "opponent")
  turnover_cols <- c(
    "turnover_diff", "turnover_won", "turnover_lost",
    "interception_won", "interception_lost",
    "fumble_won", "fumble_lost"
  )
  
  turnoverFeatures <- game_long_df |>
    filter(!is.na(result)) |>
    select(all_of(id_cols)) |>
    left_join(
      turnoverData |>
        select(game_id, posteam, all_of(turnover_cols)),
      by = join_by(game_id, team == posteam)
    )
  
  return(turnoverFeatures)
}

# Example usage:
# turnover_data <- compute_turnover_data(gameDataLong, pbpData)
