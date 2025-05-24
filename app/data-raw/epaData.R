# epaData.R
# Optimized functions to compute and combine EPA offense/defense summaries per game-team

#' Calculate detailed EPA metrics for offense or defense
#'
#' @param pbpData    Data frame of play-by-play containing at least:
#'                   game_id, season, week, posteam, defteam,
#'                   home_team, away_team, epa, ep, vegas_wp,
#'                   play, pass, rush, penalty, play_type, special
#' @param side       Character, either "offense" or "defense" (default "offense").
#' @param scaled_wp  Logical, whether to scale each play's EPA by scaled Vegas win-probability before aggregation (default FALSE).
#' @return Tibble with one row per team-game and the following prefixed columns:
#'   off_plays, off_epa_sum, off_epa_mean,
#'   off_pass_plays, off_pass_epa_sum, off_pass_epa_mean,
#'   off_rush_plays, off_rush_epa_sum, off_rush_epa_mean,
#'   off_penalty_plays, off_penalty_epa_sum, off_penalty_epa_mean,
#'   off_kick_plays, off_kick_epa_sum, off_kick_epa_mean,
#'   off_special_plays, off_special_epa_sum, off_special_epa_mean
calc_epa <- function(pbpData,
                     side = c("offense", "defense"),
                     scaled_wp = FALSE) {
  require(dplyr)
  
  side <- match.arg(side)
  team_col <- if (side == "offense") "posteam" else "defteam"
  prefix   <- if (side == "offense") "off_" else "def_"
  
  # Precompute flags and scaled/raw EPA once
  pbp <- pbpData |>
    filter(!is.na(epa), !is.na(ep), !is.na(.data[[team_col]])) |>
    mutate(
      is_ball_play     = play == 1 | play_type == "field_goal",
      is_pass_play     = play == 1 & pass == 1 & penalty == 0,
      is_rush_play     = play == 1 & rush == 1 & penalty == 0,
      is_penalty_play  = play == 1 & penalty == 1,
      is_kick_play     = play_type == "field_goal",
      is_special_play  = special == 1 & play_type != "field_goal",
      # EPA value, scaled if requested
      epa_value        = if (scaled_wp) {
        epa * (1 - 4 * (0.5 - vegas_wp)^2)
      } else {
        epa
      }
    )
  
  # Summarise per game-team
  result <- pbp |>
    group_by(game_id, season, week,
             team = .data[[team_col]], home_team, away_team) |>
    summarise(
      # total plays and EPA
      total_plays      = sum(is_ball_play, na.rm = TRUE),
      total_epa_sum    = sum(epa_value[is_ball_play],   na.rm = TRUE),
      total_epa_mean   = mean(epa_value[is_ball_play],  na.rm = TRUE),
      # passing
      pass_plays       = sum(is_pass_play,              na.rm = TRUE),
      pass_epa_sum     = sum(epa_value[is_pass_play],   na.rm = TRUE),
      pass_epa_mean    = mean(epa_value[is_pass_play],  na.rm = TRUE),
      # rushing
      rush_plays       = sum(is_rush_play,              na.rm = TRUE),
      rush_epa_sum     = sum(epa_value[is_rush_play],   na.rm = TRUE),
      rush_epa_mean    = mean(epa_value[is_rush_play],  na.rm = TRUE),
      # penalties
      penalty_plays    = sum(is_penalty_play,           na.rm = TRUE),
      penalty_epa_sum  = sum(epa_value[is_penalty_play],na.rm = TRUE),
      penalty_epa_mean = mean(epa_value[is_penalty_play],na.rm = TRUE),
      # kicks
      kick_plays       = sum(is_kick_play,              na.rm = TRUE),
      kick_epa_sum     = sum(epa_value[is_kick_play],   na.rm = TRUE),
      kick_epa_mean    = mean(epa_value[is_kick_play],  na.rm = TRUE),
      # special teams
      special_plays    = sum(is_special_play,           na.rm = TRUE),
      special_epa_sum  = sum(epa_value[is_special_play],na.rm = TRUE),
      special_epa_mean = mean(epa_value[is_special_play],na.rm = TRUE),
      .groups          = "drop"
    ) |>
    # Prefix metric columns
    rename_with(.fn = ~ paste0(prefix, .),
                .cols = -c(game_id, season, week, team, home_team, away_team)) |>
    # Replace NaN with 0
    mutate(across(starts_with(prefix), ~ ifelse(is.nan(.x), 0, .x)))
  
  return(result)
}

#' Compute EPA data combining offense and defense
#'
#' @param pbpData    Play-by-play data
#' @param scaled_wp  Logical, whether to scale EPA values (default FALSE)
#' @return Tibble with both offense and defense EPA metrics joined
compute_epa_data <- function(pbpData, scaled_wp = FALSE) {
  epa_off <- calc_epa(pbpData, side = "offense", scaled_wp = scaled_wp)
  epa_def <- calc_epa(pbpData, side = "defense", scaled_wp = scaled_wp)
  
  epaData <- epa_off |>
    left_join(epa_def,
              by = c("game_id", "season", "week", "team", "home_team", "away_team"))
  
  return(epaData)
}

# Example usage:
# epa_raw    <- compute_epa_data(pbpData, scaled_wp = FALSE)
# epa_scaled <- compute_epa_data(pbpData, scaled_wp = TRUE)
