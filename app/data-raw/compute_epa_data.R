#' Compute EPA data combining offense and defense
#'
#' @param pbp_df    Play-by-play data
#' @param scaled_wp  Logical, whether to scale EPA values (default FALSE)
#' @return Tibble with both offense and defense EPA metrics joined
compute_epa_data <- function(pbp_df, scaled_wp = FALSE) {
  epa_off <- calc_epa_ratings(pbp_df, side = "offense", scaled_wp = scaled_wp)
  epa_def <- calc_epa_ratings(pbp_df, side = "defense", scaled_wp = scaled_wp)
  
  epaData <- epa_off |>
    left_join(epa_def,
              by = c("game_id", "season", "week", "team", "home_team", "away_team"))
  
  return(epaData)
}