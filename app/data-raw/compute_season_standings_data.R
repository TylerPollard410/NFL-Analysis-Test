#' Compute full season standings with ratings
#'
#' @param game_df Data frame of all games, must include columns season, team, opponent, result, team_score, opponent_score
#' @return A tibble with combined standings and rating stats
compute_season_standings_data <- function(game_df = game_data, ...) {
  
  # Always filter to regular season
  reg_data <- game_df |> 
    filter(season_type == "REG", !is.na(result))
  
  # Base standings via nflverse
  base_tbl <- nfl_standings(games = reg_data)
  
  # Prepare long-format game data
  long <- reg_data |>  
    clean_homeaway(invert = c("result", "spread_line"))
  
  # Compute ratings per season, passing the grouping key for messaging
  rating_tbl <- long |> 
    group_by(season) |> 
    group_modify(function(.x, .y) calc_srs_ratings(.x, season_year = .y$season, ...)) |>
    #group_modify(function(.x, .y) compute_ratings(.x, season_year = .y$season), .keep = TRUE) |>
    ungroup()
  
  # Merge base standings and ratings
  out_tbl <- base_tbl |> 
    left_join(rating_tbl, by = c("season", "team")) |>
    arrange(season, team) |>
    relocate(
      MOV, SOS, SRS, OSRS, DSRS,
      .before = sov
    )
  
  out_tbl
}