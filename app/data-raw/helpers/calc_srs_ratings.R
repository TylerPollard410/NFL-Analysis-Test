# calc_srs_ratings.R
# Helper script to compute season standings and rating statistics (MOV, SOS, SRS, OSRS, DSRS)

# Dependencies: dplyr, nflverse (loaded externally in UpdateData.R)

#' Compute rating statistics (SOS, SRS, OSRS, DSRS) for a single season
#'
#' @param game_long_df        Long-format game data for one season, with columns: team, opponent, result, team_score, opponent_score
#' @param season_year  Integer specifying the season year for messaging
#' @param tol       Numeric tolerance for convergence (default 1e-3)
#' @param max_iter  Maximum iterations to attempt before stopping (default 100)
#' @param print_message Prints progress updates while looping through function
#' @return A tibble with columns team, MOV, SOS, SRS, OSRS, DSRS
calc_srs_ratings <- function(game_long_df = game_data_long, 
                             season_year = get_current_season(), 
                             tol = 1e-3, 
                             max_iter = 100, 
                             print_message = TRUE) {
  if(print_message) {
    cat("Computing Season", season_year, "...\n")
  }
  
  # Initial summary of team performance
  base <- game_long_df |> 
    group_by(team) |> 
    summarise(
      games_played   = n(),
      team_score     = sum(team_score, na.rm = TRUE),
      opponent_score = sum(opponent_score, na.rm = TRUE),
      result         = sum(result, na.rm = TRUE),
      .groups        = "drop"
    ) |> 
    mutate(
      team_PPG = team_score / games_played,
      opp_PPG  = opponent_score / games_played,
      #MOV      = (team_score - opponent_score) / games_played
      MOV      = result / games_played
    )
  
  # League average points per game
  leaguePPG <- sum(base$team_score, na.rm = TRUE) / sum(base$games_played, na.rm = TRUE)
  
  # Initialize ratings
  ratings <- base |> 
    transmute(
      team,
      #games_played, team_score, opponent_score, team_PPG, opp_PPG, result,
      MOV,
      SOS  = 0,
      SRS  = MOV,
      OSRS = team_PPG - leaguePPG,
      DSRS = SRS - OSRS
    )
  
  iter <- 0
  repeat {
    iter <- iter + 1
    prev <- ratings
    
    updates <- game_long_df |> 
      select(team, opponent, result, team_score) |> 
      left_join(
        ratings |> select(team, SRS, DSRS),
        by = c("opponent" = "team")
      ) |> 
      mutate(
        opp_SRS = SRS,
        newSRS  = result + opp_SRS,
        newOSRS = team_score + DSRS - mean(team_score, na.rm = TRUE)
      ) |> 
      group_by(team) |> 
      summarise(
        SOS  = mean(opp_SRS, na.rm = TRUE),
        SRS  = mean(newSRS, na.rm = TRUE),
        OSRS = mean(newOSRS, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      mutate(DSRS = SRS - OSRS)
    
    ratings <- ratings |> 
      select(team, MOV) |> 
      left_join(updates, by = "team")
    
    delta <- max(
      abs(ratings$SRS - prev$SRS),
      abs(ratings$OSRS - prev$OSRS),
      abs(ratings$DSRS - prev$DSRS),
      na.rm = TRUE
    )
    if (delta < tol) {
      cat("- Converged after", iter, "iterations.\n")
      break
    }
    if (iter >= max_iter) {
      cat("- Reached maximum iterations =", max_iter, "without full convergence\n")
      break
    }
  }
  
  ratings
}


# Auto-run when sourced in UpdateData.R ----
# if (exists("gameData")) {
#   seasonStandings <- compute_season_standings(gameData)
# }


