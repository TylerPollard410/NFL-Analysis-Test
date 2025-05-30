# eloData.R
# Helper script to compute game-by-game ELO ratings and manage cached ELO history

# Dependencies: dplyr, tibble (loaded externally in UpdateData.R)

#' Compute game-by-game and end-of-week ELO ratings for a series of games
#'
#' @param games Data frame of games with columns: game_id, season, week, gameday,
#'              home_team, away_team, home_score, away_score, location
#' @param initial_elo Numeric or named vector of starting ratings (default 1500)
#' @param K Numeric learning rate (default 20)
#' @param home_advantage Numeric points added for home team (default 65)
#' @param d Numeric scaling factor for ELO formula (default 400)
#' @param apply_margin_multiplier Logical; whether to weight updates by margin of victory (default TRUE)
#' @return A list with elements:
#'   \item{elo_history}{Data frame of pre- and post-game ratings for each game}
#'   \item{final_ratings}{Named vector of final team ratings after all games}
#'   \item{team_ratings}{Data frame of end-of-week ratings snapshots with season and week}
calc_elo_ratings <- function(games,
                             initial_elo = 1500,
                             K = 20,
                             home_advantage = 65,
                             d = 400,
                             apply_margin_multiplier = TRUE) {
  games <- games[order(as.Date(games$gameday)), ]
  games <- games |> filter(!is.na(home_score), !is.na(away_score))
  teams <- sort(unique(c(games$home_team, games$away_team)))
  # Initialize elo_ratings: scalar or named vector
  if (length(initial_elo) == 1) {
    elo_ratings <- setNames(rep(initial_elo, length(teams)), teams)
  } else {
    if (is.null(names(initial_elo))) stop("initial_elo vector must be named by team")
    elo_ratings <- initial_elo[teams]
  }
  elo_history <- games[, c("game_id", "season", "week", "gameday", 
                           "home_team", "away_team", "home_score", "away_score")]
  elo_history$home_elo_pre <- NA_real_
  elo_history$away_elo_pre <- NA_real_
  elo_history$home_elo_post <- NA_real_
  elo_history$away_elo_post <- NA_real_
  
  expected_prob <- function(rating_A, rating_B, home_field = 0, home_field_ind = 0, d = 400) {
    home_field_adj <- home_field * home_field_ind
    1 / (1 + 10 ^ ((rating_B - (rating_A + home_field_adj)) / d))
  }
  
  team_ratings_full <- data.frame()
  for (i in seq_len(nrow(games))) {
    game <- games[i, ]
    home_team <- game$home_team
    away_team <- game$away_team
    home_elo <- elo_ratings[home_team]
    away_elo <- elo_ratings[away_team]
    elo_history$home_elo_pre[i] <- home_elo
    elo_history$away_elo_pre[i] <- away_elo
    is_home <- ifelse(game$location == "Home", 1, 0)
    exp_home <- expected_prob(home_elo, away_elo, home_field = home_advantage, home_field_ind = is_home, d = d)
    outcome_home <- ifelse(game$home_score > game$away_score, 1,
                           ifelse(game$home_score < game$away_score, 0, 0.5))
    margin <- abs(game$home_score - game$away_score)
    multiplier <- if (apply_margin_multiplier) {
      log(margin + 1) * (2.2 / ((0.001 * abs(home_elo - away_elo)) + 2.2))
    } else 1
    home_elo_new <- home_elo + K * multiplier * (outcome_home - exp_home)
    away_elo_new <- away_elo + K * multiplier * ((1 - outcome_home) - (1 - exp_home))
    elo_history$home_elo_post[i] <- home_elo_new
    elo_history$away_elo_post[i] <- away_elo_new
    elo_ratings[home_team] <- home_elo_new
    elo_ratings[away_team] <- away_elo_new
    
    # Determine if we've reached end of current week or season
    next_week <- if (i < nrow(games)) games$week[i + 1] else NA_integer_
    if (is.na(next_week) || game$week != next_week) {
      team_ratings_full <- rbind(
        team_ratings_full,
        data.frame(team = names(elo_ratings), elo_ratings = as.numeric(elo_ratings),
                   season = game$season, week = game$week)
      )
      message("Finished ELO for Season ", game$season, " Week ", game$week)
    }
  }
  list(
    elo_history = elo_history,
    final_ratings = elo_ratings,
    team_ratings = team_ratings_full
  )
}


# Auto-run when sourced in UpdateData.R
# if (exists("gameData")) {
#   elo_result <- update_elo_data(
#     gameData = gameData,
#     initial_elo = 1500,
#     K = 20,
#     home_advantage = 0,
#     d = 400,
#     apply_margin_multiplier = TRUE,
#     recompute_all = FALSE,
#     cache_file = "./scripts/UpdateData/PriorData/eloData.rda",
#     season_factor = 0.6
#   )
#   eloData   <- elo_result$elo_history
#   eloFinals <- elo_result$final_elos
# } else stop("gameData not found; please load gameData before updating Elo data.")
