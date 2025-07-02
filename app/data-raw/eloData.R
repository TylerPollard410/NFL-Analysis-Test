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
compute_elo_ratings <- function(games,
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

#' Wrapper to compute and cache ELO history across seasons
#'
#' @param gameData Data frame of all games with required columns
#' @param initial_elo Numeric or named vector of starting ratings
#' @param K Numeric learning rate
#' @param home_advantage Numeric home-field advantage
#' @param d Numeric scaling factor
#' @param apply_margin_multiplier Logical; use margin multiplier
#' @param recompute_all Logical; if TRUE, recompute all seasons, else incremental (default FALSE)
#' @param cache_file File path to save/load cached ELO history
#' @param season_factor Numeric between 0 and 1 controlling rollover of ratings (default 0)
#' @return A list with elements:
#'   \item{elo_history}{Combined ELO history data frame}
#'   \item{final_elos}{Named vector or tibble of final season ratings}
update_elo_data <- function(
    gameData,
    initial_elo = 1500,
    K = 20,
    home_advantage = 0,
    d = 400,
    apply_margin_multiplier = TRUE,
    recompute_all = FALSE,
    cache_file = "./scripts/UpdateData/PriorData/eloData.rda",
    season_factor = 0
) {
  # Load existing cache if available
  if (!recompute_all && file.exists(cache_file)) {
    message("Loading cached Elo data from ", cache_file)
    load(cache_file)  # loads 'eloData'
    elo_prev <- elo_data |> filter(season != get_current_season())
    # Prepare last season's final ratings if rolling over
    if (season_factor > 0 && nrow(elo_prev) > 0) {
      last_season <- max(elo_prev$season, na.rm = TRUE)
      season_games_prev <- gameData |> filter(season == last_season,
                                              !is.na(home_score), !is.na(away_score))
      prev_res <- compute_elo_ratings(
        games = season_games_prev,
        initial_elo = initial_elo,
        K = K,
        home_advantage = home_advantage,
        d = d,
        apply_margin_multiplier = apply_margin_multiplier
      )
      prev_final <- tibble(team = names(prev_res$final_ratings), rating = prev_res$final_ratings)
    } else {
      prev_final <- NULL
    }
  } else {
    if (file.exists(cache_file)) message("Recomputing Elo data for all seasons...")
    else message("No cache found; computing Elo data for all seasons...")
    elo_prev <- tibble()
    prev_final <- NULL
  }
  
  # Seasons to compute
  seasons_to_run <- if (recompute_all || nrow(elo_prev) == 0) sort(unique(gameData$season))
  else get_current_season()
  
  all_history <- list()
  for (season_i in seasons_to_run) {
    season_games <- gameData |> filter(season == season_i,
                                       !is.na(home_score), !is.na(away_score))
    # Determine initial ratings
    if (is.null(prev_final) || season_factor == 0) {
      init_elo_param <- initial_elo
    } else {
      mean_prev <- mean(prev_final$rating, na.rm = TRUE)
      init_vec <- prev_final$rating * season_factor + mean_prev * (1 - season_factor)
      names(init_vec) <- prev_final$team
      init_elo_param <- init_vec
    }
    # Compute season ELO
    res <- compute_elo_ratings(
      games = season_games,
      initial_elo = init_elo_param,
      K = K,
      home_advantage = home_advantage,
      d = d,
      apply_margin_multiplier = apply_margin_multiplier
    )
    all_history[[as.character(season_i)]] <- res$elo_history
    
    # Update prev_final for next season rollover
    if (nrow(res$team_ratings) > 0) {
      final_week <- max(res$team_ratings$week, na.rm = TRUE)
      prev_final <- res$team_ratings |> filter(.data$week == final_week) |> transmute(team, rating = .data$elo_ratings)
    }
  }
  
  # Combine history and cache
  eloData <- bind_rows(elo_prev, bind_rows(all_history))
  eloFinals <- prev_final
  
  # Return both
  list(
    elo_history = eloData,
    final_elos = eloFinals
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
