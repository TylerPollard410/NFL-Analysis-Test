#' Wrapper to compute and cache ELO history across seasons
#'
#' @param game_df Data frame of all games with required columns
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
compute_elo_data <- function(
    game_df,
    initial_elo = 1500,
    K = 20,
    home_advantage = 0,
    d = 400,
    apply_margin_multiplier = TRUE,
    recompute_all = FALSE,
    cache_file = "./scripts/UpdateData/PriorData/elo_data.rda",
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
      season_games_prev <- game_df |> filter(season == last_season,
                                              !is.na(home_score), !is.na(away_score))
      prev_res <- calc_elo_ratings(
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
  seasons_to_run <- if (recompute_all || nrow(elo_prev) == 0) sort(unique(game_df$season))
  else get_current_season()
  
  all_history <- list()
  for (season_i in seasons_to_run) {
    season_games <- game_df |> filter(season == season_i,
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
    res <- calc_elo_ratings(
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