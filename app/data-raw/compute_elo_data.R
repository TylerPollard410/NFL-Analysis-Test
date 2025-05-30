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
    initial_elo            = 1500,
    K                      = 20,
    home_advantage         = 65,
    d                      = 400,
    apply_margin_multiplier = TRUE,
    recompute_all          = FALSE,
    cache_file             = "./app/data/elo_data.rda",
    season_factor          = 0.6
) {
  library(dplyr)
  
  game_df <- game_df %>%
    arrange(season, week, as.Date(gameday), game_id)
  
  if (recompute_all) {
    message("Full Elo recompute requested...")
    do_inc <- FALSE
  } else if (!file.exists(cache_file)) {
    message("No cache found; performing full recompute...")
    do_inc <- FALSE
  } else {
    message("Loading cached Elo data from ", cache_file)
    do_inc <- TRUE
  }
  
  if (!do_inc) {
    # full recompute (unchanged)...
    elo_hist  <- tibble()
    teams_all <- unique(c(game_df$home_team, game_df$away_team))
    ratings   <- setNames(rep(initial_elo, length(teams_all)), teams_all)
    for (ss in sort(unique(game_df$season))) {
      message("Season start: ", ss)
      games_ss <- game_df %>% filter(season == ss)
      if (ss != min(game_df$season)) {
        m       <- mean(ratings, na.rm = TRUE)
        ratings <- ratings * season_factor + m * (1 - season_factor)
      }
      res      <- calc_elo_ratings(
        games                  = games_ss,
        initial_elo            = ratings,
        K                      = K,
        home_advantage         = home_advantage,
        d                      = d,
        apply_margin_multiplier = apply_margin_multiplier
      )
      # same update for full recompute
      ratings  <- res$final_ratings
      elo_hist <- bind_rows(elo_hist, res$elo_history)
    }
    return(elo_hist)
  }
  
  # incremental update
  load(cache_file)    # loads elo_data
  elo_hist <- elo_data
  
  last_season <- max(elo_hist$season)
  last_week   <- max(filter(elo_hist, season == last_season)$week)
  message("Cache covers up through Season ", last_season, " Week ", last_week)
  
  remainder <- game_df %>%
    filter(season == last_season, week > last_week)
  future    <- game_df %>%
    filter(season > last_season)
  new_gms   <- bind_rows(remainder, future) %>%
    arrange(season, week, as.Date(gameday), game_id)
  
  if (nrow(new_gms) == 0) {
    message("No new games to update Elo for.")
    return(elo_hist)
  }
  
  # seed from end-of-week snapshot
  finals <- elo_hist %>%
    filter(season < last_season |
             (season == last_season & week <= last_week)) %>%
    transmute(season, week, team = home_team, elo = home_elo_post) %>%
    bind_rows(
      elo_hist %>%
        filter(season < last_season |
                 (season == last_season & week <= last_week)) %>%
        transmute(season, week, team = away_team, elo = away_elo_post)
    ) %>%
    arrange(season, week) %>%
    group_by(team) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  all_teams  <- unique(c(game_df$home_team, game_df$away_team))
  mean_final <- mean(finals$elo, na.rm = TRUE)
  ratings    <- setNames(
    sapply(all_teams, function(tm) {
      x <- finals$elo[finals$team == tm]
      if (length(x) == 0) mean_final else x
    }),
    all_teams
  )
  message("Seeded ", length(ratings),
          " teams from up through Season ", last_season,
          " Week ", last_week,
          "; mean Elo = ", round(mean(ratings),3))
  
  # now the one-line tweak: update only players who actually played
  elo_out        <- elo_hist
  current_season <- last_season
  for (ss in sort(unique(new_gms$season))) {
    if (ss != current_season) {
      message("Rolling over into Season ", ss)
      m       <- mean(ratings, na.rm = TRUE)
      ratings <- ratings * season_factor + m * (1 - season_factor)
      current_season <- ss
    }
    message("Computing Elo for Season ", ss,
            if (ss == last_season) " (remainder)" else " (new)")
    games_ss <- new_gms %>% filter(season == ss)
    
    res <- calc_elo_ratings(
      games                  = games_ss,
      initial_elo            = ratings,
      K                      = K,
      home_advantage         = home_advantage,
      d                      = d,
      apply_margin_multiplier = apply_margin_multiplier
    )
    # <— update only the teams who played this batch
    ratings[names(res$final_ratings)] <- res$final_ratings
    
    elo_out <- bind_rows(elo_out, res$elo_history)
  }
  
  elo_out
}