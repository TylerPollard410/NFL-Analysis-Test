
#' Compute comprehensive score features for modeling
#'
#' @param game_long_df Tibble of long-format game-team rows including game_id, season, week, team, home_away, team_score, opponent_score
#' @param pbp_df      Play-by-play tibble containing two-point conversion info
#' @param seasons   Integer vector of all seasons
#' @param stats_loc    File path for nflStatsWeek RDA
#' @param recompute_all Logical, if TRUE forces weekly stats recompute
#' @return Tibble with one row per game-team containing efficiency and score features
compute_scores_data <- function(game_long_df = game_data_long,
                                pbp_df = pbp_data,
                                seasons = all_seasons, 
                                sum_level = "week",
                                stat_level = "team",
                                season_level = "REG+POST",
                                stats_loc,
                                recompute_all = FALSE) {
  # uses dplyr
  
  # identifier columns for gameDataLong
  id_cols <- c("game_id", "season", "week", "team")
  
  # STEP 1: Load or generate weekly team stats
  nflStatsWeek <- calc_weekly_team_stats(
    seasons = seasons, 
    sum_level = "week",
    stat_level = "team",
    season_level = "REG+POST",
    stats_loc, 
    recompute_all = FALSE
  )
  
  # STEP 2: Select raw scoring stats
  scoresData <- nflStatsWeek |>
    select(
      season, week, team,
      passing_tds,
      rushing_tds,
      td_special   = special_teams_tds,
      def_tds,
      fumble_recovery_tds,
      passing_2pt_conversions,
      rushing_2pt_conversions,
      pat_att,
      pat_made,
      fg_att,
      fg_made,
      safeties_def = def_safeties
    )
  
  # STEP 3: Fix known data errors
  det_2011_13 <- scoresData$season == 2011 & scoresData$week == 13 & scoresData$team == "DET"
  scoresData$rushing_tds[det_2011_13] <- 1
  scoresData$pat_att[det_2011_13]     <- 2
  scoresData$pat_made[det_2011_13]    <- 2
  
  no_2011_13 <- scoresData$season == 2011 & scoresData$week == 13 & scoresData$team == "NO"
  scoresData$rushing_tds[no_2011_13]  <- 1
  scoresData$passing_tds[no_2011_13]  <- 3
  scoresData$pat_att[no_2011_13]      <- 4
  scoresData$pat_made[no_2011_13]     <- 4
  
  ari_2019_15 <- scoresData$season == 2019 & scoresData$week == 15 & scoresData$team == "ARI"
  scoresData$rushing_tds[ari_2019_15] <- 4
  scoresData$td_special[ari_2019_15]  <- 0
  
  cle_2019_15 <- scoresData$season == 2019 & scoresData$week == 15 & scoresData$team == "CLE"
  scoresData$passing_tds[cle_2019_15] <- 2
  scoresData$pat_att[cle_2019_15]     <- 3
  scoresData$pat_made[cle_2019_15]    <- 3
  
  # STEP 4: Aggregate scores
  scoresDataAgg <- scoresData |>
    mutate(across(-all_of(c("season", "week", "team")), ~ replace_na(.x, 0))) |>
    mutate(
      td_off   = passing_tds + rushing_tds,
      td_def   = def_tds + fumble_recovery_tds,
      td_total = td_off + td_special + td_def
    )
  
  # STEP 5: Two-point conversion data
  scores_two_pt_df <- pbp_df |>
    select(
      season, week, posteam,
      defensive_two_point_conv,
      two_point_attempt, two_point_conv_result
    ) |>
    filter(!is.na(posteam)) |>
    mutate(
      two_pt_att  = two_point_attempt,
      two_pt_made = as.integer(two_point_conv_result == "success"),
      two_pt_def  = defensive_two_point_conv
    ) |>
    group_by(season, week, posteam) |>
    summarise(
      two_pt_att  = sum(two_pt_att, na.rm = TRUE),
      two_pt_made = sum(two_pt_made, na.rm = TRUE),
      two_pt_def  = sum(two_pt_def, na.rm = TRUE),
      .groups     = "drop"
    )
  
  # Join two-point stats
  scoresDataAgg <- scoresDataAgg |>
    left_join(
      scores_two_pt_df,
      by = join_by(season, week, team == posteam)
    )
  
  # STEP 6: Combine with gameDataLong and compute point breakdowns
  scoresFeatures <- game_long_df |>
    filter(!is.na(result)) |>
    select(all_of(id_cols)) |>
    left_join(scoresDataAgg, by = join_by(season, week, team)) |>
    mutate(
      two_pt_def = rev(two_pt_def),
      .by        = c(game_id)
    ) |>
    mutate(across(-all_of(id_cols), ~ replace_na(.x, 0))) |>
    mutate(
      points8      = two_pt_made,
      points7      = pat_made,
      points6      = td_total - two_pt_made - pat_made,
      points3      = fg_made,
      points2      = safeties_def + two_pt_def,
      points_total = points8*8 + points7*7 + points6*6 + points3*3 + points2*2
    )
  
  return(scoresFeatures)
}

# Example usage:
# stats_loc   <- "~/Desktop/NFLAnalysisTest/scripts/UpdateData/PriorData/nflStatsWeek.rda"
# scores_data <- compute_scores_data(gameDataLong, pbpData, allSeasons, stats_loc, recompute_all=TRUE)
