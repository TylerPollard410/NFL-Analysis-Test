# app/data-raw/compute_model_data.R

# ----------------------------------------
# Convert the “long” modeling table (one row per team‐game) into a “wide” table
# (one row per game, with home_… and away_… prefixes on every feature).
# ----------------------------------------
#
# This function expects:
#   • model_data_long: a data frame with one row per team‐game (the output of compute_model_data_long())
#                      It must include at least these columns:
#                        – game_id, season, week, week_seq, team, opponent
#                        – plus every feature column (e.g. team_elo_pre, off_total_epa_sum_cum_lag, net_off_scr_roll_lag, etc.).
#   • game_data      : the original game_data (one row per game_id), so that we know which team is home vs. away.
#                      It must include at least: game_id, home_team, away_team.
#
# It returns a tibble with one row per game_id, and all features split into home_<feature> / away_<feature>.
#
# @param model_data_long A data frame (one row per team‐game) containing every “net_…_lag” feature,
#                        plus all other smoothed/lagged columns. Must include columns:
#                          game_id, season, week, week_seq, team, opponent, plus all feature columns.
# @param game_data       A data frame (one row per game) with columns: game_id, home_team, away_team.
# @return A tibble with one row per game_id, and for each original feature X, two new columns:
#         home_X and away_X.
# @importFrom dplyr left_join mutate if_else select all_of
# @importFrom tidyr pivot_wider
# @export
compute_model_data <- function(model_data_long, game_data) {
  # 1) Join model_data_long to game_data to label each row as “home” or “away”
  df <- model_data_long |>
    dplyr::left_join(
      dplyr::select(game_data, game_id, home_team, away_team),
      by = "game_id"
    ) |>
    dplyr::mutate(
      side = ifelse(team == home_team, "home", "away")
    ) |>
    dplyr::select(-dplyr::all_of(c("home_team", "away_team")))
  
  # 2) Identify the ID columns and the metadata columns; everything else is a “feature”
  id_cols       <- c("game_id", "season", "week", "week_seq")
  metadata_cols <- c("opponent", "side")
  feature_cols  <- setdiff(names(df), c(id_cols, metadata_cols))
  
  # 3) Pivot to wide: for each feature in 'feature_cols', create home_<feature> and away_<feature>
  wide <- df |>
    tidyr::pivot_wider(
      id_cols     = id_cols,
      names_from   = side,
      values_from  = feature_cols,
      names_glue = "{side}_{.value}"
    )
  
  # 4) Return the wide table (one row per game_id, with home_/away_ prefixes)
  out_df <- game_data |>
    left_join(wide, by = c("game_id", "season", "week", "week_seq", "home_team", "away_team"))
  return(out_df)
}
