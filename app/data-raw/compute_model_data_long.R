# app/data-raw/compute_model_data_long.R

# ----------------------------------------
# Assemble final modeling dataset (“model_data_long”)
# ----------------------------------------
#
# This function expects:
#   • game_data_long   : a data frame (one row per team‐game)
#   • elo_data         : Elo feature table
#   • srs_data         : SRS feature table
#   • epa_data         : EPA feature table
#   • scores_data      : Scoring feature table
#   • series_data      : Series conversion feature table
#   • turnover_data    : Turnover feature table
#   • redzone_data     : Redzone feature table
#   • window, span     : numeric parameters for rolling/EWMA
#
# It returns a single data frame, with all engineered features joined
# onto `game_data_long` in the specified order. Nothing is sourced or loaded
# inside this function—the caller must ensure all inputs and helper functions
# are already in the environment.
#
# @param game_data_long  Data frame: one row per team‐game (base for features)
# @param elo_data        Data frame: Elo ratings (wide form)
# @param srs_data        Data frame: SRS ratings (season, week, team, metrics)
# @param epa_data        Data frame: raw EPA metrics (must include `off_` & `def_` prefixes)
# @param scores_data     Data frame: scoring metrics (team‐level)
# @param series_data     Data frame: series conversion rates (team‐level)
# @param turnover_data   Data frame: turnover counts (team‐level)
# @param redzone_data    Data frame: red zone stats (team‐level)
# @param window          Integer: window size for rolling averages (default = 5)
# @param span            Numeric: span parameter for EWMA (default = 5)
# @return A tibble with one row per team‐game and all engineered features.
# @importFrom dplyr left_join
# @export
compute_model_data_long <- function(#game_long_df,
                                    # elo_df,
                                    # srs_df,
                                    # epa_df,
                                    # scores_df,
                                    # series_df,
                                    # turnover_df,
                                    # redzone_df,
                                    window = 5,
                                    span   = 5) {
  
  game_data <- compute_game_data(seasons = all_seasons)
  game_data_long <- compute_game_data_long(game_df = game_data)
  load("app/feature-data/elo_data.rda")      # loads object `elo_data`
  load("app/feature-data/srs_data.rda")      # loads object `srs_data`
  load("app/feature-data/epa_data.rda")      # loads object `epa_data`
  load("app/feature-data/scores_data.rda")   # loads object `scores_data`
  load("app/feature-data/series_data.rda")   # loads object `series_data`
  load("app/feature-data/turnover_data.rda") # loads object `turnover_data`
  load("app/feature-data/redzone_data.rda")  # loads object `redzone_data`
  
  id_cols <- c("game_id", "season", "week", "week_seq", "team", "opponent")
  
  # (A) Join Elo features
  df1 <- process_elo_data(base_df = game_data_long,
                          elo_raw = elo_data)
  
  # (B) Join SRS features
  df2 <- process_srs_data(base_df = df1,
                          srs_raw = srs_data)
  
  # (C) Join EPA features (cumulative, rolling, EWMA, net)
  df3 <- process_epa_data(base_df = df2,
                          epa_raw = epa_data,
                          window  = window,
                          span    = span)
  
  # (D) Join Scoring features
  df4 <- process_scores_data(base_df   = df3,
                             scores_raw = scores_data,
                             window     = window,
                             span       = span)
  
  # (E) Join Series conversion rates
  df5 <- process_series_data(base_df    = df4,
                             series_raw = series_data,
                             window     = window,
                             span       = span)
  
  # (F) Join Turnover features
  df6 <- process_turnover_data(base_df     = df5,
                               turnover_raw = turnover_data,
                               window       = window,
                               span         = span)
  
  # (G) Join Redzone features and return
  model_data_long <- process_redzone_data(base_df     = df6,
                                          redzone_raw = redzone_data,
                                          window      = window,
                                          span        = span)
  
  model_data_long <- model_data_long |>
    filter(!is.na(team_elo_pre))
  return(model_data_long)
}
