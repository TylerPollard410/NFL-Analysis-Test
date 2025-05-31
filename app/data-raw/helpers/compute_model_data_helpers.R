# data-raw/helpers/compute_model_data_helpers.R

# ----------------------------------------
# Utility feature-engineering functions
# ----------------------------------------

#' Add season-to-date cumulative averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to average
#' @param group_vars Character vector of grouping variables (e.g. c("season","team"))
#' @return Data frame with new columns `<col>_cum`
compute_model_data_helpers::add_cumulative_avg <- function(df, cols, group_vars) {
  df |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |> 
    dplyr::arrange(season, week, game_id, .by_group = TRUE) |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(cols),
                                ~dplyr::cummean(.x),
                                .names = "{.col}_cum")) |> 
    dplyr::ungroup()
}

#' Add rolling-window averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to roll
#' @param group_vars Character vector of grouping variables (e.g. "team")
#' @param window Integer window size (default 5)
#' @return Data frame with new columns `<col>_roll`
compute_model_data_helpers::add_rolling_avg <- function(df, cols, group_vars, window = 5) {
  df |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |> 
    dplyr::arrange(season, week, game_id, .by_group = TRUE) |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(cols),
                                ~slider::slide_dbl(.x, mean, .before = window - 1, .complete = FALSE),
                                .names = "{.col}_roll")) |> 
    dplyr::ungroup()
}

#' Add exponentially-weighted moving averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to smooth
#' @param group_vars Character vector of grouping variables (e.g. "team")
#' @param span Numeric span parameter for EWMA (default 5)
#' @return Data frame with new columns `<col>_ewma`
compute_model_data_helpers::add_ewma <- function(df, cols, group_vars, span = 5) {
  alpha <- 2 / (span + 1)
  df |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |> 
    dplyr::arrange(season, week, game_id, .by_group = TRUE) |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(cols),
                                ~stats::filter(.x, alpha, method = "recursive"),
                                .names = "{.col}_ewma")) |> 
    dplyr::ungroup()
}

#' Add lag to specified columns
#'
#' @param df A data frame
#' @param cols Character vector of column names to lag
#' @param group_vars Character vector of grouping variables
#' @param n Integer number of periods to lag (default 1)
#' @param suffix Suffix for lagged columns (default "_lag")
#' @return Data frame with new columns `<col>_lag`
compute_model_data_helpers::add_lag <- function(df, cols, group_vars, n = 1, suffix = "_lag") {
  df |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |> 
    dplyr::arrange(season, week, game_id, .by_group = TRUE) |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(cols),
                                ~dplyr::lag(.x, n = n),
                                .names = "{.col}{suffix}")) |> 
    dplyr::ungroup()
}

#' Add net metrics (offensive minus defensive)
#'
#' @param df A data frame
#' @param off_cols Character vector of offensive column names
#' @param def_cols Character vector of defensive column names
#' @param net_suffix Suffix for the net columns (default "_net")
#' @return Data frame with new columns `<off_col>_net`
compute_model_data_helpers::add_net_metric <- function(df, off_cols, def_cols, net_suffix = "_net") {
  if (length(off_cols) != length(def_cols)) {
    stop("off_cols and def_cols must be the same length")
  }
  for (i in seq_along(off_cols)) {
    off <- off_cols[i]
    def <- def_cols[i]
    net_name <- paste0(off, net_suffix)
    df[[net_name]] <- df[[off]] - df[[def]]
  }
  df
}

# ----------------------------------------
# Per-dataset processing functions
# ----------------------------------------

#' Process Elo data and join to base
compute_model_data_helpers::process_elo_data <- function(base_df, elo_raw) {
  elo_long <- elo_raw |> 
    dplyr::select(game_id, home_team, away_team, home_elo_pre, away_elo_pre) |> 
    tidyr::pivot_longer(c(home_elo_pre, away_elo_pre),
                        names_to = "side", values_to = "team_elo_pre") |> 
    dplyr::mutate(team = dplyr::if_else(side == "home_elo_pre", home_team, away_team),
                  opponent = dplyr::if_else(side == "home_elo_pre", away_team, home_team)) |> 
    dplyr::select(game_id, team, opponent, team_elo_pre)
  
  dplyr::left_join(base_df, elo_long, by = c("game_id", "team", "opponent"))
}

#' Process SRS data (lag to be applied externally)
compute_model_data_helpers::process_srs_data <- function(base_df, srs_raw) {
  srs_cols <- setdiff(names(srs_raw), c("season", "week", "team"))
  srs_feat <- srs_raw |> 
    dplyr::arrange(season, week) |> 
    # no internal lag; use add_lag after if desired
    dplyr::select(season, week, team, all_of(srs_cols))
  
  dplyr::left_join(base_df, srs_feat, by = c("season", "week", "team"))
}

#' Process EPA data (cumulative, rolling, EWMA, net; lag to be applied externally)
compute_model_data_helpers::process_epa_data <- function(base_df, epa_raw, window = 5, span = 5) {
  epa_cols <- setdiff(names(epa_raw), c("game_id", "season", "week", "team", "opponent"))
  off_cols <- grep("^off_", epa_cols, value = TRUE)
  def_cols <- grep("^def_", epa_cols, value = TRUE)
  
  df <- epa_raw |
    compute_model_data_helpers::add_cumulative_avg(off_cols, c("season","team")) |
    compute_model_data_helpers::add_cumulative_avg(def_cols, c("season","team")) |
    compute_model_data_helpers::add_rolling_avg(off_cols, "team", window) |
    compute_model_data_helpers::add_rolling_avg(def_cols, "team", window) |
    compute_model_data_helpers::add_ewma(off_cols, "team", span) |
    compute_model_data_helpers::add_ewma(def_cols, "team", span)
  
  roll_off <- paste0(off_cols, "_roll")
  roll_def <- paste0(def_cols, "_roll")
  df <- compute_model_data_helpers::add_net_metric(df, roll_off, roll_def)
  
  dplyr::left_join(base_df, df, by = c("game_id", "team"))
}

#' Process scoring data (lag to be applied externally)
compute_model_data_helpers::process_scores_data <- function(base_df, scores_raw, window = 5, span = 5) {
  score_cols <- setdiff(names(scores_raw), c("season", "week", "game_id", "team", "opponent"))
  
  df <- scores_raw |
    compute_model_data_helpers::add_cumulative_avg(score_cols, c("season","team")) |
    compute_model_data_helpers::add_rolling_avg(score_cols, "team", window) |
    compute_model_data_helpers::add_ewma(score_cols, "team", span)
  
  dplyr::left_join(base_df, df, by = c("game_id", "team"))
}

#' Process series conversion rates (lag to be applied externally)
compute_model_data_helpers::process_series_data <- function(base_df, series_raw, window = 5, span = 5) {
  series_cols <- setdiff(names(series_raw), c("season", "week", "game_id", "team", "opponent"))
  
  df <- series_raw |
    compute_model_data_helpers::add_cumulative_avg(series_cols, c("season","team")) |
    compute_model_data_helpers::add_rolling_avg(series_cols, "team", window) |
    compute_model_data_helpers::add_ewma(series_cols, "team", span)
  
  dplyr::left_join(base_df, df, by = c("game_id", "team"))
}

#' Process turnover data (lag to be applied externally)
compute_model_data_helpers::process_turnover_data <- function(base_df, turnover_raw, window = 5, span = 5) {
  turnover_cols <- setdiff(names(turnover_raw), c("season", "week", "game_id", "team", "opponent"))
  
  df <- turnover_raw |
    compute_model_data_helpers::add_cumulative_avg(turnover_cols, c("season","team")) |
    compute_model_data_helpers::add_rolling_avg(turnover_cols, "team", window) |
    compute_model_data_helpers::add_ewma(turnover_cols, "team", span)
  
  dplyr::left_join(base_df, df, by = c("game_id", "team"))
}

#' Process red zone data (lag to be applied externally)
compute_model_data_helpers::process_redzone_data <- function(base_df, redzone_raw, window = 5, span = 5) {
  rz_cols <- setdiff(names(redzone_raw), c("season", "week", "game_id", "team", "opponent"))
  
  df <- redzone_raw |
    compute_model_data_helpers::add_cumulative_avg(rz_cols, c("season","team")) |
    compute_model_data_helpers::add_rolling_avg(rz_cols, "team", window) |
    compute_model_data_helpers::add_ewma(rz_cols, "team", span)
  
  dplyr::left_join(base_df, df, by = c("game_id", "team"))
}

# ----------------------------------------
# Master function to assemble all features
# ----------------------------------------

#' Combine all processed features into one long-format data frame
compute_model_data_helpers::compute_model_data_long <- function(base_df,
                                                                elo_raw, srs_raw, epa_raw,
                                                                scores_raw, series_raw,
                                                                turnover_raw, redzone_raw,
                                                                window = 5, span = 5) {
  base_df |
    compute_model_data_helpers::process_elo_data(elo_raw) |
    compute_model_data_helpers::process_srs_data(srs_raw) |
    compute_model_data_helpers::process_epa_data(epa_raw, window, span) |
    compute_model_data_helpers::process_scores_data(scores_raw, window, span) |
    compute_model_data_helpers::process_series_data(series_raw, window, span) |
    compute_model_data_helpers::process_turnover_data(turnover_raw, window, span) |
    compute_model_data_helpers::process_redzone_data(redzone_raw, window, span)
}


supportR::github_tree("github.com/TylerPollard410/NFL-Analysis-Test", 
                      exclude = c("Model Fitting", 
                                  "scripts", 
                                  "data/modelFits", 
                                  ".DS_Store", 
                                  ".Rbuildignore",
                                  "README.md", 
                                  "app/rsconnect"))
