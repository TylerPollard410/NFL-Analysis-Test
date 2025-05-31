# data-raw/helpers/feature_eng.R

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generic Feature-Engineering Helpers
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Add season-to-date cumulative averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to average
#' @param group_vars Character vector of grouping variables (e.g. c("season","team"))
#' @return Data frame with new columns `<col>_cum`
#' @importFrom dplyr group_by arrange mutate across all_of cummean ungroup
#' @export
add_cumulative_avg <- function(df, cols, group_vars, keep = "all") {
  df |> 
    group_by(across(all_of(group_vars))) |> 
    arrange(season, week, game_id, 
            .by_group = TRUE) |> 
    mutate(across(all_of(cols),
                  ~cummean(.x),
                  .names = "{.col}_cum"),
           .keep = keep) |> 
    ungroup()
}

#' Add rolling-window averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to roll
#' @param group_vars Character vector of grouping variables (e.g. "team")
#' @param window Integer window size (default 5)
#' @return Data frame with new columns `<col>_roll`
#' @importFrom dplyr group_by arrange mutate across all_of ungroup
#' @importFrom slider slide_dbl
#' @export
add_rolling_avg <- function(df, cols, group_vars, window = 5, keep = "all") {
  df |> 
    group_by(across(all_of(group_vars))) |> 
    arrange(season, week, game_id, 
            .by_group = TRUE) |> 
    mutate(across(all_of(cols),
                  ~slider::slide_dbl(.x, mean, 
                                     .before = window - 1, 
                                     .complete = FALSE),
                  .names = "{.col}_roll"),
           .keep = keep) |> 
    ungroup()
}

#' Add exponentially-weighted moving averages (no lag)
#'
#' @param df A data frame
#' @param cols Character vector of column names to smooth
#' @param group_vars Character vector of grouping variables (e.g. "team")
#' @param span Numeric span parameter for EWMA (default 5)
#' @return Data frame with new columns `<col>_ewma`
#' @importFrom dplyr group_by arrange mutate across all_of ungroup
#' @importFrom stats filter
#' @export
add_ewma <- function(df, cols, group_vars, span = 5, keep = "all") {
  alpha <- 2 / (span + 1)
  df |> 
    group_by(across(all_of(group_vars))) |> 
    arrange(season, week, game_id, 
            .by_group = TRUE) |> 
    mutate(across(all_of(cols),
                  ~stats::filter(.x, alpha, method = "recursive"),
                  .names = "{.col}_ewma"),
           .keep = keep) |> 
    ungroup()
}

#' Add lag to specified columns
#'
#' @param df A data frame
#' @param cols Character vector of column names to lag
#' @param group_vars Character vector of grouping variables
#' @param n Integer number of periods to lag (default 1)
#' @param suffix Suffix for lagged columns (default "_lag")
#' @return Data frame with new columns `<col>_lag`
#' @importFrom dplyr group_by arrange mutate across all_of lag ungroup
#' @export
add_lag <- function(df, cols, group_vars, n = 1, default = 0, suffix = "_lag", keep = "all") {
  df |> 
    group_by(across(all_of(group_vars))) |> 
    arrange(season, week, game_id,
            .by_group = TRUE) |> 
    mutate(across(all_of(cols),
                  ~lag(.x, n = n, default = default),
                  .names = "{.col}{suffix}"),
           .keep = keep) |> 
    ungroup()
}

#' Add net metrics (offensive minus defensive)
#'
#' @param df A data frame
#' @param off_cols Character vector of offensive column names
#' @param def_cols Character vector of defensive column names
#' @param net_suffix Suffix for the net columns (default "_net")
#' @return Data frame with new columns `<off_col>_net`
#' @export
add_net_metric <- function(df, off_cols, def_cols, net_suffix = "_net") {
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
