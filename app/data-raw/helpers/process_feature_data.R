# data-raw/helpers/process_feature_data.R

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Per-dataset processing functions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Process Elo data and join to base
#'
#' @param base_df Data frame containing base game rows (from game_data_long)
#' @param elo_raw Data frame with Elo ratings (wide form: home_ and away_ prefixes)
#' @return Data frame with Elo features appended to base_df
#' @importFrom dplyr select mutate if_else left_join
#' @importFrom tidyr pivot_longer
#' @export
process_elo_data <- function(base_df, elo_raw) {
  elo_long <- elo_raw |>
    # 1) Pivot all four Elo columns into long form
    tidyr::pivot_longer(
      cols = c(home_elo_pre, away_elo_pre, home_elo_post, away_elo_post),
      names_to  = c("side", "time"),
      names_pattern = "(home|away)_elo_(pre|post)",
      values_to = "elo_value"
    ) |>
    # 2) Spread “time” so that we get separate columns for pre and post
    tidyr::pivot_wider(
      id_cols    = c(game_id, home_team, away_team, side),
      names_from = time,
      values_from = elo_value,
      names_prefix = "team_elo_"
    ) |>
    # 3) Determine which team/opponent the row represents
    dplyr::mutate(
      team     = if_else(side == "home", home_team, away_team),
      opponent = if_else(side == "home", away_team, home_team)
    ) |>
    # 4) Keep only the desired columns (game_id, team, opponent, and both Elo columns)
    dplyr::select(game_id, team, opponent, team_elo_pre, team_elo_post)
  
  elo_long <- dplyr::left_join(
    base_df |> dplyr::select(all_of(id_cols)), 
    elo_long, 
    by = c("game_id", "team", "opponent")
  ) |>
    dplyr::mutate(team_elo_post = lag(team_elo_post, n = 1, default = NA_real_),
                  .by = "team") |>
    dplyr::mutate(team_elo_pre = dplyr::case_when(is.na(team_elo_pre) & !is.na(team_elo_post) & week == 1 ~ 0.6*team_elo_post + 0.4*1500,
                                                  is.na(team_elo_pre) & !is.na(team_elo_post) & week != 1 ~ team_elo_post,
                                                  .default = team_elo_pre)) |>
    dplyr::select(-team_elo_post)
  return(elo_long)
}

#' Process SRS data (lag to be applied externally)
#'
#' @param base_df Data frame containing base game rows
#' @param srs_raw Data frame of SRS data (season, week, team, metrics)
#' @return Data frame with SRS metrics appended to base_df
#' @importFrom dplyr arrange select left_join
#' @export
process_srs_data <- function(base_df, srs_raw) {
  srs_cols <- setdiff(names(srs_raw), c("season", "week", "team"))
  srs_feat <- srs_raw |> 
    add_week_seq() |>
    dplyr::arrange(season, week) |> 
    # no internal lag; use feature_eng::add_lag after if desired
    dplyr::select(season, week, week_seq, team, dplyr::all_of(srs_cols))
  
  dplyr::left_join(base_df, srs_feat, by = c("season", "week", "week_seq", "team"))
}

#' Process EPA data (cumulative, rolling, EWMA, lag, and net vs. opponent)
#'
#' @param base_df Data frame containing base game rows
#' @param epa_raw Data frame of raw EPA metrics (long form with `off_` & `def_` prefixes)
#' @param window Integer window size for rolling means (default = 5)
#' @param span Numeric span parameter for EWMA (default = 5)
#' @return Data frame with EPA features appended to base_df
#' @importFrom dplyr left_join mutate select rename ends_with group_by ungroup
#' @export
process_epa_data <- function(base_df, epa_raw, window = 5, span = 5) {
  # 1) Add week_seq and ensure opponent column is present
  epa_seq <- epa_raw |>
    add_week_seq() |>
    mutate(
      opponent = if (!"opponent" %in% names(.data)) rev(team) else opponent,
      .by = game_id, .after = team
    ) |>
    select(-home_team, -away_team)
  
  # 2) Identify raw EPA columns
  epa_cols <- setdiff(
    names(epa_seq),
    c("game_id", "season", "week", "week_seq", "team", "opponent")
  )
  off_cols <- grep("^off_", epa_cols, value = TRUE)
  def_cols <- grep("^def_", epa_cols, value = TRUE)
  
  # 3) Compute cumulative (season-to-date, resets each season)
  df_cum <- epa_seq |>
    add_cumulative_avg(off_cols, c("season", "team"), keep = "all") |>
    add_cumulative_avg(def_cols, c("season", "team"), keep = "all")
  
  # 4) Compute rolling (last `window` games, carries across seasons)
  df_roll <- df_cum |>
    add_rolling_avg(off_cols, "team", window, keep = "all") |>
    add_rolling_avg(def_cols, "team", window, keep = "all")
  
  # 5) Compute EWMA (carries across seasons)
  df_ewma <- df_roll |>
    add_ewma(off_cols, "team", span, keep = "all") |>
    add_ewma(def_cols, "team", span, keep = "all")
  
  # 6a) Lag cumulative columns (group by season & team, default = 0)
  df_lag0 <- base_df |>
    select(all_of(id_cols)) |>
    left_join(df_ewma)
  
  
  df_lag1 <- df_lag0 |>
    add_lag(
      cols       = paste0(epa_cols, "_cum"),
      group_vars = c("season", "team"),
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "all"
    )
  
  # 6b) Lag rolling columns (group by team only, default = 0)
  df_lag2 <- df_lag1 |>
    add_lag(
      cols       = paste0(epa_cols, "_roll"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "all"
    )
  
  # 6c) Lag EWMA columns (group by team only, default = 0)
  df_lagged <- df_lag2 |>
    add_lag(
      cols       = paste0(epa_cols, "_ewma"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "all"
    )
  
  # 7) Build opponent lookup for defensive lags
  def_lookup <- df_lagged |>
    # select game_id, team, plus any column matching /^def_.*_(cum|roll|ewma)_lag$/
    dplyr::select(
      game_id,
      team,
      #dplyr::matches("^def_total_epa_sum_(cum|roll|ewma)_lag$")
      dplyr::matches("^def_.*_sum_(cum|roll|ewma)_lag$")
    ) |>
    # rename each selected def_* column to opp_<original_name>
    dplyr::rename_with(
      .cols = -c(game_id, team),
      .fn   = ~ paste0("opp_", .x)
    )
  
  # 8) Join opponent defense, compute net
  df_with_net <- #base_df |> select(game_id, season, week, week_seq, team, opponent) |> left_join(df_lagged) |>
    df_lagged |>
    dplyr::left_join(def_lookup, by = c("game_id", "opponent" = "team")) |> 
    dplyr::select(game_id, season, week, week_seq, team, opponent,
                  dplyr::matches("sum_(cum|roll|ewma)_lag$")) |>
    #select(matches("^off_.*_(cum|roll|ewma)_lag$")) |> colnames()
    dplyr::mutate(
      across(
        # 1) Pick every team‐offensive lag column:
        .cols = matches("^off_.*_(cum|roll|ewma)_lag$"),
        
        # 2) The function: .x is off_<type>_lag; we look up the matching opp_def_<type>_lag
        .fns = ~ {
          # cur_column() is something like "off_total_epa_sum_cum_lag"
          off_name <- cur_column()
          # Derive the corresponding opp_def name by:
          # a) Replace the initial "off_" with "def_"
          # b) Then prepend "opp_" to that string
          def_name <- paste0("opp_", sub("^off_", "def_", off_name))
          # Now .x is the team‐off value; get(def_name) is the opponent's def value
          .x + cur_data()[[def_name]]
          #.x + pick(def_name)[[1]]
        },
        
        # 3) Name the new column "net_<original_off_name>"
        .names = "net_{.col}"
      )
    )
  
  # 9) Select features to join back
  epa_features <- df_with_net |>
    dplyr::select(
      -contains("opp_")
    )
  
  result <- base_df |>
    dplyr::left_join(epa_features, by = id_cols)
  
  return(result)
}

#' Process scoring data (cumulative, rolling, EWMA, lag, and net vs. opponent)
#'
#' @param base_df Data frame containing base game rows
#' @param scores_raw Data frame of raw scoring metrics (long form; e.g., points_for, points_against)
#' @param window Integer window size for rolling means (default = 5)
#' @param span Numeric span parameter for EWMA (default = 5)
#' @return Data frame with scoring features appended to base_df
#' @importFrom dplyr left_join mutate select rename ends_with
#' @export
process_scores_data <- function(base_df, scores_raw, window = 5, span = 5) {
  # 1) Add week_seq and ensure opponent column is present
  scores_seq <- scores_raw |>
    add_week_seq() |>
    mutate(
      opponent = if (!"opponent" %in% names(.data)) rev(team) else opponent,
      .by = game_id, .after = team
    ) |>
    select(-any_of(c("home_team", "away_team"))) |>
    rename(
      td_passing = passing_tds,
      td_rushing = rushing_tds
    )
  
  # 2) Identify scoring metric columns
  score_cols <- setdiff(
    names(scores_seq),
    id_cols
  )
  
  score_cols <- c(
    "td_passing", "td_rushing", "td_special", "td_def",
    "fg_made", "fg_att",
    "pat_made", "pat_att",
    "two_pt_made", "two_pt_att",
    "safeties_def"
  )
  
  
  # 3) Smooth: cum, roll, EWMA
  df_cum <- scores_seq |>
    add_cumulative_avg(score_cols, c("season", "team"), keep = "all")
  df_roll <- df_cum |>
    add_rolling_avg(score_cols, "team", window, keep = "all")
  df_ewma <- df_roll |>
    add_ewma(score_cols, "team", span, keep = "all")
  
  # 4) Lag
  df_lag0 <- base_df |>
    select(all_of(id_cols)) |>
    left_join(df_ewma, by = id_cols)
  
  # 4a) Lag cumulative (group by season, team)
  df_lag1 <- df_lag0 |>
    add_lag(
      cols       = paste0(score_cols, "_cum"),
      group_vars = c("season", "team"),
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 4b) Lag rolling (group by team)
  df_lag2 <- df_lag1 |>
    add_lag(
      cols       = paste0(score_cols, "_roll"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3c) Lag EWMA (group by team)
  df_lagged <- df_lag2 |>
    add_lag(
      cols       = paste0(score_cols, "_ewma"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 4) Build opponent lookup for "points_against" lags
  # def_lookup <- df_lagged |>
  #   dplyr::select(
  #     game_id, team,
  #     dplyr::ends_with("points_against_cum_lag"),
  #     dplyr::ends_with("points_against_roll_lag"),
  #     dplyr::ends_with("points_against_ewma_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("points_against_cum_lag",  "opp_points_against_cum_lag",  .x),
  #     ends_with("points_against_cum_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("points_against_roll_lag", "opp_points_against_roll_lag", .x),
  #     ends_with("points_against_roll_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("points_against_ewma_lag", "opp_points_against_ewma_lag", .x),
  #     ends_with("points_against_ewma_lag")
  #   )
  # 
  # # 5) Join opponent, compute net
  # df_with_net <- df_lagged |>
  #   dplyr::left_join(def_lookup, by = c("game_id", "opponent" = "team")) |>
  #   dplyr::mutate(
  #     net_points_for_cum_lag  = points_for_cum_lag  - opp_points_against_cum_lag,
  #     net_points_for_roll_lag = points_for_roll_lag - opp_points_against_roll_lag,
  #     net_points_for_ewma_lag = points_for_ewma_lag - opp_points_against_ewma_lag
  #   )
  
  # 6) Select features to join back
  score_features <- df_lagged |>
    dplyr::select(
      all_of(id_cols),
      contains("points"),
      everything()
    )
  
  result <- base_df |>
    dplyr::left_join(score_features, by = id_cols)
  
  return(result)
}

#' Process series conversion rates (cumulative, rolling, EWMA, lag, and net vs. opponent)
#'
#' @param base_df Data frame containing base game rows
#' @param series_raw Data frame of raw series-conversion metrics (long form)
#' @param window Integer window size for rolling means (default = 5)
#' @param span Numeric span parameter for EWMA (default = 5)
#' @return Data frame with series features appended to base_df
#' @importFrom dplyr left_join mutate select rename ends_with
#' @export
process_series_data <- function(base_df, series_raw, window = 5, span = 5) {
  # 1) Add week_seq and ensure opponent column is present
  series_seq <- series_raw |>
    add_week_seq() |>
    mutate(
      opponent = if (!"opponent" %in% names(.data)) rev(team) else opponent,
      .by = game_id, .after = team
    ) |>
    select(-any_of(c("home_team", "away_team")))
  
  # 1) Identify series metric columns
  series_cols <- setdiff(
    names(series_raw),
    id_cols
  )
  
  off_cols <- grep("^off_", series_cols, value = TRUE)
  def_cols <- grep("^def_", series_cols, value = TRUE)
  
  # 3) Compute cumulative (season-to-date, resets each season)
  df_cum <- series_seq |>
    add_cumulative_avg(off_cols, c("season", "team"), keep = "all") |>
    add_cumulative_avg(def_cols, c("season", "team"), keep = "all")
  
  # 4) Compute rolling (last `window` games, carries across seasons)
  df_roll <- df_cum |>
    add_rolling_avg(off_cols, "team", window, keep = "all") |>
    add_rolling_avg(def_cols, "team", window, keep = "all")
  
  # 5) Compute EWMA (carries across seasons)
  df_ewma <- df_roll |>
    add_ewma(off_cols, "team", span, keep = "all") |>
    add_ewma(def_cols, "team", span, keep = "all")
  
  
  # 3a) Lag cumulative (group by season, team)
  df_lag0 <- base_df |>
    select(all_of(id_cols)) |>
    left_join(df_ewma, by = id_cols)
  
  df_lag1 <- df_lag0 |>
    add_lag(
      cols       = paste0(series_cols, "_cum"),
      group_vars = c("team"),
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3b) Lag rolling (group by team)
  df_lag2 <- df_lag1 |>
    add_lag(
      cols       = paste0(series_cols, "_roll"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3c) Lag EWMA (group by team)
  df_lagged <- df_lag2 |>
    add_lag(
      cols       = paste0(series_cols, "_ewma"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 4) Build opponent lookup for defensive series
  def_lookup <- df_lagged |>
    # select game_id, team, plus any column matching /^def_.*_(cum|roll|ewma)_lag$/
    dplyr::select(
      game_id, team,
      dplyr::matches("^def_.*_lag$")
    ) |>
    # rename each selected def_* column to opp_<original_name>
    dplyr::rename_with(
      .cols = -c(game_id, team),
      .fn   = ~ paste0("opp_", .x)
    )
  
  # 8) Join opponent defense, compute net
  df_with_net <- #base_df |> select(game_id, season, week, week_seq, team, opponent) |> left_join(df_lagged) |>
    df_lagged |>
    dplyr::left_join(def_lookup, by = c("game_id", "opponent" = "team")) |> 
    dplyr::select(all_of(id_cols),
                  dplyr::contains("_lag")) |>
    mutate(
      across(
        # pick every “off_*_(cum|roll|ewma)_lag” column
        .cols = matches("^off_.*_(cum|roll|ewma)_lag$"),
        
        # for each, build the matching opp_def name and combine appropriately
        .fns = ~ {
          off_name <- cur_column()                              
          # Build the opponent‐defensive name:
          def_name <- paste0("opp_", sub("^off_", "def_", off_name))
          
          if (grepl("^off_to_.*_lag$", off_name)) {
            # turnover metric: both off_to and opp_def_to are bad → make net negative
            - ( .x + cur_data()[[def_name]] )
          } else {
            # any other series metric (scr, scr_1st, scr_2nd, 1st, td, fg, punt, etc.)
            # is “good when high” for both off_… and opp_def_… → add them
            .x + cur_data()[[def_name]]
          }
        },
        
        # output column names “net_off_<whatever>_<type>_lag”
        .names = "net_{.col}"
      )
    )
  
  # 9) Select features to join back
  series_features <- df_with_net |>
    dplyr::select(
      all_of(id_cols),                      
      dplyr::matches("^off_.*_lag$"),        
      dplyr::matches("^def_.*_lag$"),        
      dplyr::starts_with("net_off_")         
    )
  
  result <- base_df |>
    dplyr::left_join(series_features, by = id_cols)
  
  return(result)
}

#' Process turnover data (cumulative, rolling, EWMA, lag, and net vs. opponent)
#'
#' @param base_df Data frame containing base game rows
#' @param turnover_raw Data frame of raw turnover metrics (long form)
#' @param window Integer window size for rolling means (default = 5)
#' @param span Numeric span parameter for EWMA (default = 5)
#' @return Data frame with turnover features appended to base_df
#' @importFrom dplyr left_join mutate select rename ends_with
#' @export
process_turnover_data <- function(base_df, turnover_raw, window = 5, span = 5) {
  # 1) Add week_seq and ensure opponent column is present
  turnover_seq <- turnover_raw |>
    add_week_seq() |>
    mutate(
      opponent = if (!"opponent" %in% names(.data)) rev(team) else opponent,
      .by = game_id, .after = team
    ) |>
    select(-any_of(c("home_team", "away_team")))
  
  # 1) Identify turnover metric columns
  turnover_cols <- setdiff(
    names(turnover_seq),
    id_cols
  )
  
  # 2) Smooth: cum, roll, EWMA
  df_cum <- turnover_seq |>
    add_cumulative_avg(turnover_cols, c("season", "team"), keep = "all")
  df_roll <- df_cum |>
    add_rolling_avg(turnover_cols, "team", window, keep = "all")
  df_ewma <- df_roll |>
    add_ewma(turnover_cols, "team", span, keep = "all")
  
  # 3a) Lag cumulative (group by season, team)
  df_lag0 <- base_df |>
    select(all_of(id_cols)) |>
    left_join(df_ewma, by = id_cols)
  
  df_lag1 <- df_ewma |>
    add_lag(
      cols       = paste0(turnover_cols, "_cum"),
      group_vars = c("season", "team"),
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3b) Lag rolling (group by team)
  df_lag2 <- df_lag1 |>
    add_lag(
      cols       = paste0(turnover_cols, "_roll"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3c) Lag EWMA (group by team)
  df_lagged <- df_lag2 |>
    add_lag(
      cols       = paste0(turnover_cols, "_ewma"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 4) Build opponent lookup for "turnovers_for" lags
  # def_lookup <- df_lagged |>
  #   dplyr::select(
  #     game_id, team,
  #     dplyr::ends_with("turnovers_for_cum_lag"),
  #     dplyr::ends_with("turnovers_for_roll_lag"),
  #     dplyr::ends_with("turnovers_for_ewma_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("turnovers_for_cum_lag",  "opp_turnovers_for_cum_lag",  .x),
  #     ends_with("turnovers_for_cum_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("turnovers_for_roll_lag", "opp_turnovers_for_roll_lag", .x),
  #     ends_with("turnovers_for_roll_lag")
  #   ) |>
  #   dplyr::rename_with(
  #     ~gsub("turnovers_for_ewma_lag", "opp_turnovers_for_ewma_lag", .x),
  #     ends_with("turnovers_for_ewma_lag")
  #   )
  
  # 5) Join opponent, compute net
  df_with_net <- df_lagged |>
    dplyr::mutate(
      # (a) Net turnover: use the cumulative, rolling, and EWMA‐lag versions
      net_turnover_cum_lag  = .data$turnover_won_cum_lag  - .data$turnover_lost_cum_lag,
      net_turnover_roll_lag = .data$turnover_won_roll_lag - .data$turnover_lost_roll_lag,
      net_turnover_ewma_lag = .data$turnover_won_ewma_lag - .data$turnover_lost_ewma_lag,
      
      # (b) Net interception: forced minus thrown
      net_interception_cum_lag  = .data$interception_won_cum_lag  - .data$interception_lost_cum_lag,
      net_interception_roll_lag = .data$interception_won_roll_lag - .data$interception_lost_roll_lag,
      net_interception_ewma_lag = .data$interception_won_ewma_lag - .data$interception_lost_ewma_lag,
      
      # (c) Net fumble: forced minus lost
      net_fumble_cum_lag  = .data$fumble_won_cum_lag  - .data$fumble_lost_cum_lag,
      net_fumble_roll_lag = .data$fumble_won_roll_lag - .data$fumble_lost_roll_lag,
      net_fumble_ewma_lag = .data$fumble_won_ewma_lag - .data$fumble_lost_ewma_lag
    )
  
  # 6) Select features to join back
  turnover_features <- df_with_net |>
    dplyr::select(
      all_of(id_cols),
      contains("lag")
    )
  
  result <- base_df |>
    dplyr::left_join(turnover_features, by = id_cols)
  
  return(result)
}

#' Process red zone data (cumulative, rolling, EWMA, lag, and net vs. opponent)
#'
#' @param base_df Data frame containing base game rows
#' @param redzone_raw Data frame of raw red zone metrics (long form)
#' @param window Integer window size for rolling means (default = 5)
#' @param span Numeric span parameter for EWMA (default = 5)
#' @return Data frame with red zone features appended to base_df
#' @importFrom dplyr left_join mutate select rename ends_with
#' @export
process_redzone_data <- function(base_df, redzone_raw, window = 5, span = 5) {
  # 1) Add week_seq and ensure opponent column is present
  redzone_seq <- redzone_raw |>
    add_week_seq() |>
    mutate(
      opponent = if (!"opponent" %in% names(.data)) rev(team) else opponent,
      .by = game_id, .after = team
    ) |>
    select(-any_of(c("home_team", "away_team"))) |>
    dplyr::rename_with(~str_replace(.x, "red_zone", "redzone"), everything())
  
  # 1) Identify redzone metric columns
  redzone_cols <- setdiff(
    names(redzone_seq),
    id_cols
  )
  
  # 2) Smooth: cum, roll, EWMA
  df_cum <- redzone_seq |>
    add_cumulative_avg(redzone_cols, c("season", "team"), keep = "all")
  df_roll <- df_cum |>
    add_rolling_avg(redzone_cols, "team", window, keep = "all")
  df_ewma <- df_roll |>
    add_ewma(redzone_cols, "team", span, keep = "all")
  
  # 3a) Lag cumulative (group by season, team)
  df_lag0 <- base_df |>
    select(all_of(id_cols)) |>
    left_join(df_ewma, by = id_cols)
  
  df_lag1 <- df_ewma |>
    add_lag(
      cols       = paste0(redzone_cols, "_cum"),
      group_vars = c("team"),
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3b) Lag rolling (group by team)
  df_lag2 <- df_lag1 |>
    add_lag(
      cols       = paste0(redzone_cols, "_roll"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 3c) Lag EWMA (group by team)
  df_lagged <- df_lag2 |>
    add_lag(
      cols       = paste0(redzone_cols, "_ewma"),
      group_vars = "team",
      n          = 1,
      default    = 0,
      suffix     = "_lag",
      keep       = "unused"
    )
  
  # 7) Build opponent‐defense lookup for def_red_zone_*_lag columns
  def_lookup <- df_lagged |>
    dplyr::select(
      game_id, team,
      dplyr::matches("^def_redzone_.*_(cum|roll|ewma)_lag$")
    ) |>
    dplyr::rename_with(
      .cols = -c(game_id, team),
      .fn   = ~ paste0("opp_", .x)
    )
  
  # 8) Join opponent defense, then compute net red‐zone features:
  df_with_net <- df_lagged |>
    dplyr::left_join(def_lookup, by = c("game_id", "opponent" = "team")) |>
    dplyr::mutate(
      # Net redzone_app_perc: offense rate minus opponent’s allowed rate
      net_redzone_app_perc_cum_lag  = .data$off_redzone_app_perc_cum_lag  - cur_data()[["opp_def_redzone_app_perc_cum_lag"]],
      net_redzone_app_perc_roll_lag = .data$off_redzone_app_perc_roll_lag - cur_data()[["opp_def_redzone_app_perc_roll_lag"]],
      net_redzone_app_perc_ewma_lag = .data$off_redzone_app_perc_ewma_lag - cur_data()[["opp_def_redzone_app_perc_ewma_lag"]],
      
      # Net redzone_eff: offense efficiency minus opponent’s allowed efficiency
      net_redzone_eff_cum_lag  = .data$off_redzone_eff_cum_lag  - cur_data()[["opp_def_redzone_eff_cum_lag"]],
      net_redzone_eff_roll_lag = .data$off_redzone_eff_roll_lag - cur_data()[["opp_def_redzone_eff_roll_lag"]],
      net_redzone_eff_ewma_lag = .data$off_redzone_eff_ewma_lag - cur_data()[["opp_def_redzone_eff_ewma_lag"]]
    )
  
  # 9) Select exactly the columns we want to join back to base_df
  redzone_features <- df_with_net |>
    dplyr::select(
      all_of(id_cols),                      
      dplyr::matches("^off_.*_lag$"),        
      dplyr::matches("^def_.*_lag$"),        
      dplyr::starts_with("net_off_") 
    )
  
  # 10) Join back to base_df by (game_id, team)
  result <- base_df |>
    dplyr::left_join(redzone_features, by = id_cols)
  
  return(result)
}
