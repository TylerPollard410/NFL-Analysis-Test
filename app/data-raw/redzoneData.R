# redzoneData.R
# Function to compute and combine red-zone efficiency stats per game-team

# Dependencies: dplyr (loaded externally in UpdateData.R)

#' Compute red-zone feature dataset for modeling
#'
#' @param gameDataLong Tibble of long-format game-team rows with columns: game_id, season, week, team, opponent
#' @param pbpData      Play-by-play tibble including: game_id, season, week,
#'                      posteam, home_team, away_team, fixed_drive,
#'                      fixed_drive_result, drive_inside20, drive_ended_with_score
#' @return Tibble with one row per game-team containing:
#'   off_red_zone_app_perc, off_red_zone_eff,
#'   def_red_zone_app_perc, def_red_zone_eff
compute_redzone_data <- function(gameDataLong, pbpData) {
  # uses dplyr
  
  # STEP 1: Summarise raw red-zone metrics per team-game
  redzoneData <- pbpData |>
    filter(!is.na(posteam)) |>
    select(
      game_id, season, week,
      posteam, home_team, away_team,
      fixed_drive, fixed_drive_result, drive_inside20
    ) |>
    distinct() |>
    group_by(game_id, season, week, posteam, home_team, away_team) |>
    reframe(
      drives_num        = n(),
      red_zone_app      = sum(drive_inside20, na.rm = TRUE),
      red_zone_td       = sum(fixed_drive_result == "Touchdown" & drive_inside20, na.rm = TRUE),
      red_zone_app_perc = ifelse(drives_num > 0, red_zone_app / drives_num, 0),
      red_zone_eff      = ifelse(red_zone_app > 0, red_zone_td / red_zone_app, 0)
    )
  
  # STEP 2: Build offense and defense features
  redzone_features <- redzoneData |>
    arrange(game_id, posteam) |>
    group_by(game_id) |>
    mutate(
      opponent = rev(posteam)
    ) |>
    ungroup() |>
    rename(team = posteam) |>
    transmute(
      game_id,
      season,
      week,
      team,
      opponent,
      off_red_zone_app_perc = red_zone_app_perc,
      off_red_zone_eff      = red_zone_eff,
      def_red_zone_app_perc = red_zone_app_perc[match(team, opponent)],
      def_red_zone_eff      = red_zone_eff[match(team, opponent)]
    )
  
  # STEP 3: Merge into gameDataLong ordering
  id_cols <- c("game_id", "season", "week", "team", "opponent")
  redzone_data <- gameDataLong |>
    select(all_of(id_cols)) |>
    left_join(redzone_features, by = id_cols) |>
    mutate(across(starts_with("off_"),  ~ replace_na(.x, 0)),
           across(starts_with("def_"),  ~ replace_na(.x, 0)))
  
  return(redzone_data)
}

# Example usage:
# redzone_data <- compute_redzone_data(gameDataLong, pbpData)
# redzone_cols <- colnames(select(redzone_data, starts_with("off_"), starts_with("def_")))
