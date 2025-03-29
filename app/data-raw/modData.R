## Model home and away score

# Load Libraries ----
## Data Manipulation
require(stringr)

## Tables
# library(DBI)
# library(RPostgres)
# library(data.table)

## Plotting
# library(smplot2)
# library(patchwork)

## Modeling
# library(zoo)
require(pracma)
# library(forecast)
require(timetk)
# library(elo)
# library(MASS)
# library(bestNormalize)
# library(tictoc)
# library(caret)
# library(splines)
# library(mgcv)
# library(DescTools)
# library(car)
# library(bayesplot)
# library(BayesFactor)
# library(cmdstanr)
# library(rstanarm)
# library(tidybayes)
# library(loo)
# library(brms)
# library(performance)

## NFL Verse
require(nflverse)

## Tidyverse
require(tidyverse)

#source("./app/data-raw/gameData.R")
#source("./app/data-raw/gameDataLong.R")
#source("./app/data-raw/pbpData.R")

# Aggregate pbp ----
## EPA ----
### Penalty Structure 1 ----
# epaOffData <- pbpData |>
#   filter(season %in% seasonsMod) |>
#   #filter(play == 1) |> 
#   filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
#   group_by(game_id, season, week, posteam, home_team, away_team) |>
#   mutate(
#     scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
#   ) |>
#   summarise(
#     # Total EPA
#     # off_plays = sum(play == 1),
#     # off_epa_sum = sum(epa[play == 1], na.rm = TRUE),
#     # off_epa_mean = mean(epa[play == 1], na.rm = TRUE),
#     off_plays = n(),
#     off_epa_sum = sum(epa, na.rm = TRUE),
#     off_epa_mean = mean(epa, na.rm = TRUE),
#     # Passing EPA
#     off_pass_plays = sum(play == 1 & pass == 1),
#     off_pass_epa_sum = sum(epa[play == 1 & pass == 1], na.rm = TRUE),
#     off_pass_epa_mean = mean(epa[play == 1 & pass == 1], na.rm = TRUE),
#     # Rushing EPA
#     off_rush_plays = sum(play == 1 & rush == 1),
#     off_rush_epa_sum = sum(epa[play == 1 & rush == 1], na.rm = TRUE),
#     off_rush_epa_mean = mean(epa[play == 1 & rush == 1], na.rm = TRUE),
#     # Pre Snap Penalty EPA
#     off_penalty_plays = sum(play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1, na.rm = TRUE),
#     off_penalty_epa_sum = sum(epa[play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
#     off_penalty_epa_mean = mean(epa[play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
#     # Kicker EPA
#     off_kick_plays = sum(play_type %in% c("field_goal"), na.rm = TRUE),
#     off_kick_epa_sum = sum(epa[play_type %in% c("field_goal")], na.rm = TRUE),
#     off_kick_epa_mean = mean(epa[play_type %in% c("field_goal")], na.rm = TRUE),
#     # Special Teams EPA
#     off_special_plays = sum(special == 1 & play_type != "field_goal", na.rm = TRUE),
#     off_special_epa_sum = sum(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
#     off_special_epa_mean = mean(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
#   ) |>
#   mutate(
#     across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
#   ) |>
#   ungroup() |>
#   group_by(game_id) |>
#   mutate(
#     opponent = rev(posteam), .after = posteam
#   ) |>
#   rename(
#     team = posteam
#   ) |>
#   ungroup()

### Penalty Sturcture 2 ----
epaOffData <- pbpData |>
  #filter(season %in% seasonsMod) |>
  #filter(play == 1) |> 
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    # Total EPA
    # off_plays = sum(play == 1),
    # off_epa_sum = sum(epa[play == 1], na.rm = TRUE),
    # off_epa_mean = mean(epa[play == 1], na.rm = TRUE),
    off_plays = sum(play == 1 | play_type == "field_goal", na.rm = TRUE),
    off_epa_sum = sum(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    off_epa_mean = mean(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    # Passing EPA
    off_pass_plays = sum(play == 1 & pass == 1 & penalty == 0, na.rm = TRUE),
    off_pass_epa_sum = sum(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # QB EPA (includes scrambles)
    # off_pass_plays = sum(play == 1 & pass == 1 & penalty == 0, na.rm = TRUE),
    # off_pass_epa_sum = sum(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # off_pass_epa_mean = mean(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # Rushing EPA
    off_rush_plays = sum(play == 1 & rush == 1 & penalty == 0, na.rm = TRUE),
    off_rush_epa_sum = sum(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    # Pre Snap Penalty EPA
    off_penalty_plays = sum(play == 1 & penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[play == 1 & penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[play == 1 & penalty == 1], na.rm = TRUE),
    # Kicker EPA
    off_kick_plays = sum(play_type %in% c("field_goal"), na.rm = TRUE),
    off_kick_epa_sum = sum(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    off_kick_epa_mean = mean(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    # Special Teams EPA
    off_special_plays = sum(special == 1 & play_type != "field_goal", na.rm = TRUE),
    off_special_epa_sum = sum(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    off_special_epa_mean = mean(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    # Play distribution
    off_pass_plays_perc = off_pass_plays/(off_pass_plays + off_rush_plays),
    off_rush_plays_perc = off_rush_plays/(off_pass_plays + off_rush_plays)
  ) |>
  mutate(
    across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
  ) |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  ) |>
  ungroup()

# epaOffData |> filter(season == 2024) |> group_by(team) |> 
#   summarise(
#     across(contains("epa"),
#            ~mean(.x, na.rm = TRUE))
#   ) |> 
#   select(team, contains("mean")) |>
#   arrange(desc(off_epa_mean)) |>
#   view()

epaData <- epaOffData |>
  left_join(
    epaOffData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

#epaData |> filter(season == 2024) |> view()

epaAvgs <- epaData |> 
  group_by(season, team) |>
  summarise(
    across(c(contains("plays"), contains("epa_mean")),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(c(contains("plays"), contains("epa_mean")),
           ~lag(.x, n = 1, default = 0),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  mutate(week = 1)


#epaAvgs |> filter(season == 2024) |> arrange(desc(off_epa_mean)) |> view()

epaData2 <- gameDataLong |>
  select(
    game_id, season, week, team
  ) |>
  left_join(
    epaData |> select(game_id, team, opponent, contains("plays"), contains("mean"))
  ) |>
  group_by(season, team) |>
  mutate(
    across(c(contains("plays"), contains("epa_mean")),
           ~lag(.x, n = 1, default = NA),
           .names = "{.col}")
  ) |>
  ungroup()

epaData3A <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(
    bind_rows(
      epaAvgs,
      epaData2 |>
        select(season, week, team, contains("off"), contains("def")) |>
        filter(week != 1)
    )
  ) |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value = contains("mean"),
    .period = 5,
    .f = mean,
    .partial = TRUE,
    .align = "right"
  ) |>
  mutate(
    across(c(contains("plays"), contains("mean"), -contains("roll")),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  rename_with(~str_remove(.x, "(?<=roll).+"), contains("roll")) |>
  select(
    game_id, season, week, team, opponent, everything()
  )

epaData3 <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(epaData3A)


#rm(epaData, epaData2, epaOffData, epaAvgs, pbpData, pbpPlayTypes, pbpPlayTypesView)

## SRS ----
srsData <- epaData3 |>
  left_join(
    seasonWeekStandings |>
      select(season, week, team, PFG = team_PPG, PAG = opp_PPG, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  group_by(team) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             lag = ~lag(.x, default = 0)
           ),
           .names = "{.col}")
    # lagSRS = lag(SRS, n = 1),
    # diffSRS = SRS - lagSRS,
  ) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             ewma = ~ifelse(week < 6, movavg(.x, n = 2, "e"), .x)
           ),
           .names = "{.col}_ewma")
  ) |>
  #group_by(season, team) |>
  tk_augment_slidify(
    .value   = c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
    # Multiple rolling windows
    .period  = 5,
    .f       = mean,
    .partial = TRUE,
    .align = "right",
    .names = paste0(c("PFG", "PAG", "MOV", "SOS", "SRS", "OSRS", "DSRS"), "_roll")
  ) |>
  ungroup() 

## Efficiency Stats ----
nflStatsWeek <- calculate_stats(seasons = allSeasons,
                                summary_level = "week",
                                stat_type = "team",
                                season_type = "REG+POST")

scoresData <- nflStatsWeek |>
  select(season, week, team, 
         passing_tds, rushing_tds, special_teams_tds, def_tds,
         fumble_recovery_tds, 
         passing_2pt_conversions, rushing_2pt_conversions,
         pat_att, pat_made, pat_pct,
         fg_att, fg_made, fg_pct, 
         def_safeties,
         off_interceptions = passing_interceptions,
         sack_fumbles_lost, rushing_fumbles_lost, receiving_fumbles_lost,
         def_fumbles_forced,
         def_interceptions
  ) |>
  rowwise() |>
  mutate(
    offTD = passing_tds + rushing_tds,
    totalTD = offTD + special_teams_tds + def_tds + fumble_recovery_tds,
    twoPtConv = passing_2pt_conversions + rushing_2pt_conversions,
    twoPtAtt = totalTD - pat_att,
    off_fumbles = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
    def_fumbles = def_fumbles_forced
  ) |>
  ungroup() |>
  right_join(
    gameDataLong |> select(game_id, season, week, team)
  ) |>
  select(game_id, everything()) |>
  mutate(
    rowID = row_number()
  )

scoresData3 <- scoresData |>
  select(rowID, everything()) |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value   = c(everything(), -c(1:3)), #c(-game_id, -season, -team, -rowID),
    # Multiple rolling windows
    .period  = 5,
    .f       = ~mean(., na.rm = T),
    .partial = TRUE,
    .align = "right"#,
    #.names = c("pat_att_roll", "fg_att_roll", "offTD_roll", "totalTD_roll")
  ) |>
  ungroup() |>
  mutate(
    across(where(is.numeric), ~na_if(.x, NaN))
  ) |> #arrange(rowID)
  group_by(team) |>
  mutate(
    across(contains("roll_5"), ~lag(.x, 1))
  ) |>
  mutate(
    across(contains("roll_5"), 
           ~ts_impute_vec(.x, period = 4))
  ) |>
  ungroup() |>
  arrange(rowID)

scoresData <- scoresData3 |>
  select(-c(rowID, season, week))

## Series ----
seriesWeekData <- calculate_series_conversion_rates(pbpData, weekly = TRUE)
#seriesSeasonData <- calculate_series_conversion_rates(pbpData, weekly = FALSE)

seriesData <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(seriesWeekData) |>
  group_by(season, team) |>
  mutate(
    across(-c(game_id, week, opponent),
           ~cummean(.x))
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(-c(season, game_id, week, opponent),
           ~lag(.x))
  ) |>
  ungroup()

## Red Zone -----
red_zone_pbp <- pbpData |>
  filter(!is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  select(fixed_drive, fixed_drive_result, drive_inside20, drive_ended_with_score) |>
  distinct() |>
  arrange(posteam, .by_group = TRUE)

red_zone_sum_week <- red_zone_pbp |>
  summarise(
    drives_num = n(),
    red_zone_app = sum(drive_inside20, na.rm = TRUE),
    red_zone_td = sum(fixed_drive_result == "Touchdown" & drive_inside20, na.rm = TRUE),
    red_zone_app_perc = red_zone_app/drives_num,
    red_zone_eff = red_zone_td/red_zone_app,
    red_zone_eff = ifelse(red_zone_app == 0, 0, red_zone_eff)
  ) 

red_zone_sum_season <- red_zone_sum_week |> 
  ungroup() |>
  group_by(season, posteam) |>
  summarise(
    red_zone_eff = mean(red_zone_eff)
  )
  
red_zone_data <- red_zone_sum_week |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  ) |>
  ungroup()

red_zone_data2 <-
  left_join(
    red_zone_data |> 
      select(game_id, season, week, team, opponent, home_team, away_team, 
             off_red_zone_app_perc = red_zone_app_perc, 
             off_red_zone_eff = red_zone_eff),
    red_zone_data |> 
      select(game_id, opponent, 
             def_red_zone_app_perc = red_zone_app_perc, 
             def_red_zone_eff = red_zone_eff),
      #rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

red_zone_data3 <- red_zone_data2 |>
  group_by(season, team) |>
  mutate(
    across(c(off_red_zone_app_perc, off_red_zone_eff,
             def_red_zone_app_perc, def_red_zone_eff),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  group_by(team) |>
  tk_augment_slidify(
    #.value   = c(everything(), -c(1:3)), #c(-game_id, -season, -team, -rowID),
    .value = c(off_red_zone_app_perc, off_red_zone_eff,
               def_red_zone_app_perc, def_red_zone_eff),
    # Multiple rolling windows
    .period  = 5,
    .f       = ~mean(., na.rm = T),
    .partial = TRUE,
    .align = "right"#,
    #.names = c("pat_att_roll", "fg_att_roll", "offTD_roll", "totalTD_roll")
  ) |>
  mutate(
    across(c(contains("cum"), contains("roll")),
           ~lag(.x, default = 0),
           .names = "{.col}")
  ) |>
  ungroup() |>
  select(
    game_id, team, 
    off_red_zone_app_perc_cum, off_red_zone_app_perc_roll_5,
    off_red_zone_eff_cum, off_red_zone_eff_roll_5,
    def_red_zone_app_perc_cum, def_red_zone_app_perc_roll_5,
    def_red_zone_eff_cum, def_red_zone_eff_roll_5
  )

red_zone_data <- red_zone_data3

## Weather -----
weatherData <- pbpData |>
  select(game_id, home_team, away_team, weather) |>
  distinct() |>
  left_join(
    gameData |> select(game_id, gameday, gametime, home_team, away_team, temp, wind, roof)
  ) |>
  mutate(
    rain = str_split_i(weather, "Temp", i = 1)
  )

# Model Data ----
modData <- gameData |>
  select(-c(
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    home_qb_id, away_qb_id,
    home_qb_name, away_qb_name,
    referee,
    stadium_id
  )) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             contains("PFG"), contains("PAG"), 
             contains("MOV"), contains("SOS"), 
             contains("SRS"), contains("OSRS"), contains("DSRS")
             #c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)
             ) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             contains("PFG"), contains("PAG"), 
             contains("MOV"), contains("SOS"), 
             contains("SRS"), contains("OSRS"), contains("DSRS")
             #c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)
      ) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    scoresData |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    scoresData |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    seriesData |>
      select(-c(season, week, opponent)) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    seriesData |>
      select(-c(season, week, opponent))  |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    red_zone_data |>
      #select(-c(season, week, opponent))  |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    red_zone_data |>
      #select(-c(season, week, opponent))  |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  rename_with(~str_remove(.x, "_mean"), contains("mean")) |>
  mutate(
    home_net_epa_cum = home_off_epa_cum - home_def_epa_cum,
    home_net_epa_roll = home_off_epa_roll - home_def_epa_roll,
    home_off_net_epa_cum = home_off_epa_cum + away_def_epa_cum,
    home_off_net_epa_roll = home_off_epa_roll + away_def_epa_roll,
    home_pass_net_epa_cum = home_off_pass_epa_cum + away_def_pass_epa_cum,
    home_pass_net_epa_roll = home_off_pass_epa_roll + away_def_pass_epa_roll,
    home_rush_net_epa_cum = home_off_rush_epa_cum + away_def_rush_epa_cum,
    home_rush_net_epa_roll = home_off_rush_epa_roll + away_def_rush_epa_roll,
    home_penalty_net_epa_cum = home_off_penalty_epa_cum + away_def_penalty_epa_cum,
    home_penalty_net_epa_roll = home_off_penalty_epa_roll + away_def_penalty_epa_roll,
    away_net_epa_cum = away_off_epa_cum - away_def_epa_cum,
    away_net_epa_roll = away_off_epa_roll - away_def_epa_roll,
    away_off_net_epa_cum = away_off_epa_cum + home_def_epa_cum,
    away_off_net_epa_roll = away_off_epa_roll + home_def_epa_roll,
    away_pass_net_epa_cum = away_off_pass_epa_cum + home_def_pass_epa_cum,
    away_pass_net_epa_roll = away_off_pass_epa_roll + home_def_pass_epa_roll,
    away_rush_net_epa_cum = away_off_rush_epa_cum + home_def_rush_epa_cum,
    away_rush_net_epa_roll = away_off_rush_epa_roll + home_def_rush_epa_roll,
    away_penalty_net_epa_cum = away_off_penalty_epa_cum + home_def_penalty_epa_cum,
    away_penalty_net_epa_roll = away_off_penalty_epa_roll + home_def_penalty_epa_roll,
    
    home_PFG_net = home_PFG - away_PAG,
    home_PAG_net = away_PFG - home_PAG,
    home_MOV_net = home_MOV - away_MOV,
    home_SOS_net = home_SOS - away_SOS,
    home_SRS_net = home_SRS - away_SRS,
    home_OSRS_net = home_OSRS - away_DSRS,
    home_DSRS_net = home_DSRS - away_OSRS,
    
    away_PFG_net = away_PFG - home_PAG,
    away_PAG_net = home_PFG - away_PAG,
    away_MOV_net = away_MOV - home_MOV,
    away_SOS_net = away_SOS - home_SOS,
    away_SRS_net = away_SRS - home_SRS,
    away_OSRS_net = away_OSRS - home_DSRS,
    away_DSRS_net = away_DSRS - home_OSRS,
    
    home_PFG_ewma_net = home_PFG_ewma - away_PAG_ewma,
    home_PAG_ewma_net = away_PFG_ewma - home_PAG_ewma,
    home_MOV_ewma_net = home_MOV_ewma - away_MOV_ewma,
    home_SOS_ewma_net = home_SOS_ewma - away_SOS_ewma,
    home_SRS_ewma_net = home_SRS_ewma - away_SRS_ewma,
    home_OSRS_ewma_net = home_OSRS_ewma - away_DSRS_ewma,
    home_DSRS_ewma_net = home_DSRS_ewma - away_OSRS_ewma,
    
    away_PFG_ewma_net = away_PFG_ewma - home_PAG_ewma,
    away_PAG_ewma_net = home_PFG_ewma - away_PAG_ewma,
    away_MOV_ewma_net = away_MOV_ewma - home_MOV_ewma,
    away_SOS_ewma_net = away_SOS_ewma - home_SOS_ewma,
    away_SRS_ewma_net = away_SRS_ewma - home_SRS_ewma,
    away_OSRS_ewma_net = away_OSRS_ewma - home_DSRS_ewma,
    away_DSRS_ewma_net = away_DSRS_ewma - home_OSRS_ewma,
    
    home_PFG_roll_net = home_PFG_roll - away_PAG_roll,
    home_PAG_roll_net = away_PFG_roll - home_PAG_roll,
    home_MOV_roll_net = home_MOV_roll - away_MOV_roll,
    home_SOS_roll_net = home_SOS_roll - away_SOS_roll,
    home_SRS_roll_net = home_SRS_roll - away_SRS_roll,
    home_OSRS_roll_net = home_OSRS_roll - away_DSRS_roll,
    home_DSRS_roll_net = home_DSRS_roll - away_OSRS_roll,
    
    away_PFG_roll_net = away_PFG_roll - home_PAG_roll,
    away_PAG_roll_net = home_PFG_roll - away_PAG_roll,
    away_MOV_roll_net = away_MOV_roll - home_MOV_roll,
    away_SOS_roll_net = away_SOS_roll - home_SOS_roll,
    away_SRS_roll_net = away_SRS_roll - home_SRS_roll,
    away_OSRS_roll_net = away_OSRS_roll - home_DSRS_roll,
    away_DSRS_roll_net = away_DSRS_roll - home_OSRS_roll
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 68, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))













