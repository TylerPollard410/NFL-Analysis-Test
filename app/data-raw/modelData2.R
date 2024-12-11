## Model home and away score

# Load Libraries ----
## Data Manipulation
library(stringr)

## Tables
library(DBI)
library(RPostgres)
library(data.table)

## Plotting
library(smplot2)
library(patchwork)

## Modeling
library(zoo)
library(pracma)
library(forecast)
library(timetk)
library(elo)
library(MASS)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(cmdstanr)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse
library(nflverse)

## Tidyverse
library(tidyverse)

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2021:2024
gameDataMod <- gameData |> filter(season %in% seasonsMod)
gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
pbpDataMod <- load_pbp(seasons = seasonsMod)
load("./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

rm(gameData, gameDataLong)

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
# dbListTables(con)
# dbDisconnect(con)

pbpDataMod2 <- pbpDataMod |> filter(game_id != "2024_14_GB_DET")
pbpDataMod <- pbpDataMod2


# Aggregate pbp ----
pbpPlayTypes <- pbpDataMod |>
  select(
    play,
    play_type, 
    play_type_nfl,
    pass,
    pass_attempt,
    rush,
    rush_attempt,
    special_teams_play,
    special,
    penalty,
    qb_dropback,
    qb_kneel,
    qb_spike,
    qb_scramble,
    penalty_type
  ) |>
  distinct()

# pbpPlayTypesView <- pbpPlayTypes |>
#   filter(play == 1, pass == 1)
#   filter(play == 1, pass == 0, rush == 0, penalty == 1)


## EPA ----
epaOffData <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(play == 1) |> 
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_plays = n(),
    off_epa_sum = sum(epa, na.rm = TRUE),
    off_epa_mean = mean(epa, na.rm = TRUE),
    off_pass_plays = sum(pass == 1),
    off_pass_epa_sum = sum(epa[pass == 1], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[pass == 1], na.rm = TRUE),
    off_rush_plays = sum(rush == 1),
    off_rush_epa_sum = sum(epa[rush == 1], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[rush == 1], na.rm = TRUE),
    off_penalty_plays = sum(pass == 0 & rush == 0 & special == 0 & penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
    off_special_plays = sum(special == 1 & penalty == 1, na.rm = TRUE),
    off_special_epa_sum = sum(epa[special == 1 & penalty == 0], na.rm = TRUE),
    off_special_epa_mean = mean(epa[special == 1 & penalty == 0], na.rm = TRUE),
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
  )

epaData <- epaOffData |>
  left_join(
    epaOffData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

epaAvgs <- epaData |> 
  group_by(season, team) |>
  summarise(
    across(contains("epa_mean"),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(contains("mean"),
           ~lag(.x, n = 1, default = 0),
           .names = "{.col}"
    )
  ) |>
  ungroup()

epaData2 <- gameDataLongMod |>
  left_join(
    epaData |> select(game_id, team, opponent, contains("mean"))
  ) |>
  group_by(season, team) |>
  mutate(
    across(contains("mean"),
           ~lag(.x, n = 1, default = NA),
           .names = "{.col}")
  ) |>
  ungroup()
# left_join(
#   epaAvgs,
#   by = join_by(season, team)
# )

epaData3 <- epaData2 |>
  mutate(
    across(contains("mean"),
           ~ifelse(is.na(.x), epaAvgs |> filter(season == season, team == team) |> pull(.x), .x))
  ) |>
  mutate(
    row = row_number()
  ) |>
  # select(
  #   game_id, season, week, team, contains("mean")
  # ) |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value = contains("mean"),
    .period = 4,
    .f = mean,
    .partial = TRUE,
    .align = "right"
  ) |>
  mutate(
    across(c(contains("mean"), -contains("roll")),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  arrange(row) |>
  select(-c(
    row,
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    team_qb_id,
    team_qb_name,
    opponent_qb_id,
    opponent_qb_name,
    referee,
    stadium_id
  ))

#rm(epaData, epaData2, epaOffData, epaAvgs, pbpDataMod, pbpPlayTypes, pbpPlayTypesView)
# mutate(
#   off_epa_mean_feat = ifelse(is.na(off_epa_mean), off_epa_mean, off_epa_mean),
#   off_pass_epa_mean_feat = ifelse(is.na(off_pass_epa_mean), off_pass_epa_mean, off_pass_epa_mean),
#   off_rush_epa_mean_feat = ifelse(is.na(off_rush_epa_mean), off_rush_epa_mean, off_rush_epa_mean),
#   off_penalty_epa_mean_feat = ifelse(is.na(off_penalty_epa_mean), off_penalty_epa_mean, off_penalty_epa_mean),
#   def_epa_mean_feat = ifelse(is.na(def_epa_mean), def_epa_mean, def_epa_mean),
#   def_pass_epa_mean_feat = ifelse(is.na(def_pass_epa_mean), def_pass_epa_mean, def_pass_epa_mean),
#   def_rush_epa_mean_feat = ifelse(is.na(def_rush_epa_mean), def_rush_epa_mean, def_rush_epa_mean),
#   def_penalty_epa_mean_feat = ifelse(is.na(def_penalty_epa_mean), def_penalty_epa_mean, def_penalty_epa_mean)
# )

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
           .names = "{.col}")
  ) |>
  ungroup() 

# Model Data ----
modData <- gameDataMod |>
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
             c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  mutate(
    home_epa_cum = home_off_epa_mean_cum + away_def_epa_mean_cum,
    home_epa_roll = home_off_epa_mean_roll_4 + away_def_epa_mean_roll_4,
    home_pass_epa_cum = home_off_pass_epa_mean_cum + away_def_pass_epa_mean_cum,
    home_pass_epa_roll = home_off_pass_epa_mean_roll_4 + away_def_pass_epa_mean_roll_4,
    home_rush_epa_cum = home_off_rush_epa_mean_cum + away_def_rush_epa_mean_cum,
    home_rush_epa_roll = home_off_rush_epa_mean_roll_4 + away_def_rush_epa_mean_roll_4,
    home_penalty_epa_cum = home_off_penalty_epa_mean_cum + away_def_penalty_epa_mean_cum,
    home_penalty_epa_roll = home_off_penalty_epa_mean_roll_4 + away_def_penalty_epa_mean_roll_4,
    away_epa_cum = away_off_epa_mean_cum + home_def_epa_mean_cum,
    away_epa_roll = away_off_epa_mean_roll_4 + home_def_epa_mean_roll_4,
    away_pass_epa_cum = away_off_pass_epa_mean_cum + home_def_pass_epa_mean_cum,
    away_pass_epa_roll = away_off_pass_epa_mean_roll_4 + home_def_pass_epa_mean_roll_4,
    away_rush_epa_cum = away_off_rush_epa_mean_cum + home_def_rush_epa_mean_cum,
    away_rush_epa_roll = away_off_rush_epa_mean_roll_4 + home_def_rush_epa_mean_roll_4,
    away_penalty_epa_cum = away_off_penalty_epa_mean_cum + home_def_penalty_epa_mean_cum,
    away_penalty_epa_roll = away_off_penalty_epa_mean_roll_4 + home_def_penalty_epa_mean_roll_4
    # PFG = home_PFG - away_PAG,
    # PAG = away_PFG - home_PAG,
    # MOV = home_MOV - away_MOV,
    # SOS = home_SOS - away_SOS,
    # SRS = home_SRS - away_SRS,
    # OSRS = home_OSRS - away_DSRS,
    # DSRS = away_DSRS - home_DSRS
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 70, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

rm(list = ls()[ls() != "modData"])


# Compare to nflverse ----
# fastRstatsWeek <- calculate_stats(
#   seasons = 2021:2024, summary_level = "week", stat_type = "team", season_type = "REG+POST"
#   )
# fastRstatsWeek2 <- gameDataLongMod |>
#   select(game_id, season, week, season_type, team, opponent, result, total) |>
#   left_join(
#     fastRstatsWeek |> select(-season_type),
#     by = join_by(season, week, team, opponent == opponent_team)
#   ) |>
#   filter(!is.na(result))
# 
# modDataLong <- modData |>
#   clean_homeaway(invert = "result")
# 
# ## Passing ----
# fastRepa <- fastRstatsWeek2 |>
#   select(game_id, season, week, team, opponent,
#          attempts, passing_epa,
#          carries, rushing_epa) |>
#   mutate(
#     passing_epa_mean = passing_epa/attempts,
#     rushing_epa_mean = rushing_epa/carries
#   ) |>
#   arrange(game_id)
# 
# epaDataComp <- epaData |>
#   select(game_id, season, week, team, opponent, 
#          off_plays, off_epa_mean, 
#          off_pass_plays, off_pass_epa_sum, off_pass_epa_mean,
#          off_rush_plays, off_rush_epa_sum, off_rush_epa_mean) |>
#   left_join(
#     fastRepa
#   )|>
#   ungroup()
# 
# epaDataCompSum <- epaDataComp |>
#   select(-c(game_id, week, opponent)) |>
#   group_by(season, team) |>
#   summarise(
#     across(everything(), ~round(mean(.x), 2))
#   ) |>
#   ungroup()
# 
# epa2024means <- epaDataCompSum |>
#   filter(season == 2024) |>
#   select(
#     team,
#     off_epa_mean,
#     off_pass_epa_mean,
#     passing_epa_mean,
#     off_rush_epa_mean,
#     rushing_epa_mean
#     ) |>
#   arrange(desc(off_epa_mean))







