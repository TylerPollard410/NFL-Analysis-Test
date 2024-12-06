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

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "NFLdata",
                 user = "postgre",
                 password = "NFLpass1234",
                 host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
dbListTables(con)
dbDisconnect(con)


# EPA ========
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
    off_pass_plays = sum(play_type == "pass"),
    off_pass_epa_sum = sum(epa[play_type == "pass"], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[play_type == "pass"], na.rm = TRUE),
    off_rush_plays = sum(play_type == "run"),
    off_rush_epa_sum = sum(epa[play_type == "run"], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[play_type == "run"], na.rm = TRUE),
    off_penalty_plays = sum(penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[penalty == 1], na.rm = TRUE)
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
  ) |>
  left_join(
    seasonWeekStandings |>
      select(
        season,
        week,
        team,
        PFG = team_PPG,
        PAG = opp_PPG,
        MOV,
        SOS,
        SRS,
        OSRS,
        DSRS
      ),
    by = join_by(season, week, team)
  )

srsAvgs <- seasonWeekStandings |>
  group_by(season) |>
  filter(week == max(week)) |>
  ungroup() |>
  select(
    season,
    team,
    avg_PFG = team_PPG,
    avg_PAG = opp_PPG,
    avg_MOV = MOV, 
    avg_SOS = SOS,
    avg_SRS = SRS, 
    avg_OSRS = OSRS, 
    avg_DSRS = DSRS
  ) |>
  mutate(
    season = season + 1
  ) |>
  mutate(week = 1, .after = season)

# left_join(
#   gameDataLongMod |> 
#     select(game_id, gameday, weekday, gametime, time_of_day,
#            team, location, locationID),
#   by = join_by(game_id, team)
# ) |>
# select(-home_team, -away_team) |>
# relocate(gameday, weekday, gametime, time_of_day, location, locationID,
#          .after = week)


tsDataLong <- gameDataLongMod |>
  left_join(
    epaData |> select(-home_team, -away_team)
  ) |> 
  # left_join(
  #   srsAvgs
  # ) |>
  filter(!is.na(spread_line)) |>
  select(-c(
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
  )) |>
  mutate(
    gameday = as_date(gameday)
  ) 

write_csv(tsDataLong, file = "~/Desktop/tsDataLong.csv")

# Plot times series ----
tsDataLong |>
  filter(team == "WAS") |>
  group_by(season) |>
  plot_time_series(
    .date_var = gameday,
    .value = SRS,
    #.color_var = team,
    .facet_nrow = 1,
    .facet_ncol = 4,
    .facet_scales = "free_x"
  )

tsDataLong |>
  filter(team == "WAS") |>
  group_by(season) |>
  plot_time_series(
    .date_var = gameday,
    .value = off_epa_mean,
    #.color_var = team,
    .facet_nrow = 1,
    .facet_ncol = 4,
    .facet_scales = "free_x"
  )

tsDataLong |>
  filter(team == "WAS") |>
  group_by(season) |>
  plot_time_series(
    .date_var = gameday,
    .value = def_epa_mean,
    #.color_var = team,
    .facet_nrow = 1,
    .facet_ncol = 4,
    .facet_scales = "free_x"
  )

tsFeaturesOffEpaSum <- tsDataLong |>
  group_by(team) |>
  tk_tsfeatures(
    .date_var = gameday,
    .value = off_epa_sum
  )


featureData <- tsDataLong |>
  mutate(order = row_number()) |>
  group_by(team) |>
  tk_augment_slidify(
    .value = contains("epa"),
    .f = ~ mean(.x, na.rm = TRUE),
    .period = 5,
    .align = "right",
    .names = "auto"  #~paste0("roll_", .x)
  ) %>%
  tk_augment_lags(
    .value = c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
    .lags = 1,
    .names = "auto" #~paste0("lag_", .x)
  ) %>%
  ungroup() |>
  arrange(order) |>
  select(-order) |>
  relocate(team, .after = opponent_score)




