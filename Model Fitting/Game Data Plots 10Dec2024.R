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

# Takes about 30 seconds
system.time(
  source("./app/data-raw/modelData2.R")
)

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2021:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

# Previous Data ----
modData2 <- modData

modData <- modData |> filter(!is.na(result))
histModelData <- modData |> filter(season <= 2023)
modelData <- modData |> filter(season == 2024)


colnames(modData)

library(radiant)
radiant::radiant_window()


nn_preds <- read_csv("./Model Fitting/Data/histModelData_nn_pred2.csv")

nn_preds2 <- nn_preds |>
  left_join(
    modelData |>
      select(season, week, away_team, home_team, result, spread_line, spreadCover),
    by = join_by(season == season, away_team == away_team, home_team == home_team)
  ) |>
  mutate(
    spreadBet = ifelse(Prediction > spread_line, TRUE, FALSE),
    spreadCorrect = ifelse(spreadBet == spreadCover, TRUE, FALSE),
    MAE = Prediction - result
  )

mean(abs(nn_preds2$MAE), na.rm = TRUE)
mean(nn_preds2$spreadCorrect, na.rm = TRUE)







