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

seasonsMod <- 2006:2024
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

pbpDataMod2 <- pbpDataMod |> filter(game_id != "2024_14_GB_DET")
pbpDataMod <- pbpDataMod2