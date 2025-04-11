## Update database with current data and R objects
# Last Update: 



# Libraries ----
## Helpers ----
library(tictoc)
library(progressr)

## Database 
library(future)
library(DBI)
library(RPostgres)

## Manipulate Data ----
library(DescTools)
library(stringr)
library(pracma)
library(timetk)

## nflverse
library(nflverse)
library(tidyverse)

# Set wd ----
setwd("/Users/tylerpollard/Desktop/NFLAnalysisTest")

# Universal Variables ----
allSeasons <- 2006:most_recent_season()
teamsData <- load_teams(current = FALSE)

gameIDs <- load_schedules(seasons = allSeasons) |>
  filter(!is.na(result)) |>
  pull(game_id)

# Load/Updata Data ----
## gameData -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating gameData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/gameData.R")
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished gameData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## gameDataLong -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating gameDataLong", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/gameDataLong.R")
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished gameDataLong", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## pbpData ------------------------------
### Initial
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating pbpData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/pbpData.R")
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished pbpData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

### Update
# gameIDsCurrent <- pbpData |>
#   filter(season == 2024) |>
#   distinct(game_id) |>
#   pull(game_id)
# 
# gameIDsUpdate <- gameData |>
#   filter(season == 2024) |>
#   filter(!is.na(result)) |>
#   filter(!(game_id %in% gameIDsCurrent)) |>
#   pull(game_id)
# 
# pbpDataUpdate <- load_pbp(seasons = most_recent_season()) |>
#   filter(game_id %in% gameIDsUpdate)
# 
# if(nrow(pbpDataUpdate) > 0){
#   dbAppendTable(con, "pbpData", pbpDataUpdate)
# }else{
#   print("No play-by-play data to update")
# }
# 
# pbpData_tbl <- tbl(con, "pbpData")
# 
# pbpDataUpdateRows <- pbpData_tbl |>
#   pull(game_id) |>
#   length()
# 
# pbpDataUpdateCols <- length(pbpData_tbl$lazy_query$vars)
# 
pbpDataDate <- attributes(pbpData)$nflverse_timestamp
# paste0("pbpData updated ", pbpDataDate,
#        " with ", pbpDataUpdateRows, " rows and ",
#        pbpDataUpdateCols, " cols.")
# 
# save(pbpDataDate, file = "./app/data/pbpDataDate.rda")

#dbListTables(con)
#rm(pbpDataUpdate, pbpData_tbl, pbpDataUpdateRows, pbpDataUpdateCols)


## playerOffenseData ---------------------------
#tic()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating playerOffenseData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/playerOffenseData.R")
save(playerOffenseData, file = "./app/data/playerOffenseData.rda")
#toc()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished playerOffenseData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## seasonStandings ------------------------------
### Initial
#tic()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating seasonStandings", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/seasonStandings.R")
save(seasonStandings, file = "./app/data/seasonStandings.rda")
#toc()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished seasonStandings", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## seasonWeekStandings ------------------------------
### Initial
#tic()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating seasonWeekStandings", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/seasonWeekStandings.R")
save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")
#toc()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished seasonWeekStandings", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## elo ------------------------------
### Initial
#tic()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating eloData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
source("./app/data-raw/eloData.R")
save(eloData, file = "./app/data/eloData.rda")
#toc()
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished eloData", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

## modData ----
# About 8 minutes
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Generating modData and modDataLong", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
system.time(
  source("./app/data-raw/modData.R")
)
save(modDataLong, file = "./app/data/modDataLong.rda")
save(modData, file = "./app/data/modData.rda")
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n",
    "Finished modData and modDataLong", "\n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")

