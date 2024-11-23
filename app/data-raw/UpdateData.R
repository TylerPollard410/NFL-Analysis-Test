## Updata database with current data

library(future)
library(DBI)
library(RPostgres)
library(nflverse)
library(tidyverse)

# Amazon RDS connection ----
plan("multisession")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "NFLdata",
                 user = "postgre",
                 password = "NFLpass1234",
                 host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")

## List Tables ----
dbListTables(con)

# Universal Variables ----
allSeasons <- 2006:most_recent_season()
teamsData <- load_teams(current = FALSE)

gameIDs <- load_schedules(seasons = allSeasons) |>
  filter(!is.na(result)) |>
  pull(game_id)


# Load/Updata Data ----
## pbpData ------------------------------
### Initial
#source("./app/data-raw/pbpData.R")

#dbWriteTable(con, name = "pbpData", value = pbpData)

### Update
pbpDataUpdate <- load_pbp(seasons = most_recent_season()) |>
  filter(!(game_id %in% gameIDs))

if(nrow(pbpDataUpdate) > 0){
  dbAppendTable(con, "pbpData", pbpDataUpdate)
}else{
  print("No play-by-play data to update")
}

rm(pbpDataUpdate)

## gameData -------------------------------
source("./app/data-raw/gameData.R")

dbWriteTable(con, name = "gameData", value = gameData, overwrite = TRUE)

## gameDataLong -------------------------------
source("./app/data-raw/gameDataLong.R")

dbWriteTable(con, name = "gameDataLong", value = gameDataLong, overwrite = TRUE)


## playerOffenseData ---------------------------
source("./app/data-raw/playerOffenseData.R")

dbWriteTable(con, name = "playerOffenseData", value = playerOffenseData, overwrite = TRUE)


##












