## Update database with current data and R objects
# Last Update: 


# Libraries ----
## Database 
library(future)
library(DBI)
library(RPostgres)

## seasonStandings
library(DescTools)

## nflverse
library(nflverse)
library(tidyverse)

# Set wd ----
setwd("/Users/tylerpollard/Desktop/NFLAnalysisTest")

# Amazon RDS connection ----
plan("multisession")

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
# 
# ## List Tables ----
# dbListTables(con)
# dbRemoveTable(conn = con, name = "seasonWeekStandings")

# Universal Variables ----
allSeasons <- 2006:most_recent_season()
teamsData <- load_teams(current = FALSE)

# gameIDs <- load_schedules(seasons = allSeasons) |>
#   filter(!is.na(result)) |>
#   pull(game_id)

# Load/Updata Data ----
## gameData -------------------------------
source("./app/data-raw/gameData.R")
#save(gameData, file = "./app/data/gameData.rda")

### Initial 
#dbWriteTable(con, name = "gameData", value = gameData, overwrite = TRUE)

## gameDataLong -------------------------------
source("./app/data-raw/gameDataLong.R")

#dbWriteTable(con, name = "gameDataLong", value = gameDataLong, overwrite = TRUE)

## pbpData ------------------------------
### Initial
#source("./app/data-raw/pbpData.R")

#dbWriteTable(con, name = "pbpData", value = pbpData)

# ### Update
# gameIDsCurrent <- tbl(con, "pbpData") |>
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
# pbpDataUpdateCols <- pbpData_tbl$lazy_query$vars
# 
# dbListTables(con)
# rm(pbpDataUpdate, pbpData_tbl, pbpDataUpdateRows, pbpDataUpdateCols)
# 
# 
## playerOffenseData ---------------------------
source("./app/data-raw/playerOffenseData.R")
save(playerOffenseData, file = "./app/data/playerOffenseData.rda")
# fst::write_fst(playerOffenseData, 
#                path = "./app/data/playerOffenseData.fst",
#                compress = 100)
# 
# #dbWriteTable(con, name = "playerOffenseData", value = playerOffenseData, overwrite = TRUE)
# 
# dbListTables(con)
# rm(playerOffenseData)
# 
## seasonStandings ------------------------------
# ### Initial
source("./app/data-raw/seasonStandings.R")
save(seasonStandings, file = "./app/data/seasonStandings.rda")
# 
# #dbWriteTable(con, name = "seasonStandings", value = seasonStandings, overwrite = TRUE)
# 
# dbListTables(con)
# rm(seasonStandings)
# 
## seasonWeekStandings ------------------------------
# ### Initial
source("./app/data-raw/seasonWeekStandings.R")
save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")
# 
# #dbWriteTable(con, name = "seasonWeekStandings", value = seasonWeekStandings, overwrite = TRUE)
# 
# dbListTables(con)
# rm(seasonWeekStandings)
# 
# 
## Disconnect -----
# dbDisconnect(con)
# rm(list = ls())


