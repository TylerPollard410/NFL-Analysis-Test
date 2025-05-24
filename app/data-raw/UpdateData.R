## Update database with current data and R objects
# Last Update: 


# Libraries ----
## Helpers ----
library(tictoc)
library(progressr)

## Database 
library(future)
#library(DBI)
#library(RPostgres)

## Manipulate Data ----
library(DescTools)
library(stringr)
library(pracma)
library(timetk)

## nflverse
library(nflverse)
library(tidyverse)

# Set wd ----
#setwd("/Users/tylerpollard/Desktop/NFLAnalysisTest")

# Universal Variables ----
allSeasons <- 2006:most_recent_season()
teamsData <- load_teams(current = FALSE)

gameIDs <- load_schedules(seasons = allSeasons) |>
  filter(!is.na(result)) |>
  pull(game_id)

env_vars <- ls()

# Load/Updata Data ----
## gameData -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating gameData")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/gameData.R")
gameData <- compute_game_data(allSeasons)

cat("\n", "✅" , "Finished gameData", "\n")



## gameDataLong -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating gameDataLong")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/gameDataLong.R")
gameDataLong <- compute_game_data_long(gameData)

cat("\n", "✅" , "Finished gameDataLong", "\n")



## pbpData ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating pbpData")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/pbpData.R")
pbpData <- compute_pbp_data(allSeasons)
pbpDataDate <- attributes(pbpData)$nflverse_timestamp

cat("\n", "✅" , "Finished pbpData", "\n")

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
# pbpDataDate <- attributes(pbpData)$nflverse_timestamp
# paste0("pbpData updated ", pbpDataDate,
#        " with ", pbpDataUpdateRows, " rows and ",
#        pbpDataUpdateCols, " cols.")
#
# save(pbpDataDate, file = "./app/data/pbpDataDate.rda")

#dbListTables(con)
#rm(pbpDataUpdate, pbpData_tbl, pbpDataUpdateRows, pbpDataUpdateCols)



## playerOffenseData ---------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating playerOffenseData")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/playerOffenseData.R")
playerOffenseData <- compute_player_offense_data(allSeasons, gameDataLong)
save(playerOffenseData, file = "./app/data/playerOffenseData.rda")

cat("\n", "✅" , "Finished playerOffenseData", "\n")



## seasonStandings ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating seasonStandings")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/seasonStandings.R")
seasonStandings <- compute_season_standings(
  gameData,
  tol = 1e-3, 
  max_iter = 200,
  print_message = TRUE
)
save(seasonStandings, file = "./app/data/seasonStandings.rda")

cat("\n", "✅" , "Finished seasonStandings", "\n")



## seasonWeekStandings ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating seasonWeekStandings")
env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/seasonWeekStandings.R")
## reset - recompute_all
# B1 TRUE - TRUE
# B2 TRUE - FALSE
# B3 FALSE - TRUE
# B4 FALSE - FALSE
# B5 Numeric (N) - TRUE
# B6 Numeric (N) - FALSE

seasonWeekStandingsB4 <- update_weekly_standings(
  gameData,
  tol = 1e-3,
  max_iter = 100,
  reset = 8,
  recompute_all = TRUE,
  cache_file = "./app/data/seasonWeekStandings.rda"
)
#save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")

cat("\n", "✅" , "Finished seasonWeekStandings", "\n")



# ## elo ------------------------------
# ### Initial
# cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating eloData")
# env_vars <- ls()
# rm(list = setdiff(ls(), env_vars))
# 
# source("./app/data-raw/eloData.R")
# #save(eloData, file = "./app/data/eloData.rda")
# save(eloData, eloFinals, file = "./scripts/UpdateData/PriorData/eloData.rda")
# #save(eloFinals, file = "./scripts/UpdateData/PriorData/eloFinals.rda")
# 
# cat("\n", "✅" , "Finished eloData", "\n")
# 
# 
# 
# ## modData ----
# # About 8 minutes
# cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating modData and modDataLong")
# env_vars <- ls()
# rm(list = setdiff(ls(), env_vars))
# 
# system.time(
#   source("./app/data-raw/modData.R")
# )
# #save(modDataLong, file = "./app/data/modDataLong.rda")
# save(modData, file = "./app/data/modData.rda")
# 
# cat("\n", "✅" , "Finished modData and modDataLong", "\n")
# 


# Final message ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "\n",
    "✅" ,  "DATA UPDATE COMPLETE",
    "-", attributes(gameData)$nflverse_timestamp
)



