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
#save(playerOffenseData, file = "./app/data/playerOffenseData.rda")
save(playerOffenseData, file = "~/Desktop/NFL Analysis Data/UpdateData/playerOffenseData.rda")

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
#save(seasonStandings, file = "./app/data/seasonStandings.rda")
save(seasonStandings, file = "~/Desktop/NFL Analysis Data/UpdateData/seasonStandings.rda")

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

seasonWeekStandings <- update_weekly_standings(
  gameData,
  tol = 1e-3,
  max_iter = 200,
  reset = TRUE,
  recompute_all = FALSE, #TRUE,
  cache_file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings.rda" #"./app/data/seasonWeekStandings.rda"
)
#save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")
save(seasonWeekStandings, file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings.rda")

seasonWeekStandings_roll20 <- update_weekly_standings(
  gameData,
  tol = 1e-3,
  max_iter = 200,
  reset = 20,
  recompute_all = FALSE, #TRUE,
  cache_file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings_roll20.rda" #"./app/data/seasonWeekStandings_roll20.rda"
)
#save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")
save(seasonWeekStandings_roll20, file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings_roll20.rda")

seasonWeekStandings_roll10 <- update_weekly_standings(
  gameData,
  tol = 1e-3,
  max_iter = 200,
  reset = 10,
  recompute_all = FALSE, #TRUE,
  cache_file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings_roll10.rda" #"./app/data/seasonWeekStandings_roll10.rda"
)
#save(seasonWeekStandings, file = "./app/data/seasonWeekStandings.rda")
save(seasonWeekStandings_roll10, file = "~/Desktop/NFL Analysis Data/UpdateData/seasonWeekStandings_roll10.rda")

cat("\n", "✅" , "Finished seasonWeekStandings", "\n")



## eloData ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating eloData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/eloData.R")

elo_result <- update_elo_data(
  gameData = gameData,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE,
  recompute_all = TRUE,
  cache_file = "~/Desktop/NFL Analysis Data/UpdateData/eloData.rda",
  season_factor = 0.6
)
eloData   <- elo_result$elo_history
eloFinals <- elo_result$final_elos
rm(elo_result)
#eloData0   <- elo_result$elo_history
#eloFinals0 <- elo_result$final_elos
#eloData1   <- elo_result$elo_history
#eloFinals1 <- elo_result$final_elos
#save(eloData, eloFinals, file = "./app/data/eloData.rda")
save(eloData, eloFinals, file = "~/Desktop/NFL Analysis Data/UpdateData/eloData.rda")

cat("\n", "✅" , "Finished eloData", "\n")


## epaData ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating epaData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/epaData.R")

epaData <- compute_epa_data(pbpData, scaled_wp = FALSE)
#save(epaData, file = "./app/data/epaData.rda")
save(epaData, file = "~/Desktop/NFL Analysis Data/UpdateData/epaData.rda")

cat("\n", "✅" , "Finished epaData", "\n")



## scoresData ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating scoresData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/scoresData.R")

nflStatsWeek_loc   <- "~/Desktop/NFL Analysis Data/UpdateData/nflStatsWeek.rda"
scoresData <- compute_scores_data(
  gameDataLong, pbpData, allSeasons, 
  stats_loc = nflStatsWeek_loc, 
  recompute_all = FALSE
)
#save(scoresData, file = "./app/data/scoresData.rda")
save(scoresData, file = "~/Desktop/NFL Analysis Data/UpdateData/scoresData.rda")

cat("\n", "✅" , "Finished scoresData", "\n")



## seriesData ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating seriesData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/seriesData.R")

nflSeriesWeek_loc   <- "~/Desktop/NFL Analysis Data/UpdateData/nflSeriesWeek.rda"
seriesData <- compute_series_data(
  gameDataLong, pbpData, 
  series_loc = nflSeriesWeek_loc, 
  recompute_all = TRUE
  )
#save(seriesData, file = "./app/data/seriesData.rda")
save(seriesData, file = "~/Desktop/NFL Analysis Data/UpdateData/seriesData.rda")

cat("\n", "✅" , "Finished seriesData", "\n")



## turnoverData ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating turnoverData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/turnoverData.R")

turnoverData <- compute_turnover_data(gameDataLong, pbpData)
#save(turnoverData, file = "./app/data/turnoverData.rda")
save(turnoverData, file = "~/Desktop/NFL Analysis Data/UpdateData/turnoverData.rda")

cat("\n", "✅" , "Finished turnoverData", "\n")



## redzoneData ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating redzoneData")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/redzoneData.R")

redzoneData <- compute_redzone_data(gameDataLong, pbpData)
#save(redzoneData, file = "./app/data/redzoneData.rda")
save(redzoneData, file = "~/Desktop/NFL Analysis Data/UpdateData/redzoneData.rda")

cat("\n", "✅" , "Finished redzoneData", "\n")

c(eloData, seasonWeekStandings, epaData, scoresData, seriesData, turnoverData, redzoneData)

## modData ----
# About 8 minutes
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating modData and modDataLong")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

system.time(
  source("./app/data-raw/modData.R")
)
#save(modDataLong, file = "./app/data/modDataLong.rda")
save(modData, file = "./app/data/modData.rda")

cat("\n", "✅" , "Finished modData and modDataLong", "\n")



# Final message ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "\n",
    "✅" ,  "DATA UPDATE COMPLETE",
    "-", attributes(gameData)$nflverse_timestamp
)



