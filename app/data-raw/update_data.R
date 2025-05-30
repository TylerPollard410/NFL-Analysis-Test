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
all_seasons <- 2006:most_recent_season()
teams_data <- load_teams(current = FALSE)

game_IDs <- load_schedules(seasons = all_seasons) |>
  filter(!is.na(result)) |>
  pull(game_id)

env_vars <- ls()

# Load/Updata Data ----
## game_data -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating game_data", "\n")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_game_data.R")
game_data <- compute_game_data(
  seasons = all_seasons
)

cat("\n", "✅" , "Finished game_data", "\n")



## game_data_long -------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating game_data_long",)

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_game_data_long.R")
game_data_long <- compute_game_data_long(
  game_df = game_data
)

cat("\n", "✅" , "Finished game_data_long", "\n")



## pbp_data ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating pbp_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_pbp_data.R")
pbp_data <- compute_pbp_data(
  seasons = all_seasons
)
pbp_data_date <- attributes(pbp_data)$nflverse_timestamp

cat("\n", "✅" , "Finished pbp_data", "\n")

### Update
# gameIDsCurrent <- pbp_data |>
#   filter(season == 2024) |>
#   distinct(game_id) |>
#   pull(game_id)
#
# gameIDsUpdate <- game_data |>
#   filter(season == 2024) |>
#   filter(!is.na(result)) |>
#   filter(!(game_id %in% gameIDsCurrent)) |>
#   pull(game_id)
#
# pbp_dataUpdate <- load_pbp(seasons = most_recent_season()) |>
#   filter(game_id %in% gameIDsUpdate)
#
# if(nrow(pbp_dataUpdate) > 0){
#   dbAppendTable(con, "pbp_data", pbp_dataUpdate)
# }else{
#   print("No play-by-play data to update")
# }
#
# pbp_data_tbl <- tbl(con, "pbp_data")
#
# pbp_dataUpdateRows <- pbp_data_tbl |>
#   pull(game_id) |>
#   length()
#
# pbp_dataUpdateCols <- length(pbp_data_tbl$lazy_query$vars)
#
# pbp_dataDate <- attributes(pbp_data)$nflverse_timestamp
# paste0("pbp_data updated ", pbp_dataDate,
#        " with ", pbp_dataUpdateRows, " rows and ",
#        pbp_dataUpdateCols, " cols.")
#
# save(pbp_dataDate, file = "./app/data/pbp_dataDate.rda")

#dbListTables(con)
#rm(pbp_dataUpdate, pbp_data_tbl, pbp_dataUpdateRows, pbp_dataUpdateCols)



## player_offense_data ---------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating player_offense_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_player_data.R")
player_offense_data <- compute_player_data(
  seasons = all_seasons, 
  game_long_df = game_data_long,
  stat = "offense"
)
save(player_offense_data, file = "./app/data/player_offense_data.rda")
#save(player_offense_data, file = "~/Desktop/NFL Analysis Data/UpdateData/player_offense_data.rda")

cat("\n", "✅" , "Finished player_offense_data", "\n")



## season_standings_data ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating season_standings_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/calc_srs_ratings.R")
source("./app/data-raw/compute_season_standings_data.R")
season_standings_data <- compute_season_standings_data(
  game_df = game_data,
  tol = 1e-3, 
  max_iter = 200,
  print_message = TRUE
)
save(season_standings_data, file = "./app/data/season_standings_data.rda")
#save(season_standings, file = "~/Desktop/NFL Analysis Data/UpdateData/season_standings.rda")

cat("\n", "✅" , "Finished season_standings_data", "\n")



## weekly_standings_data ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating weekly_standings_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/calc_weekly_standings.R")
source("./app/data-raw/compute_weekly_standings_data.R")
## reset - recompute_all
# B1 TRUE - TRUE
# B2 TRUE - FALSE
# B3 FALSE - TRUE
# B4 FALSE - FALSE
# B5 Numeric (N) - TRUE
# B6 Numeric (N) - FALSE

weekly_standings_data <- compute_weekly_standings_data(
  game_data,
  tol = 1e-3,
  max_iter = 200,
  reset = TRUE,
  recompute_all = FALSE, #TRUE,
  #cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings.rda" 
  cache_file = "./app/data/weekly_standings_data.rda"
)
save(weekly_standings_data, file = "./app/data/weekly_standings_data.rda")
#save(weekly_standings, file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings.rda")
# 
# weekly_standings_roll20 <- update_weekly_standings(
#   game_data,
#   tol = 1e-3,
#   max_iter = 200,
#   reset = 20,
#   recompute_all = FALSE, #TRUE,
#   cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings_roll20.rda" #"./app/data/weekly_standings_roll20.rda"
# )
# #save(weekly_standings, file = "./app/data/weekly_standings.rda")
# save(weekly_standings_roll20, file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings_roll20.rda")
# 
# weekly_standings_roll10 <- update_weekly_standings(
#   game_data,
#   tol = 1e-3,
#   max_iter = 200,
#   reset = 10,
#   recompute_all = FALSE, #TRUE,
#   cache_file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings_roll10.rda" #"./app/data/weekly_standings_roll10.rda"
# )
# #save(weekly_standings, file = "./app/data/weekly_standings.rda")
# save(weekly_standings_roll10, file = "~/Desktop/NFL Analysis Data/UpdateData/weekly_standings_roll10.rda")

cat("\n", "✅" , "Finished weekly_standings", "\n")



## elo_data ------------------------------
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating elo_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

# source("./app/data-raw/elo_data.R")
source("./app/data-raw/calc_elo_ratings.R")
source("./app/data-raw/compute_elo_data.R")

elo_result <- compute_elo_data(
  game_df = game_data,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE,
  recompute_all = FALSE,
  #cache_file = "~/Desktop/NFL Analysis Data/UpdateData/elo_data.rda",
  cache_file = "./app/data/elo_data.rda",
  season_factor = 0.6
)
elo_data   <- elo_result$elo_history
eloFinals <- elo_result$final_elos


rm(elo_result)
#elo_data0   <- elo_result$elo_history
#eloFinals0 <- elo_result$final_elos
elo_data1   <- elo_result$elo_history
eloFinals1 <- elo_result$final_elos
save(elo_data, eloFinals, file = "./app/data/elo_data.rda")
#save(elo_data, eloFinals, file = "~/Desktop/NFL Analysis Data/UpdateData/elo_data.rda")

cat("\n", "✅" , "Finished elo_data", "\n")


## epa_data ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating epa_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/calc_epa_ratings.R")
source("./app/data-raw/compute_epa_data.R")

epa_data <- compute_epa_data(
  pbp_df = pbp_data, 
  scaled_wp = FALSE
  )
save(epa_data, file = "./app/data/epa_data.rda")
#save(epa_data, file = "~/Desktop/NFL Analysis Data/UpdateData/epa_data.rda")

cat("\n", "✅" , "Finished epa_data", "\n")



## scores_data ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating scores_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/calc_weekly_team_stats.R")
source("./app/data-raw/compute_scores_data.R")

nflStatsWeek_loc   <- "~/Desktop/NFL Analysis Data/UpdateData/nflStatsWeek.rda"
scores_data <- compute_scores_data(
  game_long_df = game_data_long,
  pbp_df = pbp_data,
  seasons = all_seasons, 
  sum_level = "week",
  stat_level = "team",
  season_level = "REG+POST",
  stats_loc = nflStatsWeek_loc,
  recompute_all = FALSE
)
save(scores_data, file = "./app/data/scores_data.rda")
#save(scores_data, file = "~/Desktop/NFL Analysis Data/UpdateData/scores_data.rda")

cat("\n", "✅" , "Finished scores_data", "\n")



## series_data ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating series_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/calc_weekly_series_stats.R")
source("./app/data-raw/compute_series_data.R")

nflSeriesWeek_loc   <- "~/Desktop/NFL Analysis Data/UpdateData/nflSeriesWeek.rda"
series_data <- compute_series_data(
  game_data_long, pbp_data, 
  series_loc = nflSeriesWeek_loc, 
  recompute_all = FALSE
)
save(series_data, file = "./app/data/series_data.rda")
#save(series_data, file = "~/Desktop/NFL Analysis Data/UpdateData/series_data.rda")

cat("\n", "✅" , "Finished series_data", "\n")



## turnover_data ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating turnover_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_turnover_data.R")

turnover_data <- compute_turnover_data(
  game_long_df = game_data_long, 
  pbp_df = pbp_data
  )
save(turnover_data, file = "./app/data/turnover_data.rda")
#save(turnover_data, file = "~/Desktop/NFL Analysis Data/UpdateData/turnover_data.rda")

cat("\n", "✅" , "Finished turnover_data", "\n")



## redzone_data ----
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n", "Generating redzone_data")

env_vars <- ls()
rm(list = setdiff(ls(), env_vars))

source("./app/data-raw/compute_redzone_data.R")

redzone_data <- compute_redzone_data(
  game_long_df = game_data_long, 
  pbp_df = pbp_data
)
save(redzone_data, file = "./app/data/redzone_data.rda")
#save(redzone_data, file = "~/Desktop/NFL Analysis Data/UpdateData/redzone_data.rda")

cat("\n", "✅" , "Finished redzone_data", "\n")

#c(elo_data, weekly_standings, epa_data, scores_data, series_data, turnover_data, redzone_data)

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
    "-", attributes(game_data)$nflverse_timestamp
)



