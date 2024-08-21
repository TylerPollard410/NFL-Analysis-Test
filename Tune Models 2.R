### Tune xgboost parameters

#load libraries
library(tidyverse)
library(data.table)
library(xgboost)
library(mlr)
library(caret)
library(nflverse)
# library(nflfastR)
# library(nflseedR)
# library(nflreadr)
# library(nflplotR)
library(lubridate)
library(plyr)
library(stringr)
library(imputeR)
library(pracma)
library(ModelMetrics)
library(useful)
library(tictoc)
library(progressr)


# Read in current data
games_df <- load_schedules()

# Load play by play data from downloaded files (takes about 9 minutes)
tic()
pbp_data <- list()
pbp_files <- list.files("pbp")
for(i in pbp_files){
  pbp_season <- str_extract(i, "[:digit:]+")
  pbp_data[[i]] <- with_progress(readRDS(paste0("pbp/", i)))
}
pbp_df <- rbindlist(pbp_data2)
toc()

# Load team ids
teams <- load_teams()

# Update teams that have moved
games_2005_df$away_team <- ifelse(games_2005_df$away_team == "OAK", "LV",
                                  ifelse(games_2005_df$away_team == "SD", "LAC",
                                         ifelse(games_2005_df$away_team == "STL", "LA", games_2005_df$away_team)))
games_2005_df$home_team <- ifelse(games_2005_df$home_team == "OAK", "LV",
                                  ifelse(games_2005_df$home_team == "SD", "LAC",
                                         ifelse(games_2005_df$home_team == "STL", "LA", games_2005_df$home_team)))

# Standings
standings <- read_csv("http://www.habitatring.com/standings.csv")

# Predictors to add
# Win Streak
# Strength of Schedule
# Offensive Efficiency 
# Defensive Efficiency


pbp_2005 %>% filter(touchdown == 1 & down == 3) %>% select(down, play_type, yards_gained, ydstogo, third_down_converted)

# Calculate successful plays
# 1st down = get 40% of remaining yards
# 2nd down = get 66% of remaining yards
# 3rd and 4th down = get 100% of remaining yards (first down)
pbp_2005_new <- pbp_2005 %>%
  filter(
    !is.na(down)
  ) %>%
  mutate(
    successful_yards_togo = ifelse(down == 1, ydstogo*.4,
                                   ifelse(down == 2, ydstogo*2/3, ydstogo)),
    successful_offensive_play = ifelse(down == 1 & (play_type == "pass" | play_type == "run") & yards_gained >= successful_yards_togo, 1,
                                   ifelse(down == 2 & (play_type == "pass" | play_type == "run") & yards_gained >= successful_yards_togo, 1,
                                          ifelse(down == 3 & (play_type == "pass" | play_type == "run") & third_down_converted == 1, 1,
                                                 ifelse(down == 4 & (play_type == "pass" | play_type == "run") & fourth_down_converted == 1, 1, 0)))),
    successful_defensive_play = ifelse(successful_offensive_play == 0, 1, 0),
    yards_above_success = yards_gained - successful_yards_togo
  ) %>%
  select(
    game_id,
    old_game_id,
    season,
    week,
    home_team,
    away_team,
    posteam,
    down,
    ydstogo,
    successful_yards_togo,
    yrdln,
    play_type,
    yards_gained,
    third_down_converted,
    fourth_down_converted,
    successful_offensive_play,
    successful_defensive_play,
    yards_above_success
  )

# Yards per game
pbp_all_yards <- ddply(pbp_2005_new, .(game_id, old_game_id, season, week, home_team, away_team, posteam, play_type), summarize,
                       yards = sum(yards_gained),
                       attempts = length(play_type),
                       successful_offensive_plays = sum(successful_offensive_play),
                       successful_defensive_plays = sum(successful_defensive_play),
                       offensive_efficiency = successful_offensive_plays/attempts,
                       defensive_efficiency = successful_defensive_plays/attempts,
                       yards_above_success = sum(yards_above_success)
) 

# Filter to only run and pass plays
pbp_yards <- pbp_all_yards %>% filter(play_type %in% c("pass", "run")) %>% arrange(old_game_id)

pbp_unique_games <- pbp_yards %>% distinct(game_id, old_game_id, season, week, home_team, away_team)

# Filter to get away and home ypg
# Home Pass Metrics
home_pass_yards <- pbp_yards %>% filter(home_team == posteam & play_type == "pass") %>% select(yards) %>% dplyr::rename("home_pass_yards" = "yards")
home_pass_explosive_yards <- pbp_yards %>% filter(home_team == posteam & play_type == "pass") %>% select(yards_above_success) %>% dplyr::rename("home_pass_explosive_yards" = "yards_above_success")
home_pass_off_efficiency <- pbp_yards %>% filter(home_team == posteam & play_type == "pass") %>% select(offensive_efficiency) %>% dplyr::rename("home_pass_off_efficiency" = "offensive_efficiency")
home_pass_def_efficiency <- pbp_yards %>% filter(away_team == posteam & play_type == "pass") %>% select(defensive_efficiency) %>% dplyr::rename("home_pass_def_efficiency" = "defensive_efficiency")

# Away Pass Metrics
away_pass_yards <- pbp_yards %>% filter(away_team == posteam & play_type == "pass") %>% select(yards) %>% dplyr::rename("away_pass_yards" = "yards")
away_pass_off_efficiency <- pbp_yards %>% filter(away_team == posteam & play_type == "pass") %>% select(offensive_efficiency) %>% dplyr::rename("away_pass_off_efficiency" = "offensive_efficiency")
away_pass_def_efficiency <- pbp_yards %>% filter(home_team == posteam & play_type == "pass") %>% select(defensive_efficiency) %>% dplyr::rename("away_pass_def_efficiency" = "defensive_efficiency")
away_pass_explosive_yards <- pbp_yards %>% filter(away_team == posteam & play_type == "pass") %>% select(yards_above_success) %>% dplyr::rename("away_pass_explosive_yards" = "yards_above_success")

# Home Run Metrics
home_run_yards <- pbp_yards %>% filter(home_team == posteam & play_type == "run") %>% select(yards) %>% dplyr::rename("home_run_yards" = "yards")
home_run_off_efficiency <- pbp_yards %>% filter(home_team == posteam & play_type == "run") %>% select(offensive_efficiency) %>% dplyr::rename("home_run_off_efficiency" = "offensive_efficiency")
home_run_def_efficiency <- pbp_yards %>% filter(away_team == posteam & play_type == "run") %>% select(defensive_efficiency) %>% dplyr::rename("home_run_def_efficiency" = "defensive_efficiency")
home_run_explosive_yards <- pbp_yards %>% filter(home_team == posteam & play_type == "run") %>% select(yards_above_success) %>% dplyr::rename("home_run_explosive_yards" = "yards_above_success")

# Away Run Metrics
away_run_yards <- pbp_yards %>% filter(away_team == posteam & play_type == "run") %>% select(yards) %>% dplyr::rename("away_run_yards" = "yards")
away_run_off_efficiency <- pbp_yards %>% filter(away_team == posteam & play_type == "run") %>% select(offensive_efficiency) %>% dplyr::rename("away_run_off_efficiency" = "offensive_efficiency")
away_run_def_efficiency <- pbp_yards %>% filter(home_team == posteam & play_type == "run") %>% select(defensive_efficiency) %>% dplyr::rename("away_run_def_efficiency" = "defensive_efficiency")
away_run_explosive_yards <- pbp_yards %>% filter(away_team == posteam & play_type == "run") %>% select(yards_above_success) %>% dplyr::rename("away_run_explosive_yards" = "yards_above_success")

pbp_yards_reduced <- cbind(pbp_unique_games, 
                   home_pass_yards,
                   home_pass_explosive_yards,
                   home_pass_off_efficiency,
                   home_pass_def_efficiency,
                   away_pass_yards,
                   away_pass_explosive_yards,
                   away_pass_off_efficiency,
                   away_pass_def_efficiency,
                   home_run_yards,
                   home_run_explosive_yards,
                   home_run_off_efficiency,
                   home_run_def_efficiency,
                   away_run_yards,
                   away_run_explosive_yards,
                   away_run_off_efficiency,
                   away_run_def_efficiency)

pbp_yards_reduced <- cbind(pbp_unique_games, 
                           home_pass_yards,
                           home_pass_explosive_yards,
                           home_pass_off_efficiency,
                           away_pass_def_efficiency,
                           
                           away_pass_yards,
                           away_pass_explosive_yards,
                           away_pass_off_efficiency,
                           home_pass_def_efficiency,
                           
                           home_run_yards,
                           home_run_explosive_yards,
                           home_run_off_efficiency,
                           away_run_def_efficiency,
                           
                           away_run_yards,
                           away_run_explosive_yards,
                           away_run_off_efficiency,
                           home_run_def_efficiency)

# Create data set for each team's metrics
teams_ypg <- list()
for(team in sort(unique(pbp_yards_reduced$away_team))){
  teams_ypg[[team]] <- pbp_yards_reduced %>%
    filter(home_team == team | away_team == team) %>%
    summarize(
      game_id = game_id,
      old_game_id = old_game_id,
      season = season,
      week = week,
      location = ifelse(home_team == team, "Home", "Away"),
      # Pass Yards
      pass_yards = ifelse(home_team == team, home_pass_yards, away_pass_yards),
      pass_explosive_yards = ifelse(home_team == team, home_pass_explosive_yards, away_pass_explosive_yards),
      pass_off_efficiency = ifelse(home_team == team, home_pass_off_efficiency, away_pass_off_efficiency),
      pass_yards_allowed = ifelse(home_team == team, away_pass_yards, home_pass_yards),
      pass_explosive_yards_allowed = ifelse(home_team == team, away_pass_explosive_yards, home_pass_explosive_yards),
      pass_def_efficiency = ifelse(home_team == team, home_pass_def_efficiency, away_pass_def_efficiency),
      # Run Yards
      run_yards = ifelse(home_team == team, home_run_yards, away_run_yards),
      run_explosive_yards = ifelse(home_team == team, home_run_explosive_yards, away_run_explosive_yards),
      run_off_efficiency = ifelse(home_team == team, home_run_off_efficiency, away_run_off_efficiency),
      run_yards_allowed = ifelse(home_team == team, away_run_yards, home_run_yards),
      run_explosive_yards_allowed = ifelse(home_team == team, away_run_explosive_yards, home_run_explosive_yards),
      run_def_efficiency = ifelse(home_team == team, home_run_def_efficiency, away_run_def_efficiency)
    )
  # Games played
  team_games_played <- c()
  # Team offensive pass yards
  team_pass_yards <- c()
  team_pass_ypg <- c()
  team_pass_explosive_yards <- c()
  team_pass_explosive_ypg <- c()
  team_pass_off_efficiency <- c()
  team_pass_off_epg <- c()
  # Team offensive run yards
  team_run_yards <- c()
  team_run_ypg <- c()
  team_run_explosive_yards <- c()
  team_run_explosive_ypg <- c()
  team_run_off_efficiency <- c()
  team_run_off_epg <- c()
  # Team defensive pass yards
  allowed_pass_yards <- c()
  allowed_pass_ypg <- c()
  allowed_pass_explosive_yards <- c()
  allowed_pass_explosive_ypg <- c()
  team_pass_def_efficiency <- c()
  team_pass_def_epg <- c()
  # Team defensive run yards
  allowed_run_yards <- c()
  allowed_run_ypg <- c()
  allowed_run_explosive_yards <- c()
  allowed_run_explosive_ypg <- c()
  team_run_def_efficiency <- c()
  team_run_def_epg <- c()
  
  for(i in unique(teams_ypg[[team]]$season)){
    teams_ypg_season <- teams_ypg[[team]] %>% filter(season == i)
    for(j in 1:length(unique(teams_ypg_season$week))){
      # Games played
      season_games_played <- j-1
      team_games_played <- c(team_games_played, season_games_played)
      
      ## Passing Metrics For
      # Total Yards
      season_total_pass_yards <- sum(teams_ypg_season$pass_yards[1:j])
      team_pass_yards <- c(team_pass_yards, season_total_pass_yards)
      # Total Explosive Yards
      season_total_pass_explosive_yards <- sum(teams_ypg_season$pass_explosive_yards[1:j])
      team_pass_explosive_yards <- c(team_pass_explosive_yards, season_total_pass_explosive_yards)
      # Offensive Efficiency
      season_pass_off_efficiency <- sum(teams_ypg_season$pass_off_efficiency[1:j])
      team_pass_off_efficiency <- c(team_pass_off_efficiency, season_pass_off_efficiency)
      
      ## Running Metrics For
      # Total Yards
      season_total_run_yards <- sum(teams_ypg_season$run_yards[1:j])
      team_run_yards <- c(team_run_yards, season_total_run_yards)
      # Total Explosive Yards
      season_total_run_explosive_yards <- sum(teams_ypg_season$run_explosive_yards[1:j])
      team_run_explosive_yards <- c(team_run_explosive_yards, season_total_run_explosive_yards)
      # Offensive Efficiency
      season_run_off_efficiency <- sum(teams_ypg_season$run_off_efficiency[1:j])
      team_run_off_efficiency <- c(team_run_off_efficiency, season_run_off_efficiency)
      
      ## Pass Metrics Against
      # Total Yards Allowed
      season_allowed_pass_yards <- sum(teams_ypg_season$pass_yards_allowed[1:j])
      allowed_pass_yards <- c(allowed_pass_yards, season_allowed_pass_yards)
      # Total Explosive Yards Allowed
      season_allowed_pass_explosive_yards <- sum(teams_ypg_season$pass_explosive_yards_allowed[1:j])
      allowed_pass_explosive_yards <- c(allowed_pass_explosive_yards, season_allowed_pass_explosive_yards)
      # Defensive Efficiency
      season_pass_def_efficiency <- sum(teams_ypg_season$pass_def_efficiency[1:j])
      team_pass_def_efficiency <- c(team_pass_def_efficiency, season_pass_def_efficiency)
      
      ## Running Metrics Against
      # Total Yards Allowed
      season_allowed_run_yards <- sum(teams_ypg_season$run_yards_allowed[1:j])
      allowed_run_yards <- c(allowed_run_yards, season_allowed_run_yards)
      # Total Explosive Yards Allowed
      season_allowed_run_explosive_yards <- sum(teams_ypg_season$run_explosive_yards_allowed[1:j])
      allowed_run_explosive_yards <- c(allowed_run_explosive_yards, season_allowed_run_explosive_yards)
      # Defensive Efficiency
      season_run_def_efficiency <- sum(teams_ypg_season$run_def_efficiency[1:j])
      team_run_def_efficiency <- c(team_run_def_efficiency, season_run_def_efficiency)
      
      ## YPG
      if(season_games_played == 0){
        # Pass Metrics For
        season_pass_ypg <- NA
        season_pass_explosive_ypg <- NA
        season_pass_off_epg <- NA
        
        # Run Metrics For
        season_run_ypg <- NA
        season_run_explosive_ypg <- NA
        season_run_off_epg <- NA
        
        # Pass Metrics Against
        season_allowed_pass_ypg <- NA
        season_allowed_pass_explosive_ypg <- NA
        season_pass_def_epg <- NA
        
        # Run Metrics Against
        season_allowed_run_ypg <- NA
        season_allowed_run_explosive_ypg <- NA
        season_run_def_epg <- NA
      }else{
        # Pass Metrics For
        season_pass_ypg <- sum(teams_ypg_season$pass_yards[1:(j-1)])/season_games_played
        season_pass_explosive_ypg <- sum(teams_ypg_season$pass_explosive_yards[1:(j-1)])/season_games_played
        season_pass_off_epg <- sum(teams_ypg_season$pass_off_efficiency[1:(j-1)])/season_games_played
        
        # Run Metrics For
        season_run_ypg <- sum(teams_ypg_season$run_yards[1:(j-1)])/season_games_played
        season_run_explosive_ypg <- sum(teams_ypg_season$run_explosive_yards[1:(j-1)])/season_games_played
        season_run_off_epg <- sum(teams_ypg_season$run_off_efficiency[1:(j-1)])/season_games_played
        
        # Pass Metrics Against
        season_allowed_pass_ypg <- sum(teams_ypg_season$pass_yards_allowed[1:(j-1)])/season_games_played
        season_allowed_pass_explosive_ypg <- sum(teams_ypg_season$pass_explosive_yards_allowed[1:(j-1)])/season_games_played
        season_pass_def_epg <- sum(teams_ypg_season$pass_def_efficiency[1:(j-1)])/season_games_played
        
        # Run Metrics Against
        season_allowed_run_ypg <- sum(teams_ypg_season$run_yards_allowed[1:(j-1)])/season_games_played
        season_allowed_run_explosive_ypg <- sum(teams_ypg_season$run_explosive_yards_allowed[1:(j-1)])/season_games_played
        season_run_def_epg <- sum(teams_ypg_season$run_def_efficiency[1:(j-1)])/season_games_played
        
      }
      # Pass Metrics For
      team_pass_ypg <- c(team_pass_ypg, season_pass_ypg)
      team_pass_explosive_ypg <- c(team_pass_explosive_ypg, season_pass_explosive_ypg)
      team_pass_off_epg <- c(team_pass_off_epg, season_pass_off_epg)
      
      # Run Metrics For
      team_run_ypg <- c(team_run_ypg, season_run_ypg)
      team_run_explosive_ypg <- c(team_run_explosive_ypg, season_run_explosive_ypg)
      team_run_off_epg <- c(team_run_off_epg, season_run_off_epg)
      
      # Pass Metrics Against
      allowed_pass_ypg <- c(allowed_pass_ypg, season_allowed_pass_ypg)
      allowed_pass_explosive_ypg <- c(allowed_pass_explosive_ypg, season_allowed_pass_explosive_ypg)
      team_pass_def_epg <- c(team_pass_def_epg, season_pass_def_epg)
      
      # Run Metrics Against
      allowed_run_ypg <- c(allowed_run_ypg, season_allowed_run_ypg)
      allowed_run_explosive_ypg <- c(allowed_run_explosive_ypg, season_allowed_run_explosive_ypg)
      team_run_def_epg <- c(team_run_def_epg, season_run_def_epg)
    }
  }
  # Games Played
  teams_ypg[[team]]$games_played <- team_games_played
  
  # Pass Metrics For
  teams_ypg[[team]]$total_pass_yards <- team_pass_yards
  teams_ypg[[team]]$pass_ypg <- team_pass_ypg
  teams_ypg[[team]]$pass_ypg2 <- movavg(teams_ypg[[team]]$pass_yards, n = 8, type = "e")
  teams_ypg[[team]]$total_pass_explosive_yards <- team_pass_explosive_yards
  teams_ypg[[team]]$pass_explosive_ypg <- team_pass_explosive_ypg
  teams_ypg[[team]]$pass_explosive_ypg2 <- movavg(teams_ypg[[team]]$pass_explosive_yards, n = 8, type = "e")
  teams_ypg[[team]]$pass_off_epg <- team_pass_off_epg
  teams_ypg[[team]]$pass_off_epg2 <- movavg(teams_ypg[[team]]$pass_off_efficiency, n = 8, type = "e")
  
  # Run Metrics For
  teams_ypg[[team]]$total_run_yards <- team_run_yards
  teams_ypg[[team]]$run_ypg <- team_run_ypg
  teams_ypg[[team]]$run_ypg2 <- movavg(teams_ypg[[team]]$run_yards, n = 8, type = "e")
  teams_ypg[[team]]$total_run_explosive_yards <- team_run_explosive_yards
  teams_ypg[[team]]$run_explosive_ypg <- team_run_explosive_ypg
  teams_ypg[[team]]$run_explosive_ypg2 <- movavg(teams_ypg[[team]]$run_explosive_yards, n = 8, type = "e")
  teams_ypg[[team]]$run_off_epg <- team_run_off_epg
  teams_ypg[[team]]$run_off_epg2 <- movavg(teams_ypg[[team]]$run_off_efficiency, n = 8, type = "e")
  
  # Pass Metrics Against
  teams_ypg[[team]]$allowed_total_pass_yards <- allowed_pass_yards
  teams_ypg[[team]]$allowed_pass_ypg <- allowed_pass_ypg
  teams_ypg[[team]]$allowed_pass_ypg2 <- movavg(teams_ypg[[team]]$pass_yards_allowed, n = 8, type = "e")
  teams_ypg[[team]]$allowed_total_pass_explosive_yards <- allowed_pass_explosive_yards
  teams_ypg[[team]]$allowed_pass_explosive_ypg <- allowed_pass_explosive_ypg
  teams_ypg[[team]]$allowed_pass_explosive_ypg2 <- movavg(teams_ypg[[team]]$pass_explosive_yards_allowed, n = 8, type = "e")
  teams_ypg[[team]]$pass_def_epg <- team_pass_def_epg
  teams_ypg[[team]]$pass_def_epg2 <- movavg(teams_ypg[[team]]$pass_def_efficiency, n = 8, type = "e")
  
  # Run Metrics Against
  teams_ypg[[team]]$allowed_total_run_yards <- allowed_run_yards
  teams_ypg[[team]]$allowed_run_ypg <- allowed_run_ypg
  teams_ypg[[team]]$allowed_run_ypg2 <- movavg(teams_ypg[[team]]$run_yards_allowed, n = 8, type = "e")
  teams_ypg[[team]]$allowed_total_run_explosive_yards <- allowed_run_explosive_yards
  teams_ypg[[team]]$allowed_run_explosive_ypg <- allowed_run_explosive_ypg
  teams_ypg[[team]]$allowed_run_explosive_ypg2 <- movavg(teams_ypg[[team]]$run_explosive_yards_allowed, n = 8, type = "e")
  teams_ypg[[team]]$run_def_epg <- team_run_def_epg
  teams_ypg[[team]]$run_def_epg2 <- movavg(teams_ypg[[team]]$run_def_efficiency, n = 8, type = "e")
  
  # Shift rows up to match with week played
  teams_ypg[[team]] <- shift.column(teams_ypg[[team]],
                                    c("pass_ypg2",
                                      "pass_explosive_ypg2",
                                      "pass_off_epg2",
                                      "run_ypg2",
                                      "run_explosive_ypg2",
                                      "run_off_epg2",
                                      "allowed_pass_ypg2",
                                      "allowed_pass_explosive_ypg2",
                                      "pass_def_epg2",
                                      "allowed_run_ypg2",
                                      "allowed_run_explosive_ypg2",
                                      "run_def_epg2"
                                    ),
                                    newNames = c(
                                      "ewm_pass_ypg",
                                      "ewm_pass_explosive_ypg",
                                      "ewm_pass_off_epg",
                                      "ewm_run_ypg",
                                      "ewm_run_explosive_ypg",
                                      "ewm_run_off_epg",
                                      "ewm_allowed_pass_ypg",
                                      "ewm_allowed_pass_explosive_ypg",
                                      "ewm_pass_def_epg",
                                      "ewm_allowed_run_ypg",
                                      "ewm_allowed_run_explosive_ypg",
                                      "ewm_run_def_epg"
                                    ),
                                    up = FALSE)
  teams_ypg[[team]] <- teams_ypg[[team]] %>% filter(season >= 2006)
}

# Create df of all team ypgs
teams_ypg_df <- bind_rows(teams_ypg, .id = "team")

#rmse_check <- teams_ypg_df %>% filter(season == 2021, team == "BAL")
#rmse(rmse_check$pass_yards, rmse_check$ewm_pass_ypg)

### Tuning n for movavg
# BAL <- teams_ypg[["BAL"]]
# rmse_df <- data.frame(n = c(), rmse_pass = c(), rmse_run = c())
# for(n in 2:16){
#   BAL_temp <- BAL %>%
#     mutate(
#       allowed_pass_ypg3 = movavg(BAL$pass_yards_allowed, n = n, type = "e"),
#       allowed_run_ypg3 = movavg(BAL$run_yards_allowed, n = n, type = "e")
#     )
#   BAL_temp <- shift.column(BAL_temp, c("allowed_pass_ypg3", "allowed_run_ypg3"), 
#                            newNames = c("allowed_pass_ypg4", "allowed_run_ypg4"),
#                            up = FALSE)
#   BAL_temp <- BAL_temp %>% filter(season >= 2006)
#   rmse_pass <- rmse(BAL_temp$pass_yards_allowed, BAL_temp$allowed_pass_ypg4)
#   rmse_run <- rmse(BAL_temp$run_yards_allowed, BAL_temp$allowed_run_ypg4)
#   df <- data.frame(n, rmse_pass, rmse_run)
#   rmse_df <- rbind(rmse_df, df)
# }
# 
# plot(rmse_df$n, rmse_df$rmse_pass, type = "b")
# plot(rmse_df$n, rmse_df$rmse_run, type = "b")

# Win variables
teams_wins <- list()
for(team in sort(unique(games_2005_df$away_team))){
  # Generate list of all teams with wins
  teams_wins[[team]] <- games_2005_df %>% 
    filter(home_team == team | away_team == team) %>%
    mutate(
      location = ifelse(home_team == team, "Home", "Away"),
      points_scored = ifelse(home_team == team, home_score, away_score),
      points_allowed = ifelse(home_team == team, away_score, home_score),
      winning_team = ifelse(home_score > away_score, home_team,
                            ifelse(home_score < away_score, away_team, "TIE")),
      win = ifelse(winning_team == team, 1, 
                   ifelse(winning_team == "TIE", .5, 0))
    ) %>%
    select(game_id, old_game_id, season, week, location, home_team, home_score, away_team, away_score, points_scored, points_allowed, winning_team, win)
  team_games_played <- c()
  team_total_points <- c()
  team_ppg <- c()
  allowed_total_points <- c()
  allowed_ppg <- c()
  team_total_wins <- c()
  team_win_percentage <- c()
  for(i in unique(teams_wins[[team]]$season)){
    teams_wins_season <- teams_wins[[team]] %>% filter(season == i)
    for(j in 1:length(unique(teams_wins_season$week))){
      season_games_played <- j-1
      team_games_played <- c(team_games_played, season_games_played)
      if(season_games_played == 0){
        season_total_points <- NA
        season_ppg <- NA
        season_allowed_points <- NA
        season_allowed_ppg <- NA
        season_total_wins <- NA
        season_win_percentage <- NA
      }else{
        season_total_points <- sum(teams_wins_season$points_scored[1:(j-1)])
        season_ppg <- season_total_points/season_games_played
        season_allowed_points <- sum(teams_wins_season$points_allowed[1:(j-1)])
        season_allowed_ppg <- season_allowed_points/season_games_played
        season_total_wins <- sum(teams_wins_season$win[1:(j-1)])
        season_win_percentage <- season_total_wins/season_games_played
      }
      team_total_points <- c(team_total_points, season_total_points)
      team_ppg <- c(team_ppg, season_ppg)
      allowed_total_points <- c(allowed_total_points, season_allowed_points)
      allowed_ppg <- c(allowed_ppg, season_allowed_ppg)
      team_total_wins <- c(team_total_wins, season_total_wins)
      team_win_percentage <- c(team_win_percentage, season_win_percentage)
    }
  }
  teams_wins[[team]]$games_played <- team_games_played
  teams_wins[[team]]$total_points <- team_total_points
  teams_wins[[team]]$ppg <- team_ppg
  teams_wins[[team]]$ppg2 <- movavg(teams_wins[[team]]$points_scored, n = 8, type = "e")
  teams_wins[[team]]$allowed_total_points <- allowed_total_points
  teams_wins[[team]]$allowed_ppg <- allowed_ppg
  teams_wins[[team]]$allowed_ppg2 <- movavg(teams_wins[[team]]$points_allowed, n = 8, type = "e")
  teams_wins[[team]]$total_wins <- team_total_wins
  teams_wins[[team]]$win_percentage <- team_win_percentage
  teams_wins[[team]]$win_percentage2 <- movavg(teams_wins[[team]]$win, n = 16, type = "e")
  teams_wins[[team]] <- shift.column(teams_wins[[team]], 
                                     c(
                                       "ppg2",
                                       "allowed_ppg2",
                                       "win_percentage2"
                                     ),
                                     newNames = c(
                                       "ewm_ppg",
                                       "ewm_allowed_ppg",
                                       "ewm_win_percentage"
                                     ),
                                     up = FALSE)
  teams_wins[[team]] <- teams_wins[[team]] %>% filter(season >= 2006)
}

teams_wins_df <- bind_rows(teams_wins, .id = "team")

games_2006_df <- games_2005_df %>% filter(season >= 2006)
teams_ypg_df <- teams_ypg_df %>% arrange(old_game_id)
teams_wins_df <- teams_wins_df %>% arrange(old_game_id)

final_games_df <- games_2006_df %>%
  mutate(
    ## Away Teams ypg df
    # Games Played
    away_games_played = teams_ypg_df %>% filter(location == "Away") %>% pull(games_played),
    
    # Passing Metrics For
    away_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_yards),
    away_total_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_pass_yards),
    away_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_ypg),
    away_ewm_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_pass_ypg),
    away_pass_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_explosive_yards),
    away_total_pass_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_pass_explosive_yards),
    away_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_explosive_ypg),
    away_ewm_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_pass_explosive_ypg),
    away_pass_off_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_off_epg),
    away_ewm_pass_off_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_pass_off_epg),
    
    # Running Metrics For
    away_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_yards),
    away_total_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_run_yards),
    away_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(run_ypg),
    away_ewm_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_run_ypg),
    away_run_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_explosive_yards),
    away_total_run_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_run_explosive_yards),
    away_run_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(run_explosive_ypg),
    away_ewm_run_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_run_explosive_ypg),
    away_run_off_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(run_off_epg),
    away_ewm_run_off_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_run_off_epg),
    
    # Passing Metrics Against
    away_allowed_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_yards_allowed),
    away_allowed_total_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_pass_yards),
    away_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_pass_ypg),
    away_ewm_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_pass_ypg),
    away_allowed_pass_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_explosive_yards_allowed),
    away_allowed_total_pass_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_pass_explosive_yards),
    away_allowed_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_pass_explosive_ypg),
    away_ewm_allowed_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_pass_explosive_ypg),
    away_pass_def_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_def_epg),
    away_ewm_pass_def_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_pass_def_epg),
    
    # Running Metrics Against
    away_allowed_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_yards_allowed),
    away_allowed_total_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_run_yards),
    away_allowed_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_run_ypg),
    away_ewm_allowed_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_run_ypg),
    away_allowed_run_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_explosive_yards_allowed),
    away_allowed_total_run_explosive_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_run_explosive_yards),
    away_allowed_run_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_run_explosive_ypg),
    away_ewm_allowed_run_explosive_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_run_explosive_ypg),
    away_run_def_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(run_def_epg),
    away_ewm_run_def_epg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_run_def_epg),
    
    
    # away_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_yards),
    # away_total_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_pass_yards),
    # away_total_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(total_run_yards),
    # away_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_ypg),
    # away_ewm_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_pass_ypg),
    # away_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(run_ypg),
    # away_ewm_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_run_ypg),
    # away_allowed_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(pass_yards_allowed),
    # away_allowed_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(run_yards_allowed),
    # away_allowed_total_pass_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_pass_yards),
    # away_allowed_total_run_yards = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_total_run_yards),
    # away_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_pass_ypg),
    # away_ewm_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_pass_ypg),
    # away_allowed_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(allowed_run_ypg),
    # away_ewm_allowed_run_ypg = teams_ypg_df %>% filter(location == "Away") %>% pull(ewm_allowed_run_ypg),
    
    ## Home teams ypg df
    # Games Played
    home_games_played = teams_ypg_df %>% filter(location == "Home") %>% pull(games_played),
    
    # Passing Metrics for
    home_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_yards),
    home_total_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_pass_yards),
    home_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_ypg),
    home_ewm_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_pass_ypg),
    home_pass_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_explosive_yards),
    home_total_pass_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_pass_explosive_yards),
    home_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_explosive_ypg),
    home_ewm_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_pass_explosive_ypg),
    home_pass_off_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_off_epg),
    home_ewm_pass_off_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_pass_off_epg),
    
    # Running Metrics For
    home_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(run_yards),
    home_total_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_run_yards),
    home_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(run_ypg),
    home_ewm_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_run_ypg),
    home_run_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(run_explosive_yards),
    home_total_run_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_run_explosive_yards),
    home_run_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(run_explosive_ypg),
    home_ewm_run_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_run_explosive_ypg),
    home_run_off_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(run_off_epg),
    home_ewm_run_off_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_run_off_epg),
    
    # Passing Metrics Against
    home_allowed_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_yards_allowed),
    home_allowed_total_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_pass_yards),
    home_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_pass_ypg),
    home_ewm_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_pass_ypg),
    home_allowed_pass_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_explosive_yards_allowed),
    home_allowed_total_pass_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_pass_explosive_yards),
    home_allowed_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_pass_explosive_ypg),
    home_ewm_allowed_pass_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_pass_explosive_ypg),
    home_pass_def_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_def_epg),
    home_ewm_pass_def_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_pass_def_epg),
    
    # Running Metrics Against
    home_allowed_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(run_yards_allowed),
    home_allowed_total_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_run_yards),
    home_allowed_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_run_ypg),
    home_ewm_allowed_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_run_ypg),
    home_allowed_run_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(run_explosive_yards_allowed),
    home_allowed_total_run_explosive_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_run_explosive_yards),
    home_allowed_run_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_run_explosive_ypg),
    home_ewm_allowed_run_explosive_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_run_explosive_ypg),
    home_run_def_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(run_def_epg),
    home_ewm_run_def_epg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_run_def_epg),
    
    
    # home_total_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_pass_yards),
    # home_total_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(total_run_yards),
    # home_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_ypg),
    # home_ewm_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_pass_ypg),
    # home_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(run_ypg),
    # home_ewm_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_run_ypg),
    # home_allowed_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(pass_yards_allowed),
    # home_allowed_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(run_yards_allowed),
    # home_allowed_total_pass_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_pass_yards),
    # home_allowed_total_run_yards = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_total_run_yards),
    # home_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_pass_ypg),
    # home_ewm_allowed_pass_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_pass_ypg),
    # home_allowed_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(allowed_run_ypg),
    # home_ewm_allowed_run_ypg = teams_ypg_df %>% filter(location == "Home") %>% pull(ewm_allowed_run_ypg),
    
    ## Away wins df
    away_season_points = teams_wins_df %>% filter(location == "Away") %>% pull(total_points),
    away_ppg = teams_wins_df %>% filter(location == "Away") %>% pull(ppg),
    away_ewm_ppg = teams_wins_df %>% filter(location == "Away") %>% pull(ewm_ppg),
    away_allowed_season_points = teams_wins_df %>% filter(location == "Away") %>% pull(allowed_total_points),
    away_allowed_ppg = teams_wins_df %>% filter(location == "Away") %>% pull(allowed_ppg),
    away_ewm_allowed_ppg = teams_wins_df %>% filter(location == "Away") %>% pull(ewm_allowed_ppg),
    away_wins = teams_wins_df %>% filter(location == "Away") %>% pull(total_wins),
    away_win_percentage = teams_wins_df %>% filter(location == "Away") %>% pull(win_percentage),
    away_ewm_win_percentage = teams_wins_df %>% filter(location == "Away") %>% pull(ewm_win_percentage),
    
    ## Home win df
    home_season_points = teams_wins_df %>% filter(location == "Home") %>% pull(total_points),
    home_ppg = teams_wins_df %>% filter(location == "Home") %>% pull(ppg),
    home_ewm_ppg = teams_wins_df %>% filter(location == "Home") %>% pull(ewm_ppg),
    home_allowed_season_points = teams_wins_df %>% filter(location == "Home") %>% pull(allowed_total_points),
    home_allowed_ppg = teams_wins_df %>% filter(location == "Home") %>% pull(allowed_ppg),
    home_ewm_allowed_ppg = teams_wins_df %>% filter(location == "Home") %>% pull(ewm_allowed_ppg),
    home_wins = teams_wins_df %>% filter(location == "Home") %>% pull(total_wins),
    home_win_percentage = teams_wins_df %>% filter(location == "Home") %>% pull(win_percentage),
    home_ewm_win_percentage = teams_wins_df %>% filter(location == "Home") %>% pull(ewm_win_percentage),
    
    # Odds
    away_moneyline_pct = ifelse(away_moneyline > 0, 100/(away_moneyline + 100), abs(away_moneyline)/(abs(away_moneyline) + 100)),
    home_moneyline_pct = ifelse(home_moneyline > 0, 100/(home_moneyline + 100), abs(home_moneyline)/(abs(home_moneyline) + 100)),
    moneyline_vegas_edge = (away_moneyline_pct + home_moneyline_pct) - 1,
    away_spread_pct = ifelse(away_spread_odds > 0, 100/(away_spread_odds + 100), abs(away_spread_odds)/(abs(away_spread_odds) + 100)),
    home_spread_pct = ifelse(home_spread_odds > 0, 100/(home_spread_odds + 100), abs(home_spread_odds)/(abs(home_spread_odds) + 100)),
    spread_vegas_edge = (away_spread_pct + home_spread_pct) - 1,
    over_pct = ifelse(over_odds > 0, 100/(over_odds + 100), abs(over_odds)/(abs(over_odds) + 100)),
    under_pct = ifelse(under_odds > 0, 100/(under_odds + 100), abs(under_odds)/(abs(under_odds) + 100)),
    total_vegas_edge = (over_pct + under_pct) - 1,
    
    # Winning Team
    winning_team = teams_wins_df %>% filter(location == "Home") %>% pull(winning_team)
  )


# Select predictor variables for model
games_df <- final_games_df %>% select(
  # Game Description Metrics
  game_id,
  old_game_id,
  season,
  game_type,
  week,
  weekday,
  gametime,
  away_team,
  home_team,
  location,
  div_game,
  roof,
  
  # Team Rest
  away_rest,
  home_rest,
  
  # Games Played
  away_games_played,
  home_games_played,
  
  # Point Metrics
  away_score,
  away_season_points,
  away_ewm_ppg,
  away_allowed_season_points,
  away_ewm_allowed_ppg,
  home_score,
  home_season_points,
  home_ewm_ppg,
  home_allowed_season_points,
  home_ewm_allowed_ppg,
  
  # Win Metrics
  away_wins,
  away_ewm_win_percentage,
  home_wins,
  home_ewm_win_percentage,
  
  ## Yard Metrics
  # Away Pass For
  away_pass_yards,
  away_total_pass_yards,
  away_pass_ypg,
  away_ewm_pass_ypg,
  away_pass_explosive_yards,
  away_total_pass_explosive_yards,
  away_pass_explosive_ypg,
  away_ewm_pass_explosive_ypg,
  away_pass_off_epg,
  away_ewm_pass_off_epg,
  
  # Away Run For
  away_run_yards,
  away_total_run_yards,
  away_run_ypg,
  away_ewm_run_ypg,
  away_run_explosive_yards,
  away_total_run_explosive_yards,
  away_run_explosive_ypg,
  away_ewm_run_explosive_ypg,
  away_run_off_epg,
  away_ewm_run_off_epg,
  
  # Away Pass Against
  away_allowed_pass_yards,
  away_allowed_total_pass_yards,
  away_allowed_pass_ypg,
  away_ewm_allowed_pass_ypg,
  away_allowed_pass_explosive_yards,
  away_allowed_total_pass_explosive_yards,
  away_allowed_pass_explosive_ypg,
  away_ewm_allowed_pass_explosive_ypg,
  away_pass_def_epg,
  away_ewm_pass_def_epg,
  
  # Away Run Against
  away_allowed_run_yards,
  away_allowed_total_run_yards,
  away_allowed_run_ypg,
  away_ewm_allowed_run_ypg,
  away_allowed_run_explosive_yards,
  away_allowed_total_run_explosive_yards,
  away_allowed_run_explosive_ypg,
  away_ewm_allowed_run_explosive_ypg,
  away_run_def_epg,
  away_ewm_run_def_epg,
  
  # Home Pass For
  home_pass_yards,
  home_total_pass_yards,
  home_pass_ypg,
  home_ewm_pass_ypg,
  home_pass_explosive_yards,
  home_total_pass_explosive_yards,
  home_pass_explosive_ypg,
  home_ewm_pass_explosive_ypg,
  home_pass_off_epg,
  home_ewm_pass_off_epg,
  
  # Home Run For
  home_run_yards,
  home_total_run_yards,
  home_run_ypg,
  home_ewm_run_ypg,
  home_run_explosive_yards,
  home_total_run_explosive_yards,
  home_run_explosive_ypg,
  home_ewm_run_explosive_ypg,
  home_run_off_epg,
  home_ewm_run_off_epg,
  
  # Home Pass Against
  home_allowed_pass_yards,
  home_allowed_total_pass_yards,
  home_allowed_pass_ypg,
  home_ewm_allowed_pass_ypg,
  home_allowed_pass_explosive_yards,
  home_allowed_total_pass_explosive_yards,
  home_allowed_pass_explosive_ypg,
  home_ewm_allowed_pass_explosive_ypg,
  home_pass_def_epg,
  home_ewm_pass_def_epg,
  
  # Home Run Against
  home_allowed_run_yards,
  home_allowed_total_run_yards,
  home_allowed_run_ypg,
  home_ewm_allowed_run_ypg,
  home_allowed_run_explosive_yards,
  home_allowed_total_run_explosive_yards,
  home_allowed_run_explosive_ypg,
  home_ewm_allowed_run_explosive_ypg,
  home_run_def_epg,
  home_ewm_run_def_epg,
  
  # Vegas Metrics
  away_moneyline,
  home_moneyline,
  away_moneyline_pct,
  home_moneyline_pct,
  moneyline_vegas_edge,
  spread_line,
  away_spread_odds,
  home_spread_odds,
  away_spread_pct,
  home_spread_pct,
  spread_vegas_edge,
  total_line,
  under_odds,
  over_odds,
  over_pct,
  under_pct,
  total_vegas_edge,
  
  # Response Variables
  result,
  total,
  winning_team
)

# Clean data for model
games_df$roof <- ifelse(games_df$roof == "closed" | games_df$roof == "open", "retractable", games_df$roof)
# games_df$temp[games_df$roof == "dome" | games_df$roof == "retractable"] <- 72
# games_df$wind[games_df$roof == "dome" | games_df$roof == "retractable"] <- 0
# games_df <- games_df[complete.cases(games_df), ]
#games_df3 <- games_df[!complete.cases(games_df), ]
# games_df$roof <- as_factor(games_df$roof)
# games_df$surface <- as_factor(games_df$surface)
# games_df$game_id <- as.character(games_df$game_id)
# games_df$gameday <- as.character(games_df$gameday)
# games_df$season <- factor(games_df$season)
# levels(games_df$season) <- list("1999" = 1, "2000" = 2, "2001" = 3, "2002" = 4, "2003" = 5, "2004" = 6, "2005" = 7, "2006" = 8,
#                                "2007" = 9, "2008" = 10, "2009" = 11, "2010" = 12, "2011" = 13, "2012" = 14, "2013" = 15, "2014" = 16,
#                                "2015" = 17, "2016" = 18, "2017" = 19, "2018" = 20, "2019" = 21, "2020" = 22)
# games_df$game_type <- factor(games_df$game_type, levels = c("REG", "WC", "DIV", "CON", "SB"))
#games_df$gametime <- as.character(games_df$gametime)
#games_df$away_qb_id <- as.character(games_df$away_qb_id)
#games_df$home_qb_id <- as.character(games_df$home_qb_id)
# games_df$away_qb_name <- as.character(games_df$away_qb_name)
# games_df$home_qb_name <- as.character(games_df$home_qb_name)
# games_df$away_coach <- as.character(games_df$away_coach)
# games_df$home_coach <- as.character(games_df$home_coach)
# games_df$referee <- as.character(games_df$referee)
#games_df$stadium_id <- as.character(games_df$stadium_id)
# games_df$stadium <- as.character(games_df$stadium)
# games_df$div_game <- as_factor(games_df$div_game)
# games_df$overtime <- as_factor(games_df$overtime)
# levels(games_df$overtime) <- list("Yes" = 1, "No" = 0)
games_df$div_game <- ifelse(games_df$div_game == 1, "Yes", "No")
games_df$gametime <- ifelse(between(as.numeric(hm(games_df$gametime)), as.numeric(hm("00:00")), as.numeric(hm("12:00"))), "Morning",
                            ifelse(between(as.numeric(hm(games_df$gametime)), as.numeric(hm("12:00")), as.numeric(hm("16:00"))), "Afternoon",
                                   ifelse(between(as.numeric(hm(games_df$gametime)), as.numeric(hm("16:00")), as.numeric(hm("18:59"))), "Evening",
                                          ifelse(between(as.numeric(hm(games_df$gametime)), as.numeric(hm("19:00")), as.numeric(hm("24:00"))), "Night", NA))))

# Create some response variables
games_df <- games_df %>%
  mutate(
    Cover_Team = ifelse(result > spread_line, "Home",
                        ifelse(result < spread_line, "Away", "Push")),
    `O/U` = ifelse(total > total_line, "Over",
                   ifelse(total < total_line, "Under", "Push"))
  )

###### GB Tree for Home cover ######

# Filter data for specific model using only regular season games
colnames(games_df)

model_data <- games_df %>%
  select(
    # Game Description Metrics
    season,
    week,
    weekday,
    gametime,
    div_game,
    roof,
    
    # Team Rest
    away_rest,
    home_rest,
    
    # Points
    away_ewm_ppg,
    away_ewm_allowed_ppg,
    home_ewm_ppg,
    home_ewm_allowed_ppg,
    
    # Wins
    away_ewm_win_percentage,
    home_ewm_win_percentage,
    
    ## Yards and Efficiency
    # Away for
    away_ewm_pass_ypg,
    away_ewm_pass_explosive_ypg,
    away_ewm_pass_off_epg,
    away_ewm_run_ypg,
    away_ewm_run_explosive_ypg,
    away_ewm_run_off_epg,
    
    # Away Against
    away_ewm_allowed_pass_ypg,
    away_ewm_allowed_pass_explosive_ypg,
    away_ewm_pass_def_epg,
    away_ewm_allowed_run_ypg,
    away_ewm_allowed_run_explosive_ypg,
    away_ewm_run_def_epg,
    
    # Home For
    home_ewm_pass_ypg,
    home_ewm_pass_explosive_ypg,
    home_ewm_pass_off_epg,
    home_ewm_run_ypg,
    home_ewm_run_explosive_ypg,
    home_ewm_run_off_epg,
    
    # Home Against
    home_ewm_allowed_pass_ypg,
    home_ewm_allowed_pass_explosive_ypg,
    home_ewm_pass_def_epg,
    home_ewm_allowed_run_ypg,
    home_ewm_allowed_run_explosive_ypg,
    home_ewm_run_def_epg,
    
    # Vegas Metrics
    away_moneyline,
    home_moneyline,
    away_moneyline_pct,
    home_moneyline_pct,
    moneyline_vegas_edge,
    spread_line,
    away_spread_odds,
    home_spread_odds,
    away_spread_pct,
    home_spread_pct,
    spread_vegas_edge,
    total_line,
    under_odds,
    over_odds,
    under_pct,
    over_pct,
    total_vegas_edge,
    
    # Response Variables
    result,
    total,
    winning_team,
    Cover_Team,
    `O/U`
  )


###### GB Tree for O/U cover ######
gbtree_cover_data <- model_data %>% 
  mutate(
    # Era = ifelse(between(season, 2006, 2013), "Era1",
    #              ifelse(between(season, 2014, 2017), "Era2", "Era3")),
    # away_ewm_total_ypg = sum(away_ewm_pass_ypg, away_ewm_run_ypg),
    # home_ewm_total_ypg = sum(home_ewm_pass_ypg, home_ewm_run_ypg),
    # away_ewm_total_allowed_ypg = sum(away_ewm_allowed_pass_ypg, away_ewm_allowed_run_ypg),
    # home_ewm_total_allowed_ypg = sum(home_ewm_allowed_pass_ypg, home_ewm_allowed_run_ypg),
    favorite = ifelse(spread_line > 0, "Home", "Away"),
    spread_line = abs(spread_line),
    #label = ifelse(Cover_Team == "Home", 1, 0)
    label = ifelse(`O/U` == "Over", 1, 0)
  ) %>%
  dplyr::select(
    label,
    #season,
    #Era,
    #weekday,
    #gametime,
    # Rest Metrics
    #away_rest,
    #home_rest,
    
    # Point Metrics
    away_ewm_ppg,
    away_ewm_allowed_ppg,
    home_ewm_ppg,
    home_ewm_allowed_ppg,
    
    # Win Metrics
    away_ewm_win_percentage,
    home_ewm_win_percentage,
    
    # Yard Metrics
    # Away
    # away_ewm_pass_ypg,
    # away_ewm_pass_explosive_ypg,
    away_ewm_pass_off_epg,
    
    # away_ewm_run_ypg,
    # away_ewm_run_explosive_ypg,
    away_ewm_run_off_epg,
    
    # away_ewm_allowed_pass_ypg,
    # away_ewm_allowed_pass_explosive_ypg,
    away_ewm_pass_def_epg,
    
    # away_ewm_allowed_run_ypg,
    # away_ewm_allowed_run_explosive_ypg,
    away_ewm_run_def_epg,
    
    # Home
    # home_ewm_pass_ypg,
    # home_ewm_pass_explosive_ypg,
    home_ewm_pass_off_epg,
    
    # home_ewm_run_ypg,
    # home_ewm_run_explosive_ypg,
    home_ewm_run_off_epg,
    
    # home_ewm_allowed_pass_ypg,
    # home_ewm_allowed_pass_explosive_ypg,
    home_ewm_pass_def_epg,
    
    # home_ewm_allowed_run_ypg,
    # home_ewm_allowed_run_explosive_ypg,
    home_ewm_run_def_epg,
    
    spread_line,
    #favorite,
    #away_spread_pct,
    #home_spread_pct,
    spread_vegas_edge
    #div_game,
    #roof
  )

## Build parameters
#convert characters to factors
cover_full_train <- gbtree_cover_data[complete.cases(gbtree_cover_data), ]
cover_full_train$label <- factor(cover_full_train$label)
# cover_full_train$season <- factor(cover_full_train$season)

fact_col <- colnames(cover_full_train)[sapply(cover_full_train,is.character)]
for(i in fact_col) set(cover_full_train,j=i,value = factor(cover_full_train[[i]]))
num_col <- colnames(cover_full_train)[sapply(cover_full_train,is.numeric)]

# Create training and test data
train_index <- createDataPartition(cover_full_train$label, p = 0.8, list = FALSE, times = 1)
cover_train <- cover_full_train[train_index, ]
cover_test <- cover_full_train[-train_index, ]

cover_train2 <- cover_train %>% select(-spread_vegas_edge)
cover_test2 <- cover_test %>% select(-spread_vegas_edge)

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

fit_control <- trainControl(method = "cv", number = 10)
preProcValues <- preProcess(cover_train, method = c("scale", "center"))

model <- train(label ~ .,
               data = cover_train2,
               method = "xgbTree",
               preProcess = c("scale", "center"),
               weights = cover_train$spread_vegas_edge,
               trControl = fit_control,
               verbosity = 0)

model
summary(model)

prediction <- predict(model, newdata = cover_test2)
postResample(prediction, cover_test2$label)
caret::confusionMatrix(prediction, cover_test2$label)

varImp(model)


###### GB Tree for O/U total ######
gbtree_total_data <- model_data %>% 
  mutate(
    # Era = ifelse(between(season, 2006, 2013), "Era1",
    #              ifelse(between(season, 2014, 2017), "Era2", "Era3")),
    # away_ewm_total_ypg = sum(away_ewm_pass_ypg, away_ewm_run_ypg),
    # home_ewm_total_ypg = sum(home_ewm_pass_ypg, home_ewm_run_ypg),
    # away_ewm_total_allowed_ypg = sum(away_ewm_allowed_pass_ypg, away_ewm_allowed_run_ypg),
    # home_ewm_total_allowed_ypg = sum(home_ewm_allowed_pass_ypg, home_ewm_allowed_run_ypg),
    favorite = ifelse(spread_line > 0, "Home", "Away"),
    spread_line = abs(spread_line),
    label = ifelse(`O/U` == "Over", 1, 0)
  ) %>%
  dplyr::select(
    label,
    #season,
    #Era,
    #weekday,
    #gametime,
    # Rest Metrics
    #away_rest,
    #home_rest,
    
    # Point Metrics
    away_ewm_ppg,
    away_ewm_allowed_ppg,
    home_ewm_ppg,
    home_ewm_allowed_ppg,
    
    # Win Metrics
    away_ewm_win_percentage,
    home_ewm_win_percentage,
    
    # Yard Metrics
    # Away
    # away_ewm_pass_ypg,
    # away_ewm_pass_explosive_ypg,
    away_ewm_pass_off_epg,
    
    # away_ewm_run_ypg,
    # away_ewm_run_explosive_ypg,
    away_ewm_run_off_epg,
    
    # away_ewm_allowed_pass_ypg,
    # away_ewm_allowed_pass_explosive_ypg,
    away_ewm_pass_def_epg,
    
    # away_ewm_allowed_run_ypg,
    # away_ewm_allowed_run_explosive_ypg,
    away_ewm_run_def_epg,
    
    # Home
    # home_ewm_pass_ypg,
    # home_ewm_pass_explosive_ypg,
    home_ewm_pass_off_epg,
    
    # home_ewm_run_ypg,
    # home_ewm_run_explosive_ypg,
    home_ewm_run_off_epg,
    
    # home_ewm_allowed_pass_ypg,
    # home_ewm_allowed_pass_explosive_ypg,
    home_ewm_pass_def_epg,
    
    # home_ewm_allowed_run_ypg,
    # home_ewm_allowed_run_explosive_ypg,
    home_ewm_run_def_epg,
    
    spread_line,
    #favorite,
    #away_spread_pct,
    #home_spread_pct,
    spread_vegas_edge
    #div_game,
    #roof
  )

## Build parameters
#convert characters to factors
total_full_train <- gbtree_total_data[complete.cases(gbtree_total_data), ]
total_full_train$label <- factor(total_full_train$label)
total_full_train$season <- factor(total_full_train$season)
#total_full_train$week <- factor(total_full_train$week)

fact_col <- colnames(total_full_train)[sapply(total_full_train,is.character)]
for(i in fact_col) set(total_full_train,j=i,value = factor(total_full_train[[i]]))
num_col <- colnames(total_full_train)[sapply(total_full_train,is.numeric)]

# Create training and test data
train_index <- createDataPartition(total_full_train$label, p = 0.8, list = FALSE, times = 1)
total_train <- total_full_train[train_index, ]
total_test <- total_full_train[-train_index, ]

total_train <- total_full_train 

season_index <- list()
#total_train$season <- factor(total_train$season)
for(season_i in unique(total_train$season)){
  season_vec <- as.vector(which(total_train$season != season_i))
  season_index[[season_i]] <- season_vec
}

total_train <- total_train %>% select(-season, -week)
total_test <- total_test %>% select(-season, -week)
#set parallel backend
# library(parallel)
# library(parallelMap) 
# parallelStartSocket(cpus = detectCores())



fit_control <- trainControl(method = "cv", index = season_index)

fit_control <- trainControl(method = "cv")
preProcValues <- preProcess(total_train, method = c("scale", "center"))

totalTuneGrid <- expand.grid(nrounds = seq(50, 500, by = 50),
                             max_depth = 3:10,
                             eta = seq(0,1, by = 0.1),
                             gamma = 0, 
                             colsample_bytree = seq(.5, 1, by = 0.1),
                             min_child_weight = 1:10,
                             subsample = seq(0.5, 1, by = 0.1))

# totalTuneGrid <- expand.grid(nrounds = 500,
#                              max_depth = 8,
#                              eta = 0.00963,
#                              gamma = 0,
#                              min_child_weight = 1.587276,
#                              subsample = .7558104,
#                              colsample_bytree = .9350209)

model <- train(label ~ .,
               data = total_train,
               method = "xgbTree",
               #preProcess = c("scale", "center"),
               #weights = total_train$spread_vegas_edge,
               #tuneGrid = totalTuneGrid,
               tuneLength = 100,
               trControl = fit_control,
               verbosity = 0)

model
summary(model)

tune_params <- model$bestTune

prediction <- predict(model, newdata = total_test)
postResample(prediction, total_test$label)
caret::confusionMatrix(prediction, total_test$label)

varImp(model)

weeks2022 <- unique(total_full_train %>% filter(season == 2022) %>% select(week))

total_train <- total_full_train %>% filter(season != 2022 & week < 6) 
total_test <- total_full_train %>% filter(season == 2022 & week == 6) 

season_index <- list()
#total_train$season <- factor(total_train$season)
for(season_i in unique(total_train$season)){
  season_vec <- as.vector(which(total_train$season != season_i))
  season_index[[season_i]] <- season_vec
}

total_train <- total_train %>% select(-season, -week)
total_test <- total_test %>% select(-season, -week)

fit_control <- trainControl(method = "cv", index = season_index)

model <- train(label ~ .,
               data = total_train,
               method = "xgbTree",
               #preProcess = c("scale", "center"),
               #weights = total_train$spread_vegas_edge,
               tuneGrid = tune_params,
               #tuneLength = 100,
               trControl = fit_control,
               verbosity = 0)

model
summary(model)

prediction <- predict(model, newdata = total_test)
postResample(prediction, total_test$label)
caret::confusionMatrix(prediction, total_test$label)

varImp(model, scale = FALSE)

model2 <- train(label ~.,
                data = total_train,
                method = "rf",
                trControl = fit_control)

model2
summary(model2)

prediction <- predict(model2, newdata = total_test)
postResample(prediction, total_test$label)
caret::confusionMatrix(prediction, total_test$label)

varImp(model2, scale = FALSE)



#create tasks
total_train2 <- as.data.frame(total_train) #%>% dplyr::select(-season, -Era)
total_test2 <- as.data.frame(total_test) 

traintask <- makeClassifTask(data = total_train2, target = "label", blocking = total_train2$season)
testtask <- makeClassifTask(data = total_test2, target = "label", blocking = total_test$season)

#do one hot encoding`<br/> 
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=500)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("eta",lower = 0,upper = 1),
                        #makeNumericParam("gamma",lower = 1L,upper = 10L),
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV", fixed = TRUE, blocking.cv = TRUE)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc,
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)
mytune$y 
mytune$x

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

xgmodel <- train(learner = lrn_tune, task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)


# NOt sure why but this is necessary for xgboost
# gbtree_cover_data <- gbtree_cover_data %>%
#   mutate(
#     label = as.numeric(label),
#     label = label - 1
#   )

# Set season to test
gbtree_cover_data$season <- factor(gbtree_cover_data$season)
test_season <- 2022

# Create training and test datasets
cover_train <- gbtree_cover_data %>% filter(season != test_season)
cover_test <- gbtree_cover_data %>% filter(season == test_season)

#convert data frame to data table
setDT(cover_train) 
setDT(cover_test)

# preparing matrix
new_cover_train <- model.matrix(~. + 0, data = cover_train %>% dplyr::select(-label, -season),
                                label = cover_train$label)
new_cover_test <- model.matrix(~. + 0, data = cover_test %>% dplyr::select(-label, -season),
                               label = cover_test$label)

cover_dtrain <- xgb.DMatrix(data = new_cover_train,label = cover_train$label) 
cover_dtest <- xgb.DMatrix(data = new_cover_test,label = cover_test$label)

# Default parameters

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, 
               gamma=0, 
               max_depth=6, 
               min_child_weight=1, 
               subsample=1, 
               colsample_bytree=1)

cover_indicies <- list()
cover_seasons <- unique(cover_train$season)
for(i in 1:length(unique(cover_train$season))){
  cover_indicies[[i]] <- as.vector(which(cover_train$season == cover_seasons[i]))
}

cover_xgbcv <- xgb.cv(params = params, 
                      data = cover_dtrain, 
                      nrounds = 1000, 
                      showsd = T, 
                      metrics = c("error", "logloss"),
                      nfold = 5,
                      stratified = TRUE,
                      train_folds = cover_indicies, 
                      print_every_n = 10, 
                      early_stop_round = 20,
                      maximize = F)

min(cover_xgbcv$evaluation_log$test_logloss_mean)
which(cover_xgbcv$evaluation_log$test_logloss_mean == min(cover_xgbcv$evaluation_log$test_logloss_mean))

min(cover_xgbcv$evaluation_log$test_error_mean)
which(cover_xgbcv$evaluation_log$test_error_mean == min(cover_xgbcv$evaluation_log$test_error_mean))

#first default - model training
xgb1 <- xgb.train (params = params, 
                   data = cover_dtrain, 
                   nrounds = 97, 
                   watchlist = list(validation1=cover_dtest,validation2=cover_dtrain), 
                   print_every_n = 1, 
                   early_stop_round = NULL, 
                   maximize = F , 
                   eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,cover_dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)





