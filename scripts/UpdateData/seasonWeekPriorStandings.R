# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)

## Data Manipulation
library(stringr)
library(rvest)

## Tables ----
library(htmltools)
library(gt)
library(gtsummary)
library(gtExtras)
library(reactable)
library(reactablefmtr)

## Plotting ----
library(smplot2)
# library(cowplot)
# library(GGally)
# library(patchwork)

## Modeling ----
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
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Load nflverse data ----
allSeasons <- 2002:most_recent_season()

##  NFL Team Graphics, Colors, and Logos
teamsData <- load_teams(current = FALSE)

##  Game/Schedule Data
gameData <- load_schedules(seasons = allSeasons) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) 

# Standings Table ----
## Historical season games ----
### By Season and Week ----
#### Prior ----
seasonWeekPriorStandings <- data.frame()
seasonWeekPriorStandingsConvergence <- data.frame()

tic()
for(i in allSeasons){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG")
  seasonWeekPriors <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeekPriors){
    gameDataTemp <- gameDataSeason |>
      filter(week %in% 1:j) |>
      filter(complete.cases(result)) |>
      mutate(
        home_team = clean_team_abbrs(home_team),
        away_team = clean_team_abbrs(away_team)
      ) 
    
    gameDataLongTemp <- gameDataTemp |>
      clean_homeaway(invert = c("result", "spread_line")) 
    
    standingTemp <- gameDataLongTemp |>
      filter(!is.na(result)) |>
      select(
        week,
        team,
        team_score,
        opponent,
        opponent_score,
        location,
        result
      ) |>
      mutate(
        win = ifelse(result > 0, 1, 0),
        loss = ifelse(result < 0, 1, 0),
        tie = ifelse(result == 0, 1, 0)
      ) |>
      group_by(team) |>
      summarise(
        games_played = n(),
        across(c(win, 
                 loss,
                 tie,
                 team_score,
                 opponent_score,
                 result),
               ~sum(.x)),
      ) |>
      mutate(
        win_loss_percent = (win + tie/2)/(win + loss + tie/2),
        MOV = result/games_played
      ) |>
      mutate(team_PPG = team_score/games_played, .after = team_score) |>
      mutate(opp_PPG = opponent_score/games_played, .after = opponent_score)
    
    leaguePPGTemp <- sum(standingTemp$team_score)/sum(standingTemp$games_played)
    standingTemp <- standingTemp |>
      mutate(SOS = 0, .after = MOV) |>
      mutate(SRS = MOV, .after = SOS) |>
      mutate(OSRS = team_PPG - leaguePPGTemp) |>
      mutate(DSRS = SRS - OSRS)
    
    max_iterations <- 100
    tolerance <- 0.001
    for (k in 1:max_iterations) {
      previous_SRS <- standingTemp$SRS
      previous_OSRS <- standingTemp$OSRS
      previous_DSRS <- standingTemp$DSRS
      
      standingTemp <- standingTemp |>
        left_join(
          gameDataLongTemp |>
            select(team, opponent, result, team_score) |>
            left_join(standingTemp |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
            mutate(
              SOS = SRS,
              SRS = result + SOS,
              OSOS = DSRS,
              OSRS = team_score + OSOS - mean(team_score)
            ) |>
            group_by(team) |>
            summarise(
              newSOS = mean(SOS, na.rm = TRUE),
              newSRS = mean(SRS, na.rm = TRUE),
              newOSRS = mean(OSRS)
            ), 
          by = join_by(team)
        ) |>
        mutate(
          SOS = ifelse(is.na(newSOS), 0, newSOS),
          SRS = ifelse(is.na(newSRS), 0, newSRS),
          OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
          DSRS = SRS - OSRS
        ) |>
        select(-newSOS, -newSRS, -newOSRS)
      
      if(max(abs(standingTemp$SRS - previous_SRS),
             abs(standingTemp$OSRS - previous_OSRS),
             abs(standingTemp$DSRS - previous_DSRS)) < tolerance){
        cat("Converged after", k, "iterations.\n")
        break
      }
      
      # If last iteration and not converged
      if (k == max_iterations) {
        cat("Reached maximum iterations = ",k, "without full convergence.\n")
      }
    }
    standingTemp <- standingTemp |> 
      mutate(week = j, .before = 1) |>
      mutate(season = i, .before = 1) |>
      arrange(team)
    
    seasonWeekPriorStandings <- rbind(seasonWeekPriorStandings, standingTemp)
    
    seasonWeekPriorStandingsConvergence <- rbind(
      seasonWeekPriorStandingsConvergence,
      data.frame(season = i, week = j, Converge = k)
    )
    
    cat("Season", i, "Week", j, "\n")
  }
}
toc()
save(seasonWeekPriorStandings, file = "./data/seasonWeekPriorStandings.rda")

##### Plot results ----
seasonWeekPriorStandingsMerge <- seasonWeekPriorStandings |>
  mutate(week = week + 1) |>
  select(
    season,
    week,
    team,
    games_played,
    win,
    loss,
    tie,
    team_PPG,
    opp_PPG,
    win_loss_percent,
    MOV,
    SOS,
    SRS,
    OSRS,
    DSRS
  )

seasonWeekPriorGameData <- gameData |>
  left_join(seasonWeekPriorStandingsMerge |>
              select(
                season,
                week,
                team,
                games_played,
                win,
                loss,
                tie,
                team_PPG,
                opp_PPG,
                win_loss_percent,
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("home_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "home_team" == "team")) |>
  left_join(seasonWeekPriorStandingsMerge |>
              select(
                season,
                week,
                team,
                games_played,
                win,
                loss,
                tie,
                team_PPG,
                opp_PPG,
                win_loss_percent,
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("away_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "away_team" == "team"))

seasonWeekPriorGameDataLong <- gameDataLong |>
  left_join(seasonWeekPriorStandingsMerge |>
              select(
                season,
                week,
                team,
                games_played,
                win,
                loss,
                tie,
                team_PPG,
                opp_PPG,
                win_loss_percent,
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("team_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, team)) |>
  left_join(seasonWeekPriorStandingsMerge |>
              select(
                season,
                week,
                team,
                games_played,
                win,
                loss,
                tie,
                team_PPG,
                opp_PPG,
                win_loss_percent,
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("opponent_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "opponent" == "team"))

seasonWeekPriorGameData <- seasonWeekPriorGameData |>
  mutate(
    SRS_spread = home_SRS - away_SRS + 2,
    SRS_spread_diff = SRS_spread - spread_line
  ) |>
  filter(game_type == "REG") |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

ggplot(data = seasonWeekPriorGameData, 
       aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  facet_wrap(vars(season))

ggplot(data = seasonWeekPriorGameData, 
       aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE)

seasonWeekPriorGameData |>
  group_by(season) |>
  summarise(
    mean(SRS_spread_diff, na.rm = TRUE)
  ) |>
  print(n = length(seasonTrain))

ggplot(data = seasonWeekPriorGameData) +
  geom_histogram(
    aes(x = SRS_spread_diff, after_stat(density)), bins = 100,
    color = "#99c7c7", fill = "#bcdcdc") +
  geom_vline(aes(xintercept = mean(SRS_spread_diff, na.rm = TRUE)),
             color = "orange", linewidth = 2) +
  geom_vline(aes(xintercept = median(SRS_spread_diff, na.rm = TRUE)),
             color = "orange3", linewidth = 2) +
  geom_density(#data = final_data3,
    aes(x = SRS_spread_diff),
    color = "#007C7C", 
    linewidth = 1) +
  theme_bw()



iters <- 5000
burn <- 1000
chains <- 1
sims <- (iters-burn)*chains

gameDataTrain <- gameData2 |>
  filter(!(season %in% 2023:2024)) |>
  mutate(
    spread_line = scale(spread_line)
  )
str(gameDataTrain)

gameDataTest <- gameData2 |>
  filter(season %in% 2023:2024) |>
  mutate(
    spread_line = scale(spread_line,
                        center = attr(gameDataTrain$spread_line, "scaled:center"),
                        scale = attr(gameDataTrain$spread_line, "scaled:scale"))
  )
str(gameDataTest)


