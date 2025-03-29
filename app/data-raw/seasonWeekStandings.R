# Libraries ----
require(DescTools)
require(tictoc)

## NFL Verse ----
require(nflverse)

## Tidyverse ----
require(tidyverse)

# Load nflverse data ----
# allSeasons <- 2006:most_recent_season()
# 
# ##  NFL Team Graphics, Colors, and Logos
# teamsData <- load_teams(current = FALSE)
# 
# ##  Game/Schedule Data
# gameData <- load_schedules(seasons = allSeasons) |>
#   mutate(
#     home_team = clean_team_abbrs(home_team),
#     away_team = clean_team_abbrs(away_team)
#   ) 
# 
# gameDataLong <- gameData |>
#   clean_homeaway(invert = c("result", "spread_line")) 

# Standings Table ----
## Historical season games ----
### By Season and Week ----
#### No prior ----
load(file = "./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |>
  filter(season != get_current_season())
#seasonWeekStandingsConvergence <- data.frame()
newSeasons <- get_current_season()
#gameData <- gameData |> filter(season == 2024)

#tic()
for(i in newSeasons){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    #filter(game_type == "REG")
    filter(complete.cases(result))
  seasonWeeks <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeeks){
    gameDataTemp <- gameDataSeason |>
      filter(week %in% 1:j) |>
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
    
    max_iterations <- 200
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
    
    seasonWeekStandings <- rbind(seasonWeekStandings, standingTemp)
    
    # seasonWeekStandingsConvergence <- rbind(
    #   seasonWeekStandingsConvergence,
    #   data.frame(season = i, week = j, Converge = k)
    # )
    
    cat("Season", i, "Week", j, "\n")
  }
}
#toc()

# Save to data folder ----
# save(
#   seasonWeekStandings,
#   file = "./app/data/seasonWeekStandings.rda"
# )

# Remove supp Vars ----
#allSeasons <- 2006:most_recent_season()
rm(gameDataTemp,
   gameDataLongTemp,
   gameDataSeason,
   #seasonWeekStandingsConvergence,
   standingTemp,
   i, j, k,
   leaguePPGTemp,
   max_iterations,
   previous_DSRS,
   previous_OSRS, 
   previous_SRS,
   seasonWeeks,
   tolerance)


