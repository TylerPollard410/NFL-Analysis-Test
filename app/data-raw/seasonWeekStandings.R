# Libraries ----
require(DescTools)
require(tictoc)

## NFL Verse ----
require(nflverse)

## Tidyverse ----
require(tidyverse)



# Create Function to compute SRS ----
compute_srs_continuous <- function(game_data) {
  
  # Ensure that the game_data has complete cases for result
  game_data <- game_data |> filter(complete.cases(result))
  
  # Obtain a sorted list of unique (season, week) combinations.
  # The function will update the SRS cumulatively for games that occurred on or 
  # before the current (season, week) tuple.
  weekGrid <- game_data |>
    distinct(season, week) |>
    arrange(season, week)
  
  # This will hold our weekly SRS standings
  season_week_standings <- data.frame()
  
  # Iterate over each (season, week) combination in order.
  # Note: For cumulative update, we select all games where either the season is earlier
  # than the current one or, if the season is equal, the week is less than or equal.
  for(r in 1:nrow(weekGrid)) {
    currentSeason <- weekGrid$season[r]
    currentWeek   <- weekGrid$week[r]
    
    # Cumulative game selection: select games that occurred in an earlier season
    # or in the same season up to the current week.
    game_dataTemp <- game_data |>
      filter((season < currentSeason) | (season == currentSeason & week <= currentWeek)) |>
      mutate(
        home_team = clean_team_abbrs(home_team),
        away_team = clean_team_abbrs(away_team)
      )
    
    # Convert to long format (the function clean_homeaway is assumed available)
    game_dataLongTemp <- game_dataTemp |>
      clean_homeaway(invert = c("result", "spread_line"))
    
    # Compute cumulative team standings from the long-format data
    standingTemp <- game_dataLongTemp |>
      filter(!is.na(result)) |>
      select(week, team, team_score, opponent, opponent_score, location, result) |>
      mutate(
        win  = ifelse(result > 0, 1, 0),
        loss = ifelse(result < 0, 1, 0),
        tie  = ifelse(result == 0, 1, 0)
      ) |>
      group_by(team) |>
      summarise(
        games_played   = n(),
        win            = sum(win),
        loss           = sum(loss),
        tie            = sum(tie),
        team_score     = sum(team_score),
        opponent_score = sum(opponent_score),
        result         = sum(result),
        .groups = "drop"
      ) |>
      mutate(
        win_loss_percent = (win + tie/2) / (win + loss + tie/2),
        MOV              = result / games_played,
        team_PPG         = team_score / games_played,
        opp_PPG          = opponent_score / games_played
      )
    
    # Calculate the league average PPG for adjustment
    leaguePPGTemp <- sum(standingTemp$team_score) / sum(standingTemp$games_played)
    
    # Initialize SRS and related values for the teams
    standingTemp <- standingTemp |>
      mutate(
        SOS   = 0,
        SRS   = MOV,
        OSRS  = team_PPG - leaguePPGTemp,
        DSRS  = SRS - OSRS
      )
    
    # Iteratively update SRS (convergence loop)
    max_iterations <- 200
    tolerance <- 0.001
    for (k in 1:max_iterations) {
      previous_SRS   <- standingTemp$SRS
      previous_OSRS  <- standingTemp$OSRS
      previous_DSRS  <- standingTemp$DSRS
      
      # Update SRS via a join with game-specific adjustments.
      standingTemp <- standingTemp |>
        left_join(
          game_dataLongTemp |>
            select(team, opponent, result, team_score) |>
            left_join(
              standingTemp |> select(team, SRS, DSRS),
              by = c("opponent" = "team")
            ) |>
            mutate(
              SOS   = SRS,
              SRS   = result + SOS,
              OSOS  = DSRS,
              OSRS  = team_score + OSOS - mean(team_score)
            ) |>
            group_by(team) |>
            summarise(
              newSOS  = mean(SOS, na.rm = TRUE),
              newSRS  = mean(SRS, na.rm = TRUE),
              newOSRS = mean(OSRS),
              .groups = "drop"
            ),
          by = "team"
        ) |>
        mutate(
          SOS  = ifelse(is.na(newSOS), 0, newSOS),
          SRS  = ifelse(is.na(newSRS), 0, newSRS),
          OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
          DSRS = SRS - OSRS
        ) |>
        select(-newSOS, -newSRS, -newOSRS)
      
      # Check for convergence of the SRS calculations
      if(max(abs(standingTemp$SRS - previous_SRS),
             abs(standingTemp$OSRS - previous_OSRS),
             abs(standingTemp$DSRS - previous_DSRS)) < tolerance){
        cat("Converged after", k, "iterations for Season", currentSeason, "Week", currentWeek, "\n")
        break
      }
      
      if (k == max_iterations) {
        cat("Reached maximum iterations of", k, "without full convergence for Season", currentSeason, "Week", currentWeek, "\n")
      }
    }
    
    # Append current season and week to the results
    standingTemp <- standingTemp |>
      mutate(season = currentSeason, week = currentWeek) |>
      arrange(team) |>
      select(
        season,
        week,
        team,
        games_played,
        win,
        loss,
        tie,
        team_score,
        team_PPG,
        opponent_score,
        opp_PPG,
        result,
        win_loss_percent,
        MOV,SOS,SRS,OSRS,DSRS
      )
    
    season_week_standings <- rbind(season_week_standings, standingTemp)
    
    #cat("Processed Season", currentSeason, "Week", currentWeek, "\n")
  }
  
  return(season_week_standings)
}

## Load Previously Calculated Data ----
load(file = "./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |>
  filter(season != get_current_season())

newSeasons <- get_current_season()
current_season_data <- gameData |> filter(season == get_current_season())

seasonWeekStandings_update <- compute_srs_continuous(current_season_data)
seasonWeekStandings <- bind_rows(seasonWeekStandings,
                                 seasonWeekStandings_update)

# Test all seasons ----
# srs_results <- unique(gameData$season) |> 
#   map_dfr(~ {
#     season_data <- gameData |> filter(season == .x, complete.cases(result))
#     compute_srs_continuous(season_data)
#   })

# Save to data folder ----
# save(
#   seasonWeekStandings,
#   file = "./app/data/seasonWeekStandings.rda"
# )

# Remove supp Vars ----
#allSeasons <- 2006:most_recent_season()
rm(newSeasons,
   current_season_data,
   seasonWeekStandings_update)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Old Script -----
# 
# #tic()
# for(i in newSeasons){
#   gameDataSeason <- gameData |>
#     filter(season == i) |>
#     #filter(game_type == "REG")
#     filter(complete.cases(result))
#   seasonWeeks <- max(gameDataSeason$week)
#   
#   for(j in 1:seasonWeeks){
#     gameDataTemp <- gameDataSeason |>
#       filter(week %in% 1:j) |>
#       mutate(
#         home_team = clean_team_abbrs(home_team),
#         away_team = clean_team_abbrs(away_team)
#       ) 
#     
#     gameDataLongTemp <- gameDataTemp |>
#       clean_homeaway(invert = c("result", "spread_line")) 
#     
#     standingTemp <- gameDataLongTemp |>
#       filter(!is.na(result)) |>
#       select(
#         week,
#         team,
#         team_score,
#         opponent,
#         opponent_score,
#         location,
#         result
#       ) |>
#       mutate(
#         win = ifelse(result > 0, 1, 0),
#         loss = ifelse(result < 0, 1, 0),
#         tie = ifelse(result == 0, 1, 0)
#       ) |>
#       group_by(team) |>
#       summarise(
#         games_played = n(),
#         across(c(win, 
#                  loss,
#                  tie,
#                  team_score,
#                  opponent_score,
#                  result),
#                ~sum(.x)),
#       ) |>
#       mutate(
#         win_loss_percent = (win + tie/2)/(win + loss + tie/2),
#         MOV = result/games_played
#       ) |>
#       mutate(team_PPG = team_score/games_played, .after = team_score) |>
#       mutate(opp_PPG = opponent_score/games_played, .after = opponent_score)
#     
#     leaguePPGTemp <- sum(standingTemp$team_score)/sum(standingTemp$games_played)
#     standingTemp <- standingTemp |>
#       mutate(SOS = 0, .after = MOV) |>
#       mutate(SRS = MOV, .after = SOS) |>
#       mutate(OSRS = team_PPG - leaguePPGTemp) |>
#       mutate(DSRS = SRS - OSRS)
#     
#     max_iterations <- 200
#     tolerance <- 0.001
#     for (k in 1:max_iterations) {
#       previous_SRS <- standingTemp$SRS
#       previous_OSRS <- standingTemp$OSRS
#       previous_DSRS <- standingTemp$DSRS
#       
#       standingTemp <- standingTemp |>
#         left_join(
#           gameDataLongTemp |>
#             select(team, opponent, result, team_score) |>
#             left_join(standingTemp |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
#             mutate(
#               SOS = SRS,
#               SRS = result + SOS,
#               OSOS = DSRS,
#               OSRS = team_score + OSOS - mean(team_score)
#             ) |>
#             group_by(team) |>
#             summarise(
#               newSOS = mean(SOS, na.rm = TRUE),
#               newSRS = mean(SRS, na.rm = TRUE),
#               newOSRS = mean(OSRS)
#             ), 
#           by = join_by(team)
#         ) |>
#         mutate(
#           SOS = ifelse(is.na(newSOS), 0, newSOS),
#           SRS = ifelse(is.na(newSRS), 0, newSRS),
#           OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
#           DSRS = SRS - OSRS
#         ) |>
#         select(-newSOS, -newSRS, -newOSRS)
#       
#       if(max(abs(standingTemp$SRS - previous_SRS),
#              abs(standingTemp$OSRS - previous_OSRS),
#              abs(standingTemp$DSRS - previous_DSRS)) < tolerance){
#         cat("Converged after", k, "iterations.\n")
#         break
#       }
#       
#       # If last iteration and not converged
#       if (k == max_iterations) {
#         cat("Reached maximum iterations = ",k, "without full convergence.\n")
#       }
#     }
#     standingTemp <- standingTemp |> 
#       mutate(week = j, .before = 1) |>
#       mutate(season = i, .before = 1) |>
#       arrange(team)
#     
#     seasonWeekStandings <- rbind(seasonWeekStandings, standingTemp)
#     
#     # seasonWeekStandingsConvergence <- rbind(
#     #   seasonWeekStandingsConvergence,
#     #   data.frame(season = i, week = j, Converge = k)
#     # )
#     
#     cat("Season", i, "Week", j, "\n")
#   }
# }
# #toc()

# rm(gameDataTemp,
#    gameDataLongTemp,
#    gameDataSeason,
#    #seasonWeekStandingsConvergence,
#    standingTemp,
#    i, j, k,
#    leaguePPGTemp,
#    max_iterations,
#    previous_DSRS,
#    previous_OSRS, 
#    previous_SRS,
#    seasonWeeks,
#    tolerance)


