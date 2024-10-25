# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
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
## Set universal params ---- 
future::plan("multisession")
saveFileExt <- "~/Desktop/NFL Analysis Data/"
seasonTrain <- 2003:2023

## Data Dictionaries ----
dataDictionaries <- list(
  ## Data Dictionary: Combine
  dictionary_combine,
  
  ## Data Dictionary: Contracts
  dictionary_contracts,
  
  ## Data Dictionary: Depth Charts
  dictionary_depth_charts,
  
  ## Data Dictionary: Draft Picks
  dictionary_draft_picks,
  
  ## Data Dictionary: ESPN QBR
  dictionary_espn_qbr,
  
  ## Data Dictionary: Expected Fantasy Points
  dictionary_ff_opportunity,
  
  ## Data Dictionary: Fantasy Player IDs
  dictionary_ff_playerids,
  
  ## Data Dictionary: Fantasy Football Rankings
  dictionary_ff_rankings,
  
  ## Data Dictionary: FTN Charting Data
  dictionary_ftn_charting,
  
  ## Data Dictionary: Injuries
  dictionary_injuries,
  
  ## Data Dictionary: Next Gen Stats
  dictionary_nextgen_stats,
  
  ## Data Dictionary: Participation
  dictionary_participation,
  
  ## Data Dictionary: Play by Play
  dictionary_pbp,
  
  ## Data Dictionary: PFR Passing
  dictionary_pfr_passing,
  
  ## Data Dictionary: Player Stats
  dictionary_player_stats,
  
  ## Data Dictionary: Player Stats Defense
  dictionary_player_stats_def,
  
  ## Data Dictionary: Rosters
  dictionary_rosters,
  
  ## Data Dictionary: Schedules
  dictionary_schedules,
  
  ## Data Dictionary: Snap Counts
  dictionary_snap_counts,
  
  ## Data Dictionary: Trades
  dictionary_trades
)

## nflverse data ----
##  Play By Play
## Take 121.972 seconds
tic()
pbpData <- load_pbp(seasons = TRUE) 
toc()
pbpData <- pbpData |> filter(season %in% seasonTrain)

pbpID <- unique(pbpData$game_id)

##  Player Level Weekly Stats
playerOffenseStatsData <- load_player_stats(seasons = TRUE, stat_type = "offense") |>
  filter(season %in% seasonTrain)

playerDefenseStatsData <- load_player_stats(seasons = TRUE, stat_type = "defense") |>
  filter(season %in% seasonTrain)

playerKickingStatsData <- load_player_stats(seasons = TRUE, stat_type = "kicking") |>
  filter(season %in% seasonTrain)

##  Participation Data
#load_participation()

##  Players
#load_players()

##  Rosters
#load_rosters()

##  Rosters
#load_rosters_weekly()

##  NFL Team Graphics, Colors, and Logos
teamsData <- load_teams(current = FALSE)

##  Game/Schedule Data
gameData <- load_schedules(seasons = TRUE) |>
  filter(season %in% seasonTrain)
gameID <- sort(unique(gameData$game_id))

gameDataLong <- gameData |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) |>
  clean_homeaway(invert = c("result", "spread_line")) 
  
  
  ##  Officials
  #load_officials()
  
  ##  Trades
  #load_trades()
  
  ##  Draft Picks from PFR
  #load_draft_picks()
  
  ##  Combine Data from PFR
  #load_combine()
  
  ##  Player Level Weekly NFL Next Gen Stats
  playerPassingNextGenData <- load_nextgen_stats(seasons = TRUE, stat_type = "passing") |>
  filter(season %in% seasonTrain)

playerRushingNextGenData <- load_nextgen_stats(seasons = TRUE, stat_type = "rushing") |>
  filter(season %in% seasonTrain)

playerReceivingNextGenData <- load_nextgen_stats(seasons = TRUE, stat_type = "receiving") |>
  filter(season %in% seasonTrain)

##  Weekly Depth Charts
#load_depth_charts()

##  Injury Reports
injuryData <- load_injuries(seasons = TRUE) |>
  filter(season %in% seasonTrain)

##  ESPN's QBR
qbrSeasonData <- load_espn_qbr(seasons = TRUE, summary_type = "season") |>
  filter(season %in% seasonTrain)

qbrWeekData <- load_espn_qbr(seasons = TRUE, summary_type = "week") |>
  filter(season %in% seasonTrain)

##  Advanced Stats from PFR
pfrPassSeasonData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "pass", 
  summary_level = "season") |>
  filter(season %in% seasonTrain)
pfrPassWeekData <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = "pass", 
  summary_level = "week") |>
  filter(season %in% seasonTrain)

pfrRushSeasonData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "rush", 
  summary_level = "season") |>
  filter(season %in% seasonTrain)
pfrRushWeekData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "rush",
  summary_level = "week") |>
  filter(season %in% seasonTrain)

pfrRecSeasonData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "rec",
  summary_level = "season") |>
  filter(season %in% seasonTrain)
pfrRecWeekData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "rec", 
  summary_level = "week") |>
  filter(season %in% seasonTrain)

pfrDefSeasonData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "def", 
  summary_level = "season") |>
  filter(season %in% seasonTrain)
pfrDefWeekData <- load_pfr_advstats(
  seasons = TRUE, 
  stat_type = "def", 
  summary_level = "week") |>
  filter(season %in% seasonTrain)

##  Snap Counts from PFR
pfrSnapData <- load_snap_counts(seasons = TRUE) |>
  filter(season %in% seasonTrain)

##  Contracts
#load_contracts()

# Compare meta data ----
datasetNames <- c(
  'gameData',
  'injuryData',
  'pbpData', 
  'pfrDefSeasonData', 
  'pfrDefWeekData', 
  'pfrPassSeasonData',
  'pfrPassWeekData', 
  'pfrRecSeasonData', 
  'pfrRecWeekData', 
  'pfrRushSeasonData', 
  'pfrRushWeekData',
  'pfrSnapData', 
  'playerDefenseStatsData', 
  'playerKickingStatsData',
  'playerOffenseStatsData',
  'playerPassingNextGenData',
  'playerReceivingNextGenData', 
  'playerRushingNextGenData',
  'qbrSeasonData', 
  'qbrWeekData', 
  'teamsData'
)

for(i in datasetNames){
  
}


# Explore game data ----
gameData <- gameData |>
  filter(complete.cases(result)) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 

gameCount <- gameData |>
  group_by(
    season,
    week
  ) |>
  summarise(
    GP = n()
  ) |>
  pivot_wider(
    names_from = week,
    values_from = GP
  )

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) 

gameDataLong |>
  filter(
    game_type == "REG"
  ) |>
  group_by(
    season,
    team
  ) |>
  summarise(
    GP = n()
  ) |>
  pivot_wider(
    names_from = season,
    values_from = GP
  ) |>
  mutate(
    across(-contains("team"), function(x){ifelse(x == 16, NA, 1)})
  ) |>
  view(title = "dataView")


# Standings Table ----
## 2024 season ----
### PFR standing data ----
standingPFRData <- read_html("https://www.pro-football-reference.com/years/2023/index.htm")
standingPFRDataTables <- standingPFRData |> html_table()
standingAFCDataTables <- standingPFRDataTables[[1]] |>
  slice(-c(1,6,11,16)) |>
  mutate(
    across(-Tm, as.numeric)
    
  )
standingNFCDataTables <- standingPFRDataTables[[2]] |>
  slice(-c(1,6,11,16)) |>
  mutate(
    across(-Tm, as.numeric)
  )

standing2024pfr <- bind_rows(
  standingAFCDataTables,
  standingNFCDataTables) |>
  rename(team_name = Tm) |>
  mutate(
    team_name = str_replace_all(team_name, "[:punct:]|[:symbol:]", "")
  ) |>
  left_join(
    teamsData |>
      select(team_name, team_abbr)
  ) |>
  mutate(
    team_abbr = clean_team_abbrs(team_abbr, 
                                 current = TRUE,
                                 keep_non_matches = FALSE)
  ) |>
  distinct() |>
  arrange(team_abbr) |>
  select(
    team_abbr,
    team_name,
    everything()
  ) |>
  rename(team = team_abbr) 

standing2024$win == standing2024pfr$W

### Calculate using OSRS ----
gameData2024 <- load_schedules(seasons = 2024) |>
  filter(complete.cases(result)) |>
  filter(game_type == "REG") |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 

gameDataLong2024 <- gameData2024 |>
  clean_homeaway(invert = c("result", "spread_line")) 


standing2024 <- gameDataLong2024 |>
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

leaguePPG2024 <- sum(standing2024$team_score)/sum(standing2024$games_played)
standing2024 <- standing2024 |>
  mutate(SOS = 0, .after = MOV) |>
  mutate(SRS = MOV, .after = SOS) |>
  mutate(OSRS = team_PPG - leaguePPG2024) |>
  mutate(DSRS = SRS - OSRS)

max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_SRS <- standing2024$SRS
  previous_OSRS <- standing2024$OSRS
  previous_DSRS <- standing2024$DSRS
  
  standing2024 <- standing2024 |>
    left_join(
      gameDataLong2024 |>
        select(team, opponent, result, team_score) |>
        left_join(standing2024 |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
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
        )
    ) |>
    mutate(
      SOS = ifelse(is.na(newSOS), 0, newSOS),
      SRS = ifelse(is.na(newSRS), 0, newSRS),
      OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
      DSRS = SRS - OSRS
    ) |>
    select(-newSOS, -newSRS, -newOSRS)
  
  if(max(abs(standing2024$SRS - previous_SRS),
         abs(standing2024$OSRS - previous_OSRS),
         abs(standing2024$DSRS - previous_DSRS)) < tolerance){
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations = ",i, "without full convergence.\n")
  }
}
standing2024 <- standing2024 |> arrange(desc(SRS))
standing2024pfr <- standing2024pfr |> arrange(desc(SRS))

### Calculate using OSRS ----
gameData2002 <- load_schedules(seasons = 2002) |>
  filter(complete.cases(result)) |>
  filter(game_type == "REG") |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 

gameDataLong2002 <- gameData2002 |>
  clean_homeaway(invert = c("result", "spread_line")) 


standing2002 <- gameDataLong2002 |>
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

leaguePPG2002 <- sum(standing2002$team_score)/sum(standing2002$games_played)
standing2002 <- standing2002 |>
  mutate(SOS = 0, .after = MOV) |>
  mutate(SRS = MOV, .after = SOS) |>
  mutate(OSRS = team_PPG - leaguePPG2002) |>
  mutate(DSRS = SRS - OSRS)

max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_SRS <- standing2002$SRS
  previous_OSRS <- standing2002$OSRS
  previous_DSRS <- standing2002$DSRS
  
  standing2002 <- standing2002 |>
    left_join(
      gameDataLong2002 |>
        select(team, opponent, result, team_score) |>
        left_join(standing2002 |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
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
        )
    ) |>
    mutate(
      SOS = ifelse(is.na(newSOS), 0, newSOS),
      SRS = ifelse(is.na(newSRS), 0, newSRS),
      OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
      DSRS = SRS - OSRS
    ) |>
    select(-newSOS, -newSRS, -newOSRS)
  
  if(max(abs(standing2002$SRS - previous_SRS),
         abs(standing2002$OSRS - previous_OSRS),
         abs(standing2002$DSRS - previous_DSRS)) < tolerance){
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations = ",i, "without full convergence.\n")
  }
}
standing2002 <- standing2002 |> arrange(desc(SRS))
standing2002pfr <- standing2002pfr |> arrange(desc(SRS))

## All regular season games ----
gameData |>
  filter(game_type=="REG") |>
  group_by(season) |>
  summarise(
    weeks = max(week)
  ) |>
  print(n = length(seasonTrain))


### By Season ----
seasonStandings <- data.frame()
seasonStandingsConvergence <- data.frame()

tic()
for(i in seasonTrain){
  gameDataTemp <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG") |>
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
    mutate(season = i, .before = 1) |>
    arrange(team)
  
  seasonStandingsConvergence <- rbind(
    seasonStandingsConvergence,
    data.frame(season = i, Converge = k)
  )
  
  seasonStandings <- rbind(seasonStandings, standingTemp)
  
  cat("Season", i, "\n")
}
toc()
save(seasonStandings, file = "./Model Fitting/Data/seasonStandings.RData")

### By Season and Week ----
#### No prior ----
seasonWeekStandings <- data.frame()
seasonWeekStandingsConvergence <- data.frame()

tic()
for(i in seasonTrain){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG")
  seasonWeeks <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeeks){
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
    
    seasonWeekStandings <- rbind(seasonWeekStandings, standingTemp)
    
    seasonWeekStandingsConvergence <- rbind(
      seasonWeekStandingsConvergence,
      data.frame(season = i, week = j, Converge = k)
    )
    
    cat("Season", i, "Week", j, "\n")
  }
}
toc()
save(seasonWeekStandings, file = "./Model Fitting/Data/seasonWeekStandings.RData")

##### Plot results ----
seasonWeekStandingsMerge <- seasonWeekStandings |>
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

seasonWeekGameData <- gameData |>
  left_join(seasonWeekStandingsMerge |>
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
  left_join(seasonWeekStandingsMerge |>
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

seasonWeekGameDataLong <- gameDataLong |>
  left_join(seasonWeekStandingsMerge |>
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
  left_join(seasonWeekStandingsMerge |>
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

seasonWeekGameData <- seasonWeekGameData |>
  mutate(
    SRS_spread = home_SRS - away_SRS + 2,
    SRS_spread_diff = SRS_spread - spread_line
    ) |>
  filter(game_type == "REG") |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

ggplot(data = seasonWeekGameData, 
       aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  facet_wrap(vars(season))

ggplot(data = seasonWeekGameData, 
       aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE)

seasonWeekGameData |>
  group_by(season) |>
  summarise(
    mean(SRS_spread_diff, na.rm = TRUE)
    ) |>
  print(n = length(seasonTrain))

ggplot(data = seasonWeekGameData) +
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


