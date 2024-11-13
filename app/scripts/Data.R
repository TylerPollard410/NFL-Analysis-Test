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
## Set universal params ---- 
future::plan("multisession")
saveFileExt <- "~/Desktop/NFL Analysis Data/"
seasonTrain <- 2002:2023

## Data Dictionaries ----
dataDictionaries <- list(
  ## Data Dictionary: Combine
  combine = dictionary_combine,
  
  ## Data Dictionary: Contracts
  contracts = dictionary_contracts,
  
  ## Data Dictionary: Depth Charts
  depth_charts = dictionary_depth_charts,
  
  ## Data Dictionary: Draft Picks
  draft_picks = dictionary_draft_picks,
  
  ## Data Dictionary: ESPN QBR
  espn_qbr = dictionary_espn_qbr,
  
  ## Data Dictionary: Expected Fantasy Points
  ff_opportunity = dictionary_ff_opportunity,
  
  ## Data Dictionary: Fantasy Player IDs
  ff_playerids = dictionary_ff_playerids,
  
  ## Data Dictionary: Fantasy Football Rankings
  ff_rankings = dictionary_ff_rankings,
  
  ## Data Dictionary: FTN Charting Data
  ftn_charting = dictionary_ftn_charting,
  
  ## Data Dictionary: Injuries
  injuries = dictionary_injuries,
  
  ## Data Dictionary: Next Gen Stats
  nextgen_stats = dictionary_nextgen_stats,
  
  ## Data Dictionary: Participation
  participation = dictionary_participation,
  
  ## Data Dictionary: Play by Play
  pbp = dictionary_pbp,
  
  ## Data Dictionary: PFR Passing
  pfr_passing = dictionary_pfr_passing,
  
  ## Data Dictionary: Player Stats
  player_stats = dictionary_player_stats,
  
  ## Data Dictionary: Player Stats Defense
  player_stats_def = dictionary_player_stats_def,
  
  ## Data Dictionary: Rosters
  rosters = dictionary_rosters,
  
  ## Data Dictionary: Schedules
  schedules = dictionary_schedules,
  
  ## Data Dictionary: Snap Counts
  snap_counts = dictionary_snap_counts,
  
  ## Data Dictionary: Trades
  trades = dictionary_trades
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
  filter(season >= 2002) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 
#gameID <- sort(unique(gameData$game_id))

gameDataLong <- gameData |>
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

# Compare meta data
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

# Data Standings Table ----
## 2024 season ----
### PFR standing data ----
standingPFRData <- read_html("https://www.pro-football-reference.com/years/2024/index.htm")
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
  mutate(opp_PPG = opponent_score/games_played, .after = opponent_score) |>
  select(
    "team",
    "games_played",
    "win", 
    "loss",
    "tie", 
    "win_loss_percent",
    everything()
  )

standing2024$win == standing2024pfr$W

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
standing2024 <- standing2024 |> arrange(desc(SRS)) |>
  left_join(teamsData |> select(team_abbr, team_name), 
            by = c("team" = "team_abbr"))

### Compare Results ----
standing2024_comp <- standing2024 |>
  mutate(SRSrank = row_number()) |>
  select("team", "team_name", "SRSrank", everything()) |>
  mutate(
    win_loss_percent = round(win_loss_percent, 3),
    team_PPG = round(team_PPG, 3),
    opp_PPG = round(opp_PPG, 3),
    MOV = round(MOV, 1),
    SOS = round(SOS, 1),
    SRS = round(SRS, 1),
    OSRS = round(OSRS, 1),
    DSRS = round(DSRS, 1)
  )

colnames(standing2024_comp)
colnames(standing2024pfr)

standing2024pfr_comp <- standing2024pfr |> arrange(desc(SRS)) |>
  mutate(
    SRSrank = row_number(),
    games_played = W + L,
    team_PPG = round(PF/games_played, 3),
    opp_PPG = round(PA/games_played, 3)
  ) |>
  select(
    "team", 
    "team_name", 
    "SRSrank",
    "games_played",
    "win" = "W",
    "loss" = "L",
    "win_loss_percent" = "W-L%",
    "team_score" = "PF",
    "team_PPG",
    "opponent_score" = "PA",
    "opp_PPG",
    "result" = "PD",
    "MOV" = "MoV",
    "SOS" = "SoS",
    "SRS",
    "OSRS",
    "DSRS"
  ) |>
  rename_with(~paste0(.x, "_pfr"), .cols = c(-team,-team_name))

standing2024Comp <- standing2024_comp |>
  left_join(standing2024pfr_comp,
            by = join_by(team, team_name)) |>
  mutate(
    MOV_diff = MOV_pfr - MOV,
    SOS_diff = SOS_pfr - SOS,
    SRS_diff = SRS_pfr - SRS,
    OSRS_diff = OSRS_pfr - OSRS,
    DSRS_diff = DSRS_pfr - DSRS
  ) |>
  select(
    "team", 
    "team_name", 
    #contains("SRSrank"),
    contains("games_played"),
    contains("win"),
    contains("loss"),
    contains("win_loss_percent"),
    contains("team_score"),
    contains("team_PPG"),
    contains("opponent_score"),
    contains("opp_PPG"),
    contains("result"),
    contains("MOV"),
    contains("SOS"),
    starts_with("SRS"),
    starts_with("OSRS"),
    starts_with("DSRS")
  )

standing2024Comp2 <- standing2024Comp |>
  select(
    "team", 
    "team_name", 
    contains("MOV"),
    contains("SOS"),
    starts_with("SRSrank"),
    starts_with("SRS"),
    starts_with("OSRS"),
    starts_with("DSRS"),
    -contains("pfr")
  ) |>
  mutate(
    SRSrank_pfr = standing2024Comp$SRSrank_pfr, .after = SRSrank
  )

## 2002 season ----
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
standing2002 <- standing2002 |> 
  mutate(season = 2002, .before = 1)
standing2002pfr <- standing2002pfr |> arrange(desc(SRS))

## Historical season games ----
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

seasonTrain <- sort(unique(gameData$season))

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
save(seasonStandings, file = "./_data/seasonStandings.RData")

#### Add seeds ----
load(file = "./_data/seasonStandings.RData")
seasonStandings <- seasonStandings |> 
  left_join(teamsData |> select(team_abbr, team_name, team_conf, team_division), 
            by = c("team" = "team_abbr")) |>
  select(
    team,
    team_name,
    team_conf, 
    team_division,
    everything()
  )

seasonStandingsNFLverse <- calculate_standings(
  nflverse_object = gameData |> filter(!is.na(result)),
  tiebreaker_depth = 2
)

seasonStandingsTableData <- seasonStandings |>
  left_join(
    seasonStandingsNFLverse |>
      select(
        season,
        team,
        div_rank,
        seed,
        div_pct,
        conf_pct,
        sov,
        sos),
    by = join_by(season, team)
  ) |>
  select(
    season,
    team,
    team_name,
    team_conf,
    team_division,
    div_rank,
    seed,
    games_played,
    win,
    loss,
    tie,
    win_loss_percent,
    conf_pct,
    div_pct,
    everything()
  ) |>
  left_join(
    teamsData,
    by = join_by(team == team_abbr,team_name, team_conf, team_division)
  ) |>
  rename(
    "GP" = games_played,
    "W" = win,
    "L" = loss,
    "T" = tie,
    "W-L%" = win_loss_percent,
    "CON%" = conf_pct,
    "DIV%" = div_pct,
    "PF" = team_score,
    "PA" = opponent_score,
    "PD" = result
  )
seasonStandings <- seasonStandingsTableData
save(
  seasonStandings,
  file = "./_data/seasonStandings.RData"
)

### By Season and Week ----
#### No prior ----
seasonWeekStandings <- data.frame()
seasonWeekStandingsConvergence <- data.frame()
seasonTrain <- 2002:most_recent_season()

tic()
for(i in seasonTrain){
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
    
    seasonWeekStandingsConvergence <- rbind(
      seasonWeekStandingsConvergence,
      data.frame(season = i, week = j, Converge = k)
    )
    
    cat("Season", i, "Week", j, "\n")
  }
}
toc()
save(seasonWeekStandings, file = "./_data/seasonWeekStandings.RData")

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

seasonStandingsComb <- bind_rows(
  standing2002,
  seasonStandings
) |>
  mutate(week = 1, .after = season) |>
  mutate(season = season + 1) |>
  filter(season <= 2023)

seasonWeekStandingsMerge2 <- seasonWeekStandingsMerge


seasonWeekGameData1 <- gameData |>
  left_join(seasonStandingsComb |>
              select(
                season,
                week,
                team,
                # games_played,
                # win,
                # loss,
                # tie,
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
  left_join(seasonStandingsComb |>
              select(
                season,
                week,
                team,
                # games_played,
                # win,
                # loss,
                # tie,
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
            by = join_by(season, week, "away_team" == "team")) |>
  filter(week == 1)
seasonWeekGameData2 <- gameData |>
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
            by = join_by(season, week, "away_team" == "team")) |>
  filter(week != 1)

seasonWeekGameData <- bind_rows(
  seasonWeekGameData1,
  seasonWeekGameData2
) |>
  arrange(season, week, gsis)


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
            by = join_by(season, week, "opponent" == "team")) |>
  filter(week != 1)

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
             color = "orange", linewidth = 1) +
  geom_vline(aes(xintercept = median(SRS_spread_diff, na.rm = TRUE)),
             color = "orange3", linewidth = 1) +
  geom_density(#data = final_data3,
    aes(x = SRS_spread_diff),
    color = "#007C7C", 
    linewidth = 1) +
  facet_wrap(vars(season)) +
  theme_bw()

#### Prior ----
seasonWeekPriorStandings <- data.frame()
seasonWeekPriorStandingsConvergence <- data.frame()

tic()
for(i in seasonTrain){
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
save(seasonWeekPriorStandings, file = "./Model Fitting/Data/seasonWeekPriorStandings.RData")

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


