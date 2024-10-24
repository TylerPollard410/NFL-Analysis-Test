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

## MoV, SOS, SRS 2005----
gameDataLong2005 <- gameDataLong |> 
  filter(season == 2005) #|>
  #filter(game_type == "REG") 
standing2005 <- gameDataLong2005 |>
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
    MoV = result/games_played
  )

gameData2005 <- gameData |> 
  filter(season == 2005) #|>
  #filter(game_type == "REG")
standing2005b <- calculate_standings(gameData2005) |>
  arrange(team) |>
  mutate(
    win_pct_scale = scale(win_pct),
    sov_scale = scale(sov),
    sos_scale = scale(sos)
  )

standing2005b |>
  summarise(
    across(contains("win_pct"), ~mean(.x)),
    across(contains("sov"), ~mean(.x)),
    across(contains("sos"), ~mean(.x))
  )

standing2005$win == standing2005b$wins

standingPFRData <- read_html("https://www.pro-football-reference.com/years/2005/index.htm")
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

standing2005c <- bind_rows(
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
    team_abbr = ifelse(team_name == "Washington Redskins", "WAS", 
                       ifelse(team_name == "St Louis Rams", "STL", 
                              team_abbr)),
    team_abbr = clean_team_abbrs(team_abbr, 
                                 current = TRUE,
                                 keep_non_matches = TRUE)
  ) |>
  arrange(team_abbr) |>
  select(
    team_abbr,
    team_name,
    everything()
  ) |>
  rename(team = team_abbr)

standing2005$win == standing2005b$wins
standing2005$win == standing2005c$W


standing2005d <- left_join(standing2005c, standing2005b) |>
  arrange(desc(SRS)) |>
  select(-c(
    "conf", "division","div_rank","seed","games","wins","losses","ties","conf_pct","div_pct"
  ))


ind_teams <- gameDataLong2005 |> filter(team == "IND") |> pull(opponent)

# Load necessary libraries
library(dplyr)
library(nflreadr)

# Step 1: Set the Season Year to 2024
current_season <- 2024

# Step 2: Load Game Data using nflreadr
# Note: The load_schedules() function will fetch data for the specified season.
# For future seasons, data will only be available once the games have been played.
# Attempting to load data for future games may result in an empty or incomplete dataset.

# Fetch the schedule for the 2024 season
game_data <- load_schedules(current_season)

# # Check if data is available
# if (nrow(game_data) == 0) {
#   cat("No game data available for the 2024 season yet.\n")
# } else {
#   # Step 3: Prepare the Data
#   # Filter out games that haven't been played yet
#   played_games <- game_data %>%
#     filter(!is.na(home_score) & !is.na(away_score))
#   
#   if (nrow(played_games) == 0) {
#     cat("No completed games found for the 2024 season yet.\n")
#   } else {
#     # Create the game data frame in the required format
#     game_data_formatted <- played_games %>%
#       select(
#         Week = week,
#         Game_Date = gameday,
#         Team = home_team,
#         Opponent = away_team,
#         Team_Score = home_score,
#         Opponent_Score = away_score,
#         Location = "Home"
#       ) %>%
#       bind_rows(
#         played_games %>%
#           select(
#             Week = week,
#             Game_Date = gameday,
#             Team = away_team,
#             Opponent = home_team,
#             Team_Score = away_score,
#             Opponent_Score = home_score,
#             Location = "Away"
#           )
#       ) %>%
#       arrange(Game_Date)
#     
#     # Step 4: Calculate Point Differentials for Each Game
#     game_data_formatted <- game_data_formatted %>%
#       mutate(Point_Differential = Team_Score - Opponent_Score)
#     
#     # Step 5: Calculate Initial Average Point Differential for Each Team
#     team_stats <- game_data_formatted %>%
#       group_by(Team) %>%
#       summarise(
#         Games_Played = n(),
#         Total_Points_For = sum(Team_Score),
#         Total_Points_Against = sum(Opponent_Score),
#         Average_Point_Differential = mean(Point_Differential)
#       ) %>%
#       ungroup() %>%
#       mutate(
#         SRS = Average_Point_Differential, # Initial SRS (since SOS is 0 at first)
#         SOS = 0 # Initialize SOS to 0
#       )

standing2005new <- standing2005 |>
  mutate(
    SOS = 0,
    SRS = MoV
  )

# Step 6: Iterative Calculation of SRS and SOS
max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_SRS <- standing2005new$SRS
  
  # standing2005new <- standing2005new |>
  #   select(team, SRS)
  # 
  # gameDataLong2005A <- gameDataLong2005 |>
  #   select(team, opponent, MOV = result)
  # 
  # gameDataLong2005B <- gameDataLong2005A |>
  #   left_join(standing2005newA, by = c("opponent" = "team")) |>
  #   rename(SOS = SRS) |>
  #   group_by(team)
  # 
  # gameDataLong2005C <- gameDataLong2005B |>
  #   mutate(SRS = MOV + SOS)
  # 
  # gameDataLong2005D <- gameDataLong2005C |> summarise(
  #   newSOS = mean(SOS, na.rm = TRUE),
  #   newSRS = mean(SRS, na.rm = TRUE)
  # )
  # 
  # standing2005new <- standing2005new |>
  #   left_join(gameDataLong2005D) |>
  #   mutate(
  #     SOS = ifelse(is.na(newSOS), 0, newSOS),
  #     SRS = ifelse(is.na(newSRS), 0, newSRS)
  # )
  
  standing2005new <- standing2005new |>
    left_join(
      gameDataLong2005 |>
        select(team, opponent, MOV = result) |>
        left_join(standing2005new |> select(team, SRS), by = c("opponent" = "team")) |>
        rename(SOS = SRS) |>
        group_by(team) |>
        mutate(
          SRS = MOV + SOS
        ) |>
        summarise(
          newSOS = mean(SOS, na.rm = TRUE),
          newSRS = mean(SRS, na.rm = TRUE)
        )
    ) |>
    mutate(
      SOS = ifelse(is.na(newSOS), 0, newSOS),
      SRS = ifelse(is.na(newSRS), 0, newSRS)
    ) |>
    select(-newSOS, -newSRS)
  
  if(max(abs(standing2005new$SRS - previous_SRS)) < tolerance){
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations = ",i, "without full convergence.\n")
  }
}
standing2005newB <- standing2005new
standing2005newComp <- left_join(
  standing2005new,
  standing2005newB |> select(team, MoVold = MoV, SOSold = SOS, SRSold = SRS)
)

standing2005newDiff <- standing2005newComp |>
  mutate(
    MoVdiff = MoV - MoVold,
    SOSdiff = SOS - SOSold,
    SRSdiff = SRS - SRSold
  ) |>
  select(-c(MoV, MoVold, SOS, SOSold, SRS, SRSold))



## MoV, SOS, SRS 2005----
gameData2024 <- load_schedules(seasons = 2024) |>
  filter(complete.cases(result)) |>
  slice(1:93) |>
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
    MoV = result/games_played
  ) |>
  mutate(team_PPG = team_score/games_played, .after = team_score) |>
  mutate(opp_PPG = opponent_score/games_played, .after = opponent_score)

gameData2024 <- gameData |> 
  filter(season == 2024) |>
  filter(game_type == "REG")


standing2024b <- calculate_standings(gameData2024) |>
  arrange(team) |>
  mutate(
    win_pct_scale = scale(win_pct),
    sov_scale = scale(sov),
    sos_scale = scale(sos)
  )

standing2024b |>
  summarise(
    across(contains("win_pct"), ~mean(.x)),
    across(contains("sov"), ~mean(.x)),
    across(contains("sos"), ~mean(.x))
  )

standing2024$win == standing2024b$wins

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

standing2024c <- bind_rows(
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

standing2024$win == standing2024b$wins
standing2024$win == standing2024c$W


standing2024d <- left_join(standing2024c, standing2024b) |>
  arrange(desc(SRS)) |>
  select(-c(
    "conf", "division","div_rank","seed","games","wins","losses","ties","conf_pct","div_pct"
  ))


### 2024 MOV ----
standing2024new <- standing2024 |>
  mutate(SOS = 0, .after = MoV) |>
  mutate(SRS = MoV, .after = SOS) #|>
  #mutate(OSRS = PFG) |>
  #mutate(DSRS = 0)

# Step 6: Iterative Calculation of SRS and SOS
max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_SRS <- standing2024new$SRS
  
  standing2024new <- standing2024new |>
    left_join(
      gameDataLong2024 |>
        select(team, opponent, MOV = result) |>
        left_join(standing2024new |> select(team, SRS), by = c("opponent" = "team")) |>
        rename(SOS = SRS) |>
        group_by(team) |>
        mutate(
          SRS = MOV + SOS
        ) |>
        summarise(
          newSOS = mean(SOS, na.rm = TRUE),
          newSRS = mean(SRS, na.rm = TRUE)
        )
    ) |>
    mutate(
      SOS = ifelse(is.na(newSOS), 0, newSOS),
      SRS = ifelse(is.na(newSRS), 0, newSRS)
    ) |>
    select(-newSOS, -newSRS)
  
  if(max(abs(standing2024new$SRS - previous_SRS)) < tolerance){
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations = ",i, "without full convergence.\n")
  }
}
standing2024new <- standing2024new |> arrange(desc(SRS))

standing2024final <- left_join(
  standing2024d,
  standing2024new |> select(team, MOVnew = MoV, SOSnew = SOS, SRSnew = SRS)
) |>
  mutate(
    MOVdiff = MOVnew - MoV,
    SOSdiff = SOSnew - SoS,
    SRSdiff = SRSnew - SRS
  )

standing2024final$W == standing2024new$win
standing2024final$L == standing2024new$loss
standing2024final$`W-L%` == round(standing2024new$win_loss_percent, 3)
standing2024final$PF == standing2024new$team_score
standing2024final$PA == standing2024new$opponent_score
standing2024final$PD == standing2024new$result


### 2024 OSRS ----
standing2024new3 <- standing2024new |>
  mutate(OSRS = SRS) |>
  mutate(DSRS = 0) #|>
#mutate(OSRS = PFG) |>
#mutate(DSRS = 0)

team_stats <- game_data_formatted %>%
  group_by(Team) %>%
  summarise(
    Games_Played = n(),
    Total_Points_For = sum(Points_Scored),
    Total_Points_Against = sum(Points_Allowed),
    Average_Points_For = mean(Points_Scored),
    Average_Points_Against = mean(Points_Allowed)
  ) %>%
  ungroup() %>%
  mutate(
    Off_SRS = Average_Points_For,  # Initial Offensive SRS
    Def_SRS = Average_Points_Against,  # Initial Defensive SRS
    SRS = Off_SRS - Def_SRS,  # Initial Overall SRS
    Off_SOS = 0,  # Initialize Offensive SOS
    Def_SOS = 0   # Initialize Defensive SOS
  )

# Iterative Calculation of Offensive and Defensive SRS
max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_Off_SRS <- team_stats$Off_SRS
  previous_Def_SRS <- team_stats$Def_SRS
  
  # Update Offensive SOS for each team (Average of Opponents' Defensive SRS)
  Off_SOS_update <- game_data_formatted %>%
    left_join(team_stats %>% select(Team, Def_SRS), by = c("Opponent" = "Team")) %>%
    group_by(Team) %>%
    summarise(Off_SOS = mean(Def_SRS, na.rm = TRUE))
  
  # Update Defensive SOS for each team (Average of Opponents' Offensive SRS)
  Def_SOS_update <- game_data_formatted %>%
    left_join(team_stats %>% select(Team, Off_SRS), by = c("Opponent" = "Team")) %>%
    group_by(Team) %>%
    summarise(Def_SOS = mean(Off_SRS, na.rm = TRUE))
  
  # Merge SOS updates into team_stats
  team_stats <- team_stats %>%
    left_join(Off_SOS_update, by = "Team") %>%
    left_join(Def_SOS_update, by = "Team") %>%
    mutate(
      Off_SOS = ifelse(is.na(Off_SOS), 0, Off_SOS),
      Def_SOS = ifelse(is.na(Def_SOS), 0, Def_SOS)
    )
  
  # Update Offensive SRS
  team_stats <- team_stats %>%
    mutate(
      Off_SRS = Average_Points_For + Off_SOS,
      Def_SRS = Average_Points_Against - Def_SOS,  # Lower Def_SRS is better
      SRS = Off_SRS - Def_SRS  # Overall SRS
    )
  
  # Check for convergence
  if (max(abs(team_stats$Off_SRS - previous_Off_SRS), abs(team_stats$Def_SRS - previous_Def_SRS)) < tolerance) {
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations without full convergence.\n")
  }
}


standing2024new3 <- standing2024new |>
  mutate(OSRS = team_PPG - mean(team_PPG)) |>
  mutate(DSRS = 0) 

GD1 <- gameDataLong2024 |>
  select(team, opponent, team_score, opponent_score) |>
  group_by(opponent) |>
  mutate(
    tempDSOS = mean(opponent_score),
    tempOSOS = mean(team_score)
  )


GD2 <- GD1 |>
  group_by(team) |>
  summarise(newOSRS = round(mean(tempOSRS), 2))

GD3 <- left_join(standing2024new3, GD2) |>
  mutate(
    OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
    DSRS = SRS - OSRS
    ) |>
  select(-newOSRS)
  

GD1 <- gameDataLong2024 |>
  select(team, opponent, team_score, opponent_score) |>
  left_join(standing2024new3 |> select(team, OSRS))
  ungroup() 


# Step 6: Iterative Calculation of SRS and SOS
max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_OSRS <- standing2024new3$OSRS
  previous_DSRS <- standing2024new3$DSRS
  
  standing2024new3 <- standing2024new3 |>
    left_join(
      gameDataLong2024 |>
        select(team, opponent, team_score) |>
        group_by(opponent) |>
        mutate(
          tempDSRS = mean(team_score),
          tempOSRS = team_score - tempDSRS
        ) |>
        group_by(team) |>
        summarise(newOSRS = mean(tempOSRS))
    ) |>
    mutate(
      OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
      DSRS = SRS - OSRS
    ) |>
    select(-newOSRS)
  
  if(max(abs(standing2024new3$OSRS - previous_OSRS)) < tolerance){
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations = ",i, "without full convergence.\n")
  }
}
standing2024new3 <- standing2024new3 |> arrange(desc(SRS))
standing2024new3
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


