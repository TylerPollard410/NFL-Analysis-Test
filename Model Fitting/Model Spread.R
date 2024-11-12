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
library(patchwork)

## Modeling ----
library(pracma)
library(forecast)
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

# Setwd
setwd("Model Fitting")

# Load Data ----
## Teams Data 
teamsData <- load_teams(current = FALSE)

##  Game/Schedule Data
gameData <- load_schedules(seasons = TRUE) |>
  filter(season >= 2002) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) 

## Standings
#load(file = "./_data/seasonStandings.RData")
load(file = "../app/_data/seasonWeekStandings.RData")

## Rosters
rostersWeekData <- load_rosters_weekly(seasons = TRUE) |>
  filter(season >= 2002) 

## Injuries
injuryData <- load_injuries(seasons = TRUE) |>
  filter(season >= 2002) 

injurySnippet <- injuryData |>
  filter(season == 2024) |>
  filter(week == 1)

## Player Stats 
playerOffenseData <- load_player_stats(seasons = TRUE, stat_type = "offense") |>
  filter(season >= 2002) 

playerOffenseSnippet <- playerOffenseData |>
  filter(season == 2024) |>
  filter(week == 1)

playerDefenseData <- load_player_stats(seasons = TRUE, stat_type = "defense") |>
  filter(season >= 2002) 

playerDefenseSnippet <- playerDefenseData |>
  filter(season == 2024) |>
  filter(week == 1)

playerKickingData <- load_player_stats(seasons = TRUE, stat_type = "kicking") |>
  filter(season >= 2002) 

playerKickingSnippet <- playerKickingData |>
  filter(season == 2024) |>
  filter(week == 1)

## Next Gen
nextGenData <- load_nextgen_stats(seasons = TRUE) |>
  filter(season >= 2002)

nextGenSnippet <- nextGenData |>
  filter(season == 2024) |>
  filter(week == 1)

## play by play
pbpData <- load_pbp(seasons = TRUE) |>
  filter(season >= 2002)

# EPA ----
## Game Level calc ----
unique(pbpData$play_type)
unique(pbpData$play_type_nfl)

### Passing ----
tic()
epaPass <- pbpData |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "pass") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_pass_epa_total = sum(epa),
    off_pass_epa = mean(epa),
    off_pass_wpa_total = sum(vegas_wpa),
    off_pass_wpa = mean(vegas_wpa),
    off_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_pass_epa_adj = mean(epa*scaled_vegas_wp),
    def_pass_epa_total = sum(-epa),
    def_pass_epa = mean(-epa),
    def_pass_wpa_total = sum(-vegas_wpa),
    def_pass_wpa = mean(-vegas_wpa),
    def_pass_epa_adj_total = sum(-epa*scaled_vegas_wp),
    def_pass_epa_adj = mean(-epa*scaled_vegas_wp)
  ) |>
  ungroup()
toc()

### Rushing ----
tic()
epaRush <- pbpData |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "run") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_rush_epa_total = sum(epa),
    off_rush_epa = mean(epa),
    off_rush_wpa_total = sum(vegas_wpa),
    off_rush_wpa = mean(vegas_wpa),
    off_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_rush_epa_adj = mean(epa*scaled_vegas_wp),
    def_rush_epa_total = sum(-epa),
    def_rush_epa = mean(-epa),
    def_rush_wpa_total = sum(-vegas_wpa),
    def_rush_wpa = mean(-vegas_wpa),
    def_rush_epa_adj_total = sum(-epa*scaled_vegas_wp),
    def_rush_epa_adj = mean(-epa*scaled_vegas_wp)
  ) |>
  ungroup()
toc()

### Combined ----
epaData <- left_join(
  epaPass,
  epaRush,
  by = join_by(game_id, season, week, posteam, home_team, away_team)
)
rm(epaPass, epaRush)

### Merge with Game Data ----
epaGameData <- left_join(
  gameData,
  epaData |> 
    select(-home_team, -away_team) |>
    rename_with(~paste0("home_", .x), .cols = contains("off")) |>
    rename_with(~paste0("away_", .x), .cols = contains("def")),
  by = join_by(game_id, season, week, "home_team" == "posteam")) |>
  left_join(
    epaData |> 
      select(-home_team, -away_team) |>
      rename_with(~paste0("away_", .x), .cols = contains("off")) |>
      rename_with(~paste0("home_", .x), .cols = contains("def")),
    by = join_by(game_id, season, week, "away_team" == "posteam"))

### Merge SRS data ----
epaGameData <- epaGameData |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team, 
             home_MOV = MOV, 
             home_SOS = SOS, 
             home_SRS = SRS, 
             home_OSRS = OSRS, 
             home_DSRS = DSRS),
    by = join_by(season, week, "home_team" == "team")
  ) |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team, 
             away_MOV = MOV, 
             away_SOS = SOS, 
             away_SRS = SRS, 
             away_OSRS = OSRS, 
             away_DSRS = DSRS),
    by = join_by(season, week, "away_team" == "team")
  )

epaGameDataLong <- epaGameData |>
  clean_homeaway()


## EPA trends ----
epaGameDataLag <- epaGameData |>
  select(
    game_id, season, week, game_type, 
    home_team, home_score,
    away_team, away_score,
    home_rest, away_rest,
    div_game, 
    roof, 
    surface, 
    temp, 
    wind,
    home_MOV, away_MOV,
    home_SOS, away_SOS,
    home_SRS, away_SRS,
    home_OSRS, away_OSRS,
    home_DSRS, away_DSRS,
    home_off_pass_epa_adj,
    home_off_rush_epa_adj,
    home_def_pass_epa_adj,
    home_def_rush_epa_adj,
    away_off_pass_epa_adj,
    away_off_rush_epa_adj,
    away_def_pass_epa_adj,
    away_def_rush_epa_adj
  )

### No Moving Avg ----
epaGameDataLongLag <- epaGameDataLag |>
  clean_homeaway() |>
  group_by(season, team) |>
  mutate(
    team_MOV = lag(team_MOV),
    team_SOS = lag(team_SOS),
    team_SRS = lag(team_SRS),
    team_OSRS = lag(team_OSRS),
    team_DSRS = lag(team_DSRS),
    # opponent_SRS = lag(opponent_SRS),
    team_off_pass_epa_adj = lag(team_off_pass_epa_adj),
    team_off_rush_epa_adj = lag(team_off_rush_epa_adj),
    team_def_pass_epa_adj = lag(team_def_pass_epa_adj),
    team_def_rush_epa_adj = lag(team_def_rush_epa_adj)
    # opponent_off_pass_epa_adj = lag(opponent_off_pass_epa_adj),
    # opponent_off_rush_epa_adj = lag(opponent_off_rush_epa_adj),
    # opponent_def_pass_epa_adj = lag(opponent_def_pass_epa_adj),
    # opponent_def_rush_epa_adj = lag(opponent_def_rush_epa_adj)
  ) |>
  ungroup()

epaGameDataLag <- epaGameDataLag |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(location == "home") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) |>
  left_join(epaGameDataLongLag |>
              filter(location == "away") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) 

### Moving Avg ----
window <- 8
epaGameDataLongLag <- epaGameDataLag |>
  clean_homeaway() |>
  group_by(team) |>
  mutate(
    team_MOV = lag(team_MOV),
    team_SOS = lag(team_SOS),
    team_SRS = lag(team_SRS),
    team_OSRS = lag(team_OSRS),
    team_DSRS = lag(team_DSRS),
    # opponent_SRS = lag(opponent_SRS),
    team_off_pass_epa_adj = lag(team_off_pass_epa_adj),
    team_off_rush_epa_adj = lag(team_off_rush_epa_adj),
    team_def_pass_epa_adj = lag(team_def_pass_epa_adj),
    team_def_rush_epa_adj = lag(team_def_rush_epa_adj)
    # opponent_off_pass_epa_adj = lag(opponent_off_pass_epa_adj),
    # opponent_off_rush_epa_adj = lag(opponent_off_rush_epa_adj),
    # opponent_def_pass_epa_adj = lag(opponent_def_pass_epa_adj),
    # opponent_def_rush_epa_adj = lag(opponent_def_rush_epa_adj)
  ) |>
  mutate(
    team_MOV_mov = ifelse(is.na(team_MOV), lag(team_MOV), team_MOV),
    team_SOS_mov = ifelse(is.na(team_SOS), lag(team_SOS), team_SOS),
    team_SRS_mov = ifelse(is.na(team_SRS), lag(team_SRS), team_SRS),
    team_OSRS_mov = ifelse(is.na(team_OSRS), lag(team_OSRS), team_OSRS),
    team_DSRS_mov = ifelse(is.na(team_DSRS), lag(team_DSRS), team_DSRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj), lag(team_off_pass_epa_adj), team_off_pass_epa_adj),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj), lag(team_off_rush_epa_adj), team_off_rush_epa_adj),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj), lag(team_def_pass_epa_adj), team_def_pass_epa_adj),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj), lag(team_def_rush_epa_adj), team_def_rush_epa_adj)
  ) |>
  mutate(
    team_MOV_mov = ifelse(is.na(team_MOV), 0, team_MOV),
    team_SOS_mov = ifelse(is.na(team_SOS), 0, team_SOS),
    team_SRS_mov = ifelse(is.na(team_SRS_mov), 0, team_SRS),
    team_OSRS_mov = ifelse(is.na(team_OSRS), 0, team_OSRS),
    team_DSRS_mov = ifelse(is.na(team_DSRS), 0, team_DSRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj_mov), 0, team_off_pass_epa_adj_mov),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj_mov), 0, team_off_rush_epa_adj_mov),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj_mov), 0, team_def_pass_epa_adj_mov),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj_mov), 0, team_def_rush_epa_adj_mov)
  ) |>
  mutate(
    team_MOV_mov = movavg(team_MOV_mov, n = window, type = "r"),
    team_SOS_mov = movavg(team_SOS_mov, n = window, type = "r"),
    team_SRS_mov = movavg(team_SRS_mov, n = window, type = "r"),
    team_OSRS_mov = movavg(team_OSRS_mov, n = window, type = "r"),
    team_DSRS_mov = movavg(team_DSRS_mov, n = window, type = "r"),
    team_off_pass_epa_adj_mov = movavg(team_off_pass_epa_adj_mov, n = window, type = "r"),
    team_off_rush_epa_adj_mov = movavg(team_off_rush_epa_adj_mov, n = window, type = "r"),
    team_def_pass_epa_adj_mov = movavg(team_def_pass_epa_adj_mov, n = window, type = "r"),
    team_def_rush_epa_adj_mov = movavg(team_def_rush_epa_adj_mov, n = window, type = "r")
  ) |>
  ungroup()

# window <- 8
# BALsrs <- epaGameDataLongLag |> 
#   filter(season %in% 2022:2024) |>
#   filter(team == "BAL") |>
#   select(game_id, season, week, game_type, team, team_SRS) |>
#   mutate(team_SRS_mov = ifelse(is.na(team_SRS), lag(team_SRS, n = 1), team_SRS)) |>
#   filter(season %in% 2023:2024) |>
#   mutate(movS = movavg(team_SRS_mov, 8, type = "s")) |>
#   mutate(movT = movavg(team_SRS_mov, 8, type = "t")) |>
#   mutate(movW = movavg(team_SRS_mov, 8, type = "w")) |>
#   mutate(movM = movavg(team_SRS_mov, 8, type = "m")) |>
#   mutate(movE = movavg(team_SRS_mov, 8, type = "e")) |>
#   mutate(movR = movavg(team_SRS_mov, 8, type = "r"))
# BALsrs

epaGameDataLag <- epaGameDataLag |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(location == "home") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              select(game_id, 
                     contains("MOV"),
                     contains("SOS"),
                     contains("SRS"),
                     contains("OSRS"),
                     contains("DSRS"),
                     contains("epa")),
            by = join_by(game_id)) |>
  left_join(epaGameDataLongLag |>
              filter(location == "away") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              select(game_id, 
                     contains("MOV"),
                     contains("SOS"),
                     contains("SRS"),
                     contains("OSRS"),
                     contains("DSRS"),
                     contains("epa")),
            by = join_by(game_id)) 



# Modelling -----
## Clean Data ----
colnames(epaGameDataLag)
modelDataFull <- epaGameDataLag |>
  select(
    game_id, season, week, game_type, 
    home_team, home_score,
    away_team, away_score,
    home_rest, away_rest,
    div_game, 
    roof, 
    surface, 
    temp, 
    wind,
    home_MOV, home_MOV_mov,
    away_MOV, away_MOV_mov,
    home_SOS, home_SOS_mov,
    away_SOS, away_SOS_mov,
    home_SRS, home_SRS_mov,
    away_SRS, away_SRS_mov,
    home_OSRS, home_OSRS_mov,
    away_OSRS, away_OSRS_mov,
    home_DSRS, home_DSRS_mov,
    away_DSRS, away_DSRS_mov,
    home_off_pass_epa_adj, home_off_pass_epa_adj_mov,
    home_off_rush_epa_adj, home_off_rush_epa_adj_mov,
    home_def_pass_epa_adj, home_def_pass_epa_adj_mov,
    home_def_rush_epa_adj, home_def_rush_epa_adj_mov,
    away_off_pass_epa_adj, away_off_pass_epa_adj_mov,
    away_off_rush_epa_adj, away_off_rush_epa_adj_mov,
    away_def_pass_epa_adj, away_def_pass_epa_adj_mov,
    away_def_rush_epa_adj, away_def_rush_epa_adj_mov
  ) |>
  left_join(
    gameData |> 
      select(
        game_id,
        result,
        spread_line, home_spread_odds, away_spread_odds,
        total, 
        total_line, under_odds, over_odds,
        home_moneyline, away_moneyline
      )
  ) |>
  mutate(
    home_spread_prob = ifelse(home_spread_odds < 0, 
                              abs(home_spread_odds)/(abs(home_spread_odds) + 100),
                              100/(home_spread_odds + 100)),
    .after = home_spread_odds
  ) |>
  mutate(
    away_spread_prob = ifelse(away_spread_odds < 0, 
                              abs(away_spread_odds)/(abs(away_spread_odds) + 100),
                              100/(away_spread_odds + 100)),
    .after = away_spread_odds
  ) |>
  mutate(
    under_prob = ifelse(under_odds < 0, 
                        abs(under_odds)/(abs(under_odds) + 100),
                        100/(under_odds + 100)),
    .after = under_odds
  ) |>
  mutate(
    over_prob = ifelse(over_odds < 0, 
                       abs(over_odds)/(abs(over_odds) + 100),
                       100/(over_odds + 100)),
    .after = over_odds
  ) |>
  mutate(
    home_moneyline_prob = ifelse(home_moneyline < 0, 
                                 abs(home_moneyline)/(abs(home_moneyline) + 100),
                                 100/(home_moneyline + 100)),
    .after = home_moneyline
  ) |>
  mutate(
    away_moneyline_prob = ifelse(away_moneyline < 0, 
                                 abs(away_moneyline)/(abs(away_moneyline) + 100),
                                 100/(away_moneyline + 100)),
    .after = away_moneyline
  ) |>
  mutate(
    cover = ifelse(result > spread_line, TRUE, 
                   ifelse(result < spread_line, FALSE, NA)),
    .after = spread_line
  )  |>
  mutate(
    total_cover = ifelse(total > total_line, TRUE, 
                   ifelse(total < total_line, FALSE, NA)),
    .after = total_line
  )
colnames(modelDataFull)

# Lag data
modelDataFullLong <- modelDataFull |>
  clean_homeaway() 

# Complete Cases
modelDataFull |>
  filter(complete.cases(home_score, away_score)) |> 
  filter(!(season == 2024 & week > 9)) |> 
  summarise(across(everything(), ~sum(complete.cases(.x)))) |> t()

# Make factors
modelData <- modelDataFull |>
  filter(complete.cases(home_score, away_score)) |> 
  filter(!(season == 2024 & week > 9)) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE),
    game_type_bin = factor(game_type,
                           levels = c("REG", "WC", "DIV", "CON", "SB"),
                           ordered = TRUE),
    game_type = factor(game_type,
                       levels = c("REG", "WC", "DIV", "CON", "SB"),
                       labels = c("REG", "POST", "POST", "POST", "POST"),
                       ordered = TRUE),
    home_team = factor(home_team),
    away_team = factor(away_team),
    div_game = factor(div_game,
                      levels = c(0,1),
                      labels = c("No", "Yes")),
    roof = factor(roof),
    surface = factor(surface)
  ) |>
  mutate(
    temp2 = ifelse(is.na(temp), 70, temp), .after = temp
  ) |>
  mutate(
    wind2 = ifelse(is.na(wind), 0, wind), .after = wind
  ) |>
  select(game_id, season, week, game_type, game_type_bin, everything()) |>
  mutate(spread_line_scale = spread_line, .after = spread_line)

modelDataLong <- modelData |>
  clean_homeaway(invert = c("result", "spread_line", "spread_line_scale"))

## Plot ----
### Histogram ----
modelDataPlot <- modelData |> 
  filter(season >= 2021) |>
  filter(!(season == 2024 & week > 9))

ggplot(data = modelDataPlot) +
  geom_histogram(
    aes(x = total/total_line, after_stat(density)),
    color = "lightblue3", fill = "lightblue", bins = 100
    ) +
  geom_density(
    aes(x = total/total_line),
    color = "lightblue4", 
    linewidth = 1) +
  theme_bw() 

### Boxplot ----
ggplot(data = modelDataPlot) +
  geom_boxplot(
    aes(y = total),
    color = "lightblue3", fill = "lightblue") +
  theme_bw() 

ggplot(data = modelDataPlot) +
  geom_boxplot(
    aes(x = away_team, y = total),
    color = "lightblue3", fill = "lightblue") +
  theme_bw() 

### Scatter ----
# home_rest + away_rest +
#   home_SRS + away_SRS +
#   home_off_pass_epa_adj + away_def_pass_epa_adj +
#   home_off_rush_epa_adj + away_def_rush_epa_adj +
#   home_def_pass_epa_adj + away_off_pass_epa_adj +
#   home_def_rush_epa_adj + away_off_rush_epa_adj +
#   game_type +
#   div_game +
#   #roof +
#   #surface +
#   temp2 +
#   wind2 +
#   (1|season) + (1|week)

#### Continuous ----
ggplot(data = modelDataPlot, aes(x = week, y = total)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  sm_statCorr() +
  theme_bw()

ggplot(data = modelDataPlot, aes(x = home_SRS, y = total)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  sm_statCorr() +
  facet_wrap(vars(week)) +
  theme_bw()

ggplot(data = modelDataPlot, aes(x = result, y = total)) +
  geom_point(alpha = 0.1) +
  geom_density2d() +
  theme_bw()

#### Categorical
ggplot(data = modelData, aes(x = home_team, y = total)) +
  geom_boxplot(alpha = 0.1) +
  geom_smooth() +
  theme_bw()

ggplot(data = modelData, aes(x = home_team, y = total)) +
  geom_boxplot(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(vars(game_type), nrow = 2) +
  theme_bw()

## Split data ----
seasons <- sort(unique(as.numeric(as.character(modelData$season))))
seasonsTrain <- tail(seasons, 3) - 1
seasonsTest <- tail(seasons, 1)

modelDataTrain <- modelData |>
  filter(season %in% seasonsTrain) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  result, spread_line, 
                                  cover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, 
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x)})
  )

str(modelDataTrain)

table(modelDataTrain$roof, modelDataTrain$game_type)

modelDataTest <- modelData |>
  filter(season %in% seasonsTest) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  result, spread_line, 
                                  cover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, 
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x,
                             center = attr(modelDataTrain |> pull(x), "scaled:center"),
                             scale = attr(modelDataTrain |> pull(x), "scaled:scale"))})
  )
str(gameDataTest)

save(list = ls(), file = "../../NFL Analysis Data/ModelData.RData")

## Fit ----
### Model ----
iters <- 5000
burn <- 1000
chains <- 1
sims <- (iters-burn)*chains

Fit <- brm(
  bf(total ~ 
       #spread_line_scale +
       #(1|home_team) + #(1|away_team) +
       home_MOV_mov + 
       #away_MOV_mov +
       #home_SOS_mov +
       #away_SOS_mov +
       #home_SRS_mov +
       #away_SRS_mov +
       #home_OSRS_mov +
       away_OSRS_mov +
       #home_DSRS_mov +
       #away_DSRS_mov +
       #home_rest +
       away_rest +
       home_off_pass_epa_adj + 
       #away_def_pass_epa_adj +
       #home_off_rush_epa_adj + 
       #away_def_rush_epa_adj +
       #home_def_pass_epa_adj + 
       away_off_pass_epa_adj +
       #home_def_rush_epa_adj + 
       #away_off_rush_epa_adj +
       #game_type +
       div_game +
       roof +
       #surface +
       #temp2 +
       wind2 +
       (1|season)# +
       #(1|week)
  ),
  data = modelDataTrain,
  family = brmsfamily(family = "negbinomial"),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = TRUE,
  control = list(adapt_delta = 0.95)
)

fit <- 22
assign(paste0("fit", fit), Fit)

#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

### Diagnostics ----
# prior_summary(Fit)
# posterior_summary(Fit)
# launch_shinystan(Fit)

print(Fit, digits = 4)
fixedEff <- fixef(Fit)
fixedEff2 <- data.frame(fixedEff) |>
  mutate(
    p_val = dnorm(Estimate/Est.Error)
  ) |>
  mutate(
    across(everything(), function(x){round(x, 4)})
  ) |>
  mutate(
    Sig = ifelse(p_val < 0.01, "***",
                 ifelse(p_val < 0.05, "**",
                        ifelse(p_val < 0.1, "*", "")))
  )
fixedEff2

#plot(Fit)
PPCplot <- pp_check(Fit, ndraws = 100) + 
  labs(title = paste0("Fit", fit, " PPC")) +
  theme_bw()

PPCplot

performance::check_distribution(Fit)
performance::check_outliers(Fit)
performance::check_heteroskedasticity(Fit)
performance_rmse(Fit)
performance_mae(Fit)
model_performance(Fit)

variance_decomposition(Fit)
ranef(Fit)


FitR2 <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())

bayes_factor(Fit, fit19)
bayes_factor(fit15, fit13)
bayes_factor(fit13, fit15)
bayes_factor(fit9, Fit)
bayes_factor(fit12, fit8)
bayes_factor(fit8, fit7)

Fitloo <- loo(Fit)
Fitloo2 <- loo(fit13)
loo_compare(Fitloo, Fitloo2)

Fitsmooths <- conditional_smooths(Fit,
                                  method = "posterior_predict")
plot(Fitsmooths, 
     stype = "raster", 
     ask = FALSE,
     theme = theme(legend.position = "bottom"))
plot(Fitsmooths, 
     stype = "contour", 
     ask = FALSE,
     theme = theme(legend.position = "bottom"))

Fiteffects <- conditional_effects(Fit, 
                                  method = "posterior_predict",
                                  robust = FALSE)
plot(Fiteffects, 
     points = TRUE, 
     ask = FALSE)

### Prediction ----
#### Datasets ----
modelDataTrainNA <- modelDataTrain |>
  filter(!is.na(home_SRS))

modelDataTestNA <- modelDataTest |>
  filter(!is.na(home_SRS))

#### Errors ----
# Fit
Fitted <- posterior_predict(Fit)
FittedMean <- colMeans(Fitted)
FittedMed <- apply(Fitted, 2, function(x){quantile(x, 0.5)})
FittedLCB <- apply(Fitted, 2, function(x){quantile(x, 0.025)})
FittedUCB <- apply(Fitted, 2, function(x){quantile(x, 0.975)})

# Prediction
Preds <- posterior_predict(Fit, 
                           newdata = modelDataTestNA,
                           allow_new_levels = TRUE, 
                           re_formula = NULL
)
PredsMean <- colMeans(Preds)
PredsMed <- apply(Preds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCB <- apply(Preds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCB <- apply(Preds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

totalTrain <- modelDataTrainNA$total
totalTest <- modelDataTestNA$total
predMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_fit = mean(abs(FittedMean - totalTrain)),
  MAD_fit = mean(abs(FittedMed - totalTrain)),
  COV_fit = mean(FittedLCB < totalTrain & totalTrain < FittedUCB),
  MAE_pred = mean(abs(PredsMean - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMed - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsLCB < totalTest & totalTest < PredsUCB)
)
predMetrics

#### Prob Errors ----
##### Fit ----
totalLineTrain <- modelDataTrainNA$total_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbs <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- Fitted[, j]
  probs <- fitted > totalLineTrain[j]
  FittedProbs[, j] <- probs
}
FittedBet <- colMeans(FittedProbs)
FittedBetLogical <- FittedBet > 0.5
FittedResultLogical <- totalTrain > totalLineTrain
FittedResultProb <- mean(FittedBetLogical == FittedResultLogical, na.rm = TRUE)
FittedResultProb

totalDataTrain <- modelDataTrainNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         total, total_line,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = FittedMean,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverTotal = ifelse(total > total_line, TRUE,
                        ifelse(total < total_line, FALSE, NA)),
    coverSuccess = coverBet == coverTotal,
    totalCoverProb = FittedBet,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                            ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == coverTotal
  )

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

##### Pred ----
totalLineTest <- modelDataTestNA$total_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbs <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- Preds[, j]
  probs <- fitted > totalLineTest[j]
  PredsProbs[, j] <- probs
}
PredsBet <- colMeans(PredsProbs)
PredsBetLogical <- PredsBet > 0.5
PredsResultLogical <- totalTest > totalLineTest
PredsResultProb <- mean(PredsBetLogical == PredsResultLogical, na.rm = TRUE)
PredsResultProb

totalDataTest <- modelDataTestNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         total, total_line,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = PredsMean,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverTotal = ifelse(total > total_line, TRUE,
                        ifelse(total < total_line, FALSE, NA)),
    coverSuccess = coverBet == coverTotal,
    totalCoverProb = PredsBet,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == coverTotal
  )

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

##### Combined Success ----
totalSuccess <- bind_cols(
  Fit = paste0("Fit", fit),
  totalSuccessTrain,
  totalSuccessTest
)

#predMetricsComb <- data.frame()
predMetricsComb <- bind_rows(
  predMetricsComb,
  predMetrics
) |>
  arrange(MAE_pred)
predMetricsComb

#totalSuccessComb <- data.frame()
totalSuccessComb <- bind_rows(
  totalSuccessComb,
  totalSuccess
) |>
  arrange(desc(totalOddsProbTest))
totalSuccessComb


#looFits <- list()
# for(i in 1:fit){
#   looFits[[paste0("Fit",i)]] <- loo(get(paste0("fit", i)))
# }
looFits[[paste0("Fit",fit)]] <- loo(get(paste0("fit", fit)))
looComb <- loo_compare(looFits)
looComb

save(predMetricsComb, totalSuccessComb, looComb, fitFormulas,
     file = "./Data/resultFitDiagnostics.RData")



### Plotting ----
#### Fit ----
##### Home ----
ppc_dens_overlay(y = modelDataTestNA$total, yrep = Preds) +
  labs(title = "Home Fit Predict") +
  theme_bw()

ppc_dens_overlay(y = modelDataTestNA$away_score, yrep = awayfinalPreds) +
  labs(title = "Away Fit Predict") +
  theme_bw()

FitFitDF <- bind_cols(
  StormdataTrain,
  LCB = FitfinalFitLCB,
  Mean = FitfinalFitMean,
  Med = FitfinalFitMed,
  UCB = FitfinalFitUCB
)

FitstormsFitplot <- ggplot(data = FitFitDF, aes(x = StormElapsedTime)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), fill = "lightblue") +
  geom_line(aes(y = VMAX, color = "Observed")) +
  geom_line(aes(y = Mean, color = "PPD Mean")) +
  facet_wrap(vars(StormID))+
  scale_y_continuous(limits = c(0,275), breaks = seq(0,275,50)) +
  labs(title = "Fit PPD Mean vs Observed VMAX",
       subtitle = "95% Credible Interval about PPD Mean") +
  scale_color_manual(name = NULL, values = c("black","red")) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  theme_bw()
FitstormsFitplot

#### Residuals ----
FitResiduals <- residuals(Fit, 
                          method = "posterior_predict",
                          summary = FALSE)

FitResidualsSum <- colMeans(FitResiduals)

# Extract residuals and fitted values from the baseline model
residuals_baseline <- residuals(Fit, summary = TRUE)
fitted_vals <- fitted(fit_baseline)

# Plot residuals vs Sea_Surface_Temp to check for heteroscedasticity
plot(StormdataTrain5$arcsinhMINSLP, FitResidualsSum, 
     xlab = "arcsinhMINSLP", ylab = "Residuals", 
     main = "Residuals vs arcsinhMINSLP")

# Similarly, plot residuals against other predictors
plot(hurricane_data$Pressure, residuals_baseline, 
     xlab = "Pressure", ylab = "Residuals", 
     main = "Residuals vs Pressure")

modelParams <- row.names(fixef(Fit15))
modelParams <- str_subset(modelParams, "sigma", negate = TRUE)
modelParams <- str_subset(modelParams, "Intercept", negate = TRUE)
modelParams[1] <- "basin"
modelParams[4] <- "Land"
modelParams
resids_list <- list()
for(i in modelParams){
  resids_list[[i]] <- ppc_error_scatter_avg_vs_x(StormdataTrain5$VMAX, 
                                                 FitfinalFit,
                                                 as.numeric(StormdataTrain5[[i]])) +
    geom_smooth(method = "lm", orientation = "y", level = 0.95) +
    labs(y = i)
}
resids_list

ppc_error_scatter_avg(StormdataTrain5$VMAX, 
                      FitfinalFit)

ppc_error_scatter_avg_vs_x(StormdataTrain5$VMAX, 
                           FitfinalFit,
                           as.numeric(StormdataTrain5[["arcsinhMINSLP"]])) +
  geom_smooth(orientation = "y", level = 0.95) +
  labs(y = "arcsinhMINSLP")



#### Prediction ----
FitPredDF <- bind_cols(
  StormdataTest,
  LCB = FitfinalPredsLCB,
  Mean = FitfinalPredsMean,
  Med = FitfinalPredsMed,
  UCB = FitfinalPredsUCB
) |>
  mutate(
    VMAX = Actual_Yvec
  ) 

FitstormsPredplot <- ggplot(data = FitPredDF, aes(x = StormElapsedTime)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), fill = "lightblue") +
  geom_line(aes(y = VMAX, color = "Observed")) +
  geom_line(aes(y = Mean, color = "PPD Mean")) +
  facet_wrap(vars(StormID))+#, ncol = 6)+
  scale_y_continuous(limits = c(0,275), breaks = seq(0,275,50)) +
  labs(title = "Fit PPD Mean vs Observed VMAX",
       subtitle = "95% Credible Interval about PPD Mean") +
  scale_color_manual(name = NULL, values = c("black","red")) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  theme_bw()
FitstormsPredplot

### PPC ----
#### Home ----
###### Quantile 2.5 
homeLCBsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.025)
                     })
homeLCBpvalueVec <- homeLCBsims < quantile(modelDataTrainNA$home_score, 0.025)
homeLCBpvalue <- sum(homeLCBpvalueVec)
homeLCBpvalue <- round(homeLCBpvalue/(sims), 3)
homeLCBpvalue <- min(homeLCBpvalue, 1 - homeLCBpvalue)

home_ppcLCB <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = paste0("2.5% Quantile (p-val = ", homeLCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcLCB

###### Quantile 97.5 
homeUCBsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.975)
                     })
homeUCBpvalueVec <- homeUCBsims < quantile(modelDataTrainNA$home_score, 0.975)
homeUCBpvalue <- as.numeric(sum(homeUCBpvalueVec))
homeUCBpvalue <- round(homeUCBpvalue/sims, 3)
homeUCBpvalue <- min(homeUCBpvalue, 1 - homeUCBpvalue)

home_ppcUCB <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.975), freq = FALSE) +
  labs(title = paste0("97.5% Quantile (p-val = ", homeUCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcUCB

###### Mean 
homeMEANsims <- apply(homefinalFit, 
                      MARGIN = 1,
                      function(x){
                        mean(x)
                      })
homeMEANpvalueVec <- homeMEANsims < mean(modelDataTrainNA$home_score)
homeMEANpvalue <- sum(homeMEANpvalueVec)
homeMEANpvalue <- round(homeMEANpvalue/sims, 3)
homeMEANpvalue <- min(homeMEANpvalue, 1 - homeMEANpvalue)

home_ppcMEAN <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) mean(y), freq = FALSE) +
  labs(title = paste0("Mean (p-val = ", homeMEANpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcMEAN

###### Med 
homeMEDsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.5)
                     })
homeMEDpvalueVec <- homeMEDsims < quantile(modelDataTrainNA$home_score, 0.5)
homeMEDpvalue <- sum(homeMEDpvalueVec)
homeMEDpvalue <- round(homeMEDpvalue/sims, 3)
homeMEDpvalue <- min(homeMEDpvalue, 1 - homeMEDpvalue)

home_ppcMED <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.5), freq = FALSE) +
  labs(title = paste0("Median (p-val = ", homeMEDpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcMED

###### SD 
homeSDsims <- apply(homefinalFit, 
                    MARGIN = 1,
                    function(x){
                      sd(x)
                    })
homeSDpvalueVec <- homeSDsims < sd(modelDataTrainNA$home_score)
homeSDpvalue <- sum(homeSDpvalueVec)
homeSDpvalue <- round(homeSDpvalue/sims, 3)
homeSDpvalue <- min(homeSDpvalue, 1 - homeSDpvalue)

home_ppcSD <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) sd(y), freq = FALSE) +
  labs(title = paste0("SD (p-val = ", homeSDpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcSD

###### Range 
homeRANGEsims <- apply(homefinalFit, 
                       MARGIN = 1,
                       function(x){
                         max(x)-min(x)
                       })
homeRANGEpvalueVec <- homeRANGEsims < (max(modelDataTrainNA$home_score)-min(modelDataTrainNA$home_score))
homeRANGEpvalue <- sum(homeRANGEpvalueVec)
homeRANGEpvalue <- round(homeRANGEpvalue/sims, 3)
homeRANGEpvalue <- min(homeRANGEpvalue, 1 - homeRANGEpvalue)

home_ppcRANGE <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) max(y)-min(y), freq = FALSE) +
  labs(title = paste0("Range (p-val = ", homeRANGEpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcRANGE

#### Away ----
###### Quantile 2.5 
awayLCBsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.025)
                     })
awayLCBpvalueVec <- awayLCBsims < quantile(modelDataTrainNA$away_score, 0.025)
awayLCBpvalue <- sum(awayLCBpvalueVec)
awayLCBpvalue <- round(awayLCBpvalue/(sims), 3)
awayLCBpvalue <- min(awayLCBpvalue, 1 - awayLCBpvalue)

away_ppcLCB <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = paste0("2.5% Quantile (p-val = ", awayLCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcLCB

###### Quantile 97.5 
awayUCBsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.975)
                     })
awayUCBpvalueVec <- awayUCBsims < quantile(modelDataTrainNA$away_score, 0.975)
awayUCBpvalue <- as.numeric(sum(awayUCBpvalueVec))
awayUCBpvalue <- round(awayUCBpvalue/sims, 3)
awayUCBpvalue <- min(awayUCBpvalue, 1 - awayUCBpvalue)

away_ppcUCB <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.975), freq = FALSE) +
  labs(title = paste0("97.5% Quantile (p-val = ", awayUCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcUCB

###### Mean 
awayMEANsims <- apply(awayfinalFit, 
                      MARGIN = 1,
                      function(x){
                        mean(x)
                      })
awayMEANpvalueVec <- awayMEANsims < mean(modelDataTrainNA$away_score)
awayMEANpvalue <- sum(awayMEANpvalueVec)
awayMEANpvalue <- round(awayMEANpvalue/sims, 3)
awayMEANpvalue <- min(awayMEANpvalue, 1 - awayMEANpvalue)

away_ppcMEAN <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) mean(y), freq = FALSE) +
  labs(title = paste0("Mean (p-val = ", awayMEANpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcMEAN

###### Med 
awayMEDsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.5)
                     })
awayMEDpvalueVec <- awayMEDsims < quantile(modelDataTrainNA$away_score, 0.5)
awayMEDpvalue <- sum(awayMEDpvalueVec)
awayMEDpvalue <- round(awayMEDpvalue/sims, 3)
awayMEDpvalue <- min(awayMEDpvalue, 1 - awayMEDpvalue)

away_ppcMED <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.5), freq = FALSE) +
  labs(title = paste0("Median (p-val = ", awayMEDpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcMED

###### SD 
awaySDsims <- apply(awayfinalFit, 
                    MARGIN = 1,
                    function(x){
                      sd(x)
                    })
awaySDpvalueVec <- awaySDsims < sd(modelDataTrainNA$away_score)
awaySDpvalue <- sum(awaySDpvalueVec)
awaySDpvalue <- round(awaySDpvalue/sims, 3)
awaySDpvalue <- min(awaySDpvalue, 1 - awaySDpvalue)

away_ppcSD <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) sd(y), freq = FALSE) +
  labs(title = paste0("SD (p-val = ", awaySDpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcSD

###### Range 
awayRANGEsims <- apply(awayfinalFit, 
                       MARGIN = 1,
                       function(x){
                         max(x)-min(x)
                       })
awayRANGEpvalueVec <- awayRANGEsims < (max(modelDataTrainNA$away_score)-min(modelDataTrainNA$away_score))
awayRANGEpvalue <- sum(awayRANGEpvalueVec)
awayRANGEpvalue <- round(awayRANGEpvalue/sims, 3)
awayRANGEpvalue <- min(awayRANGEpvalue, 1 - awayRANGEpvalue)

away_ppcRANGE <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) max(y)-min(y), freq = FALSE) +
  labs(title = paste0("Range (p-val = ", awayRANGEpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcRANGE

### Bayes p-values ----
homeFitpvalues <- tibble(
  Fit = paste0("Fit", fit),
  LCB = homeLCBpvalue,
  Median = homeMEDpvalue,
  UCB = homeUCBpvalue,
  Range = homeRANGEpvalue,
  Mean = homeMEANpvalue,
  SD = homeSDpvalue
)
homeFitpvalues

awayFitpvalues <- tibble(
  Fit = paste0("Fit", fit),
  LCB = awayLCBpvalue,
  Median = awayMEDpvalue,
  UCB = awayUCBpvalue,
  Range = awayRANGEpvalue,
  Mean = awayMEANpvalue,
  SD = awaySDpvalue
)
awayFitpvalues

### Combined Plot ----
home_ppcComb <- 
  homePPC /
  (home_ppcLCB | home_ppcMED | home_ppcUCB) /
  (home_ppcRANGE | home_ppcMEAN | home_ppcSD)
home_ppcComb

away_ppcComb <- 
  awayPPC /
  (away_ppcLCB | away_ppcMED | away_ppcUCB) /
  (away_ppcRANGE | away_ppcMEAN | away_ppcSD)
away_ppcComb

### CV ----
set.seed(52)
kfoldID <- kfold_split_grouped(K = 5, StormdataTrain$StormID)
Fitkfoldgroup <- kfold(Fit,
                       folds = kfoldID,
                       chains = 1,
                       save_fits = TRUE)
save(Fitkfoldgroup,
     file = "~/Desktop/Temp Hurricane Model Data/Fit12kfold.RData")
FitkfoldPreds <- kfold_predict(Fitkfoldgroup)
FitkfoldPredsDat <- FitkfoldPreds$yrep
FitkfoldPredsMean <- colMeans(FitkfoldPredsDat)
FitkfoldPredsMed <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.5)})
FitkfoldPredsLCB <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.025)})
FitkfoldPredsUCB <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.975)})

FitkfoldMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_kfold = mean(abs(FitkfoldPredsMean - FitkfoldPreds$y)),
  MAD_kfold = mean(abs(FitkfoldPredsMed - FitkfoldPreds$y)),
  COV_kfold = mean(FitkfoldPredsLCB < FitkfoldPreds$y & FitkfoldPreds$y < FitkfoldPredsUCB)
)
FitkfoldMetrics
