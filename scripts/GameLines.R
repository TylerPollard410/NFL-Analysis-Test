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

# Set WD
setwd("app")

# Load data ----
##  NFL Team Graphics, Colors, and Logos
teamsData <- load_teams(current = FALSE)

##  Game/Schedule Data
gameData <- load_schedules(seasons = TRUE) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  ) 

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) 


### By Season ----
gameSeasons <- 2002:2023
seasonStandings <- data.frame()
seasonStandingsConvergence <- data.frame()

tic()
for(i in gameSeasons){
  gameDataTemp <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG") |>
    filter(complete.cases(result))
  
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
    #mutate(SRS = MOV, .after = SOS) |>
    mutate(OSRS = team_PPG - leaguePPGTemp) |>
    mutate(DSRS = leaguePPGTemp - opp_PPG) |>
    mutate(SRS = OSRS + DSRS, .after = SOS)
  #mutate(DSRS = SRS - OSRS)
  
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
  nflverse_object = gameData |> 
    filter(season %in% gameSeasons) |>
    filter(!is.na(result)),
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

seasonStandings |> 
  group_by(season) |> 
  summarise(across(c(MOV, SOS, SRS, OSRS, DSRS), ~mean(.x))) |> 
  print(n = 22)

save(
  seasonStandings,
  file = "./_data/seasonStandings.RData"
)

load(file = "./_data/seasonStandings.RData")

### By Season and Week ----
#### No prior ----
seasonWeekStandings <- data.frame()
seasonWeekStandingsConvergence <- data.frame()

gameWeekSeason <- 2003:2024

tic()
for(i in gameWeekSeason){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG") |>
    filter(complete.cases(result))
  seasonWeeks <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeeks){
    gameDataTemp <- gameDataSeason |>
      filter(week %in% 1:j)
    
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

seasonWeekStandings |> 
  group_by(season, week) |> 
  summarise(across(c(MOV, SOS, SRS, OSRS, DSRS), ~mean(.x))) |> 
  print(n = 22)

seasonWeekStandings |> 
  group_by(season) |> 
  summarise(across(c(MOV, SOS, SRS, OSRS, DSRS), ~mean(.x))) |> 
  print(n = 22)

seasonWeekStandingsConvergence |>
  group_by(week) |>
  summarise(iters = mean(Converge)) |>
  print(n = 22)

save(seasonWeekStandings, file = "./_data/seasonWeekStandings.RData")
save(seasonWeekStandingsConvergence, file = "./_data/seasonWeekStandingsConvergence.RData")
load(file = "./_data/seasonWeekStandings.RData")

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

# seasonStandingsComb <- bind_rows(
#   standing2002,
#   seasonStandings
# ) |>
#   mutate(week = 1, .after = season) |>
#   mutate(season = season + 1) |>
#   filter(season <= 2023)

seasonStandingsInit <- seasonStandings |>
  mutate(week = 1, .after = season) |>
  mutate(season = season + 1) #|>
#filter(season <= 2023)

seasonWeekStandingsMerge2 <- seasonWeekStandingsMerge


seasonWeekGameData1 <- gameData |>
  left_join(seasonStandingsInit |>
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
                win_loss_percent = "W-L%",
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("home_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "home_team" == "team")) |>
  left_join(seasonStandingsInit |>
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
                win_loss_percent = "W-L%",
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
  filter(season > 2002) |>
  arrange(season, week, gsis)

###### No Home Field Adv ----
seasonWeekGameDataNoHF <- seasonWeekGameData |>
  mutate(
    SRS_spread = home_SRS - away_SRS,
    SRS_spread_diff = spread_line - SRS_spread
  ) |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

ggplot(data = seasonWeekGameDataNoHF, 
       aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  facet_wrap(vars(season))

seasonWeekNoHF_SLvsSRSplot <- ggplot(data = seasonWeekGameDataNoHF, 
       aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior No Home Field Adv",
       subtitle = "spread_line vs SRS_spread")
seasonWeekNoHF_SLvsSRSplot

seasonWeekNoHF_RESvsSRSplot <- ggplot(data = seasonWeekGameDataNoHF, 
                                     aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior No Home Field Adv",
       subtitle = "result vs SRS_spread")
seasonWeekNoHF_RESvsSRSplot

ggplot(data = seasonWeekGameDataNoHF) +
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

HFadv <- seasonWeekGameDataNoHF |>
  group_by(season) |>
  summarise(
    HFavg  = mean(SRS_spread_diff, na.rm = TRUE)
  ) |>
  ungroup()
HFadv
acf(HFadv$HFavg, type = "correlation")
acf(HFadv$HFavg, type = "covariance")
acf(HFadv$HFavg, type = "partial")
PlotACF(HFadv$HFavg)
tseries::adf.test(HFadv$HFavg)
PlotACF(diff(HFadv$HFavg))
HFadv$HFavg
MoveAvg(HFadv$HFavg, order = 2, align = "right")
movavg(HFadv$HFavg, n = 2, type = "s")
auto.arima(HFadv$HFavg, trace = TRUE)

HFfit <- Arima(HFadv$HFavg, order = c(0,1,1), 
               include.mean = TRUE,
               include.drift = TRUE
               )
HFfit
HFfitted <- fitted(HFfit)
HFfitted
movavg(HFadv$HFavg, 
       n = 2, 
       type = "r" #type=c("s", "t", "w", "m", "e", "r")
)


###### Home Field Adv ----
newHF <- as.numeric(HFfitted)
HFadv <- HFadv |>
  mutate(
    season = as.numeric(as.character(season)),
    HFfit = newHF
  )

seasonWeekGameDataHF <- seasonWeekGameData |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  left_join(HFadv, by = join_by(season)) |>
  mutate(
    SRS_spread = home_SRS - away_SRS + HFfit,
    SRS_spread_diff = SRS_spread - spread_line
  ) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

seasonWeekHF_SLvsSRSplot <- ggplot(data = seasonWeekGameDataHF, 
                                     aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior Home Field Adv Fit",
       subtitle = "spread_line vs SRS_spread")
seasonWeekHF_SLvsSRSplot

seasonWeekHF_RESvsSRSplot <- ggplot(data = seasonWeekGameDataHF, 
                                      aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior Home Field Adv Fit",
       subtitle = "result vs SRS_spread")
seasonWeekHF_RESvsSRSplot

seasonWeekGameDataHF |>
  group_by(season) |>
  summarise(
    mean(SRS_spread_diff, na.rm = TRUE)
  ) |>
  print(n = length(gameWeekSeason))

ggplot(data = seasonWeekGameDataNoHF) +
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

###### HomeField 2 Pt ----
seasonWeekGameDataHF2 <- seasonWeekGameData |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  left_join(HFadv, by = join_by(season)) |>
  mutate(
    SRS_spread = home_SRS - away_SRS + 2,
    SRS_spread_diff = SRS_spread - spread_line
  ) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

seasonWeekHF2_SLvsSRSplot <- ggplot(data = seasonWeekGameDataHF2, 
                                   aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior Home Field Adv +2",
       subtitle = "spread_line vs SRS_spread")
seasonWeekHF2_SLvsSRSplot

seasonWeekHF2_RESvsSRSplot <- ggplot(data = seasonWeekGameDataHF2, 
                                    aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "No Prior Home Field Adv +2",
       subtitle = "result vs SRS_spread")
seasonWeekHF2_RESvsSRSplot

#### Prior ----
##### 4/5 Game ----
seasonWeekPriorStandings <- data.frame()
seasonWeekPriorStandingsConvergence <- data.frame()

gameWeekSeasonPrior <- 2003:2024

tic()
for(i in gameWeekSeasonPrior){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG") |>
    filter(complete.cases(result))
  seasonWeekPriors <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeekPriors){
    gameDataTemp <- gameDataSeason |>
      filter(week %in% 1:j)
    
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
    
    leaguePPGTemp <- (sum(standingTemp$team_score) + sum(standingTemp$opponent_score))/sum(standingTemp$games_played)/2
    
    standingTemp <- standingTemp |>
      mutate(SOS = 0, .after = MOV) |>
      mutate(OSRS = team_PPG - leaguePPGTemp) |>
      mutate(DSRS = leaguePPGTemp - opp_PPG) |>
      mutate(SRS = OSRS + DSRS, .after = SOS)
    
    if(j <= 4){
      previous_ratings <- seasonStandings |>
        filter(season == (i - 1)) |>
        select(
          team,
          prevSRS = SRS, 
          prevOSRS = OSRS, 
          prevDSRS = DSRS
        )
      current_weight <- j/5
      prev_weight <- 1 - j/5
      
      standingTemp <- standingTemp |>
        left_join(previous_ratings, by = join_by(team))
      
      standingTemp <- standingTemp |>
        mutate(
          OSRS = (prevOSRS*prev_weight) + (OSRS*current_weight),
          DSRS = (prevDSRS*prev_weight) + (DSRS*current_weight)
        ) |>
        mutate(
          SRS = OSRS + DSRS, .after = SOS
        ) |>
        select(-contains("prev"))
    }
    
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
              newSOS = SRS,
              newSRS = result + newSOS,
              newOSOS = DSRS,
              newOSRS = team_score + newOSOS - mean(team_score)
            ) |>
            group_by(team) |>
            summarise(
              newSOS = mean(newSOS, na.rm = TRUE),
              newSRS = mean(newSRS, na.rm = TRUE),
              newOSRS = mean(newOSRS)
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
save(seasonWeekPriorStandingsConvergence, file = "./_data/seasonWeekPriorStandingsConvergence.RData")
save(seasonWeekPriorStandings, file = "./_data/seasonWeekPriorStandings.RData")


##### 4 two Game ----
seasonWeekPriorStandings <- data.frame()
seasonWeekPriorStandingsConvergence <- data.frame()

gameWeekSeasonPrior <- 2003:2024

tic()
for(i in gameWeekSeasonPrior){
  gameDataSeason <- gameData |>
    filter(season == i) |>
    filter(game_type == "REG") |>
    filter(complete.cases(result))
  seasonWeekPriors <- max(gameDataSeason$week)
  
  for(j in 1:seasonWeekPriors){
    gameDataTemp <- gameDataSeason |>
      filter(week %in% 1:j)
    
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
    
    leaguePPGTemp <- (sum(standingTemp$team_score) + sum(standingTemp$opponent_score))/sum(standingTemp$games_played)/2
    
    standingTemp <- standingTemp |>
      mutate(SOS = 0, .after = MOV) |>
      mutate(OSRS = team_PPG - leaguePPGTemp) |>
      mutate(DSRS = leaguePPGTemp - opp_PPG) |>
      mutate(SRS = OSRS + DSRS, .after = SOS)
    
    if(j <= 4){
      previous_ratings <- seasonStandings |>
        filter(season == (i - 1)) |>
        select(
          team,
          prevSRS = SRS, 
          prevOSRS = OSRS, 
          prevDSRS = DSRS
        )
      prev_weight <- 0.85 - j*0.05
      current_weight <- 1 - prev_weight
      
      standingTemp <- standingTemp |>
        left_join(previous_ratings, by = join_by(team))
      
      standingTemp <- standingTemp |>
        mutate(
          OSRS = (prevOSRS*prev_weight) + (OSRS*current_weight),
          DSRS = (prevDSRS*prev_weight) + (DSRS*current_weight)
        ) |>
        mutate(
          SRS = OSRS + DSRS, .after = SOS
        ) |>
        select(-contains("prev"))
    }
    
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
              newSOS = SRS,
              newSRS = result + newSOS,
              newOSOS = DSRS,
              newOSRS = team_score + newOSOS - mean(team_score)
            ) |>
            group_by(team) |>
            summarise(
              newSOS = mean(newSOS, na.rm = TRUE),
              newSRS = mean(newSRS, na.rm = TRUE),
              newOSRS = mean(newOSRS)
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
save(seasonWeekPriorStandingsConvergence, file = "./_data/seasonWeekPriorStandingsConvergence.RData")
save(seasonWeekPriorStandings, file = "./_data/seasonWeekPriorStandings.RData")


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

# seasonStandingsComb <- bind_rows(
#   standing2002,
#   seasonStandings
# ) |>
#   mutate(week = 1, .after = season) |>
#   mutate(season = season + 1) |>
#   filter(season <= 2023)

seasonStandingsInit <- seasonStandings |>
  mutate(week = 1, .after = season) |>
  mutate(season = season + 1) #|>
#filter(season <= 2023)

seasonWeekPriorStandingsMerge2 <- seasonWeekPriorStandingsMerge


seasonWeekPriorGameData1 <- gameData |>
  left_join(seasonStandingsInit |>
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
                win_loss_percent = "W-L%",
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("home_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "home_team" == "team")) |>
  left_join(seasonStandingsInit |>
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
                win_loss_percent = "W-L%",
                MOV,
                SOS,
                SRS,
                OSRS,
                DSRS
              ) |>
              rename_with(~paste0("away_", .x), .cols = c(-season,-week, -team)),
            by = join_by(season, week, "away_team" == "team")) |>
  filter(week == 1)

seasonWeekPriorGameData2 <- gameData |>
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
            by = join_by(season, week, "away_team" == "team")) |>
  filter(week != 1)

seasonWeekPriorGameData <- bind_rows(
  seasonWeekPriorGameData1,
  seasonWeekPriorGameData2
) |>
  filter(season > 2002) |>
  arrange(season, week, gsis)

###### No Home Field Adv ----
seasonWeekPriorGameDataNoHF <- seasonWeekPriorGameData |>
  mutate(
    SRS_spread = home_SRS - away_SRS,
    SRS_spread_diff = spread_line - SRS_spread
  ) |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

ggplot(data = seasonWeekPriorGameDataNoHF, 
       aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  facet_wrap(vars(season))

seasonWeekPriorGameDataNoHF |> 
  filter(is.na(SRS_spread)) |>
  view()

seasonWeekPriorNoHF_SLvsSRSplot <- ggplot(data = seasonWeekPriorGameDataNoHF, 
                                   aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior No Home Field Adv",
       subtitle = "spread_line vs SRS_spread")
seasonWeekPriorNoHF_SLvsSRSplot

seasonWeekPriorNoHF_RESvsSRSplot <- ggplot(data = seasonWeekPriorGameDataNoHF, 
                                    aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior No Home Field Adv",
       subtitle = "result vs SRS_spread")
seasonWeekPriorNoHF_RESvsSRSplot

ggplot(data = seasonWeekPriorGameDataNoHF) +
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

HFadvPrior <- seasonWeekPriorGameDataNoHF |>
  group_by(season) |>
  summarise(
    HFavg  = mean(SRS_spread_diff, na.rm = TRUE)
  ) |>
  ungroup()
HFadvPrior
acf(HFadvPrior$HFavg, type = "correlation")
acf(HFadvPrior$HFavg, type = "covariance")
acf(HFadvPrior$HFavg, type = "partial")
PlotACF(HFadvPrior$HFavg)
tseries::adf.test(HFadvPrior$HFavg)
PlotACF(diff(HFadvPrior$HFavg))
HFadvPrior$HFavg
MoveAvg(HFadvPrior$HFavg, order = 2, align = "right")
movavg(HFadvPrior$HFavg, n = 2, type = "s")
auto.arima(HFadvPrior$HFavg, trace = TRUE)

HFfitPrior <- Arima(HFadvPrior$HFavg, 
                    order = c(0,1,0)
                    )
HFfitPrior
HFfittedPrior <- fitted(HFfitPrior)
HFfittedPrior
movavg(HFadvPrior$HFavg, 
       n = 2, 
       type = "r" #type=c("s", "t", "w", "m", "e", "r")
)


###### Home Field Adv ----
newHFPrior <- as.numeric(HFfittedPrior)
HFadvPrior <- HFadvPrior |>
  mutate(
    season = as.numeric(as.character(season)),
    HFfit = newHFPrior
  )
HFadvPrior
seasonWeekPriorGameDataHF <- seasonWeekPriorGameData |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  left_join(HFadvPrior, by = join_by(season)) |>
  mutate(
    SRS_spread = home_SRS - away_SRS + HFfit,
    SRS_spread_diff = SRS_spread - spread_line
  ) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

ggplot(data = seasonWeekPriorGameDataHF, 
       aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  facet_wrap(vars(season))

seasonWeekPriorHF_SLvsSRSplot <- ggplot(data = seasonWeekPriorGameDataHF, 
                                          aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior Home Field Adv Fit",
       subtitle = "spread_line vs SRS_spread")
seasonWeekPriorHF_SLvsSRSplot

seasonWeekPriorHF_RESvsSRSplot <- ggplot(data = seasonWeekPriorGameDataHF, 
                                           aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior Home Field Adv Fit",
       subtitle = "result vs SRS_spread")
seasonWeekPriorHF_RESvsSRSplot

seasonWeekPriorGameDataHF |>
  group_by(season) |>
  summarise(
    mean(SRS_spread_diff, na.rm = TRUE)
  ) |>
  print(n = length(gameWeekSeason))

ggplot(data = seasonWeekPriorGameDataNoHF) +
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

###### HomeField 2 Pt ----
seasonWeekPriorGameDataHF2 <- seasonWeekPriorGameData |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  left_join(HFadv, by = join_by(season)) |>
  mutate(
    SRS_spread = home_SRS - away_SRS + 2,
    SRS_spread_diff = SRS_spread - spread_line
  ) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE)
  )

seasonWeekPriorHF2_SLvsSRSplot <- ggplot(data = seasonWeekPriorGameDataHF2, 
                                        aes(x = SRS_spread, y = spread_line, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior Home Field Adv +2",
       subtitle = "spread_line vs SRS_spread")
seasonWeekPriorHF2_SLvsSRSplot

seasonWeekPriorHF2_RESvsSRSplot <- ggplot(data = seasonWeekPriorGameDataHF2, 
                                         aes(x = SRS_spread, y = result, color = week, group = week)) +
  geom_point() +
  sm_statCorr(legends = TRUE) +
  theme_bw() +
  labs(title = "Yes Prior Home Field Adv +2",
       subtitle = "result vs SRS_spread")
seasonWeekPriorHF2_RESvsSRSplot

## Compare Plots ----
(seasonWeekNoHF_SLvsSRSplot | 
  seasonWeekHF_SLvsSRSplot |
  seasonWeekHF2_SLvsSRSplot) +
  plot_layout(
    guides = "collect"
  )

(seasonWeekPriorNoHF_SLvsSRSplot | 
    seasonWeekPriorHF_SLvsSRSplot |
    seasonWeekPriorHF2_SLvsSRSplot) +
  plot_layout(
    guides = "collect"
  )

(seasonWeekNoHF_RESvsSRSplot | 
    seasonWeekHF_RESvsSRSplot |
    seasonWeekHF2_RESvsSRSplot) +
  plot_layout(
    guides = "collect"
  )

(seasonWeekPriorNoHF_RESvsSRSplot | 
    seasonWeekPriorHF_RESvsSRSplot |
    seasonWeekPriorHF2_RESvsSRSplot) +
  plot_layout(
    guides = "collect"
  )


# Compare Prediction Perf ----
## Get base total line ----
totalAvg <- gameData |>
  filter(game_type == "REG") |>
  filter(!is.na(result)) |>
  select(
    season,
    week,
    home_team,
    home_score,
    away_team,
    away_score,
    total,
    total_line
  )

totalAvgSum <- totalAvg |>
  group_by(season, week) |>
  summarise(
    home_score_avg = mean(home_score, na.rm = TRUE),
    away_score_avg = mean(away_score, na.rm = TRUE),
    total_avg = mean(total, na.rm = TRUE),
    total_line_avg = mean(total_line, na.rm = TRUE)
  )

totalSeasonWeekAvg <- totalAvgSum$total_avg

PlotACF(totalSeasonWeekAvg)
tseries::adf.test(totalSeasonWeekAvg)
PlotACF(diff(totalSeasonWeekAvg))
# MoveAvg(totalSeasonWeekAvg, order = 2, align = "right")
# movavg(totalSeasonWeekAvg, n = 2, type = "s")
auto.arima(totalSeasonWeekAvg, 
           seasonal = FALSE,
           trace = TRUE,
           test = "adf")
auto.arima(totalSeasonWeekAvg,
           seasonal = TRUE,
           trace = TRUE,
           test = "adf")

totalSeasonWeekAvgfit <- Arima(totalSeasonWeekAvg, order = c(1,1,1))
totalSeasonWeekAvgfit
plot(totalSeasonWeekAvgfit)
totalSeasonWeekAvgfitted <- fitted(totalSeasonWeekAvgfit)
totalSeasonWeekMovAvg <- movavg(totalSeasonWeekAvg, n = 7, type = "s")
plot(totalSeasonWeekAvg, type = "l")
lines(totalAvgSum$total_line_avg, col = "red")
lines(totalSeasonWeekAvgfitted, col = "green")
lines(totalSeasonWeekMovAvg, col = "blue")
legend(x = 0, y = 60, 
       legend = c("Average Total", 
                  "Average Total Line", 
                  "ARIMA(1,1,1)", 
                  "Moving Avg"),
       col = c("black", "red", "green", "blue"))

totalAvgSum <- totalAvgSum |>
  ungroup() |>
  mutate(
    total_fitted = as.numeric(totalSeasonWeekAvgfitted)
  )

# totalAvgSum2 <- totalAvg |>
#   group_by(season) |>
#   summarise(
#     home_score_avg = mean(home_score, na.rm = TRUE),
#     away_score_avg = mean(away_score, na.rm = TRUE),
#     total_avg = mean(total, na.rm = TRUE),
#     total_line_avg = mean(total_line, na.rm = TRUE)
#   )
# 
# totalSeasonWeekAvg2 <- totalAvgSum2$total_avg
# 
# PlotACF(totalSeasonWeekAvg2)
# tseries::adf.test(totalSeasonWeekAvg2)
# PlotACF(diff(totalSeasonWeekAvg2))
# # MoveAvg(totalSeasonWeekAvg, order = 2, align = "right")
# # movavg(totalSeasonWeekAvg, n = 2, type = "s")
# auto.arima(totalSeasonWeekAvg2, 
#            seasonal = FALSE,
#            trace = TRUE,
#            test = "adf")
# auto.arima(totalSeasonWeekAvg2,
#            seasonal = TRUE,
#            trace = TRUE,
#            test = "adf")
# 
# totalSeasonWeekAvgfit2 <- Arima(totalSeasonWeekAvg2, order = c(1,1,1))
# totalSeasonWeekAvgfit2
# plot(totalSeasonWeekAvgfit2)
# totalSeasonWeekAvgfitted2 <- fitted(totalSeasonWeekAvgfit2)
# plot(totalSeasonWeekAvg2, type = "l")
# lines(totalSeasonWeekAvgfitted2, col = "red")

## Calc Perf ----
calculatePredPerf <- function(data, sigSpread = 5, sigOU = 0){
  if(!is.numeric(data$season)){
    data <- data |>
      mutate(
        season = as.numeric(as.character(season))
      )
  }
  if(!is.numeric(data$week)){
    data <- data |>
      mutate(
        week = as.numeric(as.character(week))
      )
  }
  
  pred_data <- data |>
    left_join(totalAvgSum |>
                select(season, week, total_fitted), 
              by = join_by(season, week)) |>
    mutate(
      SRS_total = (home_OSRS + away_OSRS) - (home_DSRS + away_DSRS) + total_fitted
    ) |>
    mutate(
      spread_bet = ifelse(SRS_spread > spread_line, home_team, away_team),
      spread_result = ifelse(result > spread_line, home_team, 
                             ifelse(result < spread_line, away_team, NA)),
      moneyline_bet = ifelse(SRS_spread > 0, home_team, away_team),
      moneyline_result = ifelse(result > 0, home_team, 
                                ifelse(result < 0, away_team, NA)),
      total_bet = ifelse(SRS_total > total_line, "Over", "Under"),
      total_result = ifelse(total > total_line, "Over", 
                             ifelse(result < total_line, "Under", NA))
    ) |>
    mutate(
      #sig_spread_diff = sigSpread,
      #sig_total_diff = sigOU,
      spread_result = ifelse(abs(SRS_spread_diff) > sigSpread, spread_result, NA),
      total_result = ifelse(abs(SRS_total - total_line) > sigOU, total_result, NA)
    ) |>
    mutate(
      spread_win = ifelse(is.na(spread_result), NA,
                          ifelse(spread_bet == spread_result, 1, 0)),
      moneyline_win = ifelse(is.na(moneyline_result), NA,
                             ifelse(moneyline_bet == moneyline_result, 1, 0)),
      total_win = ifelse(is.na(total_result), NA,
                          ifelse(total_bet == total_result, 1, 0))
    )
  return(pred_data)
}

seasonWeekNoHFpreds <- calculatePredPerf(seasonWeekGameDataNoHF)
seasonWeekHFpreds <- calculatePredPerf(seasonWeekGameDataHF)
seasonWeekHF2preds <- calculatePredPerf(seasonWeekGameDataHF2)
seasonWeekPriorNoHFpreds <- calculatePredPerf(seasonWeekPriorGameDataNoHF)
seasonWeekPriorHFpreds <- calculatePredPerf(seasonWeekPriorGameDataHF)
seasonWeekPriorHF2preds <- calculatePredPerf(seasonWeekPriorGameDataHF2)

## Summarize Perf ----
sumPredPerf <- function(data, group_vars = NULL){
  sumData <- data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      spreadWins = sum(spread_win == 1, na.rm = TRUE),
      spreadLoss = sum(spread_win == 0, na.rm = TRUE),
      spreadPush = sum(is.na(spread_win)),
      spreadBets = spreadWins + spreadLoss,
      "spreadWin%" = mean(spread_win, na.rm = TRUE),
      moneylineWins = sum(moneyline_win == 1, na.rm = TRUE),
      moneylineLoss = sum(moneyline_win == 0, na.rm = TRUE),
      moneylinePush = sum(is.na(moneyline_win)),
      "moneylineWin%" = mean(moneyline_win, na.rm = TRUE),
      totalWins = sum(total_win == 1, na.rm = TRUE),
      totalLoss = sum(total_win == 0, na.rm = TRUE),
      totalPush = sum(is.na(total_win)),
      totalBets = totalWins + totalLoss,
      "totalWin%" = mean(total_win, na.rm = TRUE)
    )
  return(sumData)
}


grouping <- NULL #c("season")
### Season Week No HF ----
seasonWeekNoHFpredsSum <- sumPredPerf(
  data = seasonWeekNoHFpreds,
  group_vars = grouping
  )

### Season Week HF ----
seasonWeekHFpredsSum <- sumPredPerf(
  data = seasonWeekHFpreds,
  group_vars = grouping
)

### Season Week HF2 ----
seasonWeekHF2predsSum <- sumPredPerf(
  data = seasonWeekHF2preds,
  group_vars = grouping
)

### Season Week Prior No HF ----
seasonWeekPriorNoHFpredsSum <- sumPredPerf(
  data = seasonWeekPriorNoHFpreds,
  group_vars = grouping
)

### Season Week Prior HF ----
seasonWeekPriorHFpredsSum <- sumPredPerf(
  data = seasonWeekPriorHFpreds,
  group_vars = grouping
)

### Season Week Prior HF2 ----
seasonWeekPriorHF2predsSum <- sumPredPerf(
  data = seasonWeekPriorHF2preds,
  group_vars = grouping
)

seasonWeekperfSumTable <- bind_rows(
  seasonWeekNoHFpredsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |> 
    mutate(Prior = "No", HF = "No"),
  seasonWeekHFpredsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |>
    mutate(Prior = "No", HF = "Yes"),
  seasonWeekHF2predsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |>
    mutate(Prior = "No", HF = "Yes2"),
  seasonWeekPriorNoHFpredsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |> 
    mutate(Prior = "Yes", HF = "No"),
  seasonWeekPriorHFpredsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |>
    mutate(Prior = "Yes", HF = "Yes"),
  seasonWeekPriorHF2predsSum |> 
    select(all_of(grouping), contains("%"), contains("Bets")) |>
    mutate(Prior = "Yes", HF = "Yes2")
)
seasonWeekperfSumTable


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


