## Model home and away score

# Load Libraries ----
## Data Manipulation
library(stringr)

## Tables
library(DBI)
library(RPostgres)
library(data.table)

## Plotting
library(smplot2)
library(patchwork)

## Modeling
library(zoo)
library(pracma)
library(forecast)
library(timetk)
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
library(cmdstanr)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse
library(nflverse)

## Tidyverse
library(tidyverse)

# Takes about 10 seconds
system.time(
  source("./app/data-raw/modelData2.R")
)

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2021:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

# Previous Data ----
modData2 <- modData
modData2 <- modData2 |> 
  filter(!is.na(result)) |>
  select(
    season,
    season_type,
    week,
    home_team,
    home_score,
    away_team,
    away_score,
    result,
    total,
    location,
    div_game,
    roof,
    surface,
    temp,
    wind,
    contains("home"),
    contains("away")
  ) |>
  mutate(
    across(where(is.character),
           ~factor(.x))
  )
# [1] "game_id"                          "season"                           "game_type"                       
# [4] "season_type"                      "week"                             "gameday"                         
# [7] "weekday"                          "gametime"                         "time_of_day"                     
# [10] "away_team"                        "away_score"                       "home_team"                       
# [13] "home_score"                       "location"                         "result"                          
# [16] "winner"                           "total"                            "overtime"                        
# [19] "away_rest"                        "home_rest"                        "away_moneyline"                  
# [22] "away_moneyline_prob"              "home_moneyline"                   "home_moneyline_prob"             
# [25] "spread_line"                      "spreadCover"                      "away_spread_odds"                
# [28] "away_spread_prob"                 "home_spread_odds"                 "home_spread_prob"                
# [31] "total_line"                       "totalCover"                       "under_odds"                      
# [34] "under_prob"                       "over_odds"                        "over_prob"                       
# [37] "div_game"                         "roof"                             "surface"                         
# [40] "temp"                             "wind"                             "away_coach"                      
# [43] "home_coach"                       "stadium"                          "home_off_epa_mean_cum"           
# [46] "home_off_pass_epa_mean_cum"       "home_off_rush_epa_mean_cum"       "home_off_penalty_epa_mean_cum"   
# [49] "home_off_special_epa_mean_cum"    "home_def_epa_mean_cum"            "home_def_pass_epa_mean_cum"      
# [52] "home_def_rush_epa_mean_cum"       "home_def_penalty_epa_mean_cum"    "home_def_special_epa_mean_cum"   
# [55] "home_off_epa_mean_roll_4"         "home_off_pass_epa_mean_roll_4"    "home_off_rush_epa_mean_roll_4"   
# [58] "home_off_penalty_epa_mean_roll_4" "home_off_special_epa_mean_roll_4" "home_def_epa_mean_roll_4"        
# [61] "home_def_pass_epa_mean_roll_4"    "home_def_rush_epa_mean_roll_4"    "home_def_penalty_epa_mean_roll_4"
# [64] "home_def_special_epa_mean_roll_4" "home_PFG"                         "home_PAG"                        
# [67] "home_MOV"                         "home_SOS"                         "home_SRS"                        
# [70] "home_OSRS"                        "home_DSRS"                        "away_off_epa_mean_cum"           
# [73] "away_off_pass_epa_mean_cum"       "away_off_rush_epa_mean_cum"       "away_off_penalty_epa_mean_cum"   
# [76] "away_off_special_epa_mean_cum"    "away_def_epa_mean_cum"            "away_def_pass_epa_mean_cum"      
# [79] "away_def_rush_epa_mean_cum"       "away_def_penalty_epa_mean_cum"    "away_def_special_epa_mean_cum"   
# [82] "away_off_epa_mean_roll_4"         "away_off_pass_epa_mean_roll_4"    "away_off_rush_epa_mean_roll_4"   
# [85] "away_off_penalty_epa_mean_roll_4" "away_off_special_epa_mean_roll_4" "away_def_epa_mean_roll_4"        
# [88] "away_def_pass_epa_mean_roll_4"    "away_def_rush_epa_mean_roll_4"    "away_def_penalty_epa_mean_roll_4"
# [91] "away_def_special_epa_mean_roll_4" "away_PFG"                         "away_PAG"                        
# [94] "away_MOV"                         "away_SOS"                         "away_SRS"                        
# [97] "away_OSRS"                        "away_DSRS"                        "home_epa_cum"                    
# [100] "home_epa_roll"                    "home_pass_epa_cum"                "home_pass_epa_roll"              
# [103] "home_rush_epa_cum"                "home_rush_epa_roll"               "home_penalty_epa_cum"            
# [106] "home_penalty_epa_roll"            "away_epa_cum"                     "away_epa_roll"                   
# [109] "away_pass_epa_cum"                "away_pass_epa_roll"               "away_rush_epa_cum"               
# [112] "away_rush_epa_roll"               "away_penalty_epa_cum"             "away_penalty_epa_roll" 


# Pre-Processing -------
## Near Zero Variance ------
modelData <- histModelData |> select(-home_score, -away_score)
nzv <- nearZeroVar(modelData, saveMetrics = TRUE)
nzv
nzv[nzv$nzv,][1:10,]

modelData <- modelData |> select(-contains("special"))


## Correlated Params -----
modelDataNum <- modelData |> select(where(is.numeric))

descrCor <-  cor(modelDataNum)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highCorr

descrCor <- cor(modelDataNum)
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
highlyCorDescr
modelDataNum2 <- modelDataNum[,-highlyCorDescr]
descrCor2 <- cor(modelDataNum2)
summary(descrCor2[upper.tri(descrCor2)])

## Create time slices ----





