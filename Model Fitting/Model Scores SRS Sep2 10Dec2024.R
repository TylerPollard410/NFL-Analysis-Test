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

# Takes about 
system.time(
  source("./app/data-raw/modelData2.R")
)

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2022:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

# Previous Data ----
modDataOG <- modData
modData2 <- modData |> 
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
    spread_line,
    total_line,
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
  ) #|>
modData3 <- modData[complete.cases(modData), ]
modData2 <- modData2[complete.cases(modData2), ]
#filter(!(season == 2024 & week > 14))
histModelData1 <- modData2 |> 
  filter(season %in% 2023:2023)
modelData1 <- modData2 |> 
  filter(season == 2024) |> 
  filter(complete.cases(result)) 

preProcValues <- preProcess(histModelData1 |> 
                              select(-home_score, -away_score,
                                     -result, -total,
                                     -season, -week,
                                     -contains("totalTD"), 
                                     -contains("fg_made"), 
                                     #-contains("fg_att"),
                                     -contains("twoPtConv"), 
                                     -contains("twoPtAtt"),
                                     -contains("safeties"),
                                     -contains("pat_made")
                                     #-contains("pat_att")
                                     #-spread_line, -total_line
                              ),
                            method = c("center", "scale"))
histModelData2 <- predict(preProcValues, histModelData1)
modelData2 <- predict(preProcValues, modelData1)

histModelData <- histModelData2
modelData <- modelData2

home_totalTD_range <- range(modData |> filter(!is.na(home_totalTD)) |> pull(home_totalTD))
home_fg_att_range<- range(modData |> filter(!is.na(home_fg_att)) |> pull(home_fg_att))
home_fg_made_range <- range(modData |> filter(!is.na(home_fg_made)) |> pull(home_fg_made))

away_totalTD_range <- range(modData |> filter(!is.na(away_totalTD)) |> pull(away_totalTD))
away_fg_att_range <- range(modData |> filter(!is.na(away_fg_att)) |> pull(away_fg_att))
away_fg_made_range <- range(modData |> filter(!is.na(away_fg_made)) |> pull(away_fg_made))

range_totalTD <- c(min(home_totalTD_range,away_totalTD_range), 
                   max(home_totalTD_range,away_totalTD_range))
range_fg_att_range <- c(min(home_fg_att_range,away_fg_att_range), 
                        max(home_fg_att_range,away_fg_att_range))
range_fg_made_range <- c(min(home_fg_made_range,away_fg_made_range), 
                         max(home_fg_made_range,away_fg_made_range))

# Fit historical ----
# Family: MV(skew_normal, skew_normal) 
# Links: mu = identity; sigma = identity; alpha = identity
# mu = identity; sigma = identity; alpha = identity 
# Formula: home_score ~ 0 + Intercept + home_PFG + away_PAG + home_SRS + away_SRS + home_off_pass_epa_mean_cum + home_off_rush_epa_mean_cum + away_def_pass_epa_mean_cum + away_def_rush_epa_mean_cum + home_rest + away_rest + location + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 
# away_score ~ 0 + Intercept + away_PFG + home_PAG + home_SRS + away_SRS + away_off_pass_epa_mean_cum + away_off_rush_epa_mean_cum + home_def_pass_epa_mean_cum + home_def_rush_epa_mean_cum + home_rest + away_rest + location + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 

# Family: MV(negbinomial, negbinomial) 
# Links: mu = log; shape = identity
# mu = log; shape = identity 
# Formula: home_score ~ 0 + Intercept + home_OSRS + away_DSRS + home_off_pass_epa_cum + home_off_rush_epa_cum + away_def_pass_epa_cum + away_def_rush_epa_cum + home_rest + away_rest + div_game + roof + temp + wind + (1 | home_team) + (1 | away_team) 
# away_score ~ 0 + Intercept + home_DSRS + away_OSRS + away_off_pass_epa_cum + away_off_rush_epa_cum + home_def_pass_epa_cum + home_def_rush_epa_cum + home_rest + away_rest + div_game + roof + temp + wind + (1 | home_team) + (1 | away_team)

iters <- 2000
burn <- 1000
chains <- 2
sims <- (iters-burn)*chains

## Formulas ----
### Home ----
#### TD ----
##### Off TD
formulaFitHomeTD <- 
  bf(home_totalTD ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_OSRS +
       # away_DSRS +
       # home_OSRS:away_DSRS +
       home_off_epa_cum +
       home_off_pass_epa_cum +
       home_off_rush_epa_cum +
       home_off_special_epa_cum +
       home_off_penalty_epa_cum +
       home_off_epa_roll +
       home_off_pass_epa_roll +
       home_off_rush_epa_roll +
       home_off_special_epa_roll +
       home_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       away_def_epa_cum +
       away_def_pass_epa_cum +
       away_def_rush_epa_cum +
       away_def_special_epa_cum +
       away_def_penalty_epa_cum +
       away_def_epa_roll +
       away_def_pass_epa_roll +
       away_def_rush_epa_roll +
       away_def_special_epa_roll +
       away_def_penalty_epa_roll +
       home_off_pass_epa_cum:away_def_pass_epa_cum +
       home_off_rush_epa_cum:away_def_rush_epa_cum +
       home_off_pass_epa_roll:away_def_pass_epa_roll +
       home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_off_penalty_epa_cum:away_def_penalty_epa_cum +
       home_off_penalty_epa_roll:away_def_penalty_epa_roll +
       home_off_scr +
       #home_off_1st +
       home_off_td +
       home_off_fg +
       away_def_scr +
       #away_def_1st +
       away_def_td +
       away_def_fg +
       home_off_scr:away_def_scr +
       #home_off_1st:away_def_1st +
       home_off_td:away_def_td +
       home_off_fg:away_def_fg +
       home_off_to +
       away_def_to +
       home_off_to:away_def_to +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")
#### FG ----
formulaFitHomeFG <- 
  bf(home_fg_made ~ 0 + Intercept + # bf(home_fg_made|trials(home_fg_att) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_OSRS +
       # away_DSRS +
       # home_OSRS:away_DSRS +
       home_off_epa_cum +
       home_off_pass_epa_cum +
       home_off_rush_epa_cum +
       home_off_special_epa_cum +
       home_off_penalty_epa_cum +
       home_off_kick_epa_cum +
       home_off_epa_roll +
       home_off_pass_epa_roll +
       home_off_rush_epa_roll +
       home_off_special_epa_roll +
       home_off_penalty_epa_roll +
       home_off_kick_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       away_def_epa_cum +
       away_def_pass_epa_cum +
       away_def_rush_epa_cum +
       away_def_special_epa_cum +
       away_def_penalty_epa_cum +
       away_def_kick_epa_cum +
       away_def_epa_roll +
       away_def_pass_epa_roll +
       away_def_rush_epa_roll +
       away_def_special_epa_roll +
       away_def_penalty_epa_roll +
       away_def_kick_epa_roll +
       home_off_pass_epa_cum:away_def_pass_epa_cum +
       home_off_rush_epa_cum:away_def_rush_epa_cum +
       home_off_pass_epa_roll:away_def_pass_epa_roll +
       home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_off_penalty_epa_cum:away_def_penalty_epa_cum +
       home_off_penalty_epa_roll:away_def_penalty_epa_roll +
       home_off_kick_epa_cum:away_def_kick_epa_cum +
       home_off_kick_epa_roll:away_def_kick_epa_roll +
       home_off_scr +
       #home_off_1st +
       home_off_td +
       home_off_fg +
       away_def_scr +
       #away_def_1st +
       away_def_td +
       away_def_fg +
       home_off_scr:away_def_scr +
       #home_off_1st:away_def_1st +
       home_off_td:away_def_td +
       home_off_fg:away_def_fg +
       home_off_to +
       away_def_to +
       home_off_to:away_def_to +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")
#### Extra Pt ----
formulaFitHomeXP <- 
  bf(home_pat_made|trials(home_totalTD) ~ 0 + Intercept + # bf(home_pat_made|trials(home_pat_att) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_OSRS +
       # away_DSRS +
       # home_OSRS:away_DSRS +
       # home_off_epa_cum +
       #home_off_pass_epa_cum +
       #home_off_rush_epa_cum +
       home_off_special_epa_cum +
       home_off_kick_epa_cum +
       # home_off_penalty_epa_cum +
       # home_off_epa_roll +
       #home_off_pass_epa_roll +
       #home_off_rush_epa_roll +
       home_off_special_epa_roll +
       home_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       #away_def_pass_epa_cum +
       #away_def_rush_epa_cum +
       #away_def_special_epa_cum +
       #away_def_kick_epa_cum +
       # away_def_penalty_epa_cum +
       # away_def_epa_roll +
       # away_def_pass_epa_roll +
       # away_def_rush_epa_roll +
       #away_def_special_epa_roll +
       away_def_kick_epa_cum +
       # away_def_penalty_epa_roll +
       # home_off_pass_epa_cum:away_def_pass_epa_cum +
       # home_off_rush_epa_cum:away_def_rush_epa_cum +
       # home_off_pass_epa_roll:away_def_pass_epa_roll +
       # home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "binomial")
#### Two Pt ----
formulaFitHomeTP <- 
  bf(home_twoPtConv|trials(home_totalTD - home_pat_made) ~ 0 + Intercept + #bf(home_twoPtConv|trials(home_twoPtAtt) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_OSRS +
       # away_DSRS +
       # home_OSRS:away_DSRS +
       # home_off_epa_cum +
       home_off_pass_epa_cum +
       home_off_rush_epa_cum +
       #home_off_special_epa_cum +
       #home_off_kick_epa_cum +
       # home_off_penalty_epa_cum +
       # home_off_epa_roll +
       #home_off_pass_epa_roll +
       home_off_rush_epa_roll +
       #home_off_special_epa_roll +
       #home_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       away_def_pass_epa_cum +
       #away_def_rush_epa_cum +
       #away_def_special_epa_cum +
       # away_def_penalty_epa_cum +
       # away_def_epa_roll +
       #away_def_pass_epa_roll +
       #away_def_rush_epa_roll +
       #away_def_special_epa_roll +
       # away_def_penalty_epa_roll +
       home_off_pass_epa_cum:away_def_pass_epa_cum +
       #home_off_rush_epa_cum:away_def_rush_epa_cum +
       #home_off_pass_epa_roll:away_def_pass_epa_roll +
       #home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "binomial")
#### TSafety ----
formulaFitHomeSafe <- 
  bf(home_safeties ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_DSRS +
       # away_OSRS +
       # home_DSRS:away_OSRS +
       # home_off_epa_cum +
       #home_off_pass_epa_cum +
       #home_off_rush_epa_cum +
       #home_off_special_epa_cum +
       #home_off_kick_epa_cum +
       # home_off_penalty_epa_cum +
       # home_off_epa_roll +
       #home_off_pass_epa_roll +
       #home_off_rush_epa_roll +
       #home_off_special_epa_roll +
       #home_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       #away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       #away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       #home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       #home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       #away_def_pass_epa_cum +
       #away_def_rush_epa_cum +
       #away_def_special_epa_cum +
       # away_def_penalty_epa_cum +
       # away_def_epa_roll +
       #away_def_pass_epa_roll +
       #away_def_rush_epa_roll +
       #away_def_special_epa_roll +
       # away_def_penalty_epa_roll +
       #home_off_pass_epa_cum:away_def_pass_epa_cum +
       #home_off_rush_epa_cum:away_def_rush_epa_cum +
       #home_off_pass_epa_roll:away_def_pass_epa_roll +
       #home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")
### Away ----
#### TD ----
formulaFitAwayTD <- 
  bf(away_totalTD ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # home_OSRS +
       # away_DSRS +
       # home_OSRS:away_DSRS +
       away_off_epa_cum +
       away_off_pass_epa_cum +
       away_off_rush_epa_cum +
       away_off_special_epa_cum +
       away_off_penalty_epa_cum +
       away_off_epa_roll +
       away_off_pass_epa_roll +
       away_off_rush_epa_roll +
       away_off_special_epa_roll +
       away_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       home_def_epa_cum +
       home_def_pass_epa_cum +
       home_def_rush_epa_cum +
       home_def_special_epa_cum +
       home_def_penalty_epa_cum +
       home_def_epa_roll +
       home_def_pass_epa_roll +
       home_def_rush_epa_roll +
       home_def_special_epa_roll +
       home_def_penalty_epa_roll +
       away_off_pass_epa_cum:home_def_pass_epa_cum +
       away_off_rush_epa_cum:home_def_rush_epa_cum +
       away_off_pass_epa_roll:home_def_pass_epa_roll +
       away_off_rush_epa_roll:home_def_rush_epa_roll +
       away_off_penalty_epa_cum:home_def_penalty_epa_cum +
       away_off_penalty_epa_roll:home_def_penalty_epa_roll +
       away_off_scr +
       #away_off_1st +
       away_off_td +
       away_off_fg +
       home_def_scr +
       #home_def_1st +
       home_def_td +
       home_def_fg +
       away_off_scr:home_def_scr +
       #away_off_1st:home_def_1st +
       away_off_td:home_def_td +
       away_off_fg:home_def_fg +
       away_off_to +
       home_def_to +
       away_off_to:home_def_to +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")
#### FG ----
formulaFitAwayFG <- 
  bf(away_fg_made ~ 0 + Intercept + # bf(home_fg_made|trials(home_fg_att) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # away_OSRS +
       # home_DSRS +
       # away_OSRS:home_DSRS +
       away_off_epa_cum +
       away_off_pass_epa_cum +
       away_off_rush_epa_cum +
       away_off_special_epa_cum +
       away_off_penalty_epa_cum +
       away_off_epa_roll +
       away_off_pass_epa_roll +
       away_off_rush_epa_roll +
       away_off_special_epa_roll +
       away_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       home_def_epa_cum +
       home_def_pass_epa_cum +
       home_def_rush_epa_cum +
       home_def_special_epa_cum +
       home_def_penalty_epa_cum +
       home_def_epa_roll +
       home_def_pass_epa_roll +
       home_def_rush_epa_roll +
       home_def_special_epa_roll +
       home_def_penalty_epa_roll +
       away_off_pass_epa_cum:home_def_pass_epa_cum +
       away_off_rush_epa_cum:home_def_rush_epa_cum +
       away_off_pass_epa_roll:home_def_pass_epa_roll +
       away_off_rush_epa_roll:home_def_rush_epa_roll +
       away_off_penalty_epa_cum:home_def_penalty_epa_cum +
       away_off_penalty_epa_roll:home_def_penalty_epa_roll +
       away_off_scr +
       #away_off_1st +
       away_off_td +
       away_off_fg +
       home_def_scr +
       #home_def_1st +
       home_def_td +
       home_def_fg +
       away_off_scr:home_def_scr +
       #away_off_1st:home_def_1st +
       away_off_td:home_def_td +
       away_off_fg:home_def_fg +
       away_off_to +
       home_def_to +
       away_off_to:home_def_to +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")
#### Extra Pt ----
formulaFitAwayXP <- 
  bf(away_pat_made|trials(away_totalTD) ~ 0 + Intercept + # bf(home_pat_made|trials(home_pat_att) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # away_OSRS +
       # home_DSRS +
       # away_OSRS:home_DSRS +
       # home_off_epa_cum +
       #home_off_pass_epa_cum +
       #home_off_rush_epa_cum +
       away_off_special_epa_cum +
       away_off_kick_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       #away_off_pass_epa_roll +
       #away_off_rush_epa_roll +
       away_off_special_epa_roll +
       away_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       #away_def_pass_epa_cum +
       #away_def_rush_epa_cum +
       home_def_special_epa_cum +
       home_def_kick_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       home_def_special_epa_roll +
       home_def_kick_epa_cum +
       # away_def_penalty_epa_roll +
       # home_off_pass_epa_cum:away_def_pass_epa_cum +
       # home_off_rush_epa_cum:away_def_rush_epa_cum +
       # home_off_pass_epa_roll:away_def_pass_epa_roll +
       # home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "binomial")
#### Two Pt ----
formulaFitAwayTP <- 
  bf(away_twoPtConv|trials(away_totalTD - away_pat_made) ~ 0 + Intercept + #bf(home_twoPtConv|trials(home_twoPtAtt) ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       home_SRS +
       away_SRS +
       home_SRS:away_SRS +
       # spread_line +
       # total_line +
       # away_OSRS +
       # home_DSRS +
       # away_OSRS:home_DSRS +
       # home_off_epa_cum +
       away_off_pass_epa_cum +
       away_off_rush_epa_cum +
       #away_off_special_epa_cum +
       #away_off_kick_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       away_off_pass_epa_roll +
       away_off_rush_epa_roll +
       #home_off_special_epa_roll +
       #home_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       # away_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       # away_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       # home_def_epa_cum +
       # home_def_pass_epa_cum +
       # home_def_rush_epa_cum +
       # home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       home_def_pass_epa_cum +
       home_def_rush_epa_cum +
       #home_def_special_epa_cum +
       # home_def_penalty_epa_cum +
       # home_def_epa_roll +
       home_def_pass_epa_roll +
       home_def_rush_epa_roll +
       #home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       away_off_pass_epa_cum:home_def_pass_epa_cum +
       away_off_rush_epa_cum:home_def_rush_epa_cum +
       away_off_pass_epa_roll:home_def_pass_epa_roll +
       away_off_rush_epa_roll:home_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "binomial")
#### TSafety ----
formulaFitAwaySafe <- 
  bf(away_safeties ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       # home_SRS +
       # away_SRS +
       # home_SRS:away_SRS +
       # spread_line +
       # total_line +
       away_OSRS +
       home_DSRS +
       away_OSRS:home_DSRS +
       # home_off_epa_cum +
       #home_off_pass_epa_cum +
       #home_off_rush_epa_cum +
       #home_off_special_epa_cum +
       #home_off_kick_epa_cum +
       # home_off_penalty_epa_cum +
       # home_off_epa_roll +
       #home_off_pass_epa_roll +
       #home_off_rush_epa_roll +
       #home_off_special_epa_roll +
       #home_off_kick_epa_roll +
       # home_off_penalty_epa_roll +
       home_off_epa_cum +
       # away_off_pass_epa_cum +
       # away_off_rush_epa_cum +
       # away_off_special_epa_cum +
       # away_off_penalty_epa_cum +
       home_off_epa_roll +
       # away_off_pass_epa_roll +
       # away_off_rush_epa_roll +
       # away_off_special_epa_roll +
       # away_off_penalty_epa_roll +
       away_def_epa_cum +
       # away_def_pass_epa_cum +
       # away_def_rush_epa_cum +
       # away_def_special_epa_cum +
       # away_def_penalty_epa_cum +
       away_def_epa_roll +
       # home_def_pass_epa_roll +
       # home_def_rush_epa_roll +
       # home_def_special_epa_roll +
       # home_def_penalty_epa_roll +
       # away_def_epa_cum +
       #away_def_pass_epa_cum +
       #away_def_rush_epa_cum +
       #away_def_special_epa_cum +
       # away_def_penalty_epa_cum +
       # away_def_epa_roll +
       #away_def_pass_epa_roll +
       #away_def_rush_epa_roll +
       #away_def_special_epa_roll +
       # away_def_penalty_epa_roll +
       #home_off_pass_epa_cum:away_def_pass_epa_cum +
       #home_off_rush_epa_cum:away_def_rush_epa_cum +
       #home_off_pass_epa_roll:away_def_pass_epa_roll +
       #home_off_rush_epa_roll:away_def_rush_epa_roll +
       home_rest +
       away_rest +
       home_rest:away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|home_team) +
       (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")

## Forms from sig2 ----
### Home ----
#### TD ----
##### Off TD
formulaFitHomeTD2 <- 
  bf(home_totalTD ~ 0 + Intercept + #home_totalTD|trunc(ub = 10) ~ 0 + Intercept +
       home_SRS + 
       away_SRS +
       #home_off_pass_epa_cum +
       #away_def_pass_epa_cum +
       home_off_rush_epa_cum +
       away_def_rush_epa_cum +
       home_off_epa_roll + 
       #away_def_epa_cum +
       away_def_penalty_epa_cum +
       away_def_special_epa_roll + 
       #home_off_scr + 
       #away_def_scr + 
       home_off_td +
       away_def_td +
       #away_def_to + 
       home_off_interceptions +
       away_def_interceptions +
       home_off_fumbles +
       away_def_fumbles +
       home_off_interceptions:away_def_interceptions +
       #home_off_fumbles:away_def_fumbles +
       wind + 
       #home_off_pass_epa_cum:away_def_pass_epa_cum + 
       home_off_rush_epa_cum:away_def_rush_epa_cum + 
       #home_off_scr:away_def_scr + 
       home_off_td:away_def_td +
       (1|home_team) +
       (1|away_team)
     #nl = TRUE
  ) + brmsfamily(family = "discrete_weibull")
#### FG ----
formulaFitHomeFG2 <- 
  bf(home_fg_made ~ 0 + Intercept + # bf(home_fg_made|trunc(ub = 10) ~ 0 + Intercept +
       home_off_special_epa_cum + 
       home_off_kick_epa_cum +
       away_def_kick_epa_cum +
       home_off_rush_epa_roll +
       away_def_rush_epa_roll +
       home_off_penalty_epa_roll +
       away_def_penalty_epa_roll +
       home_off_kick_epa_roll + 
       away_def_kick_epa_roll +
       home_off_rush_epa_roll:away_def_rush_epa_roll + 
       home_off_penalty_epa_roll:away_def_penalty_epa_roll + 
       home_off_kick_epa_cum:away_def_kick_epa_cum +
       home_off_kick_epa_roll:away_def_kick_epa_roll +
       home_off_to +
       away_def_to +
       home_off_to:away_def_to +
       #home_off_fg_att_roll +
       #home_off_fg_pct_cum +
       #home_off_fg_pct_roll +
       location +
       (1|home_team) +
       (1|away_team)
     #family = brmsfamily(family = "discrete_weibull")
  ) + brmsfamily(family = "discrete_weibull")
#### Extra Pt ----
# formulaFitHomeXP2 <- 
#   bf(home_pat_made|trials(home_totalTD) ~ 0 + Intercept + # bf(home_pat_made|trials(home_pat_att) ~ 0 + Intercept +
#        away_SRS + 
#        home_off_special_epa_cum + 
#        home_off_special_epa_roll + 
#        home_off_kick_epa_roll +
#        home_rest + 
#        away_rest +
#        home_rest:away_rest +
#        roof +
#        (1|home_team) +
#        (1|away_team)
#      #family = brmsfamily(family = "binomial")
#   ) + brmsfamily(family = "binomial")
#### Two Pt ----
# formulaFitHomeTP2 <- 
#   bf(home_twoPtConv|trials(home_totalTD) ~ 0 + Intercept + #bf(home_twoPtConv|trials(home_twoPtAtt) ~ 0 + Intercept +
#        away_SRS +
#        home_off_rush_epa_cum + 
#        home_off_rush_epa_roll +
#        roof +
#        (1|home_team) +
#        (1|away_team)
#      #family = brmsfamily(family = "binomial")
#   ) + brmsfamily(family = "binomial")
### Away ----
#### TD ----
formulaFitAwayTD2 <- 
  bf(away_totalTD ~ 0 + Intercept + #bf(away_totalTD|trunc(ub = 10) ~ 0 + Intercept +
       away_off_pass_epa_cum +
       away_off_rush_epa_cum +
       away_off_penalty_epa_cum +
       home_def_penalty_epa_cum + 
       away_off_penalty_epa_roll +
       home_def_penalty_epa_roll + 
       away_off_td + 
       away_off_to + 
       home_rest + 
       away_off_penalty_epa_cum:home_def_penalty_epa_cum +
       away_off_penalty_epa_roll:home_def_penalty_epa_roll +
       #away_off_interceptions +
       home_def_interceptions +
       #away_off_fumbles +
       #home_def_fumbles +
       #away_off_interceptions:home_def_interceptions +
       #away_off_fumbles:home_def_fumbles +
       roof +
       (1|home_team) +
       (1|away_team)
     #nl = TRUE
     #family = brmsfamily(family = "discrete_weibull")
  ) + brmsfamily(family = "discrete_weibull")
#### FG ----
formulaFitAwayFG2 <- 
  bf(away_fg_made ~ 0 + Intercept + # bf(away_fg_made|trunc(ub = 10) ~ 0 + Intercept +
       away_SRS + 
       away_off_special_epa_cum + 
       away_off_pass_epa_roll + 
       #away_off_special_epa_roll + 
       away_off_penalty_epa_roll +
       home_def_special_epa_cum + 
       home_def_special_epa_roll +
       #home_def_td + 
       away_off_to + 
       #away_off_fg_att_roll +
       away_off_fg_pct_cum +
       away_off_fg_pct_roll +
       div_game +
       (1|home_team) +
       (1|away_team)
     #family = brmsfamily(family = "discrete_weibull")
  ) + brmsfamily(family = "discrete_weibull")
#### Extra Pt ----
# formulaFitAwayXP2 <- 
#   bf(away_pat_made|trials(away_totalTD) ~ 0 + Intercept + # bf(home_pat_made|trials(home_pat_att) ~ 0 + Intercept +
#        home_SRS + 
#        away_SRS + 
#        home_SRS:away_SRS +
#        away_off_kick_epa_cum +
#        away_off_kick_epa_roll + 
#        home_def_special_epa_roll +
#        #home_rest + 
#        location +
#        temp + 
#        wind +
#        (1|home_team) +
#        (1|away_team)
#      #family = brmsfamily(family = "binomial")
#   ) + brmsfamily(family = "binomial")
#### Two Pt ----
# formulaFitAwayTP2 <- 
#   bf(away_twoPtConv|trials(away_totalTD) ~ 0 + Intercept + #bf(home_twoPtConv|trials(home_twoPtAtt) ~ 0 + Intercept +
#        away_off_pass_epa_cum + 
#        away_off_rush_epa_roll + 
#        home_def_pass_epa_roll +
#        #home_rest +
#        #away_rest +
#        #home_rest:away_rest +
#        roof +
#        (1|home_team) +
#        (1|away_team)
#      #family = brmsfamily(family = "binomial")
#   ) + brmsfamily(family = "binomial")

## Fit ----
priorPoints <- set_prior(horseshoe(), class = "b")
priorhomeTD <- prior(normal(0,5), nlpar = "homeTD")
priorawayTD <- prior(normal(0,5), nlpar = "awayTD")

# 10 mins
system.time(
  Fit <- brm(
    formulaFitHomeTD2 + formulaFitHomeFG2 + #formulaFitHomeXPA2 + 
      #formulaFitHomeXP2 + formulaFitHomeTP2 + #formulaFitHomeSafe +
      formulaFitAwayTD2 + formulaFitAwayFG2 + #formulaFitAwayXPA2 +
      #formulaFitAwayXP2 + formulaFitAwayTP2 + #formulaFitAwaySafe +
      # formulaFitHomeTD + formulaFitHomeFG + formulaFitHomeXP + formulaFitHomeTP + #formulaFitHomeSafe +
      #   formulaFitAwayTD + formulaFitAwayFG + formulaFitAwayXP + formulaFitAwayTP + #formulaFitAwaySafe +
      set_rescor(FALSE),
    data = histModelData,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    warmup = burn,
    iter = iters,
    chains = chains,
    cores = parallel::detectCores(),
    prior = priorPoints,
    drop_unused_levels = FALSE,
    #normalize = TRUE,
    init = 0,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan"
  )
)

# system.time(
#   Fit2 <- brm(
#     #formulaFitHomeTD2 + formulaFitHomeFG2 + #formulaFitHomeXPA2 + 
#       formulaFitHomeXP2 + formulaFitHomeTP2 + #formulaFitHomeSafe +
#       #formulaFitAwayTD2 + formulaFitAwayFG2 + #formulaFitAwayXPA2 +
#       formulaFitAwayXP2 + formulaFitAwayTP2 + #formulaFitAwaySafe +
#       # formulaFitHomeTD + formulaFitHomeFG + formulaFitHomeXP + formulaFitHomeTP + #formulaFitHomeSafe +
#       #   formulaFitAwayTD + formulaFitAwayFG + formulaFitAwayXP + formulaFitAwayTP + #formulaFitAwaySafe +
#       set_rescor(FALSE),
#     data = histModelData,
#     save_pars = save_pars(all = TRUE),
#     seed = 52,
#     warmup = burn,
#     iter = iters,
#     chains = chains,
#     cores = parallel::detectCores(),
#     prior = priorPoints,
#     drop_unused_levels = FALSE,
#     #normalize = TRUE,
#     init = 0,
#     control = list(adapt_delta = 0.95),
#     backend = "cmdstan"
#   )
# )

fit <- 2
assign(paste0("fit", fit), Fit)
#assign(paste0("fitB", fit), Fit2)
save(fit16, file= paste0("~/Desktop/fit", fit, ".RData"))

Fit <- fit8
#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
#fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))
## Diagnostics ----
prior_summary(Fit)
# posterior_summary(Fit)
# launch_shinystan(Fit)
print(Fit, digits = 4)
fixedEff <- fixef(Fit)
fixedEff <- data.frame(fixedEff) |>
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
print(fixedEff, digits = 4)
fixedSigEff <- fixedEff |> filter(p_val < 0.2)
fixedSigEff <- fixedSigEff |> 
  rownames_to_column() |>
  mutate(
    response = str_split_i(rowname, "_", i = 1),
    param = str_remove(rowname, paste0(response,"_"))
  ) |> 
  relocate(c(response, param), .after = "rowname") |>
  select(-rowname)
fixedSigEff

assign(paste0("fixedEff", fit), fixedEff)
assign(paste0("fixedSigEff", fit), fixedSigEff)

# fixedSigEff1forms <- fixedSigEff1 |>
#   select(response, param) |>
#   group_by(response) |>
#   summarise(
#     formula = paste(param, collapse = " + ")
#   )
# fixedSigEff1forms$formula[7]
# fixedSigEff1forms$formula[5]
# fixedSigEff1forms$formula[6]
# fixedSigEff1forms$formula[8]
# fixedSigEff1forms$formula[3]
# fixedSigEff1forms$formula[1]
# fixedSigEff1forms$formula[2]
# fixedSigEff1forms$formula[4]
# 
# fixedSigEff1
# print(fixedSigEff, digits = 4)
# save(fixedSigEff, file = paste0("./Model Fitting/Data/fixedSigEff", fit, ".RData"))
# 
# fixedSigEff13141516 <- bind_rows(
#   fixedSigEff13 |> 
#     mutate(Fit = "fit13", .before = 1) |> 
#     mutate(response = factor(response, levels = unique(response))), 
#   fixedSigEff2 |> 
#     mutate(Fit = "fit14", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff15 |> 
#     mutate(Fit = "fit15", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff16 |> 
#     mutate(Fit = "fit16", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff17 |> 
#     mutate(Fit = "fit17", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response)))
#   ) |>
#   group_by(response, Fit) |>
#   arrange(p_val, .by_group = TRUE) |>
#   ungroup()
# 
# fixedSigEff1314 |> 
#   filter(response == "hometotalTD") |>
#   pull(param) |> unique()

randEff <- ranef(Fit, summary = TRUE)
print(randEff, digits = 4)
VarCorr(Fit)

#plot(Fit, ask = FALSE)

postSum <- posterior_summary(Fit)
#postSum[grepl("^sd_", rownames(postSum)), ]
FitR2temp <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())
FitR2temp

FitR2 <- bind_rows(
  FitR2temp,
  FitR2
)
FitR2 #<- FitR2temp

# loo7 <- loo(fit7)
# 
# loo_compare(
#   #loo7, 
#   loo8,
#   loo9,
#   loo10,
#   loo11,
#   loo12,
#   loo20
# )

## PPC Plot ----
# homePPCtd <- pp_check(Fit, resp = "hometotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC td")) +
#   theme_bw()
# homePPCfg <- pp_check(Fit, resp = "homefgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC fg")) +
#   theme_bw()
# homePPCxp <- pp_check(Fit2, resp = "homepatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC xp")) +
#   theme_bw()
# homePPCtp <- pp_check(Fit2, resp = "hometwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC tp")) +
#   theme_bw()
# homePPCsafe <- pp_check(Fit, resp = "homesafeties", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC safe")) +
#   theme_bw()

homePPCbarstd <- pp_check(Fit, resp = "hometotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC td")) +
  theme_bw()
homePPCbarsfg <- pp_check(Fit, resp = "homefgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC fg")) +
  theme_bw()
# homePPCbarsxpa <- pp_check(Fit, resp = "homepatatt", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Home PPC xpa")) +
#   theme_bw()
# homePPCbarsxp <- pp_check(Fit2, resp = "homepatmade", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Home PPC xp")) +
#   theme_bw()
# homePPCbarstp <- pp_check(Fit2, resp = "hometwoPtConv", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Home PPC tp")) +
#   theme_bw()
# homePPCbarssafe <- pp_check(Fit, resp = "homesafeties", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Home PPC safe")) +
#   theme_bw()

# awayPPCtd <- pp_check(Fit, resp = "awaytotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC td")) +
#   theme_bw()
# awayPPCfg <- pp_check(Fit, resp = "awayfgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC fg")) +
#   theme_bw()
# awayPPCxp <- pp_check(Fit2, resp = "awaypatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC xp")) +
#   theme_bw()
# awayPPCtp <- pp_check(Fit2, resp = "awaytwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC tp")) +
#   theme_bw()
# awayPPCsafe <- pp_check(Fit, resp = "awaysafeties", ndraws = 100) + 
#   labs(title = paste0("Fit", fit, " Away PPC safe")) +
#   theme_bw()

awayPPCbarstd <- pp_check(Fit, resp = "awaytotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC td")) +
  theme_bw()
awayPPCbarsfg <- pp_check(Fit, resp = "awayfgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC fg")) +
  theme_bw()
# awayPPCbarsxpa <- pp_check(Fit, resp = "awaypatatt", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Away PPC xp")) +
#   theme_bw()
# awayPPCbarsxp <- pp_check(Fit2, resp = "awaypatmade", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Away PPC xp")) +
#   theme_bw()
# awayPPCbarstp <- pp_check(Fit2, resp = "awaytwoPtConv", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Away PPC tp")) +
#   theme_bw()
# awayPPCbarssafe <- pp_check(Fit, resp = "awaysafeties", ndraws = 100, type = "bars") + 
#   labs(title = paste0("Fit", fit, " Away PPC safe")) +
#   theme_bw()

# homePPCtd
# homePPCfg
# homePPCxp
# homePPCtp
# homePPCsafe
homePPCbarstd
homePPCbarsfg
#homePPCbarsxpa
# homePPCbarsxp
# homePPCbarstp
#homePPCbarssafe
# awayPPCtd
# awayPPCfg
# awayPPCxp
# awayPPCtp
# awayPPCsafe
awayPPCbarstd
awayPPCbarsfg
#awayPPCbarsxpa
# awayPPCbarsxp
# awayPPCbarstp
#awayPPCbarssafe

## Fitted
homefinalFittd <- posterior_predict(Fit, resp = "hometotalTD")
homefinalFitfg <- posterior_predict(Fit, resp = "homefgmade")
#homefinalFitxp <- posterior_predict(Fit2, resp = "homepatmade")
#homefinalFittp <- posterior_predict(Fit2, resp = "hometwoPtConv")
#homefinalFitsafe <- posterior_predict(Fit, resp = "homesafeties")

## Preds
homefinalPredstd <- posterior_predict(Fit,
                                      resp = "hometotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsfg <- posterior_predict(Fit,
                                      resp = "homefgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
# homefinalPredsxp <- posterior_predict(Fit2,
#                                       resp = "homepatmade",
#                                       newdata = modelData,
#                                       allow_new_levels = TRUE,
#                                       re_formula = NULL
# )
# homefinalPredstp <- posterior_predict(Fit2,
#                                       resp = "hometwoPtConv",
#                                       newdata = modelData,
#                                       allow_new_levels = TRUE,
#                                       re_formula = NULL
# )
# homefinalPredssafe <- posterior_predict(Fit,
#                                         resp = "homesafeties",
#                                         newdata = modelData,
#                                         allow_new_levels = TRUE,
#                                         re_formula = NULL
# )

## Fitted
awayfinalFittd <- posterior_predict(Fit, resp = "awaytotalTD")
awayfinalFitfg <- posterior_predict(Fit, resp = "awayfgmade")
#awayfinalFitxp <- posterior_predict(Fit2, resp = "awaypatmade")
#awayfinalFittp <- posterior_predict(Fit2, resp = "awaytwoPtConv")
#awayfinalFitsafe <- posterior_predict(Fit, resp = "awaysafeties")

## Preds
awayfinalPredstd <- posterior_predict(Fit,
                                      resp = "awaytotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsfg <- posterior_predict(Fit,
                                      resp = "awayfgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
# awayfinalPredsxp <- posterior_predict(Fit2,
#                                       resp = "awaypatmade",
#                                       newdata = modelData,
#                                       allow_new_levels = TRUE,
#                                       re_formula = NULL
# )
# awayfinalPredstp <- posterior_predict(Fit2,
#                                       resp = "awaytwoPtConv",
#                                       newdata = modelData,
#                                       allow_new_levels = TRUE,
#                                       re_formula = NULL
# )
# awayfinalPredssafe <- posterior_predict(Fit,
#                                         resp = "awaysafeties",
#                                         newdata = modelData,
#                                         allow_new_levels = TRUE,
#                                         re_formula = NULL
# )

homePPDbarstd <- ppc_bars(y = modelData$home_totalTD, 
                          yrep = homefinalPredstd[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD td")) +
  theme_bw()
homePPDbarsfg <- ppc_bars(y = modelData$home_fg_made, 
                          yrep = homefinalPredsfg[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD fg")) +
  theme_bw()
# homePPDbarsxp <- ppc_bars(y = modelData$home_pat_made, 
#                           yrep = homefinalPredsxp[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home PPD xp")) +
#   theme_bw()
# homePPDbarstp <- ppc_bars(y = modelData$home_twoPtConv, 
#                           yrep = homefinalPredstp[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home PPD tp")) +
#   theme_bw()

awayPPDbarstd <- ppc_bars(y = modelData$away_totalTD, 
                          yrep = awayfinalPredstd[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD td")) +
  theme_bw()
awayPPDbarsfg <- ppc_bars(y = modelData$away_fg_made, 
                          yrep = awayfinalPredsfg[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD fg")) +
  theme_bw()
# awayPPDbarsxp <- ppc_bars(y = modelData$away_pat_made, 
#                           yrep = awayfinalPredsxp[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away PPD xp")) +
#   theme_bw()
# awayPPDbarstp <- ppc_bars(y = modelData$away_twoPtConv, 
#                           yrep = awayfinalPredstp[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away PPD tp")) +
#   theme_bw()

homePPDbarstd
homePPDbarsfg
# homePPDbarsxp
# homePPDbarstp

awayPPDbarstd
awayPPDbarsfg
# awayPPDbarsxp
# awayPPDbarstp

#### Home Score ----
homefinalFit <- 
  7*homefinalFittd + 
  3*homefinalFitfg #+
  #1*homefinalFitxp +
  #2*homefinalFittp #+
#2*homefinalFitsafe

## Fitted
#homefinalFit <- posterior_predict(Fit, resp = "homescore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <-
  7*homefinalPredstd +
  3*homefinalPredsfg #+
  #1*homefinalPredsxp +
  #2*homefinalPredstp #+
#2*homefinalPredssafe

# homefinalPreds <- posterior_predict(Fit,
#                                     resp = "homescore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
awayfinalFit <- 
  7*awayfinalFittd + 
  3*awayfinalFitfg #+
  #1*awayfinalFitxp +
  #2*awayfinalFittp #+
#2*awayfinalFitsafe
## Fitted
#awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <-
  7*awayfinalPredstd +
  3*awayfinalPredsfg #+
  #1*awayfinalPredsxp +
  #2*awayfinalPredstp #+
#2*awayfinalPredssafe
# awayfinalPreds <- posterior_predict(Fit,
#                                     resp = "awayscore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("home", "away"),
  MAE_fit = c(mean(abs(homefinalFitMean - histModelData$home_score)),
              mean(abs(awayfinalFitMean - histModelData$away_score))),
  COV_fit = c(mean(homefinalFitLCB < histModelData$home_score &  histModelData$home_score < homefinalFitUCB),
              mean(awayfinalFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalFitUCB)),
  MAE_pred = c(mean(abs(homefinalPredsMean - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalPredsMean - modelData$away_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(homefinalPredsMed - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalPredsMed - modelData$away_score), na.rm = TRUE)),
  COV_pred = c(mean(homefinalPredsLCB < modelData$home_score & modelData$home_score < homefinalPredsUCB, na.rm = TRUE),
               mean(awayfinalPredsLCB < modelData$away_score & modelData$away_score < awayfinalPredsUCB, na.rm = TRUE))
  # home_MAE_fit = mean(abs(homefinalFitMean - histModelData$home_score)),
  # home_COV_fit = mean(homefinalFitLCB < histModelData$home_score &  histModelData$home_score < homefinalFitUCB),
  # away_MAE_fit = mean(abs(awayfinalFitMean - histModelData$away_score)),
  # away_COV_fit = mean(awayfinalFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalFitUCB),
  # home_MAE_pred = mean(abs(homefinalPredsMean - modelData$home_score), na.rm = TRUE),
  # home_MAD_pred = mean(abs(homefinalPredsMed - modelData$home_score), na.rm = TRUE),
  # home_COV_pred = mean(homefinalPredsLCB < modelData$home_score & modelData$home_score < homefinalPredsUCB),
  # away_MAE_pred = mean(abs(awayfinalPredsMean - modelData$away_score), na.rm = TRUE),
  # away_MAD_pred = mean(abs(awayfinalPredsMed - modelData$away_score), na.rm = TRUE),
  # away_COV_pred = mean(awayfinalPredsLCB < modelData$away_score & modelData$away_score < awayfinalPredsUCB, na.rm = TRUE)
)
predMetricsHA


#### Spread ----
FittedSpread <- homefinalFit - awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMeanSpread <- colMeans(FittedSpread)
FittedMedSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.5)})
FittedLCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.025)})
FittedUCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsSpread <- homefinalPreds - awayfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMeanSpread <- colMeans(PredsSpread)
PredsMedSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- histModelData$result
spreadTest <- modelData$result
predMetricsSpreadTemp <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Spread", 2),
  MAE_fit = mean(abs(FittedMeanSpread - spreadTrain)),
  MAD_fit = mean(abs(FittedMedSpread - spreadTrain)),
  COV_fit = mean(FittedLCBSpread < spreadTrain & spreadTrain < FittedUCBSpread),
  MAE_pred = mean(abs(PredsMeanSpread - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedSpread - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBSpread < spreadTest & spreadTest < PredsUCBSpread)
)
predMetricsSpreadTemp

predMetricsSpread <- bind_rows(
  predMetricsSpreadTemp,
  predMetricsSpread
)
predMetricsSpread #<- predMetricsSpreadTemp

##### Plot ----
set.seed(52)
spreadPPC <- ppc_dens_overlay(y = histModelData$result, 
                              yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC td")) +
  theme_bw()
spreadPPC

spreadPPCbars <- ppc_bars(y = histModelData$result, 
                          yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC td")) +
  theme_bw()
spreadPPCbars

set.seed(52)
spreadPPD <- ppc_dens_overlay(y = modelData$result, 
                              yrep = PredsSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC td")) +
  theme_bw()
spreadPPD

spreadPPDbars <- ppc_bars(y = modelData$result, 
                          yrep = PredsSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC td")) +
  theme_bw()
spreadPPDbars



##### Prob Errors ----
##### Fit ----
spreadLineTrain <- modData |>
  filter(season %in% 2023:2023) |>
  pull(spread_line)
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- FittedSpread[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbsSpread[, j] <- probs
}
FittedBetSpread <- colMeans(FittedProbsSpread)
FittedBetLogicalSpread <- FittedBetSpread > 0.5
FittedLogicalSpread <- spreadTrain > spreadLineTrain
FittedProbSpread <- mean(FittedBetLogicalSpread == FittedLogicalSpread, na.rm = TRUE)
FittedProbSpread

spreadDataTrain <- modData |> filter(season %in% 2023:2023) |>
  select(season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTrain$spreadCoverSuccess))
sum(!is.na(spreadDataTrain$spreadCoverSuccess))

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

##### Pred ----
spreadLineTest <- modData |>
  filter(season == 2024) |>
  filter(!is.na(result)) |>
  pull(spread_line)
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- PredsSpread[, j]
  probs <- fitted > spreadLineTest[j]
  PredsProbsSpread[, j] <- probs
}
PredsBetSpread <- colMeans(PredsProbsSpread)
PredsBetLogicalSpread <- PredsBetSpread > 0.5
PredsLogicalSpread <- spreadTest > spreadLineTest
PredsProbSpread <- mean(PredsBetLogicalSpread == PredsLogicalSpread, na.rm = TRUE)
PredsProbSpread

spreadDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE, 
    #                         ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTest$spreadCoverSuccess))
sum(!is.na(spreadDataTest$spreadCoverSuccess))

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Total ----
FittedTotal <- homefinalFit + awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMeanTotal <- colMeans(FittedTotal)
FittedMedTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.5)})
FittedLCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.025)})
FittedUCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsTotal <- homefinalPreds + awayfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMeanTotal <- colMeans(PredsTotal)
PredsMedTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

totalTrain <- modData |>
  filter(season %in% 2023:2023) |>
  pull(total)
totalTest <- modData |>
  filter(season == 2024) |>
  filter(!is.na(result)) |>
  pull(total)
predMetricsTotalTemp <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Total", 2),
  MAE_fit = mean(abs(FittedMeanTotal - totalTrain)),
  MAD_fit = mean(abs(FittedMedTotal - totalTrain)),
  COV_fit = mean(FittedLCBTotal < totalTrain & totalTrain < FittedUCBTotal),
  MAE_pred = mean(abs(PredsMeanTotal - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedTotal - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBTotal < totalTest & totalTest < PredsUCBTotal)
)
predMetricsTotalTemp

predMetricsTotal <- bind_rows(
  predMetricsTotalTemp,
  predMetricsTotal
)
predMetricsTotal #<- predMetricsTotalTemp

##### Plot ----
set.seed(52)
totalPPC <- ppc_dens_overlay(y = totalTrain,
                             yrep = FittedTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPC

totalPPCbars <- ppc_bars(y =totalTrain, 
                         yrep = FittedTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPCbars

set.seed(52)
totalPPD <- ppc_dens_overlay(y = totalTest, 
                             yrep = PredsTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPD

totalPPDbars <- ppc_bars(y = totalTest, 
                         yrep = PredsTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPDbars

##### Prob Errors ----
##### Fit ----
totalLineTrain <- modData |>
  filter(season %in% 2023:2023) |>
  pull(total_line)
#totalTrain <- as.numeric(totalTrainScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

FittedProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- FittedTotal[, j]
  probs <- fitted > totalLineTrain[j]
  FittedProbsTotal[, j] <- probs
}
FittedBetTotal <- colMeans(FittedProbsTotal)
FittedBetLogicalTotal <- FittedBetTotal > 0.5
FittedLogicalTotal <- totalTrain > totalLineTrain
FittedProbTotal <- mean(FittedBetLogicalTotal == FittedLogicalTotal, na.rm = TRUE)
FittedProbTotal

totalDataTrain <- modData |> filter(season %in% 2023:2023) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = FittedMeanTotal,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = FittedBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

##### Pred ----
totalLineTest <- modData |>
  filter(season == 2024) |>
  filter(!is.na(result)) |>
  pull(total_line)
#totalTest <- as.numeric(totalTestScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

PredsProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- PredsTotal[, j]
  probs <- fitted > totalLineTest[j]
  PredsProbsTotal[, j] <- probs
}
PredsBetTotal <- colMeans(PredsProbsTotal)
PredsBetLogicalTotal <- PredsBetTotal > 0.5
PredsLogicalTotal <- totalTest > totalLineTest
PredsProbTotal <- mean(PredsBetLogicalTotal == PredsLogicalTotal, na.rm = TRUE)
PredsProbTotal

totalDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = PredsMeanTotal,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = PredsBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

successPerfTemp <- bind_rows( 
  bind_cols(
    Response = "Spread",
    spreadSuccessTrain,
    spreadSuccessTest
  ) |> rename_with(~str_remove(.x, "spread"), .cols = contains("spread")),
  bind_cols(
    Response = "Total",
    totalSuccessTrain,
    totalSuccessTest
  ) |> rename_with(~str_remove(.x, "total"), .cols = contains("total"))
)|>
  mutate(
    Fit = paste0("Fit ", fit), .before = 1
  )
successPerfTemp
successPerf <- bind_rows(
  successPerf,
  successPerfTemp
)
successPerf #<- successPerfTemp

## Predict Week ----
testWeekData <- modData2 |>
  filter(season == 2024 & week == 15) |>
  filter(is.na(result) & !is.na(spread_line))

testWeekData2 <- predict(preProcValues, testWeekData)

## Preds
homefinalPredstd <- posterior_predict(fit2,
                                      resp = "hometotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsfg <- posterior_predict(Fit,
                                      resp = "homefgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsxp <- posterior_predict(Fit,
                                      resp = "homepatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredstp <- posterior_predict(Fit,
                                      resp = "hometwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Preds
awayfinalPredstd <- posterior_predict(Fit,
                                      resp = "awaytotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsfg <- posterior_predict(Fit,
                                      resp = "awayfgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsxp <- posterior_predict(Fit,
                                      resp = "awaypatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredstp <- posterior_predict(Fit,
                                      resp = "awaytwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Prediction on new data
homefinalPreds <-
  6*homefinalPredstd +
  3*homefinalPredsfg +
  1*homefinalPredsxp +
  2*homefinalPredstp #+
#2*homefinalPredssafe

# homefinalPreds <- posterior_predict(Fit,
#                                     resp = "homescore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
awayfinalFit <- 
  6*awayfinalFittd + 
  3*awayfinalFitfg +
  1*awayfinalFitxp +
  2*awayfinalFittp #+
#2*awayfinalFitsafe
## Fitted
#awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <-
  6*awayfinalPredstd +
  3*awayfinalPredsfg +
  1*awayfinalPredsxp +
  2*awayfinalPredstp #+

spreadNew <- 
  
  
  # Iterate -----
## Update Priors ----
# Helper function to create updated priors
prior_summary(fit20, all = FALSE)
create_updated_priors <- function(post_summary) {
  priors <- empty_prior()
  
  # Fixed effects (coefficients)
  fixed_effects <- grep("^b_", rownames(post_summary), value = TRUE)
  for (param in fixed_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    coef_name <- sub("b_(hometotalTD|homefgmade|homepatmade|hometwoPtConv|awaytotalTD|awayfgmade|awaypatmade|awaytwoPtConv)_", "", param)
    #response <- ifelse(grepl("_homescore_", param), "homescore", "awayscore")
    response <- str_split_i(param, "_", i = 2)
    #priorTemp <- prior(normal(estimate, est_error), class = "b", coef = coef_name, resp = response)
    priorTemp <- do.call("prior",
                         list(prior = call("normal", estimate, est_error),
                              class = "b", 
                              coef = coef_name,
                              resp = response))
    priors <- c(priors, priorTemp)
  }
  
  # Correlations
  correlations <- grep("^cor_", rownames(post_summary), value = TRUE)
  for (param in correlations) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    group_name <- sub("cor_(home_team|away_team)__(.*)", "\\1", param)
    priorTemp <- do.call("prior",
                         list(prior = call("lkj_corr_cholesky", 1),
                              class = "L", 
                              group = group_name))
    priors <- c(priors, priorTemp)
    #priors <- c(priors, prior(lkj_corr_cholesky(1), class = "cor"))  # Retain LKJ prior but acknowledge posterior
  }
  
  # Random effects (standard deviations)
  random_effects <- grep("^sd_", rownames(post_summary), value = TRUE)
  for (param in random_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    group_name <- sub("sd_(home_team|away_team)__(.*)", "\\1", param)
    #response <- ifelse(grepl("awayscore", param), "awayscore", "homescore")
    response <- str_split_i(param, "_", i = 5)
    priorTemp <- do.call("prior",
                         list(prior = call("student_t", 3, estimate, est_error),
                              class = "sd", 
                              coef = "Intercept",
                              group = group_name,
                              resp = response#,
                              #lb = 0
                         )
    )
    priors <- c(priors, priorTemp)
    #priors <- c(priors, prior(student_t(3, estimate, est_error), class = "sd", group = group_name, resp = response))
  }
  
  # Dispersion (shape parameter for negative binomial)
  # shapes <- grep("^shape_", rownames(post_summary), value = TRUE)
  # for (param in shapes) {
  #   mean <- post_summary[param, "Estimate"]
  #   variance <- post_summary[param, "Est.Error"]^2
  #   response <- sub("shape_", "", param)  # Extract response (e.g., homescore or awayscore)
  #   alpha <- (mean^2 / variance)
  #   beta <- variance / mean
  #   priorTemp <- do.call("prior",
  #                        list(prior = call("gamma", alpha, beta),
  #                             class = "shape", 
  #                             resp = response,
  #                             lb = 0))
  #   priors <- c(priors, priorTemp)
  # }
  
  stand_devs <- grep("^sigma_", rownames(post_summary), value = TRUE)
  for (param in stand_devs) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    response <- ifelse(grepl("awayscore", param), "awayscore", "homescore")
    priorTemp <- do.call("prior",
                         list(prior = call("student_t", 3, estimate, est_error),
                              class = "sigma", 
                              resp = response,
                              lb = 0
                         )
    )
    priors <- c(priors, priorTemp)
  }
  
  return(priors)
}

# Create updated priors
updated_priors <- c(create_updated_priors(post_summary = postSum))
updated_priors

## Fit Model ----
# Initialize values
predWeeks <- max(modelData$week)
iterFit <- fit9
homefinalIterFitComb <- NULL
awayfinalIterFitComb <- NULL
homefinalIterPredsComb <- NULL
awayfinalIterPredsComb <- NULL
prePriors <- posterior_summary(fit11)
prePriorCoefs <- prior_summary(fit11)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

### Run loop
for(i in 1:predWeeks){
  predictWeekData <- modelData |>
    filter(week == i)
  
  # nonUnique <- predictWeekData |>
  #   select(where(is.character))
  # 
  # nonUnique <- predictWeekData |>
  #   summarise(
  #     across(where(is.character),
  #            ~length(unique(.x)))
  #   ) |> unlist()
  # removeVars <- names(nonUnique[nonUnique == 1])
  
  # Make Prediction for week 
  homefinalIterPreds <- posterior_predict(iterFit,
                                          resp = "homescore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  awayfinalIterPreds <- posterior_predict(iterFit,
                                          resp = "awayscore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  # Update prediction matrix
  homefinalIterPredsComb <- cbind(homefinalIterPredsComb, homefinalIterPreds)
  awayfinalIterPredsComb <- cbind(awayfinalIterPredsComb, awayfinalIterPreds)
  
  iterData <- modelData |>
    filter(week == i)
  
  iterCoefs <- default_prior(
    formulaFitHome + formulaFitAway + set_rescor(FALSE),
    data = iterData,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    warmup = burn,
    iter = iters,
    chains = chains,
    #prior = updated_priors2,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan",
    drop_unused_levels = TRUE
  )$coef
  
  iterPrePrior <- posterior_summary(iterFit)
  iterPrePriorCoefs <- prior_summary(iterFit)$coef
  
  dropCoefs <- iterPrePriorCoefs[!(iterPrePriorCoefs %in% iterCoefs)]
  newCoefs <- iterCoefs[!(iterCoefs %in% iterPrePriorCoefs)]
  
  updated_priors <- create_updated_priors(post_summary = iterPrePrior)
  updated_priors_keep <- updated_priors |> filter(!(coef %in% dropCoefs))
  updated_priors_new <- old_priors |> filter(coef %in% newCoefs)
  updated_priors2 <- c(updated_priors_keep, updated_priors_new)
  
  # Fit new model
  iterFit <- brm(
    formulaFitHome + formulaFitAway + set_rescor(FALSE),
    data = iterData,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    warmup = burn,
    iter = iters,
    chains = chains,
    prior = updated_priors2,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan"
  )
  
  # Get fitted values for week
  homefinalIterFit <- posterior_predict(iterFit, 
                                        resp = "homescore")
  awayfinalIterFit <- posterior_predict(iterFit, 
                                        resp = "awayscore")
  
  # Update prediction matrix
  homefinalIterFitComb <- cbind(homefinalIterFitComb, homefinalIterFit)
  awayfinalIterFitComb <- cbind(awayfinalIterFitComb, awayfinalIterFit)
  
  print(paste0("Finished Week ", i))
}




## Fit Model 2 ----
# Initialize values
predWeeks <- max(modelData$week)
iterFitBase <- fit8
iterFitBaseB <- fitB8
iterFit <- iterFitBase
iterFitB <- iterFitBaseB
homefinalIterFittdComb2 <- list()
homefinalIterFitfgComb2 <- list()
homefinalIterFitxpComb2 <- list()
homefinalIterFittpComb2 <- list()
homefinalIterFitComb2 <- list()

awayfinalIterFittdComb2 <- list()
awayfinalIterFitfgComb2 <- list()
awayfinalIterFitxpComb2 <- list()
awayfinalIterFittpComb2 <- list()
awayfinalIterFitComb2 <- list()

homefinalIterPredstdComb2 <- list()
homefinalIterPredsfgComb2 <- list()
homefinalIterPredsxpComb2 <- list()
homefinalIterPredstpComb2 <- list()
homefinalIterPredsComb2 <- list()

awayfinalIterPredstdComb2 <- list()
awayfinalIterPredsfgComb2 <- list()
awayfinalIterPredsxpComb2 <- list()
awayfinalIterPredstpComb2 <- list()
awayfinalIterPredsComb2 <- list()

prePriors <- posterior_summary(iterFitBase)
prePriorCoefs <- prior_summary(iterFitBase)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

prePriorsB <- posterior_summary(iterFitBaseB)
prePriorCoefsB <- prior_summary(iterFitBaseB)$coef
old_priorsB <- create_updated_priors(post_summary = prePriorsB)

### Run loop  ----
# took 29 minutes
system.time(
  for(i in 1:predWeeks){
    predictWeekData <- modelData |>
      filter(week == i) |>
      mutate(
        home_totalTD = NA,
        home_fg_made = NA,
        home_pat_made = NA,
        home_twoPtConv = NA,
        away_totalTD = NA,
        away_fg_made = NA,
        away_pat_made = NA,
        away_twoPtConv = NA
      )
    
    # nonUnique <- predictWeekData |>
    #   select(where(is.character))
    # 
    # nonUnique <- predictWeekData |>
    #   summarise(
    #     across(where(is.character),
    #            ~length(unique(.x)))
    #   ) |> unlist()
    # removeVars <- names(nonUnique[nonUnique == 1])
    
    # Make Prediction for week 
    homefinalIterPredstd <- posterior_predict(iterFit,
                                              resp = "hometotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    awayfinalIterPredstd <- posterior_predict(iterFit,
                                              resp = "awaytotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    homefinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "homefgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    awayfinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "awayfgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    homefinalIterPredstdMed <- apply(homefinalIterPredstd, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    awayfinalIterPredstdMed <- apply(awayfinalIterPredstd, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    predictWeekData <- predictWeekData |>
      mutate(
        home_totalTD = homefinalIterPredstdMed,
        away_totalTD = awayfinalIterPredstdMed
      )

    homefinalIterPredsxp <- posterior_predict(iterFitB,
                                              resp = "homepatmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    homefinalIterPredstp <- posterior_predict(iterFitB,
                                              resp = "hometwoPtConv",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    awayfinalIterPredsxp <- posterior_predict(iterFitB,
                                              resp = "awaypatmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    awayfinalIterPredstp <- posterior_predict(iterFitB,
                                              resp = "awaytwoPtConv",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    homefinalIterPreds <- 
      6*homefinalIterPredstd + 
      3*homefinalIterPredsfg +
      1*homefinalIterPredsxp +
      2*homefinalIterPredstp
    
    awayfinalIterPreds <- 
      6*awayfinalIterPredstd + 
      3*awayfinalIterPredsfg +
      1*awayfinalIterPredsxp +
      2*awayfinalIterPredstp
    
    homefinalIterPredsComb2[[i]] <- homefinalIterPreds
    awayfinalIterPredsComb2[[i]] <- awayfinalIterPreds
    
    # Update prediction matrix
    homefinalIterPredstdComb2[[i]] <- homefinalIterPredstd
    homefinalIterPredsfgComb2[[i]] <- homefinalIterPredsfg
    homefinalIterPredsxpComb2[[i]] <- homefinalIterPredsxp
    homefinalIterPredstpComb2[[i]] <- homefinalIterPredstp
    awayfinalIterPredstdComb2[[i]] <- awayfinalIterPredstd
    awayfinalIterPredsfgComb2[[i]] <- awayfinalIterPredsfg
    awayfinalIterPredsxpComb2[[i]] <- awayfinalIterPredsxp
    awayfinalIterPredstpComb2[[i]] <- awayfinalIterPredstp
    
    iterData <- modelData |>
      filter(week %in% 1:i)
    
    iterFit <- update(iterFitBase,
                      newdata = iterData,
                      prior = old_priors,
                      drop_unused_levels = FALSE
    )
    
    iterFitB <- update(iterFitBaseB,
                      newdata = iterData,
                      prior = old_priorsB,
                      drop_unused_levels = FALSE
    )
    
    # iterCoefs <- default_prior(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   #prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan",
    #   drop_unused_levels = TRUE
    # )$coef
    # 
    # iterPrePrior <- posterior_summary(iterFit)
    # iterPrePriorCoefs <- prior_summary(iterFit)$coef
    # 
    # dropCoefs <- iterPrePriorCoefs[!(iterPrePriorCoefs %in% iterCoefs)]
    # newCoefs <- iterCoefs[!(iterCoefs %in% iterPrePriorCoefs)]
    # 
    # updated_priors <- create_updated_priors(post_summary = iterPrePrior)
    # updated_priors_keep <- updated_priors |> filter(!(coef %in% dropCoefs))
    # updated_priors_new <- old_priors |> filter(coef %in% newCoefs)
    # updated_priors2 <- c(updated_priors_keep, updated_priors_new)
    # 
    # # Fit new model
    # iterFit <- brm(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan"
    # )
    
    # Get fitted values for week
    homefinalIterFittd <- posterior_predict(iterFit, resp = "hometotalTD")
    homefinalIterFitfg <- posterior_predict(iterFit, resp = "homefgmade")
    homefinalIterFitxp <- posterior_predict(iterFitB, resp = "homepatmade")
    homefinalIterFittp <- posterior_predict(iterFitB, resp = "hometwoPtConv")
    
    
    awayfinalIterFittd <- posterior_predict(iterFit, resp = "awaytotalTD")
    awayfinalIterFitfg <- posterior_predict(iterFit, resp = "awayfgmade")
    awayfinalIterFitxp <- posterior_predict(iterFitB, resp = "awaypatmade")
    awayfinalIterFittp <- posterior_predict(iterFitB, resp = "awaytwoPtConv")
    
    homefinalIterFit <- 
      6*homefinalIterFittd + 
      3*homefinalIterFitfg +
      1*homefinalIterFitxp +
      2*homefinalIterFittp
    
    awayfinalIterFit <- 
      6*awayfinalIterFittd + 
      3*awayfinalIterFitfg +
      1*awayfinalIterFitxp +
      2*awayfinalIterFittp
    
    homefinalIterFitComb2[[i]] <- homefinalIterFit
    awayfinalIterFitComb2[[i]] <- awayfinalIterFit
    
    # Update prediction matrix
    homefinalIterFittdComb2[[i]] <- homefinalIterFittd
    homefinalIterFitfgComb2[[i]] <- homefinalIterFitfg
    homefinalIterFitxpComb2[[i]] <- homefinalIterFitxp
    homefinalIterFittpComb2[[i]] <- homefinalIterFittp
    awayfinalIterFittdComb2[[i]] <- awayfinalIterFittd
    awayfinalIterFitfgComb2[[i]] <- awayfinalIterFitfg
    awayfinalIterFitxpComb2[[i]] <- awayfinalIterFitxp
    awayfinalIterFittpComb2[[i]] <- awayfinalIterFittp
    
    print(paste0("Finished Week ", i))
  }
)

fit8end <- iterFit
fitB8end <- iterFitB

save(fit8, fitB8, fit8end, fitB8end,
     homefinalIterFittdComb2,
     homefinalIterFitfgComb2,
     homefinalIterFitxpComb2,
     homefinalIterFittpComb2,
     homefinalIterFitComb2,
     
     awayfinalIterFittdComb2,
     awayfinalIterFitfgComb2,
     awayfinalIterFitxpComb2,
     awayfinalIterFittpComb2,
     awayfinalIterFitComb2,
     
     homefinalIterPredstdComb2,
     homefinalIterPredsfgComb2,
     homefinalIterPredsxpComb2,
     homefinalIterPredstpComb2,
     homefinalIterPredsComb2,
     
     awayfinalIterPredstdComb2,
     awayfinalIterPredsfgComb2,
     awayfinalIterPredsxpComb2,
     awayfinalIterPredstpComb2,
     awayfinalIterPredsComb2,
     file = "~/Desktop/NFL Analysis Data/iter data Multi2.RData")

## Diagnostics ----




## Fitted
dim(homefinalIterFitComb2[[1]])

homefinalIterPredstd2 <- do.call(cbind, homefinalIterPredstdComb2)
homefinalIterPredsfg2 <- do.call(cbind, homefinalIterPredsfgComb2)
homefinalIterPredsxp2 <- do.call(cbind, homefinalIterPredsxpComb2)
homefinalIterPredstp2 <- do.call(cbind, homefinalIterPredstpComb2)
awayfinalIterPredstd2 <- do.call(cbind, awayfinalIterPredstdComb2)
awayfinalIterPredsfg2 <- do.call(cbind, awayfinalIterPredsfgComb2)
awayfinalIterPredsxp2 <- do.call(cbind, awayfinalIterPredsxpComb2)
awayfinalIterPredstp2 <- do.call(cbind, awayfinalIterPredstpComb2)

homefinalIterPreds <- 
  7*homefinalIterPredstd2 +
  3*homefinalIterPredsfg2 #+
  1*homefinalIterPredsxp2 +
  2*homefinalIterPredstp2

awayfinalIterPreds <- 
  7*awayfinalIterPredstd2 +
  3*awayfinalIterPredsfg2 #+
  1*awayfinalIterPredsxp2 +
  2*awayfinalIterPredstp2

## PPD Plots
homeIterPPDbarstd <- ppc_bars(y = modelData$home_totalTD, 
                              yrep = homefinalIterPredstd2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD td")) +
  theme_bw()
homeIterPPDbarsfg <- ppc_bars(y = modelData$home_fg_made, 
                              yrep = homefinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD fg")) +
  theme_bw()
homeIterPPDbarsxp <- ppc_bars(y = modelData$home_pat_made, 
                              yrep = homefinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD xp")) +
  theme_bw()
homeIterPPDbarstp <- ppc_bars(y = modelData$home_twoPtConv, 
                              yrep = homefinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD tp")) +
  theme_bw()

awayIterPPDbarstd <- ppc_bars(y = modelData$away_totalTD, 
                              yrep = awayfinalIterPredstd2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD td")) +
  theme_bw()
awayIterPPDbarsfg <- ppc_bars(y = modelData$away_fg_made, 
                              yrep = awayfinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD fg")) +
  theme_bw()
awayIterPPDbarsxp <- ppc_bars(y = modelData$away_pat_made, 
                              yrep = awayfinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD xp")) +
  theme_bw()
awayIterPPDbarstp <- ppc_bars(y = modelData$away_twoPtConv, 
                              yrep = awayfinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD tp")) +
  theme_bw()

homeIterPPDbarstd
homeIterPPDbarsfg
homeIterPPDbarsxp
homeIterPPDbarstp

awayIterPPDbarstd
awayIterPPDbarsfg
awayIterPPDbarsxp
awayIterPPDbarstp



### Home Score
homefinalIterFit <- homefinalIterFitComb2[[15]]
homefinalIterFitMean <- colMeans(homefinalIterFit)
homefinalIterFitMed <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.5)})
homefinalIterFitLCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.025)})
homefinalIterFitUCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalIterPreds <- do.call(cbind, homefinalIterPredsComb2)
homefinalIterPredsMean <- colMeans(homefinalIterPreds)
homefinalIterPredsMed <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalIterPredsLCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalIterPredsUCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score
## Fitted
awayfinalIterFit <- awayfinalIterFitComb2[[15]]
awayfinalIterFitMean <- colMeans(awayfinalIterFit)
awayfinalIterFitMed <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.5)})
awayfinalIterFitLCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.025)})
awayfinalIterFitUCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalIterPreds <- do.call(cbind, awayfinalIterPredsComb2)
awayfinalIterPredsMean <- colMeans(awayfinalIterPreds)
awayfinalIterPredsMed <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalIterPredsLCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalIterPredsUCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predIterMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("home", "away"),
  MAE_fit = c(mean(abs(homefinalIterFitMean - modelData$home_score)),
              mean(abs(awayfinalIterFitMean - modelData$away_score))),
  COV_fit = c(mean(homefinalIterFitLCB < modelData$home_score &  modelData$home_score < homefinalIterFitUCB),
              mean(awayfinalIterFitLCB < modelData$away_score &  modelData$away_score < awayfinalIterFitUCB)),
  MAE_pred = c(mean(abs(homefinalIterPredsMean - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalIterPredsMean - modelData$away_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(homefinalIterPredsMed - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalIterPredsMed - modelData$away_score), na.rm = TRUE)),
  COV_pred = c(mean(homefinalIterPredsLCB < modelData$home_score & modelData$home_score < homefinalIterPredsUCB, na.rm = TRUE),
               mean(awayfinalIterPredsLCB < modelData$away_score & modelData$away_score < awayfinalIterPredsUCB, na.rm = TRUE))
  # home_MAE_fit = mean(abs(homefinalIterFitMean - histModelData$home_score)),
  # home_COV_fit = mean(homefinalIterFitLCB < histModelData$home_score &  histModelData$home_score < homefinalIterFitUCB),
  # away_MAE_fit = mean(abs(awayfinalIterFitMean - histModelData$away_score)),
  # away_COV_fit = mean(awayfinalIterFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalIterFitUCB),
  # home_MAE_pred = mean(abs(homefinalIterPredsMean - modelData$home_score), na.rm = TRUE),
  # home_MAD_pred = mean(abs(homefinalIterPredsMed - modelData$home_score), na.rm = TRUE),
  # home_COV_pred = mean(homefinalIterPredsLCB < modelData$home_score & modelData$home_score < homefinalIterPredsUCB),
  # away_MAE_pred = mean(abs(awayfinalIterPredsMean - modelData$away_score), na.rm = TRUE),
  # away_MAD_pred = mean(abs(awayfinalIterPredsMed - modelData$away_score), na.rm = TRUE),
  # away_COV_pred = mean(awayfinalIterPredsLCB < modelData$away_score & modelData$away_score < awayfinalIterPredsUCB, na.rm = TRUE)
)
predIterMetricsHA


#### Spread ----
FittedIterSpread <- homefinalIterFit - awayfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanSpread <- colMeans(FittedIterSpread)
FittedIterMedSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.5)})
FittedIterLCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.025)})
FittedIterUCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsIterSpread <- homefinalIterPreds - awayfinalIterPreds
# PredsIter <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsIterMeanSpread <- colMeans(PredsIterSpread)
PredsIterMedSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsIterLCBSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsIterUCBSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- modelData1$result
spreadTest <- modelData1$result
predIterMetricsSpread <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Spread", 2),
  MAE_vegas = mean(abs(modelData1$result - modelData1$spread_line)),
  MAE_fit = mean(abs(FittedIterMeanSpread - spreadTrain)),
  MAD_fit = mean(abs(FittedIterMedSpread - spreadTrain)),
  COV_fit = mean(FittedIterLCBSpread < spreadTrain & spreadTrain < FittedIterUCBSpread),
  MAE_pred = mean(abs(PredsIterMeanSpread - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsIterMedSpread - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsIterLCBSpread < spreadTest & spreadTest < PredsIterUCBSpread)
)
predIterMetricsSpread

##### Plot ----
set.seed(52)
spreadPPC <- ppc_dens_overlay(y = modelData$result, 
                              yrep = FittedIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPC

set.seed(52)
spreadPPD <- ppc_dens_overlay(y = modelData$result, 
                              yrep = PredsIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPD

spreadPPDbars <- ppc_bars(y = modelData$result, 
                              yrep = PredsIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPDbars

##### Prob Errors ----
##### Fit ----
spreadLineTrain <- modelData1$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedIterProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- FittedIterSpread[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedIterProbsSpread[, j] <- probs
}
FittedIterBetSpread <- colMeans(FittedIterProbsSpread)
FittedIterBetLogicalSpread <- FittedIterBetSpread > 0.5
FittedIterLogicalSpread <- spreadTrain > spreadLineTrain
FittedIterProbSpread <- mean(FittedIterBetLogicalSpread == FittedIterLogicalSpread, na.rm = TRUE)
FittedIterProbSpread

spreadDataTrain <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedIterMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTrain$spreadCoverSuccess))
sum(!is.na(spreadDataTrain$spreadCoverSuccess))

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

##### Pred ----
spreadLineTest <- modelData1$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsIterProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- PredsIterSpread[, j]
  probs <- fitted > spreadLineTest[j]
  PredsIterProbsSpread[, j] <- probs
}
PredsIterBetSpread <- colMeans(PredsIterProbsSpread)
PredsIterBetLogicalSpread <- PredsIterBetSpread > 0.5
PredsIterLogicalSpread <- spreadTest > spreadLineTest
PredsIterProbSpread <- mean(PredsIterBetLogicalSpread == PredsIterLogicalSpread, na.rm = TRUE)
PredsIterProbSpread

spreadDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob
         #over_odds, over_prob,
         #under_odds, under_prob
         ) |>
  mutate(
    spreadPred = PredsIterMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE,
                            ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTest$spreadCoverSuccess))
sum(!is.na(spreadDataTest$spreadCoverSuccess))

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Total ----
FittedIterTotal <- homefinalIterFit + awayfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanTotal <- colMeans(FittedIterTotal)
FittedIterMedTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.5)})
FittedIterLCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.025)})
FittedIterUCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsIterTotal <- homefinalIterPreds + awayfinalIterPreds
# PredsIter <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsIterMeanTotal <- colMeans(PredsIterTotal)
PredsIterMedTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsIterLCBTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsIterUCBTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

totalTrain <- modelData1$total
totalTest <- modelData1$total
predIterMetricsTotal <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Total", 2),
  MAE_vegas = mean(abs(modelData1$total_line - totalTrain)),
  MAE_fit = mean(abs(FittedIterMeanTotal - totalTrain)),
  MAD_fit = mean(abs(FittedIterMedTotal - totalTrain)),
  COV_fit = mean(FittedIterLCBTotal < totalTrain & totalTrain < FittedIterUCBTotal),
  MAE_pred = mean(abs(PredsIterMeanTotal - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsIterMedTotal - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsIterLCBTotal < totalTest & totalTest < PredsIterUCBTotal)
)
predIterMetricsTotal

##### Plot ----
set.seed(52)
totalPPC <- ppc_dens_overlay(y = modelData1$total, 
                             yrep = FittedIterTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPC

set.seed(52)
totalPPD <- ppc_dens_overlay(y = modelData$total, 
                             yrep = PredsIterTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPD

##### Prob Errors ----
##### Fit ----
totalLineTrain <- modelData1$total_line
#totalTrain <- as.numeric(totalTrainScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

FittedIterProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- FittedIterTotal[, j]
  probs <- fitted > totalLineTrain[j]
  FittedIterProbsTotal[, j] <- probs
}
FittedIterBetTotal <- colMeans(FittedIterProbsTotal)
FittedIterBetLogicalTotal <- FittedIterBetTotal > 0.5
FittedIterLogicalTotal <- totalTrain > totalLineTrain
FittedIterProbTotal <- mean(FittedIterBetLogicalTotal == FittedIterLogicalTotal, na.rm = TRUE)
FittedIterProbTotal

totalDataTrain <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = FittedIterMeanTotal,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = FittedIterBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

##### Pred ----
totalLineTest <- modelData1$total_line
#totalTest <- as.numeric(totalTestScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

PredsIterProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- PredsIterTotal[, j]
  probs <- fitted > totalLineTest[j]
  PredsIterProbsTotal[, j] <- probs
}
PredsIterBetTotal <- colMeans(PredsIterProbsTotal)
PredsIterBetLogicalTotal <- PredsIterBetTotal > 0.5
PredsIterLogicalTotal <- totalTest > totalLineTest
PredsIterProbTotal <- mean(PredsIterBetLogicalTotal == PredsIterLogicalTotal, na.rm = TRUE)
PredsIterProbTotal

totalDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = PredsIterMeanTotal,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = PredsIterBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

successPerfTemp <- bind_rows( 
  bind_cols(
    Response = "Spread",
    spreadSuccessTrain,
    spreadSuccessTest
  ) |> rename_with(~str_remove(.x, "spread"), .cols = contains("spread")),
  bind_cols(
    Response = "Total",
    totalSuccessTrain,
    totalSuccessTest
  ) |> rename_with(~str_remove(.x, "total"), .cols = contains("total"))
)|>
  mutate(
    Fit = paste0("Fit ", fit), .before = 1
  )
successPerfTemp
successPerf <- bind_rows(
  successPerf,
  successPerfTemp
)

ppc_error_scatter_avg(modelData1$result, FittedIterSpread)
ppc_error_scatter_avg(modelData1$result, PredsIterSpread)
ppc_error_scatter_avg(modelData1$total, FittedIterTotal)
ppc_error_scatter_avg(modelData1$total, PredsIterTotal)

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$home_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$home_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$home_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$home_OSRS)

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$week)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, 
                              modelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, 
                              modelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$home_team)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$home_team)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$away_team)





