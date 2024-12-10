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

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2021:2024
gameDataMod <- gameData |> filter(season %in% seasonsMod)
gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
pbpDataMod <- load_pbp(seasons = seasonsMod)
load("./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

rm(gameData, gameDataLong)

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
# dbListTables(con)
# dbDisconnect(con)

pbpDataMod2 <- pbpDataMod |> filter(game_id != "2024_14_GB_DET")
pbpDataMod <- pbpDataMod2


# Aggregate pbp ----
pbpPlayTypes <- pbpDataMod |>
  select(
    play,
    play_type, 
    play_type_nfl,
    pass,
    pass_attempt,
    rush,
    rush_attempt,
    penalty,
    qb_dropback,
    qb_kneel,
    qb_spike,
    qb_scramble,
    penalty_type
  ) |>
  distinct()

pbpPlayTypesView <- pbpPlayTypes |>
  filter(pass == 0, rush == 0, penalty == 1)


## EPA ----
epaOffData <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(play == 1) |> 
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_plays = n(),
    off_epa_sum = sum(epa, na.rm = TRUE),
    off_epa_mean = mean(epa, na.rm = TRUE),
    off_pass_plays = sum(pass == 1),
    off_pass_epa_sum = sum(epa[pass == 1], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[pass == 1], na.rm = TRUE),
    off_rush_plays = sum(rush == 1),
    off_rush_epa_sum = sum(epa[rush == 1], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[rush == 1], na.rm = TRUE),
    off_penalty_plays = sum(pass == 0 & rush == 0 & penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[pass == 0 & rush == 0 & penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[pass == 0 & rush == 0 & penalty == 1], na.rm = TRUE)
  ) |>
  mutate(
    across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
  ) |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  )

epaData <- epaOffData |>
  left_join(
    epaOffData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

epaAvgs <- epaData |> 
  group_by(season, team) |>
  summarise(
    across(contains("epa_mean"),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(contains("mean"),
           ~lag(.x, n = 1, default = 0),
           .names = "{.col}"
    )
  ) |>
  ungroup()

epaData2 <- gameDataLongMod |>
  left_join(
    epaData |> select(game_id, team, opponent, contains("mean"))
  ) |>
  group_by(season, team) |>
  mutate(
    across(contains("mean"),
           ~lag(.x, n = 1, default = NA),
           .names = "{.col}")
  ) |>
  ungroup()
# left_join(
#   epaAvgs,
#   by = join_by(season, team)
# )

epaData3 <- epaData2 |>
  mutate(
    across(contains("mean"),
           ~ifelse(is.na(.x), epaAvgs |> filter(season == season, team == team) |> pull(.x), .x))
  ) |>
  mutate(
    row = row_number()
  ) |>
  # select(
  #   game_id, season, week, team, contains("mean")
  # ) |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value = contains("mean"),
    .period = 4,
    .f = mean,
    .partial = TRUE,
    .align = "right"
  ) |>
  mutate(
    across(c(contains("mean"), -contains("roll")),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  arrange(row) |>
  select(-c(
    row,
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    team_qb_id,
    team_qb_name,
    opponent_qb_id,
    opponent_qb_name,
    referee,
    stadium_id
  ))

#rm(epaData, epaData2, epaOffData, epaAvgs, pbpDataMod, pbpPlayTypes, pbpPlayTypesView)
# mutate(
#   off_epa_mean_feat = ifelse(is.na(off_epa_mean), off_epa_mean, off_epa_mean),
#   off_pass_epa_mean_feat = ifelse(is.na(off_pass_epa_mean), off_pass_epa_mean, off_pass_epa_mean),
#   off_rush_epa_mean_feat = ifelse(is.na(off_rush_epa_mean), off_rush_epa_mean, off_rush_epa_mean),
#   off_penalty_epa_mean_feat = ifelse(is.na(off_penalty_epa_mean), off_penalty_epa_mean, off_penalty_epa_mean),
#   def_epa_mean_feat = ifelse(is.na(def_epa_mean), def_epa_mean, def_epa_mean),
#   def_pass_epa_mean_feat = ifelse(is.na(def_pass_epa_mean), def_pass_epa_mean, def_pass_epa_mean),
#   def_rush_epa_mean_feat = ifelse(is.na(def_rush_epa_mean), def_rush_epa_mean, def_rush_epa_mean),
#   def_penalty_epa_mean_feat = ifelse(is.na(def_penalty_epa_mean), def_penalty_epa_mean, def_penalty_epa_mean)
# )

## SRS ----
srsData <- epaData3 |>
  left_join(
    seasonWeekStandings |>
      select(season, week, team, PFG = team_PPG, PAG = opp_PPG, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  group_by(team) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             lag = ~lag(.x, default = 0)
           ),
           .names = "{.col}")
    # lagSRS = lag(SRS, n = 1),
    # diffSRS = SRS - lagSRS,
  ) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             ewma = ~ifelse(week < 6, movavg(.x, n = 2, "e"), .x)
           ),
           .names = "{.col}")
  ) |>
  ungroup() 

# Model Data ----
modData <- gameDataMod |>
  select(-c(
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    home_qb_id, away_qb_id,
    home_qb_name, away_qb_name,
    referee,
    stadium_id
  )) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  mutate(
    home_epa_cum = home_off_epa_mean_cum + away_def_epa_mean_cum,
    home_epa_roll = home_off_epa_mean_roll_4 + away_def_epa_mean_roll_4,
    home_pass_epa_cum = home_off_pass_epa_mean_cum + away_def_pass_epa_mean_cum,
    home_pass_epa_roll = home_off_pass_epa_mean_roll_4 + away_def_pass_epa_mean_roll_4,
    home_rush_epa_cum = home_off_rush_epa_mean_cum + away_def_rush_epa_mean_cum,
    home_rush_epa_roll = home_off_rush_epa_mean_roll_4 + away_def_rush_epa_mean_roll_4,
    home_penalty_epa_cum = home_off_penalty_epa_mean_cum + away_def_penalty_epa_mean_cum,
    home_penalty_epa_roll = home_off_penalty_epa_mean_roll_4 + away_def_penalty_epa_mean_roll_4,
    away_epa_cum = away_off_epa_mean_cum + home_def_epa_mean_cum,
    away_epa_roll = away_off_epa_mean_roll_4 + home_def_epa_mean_roll_4,
    away_pass_epa_cum = away_off_pass_epa_mean_cum + home_def_pass_epa_mean_cum,
    away_pass_epa_roll = away_off_pass_epa_mean_roll_4 + home_def_pass_epa_mean_roll_4,
    away_rush_epa_cum = away_off_rush_epa_mean_cum + home_def_rush_epa_mean_cum,
    away_rush_epa_roll = away_off_rush_epa_mean_roll_4 + home_def_rush_epa_mean_roll_4,
    away_penalty_epa_cum = away_off_penalty_epa_mean_cum + home_def_penalty_epa_mean_cum,
    away_penalty_epa_roll = away_off_penalty_epa_mean_roll_4 + home_def_penalty_epa_mean_roll_4
    # PFG = home_PFG - away_PAG,
    # PAG = away_PFG - home_PAG,
    # MOV = home_MOV - away_MOV,
    # SOS = home_SOS - away_SOS,
    # SRS = home_SRS - away_SRS,
    # OSRS = home_OSRS - away_DSRS,
    # DSRS = away_DSRS - home_DSRS
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 70, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

rm(epaAvgs, epaData, epaData2, epaData3, epaOffData, srsData, seasonWeekStandings)

# Previous Data ----
histModelData <- modData |> filter(season <= 2023)
modelData <- modData |> filter(season == 2024) |> filter(complete.cases(result))

# Fit historical ----
iters <- 3000
burn <- 1000
chains <- 2
sims <- (iters-burn)*chains


Fit <- brm(
  bf(mvbind(home_score, away_score) ~ 
       home_SRS + away_SRS +
       home_epa_cum + away_epa_cum +
       home_epa_roll + away_epa_roll +
       home_penalty_epa_cum + away_penalty_epa_cum +
       home_rest +
       away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       #surface +
       # (1 | gr(home_team, id = "H")) +
       # (1 | gr(away_team, id = "A")) +
       (1 | mm(home_team, away_team))# + 
       #(1 | season)
  ),
  data = histModelData,
  family = brmsfamily(family = "negbinomial"),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = TRUE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstan"
)

fit <- 1
assign(paste0("fit", fit), Fit)

#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
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
ranef(Fit)

FitR2 <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())
FitR2

## PPC Plot ----
homePPC <- pp_check(Fit, resp = "homescore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

awayPPC <- pp_check(Fit, resp = "awayscore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Away PPC")) +
  theme_bw()

homePPC
awayPPC

#### Home Score ----
## Fitted
homefinalFit <- posterior_predict(Fit, resp = "homescore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- posterior_predict(Fit,
                                    resp = "homescore",
                                    newdata = modelData,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
## Fitted
awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <- posterior_predict(Fit,
                                    resp = "awayscore",
                                    newdata = modelData,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = paste0("Fit", fit),
  home_MAE_fit = mean(abs(homefinalFitMean - histModelData$home_score)),
  home_COV_fit = mean(homefinalFitLCB < histModelData$home_score &  histModelData$home_score < homefinalFitUCB),
  away_MAE_fit = mean(abs(awayfinalFitMean - histModelData$away_score)),
  away_COV_fit = mean(awayfinalFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalFitUCB),
  home_MAE_pred = mean(abs(homefinalPredsMean - modelData$home_score), na.rm = TRUE),
  home_MAD_pred = mean(abs(homefinalPredsMed - modelData$home_score), na.rm = TRUE),
  home_COV_pred = mean(homefinalPredsLCB < modelData$home_score & modelData$home_score < homefinalPredsUCB),
  away_MAE_pred = mean(abs(awayfinalPredsMean - modelData$away_score), na.rm = TRUE),
  away_MAD_pred = mean(abs(awayfinalPredsMed - modelData$away_score), na.rm = TRUE),
  away_COV_pred = mean(awayfinalPredsLCB < modelData$away_score & modelData$away_score < awayfinalPredsUCB, na.rm = TRUE)
)
predMetricsHA


#### Spread ----
Fitted <- homefinalFit - awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMean <- colMeans(Fitted)
FittedMed <- apply(Fitted, 2, function(x){quantile(x, 0.5)})
FittedLCB <- apply(Fitted, 2, function(x){quantile(x, 0.025)})
FittedUCB <- apply(Fitted, 2, function(x){quantile(x, 0.975)})

# Prediction
Preds <- homefinalPreds - awayfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMean <- colMeans(Preds)
PredsMed <- apply(Preds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCB <- apply(Preds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCB <- apply(Preds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- histModelData$result
spreadTest <- modelData$result
predMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_fit = mean(abs(FittedMean - spreadTrain)),
  MAD_fit = mean(abs(FittedMed - spreadTrain)),
  COV_fit = mean(FittedLCB < spreadTrain & spreadTrain < FittedUCB),
  MAE_pred = mean(abs(PredsMean - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMed - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCB < spreadTest & spreadTest < PredsUCB)
)
predMetrics

### Prob Errors ----
#### Fit ----
spreadLineTrain <- histModelData$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbs <- matrix(, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- Fitted[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbs[, j] <- probs
}
FittedBet <- colMeans(FittedProbs)
FittedBetLogical <- FittedBet > 0.5
FittedResultLogical <- spreadTrain > spreadLineTrain
FittedResultProb <- mean(FittedBetLogical == FittedResultLogical, na.rm = TRUE)
FittedResultProb

spreadDataTrain <- histModelData |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMean,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

#### Pred ----
spreadLineTest <- modelData$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbs <- matrix(, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- Preds[, j]
  probs <- fitted > spreadLineTest[j]
  PredsProbs[, j] <- probs
}
PredsBet <- colMeans(PredsProbs)
PredsBetLogical <- PredsBet > 0.5
PredsResultLogical <- spreadTest > spreadLineTest
PredsResultProb <- mean(PredsBetLogical == PredsResultLogical, na.rm = TRUE)
PredsResultProb

spreadDataTest <- modelData |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMean,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest







modelDataTrain <- trainingData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week <= 8) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x)})
  )

modelDataTest <- testData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week == 9) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x,
                             center = attr(modelDataTrain |> pull(x), "scaled:center"),
                             scale = attr(modelDataTrain |> pull(x), "scaled:scale"))})
  )























