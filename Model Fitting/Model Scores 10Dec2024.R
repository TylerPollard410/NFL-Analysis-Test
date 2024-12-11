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
modData <- modData |> filter(!is.na(spread_line))
histModelData <- modData |> filter(season <= 2023)
modelData <- modData |> filter(season == 2024) |> filter(complete.cases(result))

# Fit historical ----
# Family: MV(skew_normal, skew_normal) 
# Links: mu = identity; sigma = identity; alpha = identity
# mu = identity; sigma = identity; alpha = identity 
# Formula: home_score ~ 0 + Intercept + home_PFG + away_PAG + home_SRS + away_SRS + home_off_pass_epa_mean_cum + home_off_rush_epa_mean_cum + away_def_pass_epa_mean_cum + away_def_rush_epa_mean_cum + home_rest + away_rest + location + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 
# away_score ~ 0 + Intercept + away_PFG + home_PAG + home_SRS + away_SRS + away_off_pass_epa_mean_cum + away_off_rush_epa_mean_cum + home_def_pass_epa_mean_cum + home_def_rush_epa_mean_cum + home_rest + away_rest + location + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 

# Family: MV(skew_normal, skew_normal) 
# Links: mu = identity; sigma = identity; alpha = identity
# mu = identity; sigma = identity; alpha = identity 
# Formula: home_score ~ 0 + Intercept + home_OSRS + away_DSRS + home_off_pass_epa_mean_cum + home_off_rush_epa_mean_cum + away_def_pass_epa_mean_cum + away_def_rush_epa_mean_cum + home_rest + away_rest + location + div_game + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 
# away_score ~ 0 + Intercept + home_DSRS + away_OSRS + away_off_pass_epa_mean_cum + away_off_rush_epa_mean_cum + home_def_pass_epa_mean_cum + home_def_rush_epa_mean_cum + home_rest + away_rest + location + div_game + roof + temp + wind + (1 | H | home_team) + (1 | A | away_team) 

iters <- 3000
burn <- 1000
chains <- 2
sims <- (iters-burn)*chains

formulaFitHome <- 
  bf(home_score ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       # home_SRS +
       # away_SRS +
       home_OSRS +
       away_DSRS +
       # home_off_epa_mean_cum +
       home_off_pass_epa_mean_cum +
       home_off_rush_epa_mean_cum +
       # home_off_special_epa_mean_cum +
       # home_off_penalty_epa_mean_cum +
       # away_off_epa_mean_cum +
       # away_off_pass_epa_mean_cum +
       # away_off_rush_epa_mean_cum +
       # away_off_special_epa_mean_cum +
       # away_off_penalty_epa_mean_cum +
       # home_def_epa_mean_cum +
       # home_def_pass_epa_mean_cum +
       # home_def_rush_epa_mean_cum +
       # home_def_special_epa_mean_cum +
       # home_def_penalty_epa_mean_cum +
       # away_def_epa_mean_cum +
       away_def_pass_epa_mean_cum +
       away_def_rush_epa_mean_cum +
       #away_def_special_epa_mean_cum +
       # away_def_penalty_epa_mean_cum +
       home_rest +
       away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|H|home_team) +
       (1|A|away_team)
  ) + negbinomial()
formulaFitAway <- 
  bf(away_score ~ 0 + Intercept +
       # home_PFG + 
       # away_PFG +
       # home_PAG + 
       # away_PAG +
       # home_MOV +
       # away_MOV +
       # home_SRS + 
       # away_SRS +
       home_DSRS +
       away_OSRS +
       # home_off_epa_mean_cum +
       # home_off_pass_epa_mean_cum +
       # home_off_rush_epa_mean_cum +
       # home_off_special_epa_mean_cum +
       # home_off_penalty_epa_mean_cum +
       # away_off_epa_mean_cum +
       away_off_pass_epa_mean_cum +
       away_off_rush_epa_mean_cum +
       #away_off_special_epa_mean_cum +
       # away_off_penalty_epa_mean_cum +
       # home_def_epa_mean_cum +
       home_def_pass_epa_mean_cum +
       home_def_rush_epa_mean_cum +
       #home_def_special_epa_mean_cum +
       # home_def_penalty_epa_mean_cum +
       # away_def_epa_mean_cum +
       # away_def_pass_epa_mean_cum +
       # away_def_rush_epa_mean_cum +
       #away_def_special_epa_mean_cum +
       # away_def_penalty_epa_mean_cum +
       home_rest +
       away_rest +
       location +
       div_game +
       roof +
       temp +
       wind +
       # surface +
       (1|H|home_team) +
       (1|A|away_team)
  ) + negbinomial()


Fit <- brm(
  formulaFitHome + formulaFitAway + set_rescor(FALSE),
  data = histModelData,
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = TRUE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstan"
)

fit <- 10
assign(paste0("fit", fit), Fit)

#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
prior_summary(Fit)
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
print(fixedEff2, digits = 4)
randEff <- ranef(Fit, summary = TRUE)
print(randEff, digits = 4)
VarCorr(Fit)

plot(Fit, ask = FALSE)

postSum <- posterior_summary(Fit)
postSum[grepl("^sd_", rownames(postSum)), ]

FitR2 <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())
FitR2


## PPC Plot ----
homePPC <- pp_check(Fit, resp = "homescore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

homePPCbars <- pp_check(Fit, resp = "homescore", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

awayPPC <- pp_check(Fit, resp = "awayscore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Away PPC")) +
  theme_bw()

awayPPCbars <- pp_check(Fit, resp = "awayscore", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

homePPC
awayPPC
homePPCbars
awayPPCbars

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
predMetricsSpread <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Spread", 2),
  MAE_fit = mean(abs(FittedMeanSpread - spreadTrain)),
  MAD_fit = mean(abs(FittedMedSpread - spreadTrain)),
  COV_fit = mean(FittedLCBSpread < spreadTrain & spreadTrain < FittedUCBSpread),
  MAE_pred = mean(abs(PredsMeanSpread - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedSpread - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBSpread < spreadTest & spreadTest < PredsUCBSpread)
)
predMetricsSpread

##### Plot ----
set.seed(52)
spreadPPC <- ppc_dens_overlay(y = histModelData$result, 
                              yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPC

set.seed(52)
spreadPPD <- ppc_dens_overlay(y = modelData$result, 
                              yrep = PredsSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPD

##### Prob Errors ----
##### Fit ----
spreadLineTrain <- histModelData$spread_line
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

spreadDataTrain <- histModelData |>
  select(game_id, season, week, #game_type,
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
spreadLineTest <- modelData$spread_line
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

spreadDataTest <- modelData |>
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

totalTrain <- histModelData$total
totalTest <- modelData$total
predMetricsTotal <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Total", 2),
  MAE_fit = mean(abs(FittedMeanTotal - totalTrain)),
  MAD_fit = mean(abs(FittedMedTotal - totalTrain)),
  COV_fit = mean(FittedLCBTotal < totalTrain & totalTrain < FittedUCBTotal),
  MAE_pred = mean(abs(PredsMeanTotal - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedTotal - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBTotal < totalTest & totalTest < PredsUCBTotal)
)
predMetricsTotal

##### Plot ----
set.seed(52)
totalPPC <- ppc_dens_overlay(y = histModelData$total, 
                             yrep = FittedTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPC

set.seed(52)
totalPPD <- ppc_dens_overlay(y = modelData$total, 
                             yrep = PredsTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPD

##### Prob Errors ----
##### Fit ----
totalLineTrain <- histModelData$total_line
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

totalDataTrain <- histModelData |>
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
totalLineTest <- modelData$total_line
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

totalDataTest <- modelData |>
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
successPerf


# Iterate -----
predWeeks <- max(modelData$week)

## Fitted
homefinalIterFit <- posterior_predict(Fit, resp = "homescore")
homefinalIterFitMean <- colMeans(homefinalIterFit)
homefinalIterFitMed <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.5)})
homefinalIterFitLCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.025)})
homefinalIterFitUCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalIterPreds <- posterior_predict(Fit,
                                    resp = "homescore",
                                    newdata = modelData,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
homefinalIterPredsMean <- colMeans(homefinalIterPreds)
homefinalIterPredsMed <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalIterPredsLCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalIterPredsUCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
## Fitted
awayfinalIterFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalIterFitMean <- colMeans(awayfinalIterFit)
awayfinalIterFitMed <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.5)})
awayfinalIterFitLCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.025)})
awayfinalIterFitUCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalIterPreds <- posterior_predict(Fit,
                                    resp = "awayscore",
                                    newdata = modelData,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
awayfinalIterPredsMean <- colMeans(awayfinalIterPreds)
awayfinalIterPredsMed <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalIterPredsLCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalIterPredsUCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


fit11 <-  brm(
  formulaFitHome + formulaFitAway + set_rescor(FALSE),
  data = modelData |> filter(week == 1),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  prior = updated_priors,
  normalize = TRUE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstan"
)

for(i in 1:predWeeks){
  modelDataIter <- modelData |> filter(week == i)
}






# Update Priors ----
# Helper function to create updated priors
postSum
post_summary <- postSum
create_updated_priors <- function(post_summary) {
  priors <- list()
  
  # Fixed effects (coefficients)
  fixed_effects <- grep("^b_", rownames(post_summary), value = TRUE)
  for (param in fixed_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    coef_name <- sub("b_(homescore|awayscore)_", "", param)
    response <- ifelse(grepl("_homescore_", param), "homescore", "awayscore")
    priors <- c(priors, prior(normal(estimate, est_error), class = "b", coef = coef_name, resp = response))
  }
  
  # Random effects (standard deviations)
  random_effects <- grep("^sd_", rownames(post_summary), value = TRUE)
  for (param in random_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    group_name <- sub("sd_(home_team|away_team)__(.*)", "\\1", param)
    response <- ifelse(grepl("awayscore", param), "awayscore", "homescore")
    priors <- c(priors, prior(student_t(3, estimate, est_error), class = "sd", group = group_name, resp = response))
  }
  
  # Correlations
  correlations <- grep("^cor_", rownames(post_summary), value = TRUE)
  for (param in correlations) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    priors <- c(priors, prior(lkj_corr_cholesky(1), class = "cor"))  # Retain LKJ prior but acknowledge posterior
  }
  
  # Dispersion (shape parameter for negative binomial)
  shapes <- grep("^shape_", rownames(post_summary), value = TRUE)
  for (param in shapes) {
    # estimate <- post_summary[param, "Estimate"]
    # priors <- c(priors, prior(inv_gamma(0.4, 0.3), class = "shape", resp = sub("shape_", "", param)))
    mean <- post_summary[param, "Estimate"]
    variance <- post_summary[param, "Est.Error"]^2
    response <- sub("shape_", "", param)  # Extract response (e.g., homescore or awayscore)
    alpha <- 2 + (mean^2 / variance)
    beta <- mean * (alpha - 1)
    priors <- c(priors, prior(inv_gamma(alpha, beta), class = "shape", resp = response))
  }
  
  return(priors)
}
priors


# Create updated priors
updated_priors <- c(create_updated_priors(post_summary = postSum))
updated_priors

# Spread ----



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





# Random Forest ----











