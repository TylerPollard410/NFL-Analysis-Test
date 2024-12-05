# Load necessary libraries
library(brms)
library(bayesplot)
library(zoo)
library(tidyr)
library(nflverse)
library(tidyverse)

# Load your dataset
model_data <- modelData

# Inspect the data structure
str(model_data)


# Replace missing temperature and wind values for dome/closed roof games
model_data <- model_data %>%
  mutate(
    temp = ifelse(is.na(temp) & roof %in% c("closed", "dome"), 70, temp),
    wind = ifelse(is.na(wind) & roof %in% c("closed", "dome"), 0, wind)
  ) |>
  filter(!(is.na(spread_line)))

model_data2 <- model_data |>
  mutate(
    home_mov_pass_epa_diff = home_mov_off_pass_epa + away_mov_def_pass_epa,
    home_mov_rush_epa_diff = home_mov_off_rush_epa + away_mov_def_rush_epa,
    away_mov_pass_epa_diff = away_mov_off_pass_epa + home_mov_def_pass_epa,
    away_mov_rush_epa_diff = away_mov_off_rush_epa + home_mov_def_rush_epa,
    SRS_diff = home_SRS - away_SRS
  ) |>
  rename(
    homeScore = home_score,
    awayScore = away_score,
    homeTeam = home_team,
    awayTeam = away_team,
    seasonType = season_type,
    homeRest = home_rest,
    awayRest = away_rest,
    divGame = div_game,
    homeMovPassEpaDiff = home_mov_pass_epa_diff, 
    homeMovOffPassEpa = home_mov_off_pass_epa, 
    awayMovDefPassEpa = away_mov_def_pass_epa,
    homeMovOffRushEpaDiff = home_mov_rush_epa_diff, 
    homeMovOffRushEpa = home_mov_off_rush_epa, 
    awayMovDefRushEpa = away_mov_def_rush_epa,
    awayMovOffPassEpaDiff = away_mov_pass_epa_diff,
    awayMovOffPassEpa = away_mov_off_pass_epa,
    homeMovDefPassEpa = home_mov_def_pass_epa,
    awayMovOffRushEpaDiff = away_mov_rush_epa_diff,
    awayMovOffRushEpa = away_mov_off_rush_epa,
    homeMovDefRushEpa = home_mov_def_rush_epa,
    homeLagSRS = home_lagSRS,
    awayLagSRS = away_lagSRS
  )


# Joint model for home and away scores
formula <-
  bf(
    homeScore ~ (homeLagSRS - awayLagSRS) +
      (1|H|homeTeam) + (1|A|awayTeam) +
      seasonType + location + homeRest + awayRest + divGame +
      roof + surface + temp + wind +
      (homeMovOffPassEpa + awayMovDefPassEpa) +
      (homeMovOffRushEpa + awayMovDefRushEpa)
  ) +
  bf(
    awayScore ~ (awayLagSRS - homeLagSRS) +
      (1|H|homeTeam) + (1|A|awayTeam) +
      seasonType + location + homeRest + awayRest + divGame +
      roof + surface + temp + wind +
      (awayMovOffPassEpa + homeMovDefPassEpa) +
      (awayMovOffRushEpa + homeMovDefRushEpa)
  ) + set_rescor(FALSE)


formula <- bf(
  mvbind(homeScore, awayScore) ~ (homeLagSRS - awayLagSRS) +
    (1 | homeTeam) + (1 | awayTeam) +
    seasonType + 
    location + 
    homeRest + 
    awayRest +
    divGame +
    roof + 
    surface +
    temp + 
    wind +
    (homeMovOffPassEpa + awayMovDefPassEpa) +
    (homeMovOffRushEpa + awayMovDefRushEpa) +
    (awayMovOffPassEpa + homeMovDefPassEpa) +
    (awayMovOffRushEpa + homeMovDefRushEpa)
) #+ set_rescor(TRUE)

# Define priors for SRS-related coefficients
priors <- c(
  prior(normal(0, 10), coef = "homeLagSRS"),
  prior(normal(0, 10), coef = "awayLagSRS")
)

training_data <- filter(model_data2, season < 2024) |>
  select(
    homeScore, 
    awayScore,
    homeLagSRS,
    awayLagSRS,
    homeTeam,
    awayTeam,
  seasonType, 
  location,
  homeRest, 
  awayRest,
  divGame,
  roof,
  surface,
  temp,
  wind,
  homeMovOffPassEpa, awayMovDefPassEpa,
  homeMovOffRushEpa, awayMovDefRushEpa,
  awayMovOffPassEpa, homeMovDefPassEpa,
  awayMovOffRushEpa, homeMovDefRushEpa
  ) |>
  mutate(surface = str_trim(surface))

# Predictive data for Week 1, 2024
initial_week_data <- filter(model_data2, season == 2024 & week == 1)

# Fit the model using Week 1 of 2024
fitCode <- stancode(
  formula,
  data = training_data,
  prior = priors,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000
)
fit <- brm(
  formula,
  data = training_data,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000
)

Fit <- fit
fit <- 1

homePPC <- pp_check(Fit, resp = "homeScore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

awayPPC <- pp_check(Fit, resp = "awayScore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Away PPC")) +
  theme_bw()

homePPC
awayPPC

modelDataTrain <- training_data
modelDataTEst 

## Fitted
homefinalFit <- posterior_predict(Fit, resp = "homeScore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- posterior_predict(Fit, 
                                    resp = "homeScore",
                                    newdata = initial_week_data,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
## Fitted
awayfinalFit <- posterior_predict(Fit, resp = "awayScore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <- posterior_predict(Fit, 
                                    resp = "awayScore",
                                    newdata = initial_week_data,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = paste0("Fit", fit),
  home_MAE_fit = mean(abs(homefinalFitMean - modelDataTrain$home_score)),
  home_COV_fit = mean(homefinalFitLCB < modelDataTrain$home_score &  modelDataTrain$home_score < homefinalFitUCB),
  away_MAE_fit = mean(abs(awayfinalFitMean - modelDataTrain$away_score)),
  away_COV_fit = mean(awayfinalFitLCB < modelDataTrain$away_score &  modelDataTrain$away_score < awayfinalFitUCB),
  home_MAE_pred = mean(abs(homefinalPredsMean - modelDataTest$home_score), na.rm = TRUE),
  home_MAD_pred = mean(abs(homefinalPredsMed - modelDataTest$home_score), na.rm = TRUE),
  home_COV_pred = mean(homefinalPredsLCB < modelDataTest$home_score & modelDataTest$home_score < homefinalPredsUCB),
  away_MAE_pred = mean(abs(awayfinalPredsMean - modelDataTest$away_score), na.rm = TRUE),
  away_MAD_pred = mean(abs(awayfinalPredsMed - modelDataTest$away_score), na.rm = TRUE),
  away_COV_pred = mean(awayfinalPredsLCB < modelDataTest$away_score & modelDataTest$away_score < awayfinalPredsUCB)
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

spreadTrain <- modelDataTrain$result
spreadTest <- modelDataTest$result
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
spreadLineTrain <- modelDataTrain$spread_line
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

spreadDataTrain <- modelDataTrain |>
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
spreadLineTest <- modelDataTest$spread_line
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

spreadDataTest <- modelDataTest |>
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

#### Combined Success ----
spreadSuccess <- bind_cols(
  Fit = paste0("Fit", fit),
  spreadSuccessTrain,
  spreadSuccessTest
)
spreadSuccess




