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
library(glmnet)
library(xgboost)
library(MASS)
library(fitdistrplus)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DiscreteWeibull)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(projpred)
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
# =========================================================
# 1. Data Preparation -----
# =========================================================

# Load necessary libraries (install if not already installed)
if (!require(glmnet)) install.packages("glmnet"); library(glmnet)
if (!require(MASS)) install.packages("MASS"); library(MASS)       # for stepAIC (forward selection)
if (!require(xgboost)) install.packages("xgboost"); library(xgboost)
if (!require(brms)) install.packages("brms"); library(brms)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
class(modData$home_totalTD)

# Data -----
## Clean ----
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

modData2 <- modData |> 
  filter(season >= 2007) |>
  #filter(!is.na(result)) |>
  select(
    game_id,
    season,
    season_type,
    week,
    home_team,
    home_score,
    away_team,
    away_score,
    result,
    spread_line,
    contains("spread"),
    total,
    total_line,
    totalCover,
    contains("over_"),
    contains("under_"),
    winner,
    contains("moneyline"),
    contains("rest"),
    location,
    div_game,
    roof,
    surface,
    temp,
    wind,
    contains("coach"),
    contains("stadium"),
    contains("home"),
    contains("away"),
    -contains("pat_pct"),
    -contains("fg_pct")
  ) |>
  mutate(
    across(c(where(is.character), -game_id),
           ~factor(.x))
  ) |>
  mutate(
    home_totalTDScore = 6*home_totalTD,
    home_fg_madeScore = 3*home_fg_made,
    home_pat_madeScore = home_pat_made,
    home_safetiesScore = 2*home_def_safeties,
    home_twoPtConvScore = 2*home_twoPtConv,
    away_totalTDScore = 6*away_totalTD,
    away_fg_madeScore = 3*away_fg_made,
    away_pat_madeScore = away_pat_made,
    away_safetiesScore = 2*away_def_safeties,
    away_twoPtConvScore = 2*away_twoPtConv,
    home_totalTDScore2 = home_totalTDScore + home_pat_madeScore + home_twoPtConvScore,
    away_totalTDScore2 = away_totalTDScore + away_pat_madeScore + away_twoPtConvScore
  ) |>
  mutate(
    location2 = ifelse(location == "Home", 1, 0),
    .after = location
  ) |>
  mutate(
    location = factor(location, levels = c("Neutral", "Home"))
  )

which(modData$home_totalTD != (modData$home_offTD +
                                 modData$home_special_teams_tds +
                                 modData$home_fumble_recovery_tds +
                                 modData$home_def_tds))
which(modData$away_totalTD != (modData$away_offTD +
                                 modData$away_special_teams_tds +
                                 modData$away_fumble_recovery_tds +
                                 modData$away_def_tds))
which(modData$home_totalTD != (modData$home_pat_att + modData$home_twoPtAtt))
which(modData$away_totalTD != (modData$away_pat_att + modData$away_twoPtAtt))

modData3 <- modData2 |>
  select(
    game_id,
    season,
    season_type,
    week,
    home_team,
    home_score,
    away_team,
    away_score,
    result,
    spread_line,
    contains("spread"),
    total,
    total_line,
    totalCover,
    contains("over_"),
    contains("under_"),
    winner,
    contains("moneyline"),
    contains("rest"),
    location,
    #location2,
    div_game,
    roof,
    surface,
    temp,
    wind,
    contains("coach"),
    contains("stadium"),
    contains("PFG"),
    contains("PAG"),
    contains("MOV"),
    contains("SOS"),
    contains("SRS"),
    contains("OSRS"),
    contains("DSRS"),
    contains("epa"),
    contains("cum"),
    contains("net"),
    contains("roll")
  )
colnames(modData3)

# 1.1 Load modData.csv and inspect the data structure
#modData <- read.csv("modData.csv", stringsAsFactors = FALSE)
dim(modData3)             # Check dimensions (rows and columns)
names(modData3)[1:20]     # Preview first 20 column names
head(modData3[, 1:10])    # Preview first 10 columns of first few rows
summary(modData3$result)  # Summary of the target variable 'result' (point differential)

# 1.2 Handle missing values 
# Identify columns with missing values and decide on imputation or removal
na_counts <- colSums(is.na(modData3))
na_cols <- na_counts[na_counts > 0]
print(na_cols)  # Print columns that have NAs and the count

# For simplicity, drop rows with any NA in key features (or alternatively, impute if needed)
# Assuming critical fields (e.g., result, spread_line, total_line) have no NAs except possibly push outcomes:
modData4 <- modData3 %>% drop_na(result, total, spread_line, total_line)

# If certain predictor columns have NAs and are important, we could impute (e.g., with mean or median)
# Example: modData$some_numeric[is.na(modData$some_numeric)] <- mean(modData$some_numeric, na.rm = TRUE)

# 1.3 Standardize numeric features 
# Identify numeric predictor columns (excluding target and identifier fields)
numeric_cols <- sapply(modData4, is.numeric)
numeric_cols[c("season", "week", #"location2",
               "result", "total", "home_score", "away_score",
               "spread_line", "total_line", 
               "home_spread_odds", "home_spread_prob",
               "away_spread_odds", "away_spread_prob",
               "over_odds", "over_prob",
               "under_odds", "under_prob",
               "home_moneyline", "home_moneyline_prob",
               "away_moneyline", "away_moneyline_prob",
               "overtime")] <- FALSE  # exclude outcome and raw scores if present
numeric_features <- names(modData4)[numeric_cols]
numeric_features 

# Standardize numeric features for modeling (especially important for regularization models like LASSO)
#modData[numeric_features] <- scale(modData[numeric_features])
library(caret)
library(dplyr)
predictorData <- modData4 |>
  select(c(game_id, numeric_features))
predictorData <- predictorData |>
  filter(complete.cases(predictorData))
predictorIDs <- predictorData |> select(game_id)

predictorData <- predictorData |>
  select(-game_id)
modelData <- left_join(
  predictorIDs,
  modData4
)

preProc <- preProcess(
  predictorData,
  method = c("center", "scale")
)
preProc
predictorData <- predict(preProc, predictorData)
modelData <- predict(preProc, modelData)

# 1.4 Feature Engineering 
# Derive matchup features: for example, differences between home and away team stats.
# Here we illustrate creating a few matchup features (e.g., differences in some performance metrics).
# (In practice, you'd want to choose relevant stats like offensive rankings, defensive rankings, etc.)
# For demonstration, suppose modData has columns like 'home_total_yards' and 'away_total_yards':
# if("home_total_yards" %in% names(modData) && "away_total_yards" %in% names(modData)) {
#   modData$yds_diff <- modData$home_total_yards - modData$away_total_yards  # yardage difference
# }
# # Create additional features as needed, e.g., turnover differential, etc., if such columns exist:
# if("home_turnovers" %in% names(modData) && "away_turnovers" %in% names(modData)) {
#   modData$turnover_diff <- modData$away_turnovers - modData$home_turnovers  # positive if away has more TO (bad for away)
# }
# 
# # Rolling averages: incorporate team performance in previous games.
# # For example, compute each team's rolling 3-game average point scored and allowed prior to each game.
# # This requires grouping by team and calculating lagged moving averages.
# library(zoo)
# modData <- modData %>%
#   arrange(season, week) %>%  # ensure data is sorted chronologically
#   group_by(home_team) %>%
#   mutate(home_pts_last3 = rollapply(home_score, width = 3, align = "right", FUN = mean, na.rm = TRUE, fill = NA)) %>%
#   ungroup() %>%
#   group_by(away_team) %>%
#   mutate(away_pts_last3 = rollapply(away_score, width = 3, align = "right", FUN = mean, na.rm = TRUE, fill = NA)) %>%
#   ungroup()
# 
# # Similarly, you could create defensive rolling averages (points allowed last 3 games):
# modData <- modData %>%
#   group_by(home_team) %>%
#   mutate(home_pa_last3 = rollapply(away_score, 3, align="right", FUN=mean, na.rm=TRUE, fill=NA)) %>%
#   ungroup() %>%
#   group_by(away_team) %>%
#   mutate(away_pa_last3 = rollapply(home_score, 3, align="right", FUN=mean, na.rm=TRUE, fill=NA)) %>%
#   ungroup()
# 
# # After feature engineering, remove any rows with NA that resulted from rolling calculations (first few games of season)
# modData <- modData %>% filter(!is.na(home_pts_last3) & !is.na(away_pts_last3))

# 1.5 Feature Selection 
# Using LASSO (L1 regularization via glmnet) to identify important predictors for `result` and `total`.
# Prepare model matrix for predictors (exclude target and identifiers like teams, game_id, etc.)
target_result <- modelData$result
target_total  <- modelData$total  # total points
# predictors <- modData %>% 
#   select(-game_id, -season, -season_type, -week, -home_team, -away_team,
#          -home_score, -away_score, -result, -spread_line, -total_line, -spreadCover, -totalCover) 
X <- model.matrix(~ ., data=predictorData)[, -1]  # create matrix and drop intercept

# Fit LASSO for point differential (result)
lasso_result <- cv.glmnet(X, target_result, alpha = 1, family="gaussian")
best_lambda_res <- lasso_result$lambda.min
coef(lasso_result, s = "lambda.min")  # coefficients at best lambda

# Fit LASSO for total points
lasso_total <- cv.glmnet(X, target_total, alpha = 1, family="gaussian")
best_lambda_tot <- lasso_total$lambda.min
coef(lasso_total, s = "lambda.min")

# Based on LASSO, choose features with non-zero coefficients
coef_res <- coef(lasso_result, s = "lambda.min")
selected_features_res <- row.names(coef_res)[coef_res[,1] != 0][-1]  # drop intercept, list feature names
coef_tot <- coef(lasso_total, s = "lambda.min")
selected_features_tot <- row.names(coef_tot)[coef_tot[,1] != 0][-1]

print("Selected features for result (point differential) via LASSO:")
print(selected_features_res)
print("Selected features for total points via LASSO:")
print(selected_features_tot)

# Additionally, perform forward stepwise selection (using AIC) as a check
# Start with a simple model and add predictors one by one
base_model_res <- lm(result ~ 1, 
                     data = modelData)
full_model_res <- lm(result ~ ., 
                     data = predictorData %>% mutate(result = target_result))
forward_res <- stepAIC(base_model_res, 
                       scope = list(lower=base_model_res,
                                    upper=full_model_res), 
                       direction="forward", 
                       trace=FALSE)
summary(forward_res)$coef  # coefficients of the selected model

base_model_tot <- lm(total ~ 1, 
                     data = modelData)
full_model_tot <- lm(total ~ ., 
                     data = predictorData %>% mutate(total = target_total))
forward_tot <- stepAIC(base_model_tot, 
                       scope = list(lower=base_model_tot,
                                    upper=full_model_tot), 
                       direction="forward",
                       trace=FALSE)
summary(forward_tot)$coef

# Finalize a set of features to use (could combine LASSO and forward selection results)
final_features_res <- union(selected_features_res, names(coef(forward_res))[-1])
final_features_tot <- union(selected_features_tot, names(coef(forward_tot))[-1])
print("Final chosen features for result model:")
print(final_features_res)
print("Final chosen features for total model:")
print(final_features_tot)


# =========================================================
# 2. Modeling Approaches -----
# =========================================================

# Prepare training and testing splits for model evaluation (e.g., use last season as test)
train_data <- modelData %>% filter(season < max(season))   # train on all but last season
test_data  <- modelData %>% filter(season == max(season))  # test on last season

preProcTrain <- preProcess(
  train_data |> select(colnames(predictorData)),
  method = c("center", "scale")
)
preProcTrain
train_data <- predict(preProcTrain, train_data)
test_data <- predict(preProcTrain, test_data)

# Model formulas for direct regression using selected features
form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
form_tot <- as.formula(paste("total ~", paste(final_features_tot, collapse="+")))

## 2.1 Direct Regression Models (GLMs)

# Fit GLM for predicting point differential (result)
glm_result <- glm(form_res, data = train_data, family = gaussian())
summary(glm_result)

# Fit GLM for predicting total points
glm_total <- glm(form_tot, data = train_data, family = gaussian())
summary(glm_total)

# Predict on test set
test_data$pred_result_glm <- predict(glm_result, newdata = test_data)
test_data$pred_total_glm  <- predict(glm_total, newdata = test_data)

# Evaluate GLM performance
# Compute RMSE for result and total predictions
library(Metrics)  # for rmse function (if not installed, use sqrt(mean((pred-actual)^2)))
if(!require(Metrics)) { install.packages("Metrics"); library(Metrics) }
rmse_result_glm <- rmse(test_data$result, test_data$pred_result_glm)
rmse_total_glm  <- rmse(test_data$total, test_data$pred_total_glm)
cat("GLM Result RMSE:", rmse_result_glm, "\n")
cat("GLM Total RMSE:", rmse_total_glm, "\n")

# Betting accuracy for GLM
# Against the spread (ATS) accuracy:
test_data <- test_data %>% 
  mutate( home_cover_actual = spreadCover,       # actual outcome: did home cover the spread?
          home_cover_pred   = pred_result_glm > spread_line)  # model prediction: will home cover?
# For games where spread_line and result are exactly equal (push), exclude from accuracy calc
ats_cases <- test_data %>% filter(result != spread_line)
ats_accuracy_glm <- mean(ats_cases$home_cover_actual == ats_cases$home_cover_pred)
cat("GLM model accuracy against the spread:", round(100*ats_accuracy_glm,2), "%\n")

# Over/Under accuracy:
test_data <- test_data %>%
  mutate( over_actual = totalCover,   # actual: game went over the total line?
          over_pred   = pred_total_glm > total_line )             # model prediction: will go over?
ou_cases <- test_data %>% filter(total != total_line)  # exclude pushes
ou_accuracy_glm <- mean(ou_cases$over_actual == ou_cases$over_pred)
cat("GLM model accuracy on over/under bets:", round(100*ou_accuracy_glm,2), "%\n")


## 2.2 XGBoost Gradient Boosting Models

# Prepare data for XGBoost (need matrix of features and target vector)
# Use the same features selected above for consistency
xgb_features_res <- final_features_res
xgb_features_tot <- final_features_tot

# Training matrices for XGBoost
library(Matrix)
dtrain_res <- xgb.DMatrix(
  data = as.matrix(train_data[, xgb_features_res]), 
  label = train_data$result
)
dtrain_tot <- xgb.DMatrix(
  data = as.matrix(train_data[, xgb_features_tot]), 
  label = train_data$total
)

dtest_res <- xgb.DMatrix(
  data = as.matrix(test_data[, xgb_features_res]), 
  label = test_data$result
)
dtest_tot <- xgb.DMatrix(
  data = as.matrix(test_data[, xgb_features_tot]),
  label = test_data$total
)

# Set up hyperparameter grid for tuning
param_grid <- expand.grid(max_depth = c(3, 5, 7),
                          eta       = c(0.05, 0.1, 0.3),  # learning rate
                          nrounds   = c(100, 200),
                          subsample = 1,
                          colsample_bytree = 1)

# Function to train and evaluate XGBoost with given params (for simplicity, using RMSE on test as evaluation)
evaluate_xgb <- function(max_depth, eta, nrounds, subsample=1, colsample_bytree=1, 
                         dtrain, dtest) {
  params <- list(objective = "reg:squarederror", 
                 max_depth = max_depth,
                 eta = eta,
                 subsample = subsample,
                 colsample_bytree = colsample_bytree)
  model <- xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
  pred <- predict(model, newdata = dtest)
  actual <- getinfo(dtest, "label")
  rmse_val <- sqrt(mean((pred - actual)^2))
  return(list(model=model, rmse=rmse_val))
}

# Tune XGBoost for result prediction
best_rmse_res <- Inf
best_model_res <- NULL
best_params_res <- NULL
for(i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  res <- evaluate_xgb(params$max_depth, params$eta, params$nrounds, 
                      params$subsample, params$colsample_bytree,
                      dtrain = dtrain_res, dtest = dtest_res)
  if(res$rmse < best_rmse_res) {
    best_rmse_res <- res$rmse
    best_model_res <- res$model
    best_params_res <- params
  }
}
cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")

# Tune XGBoost for total prediction
best_rmse_tot <- Inf
best_model_tot <- NULL
best_params_tot <- NULL
for(i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  res <- evaluate_xgb(params$max_depth, params$eta, params$nrounds, 
                      params$subsample, params$colsample_bytree,
                      dtrain = dtrain_tot, dtest = dtest_tot)
  if(res$rmse < best_rmse_tot) {
    best_rmse_tot <- res$rmse
    best_model_tot <- res$model
    best_params_tot <- params
  }
}
cat("Best XGBoost params for total: \n", 
    "max_depth = ", best_params_tot$max_depth, "\n",
    "eta = ", best_params_tot$eta, "\n",
    "nrounds = ", best_params_tot$nrounds, "\n",
    "subsample = ", best_params_tot$subsample, "\n",
    "colsample_bytree = ", best_params_tot$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_tot, "\n")

# Use the best XGBoost models to predict on the test set
test_data$pred_result_xgb <- predict(best_model_res, newdata = dtest_res)
test_data$pred_total_xgb  <- predict(best_model_tot, newdata = dtest_tot)

# Evaluate XGBoost performance
rmse_result_xgb <- sqrt(mean((test_data$pred_result_xgb - test_data$result)^2))
rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb, "\n")
cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
test_data <- test_data %>%
  mutate(home_cover_pred_xgb = pred_result_xgb > spread_line,
         over_pred_xgb       = pred_total_xgb > total_line)
ats_cases <- test_data %>% filter(result != spread_line)
ats_accuracy_xgb <- mean(ats_cases$home_cover_actual == ats_cases$home_cover_pred_xgb)
ou_cases <- test_data %>% filter(total != total_line)
ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb,2), "%\n")
cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")


## 2.3 Bayesian Hierarchical Model (Team Offense/Defense Strength)

# Prepare data for Bayesian model of team offense/defense using brms.
# We will model points scored by each team in a game using team-specific offense and defense effects.
# Create a long format dataset with one row per team performance in each game.
bayes_data <- modelData %>%
  select(game_id, home_team, away_team, home_score, away_score) %>%
  # Create two rows per game: one for home scoring, one for away scoring
  pivot_longer(cols = c(home_score, away_score), 
               names_to = "side",
               values_to = "points") %>%
  mutate(team = ifelse(side == "home_score", home_team, away_team),
         opponent = ifelse(side == "home_score", away_team, home_team),
         is_home = ifelse(side == "home_score", 1, 0)) %>%
  select(game_id, team, opponent, is_home, points)

bayes_data <- modelData |>
  clean_homeaway(invert = c("result", "spread_line")) |>
  mutate(
    is_home = ifelse(location == "home", 1, 0),
    .after = location
  ) |>
  select(game_id,
         team,
         opponent, 
         is_home, 
         points = team_score)

# Define a Bayesian hierarchical model:
# points ~ home_advantage * is_home + offense_ability(team) + defense_ability(opponent)
# We use (1|team) and (1|opponent) to give each team two random effects (offense & defense).
# Negative binomial family is appropriate for count-like scoring data to allow overdispersion.
bayes_formula <- bf(points ~ is_home + (1 | team) + (1 | opponent))
# Set weakly informative priors for stability (optional)
bayes_priors <- c(set_prior("normal(0,5)", class="b"),            # for fixed effects (home advantage)
                  set_prior("normal(0,2)", class="sd", group="team"),     # team offense prior
                  set_prior("normal(0,2)", class="sd", group="opponent")) # team defense prior

# Fit the Bayesian model using brms (this can be time-consuming; adjust iterations as needed)
bayes_model <- brm(bayes_formula, data = bayes_data, family = negbinomial(), 
                   prior = bayes_priors, iter = 2000, chains = 4, cores = 4)
summary(bayes_model)          # Summary of posterior estimates
ranef(bayes_model)$team[1:5, ,] # Example offense strength for first 5 teams
ranef(bayes_model)$opponent[1:5, ,] # Example defense strength for first 5 teams

# Posterior predictive check (simulate some games from the model and compare to actual)
pp_check(bayes_model, ndraws=100)  # (This will plot predictive vs actual distributions)

# Evaluate accuracy of Bayesian model on test data (last season)
# We can simulate outcomes for each test game to see probabilities of covering spread/total.
test_games <- test_data %>% 
  select(home_team, away_team, 
         spread_line, total_line, 
         result, total)
# Define a function to simulate one game outcome using the fitted Bayesian model
simulate_game <- function(home, away, n_sim = 1000) {
  # Simulate points for home and away using posterior predictive distribution
  # Create new data for the model: one row for home scoring, one for away scoring
  newdata <- data.frame(team = c(home, away),
                        opponent = c(away, home),
                        is_home = c(1, 0))
  sims <- posterior_predict(bayes_model, 
                            newdata = newdata, 
                            ndraws = n_sim)
  # sims will be a matrix:  n_sim x 2 (first column = home points, second = away points)
  home_pts_sim <- sims[,1]
  away_pts_sim <- sims[,2]
  # Compute probabilities of events:
  prob_home_cover = mean((home_pts_sim - away_pts_sim) > newdata$spread_line[1])  # home cover spread
  prob_over      = mean((home_pts_sim + away_pts_sim) > newdata$total_line[1])    # game total over
  expected_diff  = mean(home_pts_sim - away_pts_sim)
  expected_total = mean(home_pts_sim + away_pts_sim)
  return(c(prob_home_cover = prob_home_cover, prob_over = prob_over,
           exp_diff = expected_diff, exp_total = expected_total))
}

# Apply simulation to each test game to get model probabilities
bayes_preds <- t(apply(test_games, 1, function(x) {
  home <- x["home_team"]; away <- x["away_team"]
  spread <- as.numeric(x["spread_line"]); total <- as.numeric(x["total_line"])
  # Use 500 simulations for speed (adjustable for accuracy)
  sim <- simulate_game(home, away, n_sim=500)
  return(sim)
}))
bayes_preds <- as.data.frame(bayes_preds)
test_games <- cbind(test_games, bayes_preds)

# Determine betting picks from Bayesian model:
test_games <- test_games %>%
  mutate(pick_spread = ifelse(exp_diff > spread_line, 
                              paste("Bet", home_team, "to cover"), 
                              paste("Bet", away_team, "to cover")),
         pick_total  = ifelse(exp_total > total_line, 
                              "Bet Over", 
                              "Bet Under"),
         correct_spread = (result > spread_line) == (exp_diff > spread_line),
         correct_total  = (total > total_line) == (exp_total > total_line))

# Accuracy of Bayesian model's picks on test data
bayes_ats_accuracy <- mean(test_games$correct_spread, na.rm = TRUE)
bayes_ou_accuracy  <- mean(test_games$correct_total, na.rm = TRUE)
cat("Bayesian model accuracy against the spread:", round(100*bayes_ats_accuracy,2), "%\n")
cat("Bayesian model accuracy on over/under:", round(100*bayes_ou_accuracy,2), "%\n")


# =========================================================
# 3. Model Evaluation and Comparison -----
# =========================================================

# Consolidate performance metrics for each model type
performance <- data.frame(
  Model = c("GLM", "XGBoost", "Bayesian"),
  RMSE_Result = c(rmse_result_glm, rmse_result_xgb, NA),   # (Bayesian RMSE can be approximated via expected values)
  RMSE_Total  = c(rmse_total_glm, rmse_total_xgb, NA),
  ATS_Accuracy = c(ats_accuracy_glm, ats_accuracy_xgb, bayes_ats_accuracy),
  OverUnder_Accuracy = c(ou_accuracy_glm, ou_accuracy_xgb, bayes_ou_accuracy)
)
print(performance)

# Compare predictions vs Vegas lines on the test set for each model
# For example, count how many times each model would have made a different pick than Vegas (implying a betting opportunity).
# (Assume Vegas "pick" is just the line itself; a model differs if it predicts opposite side or if difference exceeds some threshold)
test_data <- test_data %>%
  mutate(vegas_home_cover = spread_line < 0,  # if spread_line < 0, home was favored (vegas expects home to cover), if >0, vegas expects away to cover
         glm_pick_cover   = pred_result_glm > 0,    # model thinks home will win (positive margin -> home wins)
         xgb_pick_cover   = pred_result_xgb > 0,
         bayes_pick_cover = bayes_preds$exp_diff > 0)
diff_picks_glm = mean(test_data$glm_pick_cover != test_data$vegas_home_cover)
diff_picks_xgb = mean(test_data$xgb_pick_cover != test_data$vegas_home_cover)
diff_picks_bayes = mean(test_data$bayes_pick_cover != test_data$vegas_home_cover)
cat("Percentage of games where model disagrees with Vegas favorite (GLM):", round(100*diff_picks_glm,2), "%\n")
cat("Percentage of games where model disagrees with Vegas favorite (XGBoost):", round(100*diff_picks_xgb,2), "%\n")
cat("Percentage of games where model disagrees with Vegas favorite (Bayesian):", round(100*diff_picks_bayes,2), "%\n")

# Implement a betting threshold strategy:
# Only bet when the model's predicted spread differs from the Vegas line by at least a certain margin (e.g., 3 points),
# and when predicted total differs from Vegas total by at least a margin (e.g., 3 points).
threshold_spread <- 3
threshold_total  <- 3
test_data <- test_data %>%
  mutate(glm_strong_spread = abs(pred_result_glm - spread_line) >= threshold_spread,
         xgb_strong_spread = abs(pred_result_xgb - spread_line) >= threshold_spread,
         bayes_strong_spread = abs(bayes_preds$exp_diff - spread_line) >= threshold_spread,
         glm_strong_total = abs(pred_total_glm - total_line) >= threshold_total,
         xgb_strong_total = abs(pred_total_xgb - total_line) >= threshold_total,
         bayes_strong_total = abs(bayes_preds$exp_total - total_line) >= threshold_total)

# Calculate betting accuracy when using threshold strategy (only consider games where strong difference exists)
glm_thresh_accuracy_spread <- mean(test_data$home_cover_actual[test_data$glm_strong_spread] == test_data$home_cover_pred[test_data$glm_strong_spread], na.rm=TRUE)
xgb_thresh_accuracy_spread <- mean(test_data$home_cover_actual[test_data$xgb_strong_spread] == test_data$home_cover_pred_xgb[test_data$xgb_strong_spread], na.rm=TRUE)
bayes_thresh_accuracy_spread <- mean(test_data$home_cover_actual[test_data$bayes_strong_spread] == (bayes_preds$exp_diff > test_data$spread_line)[test_data$bayes_strong_spread], na.rm=TRUE)
cat("ATS accuracy with threshold (GLM):", round(100*glm_thresh_accuracy_spread,2), "% on", sum(test_data$glm_strong_spread), "bets\n")
cat("ATS accuracy with threshold (XGBoost):", round(100*xgb_thresh_accuracy_spread,2), "% on", sum(test_data$xgb_strong_spread), "bets\n")
cat("ATS accuracy with threshold (Bayesian):", round(100*bayes_thresh_accuracy_spread,2), "% on", sum(test_data$bayes_strong_spread), "bets\n")

# Similar for Over/Under:
glm_thresh_accuracy_ou <- mean(test_data$over_actual[test_data$glm_strong_total] == test_data$over_pred[test_data$glm_strong_total], na.rm=TRUE)
xgb_thresh_accuracy_ou <- mean(test_data$over_actual[test_data$xgb_strong_total] == test_data$over_pred_xgb[test_data$xgb_strong_total], na.rm=TRUE)
bayes_thresh_accuracy_ou <- mean(test_data$over_actual[test_data$bayes_strong_total] == (bayes_preds$exp_total > test_data$total_line)[test_data$bayes_strong_total], na.rm=TRUE)
cat("Over/Under accuracy with threshold (GLM):", round(100*glm_thresh_accuracy_ou,2), "% on", sum(test_data$glm_strong_total), "bets\n")
cat("Over/Under accuracy with threshold (XGBoost):", round(100*xgb_thresh_accuracy_ou,2), "% on", sum(test_data$xgb_strong_total), "bets\n")
cat("Over/Under accuracy with threshold (Bayesian):", round(100*bayes_thresh_accuracy_ou,2), "% on", sum(test_data$bayes_strong_total), "bets\n")

performance$thresh_accuracy_spread <- c(glm_thresh_accuracy_spread,
                                        xgb_thresh_accuracy_spread,
                                        bayes_thresh_accuracy_spread)

# =========================================================
# 4. Automated Weekly Updates ----
# =========================================================

# Define a process to update models weekly with new games.
# Assume we get new data for each week (e.g., from an API or manual entry), in a data frame 'new_games'.
# For demonstration, let's say 'new_games' is structured like modData (with same columns) for the latest week.

update_models_weekly <- function(new_games) {
  # 4.1 Append new data to the master dataset
  modData <<- bind_rows(modData, new_games)
  
  # 4.2 Recompute or update feature engineering for the new data (e.g., rolling averages)
  # For simplicity, recompute rolling averages just for the team(s) involved in new_games:
  teams_updated <- unique(c(new_games$home_team, new_games$away_team))
  for(team in teams_updated) {
    # Filter games of this team
    team_home_games <- modData %>% filter(home_team == team) %>% arrange(season, week)
    team_away_games <- modData %>% filter(away_team == team) %>% arrange(season, week)
    # Recalculate last3 for those games (could optimize but doing simple way here)
    modData$home_pts_last3[modData$home_team == team] <- rollapply(team_home_games$home_score, 3, mean, align="right", fill=NA)
    modData$home_pa_last3[modData$home_team == team]  <- rollapply(team_home_games$away_score, 3, mean, align="right", fill=NA)
    modData$away_pts_last3[modData$away_team == team] <- rollapply(team_away_games$away_score, 3, mean, align="right", fill=NA)
    modData$away_pa_last3[modData$away_team == team]  <- rollapply(team_away_games$home_score, 3, mean, align="right", fill=NA)
  }
  # Remove any NA introduced for first games of the season for these teams (if needed).
  modData <<- modData
  
  # 4.3 Retrain or update XGBoost models with new data
  dtrain_res <<- xgb.DMatrix(data = as.matrix(modData[, xgb_features_res]), label = modData$result)
  dtrain_tot <<- xgb.DMatrix(data = as.matrix(modData[, xgb_features_tot]), label = modData$home_score + modData$away_score)
  # (For speed, one might update an existing model with xgb.train using warm start, but here we retrain fully)
  best_model_res <<- xgb.train(params = list(objective="reg:squarederror", max_depth=best_params_res$max_depth,
                                             eta=best_params_res$eta, subsample=best_params_res$subsample,
                                             colsample_bytree=best_params_res$colsample_bytree),
                               data = dtrain_res, nrounds = best_params_res$nrounds, verbose = 0)
  best_model_tot <<- xgb.train(params = list(objective="reg:squarederror", max_depth=best_params_tot$max_depth,
                                             eta=best_params_tot$eta),
                               data = dtrain_tot, nrounds = best_params_tot$nrounds, verbose = 0)
  
  # 4.4 Update Bayesian model priors with new data (optional advanced step)
  # We can use the previous posterior as new priors. Here we illustrate refitting the model quickly:
  bayes_model <<- update(bayes_model, newdata = bayes_data, cores = 4, chains = 4, iter = 1000, refresh = 0)
  
  # 4.5 (Optional) Refit simple GLMs if needed (though new data likely small impact)
  glm_result <<- update(glm_result, data = modData)
  glm_total <<- update(glm_total, data = modData)
  
  cat("Models updated with latest week of data.\n")
}

# Usage example (assuming new_games dataframe exists for current week):
# update_models_weekly(new_games)


# =========================================================
# 5. Output and Deployment -----
# =========================================================

# Suppose we have a set of upcoming games (without results, just teams and Vegas lines) for which we want predictions.
# Create a data frame 'future_games' with columns: home_team, away_team, spread_line, total_line.
# We'll generate betting recommendations for these future games.

# For demonstration, let's use the test_games we had as "future" games (ignoring their actual result).
future_games <- test_games %>% select(home_team, away_team, spread_line, total_line) %>% slice(1:5)
# In practice, replace the above with real upcoming schedule data.

# Prepare features for future_games (e.g., attach rolling averages and other predictors from modData)
future_games <- future_games %>%
  # join any team-specific features or stats needed from modData, e.g., last3 averages from the latest data
  left_join(modData %>% select(home_team, home_pts_last3, home_pa_last3) %>% 
              distinct(home_team, .keep_all=TRUE),
            by = c("home_team")) %>%
  left_join(modData %>% select(away_team, away_pts_last3, away_pa_last3) %>% 
              distinct(away_team, .keep_all=TRUE),
            by = c("away_team"))

# Predict result and total for future games using each model
future_games$pred_result_glm  <- predict(glm_result, newdata = future_games)
future_games$pred_total_glm   <- predict(glm_total, newdata = future_games)
future_games$pred_result_xgb  <- predict(best_model_res, newdata = as.matrix(future_games[, xgb_features_res]))
future_games$pred_total_xgb   <- predict(best_model_tot, newdata = as.matrix(future_games[, xgb_features_tot]))

# Bayesian model predictions: use expected values (posterior predictive mean) via simulation
bayes_preds_future <- t(apply(future_games, 1, function(x) {
  sim <- simulate_game(x["home_team"], x["away_team"], n_sim=1000)
  return(sim)
}))
bayes_preds_future <- as.data.frame(bayes_preds_future)
future_games$pred_result_bayes <- bayes_preds_future$exp_diff
future_games$pred_total_bayes  <- bayes_preds_future$exp_total

# Generate betting recommendations based on each model's predictions
# We will also incorporate a betting threshold: only recommend a bet if the predicted difference vs line is significant
threshold <- 1.5  # for example, require >1.5 point difference for a recommendation

recommendations <- future_games %>%
  rowwise() %>%
  do({
    home <- .$home_team; away <- .$away_team
    spread <- .$spread_line; total_line <- .$total_line
    recs <- list(game = paste(away, "at", home))
    # Spread pick using an ensemble approach: e.g., majority vote of models or Bayesian model
    pred_margin <- .$pred_result_bayes  # using Bayesian expected margin as primary
    if(abs(pred_margin - spread) < threshold) {
      recs$SpreadPick <- "No bet (model too close to line)"
    } else if(pred_margin > spread) {
      recs$SpreadPick <- paste("Bet", home, "to cover", spread)
    } else {
      recs$SpreadPick <- paste("Bet", away, "to cover +", abs(spread))
    }
    # Over/Under pick
    pred_tot <- .$pred_total_bayes  # using Bayesian expected total
    if(abs(pred_tot - total_line) < threshold) {
      recs$TotalPick <- "No bet (too close to line)"
    } else if(pred_tot > total_line) {
      recs$TotalPick <- "Bet Over"
    } else {
      recs$TotalPick <- "Bet Under"
    }
    recs
  }) %>% bind_rows()

# View the recommendations
print(recommendations)

# Save recommendations to CSV
write.csv(recommendations, "Betting_Recommendations.csv", row.names = FALSE)
cat("Betting recommendations saved to Betting_Recommendations.csv\n")

# Print summary statistics of the recommendation set
num_games <- nrow(future_games)
num_spread_bets <- sum(recommendations$SpreadPick != "No bet (model too close to line)")
num_total_bets  <- sum(recommendations$TotalPick != "No bet (too close to line)")
cat("Out of", num_games, "upcoming games, recommended spread bets on", num_spread_bets, 
    "games and total (over/under) bets on", num_total_bets, "games.\n")


# 6 Using Caret ----
predictorData <- modData4 |>
  select(c(game_id, numeric_features))
predictorData <- predictorData |>
  filter(complete.cases(predictorData))
predictorIDs <- predictorData |> select(game_id)

predictorData <- predictorData |>
  select(-game_id)
modelData <- left_join(
  predictorIDs,
  modData4
)

training <- modelData |>
  filter(season < 2024) |>
  select(colnames(predictorData))

training_linCombos <- findLinearCombos(training)

training <- training[, -training_linCombos$remove]

preProc <- preProcess(
  training,
  method = c("center", "scale", "corr")
)
preProc
training2 <- predict(preProc, training)
training_data <- predict(preProc, modelData |> filter(season < 2024))
testing_data <- predict(preProc, modelData |> filter(season >= 2024))

fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10)

## result ----
modelLookup("xgbTree")

training_data_res <- training_data |>
  select(
    result,
    colnames(training2)
  )
testing_data_res <- testing_data |>
  select(
    result,
    colnames(training2)
  )

set.seed(52)
system.time(
  xgbFit_res <- train(
    result ~ ., 
    data = training_data_res, 
    method = "xgbTree", 
    trControl = fitControl
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    #tuneGrid = gbmGrid
  )
)
xgbFit_res
summary(xgbFit_res)

xgbFit_res_varImp <- varImp(xgbFit_res, scale = FALSE)
plot(xgbFit_res_varImp, top = 40)

xgbFit_res_best_tune <- xgbFit_res$bestTune
xgbFit_res_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res$results$RMSE), "\n")

pred_result_xgb <- predict(xgbFit_res,
                           newdata = testing_data)
postResample(pred_result_xgb,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb <- pred_result_xgb

# Evaluate XGBoost performance
rmse_result_xgb <- sqrt(mean((testing_data$pred_result_xgb - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb = pred_result_xgb > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb <- mean(ats_cases$home_cover_actual == ats_cases$home_cover_pred_xgb)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

## result 2 ----
param_grid <- expand.grid(max_depth = c(3, 5, 7),
                          eta       = c(0.05, 0.1, 0.3),  # learning rate
                          nrounds   = c(100, 200),
                          gamma = 0,
                          min_child_weight = 1,
                          subsample = 1,
                          colsample_bytree = 1)
form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
findLinearCombos(training_data_res)

set.seed(52)
system.time(
  xgbFit_res2 <- train(
    form_res, 
    data = training_data_res, 
    method = "xgbTree", 
    trControl = fitControl,
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    tuneGrid = param_grid
  )
)
xgbFit_res2

xgbFit_res2_best_tune <- xgbFit_res2$bestTune
xgbFit_res2_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res2_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res2_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res2_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res2_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res2_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res2$results$RMSE), "\n")

pred_result_xgb2 <- predict(xgbFit_res2,
                            newdata = testing_data)
postResample(pred_result_xgb2,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb2 <- pred_result_xgb2

# Evaluate XGBoost performance
rmse_result_xgb2 <- sqrt(mean((testing_data$pred_result_xgb2 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb2, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb2 = pred_result_xgb2 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases2 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb2 <- mean(ats_cases2$home_cover_actual == ats_cases2$home_cover_pred_xgb2)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb2,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")


## result 3 ----
#param_grid <- expand.grid(max_depth = c(3, 5, 7),
# eta       = c(0.05, 0.1, 0.3),  # learning rate
# nrounds   = c(100, 200),
# gamma = 0,
# min_child_weight = 1,
# subsample = 1,
# colsample_bytree = 1)
#form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
training_data_res_linCombos <- findLinearCombos(training_data_res)

training_data_res2 <- training_data_res[, -training_data_res_linCombos$remove]

set.seed(52)
system.time(
  xgbFit_res3 <- train(
    result ~ ., 
    data = training_data_res2, 
    method = "xgbTree", 
    trControl = fitControl
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    #tuneGrid = param_grid
  )
)
xgbFit_res3

xgbFit_res3_best_tune <- xgbFit_res3$bestTune
xgbFit_res3_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res3_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res3_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res3_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res3_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res3_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res3$results$RMSE), "\n")

pred_result_xgb3 <- predict(xgbFit_res3,
                            newdata = testing_data)
postResample(pred_result_xgb3,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb3 <- pred_result_xgb3

# Evaluate XGBoost performance
rmse_result_xgb3 <- sqrt(mean((testing_data$pred_result_xgb3 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb3, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb3 = pred_result_xgb3 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases3 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb3 <- mean(ats_cases3$home_cover_actual == ats_cases3$home_cover_pred_xgb3)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb3,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

xgbFit_res3_varImp <- varImp(xgbFit_res3)
xgbFit_res3_varImp$importance[30]

plot(xgbFit_res3)

cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")


## result 4 ----
param_grid4 <- expand.grid(max_depth = c(1,2),
                          eta       = c(0.025, 0.05, 0.1),  # learning rate
                          nrounds   = c(10, 25, 50),
                          gamma = 0,
                          min_child_weight = 1,
                          subsample = 1,
                          colsample_bytree = c(0.4, 0.7, 1))
#form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
training_data_res_linCombos <- findLinearCombos(training_data_res)

training_data_res2 <- training_data_res[, -training_data_res_linCombos$remove]

set.seed(52)
system.time(
  xgbFit_res4 <- train(
    result ~ ., 
    data = training_data_res2, 
    method = "xgbTree", 
    trControl = fitControl,
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    tuneGrid = param_grid4
  )
)
xgbFit_res4

xgbFit_res4_best_tune <- xgbFit_res4$bestTune
xgbFit_res4_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res4_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res4_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res4_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res4_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res4_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res4$results$RMSE), "\n")

pred_result_xgb4 <- predict(xgbFit_res4,
                            newdata = testing_data)
postResample(pred_result_xgb4,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb4 <- pred_result_xgb4

# Evaluate XGBoost performance
rmse_result_xgb4 <- sqrt(mean((testing_data$pred_result_xgb4 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb4, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb4 = pred_result_xgb4 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases4 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb4 <- mean(ats_cases4$home_cover_actual == ats_cases4$home_cover_pred_xgb4)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb4,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

xgbFit_res4_varImp <- varImp(xgbFit_res4)
xgbFit_res4_varImp$importance |> filter(Overall > 0) |> nrow()

xgbFit_res4_vars <- xgbFit_res4_varImp$importance |>
  filter(Overall > 0) |>
  row.names()

plot(xgbFit_res4_varImp, top = 55)
plot(xgbFit_res4)

cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")

## result 5 ----
param_grid5 <- expand.grid(max_depth = c(1,2,3),
                           eta       = c(0.05, 0.1, 0.2),  # learning rate
                           nrounds   = c(50, 100, 200),
                           gamma = 0,
                           min_child_weight = 1,
                           subsample = 1,
                           colsample_bytree = 1)
#form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
training_data_res_linCombos <- findLinearCombos(training_data_res)

training_data_res2 <- training_data_res[, -training_data_res_linCombos$remove]

set.seed(52)
system.time(
  xgbFit_res5 <- train(
    result ~ ., 
    data = training_data_res2, 
    method = "xgbTree", 
    trControl = fitControl,
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    tuneGrid = param_grid5
  )
)
xgbFit_res5

xgbFit_res5_best_tune <- xgbFit_res5$bestTune
xgbFit_res5_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res5_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res5_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res5_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res5_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res5_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res5$results$RMSE), "\n")

pred_result_xgb5 <- predict(xgbFit_res5,
                            newdata = testing_data)
postResample(pred_result_xgb5,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb5 <- pred_result_xgb5

# Evaluate XGBoost performance
rmse_result_xgb5 <- sqrt(mean((testing_data$pred_result_xgb5 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb5, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb5 = pred_result_xgb5 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases5 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb5 <- mean(ats_cases5$home_cover_actual == ats_cases5$home_cover_pred_xgb5)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb5,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

xgbFit_res5_varImp <- varImp(xgbFit_res5)
xgbFit_res5_varImp$importance |> filter(Overall > 0) #|> nrow()

plot(xgbFit_res5)

cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")

## result 6 ----
param_grid5 <- expand.grid(max_depth = c(1,2,3),
                           eta       = c(0.05, 0.1, 0.2),  # learning rate
                           nrounds   = c(50, 100, 200),
                           gamma = 0,
                           min_child_weight = 1,
                           subsample = 1,
                           colsample_bytree = 1)
#form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
training_data_res_linCombos <- findLinearCombos(training_data_res)

training_data_res2 <- training_data_res[, -training_data_res_linCombos$remove]

training_data_res3 <- training_data_res2 |>
  select(result, xgbFit_res4_vars)

set.seed(52)
system.time(
  xgbFit_res6 <- train(
    result ~ ., 
    data = training_data_res3, 
    method = "xgbTree", 
    trControl = fitControl,
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    tuneGrid = param_grid4
  )
)
xgbFit_res6

xgbFit_res6_best_tune <- xgbFit_res6$bestTune
xgbFit_res6_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res6_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res6_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res6_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res6_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res6_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res6$results$RMSE), "\n")

pred_result_xgb6 <- predict(xgbFit_res6,
                            newdata = testing_data)
postResample(pred_result_xgb6,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb6 <- pred_result_xgb6

# Evaluate XGBoost performance
rmse_result_xgb6 <- sqrt(mean((testing_data$pred_result_xgb6 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb6, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb6 = pred_result_xgb6 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases6 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb6 <- mean(ats_cases6$home_cover_actual == ats_cases6$home_cover_pred_xgb6)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb6,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

xgbFit_res6_varImp <- varImp(xgbFit_res6)
xgbFit_res6_varImp$importance |> filter(Overall > 0) |> nrow()

xgbFit_res6_vars <- xgbFit_res6_varImp$importance |>
  filter(Overall > 0) |>
  row.names()

plot(xgbFit_res6_varImp, top = 43)
plot(xgbFit_res6)

cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")

## result 7 ----
param_grid7 <- expand.grid(max_depth = c(1,2,3),
                           eta       = c(0.05, 0.1, 0.2),  # learning rate
                           nrounds   = c(50, 100, 200),
                           gamma = 0,
                           min_child_weight = 1,
                           subsample = 1,
                           colsample_bytree = c(0.4, 0.7, 1))
#form_res <- as.formula(paste("result ~", paste(final_features_res, collapse="+")))
training_data_res_linCombos <- findLinearCombos(training_data_res)

training_data_res2 <- training_data_res[, -training_data_res_linCombos$remove]

training_data_res3 <- training_data_res2 |>
  select(result, xgbFit_res4_vars)

set.seed(52)
system.time(
  xgbFit_res7 <- train(
    result ~ ., 
    data = training_data_res, 
    method = "xgbTree", 
    trControl = fitControl,
    #verbose = FALSE, 
    ## Now specify the exact models 
    ## to evaluate:
    tuneGrid = param_grid7
  )
)
xgbFit_res7

xgbFit_res7_best_tune <- xgbFit_res7$bestTune
xgbFit_res7_best_tune
cat("Best XGBoost params for result: \n", 
    "max_depth = ", xgbFit_res7_best_tune$max_depth, "\n",
    "eta = ", xgbFit_res7_best_tune$eta, "\n",
    "nrounds = ", xgbFit_res7_best_tune$nrounds, "\n",
    "subsample = ", xgbFit_res7_best_tune$subsample, "\n",
    "colsample_bytree = ", xgbFit_res7_best_tune$colsample_bytree, "\n",
    "\n",
    "with RMSE:", min(xgbFit_res7$results$RMSE), "\n")

pred_result_xgb7 <- predict(xgbFit_res7,
                            newdata = testing_data)
postResample(pred_result_xgb7,
             testing_data$result)

# Use the best XGBoost models to predict on the test set
testing_data$pred_result_xgb7 <- pred_result_xgb7

# Evaluate XGBoost performance
rmse_result_xgb7 <- sqrt(mean((testing_data$pred_result_xgb7 - testing_data$result)^2))
#rmse_total_xgb  <- sqrt(mean((test_data$pred_total_xgb - test_data$total)^2))
cat("XGBoost Result RMSE:", rmse_result_xgb7, "\n")
#cat("XGBoost Total RMSE:", rmse_total_xgb, "\n")

# Betting accuracy for XGBoost
testing_data <- testing_data %>%
  #mutate( home_cover_actual = spreadCover) |>
  mutate(home_cover_pred_xgb7 = pred_result_xgb7 > spread_line)#,
#over_pred_xgb       = pred_total_xgb > total_line)
ats_cases7 <- testing_data %>% filter(result != spread_line)
ats_accuracy_xgb7 <- mean(ats_cases7$home_cover_actual == ats_cases7$home_cover_pred_xgb7)
#ou_cases <- testing_data %>% filter(total != total_line)
#ou_accuracy_xgb <- mean(ou_cases$over_actual == ou_cases$over_pred_xgb)
cat("XGBoost model accuracy against the spread:", round(100*ats_accuracy_xgb7,2), "%\n")
#cat("XGBoost model accuracy on over/under bets:", round(100*ou_accuracy_xgb,2), "%\n")

xgbFit_res7_varImp <- varImp(xgbFit_res7)
xgbFit_res7_varImp$importance |> filter(Overall > 0) |> nrow()

xgbFit_res7_vars <- xgbFit_res7_varImp$importance |>
  filter(Overall > 0) |>
  row.names()

xgbFit_res7_vars

plot(xgbFit_res7_varImp, top = 52)
plot(xgbFit_res7)

cat("Best XGBoost params for result: \n", 
    "max_depth = ", best_params_res$max_depth, "\n",
    "eta = ", best_params_res$eta, "\n",
    "nrounds = ", best_params_res$nrounds, "\n",
    "subsample = ", best_params_res$subsample, "\n",
    "colsample_bytree = ", best_params_res$colsample_bytree, "\n",
    "\n",
    "with RMSE:", best_rmse_res, "\n")

features <- list(
  "selected_features_res" = selected_features_res,
  "selected_features_tot" = selected_features_tot,
  "final_features_res" = final_features_res,
  "final_features_tot" = final_features_tot,
  "xgb_features_res" = xgb_features_res,
  "xgb_features_tot" = xgb_features_tot,
  "xgbFit_res4_vars" = xgbFit_res4_vars,
  "xgbFit_res6_vars" = xgbFit_res6_vars,
  "xgbFit_res7_vars" = xgbFit_res7_vars
)
save(features, file = "~/Desktop/NFL Analysis Data/features.RData")
