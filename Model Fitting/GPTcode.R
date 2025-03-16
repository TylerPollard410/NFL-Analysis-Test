# Load required libraries
library(data.table)
library(dplyr)
library(glmnet)
library(xgboost)
library(caret)
library(tidymodels)
library(brms)
library(ggplot2)

# Load Data
data <- fread("/mnt/data/modData.csv")
data <- modData

# Inspect Data
str(data)
summary(data)

# -------------------- Data Preprocessing --------------------

# Remove redundant columns (like game IDs, team names, etc.)
data <- data %>%
  select(-c(#game_id, #team_home, team_away,
    contains("fg_pct"), contains("pat_pct"))) 

# Convert categorical variables (if any) to factors
# data <- data %>%
#   mutate_if(is.character, as.factor) |>
#   filter(season >= 2007)

# Split into predictors and response variables
predictors <- data %>%
  filter(season >= 2007) |>
  select(-c(#game_id,
    game_type, gameday, gametime,
    home_team, away_team,
    away_coach, home_coach, stadium,
    result, total, winner,
    contains("moneyline"), 
    contains("spread"), contains("over"), contains("under"))) 

predictors <- predictors |>
  filter(complete.cases(predictors)) #|>
#mutate_if(is.character, as.factor)

predictorIDs <- predictors |> select(game_id)
predictors <- predictors |>
  select(-game_id) |>
  mutate_if(is.character, as.factor)
  
dataNA <- which(!complete.cases(predictors))
NAcols <- apply(predictors, 2, function(x) sum(is.na(x)))
NAcols[NAcols != 0]

data2 <- left_join(
  predictorIDs,
  data
)

response_spread <- data2$result
response_total <- data2$total

# -------------------- Feature Selection with LASSO --------------------

# Convert data to matrix for glmnet
X <- as.matrix(predictors)
y_spread <- response_spread
y_total <- response_total

# Perform LASSO regression for feature selection
lasso_model_spread <- cv.glmnet(X, y_spread, alpha = 1)
lasso_model_total <- cv.glmnet(X, y_total, alpha = 1)

# Identify nonzero coefficients
selected_features_spread <- rownames(coef(lasso_model_spread, s = "lambda.min"))[-1]
selected_features_total <- rownames(coef(lasso_model_total, s = "lambda.min"))[-1]

# Reduce dataset to selected features
predictors_spread <- predictors %>% 
  select(all_of(selected_features_spread)) |>
  mutate_if(is.factor, as.numeric)
predictors_total <- predictors %>% 
  select(all_of(selected_features_total)) |>
  mutate_if(is.factor, as.numeric)

str(predictors_spread)
str(predictors_total)

# -------------------- Train-Test Split --------------------

set.seed(123)
trainIndex <- createDataPartition(y_spread, p = 0.8, list = FALSE)
train_spread <- predictors_spread[trainIndex, ]
test_spread <- predictors_spread[-trainIndex, ]
train_total <- predictors_total[trainIndex, ]
test_total <- predictors_total[-trainIndex, ]
y_train_spread <- y_spread[trainIndex]
y_test_spread <- y_spread[-trainIndex]
y_train_total <- y_total[trainIndex]
y_test_total <- y_total[-trainIndex]

# -------------------- GLM Baseline Model --------------------

glm_spread <- glm(y_train_spread ~ ., data = train_spread, family = gaussian)
glm_total <- glm(y_train_total ~ ., data = train_total, family = gaussian)

# Predictions
glm_spread_pred <- predict(glm_spread, test_spread)
glm_total_pred <- predict(glm_total, test_total)

# RMSE for GLM
rmse_glm_spread <- sqrt(mean((glm_spread_pred - y_test_spread)^2))
rmse_glm_total <- sqrt(mean((glm_total_pred - y_test_total)^2))

# -------------------- XGBoost Model --------------------

dtrain_spread <- xgb.DMatrix(data = as.matrix(train_spread), label = y_train_spread)
dtest_spread <- xgb.DMatrix(data = as.matrix(test_spread), label = y_test_spread)

dtrain_total <- xgb.DMatrix(data = as.matrix(train_total), label = y_train_total)
dtest_total <- xgb.DMatrix(data = as.matrix(test_total), label = y_test_total)

# Train XGBoost models
xgb_params <- list(objective = "reg:squarederror",
                   eval_metric = "rmse",
                   max_depth = 6, 
                   eta = 0.1, 
                   nrounds = 500)

xgb_model_spread <- xgb.train(params = xgb_params,
                              data = dtrain_spread,
                              nrounds = 500)
xgb_model_total <- xgb.train(params = xgb_params, 
                             data = dtrain_total, 
                             nrounds = 500)

# Predictions
xgb_spread_pred <- predict(xgb_model_spread, dtest_spread)
xgb_total_pred <- predict(xgb_model_total, dtest_total)

# RMSE for XGBoost
rmse_xgb_spread <- sqrt(mean((xgb_spread_pred - y_test_spread)^2))
rmse_xgb_total <- sqrt(mean((xgb_total_pred - y_test_total)^2))

# -------------------- Bayesian Hierarchical Model --------------------

bayesian_model <- brm(
  bf(result ~ home_offense + away_defense + (1 | home_team) + (1 | away_team)),
  data = data,
  family = gaussian(),
  prior = c(prior(normal(0, 10), class = "b")),
  iter = 2000, chains = 4
)

bayesian_pred <- predict(bayesian_model, test_spread)$Estimate

# -------------------- Evaluation --------------------

# Function to compute accuracy of spread and total predictions vs. Vegas lines
compute_betting_accuracy <- function(predictions, actuals, vegas_line) {
  correct_preds <- sum((predictions - vegas_line) * (actuals - vegas_line) > 0)
  return(correct_preds / length(actuals))
}

# Compare against Vegas lines
spread_accuracy_xgb <- compute_betting_accuracy(xgb_spread_pred, y_test_spread, data$spread_line[-trainIndex])
total_accuracy_xgb <- compute_betting_accuracy(xgb_total_pred, y_test_total, data$total_line[-trainIndex])

spread_accuracy_glm <- compute_betting_accuracy(glm_spread_pred, y_test_spread, data$spread_line[-trainIndex])
total_accuracy_glm <- compute_betting_accuracy(glm_total_pred, y_test_total, data$total_line[-trainIndex])

spread_accuracy_bayesian <- compute_betting_accuracy(bayesian_pred, y_test_spread, data$spread_line[-trainIndex])

# -------------------- Print Results --------------------

cat("RMSE:\n")
cat("GLM Spread:", rmse_glm_spread, "\n")
cat("GLM Total:", rmse_glm_total, "\n")
cat("XGBoost Spread:", rmse_xgb_spread, "\n")
cat("XGBoost Total:", rmse_xgb_total, "\n\n")

cat("Betting Accuracy:\n")
cat("GLM Spread Accuracy:", spread_accuracy_glm * 100, "%\n")
cat("GLM Total Accuracy:", total_accuracy_glm * 100, "%\n")
cat("XGBoost Spread Accuracy:", spread_accuracy_xgb * 100, "%\n")
cat("XGBoost Total Accuracy:", total_accuracy_xgb * 100, "%\n")
cat("Bayesian Spread Accuracy:", spread_accuracy_bayesian * 100, "%\n")

# -------------------- Deployment: Weekly Updates --------------------

update_model_weekly <- function(new_week_data) {
  # Append new data and retrain
  data <<- rbind(data, new_week_data)
  
  # Retrain XGBoost
  dtrain_spread <- xgb.DMatrix(data = as.matrix(predictors_spread), label = response_spread)
  xgb_model_spread <<- xgb.train(params = xgb_params, data = dtrain_spread, nrounds = 500)
  
  # Update Bayesian model
  bayesian_model <<- update(bayesian_model, newdata = data)
  
  cat("Models updated with new week’s data.\n")
}


# Load necessary libraries
library(data.table)
library(dplyr)
library(glmnet)
library(xgboost)
library(caret)
library(brms)
library(ggplot2)

# -------------------- Load Data --------------------

# Read the CSV file (ensure it's in the correct path)
data <- fread("/mnt/data/modData.csv")

# Display dataset info
print(dim(data))
print(names(data))

# -------------------- Data Preprocessing --------------------

# Drop unnecessary columns (game IDs, team names, etc.)
data <- data %>% select(-c(game_id, home_team, away_team))

# Define predictors
predictors <- data %>% select(-c(result, total, home_score, away_score, spread_line, total_line, spreadCover, totalCover))

# Define response variables
y_home <- data$home_score
y_away <- data$away_score

# -------------------- Feature Selection with LASSO --------------------

# Convert predictors to matrix format
X <- as.matrix(predictors)

# Perform LASSO for feature selection
lasso_home <- cv.glmnet(X, y_home, alpha = 1)
lasso_away <- cv.glmnet(X, y_away, alpha = 1)

# Identify selected features
selected_features_home <- colnames(X)[which(coef(lasso_home, s = "lambda.min")[-1] != 0)]
selected_features_away <- colnames(X)[which(coef(lasso_away, s = "lambda.min")[-1] != 0)]

# Reduce dataset to selected features
X_home <- predictors %>% select(all_of(selected_features_home))
X_away <- predictors %>% select(all_of(selected_features_away))

# -------------------- Train-Test Split --------------------

set.seed(42)
trainIndex <- createDataPartition(y_home, p = 0.8, list = FALSE)

X_train_home <- X_home[trainIndex, ]
X_test_home <- X_home[-trainIndex, ]
y_train_home <- y_home[trainIndex]
y_test_home <- y_home[-trainIndex]

X_train_away <- X_away[trainIndex, ]
X_test_away <- X_away[-trainIndex, ]
y_train_away <- y_away[trainIndex]
y_test_away <- y_away[-trainIndex]

# -------------------- GLM Baseline Model --------------------

glm_home <- lm(y_train_home ~ ., data = as.data.frame(X_train_home))
glm_away <- lm(y_train_away ~ ., data = as.data.frame(X_train_away))

glm_home_pred <- predict(glm_home, newdata = as.data.frame(X_test_home))
glm_away_pred <- predict(glm_away, newdata = as.data.frame(X_test_away))

glm_result_pred <- glm_home_pred - glm_away_pred
glm_total_pred <- glm_home_pred + glm_away_pred

# Compute RMSE
rmse_glm_home <- sqrt(mean((glm_home_pred - y_test_home)^2))
rmse_glm_away <- sqrt(mean((glm_away_pred - y_test_away)^2))

# -------------------- XGBoost Model --------------------

dtrain_home <- xgb.DMatrix(data = as.matrix(X_train_home), label = y_train_home)
dtest_home <- xgb.DMatrix(data = as.matrix(X_test_home), label = y_test_home)

dtrain_away <- xgb.DMatrix(data = as.matrix(X_train_away), label = y_train_away)
dtest_away <- xgb.DMatrix(data = as.matrix(X_test_away), label = y_test_away)

xgb_params <- list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 6, eta = 0.1)

xgb_home <- xgb.train(params = xgb_params, data = dtrain_home, nrounds = 500)
xgb_away <- xgb.train(params = xgb_params, data = dtrain_away, nrounds = 500)

xgb_home_pred <- predict(xgb_home, dtest_home)
xgb_away_pred <- predict(xgb_away, dtest_away)

xgb_result_pred <- xgb_home_pred - xgb_away_pred
xgb_total_pred <- xgb_home_pred + xgb_away_pred

# Compute RMSE
rmse_xgb_home <- sqrt(mean((xgb_home_pred - y_test_home)^2))
rmse_xgb_away <- sqrt(mean((xgb_away_pred - y_test_away)^2))

# -------------------- Bayesian Model --------------------

bayesian_model <- brm(
  bf(home_score ~ ., away_score ~ ., family = gaussian()),
  data = data %>% select(all_of(c(selected_features_home, "home_score", selected_features_away, "away_score"))),
  prior = c(prior(normal(0, 10), class = "b")),
  iter = 2000, chains = 4
)

bayesian_home_pred <- predict(bayesian_model, newdata = as.data.frame(X_test_home))$Estimate
bayesian_away_pred <- predict(bayesian_model, newdata = as.data.frame(X_test_away))$Estimate

bayesian_result_pred <- bayesian_home_pred - bayesian_away_pred
bayesian_total_pred <- bayesian_home_pred + bayesian_away_pred

# -------------------- Betting Accuracy --------------------

compute_betting_accuracy <- function(predictions, actuals, vegas_line) {
  correct_preds <- sum((predictions - vegas_line) * (actuals - vegas_line) > 0)
  return(correct_preds / length(actuals))
}

spread_accuracy_xgb <- compute_betting_accuracy(xgb_result_pred, y_test_home - y_test_away, data$spread_line[-trainIndex])
total_accuracy_xgb <- compute_betting_accuracy(xgb_total_pred, y_test_home + y_test_away, data$total_line[-trainIndex])

spread_accuracy_glm <- compute_betting_accuracy(glm_result_pred, y_test_home - y_test_away, data$spread_line[-trainIndex])
total_accuracy_glm <- compute_betting_accuracy(glm_total_pred, y_test_home + y_test_away, data$total_line[-trainIndex])

spread_accuracy_bayesian <- compute_betting_accuracy(bayesian_result_pred, y_test_home - y_test_away, data$spread_line[-trainIndex])

# -------------------- Print Results --------------------

cat("\n--- RMSE ---\n")
cat("GLM Home RMSE:", rmse_glm_home, "\n")
cat("GLM Away RMSE:", rmse_glm_away, "\n")
cat("XGBoost Home RMSE:", rmse_xgb_home, "\n")
cat("XGBoost Away RMSE:", rmse_xgb_away, "\n")

cat("\n--- Betting Accuracy ---\n")
cat("GLM Spread Accuracy:", spread_accuracy_glm * 100, "%\n")
cat("GLM Total Accuracy:", total_accuracy_glm * 100, "%\n")
cat("XGBoost Spread Accuracy:", spread_accuracy_xgb * 100, "%\n")
cat("XGBoost Total Accuracy:", total_accuracy_xgb * 100, "%\n")
cat("Bayesian Spread Accuracy:", spread_accuracy_bayesian * 100, "%\n")

# -------------------- Weekly Model Update --------------------

update_model_weekly <- function(new_week_data) {
  data <<- rbind(data, new_week_data)
  
  dtrain_home_new <- xgb.DMatrix(data = as.matrix(data[selected_features_home]), label = data$home_score)
  dtrain_away_new <- xgb.DMatrix(data = as.matrix(data[selected_features_away]), label = data$away_score)
  
  xgb_home <<- xgb.train(params = xgb_params, data = dtrain_home_new, nrounds = 500)
  xgb_away <<- xgb.train(params = xgb_params, data = dtrain_away_new, nrounds = 500)
  
  cat("Models updated with new week’s data.\n")
}


