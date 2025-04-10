# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 0. SETUP & LIBRARIES ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Modeling libraries:
library(caret)                # For preprocessing utilities like nearZeroVar and findLinearCombos
library(xgboost)              # Native XGBoost training
library(rBayesianOptimization)  # For Bayesian hyperparameter optimization
#library(projpred)             # Optional variable selection
library(brms)                 # Bayesian hierarchical modeling
library(bayesplot)
library(pROC)                 # For performance metrics
library(parallel)             # For parallel processing

# General libraries for data manipulation, plotting and date handling
library(data.table)
# library(tidyverse)

library(readr)
library(tidytext)
# library(tidyr)
# library(purrr)
library(plotly)
library(patchwork)
library(doParallel)
library(rBayesianOptimization)
library(xgboost)
library(caret)
library(brms)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
library(nflverse)
library(tidyverse)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(file = "~/Desktop/NFL Analysis Data/modDataLong.rda")
load(file = "~/Desktop/NFL Analysis Data/modData.rda")

# Load the long-format data (scores model)
modDataLong <- modDataLong |>
  filter(season >= 2007)

# Load the wide-format data (for result/total models)
modData <- modData |>
  filter(season >= 2007)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# XGBOOST MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. Pre-Processing ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Remove Near-Zero Variance Predictors ---
# Here, caret::nearZeroVar identifies variables with almost no variation.
nzv_info <- nearZeroVar(modDataLong, saveMetrics = TRUE)
nzv_cols <- rownames(nzv_info)[nzv_info$nzv]
cat("Removing near-zero variance columns:\n", paste(nzv_cols, collapse = ", "), "\n")
data_long_clean <- modDataLong %>% select(-all_of(nzv_cols))

# --- 2. Exclude Non-Predictor Columns ---
# We want to remove columns that are identifiers, outcomes, or betting/odds-related,
# but we want to keep key contextual variables such as:
#   • week – a time indicator (helps the model capture season progression)
#   • locationID – which serves as a proxy for home-field advantage
#   • div_game, temp, wind – as they might influence game performance.
#
# Define a vector of columns to exclude from the candidate set:
exclude_cols <- c("season",          # Remove season as it is an identifier
                  "opponent_score",  # Outcome variable not used as predictor
                  "team_score",      # The response variable
                  "result",          # Outcome summary (e.g., win/loss margin)
                  "total",           # Total points (or similar outcome)
                  "overtime",        # Not used for prediction here
                  "opponent_rest", "team_rest",
                  "opponent_moneyline", "opponent_moneyline_prob",
                  "team_moneyline", "team_moneyline_prob",
                  "spread_line", 
                  "opponent_spread_odds", "opponent_spread_prob",
                  "team_spread_odds", "team_spread_prob",
                  "total_line", 
                  "under_odds", "under_prob", "over_odds", "over_prob")

# --- 3. Select Candidate Numeric Predictors ---
# Get all numeric columns first
numeric_cols <- data_long_clean %>% select_if(is.numeric)

# Now remove the excluded columns from numeric predictors.
candidate_numeric <- numeric_cols %>% select(-all_of(exclude_cols))

cat("Candidate predictor columns before redundancy removal:\n",
    paste(colnames(candidate_numeric), collapse = ", "), "\n")
candidate_numeric_cols <- colnames(candidate_numeric)
candidate_numeric_cols

# In this candidate set we expect key variables like:
#   • week (if stored as numeric)
#   • locationID (a proxy for home advantage)
#   • div_game, temp, wind (if numeric)
# These remain because they are not included in exclude_cols.

# --- 4. Clean Candidate Predictors (Handle NA/Inf) ---
# Before checking for linear combinations, we need a complete numeric dataset.
candidate_numeric_NA <- which(!complete.cases(candidate_numeric))
data_long_clean_NA <- data_long_clean |> 
  slice(candidate_numeric_NA) |>
  select(game_id, all_of(candidate_numeric_cols))

NAcols <- apply(data_long_clean_NA, 2, function(x) sum(is.na(x)))
NAcols[NAcols != 0]

candidate_numeric_clean <- candidate_numeric %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  filter(if_all(everything(), ~ is.finite(.)))



# --- 5. Remove Redundant (Linearly Dependent) Predictors ---
# Use caret::findLinearCombos to detect and remove any exact or near-exact linear combinations.
linCombos <- findLinearCombos(candidate_numeric_clean)
if (!is.null(linCombos$remove) && length(linCombos$remove) > 0) {
  remove_lin <- colnames(candidate_numeric_clean)[linCombos$remove]
  cat("Removing linearly dependent columns:\n", paste(remove_lin, collapse = ", "), "\n")
  candidate_numeric_clean <- candidate_numeric_clean %>% select(-all_of(remove_lin))
} else {
  cat("No linear combinations detected.\n")
}
remove_lin

# --- 6. Define the Final Candidate Feature Set ---
# This is the list of predictor names you intend to use in the model training.
candidate_features <- colnames(candidate_numeric_clean)
cat("Final candidate predictor columns:\n", paste(candidate_features, collapse = ", "), "\n")
candidate_features

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Scores Model ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# For XGBoost, we will work with team-level predictions (e.g., predicting team_score).
# Ensure that the following predictors are included:
#   - Time features: season, week (and any computed season-progress variable)
#   - Home field indicator: "location" or "locationID"
#   - Other engineered features such as EPA metrics, rolling averages, etc.
# Select candidate predictors (modify as needed)

# We include some key identifiers and contextual variables explicitly.
# For example, keep game_id (for merging later), season, week, team, location,
# the response variable 'team_score', and all candidate predictors.

xgb_data <- data_long_clean |> 
  select(game_id, season, week, location,
         team, opponent,
         team_score, opponent_score,
         total, result,
         spread_line, total_line,
         all_of(candidate_features))

# A quick glimpse of the final data structure:
glimpse(xgb_data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 4. Modular Functions for Tuning and Forecasting ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### ----- Function 1: tune_and_select_features() -----
# For a given forecast season, this function:
#   - Uses a tuning training set and a tuning test set.
#   - Performs Bayesian Optimization to tune hyperparameters.
#   - Trains a full model on the tuning data and extracts variable importance.
#   - Iteratively selects a reduced feature set based on RMSE performance on the tuning test set.
tune_and_select_features <- function(forecast_season, data, candidate_features, 
                                     tune_train_seasons, tune_test_season) {
  # Extract tuning training and test data.
  tune_train <- data %>% filter(season %in% tune_train_seasons)
  tune_test <- data %>% filter(season == tune_test_season)
  
  # Use the candidate_features as supplied.
  feature_cols <- candidate_features
  feature_cols <- feature_cols[!is.na(feature_cols)]
  
  # Prepare DMatrix objects using the full candidate set.
  dtrain_full <- xgb.DMatrix(data = as.matrix(tune_train %>% select(any_of(feature_cols))),
                             label = tune_train$team_score)
  dtest_full  <- xgb.DMatrix(data = as.matrix(tune_test %>% select(any_of(feature_cols))),
                             label = tune_test$team_score)
  
  # --- Define objective function for Bayesian Optimization ---
  # Note: We are now tuning max_depth, eta, gamma, colsample_bytree, min_child_weight,
  # subsample, lambda, alpha, and nrounds.
  xgb_eval_function <- function(max_depth, eta, gamma, colsample_bytree, 
                                min_child_weight, subsample, lambda, alpha, nrounds) {
    params <- list(
      booster          = "gbtree",
      objective        = "reg:squarederror",
      eval_metric      = "mae",
      max_depth        = as.integer(max_depth),
      eta              = eta,
      gamma            = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample        = subsample,
      lambda           = lambda,
      alpha            = alpha
    )
    
    model <- xgb.train(
      params = params,
      data = dtrain_full,
      nrounds = as.integer(nrounds),
      watchlist = list(test = dtest_full),
      early_stopping_rounds = 10,
      verbose = 0
    )
    
    best_rmse <- model$best_score   # RMSE based on test set evaluation
    return(list(Score = -best_rmse, Pred = 0))
  }
  
  # --- Define bounds for Bayesian optimization ---
  bounds <- list(
    max_depth         = c(1L, 8L),
    eta               = c(0.01, 0.3),
    gamma             = c(1e-6, 5),
    colsample_bytree  = c(0.5, 1.0),
    min_child_weight  = c(1L, 10L),
    subsample         = c(0.5, 1.0),
    lambda            = c(1e-6, 2),
    alpha             = c(1e-6, 2),
    nrounds           = c(50L, 500L)
  )
  
  cat("Starting Bayesian Optimization for forecast season", forecast_season, "\n")
  set.seed(123)
  bayes_opt_result <- BayesianOptimization(FUN = xgb_eval_function,
                                           bounds = bounds,
                                           init_points = 10,
                                           n_iter = 20,
                                           acq = "ucb",
                                           kappa = 2.576,
                                           verbose = TRUE)
  best_hyperparams <- bayes_opt_result$Best_Par
  cat("Best hyperparameters found:\n")
  print(best_hyperparams)
  
  # --- Train full model on tuning training data using best hyperparameters ---
  final_params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    eval_metric      = "mae",
    max_depth        = as.integer(best_hyperparams["max_depth"]),
    eta              = best_hyperparams["eta"],
    gamma            = best_hyperparams["gamma"],
    colsample_bytree = best_hyperparams["colsample_bytree"],
    min_child_weight = best_hyperparams["min_child_weight"],
    subsample        = best_hyperparams["subsample"],
    lambda           = best_hyperparams["lambda"],
    alpha            = best_hyperparams["alpha"]
  )
  
  nrounds_best <- as.integer(best_hyperparams["nrounds"])
  
  full_model <- xgb.train(
    params = final_params,
    data = dtrain_full,
    nrounds = nrounds_best,
    verbose = 1
  )
  
  # --- Feature Selection via Variable Importance ---
  vi <- xgb.importance(feature_names = feature_cols, model = full_model)
  vi <- vi %>% arrange(desc(Gain))
  
  if(length(feature_cols) > 20){
    candidate_k <- c(1:20, seq(25, length(feature_cols), by = 5))
  }else{
    candidate_k <- 1:length(feature_cols)
  }
  
  #candidate_k <- seq(5, length(feature_cols), by = 5)
  best_error <- Inf
  best_features <- feature_cols
  for(k in candidate_k) {
    selected_features <- vi$Feature[1:k]
    selected_features <- selected_features[!is.na(selected_features)]
    dtrain_red <- xgb.DMatrix(data = as.matrix(tune_train %>% select(any_of(selected_features))),
                              label = tune_train$team_score)
    dtest_red  <- xgb.DMatrix(data = as.matrix(tune_test %>% select(any_of(selected_features))),
                              label = tune_test$team_score)
    model_red <- xgb.train(
      params = final_params,
      data = dtrain_red,
      nrounds = nrounds_best,
      watchlist = list(test = dtest_red),
      early_stopping_rounds = 10,
      verbose = 0
    )
    error_red <- model_red$best_score
    cat("Using top", k, "features, tuning test MAE (RMSE):", error_red, "\n")
    if(error_red < best_error) {
      best_error <- error_red
      best_features <- selected_features
    }
  }
  
  cat("Selected", length(best_features), "features with tuning test MAE =", best_error, "\n")
  
  return(list(best_hyperparams = best_hyperparams,
              best_features = best_features,
              tuning_error = best_error))
}

### ----- Function 2: train_production_model() -----
# Trains an XGBoost model on production data using the tuned hyperparameters and selected feature set.
train_production_model <- function(train_data, hyperparams, feature_set) {
  dtrain <- xgb.DMatrix(data = as.matrix(train_data %>% select(all_of(feature_set))),
                        label = train_data$team_score)
  params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    eval_metric      = "mae",
    max_depth        = as.integer(hyperparams["max_depth"]),
    eta              = hyperparams["eta"],
    gamma            = hyperparams["gamma"],
    colsample_bytree = hyperparams["colsample_bytree"],
    min_child_weight = hyperparams["min_child_weight"],
    subsample        = hyperparams["subsample"],
    lambda           = hyperparams["lambda"],
    alpha            = hyperparams["alpha"]
  )
  nrounds <- as.integer(hyperparams["nrounds"])
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 1
  )
  return(model)
}

### ----- Function 3: iterative_weekly_forecast() -----
# For a given forecast season, this function performs iterative one-week-ahead forecasting.
iterative_weekly_forecast <- function(forecast_season, 
                                      prod_train_data, 
                                      forecast_data,
                                      hyperparams, 
                                      feature_set) {
  forecast_data <- forecast_data %>% arrange(week)
  current_train <- prod_train_data
  forecast_results <- list()
  
  unique_weeks <- sort(unique(forecast_data$week))
  for (wk in unique_weeks) {
    cat("Forecasting season", forecast_season, "week", wk, "\n")
    
    this_week_data <- forecast_data %>% filter(week == wk)
    model <- train_production_model(current_train, hyperparams, feature_set)
    
    dtest <- xgb.DMatrix(data = as.matrix(this_week_data %>% select(all_of(feature_set))))
    preds <- predict(model, dtest)
    
    week_forecast <- this_week_data %>% 
      select(game_id, season, week, team, location, team_score) %>% 
      mutate(pred_score = preds)
    forecast_results[[as.character(wk)]] <- week_forecast
    
    # Append this week's data (with actual outcomes) to the training set.
    current_train <- bind_rows(current_train, this_week_data)
  }
  all_forecasts <- bind_rows(forecast_results)
  return(list(model = model, all_forecasts = all_forecasts))
}

### ----- Function 4: run_full_pipeline() -----
# Loops over each forecast season. For each season:
#   - Uses tuning window: tuning training from (fs - 4):(fs - 2) and tuning test = (fs - 1)
#   - Uses production training window: seasons (fs - 3):(fs - 1)
#   - Forecasts the target season in a one-week-ahead manner.
run_full_pipeline <- function(forecast_seasons, data, candidate_features) {
  results_list <- list()
  
  for (fs in forecast_seasons) {
    cat("\n============================\n")
    cat("Processing forecast season:", fs, "\n")
    # Set tuning windows.
    # For forecasting season fs, use:
    #   - Tuning training: seasons (fs - 4) to (fs - 2)
    #   - Tuning test: season (fs - 1)
    #   - Production training: seasons (fs - 3) to (fs - 1)
    tune_train_seasons <- seq(fs - 4, fs - 2)
    tune_test_season   <- fs - 1
    prod_train_seasons <- seq(fs - 3, fs - 1)
    
    cat("Tuning: Train seasons:", paste(tune_train_seasons, collapse = ", "), 
        " | Test season:", tune_test_season, "\n")
    
    tune_data <- data %>% filter(season %in% c(tune_train_seasons, tune_test_season))
    
    tuning_output <- tune_and_select_features(fs, tune_data, candidate_features, 
                                              tune_train_seasons, tune_test_season)
    
    best_hyperparams <- tuning_output$best_hyperparams
    best_features <- tuning_output$best_features
    cat("For forecast season", fs, ": Best hyperparameters found;",
        length(best_features), "features selected.\n")
    
    prod_train <- data %>% filter(season %in% prod_train_seasons)
    
    forecast_data <- data %>% filter(season == fs)
    
    forecasts <- iterative_weekly_forecast(fs, prod_train, forecast_data, 
                                           best_hyperparams, best_features)
    
    results_list_temp <- list(
      forecast_season = fs,
      tuning = tuning_output,
      forecasts = forecasts$all_forecasts
    )
    results_list[[as.character(fs)]] <- results_list_temp
    save(results_list_temp,
         file = paste0("forecast_results_season_", fs, ".rda"))
    
    
    forecast_model <- forecasts$model
    save(forecast_model,
         file = paste0("forecast_model_season_", fs, ".rda"))
  }
  return(results_list)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 5. Run Full Pipeline ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define which forecast seasons to run.
# With our splitting approach: to forecast 2011, we need tuning training from 2007-2009 and tuning test = 2010.
# For example, run forecast seasons from 2011 through 2024.
forecast_seasons <- seq(2011, 2024)

# Run the full pipeline on the long-format (XGBoost) data.
# This will take tuning windows, perform hyperparameter/feature selection,
# then do iterative week-by-week forecasting.
setwd("~/Desktop/NFL Analysis Data/XGBoost Historic Tune")
pipeline_results <- run_full_pipeline(forecast_seasons, xgb_data, candidate_features)

save(pipeline_results, file = "full_xgb_pipeline_results.rda")

setwd("~/Desktop/NFLAnalysisTest")

# Test pipeline on one season %%%%%%%%%%%%%%%%%%%%%%%%%
# fs <- 2024
# data <- xgb_data
# results_list <- list()
# 
# cat("\n============================\n")
# cat("Processing forecast season:", fs, "\n")
# # Set tuning windows.
# # For forecasting season fs, use:
# #   - Tuning training: seasons (fs - 4) to (fs - 2)
# #   - Tuning test: season (fs - 1)
# #   - Production training: seasons (fs - 3) to (fs - 1)
# tune_train_seasons <- seq(fs - 4, fs - 2)
# tune_test_season   <- fs - 1
# prod_train_seasons <- seq(fs - 3, fs - 1)
# 
# cat("Tuning: Train seasons:", paste(tune_train_seasons, collapse = ", "), 
#     " | Test season:", tune_test_season, "\n")
# 
# tune_data <- data %>% filter(season %in% c(tune_train_seasons, tune_test_season))
# 
# tuning_output <- tune_and_select_features(fs, tune_data, candidate_features, 
#                                           tune_train_seasons, tune_test_season)
# 
# best_hyperparams <- tuning_output$best_hyperparams
# best_features <- tuning_output$best_features
# cat("For forecast season", fs, ": Best hyperparameters found;",
#     length(best_features), "features selected.\n")
# cat(best_features, sep = "\n")
# 
# prod_train <- data %>% filter(season %in% prod_train_seasons)
# 
# forecast_data <- data %>% filter(season == fs)
# 
# forecasts <- iterative_weekly_forecast(fs, prod_train, forecast_data, 
#                                        best_hyperparams, best_features)
# 
# results_list_temp <- list(
#   forecast_season = fs,
#   tuning = tuning_output,
#   forecasts = forecasts
# )
# 
# results_list[[as.character(fs)]] <- results_list_temp
# 
# save(results_list_temp,
#      file = paste0(
#        "~/Desktop/NFL Analysis Data/XGBoost Historic Tune/",
#        "forecast_results_season_", fs, ".rda"))
# 
# return(results_list)


finished_models <- list.files("~/XGB")
finished_models2 <- str_subset(finished_models, "forecast_results")
finished_models3 <- str_extract(finished_models2, "[:digit:]+")

finished_seasons <- as.numeric(finished_models3)
file_loc <- "~/XGB/"

xgb_models_list <- list()
best_feature_df <- data.frame()
for(s in finished_seasons){
  filename <- paste0(file_loc, "forecast_results_season_", s, ".rda")
  load(file = filename)
  
  xgb_models_list[[as.character(s)]] <- results_list_temp
  
  season_features <- results_list_temp$tuning$best_features
  feature_temp <- data.frame(
    Feature = season_features,
    Season = s
  )
  best_feature_df <- rbind(best_feature_df, feature_temp)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 6. Betting Evaluation (XGB Model Raw) ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_forecasts <- lapply(pipeline_results, function(x) x[["forecasts"]])
all_forecasts <- lapply(xgb_models_list, function(x) x[["forecasts"]])
all_forecasts_df <- bind_rows(all_forecasts)

# Summarize performance by forecast year (season)
yearly_summary <- all_forecasts_df %>%
  group_by(season) %>%
  summarise(
    RMSE = rmse(team_score, pred_score),
    MAE  = mae(team_score, pred_score),
    n    = n()
  )
print(yearly_summary)

# Summarize performance by forecast week
weekly_summary <- all_forecasts_df %>%
  group_by(week) %>%
  summarise(
    RMSE = rmse(team_score, pred_score),
    MAE  = mae(team_score, pred_score),
    n    = n()
  )
print(weekly_summary, n = nrow(weekly_summary))

## Pivot forecasts wider for direct comparison of home_score, away_score, result, total
betting_vars <- c("spread_line", "spreadCover", 
                  "home_spread_odds", "home_spread_prob",
                  "away_spread_odds", "away_spread_prob",
                  "total_line", "totalCover",
                  "over_odds", "over_prob",
                  "under_odds", "under_prob",
                  "winner", 
                  "home_moneyline", "home_moneyline_prob",
                  "away_moneyline", "away_moneyline_prob")

xgb_preds_df <- modData |> 
  select(
    game_id, season, week,
    home_team,
    away_team,
    home_score,
    away_score,
    result,
    total,
    all_of(betting_vars)
  ) |>
  left_join(
    all_forecasts_df |> 
      select(game_id, team, xgb_home_score = pred_score),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    all_forecasts_df |> 
      select(game_id, team, xgb_away_score = pred_score),
    by = join_by(game_id, away_team == team)
  ) |>
  mutate(
    xgb_result = xgb_home_score - xgb_away_score,
    xgb_total = xgb_home_score + xgb_away_score
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, .after = result) |>
  relocate(xgb_total, .after = total)

# Summarize performance by forecast year (season)
xgb_preds_df %>%
  group_by(season) %>%
  summarise(
    RMSE_home = rmse(home_score, xgb_home_score),
    MAE_home  = mae(home_score, xgb_home_score),
    RMSE_away = rmse(away_score, xgb_away_score),
    MAE_away  = mae(away_score, xgb_away_score),
    RMSE_result = rmse(result, xgb_result),
    MAE_result  = mae(result, xgb_result),
    RMSE_total = rmse(total, xgb_total),
    MAE_total  = mae(total, xgb_total),
    n    = n()
  ) |>
  print(n = 50)

# Summarize performance by forecast week
xgb_preds_df %>%
  filter(complete.cases(xgb_preds_df)) |>
  group_by(week) %>%
  summarise(
    RMSE_home = rmse(home_score, xgb_home_score),
    MAE_home  = mae(home_score, xgb_home_score),
    RMSE_away = rmse(away_score, xgb_away_score),
    MAE_away  = mae(away_score, xgb_away_score),
    RMSE_result = rmse(result, xgb_result),
    MAE_result  = mae(result, xgb_result),
    RMSE_total = rmse(total, xgb_total),
    MAE_total  = mae(total, xgb_total),
    n    = n()
  ) |>
  print(n = 50)



evaluate_betting_performance <- function(bet_df,
                                         result_pred_col = "xgb_result",
                                         total_pred_col = "xgb_total",
                                         group_season = FALSE,
                                         group_week = FALSE) {
  
  # Conditionally process result predictions if a column name is provided.
  if (!is.na(result_pred_col)) {
    bet_df <- bet_df %>%
      mutate(
        exp_result = .data[[result_pred_col]],
        actual_result_cover = case_when(
          result > spread_line ~ "Home",
          result < spread_line ~ "Away",
          TRUE ~ NA_character_
        ),
        exp_result_cover = case_when(
          exp_result > spread_line ~ "Home",
          exp_result < spread_line ~ "Away",
          TRUE ~ NA_character_
        ),
        correct_result = exp_result_cover == actual_result_cover
      )
  } else {
    bet_df <- bet_df %>%
      mutate(
        exp_result = NA_real_,
        actual_result_cover = NA_character_,
        exp_result_cover = NA_character_,
        correct_result = NA
      )
  }
  
  # Conditionally process total predictions if a column name is provided.
  if (!is.na(total_pred_col)) {
    bet_df <- bet_df %>%
      mutate(
        exp_total = .data[[total_pred_col]],
        actual_total_cover = case_when(
          total > total_line ~ "Over",
          total < total_line ~ "Under",
          TRUE ~ NA_character_
        ),
        exp_total_cover = case_when(
          exp_total > total_line ~ "Over",
          exp_total < total_line ~ "Under",
          TRUE ~ NA_character_
        ),
        correct_total = exp_total_cover == actual_total_cover
      )
  } else {
    bet_df <- bet_df %>%
      mutate(
        exp_total = NA_real_,
        actual_total_cover = NA_character_,
        exp_total_cover = NA_character_,
        correct_total = NA
      )
  }
  
  # Group the output by season/week as requested.
  if (group_season & group_week) {
    bet_out_df <- bet_df %>%
      group_by(season, week)
  } else if (group_season & !group_week) {
    bet_out_df <- bet_df %>%
      group_by(season)
  } else if (!group_season & group_week) {
    bet_out_df <- bet_df %>%
      group_by(week)
  } else {
    bet_out_df <- bet_df
  }
  
  # Compute summary accuracy.
  acc_df <- bet_out_df %>%
    summarise(
      games = n(),
      result_bets = if (!is.na(result_pred_col)) sum(!is.na(correct_result)) else NA_integer_,
      result_accuracy = if (!is.na(result_pred_col)) round(mean(correct_result, na.rm = TRUE) * 100, 2) else NA_real_,
      total_bets = if (!is.na(total_pred_col)) sum(!is.na(correct_total)) else NA_integer_,
      total_accuracy = if (!is.na(total_pred_col)) round(mean(correct_total, na.rm = TRUE) * 100, 2) else NA_real_
    )
  
  return(acc_df)
}

xgb_betting_accuracy <- evaluate_betting_performance(
  xgb_preds_df, 
  result_pred_col = "xgb_result",
  total_pred_col = "xgb_total",
  group_season = FALSE,
  group_week = FALSE
)
print(xgb_betting_accuracy, n = nrow(xgb_betting_accuracy))

xgb_betting_accuracy <- evaluate_betting_performance(
  xgb_preds_df, 
  result_pred_col = "xgb_result",
  total_pred_col = "xgb_total",
  group_season = TRUE,
  group_week = FALSE
)
print(xgb_betting_accuracy, n = nrow(xgb_betting_accuracy))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BRMS MODELING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

best_feature_rep <- best_feature_df |>
  mutate(Rank = row_number(), .by = Season, .keep = "all") |>
  group_by(Feature) |>
  summarise(
    Total_Rank = sum(Rank),
    Seasons = n()
  ) |>
  ungroup() |>
  mutate(Total_Rank = Total_Rank/Seasons) |>
  arrange(desc(Seasons), Total_Rank)
print(best_feature_rep, n = nrow(best_feature_rep))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. Pre-Processing ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Merge XGBoost predictions into wide data by game_id.
# (Ensure that xgb_predictions exist; if not, you may need to load them or run the xgb pipeline first.)
brms_data <- modData |>
  left_join(
    xgb_preds_df |> 
      select(game_id, home_team, away_team, contains("xgb")),
    by = join_by(game_id, home_team, away_team)
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, .after = result) |>
  relocate(xgb_total, .after = total)

# Add Net Features
brms_data <- brms_data |>
  mutate(
    net_elo = home_elo - away_elo,
    net_SRS = home_SRS_cum - away_SRS_cum,
    net_off_epa = home_off_epa_mean_cum - away_def_epa_mean_cum,
    net_def_epa = away_off_epa_mean_cum - home_def_epa_mean_cum,
    net_turnover_diff = home_turnover_diff_cum - away_turnover_diff_cum,
    net_redzone = home_off_red_zone_app_perc_roll - away_def_red_zone_app_perc_roll
  )

# Filter to complete.cases
brms_seasons <- as.numeric(names(xgb_models_list))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Results and Total Model ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create rolling CV folds for wide data based on season:
brms_seasons <- sort(unique(brms_data$season))
brms_folds <- list()
for(i in seq(from = 4, to = length(brms_seasons))) {
  train_seasons <- brms_seasons[(i-3):(i-1)]
  test_season   <- brms_seasons[i]
  brms_folds[[as.character(test_season)]] <- list(
    train = brms_data |> filter(season %in% train_seasons),
    test  = brms_data |> filter(season == test_season)
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Function to Fit brms Model ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fit_brms_model <- function(train_data, 
                           formula_obj,
                           formula_prior = NULL,
                           ...) {
  model <- brm(
    formula = formula_obj,
    data = train_data,
    prior = formula_prior,
    #family = gaussian(),  # Adjust family if necessary
    save_pars = save_pars(all = TRUE),
    chains = 4, iter = 4000, warmup = 2000,
    cores = detectCores(),
    normalize = TRUE,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 52,
    ...
  )
  return(model)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 5. Fit brms Models across CV Folds ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

brms_models <- list()
brms_summaries <- list()

# Define model formulas for the outcomes.
# Build formula.
brms_formula_result <- 
  bf(
    result ~
      xgb_result + 
      net_elo +
      net_SRS +
      net_off_epa +
      net_def_epa +
      net_turnover_diff +
      net_redzone +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "gaussian")

brms_formula_total <- 
  bf(
    total ~ 
      xgb_total + 
      net_elo +
      net_SRS +
      net_off_epa +
      net_def_epa +
      net_turnover_diff +
      net_redzone +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "gaussian")

for(season in names(brms_folds)) {
  cat("Fitting brms models for test season:", season, "\n")
  train_brms <- brms_folds[[season]]$train
  model_result <- fit_brms_model(train_brms, brms_formula_result)
  model_total  <- fit_brms_model(train_brms, brms_formula_total)
  
  brms_models[[season]] <- list(result = model_result, total = model_total)
  brms_summaries[[season]] <- list(
    result = summary(model_result),
    total  = summary(model_total)
  )
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 6. Post Processing & Model Evaluation ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

performance_metrics <- list()

for(season in names(brms_folds)) {
  test_brms <- brms_folds[[season]]$test
  
  # Posterior predictions for the result model.
  pp_result <- posterior_predict(brms_models[[season]]$result, 
                                 newdata = test_brms,
                                 re_formula = NULL,
                                 allow_new_levels = TRUE)
  ci_result <- apply(pp_result, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_result <- mean(test_brms$result >= ci_result[1, ] & test_brms$result <= ci_result[2, ], na.rm = TRUE)
  rmse_result <- sqrt(mean((colMeans(pp_result) - test_brms$result)^2, na.rm = TRUE))
  
  pp_total <- posterior_predict(brms_models[[season]]$total, 
                                newdata = test_brms,
                                re_formula = NULL,
                                allow_new_levels = TRUE)
  ci_total <- apply(pp_total, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_total <- mean(test_brms$total >= ci_total[1, ] & test_brms$total <= ci_total[2, ], na.rm = TRUE)
  rmse_total <- sqrt(mean((colMeans(pp_total) - test_brms$total)^2, na.rm = TRUE))
  
  performance_metrics[[season]] <- list(
    result = list(coverage = coverage_result, RMSE = rmse_result, pp_result = pp_result),
    total = list(coverage = coverage_total, RMSE = rmse_total, pp_total = pp_total)
    # Similarly, compute metrics for the total model if desired.
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 7. PPC Plots ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppc_plots <- list()

# Generate PPC Plots for brms models.
for(season in names(brms_models)) {
  result_ppc <- pp_check(brms_models[[season]]$result, ndraws = 100) +
    ggtitle(paste(season))
    #ggtitle(paste("PPC Plot - Result Model (Season", season, ")"))
  total_ppc <- pp_check(brms_models[[season]]$total, ndraws = 100) +
    ggtitle(paste(season))
    #ggtitle(paste("PPC Plot - Total Model (Season", season, ")"))
  # Optionally, save these plots (e.g., with ggsave()).
  
  ppc_plots[[season]] <- list(
    result = result_ppc,
    total = total_ppc
    # Similarly, compute metrics for the total model if desired.
  )
}

wrap_plots(lapply(ppc_plots, "[[", "result"), guides = "collect")
wrap_plots(lapply(ppc_plots, "[[", "total"), guides = "collect")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BETTING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

betting_eval_fold <- function(fold_result,
                              threshold = 0.6) {
  test_df <- brms_folds[[season]]$test
  test_df$result_pred <- colMeans(fold_result$pp_result)
  
  test_df <- test_df %>%
    mutate(
      diff = result - spread_line,
      actual_cover = case_when(
        result > spread_line ~ "Home",
        result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      exp_result = xgb_result,
      exp_spread_line = spread_line,
      exp_diff = exp_result - exp_spread_line,
      exp_cover = case_when(
        exp_result > spread_line ~ "Home",
        exp_result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      correct_cover = (actual_cover == exp_cover)
    )
  
  acc_posterior_mean <- mean(test_df$correct_cover, na.rm = TRUE) * 100
  
  ppd_decision <- sweep(fold_result$pp_result, 2, test_df$spread_line, 
                        FUN = function(pred, line) {
                          ifelse(pred > line, "Home", ifelse(pred < line, "Away", NA))
                        })
  comparison_matrix <- sweep(ppd_decision, 2, test_df$actual_cover,
                             FUN = function(pred, actual) {
                               ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
                             })
  game_accuracy <- colMeans(comparison_matrix, na.rm = TRUE)
  acc_full <- mean(game_accuracy, na.rm = TRUE) * 100
  
  test_home_prob <- colMeans(ppd_decision == "Home", na.rm = TRUE)
  test_away_prob <- colMeans(ppd_decision == "Away", na.rm = TRUE)
  
  test_bet_side_vegas <- ifelse(test_home_prob > test_df$home_spread_prob, "Home",
                                ifelse(test_away_prob > test_df$away_spread_prob, "Away", NA))
  test_bet_vegas_correct <- ifelse(is.na(test_bet_side_vegas) | is.na(test_df$actual_cover),
                                   NA, test_bet_side_vegas == test_df$actual_cover)
  acc_vegas <- mean(test_bet_vegas_correct, na.rm = TRUE) * 100
  bet_vegas_count <- sum(!is.na(test_bet_vegas_correct))
  
  test_bet_side_thresh <- ifelse(test_home_prob > threshold, "Home",
                                 ifelse(test_away_prob > threshold, "Away", NA))
  test_bet_thresh_correct <- ifelse(is.na(test_bet_side_thresh) | is.na(test_df$actual_cover),
                                    NA, test_bet_side_thresh == test_df$actual_cover)
  acc_thresh <- mean(test_bet_thresh_correct, na.rm = TRUE) * 100
  bet_thresh_count <- sum(!is.na(test_bet_thresh_correct))
  
  test_df <- test_df %>%
    mutate(
      # xgb_home_score = predict(home_model, newdata = test_df),
      # xgb_away_score = predict(away_model, newdata = test_df),
      #xgb_result = xgb_home_score - xgb_away_score,
      xgb_spread_line = spread_line,
      xgb_diff = xgb_result - xgb_spread_line,
      xgb_cover = case_when(
        xgb_result > spread_line ~ "Home",
        xgb_result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      xgb_correct_cover = (actual_cover == xgb_cover)
    ) %>%
    mutate(
      # xgb_result2 = predict(xgb_result_model, newdata = test_df),
      xgb_result2 = xgb_home_score - xgb_away_score,
      xgb_spread_line2 = spread_line,
      xgb_diff2 = xgb_result2 - xgb_spread_line2,
      xgb_cover2 = case_when(
        xgb_result2 > spread_line ~ "Home",
        xgb_result2 < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      xgb_correct_cover2 = (actual_cover == xgb_cover2)
    )
  
  acc_xgb <- mean(test_df$xgb_correct_cover, na.rm = TRUE) * 100
  acc_xgb2 <- mean(test_df$xgb_correct_cover2, na.rm = TRUE) * 100
  
  return(list(
    test_df = test_df,
    acc_posterior_mean = acc_posterior_mean,
    acc_full = acc_full,
    acc_vegas = acc_vegas,
    bet_vegas_count = bet_vegas_count,
    acc_thresh = acc_thresh,
    bet_thresh_count = bet_thresh_count,
    acc_xgb = acc_xgb,
    acc_xgb2 = acc_xgb2
  ))
}

result_folds <- lapply(performance_metrics, "[[", "result")
total_folds <- lapply(performance_metrics, "[[", "total")

# Apply the betting evaluation to each fold.
betting_evals <- lapply(result_folds, function(fold) {
  betting_eval_fold(season_fold, 
                    threshold = 0.6)
})

map

betting_evals <- map_dfr(~ {
    season_data <- gameData |> filter(season == .x)
    season_data_list <- calc_elo_ratings(
      season_data,
      initial_elo = 1500,
      K = 20,
      home_advantage = 0,
      d = 400,
      apply_margin_multiplier = TRUE
    )
    season_data_list$elo_history
  })

betting_evals

# Extract and print metrics.
posterior_mean_accs <- sapply(betting_evals, function(x) x$acc_posterior_mean)
full_ppd_accs     <- sapply(betting_evals, function(x) x$acc_full)
vegas_accs        <- sapply(betting_evals, function(x) x$acc_vegas)
thresh_accs       <- sapply(betting_evals, function(x) x$acc_thresh)
xgb_accs          <- sapply(betting_evals, function(x) x$acc_xgb)
xgb2_accs         <- sapply(betting_evals, function(x) x$acc_xgb2)

cat("Posterior Mean Accuracy per fold (%):\n", round(posterior_mean_accs, 2), "\n")
cat("Full PPD Accuracy per fold (%):\n", round(full_ppd_accs, 2), "\n")
cat("Vegas-based Accuracy per fold (%):\n", round(vegas_accs, 2), "\n")
cat("Threshold-based Accuracy per fold (%):\n", round(thresh_accs, 2), "\n")
cat("XGB Accuracy (model 1) per fold (%):\n", round(xgb_accs, 2), "\n")
cat("XGB Accuracy (model 2) per fold (%):\n", round(xgb2_accs, 2), "\n")

accuracy_metrics_result_temp <- tibble(
  Fold = names(betting_evals),
  PostMean = posterior_mean_accs,
  PostFull = full_ppd_accs,
  BetVegas = vegas_accs,
  BetThresh = thresh_accs,
  XGB = xgb_accs,
  XGB2 = xgb2_accs
)

print(xgb_betting_accuracy, n = nrow(xgb_betting_accuracy))
print(accuracy_metrics_result_temp)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SAVE FINAL OUTPUTS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saveRDS(brms_models, file = "brms_models.rds")
saveRDS(performance_metrics, file = "brms_performance_metrics.rds")
saveRDS(brms_summaries, file = "brms_model_summaries.rds")
