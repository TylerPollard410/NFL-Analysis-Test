# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 0. SETUP & LIBRARIES ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# # Modeling libraries:
# library(caret)                # For preprocessing utilities like nearZeroVar and findLinearCombos
# library(xgboost)              # Native XGBoost training
# library(rBayesianOptimization)  # For Bayesian hyperparameter optimization
# #library(projpred)             # Optional variable selection
# library(brms)                 # Bayesian hierarchical modeling
# library(bayesplot)
# library(pROC)                 # For performance metrics
# library(parallel)             # For parallel processing
# 
# # General libraries for data manipulation, plotting and date handling
# library(data.table)
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
library(cmdstanr)
library(brms)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
library(broom.mixed)
library(tidybayes)
library(nflverse)
library(tidyverse)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))


load(file = "~/Desktop/NFL Analysis Data/modDataLong.rda")
load(file = "~/Desktop/NFL Analysis Data/modData.rda")

# Load the long-format data (scores model)
modData <- modData |>
  filter(season >= 2007)
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Load the wide-format data (for result/total models)
source(file = "./app/data-raw/modData.R")

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
  select(game_id, all_of(candidate_numeric_cols)) |>
  slice(candidate_numeric_NA)

NAcols <- apply(data_long_clean_NA, 2, function(x) sum(is.na(x)))
NAcols <- NAcols[NAcols != 0]
data_long_clean_NA <- data_long_clean_NA |> 
  select(game_id, names(NAcols))

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

### 5B. 
# for checking single fits as they populate
finished_models <- list.files("~/XGB/")
finished_models <- list.files("~/Desktop/NFL Analysis Data/XGBoost Historic Tune")
finished_models2 <- str_subset(finished_models, "forecast_results")
finished_models3 <- str_extract(finished_models2, "[:digit:]+")

finished_seasons <- as.numeric(finished_models3)
file_loc <- "~/XGB/"

xgb_models_list <- list()
best_feature_df <- data.frame()
for(s in finished_seasons){
  filename_results <- paste0(file_loc, "forecast_results_season_", s, ".rda")
  load(file = filename_results)
  
  filename_models <- paste0(file_loc, "forecast_model_season_", s, ".rda")
  load(file = filename_models)
  
  comb_list <- 
    xgb_models_list[[as.character(s)]] <- list_modify(results_list_temp, 
                                                      model = forecast_model)
  
  season_features <- results_list_temp$tuning$best_features
  feature_temp <- data.frame(
    Feature = season_features,
    Season = s
  )
  best_feature_df <- rbind(best_feature_df, feature_temp)
}
pipeline_results <- xgb_models_list
save(pipeline_results, file = "~/XGB/pipeline_results.rda")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 6. Betting Evaluation (XGB Model Raw) ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_forecasts <- lapply(pipeline_results, function(x) x[["forecasts"]])
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
xgb_performance_season <- xgb_preds_df %>%
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
  ) 
xgb_performance_season |> print(n = 50)

# Summarize performance by forecast week
xgb_performance_week <- xgb_preds_df %>%
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
  ) 
xgb_performance_week |> print(n = 50)



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

xgb_betting_accuracy_season <- evaluate_betting_performance(
  xgb_preds_df, 
  result_pred_col = "xgb_result",
  total_pred_col = "xgb_total",
  group_season = TRUE,
  group_week = FALSE
)
print(xgb_betting_accuracy_season, n = nrow(xgb_betting_accuracy_season))

xgb_betting_accuracy_season |> 
  left_join(xgb_performance_season |> select(-n))

xgb_betting_accuracy_week <- evaluate_betting_performance(
  xgb_preds_df, 
  result_pred_col = "xgb_result",
  total_pred_col = "xgb_total",
  group_season = FALSE,
  group_week = TRUE
)
print(xgb_betting_accuracy_week, n = nrow(xgb_betting_accuracy_week))

xgb_betting_accuracy_week |> 
  left_join(xgb_performance_week |> select(-n)) |>
  print(n = 22)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BRMS MODELING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 1. Get Best Features from XGB ---
# Extract feature importances from each model and bind into a single dataframe
feature_importance_df <- pipeline_results |>
  imap_dfr(\(res, season) {
    # Skip if model is missing (optional defensive check)
    if (is.null(res$model)) return(NULL)
    
    xgb.importance(model = res$model) |>
      as_tibble() |>
      mutate(season = as.integer(season))
  })

feature_importance_summary <- feature_importance_df |>
  group_by(Feature) |>
  summarise(
    seasons_used = n_distinct(season),
    mean_gain = mean(Gain),
    mean_cover = mean(Cover),
    mean_freq = mean(Frequency),
    .groups = "drop"
  ) |>
  arrange(desc(mean_gain))

season_feature_matrix <- pipeline_results |>
  imap_dfr(\(res, season) {
    tibble(
      season = as.integer(season),
      feature = res$tuning$best_features
    )
  }) |>
  mutate(used = 1) |>
  pivot_wider(names_from = season, values_from = used, values_fill = 0)

combined_features_df <- feature_importance_summary |>
  left_join(season_feature_matrix, by = c("Feature" = "feature"))
print(combined_features_df, n = nrow(combined_features_df))
head(combined_features_df, 20)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. Pre-Processing ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
brms_data_pre <- modData |>
  left_join(
    xgb_preds_df |> 
      select(game_id, home_team, away_team, contains("xgb")),
    by = join_by(game_id, home_team, away_team)
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, .after = result) |>
  relocate(xgb_total, .after = total)



# --- 1. Remove Near-Zero Variance Predictors ---
# Here, caret::nearZeroVar identifies variables with almost no variation.
nzv_info2 <- nearZeroVar(brms_data_pre, saveMetrics = TRUE)
nzv_cols2 <- rownames(nzv_info2)[nzv_info2$nzv]
cat("Removing near-zero variance columns:\n", paste(nzv_cols2, collapse = ", "), "\n")
data_clean <- brms_data_pre %>% select(-all_of(nzv_cols2))

# --- 2. Exclude Non-Predictor Columns ---
# We want to remove columns that are identifiers, outcomes, or betting/odds-related,
# but we want to keep key contextual variables such as:
#   • week – a time indicator (helps the model capture season progression)
#   • locationID – which serves as a proxy for home-field advantage
#   • div_game, temp, wind – as they might influence game performance.
#
# Define a vector of columns to exclude from the candidate set:
drop_vars <- c("game_id", "season_type", "home_team", "away_team",
               "home_score", "away_score", "result", "spread_line", "spreadCover",
               "total", "total_line", "totalCover", "winner", "away_spread_odds", "away_spread_prob",
               "home_spread_odds", "home_spread_prob", "over_odds", "over_prob",
               "under_odds", "under_prob", "away_moneyline", "away_moneyline_prob",
               "home_moneyline", "home_moneyline_prob", "overtime",
               "stadium", "home_coach", "away_coach")

# Define game-level variables (which we will exclude from XGBoost and later reintroduce in Bayesian modeling)
game_level_vars <- c(
  "season", "week", "weekday", "time_of_day", "location", "location2", "div_game", 
  "home_rest", "away_rest",
  "roof", "surface", "temp", "wind"
)

# --- 3. Select Candidate Numeric Predictors ---
# Get all numeric columns first
numeric_cols2 <- data_clean %>% select_if(is.numeric)

# Now remove the excluded columns from numeric predictors.
candidate_numeric2 <- numeric_cols2 %>% select(-any_of(drop_vars))

cat("Candidate predictor columns before redundancy removal:\n",
    paste(colnames(candidate_numeric2), collapse = ", "), "\n")
candidate_numeric_cols2 <- colnames(candidate_numeric2)
candidate_numeric_cols2

# In this candidate set we expect key variables like:
#   • week (if stored as numeric)
#   • locationID (a proxy for home advantage)
#   • div_game, temp, wind (if numeric)
# These remain because they are not included in exclude_cols.

# --- 4. Clean Candidate Predictors (Handle NA/Inf) ---
# Before checking for linear combinations, we need a complete numeric dataset.
candidate_numeric_NA2 <- which(!complete.cases(candidate_numeric2))
data_clean_NA <- data_clean |> 
  select(game_id, all_of(candidate_numeric_cols2)) |>
  slice(candidate_numeric_NA2)

NAcols2 <- apply(data_clean_NA, 2, function(x) sum(is.na(x)))
NAcols2 <- NAcols2[NAcols2 != 0]
data_clean_NA <- data_clean_NA |> 
  select(game_id, names(NAcols2))

candidate_numeric_clean2 <- candidate_numeric2 %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  filter(if_all(everything(), ~ is.finite(.)))

# --- 5. Remove Redundant (Linearly Dependent) Predictors ---
# Use caret::findLinearCombos to detect and remove any exact or near-exact linear combinations.
linCombos2 <- findLinearCombos(candidate_numeric_clean2)
if (!is.null(linCombos2$remove) && length(linCombos2$remove) > 0) {
  remove_lin2 <- colnames(candidate_numeric_clean2)[linCombos2$remove]
  cat("Removing linearly dependent columns:\n", paste(remove_lin2, collapse = ", "), "\n")
  candidate_numeric_clean2 <- candidate_numeric_clean2 %>% select(-all_of(remove_lin2))
} else {
  cat("No linear combinations detected.\n")
}
remove_lin2

# --- 6. Define the Final Candidate Feature Set ---
# This is the list of predictor names you intend to use in the model training.
candidate_features2 <- colnames(candidate_numeric_clean2)
cat("Final candidate predictor columns:\n", paste(candidate_features2, collapse = ", "), "\n")
candidate_features2

# Merge XGBoost predictions into wide data by game_id.
# (Ensure that xgb_predictions exist; if not, you may need to load them or run the xgb pipeline first.)



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
brms_seasons <- as.numeric(names(pipeline_results))
brms_data <- brms_data |> filter(season %in% brms_seasons)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Results and Total Model ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create rolling CV folds for wide data based on season:
brms_folds <- list()
for(i in seq(from = 4, to = length(brms_seasons))) {
  train_seasons <- brms_seasons[(i-3):(i-1)]
  test_season   <- brms_seasons[i]
  
  train <- brms_data |> filter(season %in% train_seasons)
  test <- brms_data |> filter(season == test_season)
  
  preProc <- preProcess(
    train |> select(all_of(candidate_numeric_cols2), net_elo),
    method = c("center", "scale")
  )
  train_pre <- predict(preProc, train)
  test_pre <- predict(preProc, test)
  
  brms_folds[[as.character(test_season)]] <- list(
    train = train,
    test  = test,
    train_pre = train_pre,
    test_pre = test_pre
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
    cores = parallel::detectCores(),
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
      # xgb_result + 
      xgb_away_score +
      xgb_home_score +
      net_elo +
      week +
      home_SRS_cum +
      away_SRS_cum +
      home_off_epa_mean_cum +
      away_off_epa_mean_cum +
      week:home_SRS_cum +
      week:away_SRS_cum +
      week:home_off_epa_mean_cum +
      week:away_off_epa_mean_cum +
      # net_SRS +
      # net_off_epa +
      # net_def_epa +
      # net_turnover_diff +
      # net_redzone +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "student")

brms_formula_total <- 
  bf(
    total ~ 
      # xgb_total + 
      xgb_away_score +
      xgb_home_score +
      week +
      home_SRS_cum +
      away_SRS_cum +
      home_off_epa_mean_cum +
      away_off_epa_mean_cum +
      week:home_SRS_cum +
      week:away_SRS_cum +
      week:home_off_epa_mean_cum +
      week:away_off_epa_mean_cum +
      # net_SRS +
      # net_off_epa +
      # net_def_epa +
      # net_turnover_diff +
      # net_redzone +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "student")

brms_formula_home <- 
  bf(
    home_score ~
      # xgb_result + 
      net_elo +
      # net_SRS +
      # net_off_epa +
      # net_def_epa +
      # net_turnover_diff +
      # net_redzone +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "gaussian")

brms_formula_away <- 
  bf(
    away_score ~ 
      # xgb_total + 
      net_elo +
      # net_SRS +
      # net_off_epa +
      # net_def_epa +
      # net_turnover_diff +
      # net_redzone +
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

# Assuming brms_models is your nested list
brms_model_summary_df <- brms_models |>
  imap_dfr(~ {
    season <- .y
    imap_dfr(.x, ~ {
      model <- .x
      outcome <- .y
      
      tidy(model, effects = "fixed", conf.int = TRUE) |>
        mutate(
          season = season,
          outcome = outcome,
          effect_type = "fixed"
        ) |>
        bind_rows(
          tidy(model, effects = "ran_pars", conf.int = TRUE) |>
            mutate(
              season = season,
              outcome = outcome,
              effect_type = "ran_pars"
            )
        )
    })
  }) |> filter(outcome == "result")

#print(brms_model_summary_df, n = nrow(brms_model_summary_df))

brms_models_result <- lapply(brms_models, "[[", "result")
brms_models_total <- lapply(brms_models, "[[", "total")

brms_model_result_combined <- combine_models(brms_models_result$`2023`,
                                             brms_models_result$`2024`)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 6. Post Processing & Model Evaluation ----
# %%%%%%%%%%%%%%%

performance_metrics <- imap(brms_folds, \(fold, season) {
  test_brms <- fold$test_pre
  
  # Result model
  pp_result <- posterior_predict(
    brms_models[[season]]$result,
    newdata = test_brms,
    re_formula = NULL,
    allow_new_levels = TRUE
  )
  ci_result <- apply(pp_result, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_result <- mean(test_brms$result >= ci_result[1, ] & test_brms$result <= ci_result[2, ], na.rm = TRUE)
  rmse_result <- sqrt(mean((colMeans(pp_result) - test_brms$result)^2, na.rm = TRUE))
  
  # Total model
  pp_total <- posterior_predict(
    brms_models[[season]]$total,
    newdata = test_brms,
    re_formula = NULL,
    allow_new_levels = TRUE
  )
  ci_total <- apply(pp_total, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_total <- mean(test_brms$total >= ci_total[1, ] & test_brms$total <= ci_total[2, ], na.rm = TRUE)
  rmse_total <- sqrt(mean((colMeans(pp_total) - test_brms$total)^2, na.rm = TRUE))
  
  # Return result for this season
  list(
    result = list(
      coverage = coverage_result,
      RMSE = rmse_result,
      pp_result = pp_result,
      game_ids = test_brms$game_id
    ),
    total = list(
      coverage = coverage_total,
      RMSE = rmse_total,
      pp_total = pp_total,
      game_ids = test_brms$game_id
    )
  )
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 7. PPC Plots ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Generate PPC Plots for brms models.
ppc_plots <- imap(brms_models, \(model, season) {
  result_ppc <- pp_check(model$result, ndraws = 100) +
    ggtitle(season)
  
  total_ppc <- pp_check(model$total, ndraws = 100) +
    ggtitle(season)
  
  list(
    result = result_ppc,
    total = total_ppc
  )
})

wrap_plots(lapply(ppc_plots, "[[", "result"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of result for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

wrap_plots(lapply(ppc_plots, "[[", "total"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of total for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BETTING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
betting_vars <- c("spread_line", "spreadCover", 
                  "home_spread_odds", "home_spread_prob",
                  "away_spread_odds", "away_spread_prob",
                  "total_line", "totalCover",
                  "over_odds", "over_prob",
                  "under_odds", "under_prob",
                  "winner", 
                  "home_moneyline", "home_moneyline_prob",
                  "away_moneyline", "away_moneyline_prob")

aggregate_posterior_predictions <- function(performance_metrics,
                                            target = c("result", "total")) {
  target <- match.arg(target)
  posterior_col <- if (target == "result") "pp_result" else "pp_total"
  
  # Collect matrices and annotate with season + column index
  extracted <- imap(performance_metrics, \(season_data, season) {
    target_list <- season_data[[target]]
    posterior_matrix <- target_list[[posterior_col]]
    if (is.null(posterior_matrix)) return(NULL)
    
    num_games <- ncol(posterior_matrix)
    
    list(
      matrix = posterior_matrix,
      game_info = tibble(
        season = as.integer(season),
        game_index = seq_len(num_games),
        game_id = target_list[["game_ids"]]
      )
    )
  })
  
  # Filter NULLs
  extracted <- compact(extracted)
  
  # Combine matrices
  full_matrix <- map(extracted, "matrix") |>
    reduce(cbind)
  
  # Combine game info
  game_info <- map_dfr(extracted, "game_info")
  
  return(list(
    posterior = full_matrix,  # draws x all games
    game_info = game_info     # maps each column to season + index
  ))
}

compute_betting_accuracy <- function(posterior_matrix,
                                     game_data,
                                     target = c("result", "total"),
                                     vegas_line_col = NULL,
                                     vegas_prob_col1 = NULL,
                                     vegas_prob_col2 = NULL,
                                     actual_col = NULL,
                                     xgb_pred_col = NULL,
                                     prob_threshold = 0.6,
                                     group_vars = NULL) {
  target <- match.arg(target)
  
  # Target-specific label choices
  target_labels <- if (target == "result") c("Home", "Away") else c("Over", "Under")
  
  vegas_line_col <- vegas_line_col %||% ifelse(target == "result", "spread_line", "total_line")
  vegas_prob_col1 <- vegas_prob_col1 %||% ifelse(target == "result", "home_spread_prob", "over_prob")
  vegas_prob_col2 <- vegas_prob_col1 %||% ifelse(target == "result", "away_spread_prob", "under_prob")
  actual_col     <- actual_col     %||% target
  xgb_pred_col   <- xgb_pred_col   %||% paste0("xgb_", target)
  
  # Match posterior columns to game data
  df <- game_data |>
    mutate(
      posterior_mean = colMeans(posterior_matrix, na.rm = TRUE),
      actual_cover = case_when(
        .data[[actual_col]] > .data[[vegas_line_col]] ~ target_labels[1],
        .data[[actual_col]] < .data[[vegas_line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      predicted_cover_mean = case_when(
        posterior_mean > .data[[vegas_line_col]] ~ target_labels[1],
        posterior_mean < .data[[vegas_line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      correct_cover_mean = predicted_cover_mean == actual_cover
    )
  
  # Full posterior coverage decisions
  df <- df |>
    mutate(
      predicted_covers = map2(
        .x = asplit(posterior_matrix, 2),
        .y = .data[[vegas_line_col]],
        \(draws, line) {
          ifelse(draws > line, target_labels[1],
                 ifelse(draws < line, target_labels[2], NA_character_))
        }
      ),
      correct_posterior = map2_dbl(predicted_covers, actual_cover, \(preds, actual) {
        mean(preds == actual, na.rm = TRUE)
      })
    )
  
  # Vegas-based decision (only bet if confident enough)
  df <- df |>
    mutate(
      vegas_prob_side1 = map_dbl(predicted_covers, ~ mean(.x == target_labels[1], na.rm = TRUE)),
      vegas_prob_side2 = map_dbl(predicted_covers, ~ mean(.x == target_labels[2], na.rm = TRUE)),
      vegas_bet = case_when(
        vegas_prob_side1 > .data[[vegas_prob_col1]] ~ target_labels[1],
        vegas_prob_side2 > .data[[vegas_prob_col2]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      vegas_correct = vegas_bet == actual_cover
    )
  
  # Threshold-based decision (only bet if confident enough)
  df <- df |>
    mutate(
      threshold_prob_side1 = map_dbl(predicted_covers, ~ mean(.x == target_labels[1], na.rm = TRUE)),
      threshold_prob_side2 = map_dbl(predicted_covers, ~ mean(.x == target_labels[2], na.rm = TRUE)),
      threshold_bet = case_when(
        threshold_prob_side1 > prob_threshold ~ target_labels[1],
        threshold_prob_side2 > prob_threshold ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      threshold_correct = threshold_bet == actual_cover
    )
  
  # XGBoost prediction (optional)
  if (xgb_pred_col %in% colnames(df)) {
    df <- df |>
      mutate(
        xgb_cover = case_when(
          .data[[xgb_pred_col]] > .data[[vegas_line_col]] ~ target_labels[1],
          .data[[xgb_pred_col]] < .data[[vegas_line_col]] ~ target_labels[2],
          TRUE ~ NA_character_
        ),
        xgb_correct = xgb_cover == actual_cover
      )
  }
  
  # Group if needed
  grouped_df <- if (!is.null(group_vars)) df |> group_by(across(all_of(group_vars))) else df
  
  # Flag if xgb column exists
  has_xgb <- "xgb_correct" %in% colnames(df)
  
  # Summary
  summary <- grouped_df |>
    summarise(
      target = target,
      games = n(),
      PostMean_Acc = mean(correct_cover_mean, na.rm = TRUE) * 100,
      PostFull_Acc = mean(correct_posterior, na.rm = TRUE) * 100,
      Vegas_Acc = mean(vegas_correct, na.rm = TRUE) * 100,
      Vegas_Bets = sum(!is.na(vegas_correct)),
      Thresh_Acc = mean(threshold_correct, na.rm = TRUE) * 100,
      Thresh_Bets = sum(!is.na(threshold_correct)),
      Thresh = prob_threshold,
      XGB_Acc = if (has_xgb) mean(xgb_correct, na.rm = TRUE) * 100 else NA_real_,
      XGB_Bets = if (has_xgb) sum(!is.na(xgb_correct)) else NA_integer_,
      .groups = "drop"
    )
  
  return(summary)
}

## Generate Full Posteriors ----
# Generate full-season aggregated posterior for result predictions
agg_result <- aggregate_posterior_predictions(performance_metrics, target = "result")
agg_total  <- aggregate_posterior_predictions(performance_metrics, target = "total")

# Join this with full game data (vegas lines, scores, etc.)
result_input_df <- agg_result$game_info |> left_join(brms_data, by = c("game_id", "season"))
total_input_df  <- agg_total$game_info  |> left_join(brms_data, by = c("game_id", "season"))

# Compute betting accuracy
run_betting_accuracy_for_targets <- function(performance_metrics,
                                             brms_data,
                                             targets = c("result", "total"),
                                             prob_threshold = 0.6,
                                             out_format = "long",
                                             group_vars = "season") {
  summary_df <- map_dfr(targets, \(target_type) {
    # Aggregate posterior for this target
    agg <- aggregate_posterior_predictions(performance_metrics, target = target_type)
    
    # Join game info with game-level data
    df <- agg$game_info |> 
      left_join(brms_data, by = c("game_id", "season"))
    
    # Compute betting accuracy
    compute_betting_accuracy(
      posterior_matrix = agg$posterior,
      game_data = df,
      target = target_type,
      prob_threshold = prob_threshold,
      group_vars = group_vars
    ) |> 
      mutate(target = target_type)
  })
  if(out_format == "wide"){
    summary_df <- summary_df |> pivot_wider(names_from = target, values_from = c(PostMean_Acc:XGB_Bets))
  }
  return(summary_df)
}

## Combine Output -----
#betting_summary_model_comp <- list()
model_fit <- 2
betting_summary_model_comp[[model_fit]] <- list(
  overall = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = NULL #c("season", "week")
  ), # |> mutate(Fit = 1, .before = 1),
  season = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = "season" #c("season", "week")
  ), # |> mutate(Fit = 1, .before = 1),
  week = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = "week"
  ), # |> mutate(Fit = 1, .before = 1)
  brms_formula_result = brms_formula_result,
  brms_formula_total = brms_formula_total
)


# betting_summary_overall <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = NULL #c("season", "week")
# )

betting_summary_overall <- betting_summary_model_comp |>
  map("overall") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, Fit)
betting_summary_overall

best_result_bet_model <- betting_summary_overall |>
  filter(target == "result") |>
  arrange(desc(Thresh_Acc)) |>
  select(Fit, target, Games = games, Thresh_Acc, Thresh_Bets, XGB_Acc, XGB_Bets) |>
  mutate(
    Thresh_Exp_Wins = Thresh_Acc*Thresh_Bets/100,
    Thresh_Exp_Loss = (1-Thresh_Acc/100)*Thresh_Bets, 
    Thresh_Exp_Pay = (Thresh_Exp_Wins*0.91 - Thresh_Exp_Loss)*100,
    .after = Thresh_Bets
  ) |>
  mutate(
    XGB_Exp_Wins = XGB_Acc*XGB_Bets/100,
    XGB_Exp_Loss = (1-XGB_Acc/100)*XGB_Bets,
    XGB_Exp_Pay = (XGB_Exp_Wins*0.91 - XGB_Exp_Loss)*100,
  ) |>
  arrange(desc(Thresh_Exp_Pay))

best_result_bet_model

best_total_bet_model <- betting_summary_overall |>
  filter(target == "total") |>
  arrange(desc(Thresh_Acc)) |>
  select(Fit, target, Games = games, Thresh_Acc, Thresh_Bets, XGB_Acc, XGB_Bets) |>
  mutate(
    Thresh_Exp_Wins = Thresh_Acc*Thresh_Bets/100,
    Thresh_Exp_Loss = (1-Thresh_Acc/100)*Thresh_Bets, 
    Thresh_Exp_Pay = (Thresh_Exp_Wins*0.91 - Thresh_Exp_Loss)*100,
    .after = Thresh_Bets
  ) |>
  mutate(
    XGB_Exp_Wins = XGB_Acc*XGB_Bets/100,
    XGB_Exp_Loss = (1-XGB_Acc/100)*XGB_Bets,
    XGB_Exp_Pay = (XGB_Exp_Wins*0.91 - XGB_Exp_Loss)*100,
  ) |>
  arrange(desc(Thresh_Exp_Pay))
best_total_bet_model


# betting_summary_season <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = "season" #c("season", "week")
# )

betting_summary_season <- betting_summary_model_comp |>
  map("season") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, season, Fit)
print(betting_summary_season, n = nrow(betting_summary_season))

# betting_summary_week <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = "week"
# )

betting_summary_week <- betting_summary_model_comp |>
  map("week") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, week, Fit)
print(betting_summary_week, n = nrow(betting_summary_week))

## Return on Investment -----

compute_roi_table <- function(posterior_matrix, new_data,
                              target = c("result", "total"),
                              threshold_grid = seq(0.50, 0.70, by = 0.01),
                              odds = -110) {
  target <- match.arg(target)
  
  # Define the labels and relevant columns based on target type.
  target_labels <- if (target == "result") c("Home", "Away") else c("Over", "Under")
  line_col <- if (target == "result") "spread_line" else "total_line"
  actual_col <- if (target == "result") "result" else "total"
  
  # Calculate payout ratio from American odds (for -110, win nets ~0.91 profit).
  payout_ratio <- ifelse(odds < 0, 100 / abs(odds), odds / 100)
  
  # Compute cover probabilities for each game.
  # Note: We assume posterior_matrix is organized as draws x games.
  new_data <- new_data %>%
    mutate(
      home_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] > .data[[line_col]][.x])),
      away_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] < .data[[line_col]][.x]))
    ) %>%
    mutate(
      actual_cover = case_when(
        .data[[actual_col]] > .data[[line_col]] ~ target_labels[1],
        .data[[actual_col]] < .data[[line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      )
    )
  
  # Loop over the threshold grid to determine betting performance.
  roi_table <- map_dfr(threshold_grid, function(thresh) {
    temp <- new_data %>%
      mutate(
        # Apply threshold-based decision logic:
        # If the home cover probability exceeds the threshold and is greater than the away probability, bet "Home".
        # If the away cover probability exceeds the threshold and is greater than the home probability, bet "Away".
        bet_choice = case_when(
          home_cover_prob > thresh & home_cover_prob > away_cover_prob ~ target_labels[1],
          away_cover_prob > thresh & away_cover_prob > home_cover_prob ~ target_labels[2],
          TRUE ~ NA_character_
        ),
        # Check if the bet was correct.
        bet_hit = if_else(bet_choice == actual_cover, 1, 0, missing = NA_integer_),
        # Calculate payout: win returns payout_ratio, loss returns -1, no bet returns 0.
        bet_payout = case_when(
          is.na(bet_choice) ~ 0,
          bet_hit == 1 ~ payout_ratio,
          bet_hit == 0 ~ -1
        )
      )
    
    bets_placed <- sum(!is.na(temp$bet_choice))
    roi_value <- if (bets_placed > 0) sum(temp$bet_payout, na.rm = TRUE) / bets_placed else NA_real_
    accuracy_value <- if (bets_placed > 0) mean(temp$bet_hit, na.rm = TRUE) else NA_real_
    
    tibble(
      threshold   = thresh,
      bets_placed = bets_placed,
      roi         = roi_value,
      accuracy    = accuracy_value
    )
  })
  
  return(roi_table)
}

roi_table <- compute_roi_table(
  posterior_matrix = agg_result$posterior, 
  new_data = brms_data,
  target = "result")
print(roi_table, n =nrow(roi_table))

# Finally, plot ROI vs. threshold
ggplot(roi_table, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold",
    x = "Posterior Threshold",
    y = "ROI"
  )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SAVE FINAL OUTPUTS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saveRDS(brms_models, file = "brms_models.rds")
saveRDS(performance_metrics, file = "brms_performance_metrics.rds")
saveRDS(brms_summaries, file = "brms_model_summaries.rds")
