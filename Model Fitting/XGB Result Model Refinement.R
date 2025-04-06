
# 0. Setup & Libraries ------------------------------
library(readr)
library(tidytext)
library(plotly)
library(patchwork)
library(doParallel)
library(xgboost)
library(caret)
library(brms)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
library(zoo)      # for rolling averages
library(nflverse)
library(tidyverse)

set.seed(123)


# 1. Load & Prepare Data ------------------------------
# Read in the feature-engineered CSV file (ensure "modData.csv" is in your working directory)
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
#write_csv(modData, file = "~/Desktop/NFL Analysis Data/modData.csv")

source("./app/R/clean_modData.R")
nfl_data <- clean_modData(data = modData, season_start = 2007)
#write_csv(nfl_data, file = "~/Desktop/NFL Analysis Data/nfl_data.csv")

# nfl_data <- read_csv("~/Desktop/modData.csv")
# 
# # Include both regular season and postseason games.
# # Create a postseason indicator: 0 for regular season, 1 for postseason.
# nfl_data <- nfl_data |> 
#   mutate(postseason = ifelse(season_type == "REG", 0, 1))
# 
# # Order data by season and week (assumes week is available for both regular and postseason games)
# nfl_data <- nfl_data |> arrange(season, week)
# 
# # Create additional net features if not already available (e.g., net_SRS and net_off_epa)
# if(!"net_SRS" %in% names(nfl_data)){
#   nfl_data <- nfl_data |> mutate(net_SRS = home_SRS - away_SRS)
# }
# if(!"net_off_epa" %in% names(nfl_data)){
#   nfl_data <- nfl_data |> mutate(net_off_epa = home_off_epa_cum - away_def_epa_cum)
# }

# Create an overall "time" identifier for ordering (combining season and week)
nfl_data <- nfl_data |> mutate(time_id = as.numeric(paste0(season, sprintf("%02d", week))))


# 2. Define Candidate Predictors for XGBoost (Team-Specific) ------------------------------
# Define a set of columns to drop (identifiers, outcomes, betting info, etc.)
drop_vars <- c("game_id", "season", "season_type", "week", "home_team", "away_team",
               "home_score", "away_score", "result", "spread_line", "spreadCover",
               "total", "total_line", "totalCover", "winner", "away_spread_odds", "away_spread_prob",
               "home_spread_odds", "home_spread_prob", "over_odds", "over_prob",
               "under_odds", "under_prob", "away_moneyline", "away_moneyline_prob",
               "home_moneyline", "home_moneyline_prob", "stadium", "home_coach", "away_coach",
               "time_id")

# Define game-level variables (which we will exclude from XGBoost and later reintroduce in Bayesian modeling)
game_level_vars <- c(
  "weekday", "time_of_day", "location", "location2", "div_game", 
  "home_rest", "away_rest",
  "roof", "surface", "temp", "wind"
)

# Candidate predictors for XGBoost: all numeric variables not in drop_vars and not in game_level_vars.
all_candidates <- setdiff(names(nfl_data), drop_vars)
candidate_xgb_vars <- all_candidates[ sapply(nfl_data[, all_candidates], is.numeric) ]
candidate_xgb_vars <- setdiff(candidate_xgb_vars, game_level_vars)
setdiff(all_candidates, candidate_xgb_vars)

cat("Team-specific candidate predictors for XGBoost:\n")
print(candidate_xgb_vars)

## 2.A. Preprocessing: ----
candidate_data <- nfl_data |> select(candidate_xgb_vars)

# Keep only complete.cases
candidate_data <- candidate_data |> filter(complete.cases(candidate_data))

### 2.A.1. Linear Combos ----
# Identify linear combinations among the predictors
combo_info <- findLinearCombos(candidate_data)
if (!is.null(combo_info$remove)) {
  cat("Removing the following predictors due to linear dependency:\n")
  print(candidate_xgb_vars[combo_info$remove])
  
  # Remove the predictors identified as redundant
  candidate_xgb_vars_clean <- candidate_xgb_vars[-combo_info$remove]
} else {
  cat("No linear dependencies found.\n")
  candidate_xgb_vars_clean <- candidate_xgb_vars
}
candidate_xgb_vars_clean

### 2.A.2. Near-zero variance ----
nzv <- nearZeroVar(nfl_data[, candidate_xgb_vars_clean])
if (length(nzv) > 0) {
  cat("Removing the following near-zero variance predictors:\n")
  print(candidate_xgb_vars_clean[nzv])
  
  candidate_xgb_vars_clean <- candidate_xgb_vars_clean[-nzv]
} else {
  cat("No near-zero variance predictors found.\n")
}

# Now, candidate_xgb_vars_clean contains the names of the predictors 
# to be used in your XGBoost modeling.
cat("Final candidate predictors for XGBoost:\n")
print(candidate_xgb_vars_clean)

### 2.A.3. Correlation ----
# # Remove highly correlated predictors (cutoff = 0.95)
# cor_matrix <- cor(nfl_data[, candidate_xgb_vars_clean], use = "pairwise.complete.obs")
# high_corr <- findCorrelation(cor_matrix, cutoff = 0.95) #0.99
# candidate_xgb_vars_clean[high_corr]
# if(length(high_corr) > 0) {
#   candidate_xgb_vars_corr <- candidate_xgb_vars_clean[-high_corr]
# }
# 
# preProcCorr <- preProcess(
#   nfl_data |> select(candidate_xgb_vars_clean),
#   method = c("corr")
# )
# preProcCorr_vars <- preProcCorr$method$remove
# preProcCorr_vars
# setdiff(preProcCorr_vars, candidate_xgb_vars_clean[high_corr])
# 
# candidate_xgb_vars <- candidate_xgb_vars_corr
# cat("After correlation filtering:\n")
# print(candidate_xgb_vars)

# 3. Define CV Indicies for XGBoost ------------------------------
nfl_data_model <- nfl_data |>
  select(time_id, everything()) |>
  arrange(season, week, season_type)

## 3.A. Expanding Window ----
### 3.A.1. Per Week ----
# # Create a numeric "time_id" if not already present (e.g. 201601, 201602, etc.)
# #    You said you already have something like this, but for clarity:
# # nfl_data_model <- nfl_data_model |>
# #   mutate(time_id = as.numeric(paste0(season, sprintf("%02d", week))))
# 
# # Define the initial training cutoff (use 2007-2010 as your first training block)
# initial_train_cutoff <- 2009
# 
# # Identify all seasons (beyond the cutoff) we want to iterate over
# seasons_after_cutoff <- nfl_data_model |>
#   filter(season > initial_train_cutoff) |>
#   distinct(season) |>
#   pull(season) |>
#   sort()
# 
# # Prepare lists to store training/test indices for each fold
# train_indices <- list()
# test_indices  <- list()
# 
# # Loop through each season after the cutoff
# for (s in seasons_after_cutoff) {
#   
#   # Handle Regular Season Weeks for Season s 
#   # Extract all unique REG weeks for season s
#   reg_weeks <- nfl_data_model |>
#     filter(season == s, season_type == "REG") |>
#     distinct(week, time_id) |>
#     arrange(week)
#   
#   # For each regular-season week, define a fold
#   for (i in seq_len(nrow(reg_weeks))) {
#     current_week   <- reg_weeks$week[i]
#     current_timeid <- reg_weeks$time_id[i]
#     
#     # Training set: all data with time_id < current_timeid
#     train_idx <- which(nfl_data_model$time_id < current_timeid)
#     
#     # Test set: rows for (season == s, week == current_week, season_type == "REG")
#     test_idx  <- which(
#       nfl_data_model$season == s &
#         nfl_data_model$week == current_week &
#         nfl_data_model$season_type == "REG"
#     )
#     
#     if (length(train_idx) > 0 && length(test_idx) > 0) {
#       train_indices[[length(train_indices) + 1]] <- train_idx
#       test_indices[[length(test_indices) + 1]]  <- test_idx
#     }
#   }
#   
#   # Handle Postseason for Season s
#   # Check if there are any POST games for season s
#   post_games <- nfl_data_model |>
#     filter(season == s, season_type != "REG")
#   
#   if (nrow(post_games) > 0) {
#     # If postseason exists, treat all POST games of season s as one fold
#     first_post_timeid <- min(post_games$time_id)
#     
#     # Training set: all data with time_id < first_post_timeid
#     train_idx <- which(nfl_data_model$time_id < first_post_timeid)
#     
#     # Test set: all postseason rows for season s
#     test_idx <- which(
#       nfl_data_model$season == s &
#         nfl_data_model$season_type != "REG"
#     )
#     
#     if (length(train_idx) > 0 && length(test_idx) > 0) {
#       train_indices[[length(train_indices) + 1]] <- train_idx
#       test_indices[[length(test_indices) + 1]]  <- test_idx
#     }
#   }
# }
# 
# # Package the train/test indices into the format caret expects
# time_slices_expanding_week <- list(train = train_indices, test = test_indices)
# 
# # Check how many folds we have
# cat("Total number of folds:", length(time_slices_expanding_week$train), "\n")
# 
# # Example of how many observations in the first fold
# fn <- 274
# cat("Fold", fn, ": Train obs =", length(time_slices_expanding_week$train[[fn]]),
#     "Test obs =", length(time_slices_expanding_week$test[[fn]]), "\n")
# 
# # Create trainControl with custom CV splits
# cv_control_expanding_week <- trainControl(
#   method = "cv",
#   index = time_slices_expanding_week$train,
#   indexOut = time_slices_expanding_week$test,
#   summaryFunction = defaultSummary,
#   savePredictions = "final",
#   verboseIter = TRUE
# )

### 3.A.2. Per Season ----
# Define the seasons for cross-validation. We'll test on seasons 2010 to 2024.
all_seasons <- sort(unique(nfl_data_model$season))
cv_seasons <- all_seasons[all_seasons >= 2010]  # 2010 through 2024

# Initialize lists to hold training and test indices for the expanding window CV
train_indices_exp_season <- list()
test_indices_exp_season  <- list()

for (s in cv_seasons) {
  # Training: all games from seasons earlier than s
  train_idx <- which(nfl_data_model$season < s)
  # Test: all games from season s
  test_idx <- which(nfl_data_model$season == s)
  
  # Only add the fold if training data exists
  if(length(train_idx) > 0 && length(test_idx) > 0) {
    train_indices_exp_season[[as.character(s)]] <- train_idx
    test_indices_exp_season[[as.character(s)]] <- test_idx
    cat(sprintf("Expanding CV: Season %d -> Train: %d, Test: %d\n", 
                s, length(train_idx), length(test_idx)))
  }
}

time_slices_expanding_season <- list(train = train_indices_exp_season, 
                                     test = test_indices_exp_season)

cv_control_expanding_season <- trainControl(
  method = "cv",
  index = time_slices_expanding_season$train,
  indexOut = time_slices_expanding_season$test,
  summaryFunction = defaultSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

save(cv_control_expanding_season,
     file = "~/Desktop/NFL Analysis Data/cv_control_expanding_season.rda")

# Inspect the number of folds:
cat("Total Expanding CV folds:", length(time_slices_expanding_season$train), "\n")


## 3.B. Rolling Window ----
### 3.B.1. Per Week ----
# # Define rolling window parameters:
# rolling_window <- 4            # number of seasons to use for training
# initial_train_cutoff <- 2009   # we start testing on seasons after 2010
# 
# # Assume nfl_data_model already has a numeric time_id column (e.g., 201001, 201002, …)
# 
# # Prepare lists to store rolling CV fold indices
# train_indices_rolling <- list()
# test_indices_rolling  <- list()
# 
# # Identify the seasons to test (all seasons after the initial cutoff)
# seasons_to_test <- nfl_data_model |>
#   filter(season > initial_train_cutoff) |>
#   distinct(season) |>
#   arrange(season) |>
#   pull(season)
# 
# # Loop through each season in which we will create folds
# for (s in seasons_to_test) {
#   
#   # Limit training data to the most recent 'rolling_window' seasons
#   min_season_allowed <- s - rolling_window + 1
#   
#   # Part A: Regular Season Folds
#   # Get unique regular-season weeks for season s
#   reg_weeks <- nfl_data_model |>
#     filter(season == s, season_type == "REG") |>
#     distinct(week, time_id) |>
#     arrange(week)
#   
#   # For each regular season week, create a fold:
#   for (i in seq_len(nrow(reg_weeks))) {
#     current_week <- reg_weeks$week[i]
#     current_timeid <- reg_weeks$time_id[i]
#     
#     # Training set: rows with season >= min_season_allowed and time_id < current_timeid
#     train_idx <- which(nfl_data_model$season >= min_season_allowed &
#                          nfl_data_model$time_id < current_timeid)
#     
#     # Test set: regular season games for season s and current_week
#     test_idx <- which(nfl_data_model$season == s &
#                         nfl_data_model$week == current_week &
#                         nfl_data_model$season_type == "REG")
#     
#     if (length(train_idx) > 0 && length(test_idx) > 0) {
#       train_indices_rolling[[length(train_indices_rolling) + 1]] <- train_idx
#       test_indices_rolling[[length(test_indices_rolling) + 1]] <- test_idx
#     }
#   }
#   
#   # Part B: Postseason Fold for Season s
#   # Instead of splitting postseason into multiple weeks, treat all POST games for season s as one fold.
#   post_games <- nfl_data_model |>
#     filter(season == s, season_type != "REG")
#   
#   if (nrow(post_games) > 0) {
#     # Use the earliest time_id among POST games to define the cutoff.
#     first_post_timeid <- min(post_games$time_id)
#     
#     train_idx <- which(nfl_data_model$season >= min_season_allowed &
#                          nfl_data_model$time_id < first_post_timeid)
#     
#     # Test set: all postseason games for season s
#     test_idx <- which(nfl_data_model$season == s & nfl_data_model$season_type != "REG")
#     
#     if (length(train_idx) > 0 && length(test_idx) > 0) {
#       train_indices_rolling[[length(train_indices_rolling) + 1]] <- train_idx
#       test_indices_rolling[[length(test_indices_rolling) + 1]] <- test_idx
#     }
#   }
# }
# 
# # Package the indices into a list for caret
# time_slices_rolling <- list(train = train_indices_rolling, test = test_indices_rolling)
# 
# cat("Total rolling folds:", length(time_slices_rolling$train), "\n")
# 
# fn <- 274
# cat("Fold", fn, ": Train obs =", length(time_slices_rolling$train[[fn]]),
#     "Test obs =", length(time_slices_rolling$test[[fn]]), "\n")
# 
# # Set up caret's trainControl using these rolling window indices
# cv_control_rolling <- trainControl(
#   method = "cv",
#   index = time_slices_rolling$train,
#   indexOut = time_slices_rolling$test,
#   summaryFunction = defaultSummary,
#   savePredictions = "final",
#   verboseIter = TRUE
# )
# 
# # Inspect the trainControl object
# #print(cv_control_rolling)
# 
# # Then, for example, when training your XGBoost model you might do:
# # set.seed(123)
# # xgb_model <- train(
# #   x = nfl_data_model[, candidate_xgb_vars_clean],  # your predictors
# #   y = nfl_data$target,  # replace with your actual outcome variable
# #   method = "xgbTree",
# #   trControl = cv_control,
# #   tuneLength = 5
# # )

### 3.B.2. Per Season ----
# Define rolling window parameters:
# We'll use only the three seasons immediately preceding the test season for training.
rolling_window_season <- 3
#initial_train_cutoff <- 2009   # we start testing on seasons after 2010

# Initialize lists for rolling window CV indices
train_indices_roll_season <- list()
test_indices_roll_season  <- list()

for (s in cv_seasons) {
  # Define the earliest season allowed for training for test season s.
  # For example, if s is 2012 and rolling_window is 3, then we use seasons 2009, 2010, and 2011.
  min_season_allowed <- s - rolling_window_season
  
  # Training: only include games where season is between min_season_allowed and s-1.
  train_idx <- which(nfl_data_model$season >= min_season_allowed & 
                       nfl_data_model$season < s)
  
  # Test: all games from season s
  test_idx <- which(nfl_data_model$season == s)
  
  if(length(train_idx) > 0 && length(test_idx) > 0) {
    train_indices_roll_season[[as.character(s)]] <- train_idx
    test_indices_roll_season[[as.character(s)]] <- test_idx
    cat(sprintf("Rolling CV: Season %d -> Train (seasons %d to %d): %d, Test: %d\n", 
                s, min_season_allowed, s - 1, length(train_idx), length(test_idx)))
  }
}

time_slices_rolling_season <- list(train = train_indices_roll_season, 
                                   test = test_indices_roll_season)

cv_control_rolling_season <- trainControl(
  method = "cv",
  index = time_slices_rolling_season$train,
  indexOut = time_slices_rolling_season$test,
  summaryFunction = defaultSummary,
  savePredictions = "final",
  verboseIter = TRUE
)
cv_control_rolling_season

save(cv_control_rolling_season,
     file = "~/Desktop/NFL Analysis Data/cv_control_rolling_season.rda")

cat("Total Rolling CV folds:", length(time_slices_rolling_season$train), "\n")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Train XGBoost Models ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# for Score Predictions (using caret) on team-specific features
# train_control <- trainControl(method = "cv", number = 10)
# xgb_grid <- expand.grid(
#   nrounds = 100,
#   max_depth = 6,
#   eta = 0.1,
#   gamma = 0,
#   colsample_bytree = 0.8,
#   min_child_weight = 1,
#   subsample = 0.8
# )

# Make clusters for parallel processing
#cl <- makeCluster(detectCores() - 1)

## 4.A. Expanding Window ----
### 4.A.3. Result ----
# Spread score model using selected team-specific features
#result_formula <- as.formula(paste("result ~", paste(best_vars, collapse = " + ")))
#result_formula <- as.formula(paste("result ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  result_model_expand_season <- train(
    #result_formula, 
    #data = train_data,
    #x = nfl_data_model[, candidate_xgb_vars_clean],
    x = nfl_data_model[, candidate_xgb_vars],
    y = nfl_data_model$result,
    method = "xgbTree",
    trControl = cv_control_expanding_season,
    tuneLength = 10
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(result_model_expand_season, file = "~/Desktop/NFL Analysis Data/xgb_result_model_expand_season.RData")
save(result_model_expand_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_result_model_expand_season.rda")

## 4.B. Rolling Window ----
### 4.B.3. Result ----
# Spread score model using selected team-specific features
#result_formula <- as.formula(paste("result ~", paste(best_vars, collapse = " + ")))
#result_formula <- as.formula(paste("result ~", paste(candidate_xgb_vars, collapse = " + ")))

caret::getModelInfo("xgbTree")
caret::modelLookup("xgbTree")

# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'min_child_weight' was
# held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 50, max_depth = 1, eta = 0.3, gamma =
#   0, colsample_bytree = 0.6, min_child_weight = 1 and subsample = 1.

xgb_result_roll_grid <- expand.grid(
  max_depth = c(3, 4, 5),
  eta = c(0.05, 0.1),
  gamma = c(0, 1),
  subsample = c(0.6, 0.8),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 3)
)

#registerDoParallel(cl)
set.seed(123)
system.time(
  result_model_roll_season <- train(
    #result_formula, 
    #data = train_data,
    #x = nfl_data_model[, candidate_xgb_vars_clean],
    x = nfl_data_model[, candidate_xgb_vars],
    y = nfl_data_model$result,
    method = "xgbTree",
    trControl = cv_control_rolling_season,
    tuneLength = 10
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(result_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_result_model_roll_season2.RData")
save(result_model_roll_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_result_model_roll_season.rda")


save(result_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_result_model_roll_season2.RData")
load(file = "~/Desktop/NFL Analysis Data/xgb_result_model_expand_season.RData")

# 5. Evaluate and Compare Model Performance Metrics ----------------------------
# Define a helper function to compute performance metrics for a caret model
compute_metrics <- function(model) {
  # Extract predictions data frame from the model
  preds <- model$pred
  
  # Filter to keep only predictions corresponding to the best tuning parameters
  best <- model$bestTune
  for (col in names(best)) {
    preds <- preds[preds[[col]] == best[[col]], ]
  }
  
  # Compute metrics: MAE, RMSE, R², and Mean Bias
  mae_val       <- mae(preds$obs, preds$pred)
  rmse_val      <- rmse(preds$obs, preds$pred)
  r2_val        <- cor(preds$obs, preds$pred)^2
  mean_bias_val <- mean(preds$pred - preds$obs)
  
  return(data.frame(MAE = mae_val, RMSE = rmse_val, R2 = r2_val, Mean_Bias = mean_bias_val))
}

# Compute metrics for each model (Expanding CV)
metrics_home_expand   <- compute_metrics(home_model_expand_season)
metrics_away_expand  <- compute_metrics(away_model_expand_season)
metrics_result_expand <- compute_metrics(result_model_expand_season)
metrics_total_expand  <- compute_metrics(total_model_expand_season)

# Compute metrics for each model (Rolling CV)
metrics_home_roll     <- compute_metrics(home_model_roll_season)
metrics_away_roll    <- compute_metrics(away_model_roll_season)
metrics_result_roll   <- compute_metrics(result_model_roll_season)
metrics_total_roll   <- compute_metrics(total_model_roll_season)

# Create a summary table comparing Expanding vs Rolling for each target
cv_comparison <- rbind(
  cbind(Target = "Home",   CV_Method = "Expanding", metrics_home_expand),
  cbind(Target = "Home",   CV_Method = "Rolling",   metrics_home_roll),
  cbind(Target = "Away",   CV_Method = "Expanding", metrics_away_expand),
  cbind(Target = "Away",   CV_Method = "Rolling",   metrics_away_roll),
  cbind(Target = "Result", CV_Method = "Expanding", metrics_result_expand),
  cbind(Target = "Result", CV_Method = "Rolling",   metrics_result_roll),
  cbind(Target = "Total",  CV_Method = "Expanding", metrics_total_expand),
  cbind(Target = "Total",  CV_Method = "Rolling",   metrics_total_roll)
)

cat("Performance Metrics Comparison:\n")
print(cv_comparison)

# 6. Visualize Models and Compare ----

## 6.A. Predictions ----
# Helper function: Filter predictions using bestTune values
get_filtered_preds <- function(model) {
  preds <- model$pred
  for (col in names(model$bestTune)) {
    preds <- preds[preds[[col]] == model$bestTune[[col]], ]
  }
  return(preds)
}

# Helper function: Combine predictions from Expanding and Rolling models
get_combined_preds <- function(model_expand, model_roll) {
  preds_expand <- get_filtered_preds(model_expand)
  preds_roll   <- get_filtered_preds(model_roll)
  
  # Label CV methods
  preds_expand$CV_Method <- "Expanding"
  preds_roll$CV_Method   <- "Rolling"
  
  # Combine into one data frame
  preds <- rbind(preds_expand, preds_roll)
  return(preds)
}

## 6.B. Observed vs Predicted ----
# Helper function: Plot observed vs. predicted values
plot_obs_vs_pred <- function(preds_df, target_label) {
  p <- ggplot(preds_df, aes(x = obs, y = pred, color = CV_Method)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    smplot2::sm_statCorr() +
    labs(title = paste("Observed vs. Predicted", target_label),
         x = paste("Observed", target_label),
         y = paste("Predicted", target_label)) +
    theme_bw()
  return(p)
}

### 6.B.3. Result ----
preds_result <- get_filtered_preds(result_model_roll_season)
preds_result <- get_combined_preds(result_model_expand_season, result_model_roll_season)
result_obs_vs_pred_plot <- plot_obs_vs_pred(preds_result, "Result (Spread)")

# Display Result (Spread) plots
print(result_obs_vs_pred_plot)

## 6.C. Residual Diagnostics ----
### 6.A.3 Residual Diagnostics
# Helper function: Plot residual diagnostics (scatter and histogram)
plot_residuals <- function(preds_df, target_label) {
  preds_df$residual <- preds_df$obs - preds_df$pred
  
  p_scatter <- ggplot(preds_df, aes(x = pred, y = residual, color = CV_Method)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuals vs. Predicted", target_label),
         x = paste("Predicted", target_label),
         y = "Residual (Observed - Predicted)") +
    theme_bw()
  
  p_hist <- ggplot(preds_df, aes(x = residual, fill = CV_Method)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    labs(title = paste("Histogram of Residuals for", target_label),
         x = "Residual (Observed - Predicted)",
         y = "Count") +
    theme_bw()
  
  p_hist_hor <- ggplot(preds_df, aes(x = residual, fill = CV_Method)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    labs(title = "Histogram of Residuals",
         x = "Residual (Observed - Predicted)",
         y = "Count") +
    coord_flip() +
    theme_bw()
  
  return(list(scatter = p_scatter, 
              histogram = p_hist,
              histogram_hor = p_hist_hor))
}

### 6.C.3. Result ----
result_res_plots <- plot_residuals(preds_result, "Result (Spread)")

# Display Result (Spread) plots
print(result_res_plots$scatter)
print(result_res_plots$histogram)
wrap_plots(
  result_res_plots$scatter + 
    theme(legend.position = "none"), 
  result_res_plots$histogram_hor + 
    theme(legend.direction = "horizontal")
) +
  plot_layout(guides = 'collect', axes = 'collect',
              ncol = 2, widths = c(2,1)) +
  plot_annotation(
    theme = theme(
      legend.position = "bottom"
    )
  )

## 6.D. Q-Q Plots for Residuals ----
# Helper function: Compute residuals (if not already computed) and add to predictions data frame
add_residuals <- function(preds_df) {
  if (!"residual" %in% names(preds_df)) {
    preds_df$residual <- preds_df$obs - preds_df$pred
  }
  return(preds_df)
}

# Helper function: Create Q-Q plot of residuals
plot_qq_residuals <- function(preds_df, target_label) {
  p <- ggplot(preds_df, aes(sample = residual)) +
    stat_qq(alpha = 0.5, color = "blue") +
    stat_qq_line(color = "red") +
    facet_wrap(vars(CV_Method)) +
    labs(title = paste("Q-Q Plot of Residuals for", target_label),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_bw()
  return(p)
}

### 6.D.3. Result ----
preds_result <- add_residuals(preds_result)
qq_result <- plot_qq_residuals(preds_result, "Result (Spread)")
print(qq_result)

# 7. Feature Importance Analysis ----

# Function to compute and return feature importance data for a given target model pair
get_feature_importance_data <- function(model_expand, model_roll) {
  # Compute feature importance for each model using varImp()
  varImp_expand <- varImp(model_expand)$importance
  varImp_roll   <- varImp(model_roll)$importance
  
  # Convert row names to a column and add a CV method label
  varImp_expand <- varImp_expand |>
    tibble::rownames_to_column(var = "Feature") |>
    mutate(CV_Method = "Expanding")
  
  varImp_roll <- varImp_roll |>
    tibble::rownames_to_column(var = "Feature") |>
    mutate(CV_Method = "Rolling")
  
  # Combine the two importance tables into one data frame
  varImp_combined <- bind_rows(varImp_expand, varImp_roll)
  
  return(varImp_combined)
}

# Function to plot feature importance using the data from get_feature_importance_data()
plot_feature_importance <- function(model_expand, 
                                    model_roll, 
                                    target_label, 
                                    top_n = 10) {
  varImp_combined <- get_feature_importance_data(model_expand, model_roll)
  
  # Select the top n features for each CV method
  top_features <- varImp_combined |>
    group_by(CV_Method) |>
    arrange(desc(Overall)) |>
    slice_head(n = top_n) |>
    ungroup()
  
  # Create a faceted bar plot of feature importance
  p <- ggplot(top_features, aes(x = reorder_within(Feature, Overall, CV_Method),
                                y = Overall, fill = CV_Method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ CV_Method, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +  # This drops the extra label bits added by reorder_within
    labs(title = paste("Top", top_n, "Feature Importance for", target_label, "Model"),
         x = "Feature",
         y = "Importance") +
    theme_bw()
  
  return(p)
}

## 7.C. Result ----
# Get the data for further analysis or comparison:
varImp_result_data <- get_feature_importance_data(
  result_model_expand_season, 
  result_model_roll_season
) |>
  mutate(
    Response = "Result"
  )
varImp_result_data_expand <- varImp_result_data |>
  filter(CV_Method == "Expanding", Overall > 0)
varImp_result_data_roll <- varImp_result_data |>
  filter(CV_Method == "Rolling", Overall > 0)

print(varImp_result_data_expand)
print(varImp_result_data_roll)


# Generate and display the plot:
n_features_result <- max(nrow(varImp_result_data_expand), 
                         nrow(varImp_result_data_roll))
p_result <- plot_feature_importance(result_model_expand_season, 
                                    result_model_roll_season, 
                                    "Result",
                                    top_n = n_features_result)
print(p_result)
ggplotly(p_result)


# 8. Temporal Performance Analysis ----
compute_seasonal_metrics <- function(model, response_name) {
  # Filter predictions using the same helper from before
  preds <- get_filtered_preds(model)
  
  # If season is not already included in preds, merge it using row indices.
  if (!"season" %in% names(preds)) {
    preds <- preds %>%
      mutate(rowid = as.numeric(rownames(preds))) %>%
      left_join(nfl_data_model %>% mutate(rowid = row_number()) %>% select(rowid, season),
                by = "rowid")
  }
  
  metrics <- preds %>%
    group_by(season) %>%
    summarise(
      MAE = mae(obs, pred),
      RMSE = rmse(obs, pred),
      R2 = cor(obs, pred)^2,
      Mean_Bias = mean(pred - obs),
      n = n()
    ) %>%
    mutate(Response = response_name)
  
  return(metrics)
}

## 8.A. Compute and Combine Seasonal Metrics ----
### 8.A.3. Result Metrics ----
metrics_result_expand_season <- compute_seasonal_metrics(result_model_expand_season, "result")
metrics_result_roll_season   <- compute_seasonal_metrics(result_model_roll_season, "result")

metrics_result_season <- bind_rows(
  metrics_result_expand_season %>% mutate(CV_Method = "Expanding"),
  metrics_result_roll_season %>% mutate(CV_Method = "Rolling")
)
print(metrics_result_season, n = nrow(metrics_result_season))

## 8.B. Plot Temporal Performance ----
plot_temporal_metric <- function(metrics_df, metric, y_label, plot_title) {
  p <- ggplot(metrics_df, 
              aes(x = season, y = .data[[metric]], color = CV_Method)) +
    geom_line() +
    geom_point(size = 1.5) +
    labs(title = plot_title,
         x = "Season",
         y = y_label) +
    #scale_x_continuous(limits = c())
    theme_bw()
  return(p)
}

### 8.B.3. Result ----
p_rmse_result_season <- plot_temporal_metric(
  metrics_result_season,
  "RMSE", "RMSE", "Result RMSE by Season"
)
# print(p_rmse_result_season)

p_mae_result_season <- plot_temporal_metric(
  metrics_result_season, 
  "MAE", "MAE", "Result MAE by Season"
)
# print(p_mae_result_season)

p_r2_result_season <- plot_temporal_metric(
  metrics_result_season,
  "R2", "R²", "Result R² by Season"
)
# print(p_r2_result_season)

p_bias_result_season <- plot_temporal_metric(
  metrics_result_season, 
  "Mean_Bias", "Mean Bias", "Result Mean Bias by Season"
)
# print(p_bias_result_season)

wrap_plots(
  p_rmse_result_season,
  p_mae_result_season,
  p_r2_result_season,
  p_bias_result_season,
  nrow = 2,
  guides = 'collect'
)

# 9. Final Models ----
## 9.A. Identity Models ----
xgb_home_model_final <- home_model_expand_season
xgb_away_model_final <- away_model_expand_season
xgb_result_model_final <- result_model_expand_season
xgb_total_model_final <- total_model_expand_season

xgb_home_model_final <- home_model_roll_season
xgb_away_model_final <- away_model_roll_season
xgb_result_model_final <- result_model_roll_season
xgb_total_model_final <- total_model_roll_season

save(xgb_home_model_final, 
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_home_model_final.rda")
save(xgb_away_model_final, 
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_away_model_final.rda")
save(xgb_result_model_final, 
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_result_model_final.rda")
save(xgb_total_model_final, 
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_total_model_final.rda")

load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_home_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_away_model_final.rda")
#load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_result_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_total_model_final.rda")

## 9.B. Best Tunes ----
### 9.B.1. Plot Tunes ----
plot(xgb_home_model_final)
plot(xgb_away_model_final)
plot(xgb_result_model_final)
plot(xgb_total_model_final)

### 9.B.2. Extract Tunes ----
# Extract the best-tuned hyperparameters for each final model
home_best_tune   <- xgb_home_model_final$bestTune |>
  mutate(Response = "Home",   CV_Method = "Expanding")
away_best_tune   <- xgb_away_model_final$bestTune |>
  mutate(Response = "Away",   CV_Method = "Expanding")
result_best_tune <- xgb_result_model_final$bestTune |>
  mutate(Response = "Result", CV_Method = "Expanding")
total_best_tune  <- xgb_total_model_final$bestTune |>
  mutate(Response = "Total",  CV_Method = "Expanding")

best_tunes_all <- bind_rows(
  home_best_tune, 
  away_best_tune,
  result_best_tune,
  total_best_tune
)
print(best_tunes_all)

save(best_tunes_all,
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/best_tunes_all.rda")

## 9.C. Feature Importance ----
### 9.C.1. Per Model ----
varImp_home_final <- varImp(xgb_home_model_final)$importance |>
  rownames_to_column("Feature") |>
  mutate(Response = "Home") |>
  filter(Overall > 0)

varImp_away_final <- varImp(xgb_away_model_final)$importance |>
  rownames_to_column("Feature") |>
  mutate(Response = "Away") |>
  filter(Overall > 0)

varImp_result_final <- varImp(xgb_result_model_final)$importance |>
  rownames_to_column("Feature") |>
  mutate(Response = "Result") |>
  filter(Overall > 0)

varImp_total_final <- varImp(xgb_total_model_final)$importance |>
  rownames_to_column("Feature") |>
  mutate(Response = "Total") |>
  filter(Overall > 0)

cat(
  "The Home model had", nrow(varImp_home_final), "non-zero features", "\n",
  "The Away model had", nrow(varImp_away_final), "non-zero features", "\n",
  "The Result model had", nrow(varImp_result_final), "non-zero features", "\n",
  "The Total model had", nrow(varImp_total_final), "non-zero features"
)

### 9.C.2. Combined ----
varImp_final_all <- bind_rows(
  varImp_home_final,
  varImp_away_final,
  varImp_result_final,
  varImp_total_final
)

save(varImp_final_all,
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/varImp_final_all.rda")

### 9.C.3. Compare ----
plot_top_features_data <- function(imp_df, top_n = 20) {
  
  # For each Response, take the top N
  imp_top <- imp_df |>
    group_by(Response) |>
    slice_max(order_by = Overall, n = top_n) |>
    ungroup()
  
  ggplot(imp_top, aes(
    x = reorder_within(Feature, Overall, Response), 
    y = Overall,
    fill = Response
  )) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ Response, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(
      title = paste("Top", top_n, "Feature Importance per Final Model"),
      x = "Feature",
      y = "Importance"
    ) +
    theme_bw()
}

p_features <- plot_top_features(varImp_final_all, top_n = 20)
print(p_features)
ggplotly(p_features)

### 9.C.4. Common ----
best_features_all <- varImp_final_all |>
  group_by(Feature) |>
  summarise(
    Overall = sum(Overall)
  ) |>
  arrange(desc(Overall)) |>
  mutate(
    Home = Feature %in% varImp_home_final$Feature,
    Away = Feature %in% varImp_away_final$Feature,
    Result = Feature %in% varImp_result_final$Feature,
    Total = Feature %in% varImp_total_final$Feature
  ) |>
  rowwise() |>
  mutate(
    Responses = sum(Home, Away, Result, Total)
  )

## 9.D. Predictions ----
get_filtered_preds <- function(model) {
  preds <- model$pred
  for (col in names(model$bestTune)) {
    preds <- preds[preds[[col]] == model$bestTune[[col]], ]
  }
  return(preds)
}

### 9.D.1. Home ----
xgb_home_score_pred <- get_filtered_preds(xgb_home_model_final) |>
  left_join(
    nfl_data_model |> mutate(rowIndex = row_number()),
    by = "rowIndex"
  ) |>
  mutate(Response = "Home")

### 9.D.2. Away ----
xgb_away_score_pred <- get_filtered_preds(xgb_away_model_final) |>
  left_join(
    nfl_data_model |> mutate(rowIndex = row_number()),
    by = "rowIndex"
  ) |>
  mutate(Response = "Away")

### 9.D.3. Result ----
xgb_result_pred <- get_filtered_preds(xgb_result_model_final) |>
  left_join(
    nfl_data_model |> mutate(rowIndex = row_number()),
    by = "rowIndex"
  ) |>
  mutate(Response = "Result")

### 9.D.4. Total ----
xgb_total_pred <- get_filtered_preds(xgb_total_model_final) |>
  left_join(
    nfl_data_model |> mutate(rowIndex = row_number()),
    by = "rowIndex"
  ) |>
  mutate(Response = "Total")

### 9.D.5. Combine ----
xgb_cv_preds <- nfl_data_model |> 
  select(
    game_id, season, week,
    home_score,
    away_score,
    result,
    total
  ) |>
  left_join(
    xgb_home_score_pred |> 
      select(game_id, xgb_home_score = pred),
    by = join_by(game_id)
  ) |>
  left_join(
    xgb_away_score_pred |> 
      select(game_id, xgb_away_score = pred),
    by = join_by(game_id)
  ) |>
  left_join(
    xgb_result_pred |> 
      select(game_id, xgb_result = pred),
    by = join_by(game_id)
  ) |>
  left_join(
    xgb_total_pred |> 
      select(game_id, xgb_total = pred),
    by = join_by(game_id)
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, .after = result) |>
  relocate(xgb_total, .after = total)

save(xgb_cv_preds,
     file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_cv_preds.rda")

## 9.E. Betting Performance ----
betting_vars <- c("spread_line", "spreadCover", 
                  "home_spread_odds", "home_spread_prob",
                  "away_spread_odds", "away_spread_prob",
                  "total_line", "totalCover",
                  "over_odds", "over_prob",
                  "under_odds", "under_prob",
                  "winner", 
                  "home_moneyline", "home_moneyline_prob",
                  "away_moneyline", "away_moneyline_prob")

betting_df <- nfl_data |>
  select(game_id) |>
  left_join(xgb_cv_preds) |>
  left_join(
    nfl_data |>
      select(game_id, betting_vars)
  ) #|>
# mutate(
#   actual_cover = case_when(
#     result > spread_line ~ "Home",
#     result < spread_line ~ "Away",
#     TRUE ~ NA_character_
#   ),
#   .after = spreadCover
# )

betting_eval <- function(bet_df,
                         start_season = 2010,
                         group_season = FALSE,
                         group_week = FALSE) {
  bet_df <- bet_df |>
    filter(season >= start_season) |>
    mutate(
      actual_result_cover = case_when(
        result > spread_line ~ "Home",
        result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      exp_result = xgb_result,
      exp_result_cover = case_when(
        exp_result > spread_line ~ "Home",
        exp_result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      exp_result2 = xgb_home_score - xgb_away_score,
      exp_result2_cover = case_when(
        exp_result2 > spread_line ~ "Home",
        exp_result2 < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      correct_result = exp_result_cover == actual_result_cover,
      correct_result2 = exp_result2_cover == actual_result_cover
    ) |>
    mutate(
      actual_total_cover = case_when(
        total > total_line ~ "Over",
        total < total_line ~ "Under",
        TRUE ~ NA_character_
      ),
      exp_total = xgb_total,
      exp_total_cover = case_when(
        exp_total > total_line ~ "Over",
        exp_total < total_line ~ "Under",
        TRUE ~ NA_character_
      ),
      exp_total2 = xgb_home_score + xgb_away_score,
      exp_total2_cover = case_when(
        exp_total2 > total_line ~ "Over",
        exp_total2 < total_line ~ "Under",
        TRUE ~ NA_character_
      ),
      correct_total = exp_total_cover == actual_total_cover,
      correct_total2 = exp_total2_cover == actual_total_cover
    )
  
  if(group_season & group_week){
    bet_df <- bet_df |>
      group_by(season, week)
  }else if(group_season & !group_week){
    bet_df <- bet_df |>
      group_by(season)
  }else if(!group_season & group_week){
    bet_df <- bet_df |>
      group_by(week)
  }else{
    bet_df <- bet_df
  }
  
  acc_df <- bet_df |>
    summarise(
      Games = n(),
      Result_Bets = sum(!is.na(correct_result)),
      Acc_Result = round(mean(correct_result, na.rm = TRUE)*100, 2),
      Acc_Result2 = round(mean(correct_result2, na.rm = TRUE)*100, 2),
      Total_Bets = sum(!is.na(correct_total)),
      Acc_Total = round(mean(correct_total, na.rm = TRUE)*100, 2),
      Acc_Total2 = round(mean(correct_total2, na.rm = TRUE)*100, 2)
    )
  
  return(acc_df)
}

## 9.E.1. Output Accuracy ----
acc_df <- betting_eval(betting_df, 
                       start_season = 2010,
                       group_season = FALSE,
                       group_week = FALSE)
acc_df_season <- betting_eval(betting_df, 
                              start_season = 2010,
                              group_season = TRUE,
                              group_week = FALSE)
acc_df_week <- betting_eval(betting_df, 
                            start_season = 2010,
                            group_season = FALSE,
                            group_week = TRUE)

print(acc_df, n = nrow(acc_df))
print(acc_df_season, n = nrow(acc_df_season))
print(acc_df_week, n = nrow(acc_df_week))


# 10. Final Model Characteristics
# Combine into one data frame
varImp_all <- bind_rows(
  varImp_home_data_expand,
  varImp_home_data_roll,
  varImp_away_data_expand,
  varImp_away_data_roll,
  varImp_result_data_expand,
  varImp_result_data_roll,
  varImp_total_data_expand,
  varImp_total_data_roll
)
print(varImp_all)

# Reshape the data to wide format so each feature has Expanding and Rolling scores side by side
varImp_wide <- varImp_all |>
  pivot_wider(names_from = CV_Method, values_from = Overall)

# Function to create scatter plots comparing Expanding vs. Rolling importance for each response
compare_varImp <- function(data, response_label) {
  df <- data |> filter(Response == response_label)
  
  p <- ggplot(df, aes(x = Expanding, y = Rolling, label = Feature)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    geom_text(nudge_y = 0.05, nudge_x = 0.05, check_overlap = TRUE, size = 3) +
    labs(title = paste("Expanding vs. Rolling Feature Importance:", response_label),
         x = "Expanding Importance",
         y = "Rolling Importance") +
    theme_bw()
  
  return(p)
}

# Generate comparison plots for each response
p_home_comp   <- compare_varImp(varImp_wide, "Home")
p_away_comp   <- compare_varImp(varImp_wide, "Away")
p_result_comp <- compare_varImp(varImp_wide, "Result")
p_total_comp  <- compare_varImp(varImp_wide, "Total")

print(p_home_comp)
print(p_away_comp)
print(p_result_comp)
print(p_total_comp)

# Alternatively, a faceted dot plot across responses:
p_dot <- ggplot(varImp_all, aes(x = reorder(Feature, Overall), y = Overall, color = CV_Method)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +
  facet_wrap(~ Response, scales = "free_y") +
  coord_flip() +
  labs(title = "Feature Importance Across Responses",
       x = "Feature",
       y = "Importance") +
  theme_bw()

print(p_dot)

# Next Steps ----
# Examine the above temporal performance plots (RMSE, MAE, R², Mean Bias by season)
# for each response (Home, Away, Result, Total) to check for:
#  - Consistency and stability over time
#  - Signs of drift or sudden performance changes
#
# Based on these diagnostics, decide which CV method (Expanding vs. Rolling)
# performs better for each response. For example, you might choose:
#   - Home, Away, and Result: Expanding (if they show lower, more stable errors)
#   - Total: Rolling (if it outperforms in later seasons)
#
# Once you have made these decisions, down-select your models accordingly.
# Then, you can focus further analyses (e.g., feature importance, integration into Bayesian modeling)
# on the chosen models.

# 1. Compare Model Performance Across Expanding vs Rolling
# Why: This helps you choose which CV strategy better mimics your real-world forecasting accuracy.
# Compare RMSE / MAE / R² for home score, away score, result (spread), and total.
# Analyze temporal drift: does performance degrade in certain seasons for either method?
#   Consider whether rolling outperforms in early seasons but expanding overtakes with more data.
# If one strategy is clearly superior, prioritize it for downstream analysis.
# 
# 2. Evaluate Calibration of Predictions
# Why: Especially for totals and spreads (used in betting), being well-calibrated matters as much as error metrics.
# For score models: check prediction error histograms / QQ plots.
# For spread/total: plot predicted vs actual line distributions.
# If you're using posteriors later, ensure mean ± stddev align with reality.
# 
# 3. Analyze Feature Importance (Per Model + Over Time)
# Why: This gives insight into what’s driving your models and helps with interpretation + stability.
# Use xgb.importance() or SHAP values to get:
#
# Top features per model (home score vs away score vs spread vs total)
# Top features per fold/season (feature drift)
# See if certain features (e.g., net SRS, offensive EPA) consistently rank high.
# 
# 4. Compare Model Outputs: Expanding vs Rolling
# Why: Even if performance is similar, the predictions themselves may differ.
# Plot predicted home/away scores and lines side-by-side.
# 
# Investigate:
# Do rolling models react faster to mid-season shifts?
# Do expanding models smooth out variance too much?
# 
# 5. Aggregate into Betting Metrics
# Why: Ultimately, you're modeling for betting decision-making.
# Use spread and total predictions to simulate:
#   % edge over market line
# Win probability vs actual outcomes
# Check how predicted lines stack against published closing lines for each season.
# 
# Optional / Later Steps:
#   Tune model hyperparameters (if not already done).
# Use score model predictions as inputs to a second-stage betting model (if going down that path).
# Integrate into Shiny App for dynamic historical replays.

