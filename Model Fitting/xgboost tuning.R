
# 0. Setup & Libraries-------------------------------

# Make sure you have the following packages installed:
# install.packages(c("xgboost", "tidyverse", "Metrics", "rBayesianOptimization"))

library(readr)
library(tidytext)
library(rBayesianOptimization)
library(tidyr)
library(purrr)
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


# Set the target variable here.
# For example: "home_score", "away_score", "result", or "total"
target_var <- "home_score"


# 1. Load & Prepare Data-------------------------------

# Load the feature-engineered data and run your cleaning function.
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
source("./app/R/clean_modData.R")
nfl_data <- clean_modData(data = modData, season_start = 2007)

# Create a time identifier for ordering:
nfl_data <- nfl_data %>% mutate(time_id = as.numeric(paste0(season, sprintf("%02d", week))))


# 2. Define Candidate Predictors for XGBoost (Team-Specific)-------------------------------

drop_vars <- c("game_id", "season", "season_type", "week", "home_team", "away_team",
               "home_score", "away_score", "result", "spread_line", "spreadCover",
               "total", "total_line", "totalCover", "winner", "away_spread_odds", "away_spread_prob",
               "home_spread_odds", "home_spread_prob", "over_odds", "over_prob",
               "under_odds", "under_prob", "away_moneyline", "away_moneyline_prob",
               "home_moneyline", "home_moneyline_prob", "stadium", "home_coach", "away_coach",
               "time_id")
game_level_vars <- c("weekday", "time_of_day", "location", "location2", "div_game", 
                     "home_rest", "away_rest", "roof", "surface", "temp", "wind")

all_candidates <- setdiff(names(nfl_data), drop_vars)
candidate_xgb_vars <- all_candidates[sapply(nfl_data[, all_candidates], is.numeric)]
candidate_xgb_vars <- setdiff(candidate_xgb_vars, game_level_vars)
cat("Team-specific candidate predictors for XGBoost:\n")
print(candidate_xgb_vars)

# Preprocessing: keep complete cases
candidate_data <- nfl_data %>% select(candidate_xgb_vars)
complete_rows <- complete.cases(candidate_data)
complete_rows_ID <- nfl_data %>% filter(complete_rows) %>% pull(game_id)
candidate_data <- candidate_data %>% filter(complete_rows)

# Remove linear combinations if any
combo_info <- findLinearCombos(candidate_data)
if (!is.null(combo_info$remove)) {
  cat("Removing the following predictors due to linear dependency:\n")
  print(candidate_xgb_vars[combo_info$remove])
  candidate_xgb_vars_clean <- candidate_xgb_vars[-combo_info$remove]
} else {
  cat("No linear dependencies found.\n")
  candidate_xgb_vars_clean <- candidate_xgb_vars
}

# Remove near-zero variance predictors
nzv <- nearZeroVar(candidate_data)
if (length(nzv) > 0) {
  cat("Removing the following near-zero variance predictors:\n")
  print(candidate_xgb_vars_clean[nzv])
  candidate_xgb_vars_clean <- candidate_xgb_vars_clean[-nzv]
} else {
  cat("No near-zero variance predictors found.\n")
}

cat("Final candidate predictors for XGBoost:\n")
print(candidate_xgb_vars_clean)

# Final Model Data: sort by season and week and restrict to complete rows.
nfl_data_model <- nfl_data %>% 
  filter(game_id %in% complete_rows_ID) %>% 
  arrange(season, week)


# 3. Build Rolling Folds (3 Seasons -> Next Season)-------------------------------

build_rolling_folds <- function(data, start_years = 2007:2021, window_size = 3) {
  folds_list <- list()
  for (start_season in start_years) {
    train_seasons <- start_season:(start_season + window_size - 1)
    test_season <- start_season + window_size
    train_data <- data %>% filter(season %in% train_seasons)
    test_data <- data %>% filter(season == test_season)
    if (nrow(test_data) == 0) next
    folds_list[[as.character(start_season)]] <- list(
      train = train_data,
      test = test_data
    )
  }
  folds_list
}

rolling_folds <- build_rolling_folds(nfl_data_model, start_years = 2018:2021, window_size = 3)
cat("Number of rolling folds:", length(rolling_folds), "\n")


# 4. Time-Based CV Indices for Inner CV (6 folds)-------------------------------

get_time_based_cv_indices <- function(train_data, 
                                      season_col = "season", 
                                      week_col = "week",
                                      bound1 = 6, bound2 = 12) {
  # Assume train_data is sorted by season then week.
  seasons <- sort(unique(train_data[[season_col]]))
  if (length(seasons) != 3) {
    stop("train_data must contain exactly 3 seasons.")
  }
  first_season <- seasons[1]
  second_season <- seasons[2]
  third_season <- seasons[3]
  
  # Helper to get indices for a season with a condition on week.
  idx_for_season <- function(season_val, condition_fun) {
    which(train_data[[season_col]] == season_val & condition_fun(train_data[[week_col]]))
  }
  
  # For season 2:
  test_idx_2_fold1 <- idx_for_season(second_season, function(w) w <= bound1)
  test_idx_2_fold2 <- idx_for_season(second_season, function(w) w >= (bound1+1) & w <= bound2)
  test_idx_2_fold3 <- idx_for_season(second_season, function(w) w > bound2)
  # For season 3:
  test_idx_3_fold1 <- idx_for_season(third_season, function(w) w <= bound1)
  test_idx_3_fold2 <- idx_for_season(third_season, function(w) w >= (bound1+1) & w <= bound2)
  test_idx_3_fold3 <- idx_for_season(third_season, function(w) w > bound2)
  
  # Define training indices for each inner fold:
  train_idx_fold1 <- 1:(min(test_idx_2_fold1) - 1)  # All of season 1.
  train_idx_fold2 <- 1:(min(test_idx_2_fold2) - 1)  # Season 1 + season 2 up to bound1.
  train_idx_fold3 <- 1:(min(test_idx_2_fold3) - 1)  # Season 1 + all of season 2.
  train_idx_fold4 <- 1:(min(test_idx_3_fold1) - 1)  # All data from seasons 1 & 2.
  train_idx_fold5 <- 1:(min(test_idx_3_fold2) - 1)  # Seasons 1 & 2 + season 3 up to bound1.
  train_idx_fold6 <- 1:(min(test_idx_3_fold3) - 1)  # All data from seasons 1,2,3.
  
  folds <- list(
    Fold1 = test_idx_2_fold1,
    Fold2 = test_idx_2_fold2,
    Fold3 = test_idx_2_fold3,
    Fold4 = test_idx_3_fold1,
    Fold5 = test_idx_3_fold2,
    Fold6 = test_idx_3_fold3
  )
  train_folds <- list(
    Fold1 = train_idx_fold1,
    Fold2 = train_idx_fold2,
    Fold3 = train_idx_fold3,
    Fold4 = train_idx_fold4,
    Fold5 = train_idx_fold5,
    Fold6 = train_idx_fold6
  )
  list(train = train_folds, test = folds)
}

# Example usage:
cv_indices <- get_time_based_cv_indices(rolling_folds$`2018`$train, season_col = "season", week_col = "week", bound1 = 6, bound2 = 12)
# Now cv_indices$train and cv_indices$test can be passed to xgb.cv


# 5. APPROACH A: Per-Fold Hyperparameter Tuning (Time-Based CV)-------------------------------

# The functions below use the time-based CV indices for inner CV.

cv_for_fold <- function(max_depth, eta, gamma, colsample_bytree, min_child_weight,
                        subsample, lambda, alpha,
                        train_df, feature_names, nrounds = 500, early_stopping = 20, time_folds) {
  tryCatch({
    max_depth <- as.integer(round(max_depth, 0))
    min_child_weight <- as.integer(round(min_child_weight, 0))
    
    params <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = max_depth,
      eta = eta,
      gamma = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample = subsample,
      lambda = lambda,
      alpha = alpha
    )
    
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_df[, feature_names]),
      label = train_df[[target_var]]
    )
    
    train_fold_list <- time_folds[["train"]]
    test_fold_list <- time_folds[["test"]]
    xgb_cv_out <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      train_folds = train_fold_list,
      folds = test_fold_list,
      early_stopping_rounds = early_stopping,
      verbose = FALSE
    )
    
    assign("xgb_cv_out", xgb_cv_out, envir = .GlobalEnv)
    best_iter <- xgb_cv_out$best_iteration
    best_rmse <- xgb_cv_out$evaluation_log$test_rmse_mean[best_iter]
    
    if (!is.finite(best_rmse)) {
      message("Non-finite RMSE returned from xgb.cv. Evaluation log:")
      print(xgb_cv_out$evaluation_log)
      stop("Non-finite RMSE returned from xgb.cv")
    }
    list(Score = -best_rmse, Pred = NA)
  }, error = function(e) {
    message("Error in cv_for_fold with parameters:")
    message(sprintf("max_depth=%s, eta=%s, gamma=%s, colsample_bytree=%s, min_child_weight=%s, subsample=%s, lambda=%s, alpha=%s",
                    max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample, lambda, alpha))
    message("Error message: ", e$message)
    return(list(Score = 1e6, Pred = NA))
  })
}

tune_fold_bayes <- function(train_data, feature_names, time_folds, init_points = 5, n_iter = 10, nrounds = 500, early_stop = 20) {
  f_wrapper <- function(max_depth, eta, gamma, colsample_bytree, min_child_weight,
                        subsample, lambda, alpha) {
    result <- cv_for_fold(
      max_depth = max_depth, 
      eta = eta, 
      gamma = gamma, 
      colsample_bytree = colsample_bytree, 
      min_child_weight = min_child_weight,
      subsample = subsample,
      lambda = lambda,
      alpha = alpha,
      train_df = train_data,
      feature_names = feature_names,
      nrounds = nrounds,
      early_stopping = early_stop,
      time_folds = time_folds
    )
    message("Trial: ", paste("max_depth=", max_depth, " eta=", eta, " gamma=", gamma,
                             " colsample_bytree=", colsample_bytree, " min_child_weight=", min_child_weight,
                             " subsample=", subsample, " lambda=", lambda, " alpha=", alpha,
                             " Score=", result$Score))
    result
  }
  
  bounds <- list(
    max_depth = c(2L, 8L),
    eta = c(0.01, 0.3),
    gamma = c(1e-6, 5),
    colsample_bytree = c(0.5, 1.0),
    min_child_weight = c(1L, 10L),
    subsample = c(0.5, 1.0),
    lambda = c(1e-6, 2),
    alpha = c(1e-6, 2)
  )
  
  set.seed(999)
  opt_res <- tryCatch({
    BayesianOptimization(
      FUN = f_wrapper,
      bounds = bounds,
      init_points = init_points,
      n_iter = n_iter,
      acq = "ucb",
      kappa = 2.576,
      eps = 0.0,
      verbose = TRUE
    )
  }, error = function(e) {
    message("Error during BayesianOptimization:")
    message(e$message)
    NULL
  })
  
  if (is.null(opt_res)) {
    stop("BayesianOptimization failed. Check the parameter bounds or increase init_points.")
  } else {
    print(opt_res$History)
  }
  
  opt_res
}

# Now, perform per-fold hyperparameter tuning using time-based inner CV.
per_fold_results <- list()
per_fold_predictions <- data.frame()

system.time(
  for (fold_name in names(rolling_folds)) {
    cat("\n==========\nTuning for fold:", fold_name, "\n")
    train_df <- rolling_folds[[fold_name]]$train
    test_df <- rolling_folds[[fold_name]]$test
    
    time_folds <- get_time_based_cv_indices(train_df, season_col = "season", week_col = "week", bound1 = 6, bound2 = 12)
    
    fold_bayes_res <- tune_fold_bayes(
      train_data = train_df,
      feature_names = candidate_xgb_vars_clean,
      time_folds = time_folds,
      init_points = 10,
      n_iter = 15,
      nrounds = 500,
      early_stop = 20
    )
    
    best_params <- fold_bayes_res$Best_Par
    cat("Best Params for fold", fold_name, ":\n")
    print(best_params)
    
    best_cv <- cv_for_fold(
      max_depth = best_params["max_depth"], 
      eta = best_params["eta"], 
      gamma = best_params["gamma"], 
      colsample_bytree = best_params["colsample_bytree"], 
      min_child_weight = best_params["min_child_weight"],
      subsample = best_params["subsample"],
      lambda = best_params["lambda"],
      alpha = best_params["alpha"],
      train_df = train_df,
      feature_names = candidate_xgb_vars_clean,
      nrounds = 500,
      early_stopping = 20,
      time_folds = time_folds
    )
    
    final_params_list <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = as.integer(best_params["max_depth"]),
      eta = best_params["eta"],
      gamma = best_params["gamma"],
      colsample_bytree = best_params["colsample_bytree"],
      min_child_weight = as.integer(best_params["min_child_weight"]),
      subsample = best_params["subsample"],
      lambda = best_params["lambda"],
      alpha = best_params["alpha"]
    )
    
    dtrain_fold <- xgb.DMatrix(
      data = as.matrix(train_df[, candidate_xgb_vars_clean]),
      label = train_df[[target_var]]
    )
    
    set.seed(999)
    xgb_cv_run <- xgb.cv(
      params = final_params_list,
      data = dtrain_fold,
      nrounds = 500,
      train_folds = time_folds$train,
      folds = time_folds$test,
      early_stopping_rounds = 20,
      verbose = FALSE
    )
    
    final_nrounds <- xgb_cv_run$best_iteration
    
    xgb_final_model <- xgb.train(
      params = final_params_list,
      data = dtrain_fold,
      nrounds = final_nrounds,
      watchlist = list(train = dtrain_fold),
      verbose = 0
    )
    
    dtest_fold <- xgb.DMatrix(
      data = as.matrix(test_df[, candidate_xgb_vars_clean]),
      label = test_df[[target_var]]
    )
    
    fold_pred <- predict(xgb_final_model, dtest_fold)
    fold_rmse <- rmse(test_df[[target_var]], fold_pred)
    fold_mae <- mae(test_df[[target_var]], fold_pred)
    
    cat(sprintf("Fold %s => Test RMSE: %.3f | MAE: %.3f\n", fold_name, fold_rmse, fold_mae))
    
    per_fold_results[[fold_name]] <- list(
      best_params = best_params,
      nrounds = final_nrounds,
      fold_rmse = fold_rmse,
      fold_mae = fold_mae,
      train_seasons = paste(unique(train_df$season), collapse = ","),
      test_season = paste(unique(test_df$season), collapse = ",")
    )
    
    temp_pred_df <- test_df %>%
      select(game_id, season, week, !!sym(target_var)) %>%
      mutate(pred = fold_pred, fold_name = fold_name)
    per_fold_predictions <- bind_rows(per_fold_predictions, temp_pred_df)
  }
)

per_fold_summary <- map_dfr(per_fold_results, function(x) {
  tibble(
    best_params = paste(names(x$best_params), round(unlist(x$best_params), 3), sep="=", collapse=", "),
    nrounds = x$nrounds,
    fold_rmse = x$fold_rmse,
    fold_mae = x$fold_mae,
    train_seasons = x$train_seasons,
    test_season = x$test_season
  )
})

cat("\n=== Per-Fold Hyperparameter Tuning Results (Time-Based CV) ===\n")
print(per_fold_summary)


# 5.B. Evaluate the Single Global Best Param Set Across Folds-------------------------------
evaluate_global_best <- function(hparams, folds_list, feature_names, nrounds = 500, early_stopping = 20) {
  results <- data.frame()
  predictions <- data.frame()
  
  hparams[["max_depth"]] <- as.integer(round(hparams[["max_depth"]], 0))
  hparams[["min_child_weight"]] <- as.integer(round(hparams[["min_child_weight"]], 0))
  
  final_params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = hparams[["max_depth"]],
    eta = hparams[["eta"]],
    gamma = hparams[["gamma"]],
    colsample_bytree = hparams[["colsample_bytree"]],
    min_child_weight = hparams[["min_child_weight"]],
    subsample = hparams[["subsample"]],
    lambda = hparams[["lambda"]],
    alpha = hparams[["alpha"]]
  )
  
  for (f_name in names(folds_list)) {
    train_df <- folds_list[[f_name]]$train
    test_df <- folds_list[[f_name]]$test
    
    dtrain <- xgb.DMatrix(as.matrix(train_df[, feature_names]), label = train_df[[target_var]])
    dtest <- xgb.DMatrix(as.matrix(test_df[, feature_names]), label = test_df[[target_var]])
    
    xgb_final <- xgb.train(
      params = final_params,
      data = dtrain,
      nrounds = nrounds,
      watchlist = list(train = dtrain, test = dtest),
      early_stopping_rounds = early_stopping,
      verbose = 0
    )
    
    test_pred <- predict(xgb_final, dtest)
    fold_rmse <- rmse(test_df[[target_var]], test_pred)
    fold_mae <- mae(test_df[[target_var]], test_pred)
    
    results <- rbind(
      results,
      data.frame(
        fold_name = f_name,
        train_seasons = paste(unique(train_df$season), collapse = ","),
        test_season = paste(unique(test_df$season), collapse = ","),
        RMSE = fold_rmse,
        MAE = fold_mae,
        stringsAsFactors = FALSE
      )
    )
    
    temp_pred_df <- test_df %>%
      select(game_id, season, week, !!sym(target_var)) %>%
      mutate(pred = test_pred, fold_name = f_name)
    predictions <- bind_rows(predictions, temp_pred_df)
  }
  list(performance = results, predictions = predictions)
}

global_eval <- evaluate_global_best(
  hparams = global_best_params,
  folds_list = rolling_folds,
  feature_names = candidate_xgb_vars_clean,
  nrounds = 500,
  early_stopping = 20
)

cat("\n=== Performance of the Single Global Best Param Set, Fold-by-Fold ===\n")
print(global_eval$performance)

global_summary <- global_eval$performance %>%
  summarise(
    Mean_RMSE = mean(RMSE),
    Mean_MAE = mean(MAE)
  )

cat("\nSingle Global Param Averages => RMSE:", round(global_summary$Mean_RMSE, 3),
    " | MAE:", round(global_summary$Mean_MAE, 3), "\n")


# 6. Variable Importance & Feature Reduction ----------------------------

final_params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = as.integer(global_best_params[["max_depth"]]),
  eta = global_best_params[["eta"]],
  gamma = global_best_params[["gamma"]],
  colsample_bytree = global_best_params[["colsample_bytree"]],
  min_child_weight = as.integer(global_best_params[["min_child_weight"]]),
  subsample = global_best_params[["subsample"]],
  lambda = global_best_params[["lambda"]],
  alpha = global_best_params[["alpha"]]
)

dtrain_full <- xgb.DMatrix(
  data = as.matrix(nfl_data_model[, candidate_xgb_vars_clean]),
  label = nfl_data_model[[target_var]]
)

final_model_full <- xgb.train(
  params = final_params,
  data = dtrain_full,
  nrounds = 500,
  verbose = 1
)

importance_matrix <- xgb.importance(feature_names = candidate_xgb_vars_clean, model = final_model_full)
cat("\n=== Variable Importance ===\n")
print(importance_matrix)

library(ggplot2)
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "XGBoost Variable Importance", x = "Feature", y = "Gain") +
  theme_minimal()

top_features <- head(importance_matrix[order(-importance_matrix$Gain), ], 20)$Feature
cat("\n=== Top 20 Features ===\n")
print(top_features)

importance_matrix_gain <- importance_matrix[order(-importance_matrix$Gain), ]
importance_matrix_gain$cumGain <- cumsum(importance_matrix_gain$Gain) / sum(importance_matrix_gain$Gain)

ggplot(importance_matrix_gain, aes(x = reorder(Feature, -Gain), y = cumGain)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Cumulative Gain by Feature", x = "Feature", y = "Cumulative Gain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

threshold <- 0.90
top_features_threshold <- importance_matrix_gain$Feature[importance_matrix_gain$cumGain <= threshold]
cat("Features contributing to 90% cumulative gain:\n")
print(top_features_threshold)


# 7. Rolling One-Week-Ahead Forecasting ----------------------------

rolling_forecast <- function(data, base_years, forecast_year, features, final_params, nrounds_fixed = 500) {
  forecast_data <- data %>% filter(season == forecast_year) %>% arrange(week)
  train_set <- data %>% filter(season %in% base_years) %>% arrange(season, week)
  
  forecast_preds <- data.frame()
  unique_weeks <- sort(unique(forecast_data$week))
  for (w in unique_weeks) {
    current_train <- rbind(
      train_set,
      forecast_data %>% filter(week < w)
    )
    current_test <- forecast_data %>% filter(week == w)
    
    dtrain <- xgb.DMatrix(
      data = as.matrix(current_train %>% select(all_of(features))),
      label = current_train[[target_var]]
    )
    dtest <- xgb.DMatrix(
      data = as.matrix(current_test %>% select(all_of(features)))
    )
    
    model <- xgb.train(
      params = final_params,
      data = dtrain,
      nrounds = nrounds_fixed,
      verbose = 1
    )
    
    preds <- predict(model, dtest)
    current_test$pred <- preds
    forecast_preds <- rbind(forecast_preds, current_test)
    
    train_set <- rbind(train_set, current_test %>% select(-pred))
  }
  return(forecast_preds)
}

forecast_years <- 2010:2024
rolling_results <- list()

for (yr in forecast_years) {
  base_years <- (yr - 3):(yr - 1)
  cat("Forecasting year:", yr, "using base years:", paste(base_years, collapse = ", "), "\n")
  
  preds <- rolling_forecast(data = nfl_data_model, 
                            base_years = base_years, 
                            forecast_year = yr, 
                            features = top_features_threshold, 
                            final_params = final_params, 
                            nrounds_fixed = 500)
  rolling_results[[as.character(yr)]] <- preds
}

all_forecasts <- do.call(rbind, rolling_results)
overall_rmse <- rmse(all_forecasts[[target_var]], all_forecasts$pred)
overall_mae <- mae(all_forecasts[[target_var]], all_forecasts$pred)
cat(sprintf("\nOverall Out-of-Sample Forecast Performance: RMSE = %.3f, MAE = %.3f\n", overall_rmse, overall_mae))

yearly_summary <- all_forecasts %>%
  group_by(season) %>%
  summarise(
    RMSE = rmse(get(target_var), pred),
    MAE = mae(get(target_var), pred),
    n = n()
  )
print(yearly_summary)

weekly_summary <- all_forecasts %>%
  group_by(week) %>%
  summarise(
    RMSE = rmse(get(target_var), pred),
    MAE = mae(get(target_var), pred),
    n = n()
  )
print(weekly_summary, n = nrow(weekly_summary))

ggplot(all_forecasts, aes(x = !!sym(target_var), y = pred)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = paste("Actual vs Predicted", target_var),
       x = paste("Actual", target_var),
       y = paste("Predicted", target_var)) +
  theme_minimal()

ggplot(all_forecasts, aes(x = !!sym(target_var), y = pred)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ season) +
  labs(title = paste("Actual vs Predicted", target_var, "by Forecast Year"),
       x = paste("Actual", target_var),
       y = paste("Predicted", target_var)) +
  theme_minimal()

ggplot(all_forecasts, aes(x = !!sym(target_var), y = pred)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ week) +
  labs(title = paste("Actual vs Predicted", target_var, "by Forecast Week"),
       x = paste("Actual", target_var),
       y = paste("Predicted", target_var)) +
  theme_minimal()