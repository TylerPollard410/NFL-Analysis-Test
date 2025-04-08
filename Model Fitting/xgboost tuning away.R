
# 0. Setup & Libraries ----------------------------------------------

# Make sure you have the following packages installed:
# install.packages(c("xgboost","tidyverse","Metrics","rBayesianOptimization"))

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
               "away_score", "away_score", "result", "spread_line", "spreadCover",
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
complete_rows <- complete.cases(candidate_data)
complete_rows_ID <- nfl_data |> filter(complete_rows) |> pull(game_id)
candidate_data <- candidate_data |> filter(complete_rows)

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

### 2.A.2. Near-zero variance ----
#nzv <- nearZeroVar(nfl_data[, candidate_xgb_vars_clean])
nzv <- nearZeroVar(candidate_data)
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

## 2.B. Final Model Data -----
nfl_data_model <- nfl_data |>
  filter(game_id %in% complete_rows_ID) |>
  arrange(season, week)


# 3. Build Rolling Folds (3 Seasons -> Next Season) ----------

# For each start_season in 2007..2021, train on [start_season..(start_season+2)],
# test on (start_season+3).

build_rolling_folds <- function(data, start_years = 2007:2021, window_size = 3) {
  folds_list <- list()
  
  for (start_season in start_years) {
    train_seasons <- start_season:(start_season + window_size - 1)
    test_season   <- start_season + window_size
    
    train_data <- data %>% filter(season %in% train_seasons)
    test_data  <- data %>% filter(season == test_season)
    
    if (nrow(test_data) == 0) {
      next
    }
    
    folds_list[[as.character(start_season)]] <- list(
      train = train_data,
      test  = test_data
    )
  }
  folds_list
}

rolling_folds <- build_rolling_folds(nfl_data_model, 
                                     start_years=2007:2021,
                                     window_size=3)
cat("Number of rolling folds:", length(rolling_folds), "\n")


# NEW: Function to Compute Time-Based CV Indices for Inner CV (6 folds)
# This function computes the training and test indices for a given training block.
# The "boundaries" vector should specify the ending row of each test segment.
# For example, if your training block (sorted by season then week) has 803 rows,
# and you want the following splits:
#   - Fold 1 Test: rows (267+1) to 359 (i.e., season 2 weeks 1-6)
#   - Fold 2 Test: rows 360 to 443 (season 2 weeks 7-12)
#   - Fold 3 Test: rows 444 to 534 (season 2 weeks >12)
#   - Fold 4 Test: rows 535 to 625 (season 3 weeks 1-6)
#   - Fold 5 Test: rows 626 to 711 (season 3 weeks 7-12)
#   - Fold 6 Test: rows 712 to 803 (season 3 weeks >12)
# Then you would set boundaries = c(267, 359, 443, 534, 625, 711, 803)
# get_time_based_cv_indices <- function(train_data, boundaries) {
#   n <- nrow(train_data)
#   if (tail(boundaries, 1) != n) {
#     stop("The last boundary must equal the number of rows in train_data.")
#   }
#   n_folds <- length(boundaries) - 1
#   train_folds <- list()
#   test_folds <- list()
#   for(i in 1:n_folds) {
#     train_folds[[i]] <- 1:boundaries[i]
#     test_folds[[i]] <- (boundaries[i] + 1):boundaries[i+1]
#   }
#   list(train = train_folds, test = test_folds)
# }
# 
# # Example: For a training block of 803 rows, you might use:
# boundaries_example <- c(267, 359, 443, 534, 625, 711, 803)
# (You may adjust these numbers based on your actual training block composition.)

get_time_based_cv_indices <- function(train_data, 
                                      season_col = "season", 
                                      week_col = "week",
                                      # Week boundaries for splitting test within a season:
                                      bound1 = 6, bound2 = 12) {
  # Assume train_data is sorted by season then week.
  seasons <- sort(unique(train_data[[season_col]]))
  if(length(seasons) != 3) {
    stop("train_data must contain exactly 3 seasons.")
  }
  
  first_season  <- seasons[1]
  second_season <- seasons[2]
  third_season  <- seasons[3]
  
  # Get indices for each fold:
  # We work with row numbers (assuming train_data is in the correct order)
  all_idx <- 1:nrow(train_data)
  
  # Helper: indices for a given season satisfying a week condition.
  idx_for_season <- function(season_val, condition_fun) {
    which(train_data[[season_col]] == season_val & condition_fun(train_data[[week_col]]))
  }
  
  # For season 1 (first season): all indices
  idx_first <- which(train_data[[season_col]] == first_season)
  
  # For season 2 (second season)
  idx_second <- which(train_data[[season_col]] == second_season)
  # Define test folds for season 2:
  test_idx_2_fold1 <- idx_for_season(second_season, function(w) w <= bound1)
  test_idx_2_fold2 <- idx_for_season(second_season, function(w) w >= (bound1+1) & w <= bound2)
  test_idx_2_fold3 <- idx_for_season(second_season, function(w) w > bound2)
  
  # For season 3 (third season)
  idx_third <- which(train_data[[season_col]] == third_season)
  test_idx_3_fold1 <- idx_for_season(third_season, function(w) w <= bound1)
  test_idx_3_fold2 <- idx_for_season(third_season, function(w) w >= (bound1+1) & w <= bound2)
  test_idx_3_fold3 <- idx_for_season(third_season, function(w) w > bound2)
  
  # Now, define training indices for each fold.
  # For folds from season 2:
  # Fold 1: Train = all of season 1.
  train_idx_fold1 <- 1:(min(test_idx_2_fold1) - 1) #idx_first
  # Fold 2: Train = season 1 + season 2 with week ≤ bound1.
  train_idx_fold2 <- 1:(min(test_idx_2_fold2) - 1) #c(idx_first, idx_for_season(second_season, function(w) w <= bound1))
  # Fold 3: Train = season 1 + all of season 2.
  train_idx_fold3 <- 1:(min(test_idx_2_fold3) - 1) #c(idx_first, idx_second)
  
  # For folds from season 3:
  # Fold 4: Train = all data from seasons 1 and 2.
  train_idx_fold4 <- 1:(min(test_idx_3_fold1) - 1) #c(idx_first, idx_second)
  # Fold 5: Train = seasons 1 & 2 + season 3 with week ≤ bound1.
  train_idx_fold5 <- 1:(min(test_idx_3_fold2) - 1) #c(idx_first, idx_second, idx_for_season(third_season, function(w) w <= bound1))
  # Fold 6: Train = all data from seasons 1,2,3.
  train_idx_fold6 <- 1:(min(test_idx_3_fold3) - 1) #c(idx_first, idx_second, idx_third)
  
  # Now, package the indices into a list for xgb.cv:
  # folds <- list(
  #   # Test folds:
  #   fold1_test = test_idx_2_fold1,
  #   fold2_test = test_idx_2_fold2,
  #   fold3_test = test_idx_2_fold3,
  #   fold4_test = test_idx_3_fold1,
  #   fold5_test = test_idx_3_fold2,
  #   fold6_test = test_idx_3_fold3
  # )
  # 
  # train_folds <- list(
  #   fold1_train = train_idx_fold1,
  #   fold2_train = train_idx_fold2,
  #   fold3_train = train_idx_fold3,
  #   fold4_train = train_idx_fold4,
  #   fold5_train = train_idx_fold5,
  #   fold6_train = train_idx_fold6
  # )
  
  folds <- list(
    # Test folds:
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
# Suppose 'train_block' is the training portion of a rolling fold (e.g., from 2018 to 2020).
# It must be sorted by season then week.
# For instance, if train_block has 803 rows (with 267 in season1, 92+86+? in season2, etc.),
# then:
# boundaries are implicitly set by our rule.
# We call:
cv_indices <- get_time_based_cv_indices(rolling_folds$`2018`$train, 
                                        season_col = "season",
                                        week_col = "week", 
                                        bound1 = 6, 
                                        bound2 = 12)
# cv_indices$train and cv_indices$test are lists of indices that you pass to xgb.cv as:
#    train_folds = cv_indices$train, folds = cv_indices$test



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. APPROACH A: Per-Fold Hyperparameter Tuning  ----------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Here, we run a separate Bayesian Optimization for each fold (3-season block).
# We'll store a "best_params" for each fold.

# We'll define a function that does the Bayesian search for a SINGLE fold:
#   - We'll do an internal K-fold CV on the training portion if we want,
#     or we can do xgb.train + xgb.cv directly. 
#   - Because each block is smaller, we can do a simpler approach (random folds or time-based folds).
#   - Then we train final model on that block's entire training data, 
#     evaluate on the test data, store results, etc.

# Let's define the function to run xgb.cv for each candidate param set on the *training* part only
# We'll do a 3-5 fold random split within that training block for the hyperparameter search.

# Here, we run a separate Bayesian Optimization for each rolling fold (3-season block).
# We update the cv_for_fold function to use time-based CV indices computed by get_time_based_cv_indices().

cv_for_fold <- function(max_depth, 
                        eta, 
                        gamma, 
                        colsample_bytree, 
                        min_child_weight,
                        subsample,
                        lambda, 
                        alpha,
                        train_df,
                        feature_names,
                        nrounds = 500, 
                        early_stopping = 20,
                        time_folds) {
  tryCatch({
    max_depth        <- as.integer(round(max_depth, 0))
    min_child_weight <- as.integer(round(min_child_weight, 0))
    
    params <- list(
      booster          = "gbtree",
      objective        = "reg:squarederror",
      eval_metric      = "rmse",
      max_depth        = max_depth,
      eta              = eta,
      gamma            = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample        = subsample,
      lambda           = lambda,
      alpha            = alpha
    )
    
    dtrain <- xgb.DMatrix(
      data  = as.matrix(train_df[, feature_names]),
      label = train_df$away_score
    )
    
    # Use pre-defined time-based CV indices:
    train_fold_list <- time_folds[["train"]]
    test_fold_list <- time_folds[["test"]]
    xgb_cv_out <- xgb.cv(
      params               = params,
      data                 = dtrain,
      nrounds              = nrounds,
      train_folds          = train_fold_list,
      folds                = test_fold_list,
      early_stopping_rounds= early_stopping,
      verbose              = FALSE
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
    # Return a high penalty so that these parameter regions are avoided.
    return(list(Score = 1e6, Pred = NA))
  })
}

# Wrapper for Bayesian Optimization on the training block using time-based CV:
tune_fold_bayes <- function(train_data, 
                            feature_names,
                            time_folds,
                            init_points = 5,
                            n_iter = 10, 
                            nrounds = 500,
                            early_stop = 20) {
  f_wrapper <- function(max_depth, eta, gamma, colsample_bytree, min_child_weight,
                        subsample, lambda, alpha) {
    result <- cv_for_fold(
      max_depth        = max_depth, 
      eta              = eta, 
      gamma            = gamma, 
      colsample_bytree = colsample_bytree, 
      min_child_weight = min_child_weight,
      subsample        = subsample,
      lambda           = lambda,
      alpha            = alpha,
      train_df         = train_data,
      feature_names    = feature_names,
      nrounds          = nrounds,
      early_stopping   = early_stop,
      time_folds       = time_folds
    )
    # message("Trial: ", paste("max_depth=", max_depth, 
    #                          "eta=", eta,
    #                          "gamma=", gamma,
    #                          "colsample_bytree=", colsample_bytree,
    #                          "min_child_weight=", min_child_weight,
    #                          "subsample=", subsample,
    #                          "lambda=", lambda,
    #                          "alpha=", alpha, 
    #                          "Score=", result$Score))
    result
  }
  
  
  bounds <- list(
    max_depth         = c(2L, 8L),
    eta               = c(0.01, 0.3),
    gamma             = c(1e-6, 5),
    colsample_bytree  = c(0.5, 1.0),
    min_child_weight  = c(1L, 10L),
    subsample         = c(0.5, 1.0),
    lambda            = c(1e-6, 2),
    alpha             = c(1e-6, 2)
  )
  
  set.seed(999)
  opt_res <- tryCatch({
    BayesianOptimization(
      FUN         = f_wrapper,
      bounds      = bounds,
      init_points = init_points,
      n_iter      = n_iter,
      acq         = "ucb",
      kappa       = 2.576,
      eps         = 0.0,
      verbose     = TRUE
    )
  }, error = function(e) {
    message("Error during BayesianOptimization:")
    message(e$message)
    NULL
  })
  if (is.null(opt_res)) {
    stop("BayesianOptimization failed. Check the parameter bounds or increase init_points.")
  } else {
    # Optionally, inspect the history:
    print(opt_res$History)
  }
  
  opt_res
}

# Now loop through each rolling fold (higher-level fold) and perform time-based tuning.
per_fold_results <- list()
per_fold_predictions <- data.frame()

system.time(
  for (fold_name in names(rolling_folds)) {
    cat("\n==========\nTuning for fold:", fold_name, "\n")
    train_df <- rolling_folds[[fold_name]]$train
    test_df  <- rolling_folds[[fold_name]]$test
    
    # Compute time-based CV indices for this training block using your boundaries.
    time_folds <- get_time_based_cv_indices(train_df, 
                                            season_col = "season",
                                            week_col = "week", 
                                            bound1 = 6, 
                                            bound2 = 12)
    
    # 1) Bayesian Tuning using time-based CV indices:
    fold_bayes_res <- tune_fold_bayes(
      train_data    = train_df,
      feature_names = candidate_xgb_vars_clean, #candidate_xgb_vars,
      time_folds    = time_folds,
      init_points   = 10,
      n_iter        = 15,
      nrounds       = 500,
      early_stop    = 20
    )
    
    best_params <- fold_bayes_res$Best_Par
    cat("Best Params for fold", fold_name, ":\n")
    print(best_params)
    
    # 2) Identify best iteration for final training on the full training block using the same time-based CV indices:
    best_cv <- cv_for_fold(
      max_depth        = best_params["max_depth"], 
      eta              = best_params["eta"], 
      gamma            = best_params["gamma"], 
      colsample_bytree = best_params["colsample_bytree"], 
      min_child_weight = best_params["min_child_weight"],
      subsample        = best_params["subsample"],
      lambda           = best_params["lambda"],
      alpha            = best_params["alpha"],
      train_df         = train_df,
      feature_names    = candidate_xgb_vars_clean,
      nrounds          = 500,
      early_stopping   = 20,
      time_folds       = time_folds
    )
    
    final_params_list <- list(
      booster          = "gbtree",
      objective        = "reg:squarederror",
      eval_metric      = "rmse",
      max_depth        = as.integer(best_params["max_depth"]), 
      eta              = best_params["eta"], 
      gamma            = best_params["gamma"], 
      colsample_bytree = best_params["colsample_bytree"], 
      min_child_weight = as.integer(best_params["min_child_weight"]),
      subsample        = best_params["subsample"],
      lambda           = best_params["lambda"],
      alpha            = best_params["alpha"]
    )
    
    dtrain_fold <- xgb.DMatrix(
      data  = as.matrix(train_df[, candidate_xgb_vars_clean]),
      label = train_df$away_score
    )
    
    set.seed(999)
    xgb_cv_run <- xgb.cv(
      params  = final_params_list,
      data    = dtrain_fold,
      nrounds = 500,
      train_folds = time_folds$train,
      folds       = time_folds$test,
      early_stopping_rounds = 20,
      verbose = FALSE
    )
    
    final_nrounds <- xgb_cv_run$best_iteration
    
    xgb_final_model <- xgb.train(
      params  = final_params_list,
      data    = dtrain_fold,
      nrounds = final_nrounds,
      watchlist = list(train = dtrain_fold),
      verbose = 0
    )
    
    dtest_fold <- xgb.DMatrix(
      data  = as.matrix(test_df[, candidate_xgb_vars_clean]),
      label = test_df$away_score
    )
    
    fold_pred <- predict(xgb_final_model, dtest_fold)
    fold_rmse <- rmse(test_df$away_score, fold_pred)
    fold_mae  <- mae(test_df$away_score, fold_pred)
    
    cat(sprintf("Fold %s => Test RMSE: %.3f | MAE: %.3f\n", fold_name, fold_rmse, fold_mae))
    
    per_fold_results[[fold_name]] <- list(
      best_params   = best_params,
      nrounds       = final_nrounds,
      fold_rmse     = fold_rmse,
      fold_mae      = fold_mae,
      train_seasons = paste(unique(train_df$season), collapse = ","),
      test_season   = paste(unique(test_df$season), collapse = ",")
    )
    
    temp_pred_df <- test_df %>%
      select(game_id, season, week, away_score) %>%
      mutate(pred_away_score = fold_pred, fold_name = fold_name)
    per_fold_predictions <- bind_rows(per_fold_predictions, temp_pred_df)
  }
)

# Summarize fold-level results:
per_fold_summary <- map_dfr(per_fold_results, function(x) {
  tibble(
    best_params = paste(names(x$best_params), 
                        round(unlist(x$best_params), 3), 
                        sep="=", collapse=", "),
    nrounds = x$nrounds,
    fold_rmse = x$fold_rmse,
    fold_mae = x$fold_mae,
    train_seasons = x$train_seasons,
    test_season = x$test_season
  )
})

cat("\n=== Per-Fold Hyperparameter Tuning Results (Time-Based CV) ===\n")
print(per_fold_summary)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. APPROACH B: Single Global Hyperparameter Tuning (All Folds Combined)  ----------
# We do one big Bayesian Optimization. For each hyperparameter set,
# we loop over all rolling folds, train on each fold's 3-season block, predict next season,
# compute average RMSE, and use that as the "score."

global_rolling_cv <- function(max_depth, 
                              eta, 
                              gamma, 
                              colsample_bytree, 
                              min_child_weight,
                              subsample, 
                              lambda,
                              alpha,
                              folds_list, 
                              feature_names,
                              nrounds = 500, 
                              early_stopping = 20) {
  
  max_depth <- as.integer(round(max_depth, 0))
  min_child_weight <- as.integer(round(min_child_weight, 0))
  
  params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    max_depth        = max_depth,
    eta              = eta,
    gamma            = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample        = subsample,
    lambda           = lambda,
    alpha            = alpha
  )
  
  rmse_vec <- c()
  
  # Loop over each rolling fold
  for (f_name in names(folds_list)) {
    train_df <- folds_list[[f_name]]$train
    test_df  <- folds_list[[f_name]]$test
    
    dtrain <- xgb.DMatrix(
      data  = as.matrix(train_df[, feature_names]),
      label = train_df$away_score
    )
    dtest <- xgb.DMatrix(
      data  = as.matrix(test_df[, feature_names]),
      label = test_df$away_score
    )
    
    xgb_model <- xgb.train(
      params                = params,
      data                  = dtrain,
      nrounds               = nrounds,
      watchlist             = list(train = dtrain, test = dtest),
      early_stopping_rounds = early_stopping,
      verbose               = 0
    )
    
    pred_test <- predict(xgb_model, dtest)
    rmse_val <- rmse(test_df$away_score, pred_test)
    rmse_vec <- c(rmse_vec, rmse_val)
  }
  
  avg_rmse <- mean(rmse_vec)
  
  # Return negative RMSE so that the optimizer maximizes the score.
  list(Score = -avg_rmse, Pred = NA)
}

global_bayes_wrapper <- function(max_depth,
                                 eta, 
                                 gamma, 
                                 colsample_bytree, 
                                 min_child_weight,
                                 subsample, 
                                 lambda,
                                 alpha) {
  global_rolling_cv(
    max_depth        = max_depth,
    eta              = eta,
    gamma            = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample        = subsample,
    lambda           = lambda,
    alpha            = alpha,
    folds_list       = rolling_folds,
    feature_names    = candidate_xgb_vars_clean,
    nrounds          = 500,
    early_stopping   = 20
  )
}

# Update the bounds: use small positive numbers for gamma, lambda, and alpha.
bounds_global <- list(
  max_depth         = c(2L, 8L),
  eta               = c(0.01, 0.3),
  gamma             = c(1e-6, 5),
  colsample_bytree  = c(0.5, 1.0),
  min_child_weight  = c(1L, 10L),
  subsample         = c(0.5, 1.0),
  lambda            = c(1e-6, 2),
  alpha             = c(1e-6, 2)
)

set.seed(999)
global_opt_res <- tryCatch({
  BayesianOptimization(
    FUN         = global_bayes_wrapper,
    bounds      = bounds_global,
    init_points = 10,      # increased initial points
    n_iter      = 15,       # increased iterations
    acq         = "ucb",    # using UCB for robust performance near the optimum
    kappa       = 2.576,
    eps         = 0.0,
    verbose     = TRUE
  )
}, error = function(e) {
  message("Error during Global BayesianOptimization: ", e$message)
  NULL
})

if (is.null(global_opt_res)) {
  stop("Global BayesianOptimization failed. Check the parameter bounds or increase init_points.")
} else {
  cat("\n=== Global Bayesian Optimization History ===\n")
  print(global_opt_res$History)
}

cat("\n=== Single Global Hyperparameter Tuning Result ===\n")
print(global_opt_res)

global_best_params <- global_opt_res$Best_Par
cat("\nGlobal Best Params:\n")
print(global_best_params)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 5.B. Evaluate the Single Best Set Across Folds  ----------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Now we do a final pass: For each fold, train with the single global best param set,
# record performance. Then we can compare to the per-fold approach.

evaluate_global_best <- function(hparams, 
                                 folds_list, 
                                 feature_names,
                                 nrounds=500, 
                                 early_stopping=20) {
  results <- data.frame()
  predictions <- data.frame()
  
  # Convert to integer if needed
  hparams[["max_depth"]]       <- as.integer(round(hparams[["max_depth"]],0))
  hparams[["min_child_weight"]] <- as.integer(round(hparams[["min_child_weight"]],0))
  
  final_params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    max_depth        = hparams[["max_depth"]],
    eta              = hparams[["eta"]],
    gamma            = hparams[["gamma"]],
    colsample_bytree = hparams[["colsample_bytree"]],
    min_child_weight = hparams[["min_child_weight"]],
    subsample        = hparams[["subsample"]],
    lambda           = hparams[["lambda"]],
    alpha            = hparams[["alpha"]]
  )
  
  for (f_name in names(folds_list)) {
    train_df <- folds_list[[f_name]]$train
    test_df  <- folds_list[[f_name]]$test
    
    dtrain <- xgb.DMatrix(as.matrix(train_df[, feature_names]), 
                          label=train_df$away_score)
    dtest  <- xgb.DMatrix(as.matrix(test_df[, feature_names]),
                          label=test_df$away_score)
    
    xgb_final <- xgb.train(
      params               = final_params,
      data                 = dtrain,
      nrounds              = nrounds,
      watchlist            = list(train=dtrain, test=dtest),
      early_stopping_rounds= early_stopping,
      verbose              = 0
    )
    
    test_pred <- predict(xgb_final, dtest)
    fold_rmse <- rmse(test_df$away_score, test_pred)
    fold_mae  <- mae(test_df$away_score, test_pred)
    
    results <- rbind(
      results,
      data.frame(
        fold_name     = f_name,
        train_seasons = paste(unique(train_df$season), collapse=","),
        test_season   = paste(unique(test_df$season), collapse=","),
        RMSE          = fold_rmse,
        MAE           = fold_mae,
        stringsAsFactors = FALSE
      )
    )
    
    temp_pred_df <- test_df %>%
      select(game_id, season, week, away_score) %>%
      mutate(
        pred_away_score = test_pred,
        fold_name       = f_name
      )
    predictions <- bind_rows(predictions, temp_pred_df)
  }
  list(performance = results, predictions = predictions)
}

global_eval <- evaluate_global_best(
  hparams      = global_best_params,
  folds_list   = rolling_folds,
  feature_names= candidate_xgb_vars_clean,
  nrounds      = 500,
  early_stopping = 20
)

cat("\n=== Performance of the Single Global Best Param Set, Fold-by-Fold ===\n")
print(global_eval$performance)

global_summary <- global_eval$performance %>%
  summarise(
    Mean_RMSE = mean(RMSE),
    Mean_MAE  = mean(MAE)
  )

cat("\nSingle Global Param Averages => RMSE:", round(global_summary$Mean_RMSE,3),
    " | MAE:", round(global_summary$Mean_MAE,3), "\n")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. Compare Both Approaches  ----------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We have:
#   1) per_fold_summary: results from separate tuning per fold
#   2) global_eval$performance: results from one global param set

# Let’s combine them for a quick comparison:
compare_df <- per_fold_summary %>%
  select(train_seasons, test_season, fold_rmse, fold_mae) %>%
  mutate(Approach = "Per-Fold Tuning") %>%
  rename(RMSE=fold_rmse, MAE=fold_mae) %>%
  bind_rows(
    global_eval$performance %>%
      select(train_seasons, test_season, RMSE, MAE) %>%
      mutate(Approach = "Global Tuning")
  ) %>%
  arrange(test_season, Approach)

cat("\n=== Comparison of Per-Fold vs Global Tuning (fold by fold) ===\n")
print(compare_df, n = nrow(compare_df))

# Summaries:
comparison_summary <- compare_df %>%
  group_by(Approach) %>%
  summarise(
    Mean_RMSE = mean(RMSE),
    Mean_MAE  = mean(MAE),
    Folds     = n()
  )

cat("\n=== Overall Summaries ===\n")
print(comparison_summary)

# Done! 
# Next steps: 
# - Inspect which approach yields better average metrics. 
# - Possibly investigate the variance from fold to fold. 
# - Decide whether you prefer a single stable param set or a fold-specific approach.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 7. Variable Importance & Feature Reduction ------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create final parameters from the global best tuning result:
final_params <- list(
  booster          = "gbtree",
  objective        = "reg:squarederror",
  eval_metric      = "rmse",
  max_depth        = as.integer(global_best_params[["max_depth"]]),
  eta              = global_best_params[["eta"]],
  gamma            = global_best_params[["gamma"]],
  colsample_bytree = global_best_params[["colsample_bytree"]],
  min_child_weight = as.integer(global_best_params[["min_child_weight"]]),
  subsample        = global_best_params[["subsample"]],
  lambda           = global_best_params[["lambda"]],
  alpha            = global_best_params[["alpha"]]
)

# Fit a model on all historical data (nfl_data_model) using candidate_xgb_vars_clean.
dtrain_full <- xgb.DMatrix(
  data = as.matrix(nfl_data_model[, candidate_xgb_vars_clean]),
  label = nfl_data_model$away_score
)

# You can choose a fixed nrounds (e.g. 500) or run a CV to choose nrounds.
final_model_full <- xgb.train(
  params = final_params,
  data = dtrain_full,
  nrounds = 500,
  #early_stopping_rounds = 20,
  verbose = 1
)

## 7.B. Compute variable importance: -----
importance_matrix <- xgb.importance(
  feature_names = candidate_xgb_vars_clean,
  model = final_model_full)
cat("\n=== Variable Importance ===\n")
print(importance_matrix)

# Optionally, plot importance:
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "XGBoost Variable Importance", x = "Feature", y = "Gain") +
  theme_minimal()

# Option 1: Use a fixed number of top features (e.g., top 20)
top_features <- head(importance_matrix[order(-importance_matrix$Gain), ], 20)$Feature
cat("\n=== Top 20 Features ===\n")
print(top_features)


### 7.B.1. Threshold Based ----
# Compute cumulative gain and plot it:
importance_matrix_gain <- importance_matrix[order(-importance_matrix$Gain), ]
importance_matrix_gain$cumGain <- cumsum(importance_matrix_gain$Gain) / sum(importance_matrix_gain$Gain)

ggplot(importance_matrix_gain, aes(x = reorder(Feature, -Gain), y = cumGain)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Cumulative Gain by Feature", x = "Feature", y = "Cumulative Gain") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Option 2: Use a cumulative gain threshold (e.g., 90% cumulative gain)
threshold <- 0.90
top_features_threshold <- importance_matrix_gain$Feature[importance_matrix_gain$cumGain <= threshold]
cat("Features contributing to 90% cumulative gain:\n")
print(top_features_threshold)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 8. Rolling One-Week-Ahead Forecasting ------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We now refit a model and produce out-of-sample predictions in a production-like,
# one-week-ahead fashion. For each forecast year (2010 to 2024),
# we use a base training set from the prior three seasons. Then for the forecast year,
# we update the model week-by-week as new games become available.

rolling_forecast <- function(data, 
                             base_years, 
                             forecast_year, 
                             features, 
                             final_params, 
                             nrounds_fixed = 500) {
  # data: full historical data (nfl_data_model)
  # base_years: a vector of three consecutive seasons used as initial training (e.g., 2007:2009)
  # forecast_year: the year for which we want out-of-sample predictions (e.g., 2010)
  # features: vector of feature names (here, top_features)
  # final_params: hyperparameters (as defined from global tuning)
  # nrounds_fixed: fixed number of rounds to use (you can also perform CV here if desired)
  
  # data <- nfl_data_model
  # base_years <- 2007:2010
  # forecast_year <- 2011
  # features <- top_features_threshold
  # final_params <- final_params
  # nrounds_fixed <- 500
  
  # Extract forecast-year data (assumed sorted by week)
  forecast_data <- data %>% filter(season == forecast_year) %>% arrange(week)
  # Initialize training set with all data from base_years.
  train_set <- data %>% filter(season %in% base_years) %>% arrange(season, week)
  
  # Data frame to store forecasts:
  forecast_preds <- data.frame()
  
  # Loop over each unique week in the forecast year:
  unique_weeks <- sort(unique(forecast_data$week))
  for (w in unique_weeks) {
    #w <- 1
    # Use all training data available up to (but not including) week w of the forecast year.
    current_train <- rbind(
      train_set,
      forecast_data %>% filter(week < w)
    )
    
    # The current test block is the forecast data for week == w.
    current_test <- forecast_data %>% filter(week == w)
    
    # Create DMatrix objects using the selected features.
    dtrain <- xgb.DMatrix(
      data = as.matrix(current_train |> select(features)),
      label = current_train$away_score
    )
    dtest <- xgb.DMatrix(
      data = as.matrix(current_test |> select(features))
    )
    
    # Fit model using final_params and fixed nrounds.
    # (Alternatively, you could perform a quick CV to choose nrounds for each week.)
    model <- xgb.train(
      params = final_params,
      data = dtrain,
      nrounds = nrounds_fixed,
      verbose = 1
    )
    
    preds <- predict(model, dtest)
    current_test$pred_away_score <- preds
    forecast_preds <- rbind(forecast_preds, current_test)
    
    # Update train_set by adding the actual outcomes for week w.
    # (Here we assume the actual outcomes are in current_test$away_score.)
    train_set <- rbind(train_set, current_test %>% select(-pred_away_score))
  }
  return(forecast_preds)
}

# Now loop over forecast years from 2010 to 2024.
forecast_years <- 2010:2024
rolling_results <- list()

for (yr in forecast_years) {
  # Define base training years as the three seasons immediately preceding yr.
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

# Combine all forecasts:
all_forecasts <- do.call(rbind, rolling_results)

# If actual outcomes are available in nfl_data_model for forecast years,
# evaluate the overall out-of-sample performance:
overall_rmse <- rmse(all_forecasts$away_score, all_forecasts$pred_away_score)
overall_mae <- mae(all_forecasts$away_score, all_forecasts$pred_away_score)
cat(sprintf("\nOverall Out-of-Sample Forecast Performance: RMSE = %.3f, MAE = %.3f\n", overall_rmse, overall_mae))


# Summarize performance by forecast year (season)
yearly_summary <- all_forecasts %>%
  group_by(season) %>%
  summarise(
    RMSE = rmse(away_score, pred_away_score),
    MAE  = mae(away_score, pred_away_score),
    n    = n()
  )
print(yearly_summary)

# Summarize performance by forecast week
weekly_summary <- all_forecasts %>%
  group_by(week) %>%
  summarise(
    RMSE = rmse(away_score, pred_away_score),
    MAE  = mae(away_score, pred_away_score),
    n    = n()
  )
print(weekly_summary, n = nrow(weekly_summary))

ggplot(yearly_summary, aes(x = season, y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Forecast RMSE by Year", x = "Forecast Year", y = "RMSE") +
  theme_minimal()

ggplot(weekly_summary, aes(x = week, y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Forecast RMSE by Week", x = "Forecast Week", y = "RMSE") +
  theme_minimal()


# Basic scatter plot of Actual vs. Predicted home scores
ggplot(all_forecasts, aes(x = away_score, y = pred_away_score)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  smplot2::sm_statCorr() +
  labs(title = "Actual vs Predicted Home Score",
       x = "Actual Home Score",
       y = "Predicted Home Score") +
  theme_minimal()


ggplot(all_forecasts, aes(x = away_score, y = pred_away_score)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  smplot2::sm_statCorr() +
  facet_wrap(~ season) +
  labs(title = "Actual vs Predicted Home Score by Forecast Year",
       x = "Actual Home Score",
       y = "Predicted Home Score") +
  theme_minimal()


ggplot(all_forecasts, aes(x = away_score, y = pred_away_score)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  smplot2::sm_statCorr() +
  facet_wrap(~ week) +
  labs(title = "Actual vs Predicted Home Score by Forecast Year",
       x = "Actual Home Score",
       y = "Predicted Home Score") +
  theme_minimal()


# 9. Save Model Info -----
final_xgb_model_away <- final_model_full
final_xgb_params_away <- final_params
final_xgb_importance_away <- importance_matrix_gain
final_xgb_forecasts_away <- all_forecasts

file_loc <- "~/Desktop/NFL Analysis Data/finalXGBmodels/"

save(final_xgb_model_away,
     file = paste0(file_loc, "final_xgb_model_away", ".rda"))
save(final_xgb_params_away,
     file = paste0(file_loc, "final_xgb_params_away", ".rda"))
save(final_xgb_importance_away,
     file = paste0(file_loc, "final_xgb_importance_away", ".rda"))
save(final_xgb_forecasts_away,
     file = paste0(file_loc, "final_xgb_forecasts_away", ".rda"))

# 10. Betting Performance ----
load(file = paste0(file_loc, "final_xgb_forecasts_home", ".rda"))
load(file = paste0(file_loc, "final_xgb_forecasts_away", ".rda"))

xgb_cv_preds <- nfl_data |> 
  select(
    game_id, season, week,
    home_score,
    away_score,
    result,
    total
  ) |>
  left_join(
    final_xgb_forecasts_home |> 
      select(game_id, xgb_home_score = pred_home_score),
    by = join_by(game_id)
  ) |>
  left_join(
    final_xgb_forecasts_away |> 
      select(game_id, xgb_away_score = pred_away_score),
    by = join_by(game_id)
  ) |>
  # left_join(
  #   xgb_result_pred |> 
  #     select(game_id, xgb_result = pred),
  #   by = join_by(game_id)
  # ) |>
  # left_join(
  #   xgb_total_pred |> 
  #     select(game_id, xgb_total = pred),
  #   by = join_by(game_id)
  # ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) 
  #relocate(xgb_result, .after = result) |>
  #relocate(xgb_total, .after = total)

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
      # exp_result = xgb_result,
      # exp_result_cover = case_when(
      #   exp_result > spread_line ~ "Home",
      #   exp_result < spread_line ~ "Away",
      #   TRUE ~ NA_character_
      # ),
      exp_result2 = xgb_home_score - xgb_away_score,
      exp_result2_cover = case_when(
        exp_result2 > spread_line ~ "Home",
        exp_result2 < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      #correct_result = exp_result_cover == actual_result_cover,
      correct_result2 = exp_result2_cover == actual_result_cover
    ) |>
    mutate(
      actual_total_cover = case_when(
        total > total_line ~ "Over",
        total < total_line ~ "Under",
        TRUE ~ NA_character_
      ),
      # exp_total = xgb_total,
      # exp_total_cover = case_when(
      #   exp_total > total_line ~ "Over",
      #   exp_total < total_line ~ "Under",
      #   TRUE ~ NA_character_
      # ),
      exp_total2 = xgb_home_score + xgb_away_score,
      exp_total2_cover = case_when(
        exp_total2 > total_line ~ "Over",
        exp_total2 < total_line ~ "Under",
        TRUE ~ NA_character_
      ),
      #correct_total = exp_total_cover == actual_total_cover,
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
      Result_Bets = sum(!is.na(correct_result2)),
      #Acc_Result = round(mean(correct_result, na.rm = TRUE)*100, 2),
      Acc_Result2 = round(mean(correct_result2, na.rm = TRUE)*100, 2),
      Total_Bets = sum(!is.na(correct_total2)),
      #Acc_Total = round(mean(correct_total, na.rm = TRUE)*100, 2),
      Acc_Total2 = round(mean(correct_total2, na.rm = TRUE)*100, 2)
    )
  
  return(acc_df)
}

## 10.B. Output Accuracy ----
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


