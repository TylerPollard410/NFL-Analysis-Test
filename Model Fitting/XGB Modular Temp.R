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
library(tidyverse)

library(readr)
library(tidytext)
library(tidyr)
library(purrr)
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
# 2. PRE-PROCESSING ----
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
# 3. XGBOOST MODELING (LONG FORMAT SCORE MODEL) ----
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
         team_score, opponent_score,
         total, result,
         spread_line, total_line,
         all_of(candidate_features))

# A quick glimpse of the final data structure:
glimpse(xgb_data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. MODULAR FUNCTIONS FOR TUNING & FORECASTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ----- Function 1: tune_and_select_features() -----
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
  
  candidate_k <- seq(5, length(feature_cols), by = 5)
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

# ----- Function 2: train_production_model() -----
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

# ----- Function 3: iterative_weekly_forecast() -----
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
    assign("this_week_data", this_week_data, envir = .GlobalEnv)
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
  return(all_forecasts)
}

# ----- Function 4: run_full_pipeline() -----
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
    
    results_list[[as.character(fs)]] <- list(
      forecast_season = fs,
      tuning = tuning_output,
      forecasts = forecasts
    )
    
    saveRDS(results_list[[as.character(fs)]],
            file = paste0("forecast_results_season_", fs, ".rds"))
  }
  return(results_list)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. RUN THE FULL PIPELINE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define which forecast seasons to run.
# With our splitting approach: to forecast 2011, we need tuning training from 2007-2009 and tuning test = 2010.
# For example, run forecast seasons from 2011 through 2024.
forecast_seasons <- seq(2024, 2024)

fs <- 2024
data <- xgb_data
results_list <- list()

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
cat(best_features, sep = "\n")

prod_train <- data %>% filter(season %in% prod_train_seasons)

forecast_data <- data %>% filter(season == fs)

forecasts <- iterative_weekly_forecast(fs, prod_train, forecast_data, 
                                       best_hyperparams, best_features)

results_list[[as.character(fs)]] <- list(
  forecast_season = fs,
  tuning = tuning_output,
  forecasts = forecasts
)

saveRDS(results_list[[as.character(fs)]],
        file = paste0("forecast_results_season_", fs, ".rds"))

return(results_list)

# Run the full pipeline on the long-format (XGBoost) data.
# This will take tuning windows, perform hyperparameter/feature selection,
# then do iterative week-by-week forecasting.
pipeline_results <- run_full_pipeline(forecast_seasons, xgb_data, candidate_features)

saveRDS(pipeline_results, file = "full_pipeline_results.rds")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. BRMS MODELING (WIDE FORMAT: RESULT & TOTAL MODELS) ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Preprocess wide data as needed.
modData <- modData |> 
  mutate(net_off_epa_cum = home_off_epa_mean_cum - away_off_epa_mean_cum)

# Merge XGBoost predictions into wide data by game_id.
# (Ensure that xgb_predictions exist; if not, you may need to load them or run the xgb pipeline first.)
xgb_preds_all <- bind_rows(lapply(pipeline_results, function(x) x$tuning$forecast)) # if available; adjust accordingly
# If your XGBoost predictions are in a separate object, use that.
modData_wide <- modData |> 
  left_join(xgb_preds_all |> select(game_id, pred_score), by = "game_id") |>
  rename(xgb_pred = pred_score)

# Ensure key time variables remain and that other relevant context is preserved.

# Create rolling CV folds for wide data based on season:
wide_seasons <- sort(unique(modData_wide$season))
wide_folds <- list()
for(i in seq(from = 4, to = length(wide_seasons))) {
  train_seasons <- wide_seasons[(i-3):(i-1)]
  test_season   <- wide_seasons[i]
  wide_folds[[as.character(test_season)]] <- list(
    train = modData_wide |> filter(season %in% train_seasons),
    test  = modData_wide |> filter(season == test_season)
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 7. DEFINE A FUNCTION TO FIT BRMS MODELS ----
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
# 8. FIT BRMS MODELS ACROSS CV FOLDS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

brms_models <- list()
brms_summaries <- list()

# Define model formulas for the outcomes.
# Build formula.
brms_formula_result <- 
  bf(
    result ~
      xgb_pred + 
      week +
      net_off_epa_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "gaussian")

brms_formula_total <- 
  bf(
    total ~ 
      xgb_pred +
      week +
      net_off_epa_cum + 
      (1|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "gaussian")

for(season in names(wide_folds)) {
  cat("Fitting brms models for test season:", season, "\n")
  train_wide <- wide_folds[[season]]$train
  model_result <- fit_brms_model(train_wide, brms_formula_result)
  model_total  <- fit_brms_model(train_wide, brms_formula_total)
  
  brms_models[[season]] <- list(result = model_result, total = model_total)
  brms_summaries[[season]] <- list(
    result = summary(model_result),
    total  = summary(model_total)
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 9. POST-PROCESSING & MODEL EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

performance_metrics <- list()

for(season in names(wide_folds)) {
  test_wide <- wide_folds[[season]]$test
  
  # Posterior predictions for the result model.
  pp_result <- posterior_predict(brms_models[[season]]$result, newdata = test_wide)
  ci_result <- apply(pp_result, 2, quantile, probs = c(0.025, 0.975))
  coverage_result <- mean(test_wide$result >= ci_result[1, ] & test_wide$result <= ci_result[2, ])
  rmse_result <- sqrt(mean((colMeans(pp_result) - test_wide$result)^2))
  
  performance_metrics[[season]] <- list(
    result = list(coverage = coverage_result, RMSE = rmse_result)
    # Similarly, compute metrics for the total model if desired.
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 10. BETTING EVALUATION & PPC PLOTS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Betting Evaluation:
# (Insert your project-specific betting performance evaluation code here.)

# Generate PPC Plots for brms models.
for(season in names(brms_models)) {
  pp_check(brms_models[[season]]$result) +
    ggtitle(paste("PPC Plot - Result Model (Season", season, ")"))
  pp_check(brms_models[[season]]$total) +
    ggtitle(paste("PPC Plot - Total Model (Season", season, ")"))
  # Optionally, save these plots (e.g., with ggsave()).
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 11. SAVE FINAL OUTPUTS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saveRDS(brms_models, file = "brms_models.rds")
saveRDS(performance_metrics, file = "brms_performance_metrics.rds")
saveRDS(brms_summaries, file = "brms_model_summaries.rds")
