## Model home and away score

# 0. Load Libraries ----
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
library(espnscrapeR)
library(nflverse)

## Tidyverse
library(tidyverse)


# 1. Data Loading & Preparation -----------------------------------------------
# Load game data and feature-engineered data.
## 1.A. nflverse ----
source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

## 1.B. Features ----
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# feature engineered data cleaner
source("./app/R/clean_modData.R")

## 1.C. XGBoost ----
### 1.C.1. Models ----
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_home_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_away_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_result_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_total_model_final.rda")

### 1.C.2. Best features ----
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/varImp_final_all.rda")

### 1.C.3. Predictions ----
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_cv_preds.rda")

#### 1.C.3.a Model Seasons ---- 
xgb_seasons <- xgb_cv_preds |>
  filter(complete.cases(xgb_cv_preds)) |>
  pull(season) |>
  unique()

xgb_season_start <- min(xgb_seasons)
xgb_season_end <- max(xgb_seasons)


# 2. Model Data ----
## 2.A. Clean ----
model_data <- clean_modData(data = modData, 
                            season_start = xgb_season_start)

### 2.A.1. Add Time ID ----
model_data <- model_data |>
  mutate(
    time_id = as.integer(str_extract(game_id, "^\\d{4}_\\d{2}") |> str_remove("_")),
    .before = game_id
  )

## 2.B. Add XBG Predictions ----
model_data <- model_data |>
  left_join(
    xgb_cv_preds |>
      select(game_id, contains("xgb")), 
    by = join_by(game_id)
  ) |>
  mutate(
    xgb_result_calc = xgb_home_score - xgb_away_score,
    xgb_total_calc = xgb_home_score + xgb_away_score,
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, xgb_result_calc, .after = result) |>
  relocate(xgb_total, xgb_total_calc, .after = total)

## 2.C. Define Variable Sets ----
### 2.C.1 Identifiers ----
id_vars <- c("time_id", "game_id",
             "season", "week", "season_type",
             "home_team", "away_team")

team_id_vars <- c("stadium", "home_coach", "away_coach")

### 2.C.2. Responses ----
response_vars <- c("home_score", "away_score", "result", "total")

### 2.C.3. Betting ----
betting_vars <- c("spread_line", "spreadCover", 
                  "home_spread_odds", "home_spread_prob",
                  "away_spread_odds", "away_spread_prob",
                  "total_line", "totalCover",
                  "over_odds", "over_prob",
                  "under_odds", "under_prob",
                  "winner", 
                  "home_moneyline", "home_moneyline_prob",
                  "away_moneyline", "away_moneyline_prob")

### 2.C.4. Game-Level -----
# These will be included with XGB predictions to imporve fit
game_level_vars <- c(
  "weekday", "time_of_day", "location", "location2", "div_game", 
  "home_rest", "away_rest",
  "roof", "surface", "temp", "wind"
)

# 3. Split Data for Modeling ----

## 3.A. Per Season ----
# Use a rolling window: for each test season, use the previous 3 seasons as training.
rolling_window <- 3
first_test_season <- xgb_season_start + rolling_window

train_indices <- list()
test_indices  <- list()
for (s in first_test_season:xgb_season_end) {
  train_seasons <- (s - rolling_window):(s - 1)
  train_idx <- which(model_data$season %in% train_seasons)
  test_idx  <- which(model_data$season == s)
  if (length(train_idx) > 0 && length(test_idx) > 0) {
    train_indices[[as.character(s)]] <- train_idx
    test_indices[[as.character(s)]]  <- test_idx
    cat(sprintf("Season %d: Training on seasons %s (%d games); Testing on %d games\n", 
                s, paste(train_seasons, collapse = ", "), length(train_idx), length(test_idx)))
  }
}
time_slices <- list(train = train_indices, test = test_indices)
cat("Total Seasonal CV folds:", length(time_slices$train), "\n")

# 4. FIT MODELS ----
# This function fits a Bayesian model on one fold and returns the fitted model,
# the posterior predictive draws, and the test set.
fit_seasonal_model <- function(
    train_data, test_data, response = "result",
    use_team_RE = TRUE, use_season_FE = FALSE,
    # Specify which numeric predictors to center and scale:
    preprocess_vars = c("xgb_result", "home_rest", "away_rest", "temp", "wind"),
    iters = 4000, burn = 2000, chains = 4,
    rstanBackend = FALSE) {
  
  # Preprocess numeric predictors in the training data and apply the transformation to test data.
  # (Factors such as weekday, time_of_day, etc., are left unchanged.)
  if (!is.null(preprocess_vars)) {
    preProc <- preProcess(train_data[, preprocess_vars], method = c("center", "scale"))
    train_data[, preprocess_vars] <- predict(preProc, train_data[, preprocess_vars])
    test_data[, preprocess_vars]  <- predict(preProc, test_data[, preprocess_vars])
  }
  
  # Choose the XGB predictor variable based on the response.
  # For "result", we use the merged XGB output "xgb_result".
  # For "total", we use "xgb_total". (Extend as needed for other responses.)
  xgb_pred_var <- switch(response,
                         "result" = "xgb_result",
                         "total"  = "xgb_total",
                         stop("Unsupported response"))
  
  # Build the formula string.
  fixed_part <- paste(
    response, "~", xgb_pred_var, " + ",
    paste(c(
      "home_rest",
      "away_rest",
      #"weekday",
      #"time_of_day",
      #"location2",
      "div_game",
      "roof",
      "temp",
      "wind"
    ),
    collapse = " + ")
  )
  re_team <- if (use_team_RE) " + (1 | home_team) + (1 | away_team)" else ""
  fe_season <- if (use_season_FE) " + season" else ""
  formula_str <- paste0(fixed_part, fe_season, re_team)
  formula_obj <- bf(as.formula(formula_str)) + brmsfamily(family = "student", link = "identity")
  
  # Define default priors.
  priors <- c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sigma"),
    prior(student_t(3, 0, 5), class = "sd")
  )
  
  # Fit the Bayesian model with brms.
  fit <- brm(
    formula = formula_obj,
    data = train_data,
    prior = priors,
    save_pars = save_pars(all = TRUE),
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    normalize = TRUE,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = ifelse(rstanBackend, "rstan", "cmdstanr"),
    seed = 52
  )
  
  # Generate the full posterior predictive draws (PPD) for the test set.
  ppd <- posterior_predict(fit, 
                           newdata = test_data, 
                           re_formula = NULL,
                           allow_new_levels = TRUE)
  
  # Return a list with the fitted model, PPD, test data, and formula.
  return(list(model = fit, ppd = ppd, test_data = test_data, formula = formula_obj))
}

# 5. Loop Over Seasonal Folds to Fit and Store Models ------------------------
fold_results <- list()

system.time(
  for (fold in names(time_slices$train)) {
    cat("\n---------- Processing Season Fold:", fold, "----------\n")
    train_data <- model_data[time_slices$train[[fold]], ]
    test_data  <- model_data[time_slices$test[[fold]], ]
    
    # Fit the model for the "result" response; adjust arguments as needed.
    fold_fit <- fit_seasonal_model(
      train_data, test_data, response = "result",
      use_team_RE = TRUE, use_season_FE = FALSE,
      preprocess_vars = c("xgb_result", "home_rest", "away_rest", "temp", "wind")
    )
    
    # For example, compute the posterior mean predictions and RMSE:
    ppd_mean <- colMeans(fold_fit$ppd)
    rmse_val <- sqrt(mean((test_data$result - ppd_mean)^2, na.rm = TRUE))
    cat(sprintf("Fold (Season %s) RMSE = %.3f\n", fold, rmse_val))
    
    fold_results[[fold]] <- fold_fit
  }
)
# Now fold_results is a list containing the fitted models and PPD for each seasonal fold.
# You can later use lapply, purrr::map, or similar functions to run additional diagnostics,
# generate PPC plots, or compute betting metrics on each fold.

# 6. Post-Processing  ------------------------
# Performance and Betting Evaluation
# Combine predictions from all folds.
# For each fold, we compute the posterior mean prediction.
all_fold_preds <- do.call(rbind, lapply(fold_results, function(fold) {
  data.frame(
    game_id  = fold$test_data$game_id,
    season   = fold$test_data$season,
    observed = fold$test_data$result,
    predicted = colMeans(fold$ppd)
  )
}))

# Compute overall RMSE, MAE, and MAD from the combined predictions.
overall_rmse <- sqrt(mean((all_fold_preds$observed - all_fold_preds$predicted)^2, na.rm = TRUE))
overall_mae  <- mean(abs(all_fold_preds$observed - all_fold_preds$predicted), na.rm = TRUE)
overall_mad  <- median(abs(all_fold_preds$observed - all_fold_preds$predicted), na.rm = TRUE)

cat("Overall RMSE:", round(overall_rmse, 3), "\n")
cat("Overall MAE:", round(overall_mae, 3), "\n")
cat("Overall MAD:", round(overall_mad, 3), "\n")

# 7. Compute Coverage -------
# Percentage of observed values falling within the 95% PPD interval.
compute_coverage <- function(ppd, observed, lower = 0.025, upper = 0.975) {
  # For each test observation, compute the 2.5% and 97.5% quantiles of the posterior draws.
  lower_bound <- apply(ppd, 2, quantile, probs = lower)
  upper_bound <- apply(ppd, 2, quantile, probs = upper)
  mean(observed >= lower_bound & observed <= upper_bound, na.rm = TRUE)
}

fold_coverage <- sapply(fold_results, function(fold) {
  compute_coverage(fold$ppd, fold$test_data$result)
})
cat("Coverage across folds (proportion within 95% intervals):\n")
print(round(fold_coverage, 3))

# 8. Betting Evaluation ----
# Compare Model's Cover vs. Vegas Lines.
# Assume your test data contains a 'spread_line' column and that
# the model's prediction for 'result' can be compared to it.
betting_results <- lapply(fold_results, function(fold) {
  # Compute the posterior mean prediction for each game.
  fold$test_data$result_pred <- colMeans(fold$ppd)
  
  # Create a betting decision:
  # For example, if the predicted result is greater than the spread line, call it a "Home" cover.
  fold <- fold$test_data %>%
    mutate(
      actual_cover = case_when(
        result > spread_line ~ "Home",
        result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      exp_cover = ifelse(result_pred > spread_line, "Home", 
                         ifelse(result_pred < spread_line, "Away", NA)),
      correct_cover = (actual_cover == exp_cover)
    )
  
  acc <- mean(fold$correct_cover, na.rm = TRUE) * 100
  list(data = fold, accuracy = acc)
})

# Extract betting accuracy from each fold.
betting_accuracy <- sapply(betting_results, function(x) x$accuracy)
cat("Betting Accuracy (Posterior Mean vs. Spread) per fold (%):\n")
print(round(betting_accuracy, 2))

# Overall betting accuracy (averaged across folds):
overall_betting_acc <- mean(betting_accuracy, na.rm = TRUE)
cat("Overall Betting Accuracy (%):", round(overall_betting_acc, 2), "\n")

# You can now use lapply, purrr::map, or similar functions on 'fold_results'
# to generate further diagnostics (e.g., PPC plots, residual plots, or LOO comparisons)
# for each seasonal fold. These outputs can then be shared to assess whether your
# Bayesian model, with its uncertainty estimates, beats Vegas.

# 9. Generate PPC plots ----
# density overlays for each fold)
ppc_plots <- lapply(fold_results, function(fold) {
  pp_check(fold$model, newdata = fold$test_data, resp = "result", ndraws = 100, type = "dens_overlay")
})

ppc_plots
# Then, you can display these plots in your Shiny app or save them as files.


# Example function to evaluate betting metrics on one fold
betting_eval_fold <- function(fold_result,
                              # These XGB model objects must be loaded in your session:
                              home_model, away_model, xgb_result_model,
                              threshold = 0.6) {
  
  # Copy the test data from the fold and add the model's predictions
  test_df <- fold_result$test_data
  # Use the posterior mean (across draws) as your point prediction
  test_df$result_pred <- colMeans(fold_result$ppd)
  
  # Compute the actual cover: if the actual result exceeds the spread, call it "Home", else "Away"
  test_df <- test_df %>%
    mutate(
      diff = result - spread_line,
      actual_cover = case_when(
        result > spread_line ~ "Home",
        result < spread_line ~ "Away",
        TRUE ~ NA_character_
      )
    )
  
  # Compute model-derived predictions (using the posterior mean)
  test_df <- test_df %>%
    mutate(
      exp_result = result_pred,
      exp_spread_line = spread_line,
      exp_diff = exp_result - exp_spread_line,
      exp_cover = case_when(
        exp_result > spread_line ~ "Home",
        exp_result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
      correct_cover = (actual_cover == exp_cover)
    )
  
  # Compute accuracy based on the posterior mean
  acc_posterior_mean <- mean(test_df$correct_cover, na.rm = TRUE) * 100
  
  # Full Posterior Betting Decision:
  # For each test observation, use the full set of posterior draws to decide cover.
  # Here we use sweep() to subtract the spread_line from every draw.
  ppd_decision <- sweep(fold_result$ppd, 2, test_df$spread_line, 
                        FUN = function(pred, line) {
                          ifelse(pred > line, "Home", ifelse(pred < line, "Away", NA))
                        })
  # Compare each posterior draw decision with the actual cover
  comparison_matrix <- sweep(ppd_decision, 2, test_df$actual_cover,
                             FUN = function(pred, actual) {
                               ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
                             })
  # For each game, compute the proportion of draws that are correct.
  game_accuracy <- colMeans(comparison_matrix, na.rm = TRUE)
  acc_full <- mean(game_accuracy, na.rm = TRUE) * 100
  
  # Vegas-based decisions: use the provided Vegas probabilities.
  test_home_prob <- colMeans(ppd_decision == "Home", na.rm = TRUE)
  test_away_prob <- colMeans(ppd_decision == "Away", na.rm = TRUE)
  # Decision: if the model's home probability exceeds the Vegas home spread probability, decide "Home", else "Away"
  test_bet_side_vegas <- ifelse(test_home_prob > test_df$home_spread_prob, "Home",
                                ifelse(test_away_prob > test_df$away_spread_prob, "Away", NA))
  test_bet_vegas_correct <- ifelse(is.na(test_bet_side_vegas) | is.na(test_df$actual_cover),
                                   NA, test_bet_side_vegas == test_df$actual_cover)
  acc_vegas <- mean(test_bet_vegas_correct, na.rm = TRUE) * 100
  bet_vegas_count <- sum(!is.na(test_bet_vegas_correct))
  
  # Threshold-based decisions: decide a bet only if the model's predicted probability exceeds a threshold.
  test_bet_side_thresh <- ifelse(test_home_prob > threshold, "Home",
                                 ifelse(test_away_prob > threshold, "Away", NA))
  test_bet_thresh_correct <- ifelse(is.na(test_bet_side_thresh) | is.na(test_df$actual_cover),
                                    NA, test_bet_side_thresh == test_df$actual_cover)
  acc_thresh <- mean(test_bet_thresh_correct, na.rm = TRUE) * 100
  bet_thresh_count <- sum(!is.na(test_bet_thresh_correct))
  
  # Also incorporate XGBoost predictions if available.
  test_df <- test_df %>%
    mutate(
      xgb_home_score = predict(home_model, newdata = test_df),
      xgb_away_score = predict(away_model, newdata = test_df),
      xgb_result = xgb_home_score - xgb_away_score,
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
      xgb_result2 = predict(xgb_result_model, newdata = test_df),
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
  
  # Return a list with the betting metrics and enriched test dataframe.
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

# Example of applying the evaluation function to each seasonal fold:
# (Ensure that your XGB models -- home_model, away_model, and xgb_result_model -- are loaded.)
betting_evals <- lapply(fold_results, function(fold) {
  betting_eval_fold(fold, 
                    xgb_home_model_final, 
                    xgb_away_model_final,
                    xgb_result_model_final, 
                    threshold = 0.6)
})

# Now, you can extract and compare the metrics across folds:
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

# You can also construct a comparison table if desired:
accuracy_metrics_result_temp <- tibble(
  Fold = names(betting_evals),
  PostMean = posterior_mean_accs,
  PostFull = full_ppd_accs,
  BetVegas = vegas_accs,
  BetThresh = thresh_accs,
  XGB = xgb_accs,
  XGB2 = xgb2_accs
)

print(accuracy_metrics_result_temp)




# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$----
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$----
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$----
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$----
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$----





## Split ----
histModelData1 <- modData5 |> 
  filter(between(season, 2020, 2023) | (season == 2024 & week <= 6))
modelData1 <- modData5 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result))

# Pre-Process Data ----
## Predictors ----
predictorData <- histModelData1 |> 
  select(
    # xgb_pred_home_score,
    # xgb_pred_away_score,
    xgb_pred_result,
    xgb_pred_result2,
    xgb_pred_total,
    xgb_pred_total2,
    #union(best_home_vars, best_away_vars),
    best_home_vars,
    best_away_vars,
    best_result_vars,
    best_total_vars,
    home_rest,
    away_rest,
    #weekday,
    #time_of_day
    location2,
    div_game,
    roof,
    temp,
    wind
  )

### Center, Scale ----
preProc_CS_corr <- preProcess(predictorData,
                              method = c("center", "scale", "corr")
)
preProc_CS <- preProcess(predictorData,
                         method = c("center", "scale")
)
preProcValuesYeo <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)
preProcValuesArc <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)


preProc_CS_corr
preProc_CS_corr$method$remove
predictorData2 <- predict(preProc_CS, predictorData)
histModelData2 <- predict(preProc_CS, histModelData1)
modelData2 <- predict(preProc_CS, modelData1)

histModelData <- histModelData2
modelData <- modelData2

predictorDataYeo <- predict(preProcValuesYeo, predictorData)
histModelDataYeo <- predict(preProcValuesYeo, histModelData1)
modelDataYeo <- predict(preProcValuesYeo, modelData1)

histModelData <- histModelDataYeo
modelData <- modelDataYeo



# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# RESULT ----------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

#as.formula(paste(c(best_vars_home[1:20], best_vars_away[1:20]), collapse = " + "))

home_result_diff_vars <- setdiff(best_home_vars, best_result_vars)
home_result_diff_vars
away_result_diff_vars <- setdiff(best_away_vars, best_result_vars)
away_result_diff_vars
home_away_result_new_vars <- union(home_result_diff_vars, away_result_diff_vars)
home_away_result_new_vars

varImp_result$importance |> slice(1:30)
home_away_result_new_vars

## Model ----
# #div_game + roof + temp + wind + (1 | home_team) + (1 | away_team)
formula_result <- bf(
  result ~
    xgb_pred_result2 +
    # home_off_pass_epa_roll +
    # home_off_net_epa_roll +
    # home_pass_net_epa_roll +
    # home_pass_net_epa_cum +
    # home_off_net_epa_cum +
    # home_off_pass_epa_cum +
    # home_off_epa_cum +
    # away_MOV_roll_net +
    # home_off_epa_roll +
    # home_OSRS_roll +
    # home_net_epa_cum +
    # away_MOV_net +
    # home_PFG_roll +
    # home_MOV +
    # home_offTD_roll_5 +
    # home_PFG +
    # home_def_rush_plays_cum +
    # home_SRS +
    # away_net_epa_roll +
    # away_SRS_roll_net +
    # away_off_net_epa_cum +
    # away_pass_net_epa_cum +
    # away_OSRS_roll_net +
    # away_off_epa_cum +
    # away_off_net_epa_roll +
    # away_off_pass_epa_cum +
    # away_MOV_roll_net +
    # away_PFG_roll +
    # away_OSRS_net +
    # away_SRS_roll_net +
    # away_off_epa_roll +
    # away_pass_net_epa_roll +
    # away_off_pass_epa_roll +
    # home_def_pass_epa_cum +
    # home_PAG +
    # away_SRS_net +
    # away_OSRS_roll +
    # away_MOV_roll +
    # away_net_epa_cum +
    # home_net_epa_cum +
    #home_rest +
    #away_rest +
    #weekday +
    #time_of_day +
    #location2 +
    #div_game +
    roof +
    temp +
    #wind +
    (1 | home_team) +
    (1 | away_team)
) + 
  brmsfamily(family = "student", link = "identity")
#mixture(brmsfamily(family = "gaussian", link = "identity"), nmix = 2)
#brmsfamily(family = "negbinomial", link = "log")

default_prior(formula_result, histModelData)

priors_result <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 10), class = "b"),
  #prior(normal(0, 5), class = "b", dpar = "mu1"),
  #prior(normal(0, 5), class = "b", dpar = "mu2"),
  prior(student_t(3, 0, 10), class = "sigma"),
  #prior(student_t(3, 0, 10), class = "sigma1"),
  #prior(student_t(3, 0, 10), class = "sigma2"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(student_t(3, 0, 5), class = "sd")
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu1"),
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu2")
)

get_prior(formula_result,
          data = histModelData,
          prior = priors_result)

system.time(
  fit_result <- brm(
    formula_result,
    data = histModelData,
    #prior = priors_result,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)


print(fit_result, digits = 4)
check_collinearity(fit_result)

#fit_Team <- fit_Team2
#pp_check(fit_result, resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, resp = "result", ndraws = 100, type = "dens_overlay")

# pp_check(fit_result, newdata = modelData, 
#          resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, newdata = modelData,
         resp = "result", ndraws = 100, type = "dens_overlay")

### Fixed Effects ----
fixedEff_result <- fixef(fit_result)
fixedEff_result <- data.frame(fixedEff_result) |>
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
print(fixedEff_result, digits = 4)
fixedSigEff_result <- fixedEff_result |> filter(p_val < 0.2)
print(fixedSigEff_result)

randEff_result <- ranef(fit_result)
randEff_result

### MAE ----
fitResiduals_result <- 
  residuals(
    fit_result,
    resp = "result",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_result$Estimate))

predResiduals_result <- 
  residuals(
    fit_result,
    resp = "result",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_result$Estimate))

fitResult <- 26
assign(paste0("fit_result", fitResult), fit_result)
assign(paste0("fixedEff_result", fitResult), fixedEff_result)
assign(paste0("randEff_result", fitResult), randEff_result)

save(fit_result, 
     file = paste0("~/Desktop/NFL Analysis Data/fit_result",
                   fitResult,
                   ".RData")
)

save(fit_result, 
     file = paste0("~/Desktop/NFLAnalysisTest/app/data/fit_result.rda")
)

# Posteriors ----
## Training ----
train_result <- histModelData$result

set.seed(52)
posteriorFit_result <- posterior_predict(
  fit_result,
  resp = "result"
)
posteriorFitMean_result <- colMeans(posteriorFit_result)
posteriorFitMed_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.975)})

## Test ----
test_result <- modelData$result

set.seed(52)
posteriorPred_result <- posterior_predict(
  fit_result,
  resp = "result",
  newdata = modelData,
  re_formula = NULL
)
posteriorPredMean_result <- colMeans(posteriorPred_result)
posteriorPredMed_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.975)})

## LOO ----
loo_result_4 <- loo(fit_result4)
loo_result_5 <- loo(fit_result5)
loo_result_6 <- loo(fit_result6)
loo_result_7 <- loo(fit_result7)
loo_result_8 <- loo(fit_result8)
loo_result_9 <- loo(fit_result9)
loo_result_10 <- loo(fit_result10)
loo_result_11 <- loo(fit_result11)
loo_result_12 <- loo(fit_result12)
loo_result_20 <- loo(fit_result20)
loo_result_21 <- loo(fit_result21)
loo_result_22 <- loo(fit_result22)
loo_result_23 <- loo(fit_result23)
loo_result_24 <- loo(fit_result24)
loo_result_25 <- loo(fit_result25)
loo_result_26 <- loo(fit_result26)
loo_result_list <- list(
  loo_result_4,
  loo_result_5,
  loo_result_6,
  loo_result_7,
  loo_result_8,
  loo_result_9,
  loo_result_10,
  loo_result_11,
  loo_result_12,
  loo_result_20,
  loo_result_21,
  loo_result_22,
  loo_result_23,
  loo_result_24,
  loo_result_25,
  loo_result_26
)

loo_compare_result <- loo_compare(loo_result_list)
loo_compare_result

# Goodness of Fit ##########################################################
## PPC ----
set.seed(52)
sampFitID <- sample(1:sims, 200, replace = FALSE)
posteriorFitSamp_result <- posteriorFit_result[sampFitID, ]

fillPPC <- "#d1e1ec"
colorPPC <- "#b3cde0"
fill2PPC <- "#011f4b"

### Bars ----
# ppcBarsPlot_result <- ppc_bars(
#   y = train_result,
#   yrep = posteriorFitSamp_result
# ) +
#   labs(
#     # title = "Simulated density of draws from the PPD vs Observed VMAX",
#     # subtitle = "n = 1000 draws",
#     x = "result",
#     y = "Density"
#   ) +
#   #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
#   theme_bw() +
#   theme(
#     legend.position = "none"
#   )

### Density -----
ppcDensPlot_result <- ppc_dens_overlay(
  y = train_result,
  yrep = posteriorFitSamp_result
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "result",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

#### Stats ----
# Make stat functions
meanFunc <- function(y){mean(y)}
sdFunc <- function(y){sd(y)}
rangeFunc <- function(y){max(y) - min(y)}

##### Mean ----
set.seed(52) # for reproducibility
mean_result <- meanFunc(train_result)

ppcMeanStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_result,
    meanProbHigh = value > mean_result
  )

ppcMeanPlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Mean",
       subtitle = paste("p-value =", round(mean(ppcMeanStat_result$meanProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### SD ----
set.seed(52) # for reproducibility
sd_result <- sdFunc(train_result)

ppcSDStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_result,
    sdProbHigh = value > sd_result
  )

ppcSDPlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcSDStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "SD",
       subtitle = paste("p-value =", round(mean(ppcSDStat_result$sdProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Range ----
set.seed(52) # for reproducibility
range_result <- rangeFunc(train_result)

ppcRangeStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_result,
    rangeProbHigh = value > range_result
  )

ppcRangePlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Range",
       subtitle = paste("p-value =", round(mean(ppcRangeStat_result$rangeProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

#### Combined plot ----
ppcCombPlot_result <- 
  #(ppcBarsPlot_result + ppcDensPlot_result) /
  (ppcDensPlot_result) /
  (ppcMeanPlotGG_result + ppcSDPlotGG_result + ppcRangePlotGG_result) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcCombPlot_result

# Performance Metrics ----
## Training ----
### MAE
MAE_fit_result <- mean(abs(posteriorFitMean_result - train_result))

### MAD
MAD_fit_result <- mean(abs(posteriorFitMed_result - train_result))

### RMSE
RMSE_fit_result <- sqrt(mean(posteriorFitMean_result - train_result)^2)

### COV
COV_fit_result <- mean(posteriorFitLCB_result < train_result &  
                         train_result < posteriorFitUCB_result)

## Test ----
### MAE
MAE_pred_result <- mean(abs(posteriorPredMean_result - test_result))

### MAD
MAD_pred_result <- mean(abs(posteriorPredMed_result - test_result))

### RMSE
RMSE_pred_result <- sqrt(mean(posteriorPredMean_result - test_result)^2)

### COV
COV_pred_result <- mean(posteriorPredLCB_result < test_result &  
                          test_result < posteriorPredUCB_result)

## Compare Table ----
performance_metrics_result <- tibble(
  Fit = rep(paste0("Fit", fitResult), 1),
  Bet = c("result"), # "total"),
  MAE_fit = c(MAE_fit_result), # MAE_fit_total),
  MAD_fit = c(MAD_fit_result), # MAD_fit_total),
  RMSE_fit = c(RMSE_fit_result), # RMSE_fit_total),
  COV_fit = c(COV_fit_result), # COV_fit_total),
  MAE_pred = c(MAE_pred_result), # MAE_pred_total),
  MAD_pred = c(MAD_pred_result), # MAD_pred_total),
  RMSE_pred = c(RMSE_pred_result), # RMSE_pred_total),
  COV_pred = c(COV_pred_result) # COV_pred_total)
)
performance_metrics_result

# Betting #######################################################
## Training ----
train_result_bet_df <- histModelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    result, spread_line,
    home_spread_prob, away_spread_prob,
    home_spread_odds, away_spread_odds
  ) |>
  mutate(
    diff = result - spread_line,
    actual_cover = case_when(
      result > spread_line ~ "Home",
      result < spread_line ~ "Away",
      .default = NA
    ),
    .after = spread_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_result = posteriorFitMean_result,
    exp_spread_line = spread_line,
    exp_diff = exp_result - exp_spread_line,
    exp_cover = case_when(
      exp_result > spread_line ~ "Home",
      exp_result < spread_line ~ "Away",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(xgb_home_model, newdata = histModelData),
    xgb_away_score = predict(xgb_away_model, newdata = histModelData),
    xgb_result = xgb_home_score - xgb_away_score,
    #xgb_result2 = predict(xgb_result_model, newdata = histModelData),
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_result2 = predict(xgb_result_model, newdata = histModelData),
    xgb_spread_line2 = spread_line,
    xgb_diff2 = xgb_result2 - xgb_spread_line2,
    xgb_cover2 = case_when(
      xgb_result2 > spread_line ~ "Home",
      xgb_result2 < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover2,
    .after = xgb_correct_cover
  )

### Accuracy ----
train_result_spread_line <- train_result_bet_df$spread_line
train_result_actual_cover <- train_result_bet_df$actual_cover
train_vegas_home_prob <- train_result_bet_df$home_spread_prob
train_vegas_away_prob <- train_result_bet_df$away_spread_prob
train_result_thresh <- 0.6

#### Posterior Means ----
table(train_result_bet_df$correct_cover, useNA = "ifany")

train_result_acc_prob_posterior_mean <-
  mean(train_result_bet_df$correct_cover, na.rm = TRUE)*100
train_result_acc_prob_posterior_mean

#### Full Posterior ----
train_result_bet_decision <- 
  sweep(
    posteriorFit_result, 2, train_result_spread_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Home",
             ifelse(pred < line, "Away", NA))
    })

train_result_comparison_matrix <- 
  sweep(
    train_result_bet_decision, 2, train_result_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

train_result_game_accuracy <- 
  colMeans(train_result_comparison_matrix, na.rm = TRUE) 

train_result_acc_prob_full_posterior <- 
  mean(train_result_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
train_result_home_prob <- 
  colMeans(train_result_bet_decision == "Home", na.rm = TRUE)
train_result_away_prob <- 
  colMeans(train_result_bet_decision == "Away", na.rm = TRUE)

# Decision vector
train_result_bet_side_vegas <- 
  ifelse(train_result_home_prob > train_vegas_home_prob, "Home",
         ifelse(train_result_away_prob > train_vegas_away_prob, "Away", NA))

# Compare to actual outcome
train_result_bet_vegas_correct <- 
  ifelse(is.na(train_result_bet_side_vegas) | is.na(train_result_actual_cover), 
         NA, train_result_bet_side_vegas == train_result_actual_cover)

# Accuracy over placed bets
train_result_bet_vegas_acc <- 
  mean(train_result_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
train_result_bet_vegas_count <- sum(!is.na(train_result_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
train_result_bet_side_thresh <- 
  ifelse(train_result_home_prob > train_result_thresh, "Home",
         ifelse(train_result_away_prob > train_result_thresh, "Away", NA))

# Compare to actual outcome
train_result_bet_thresh_correct <- 
  ifelse(is.na(train_result_bet_side_thresh) | is.na(train_result_actual_cover), 
         NA, train_result_bet_side_thresh == train_result_actual_cover)

# Accuracy over placed bets
train_result_bet_thresh_acc <- 
  mean(train_result_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
train_result_bet_thresh_count <- sum(!is.na(train_result_bet_thresh_correct))

#### XGB Prediction ----
table(train_result_bet_df$xgb_correct_cover, useNA = "ifany")

train_result_acc_prob_xgb <-
  mean(train_result_bet_df$xgb_correct_cover, na.rm = TRUE)*100
train_result_acc_prob_xgb

table(train_result_bet_df$xgb_correct_cover2, useNA = "ifany")

train_result_acc_prob_xgb2 <-
  mean(train_result_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
train_result_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(train_result_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(train_result_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(train_result_bet_vegas_acc, 2), "%",
            "on", train_result_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(train_result_bet_thresh_acc, 2), "%",
            "on", train_result_bet_thresh_count, "bets",
            "with", train_result_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_result_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_result_acc_prob_xgb2, 2), "%"))

## Test ----
test_result_bet_df <- modelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    result, spread_line,
    home_spread_prob, away_spread_prob,
    home_spread_odds, away_spread_odds
  ) |>
  mutate(
    diff = result - spread_line,
    actual_cover = case_when(
      result > spread_line ~ "Home",
      result < spread_line ~ "Away",
      .default = NA
    ),
    .after = spread_line
  ) |>
  mutate(
    #exp_home_score = posteriorPredMean_home,
    #exp_away_score = posteriorPredMean_away,
    exp_result = posteriorPredMean_result,
    exp_spread_line = spread_line,
    exp_diff = exp_result - exp_spread_line,
    exp_cover = case_when(
      exp_result > spread_line ~ "Home",
      exp_result < spread_line ~ "Away",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(home_model, newdata = modelData),
    xgb_away_score = predict(away_model, newdata = modelData),
    xgb_result = xgb_home_score - xgb_away_score,
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_result2 = predict(xgb_result_model, newdata = modelData),
    xgb_spread_line2 = spread_line,
    xgb_diff2 = xgb_result2 - xgb_spread_line2,
    xgb_cover2 = case_when(
      xgb_result2 > spread_line ~ "Home",
      xgb_result2 < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover2,
    .after = xgb_correct_cover
  )

### Accuracy ----
test_result_spread_line <- test_result_bet_df$spread_line
test_result_actual_cover <- test_result_bet_df$actual_cover
test_vegas_home_prob <- test_result_bet_df$home_spread_prob
test_vegas_away_prob <- test_result_bet_df$away_spread_prob
test_result_thresh <- 0.6

#### Posterior Means ----
table(test_result_bet_df$correct_cover, useNA = "ifany")

test_result_acc_prob_posterior_mean <-
  mean(test_result_bet_df$correct_cover, na.rm = TRUE)*100
test_result_acc_prob_posterior_mean

#### Full Posterior ----
test_result_bet_decision <- 
  sweep(
    posteriorPred_result, 2, test_result_spread_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Home",
             ifelse(pred < line, "Away", NA))
    })

test_result_comparison_matrix <- 
  sweep(
    test_result_bet_decision, 2, test_result_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

test_result_game_accuracy <- 
  colMeans(test_result_comparison_matrix, na.rm = TRUE) 

test_result_acc_prob_full_posterior <- 
  mean(test_result_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
test_result_home_prob <- 
  colMeans(test_result_bet_decision == "Home", na.rm = TRUE)
test_result_away_prob <- 
  colMeans(test_result_bet_decision == "Away", na.rm = TRUE)

# Decision vector
test_result_bet_side_vegas <- 
  ifelse(test_result_home_prob > test_vegas_home_prob, "Home",
         ifelse(test_result_away_prob > test_vegas_away_prob, "Away", NA))

# Compare to actual outcome
test_result_bet_vegas_correct <- 
  ifelse(is.na(test_result_bet_side_vegas) | is.na(test_result_actual_cover), 
         NA, test_result_bet_side_vegas == test_result_actual_cover)

# Accuracy over placed bets
test_result_bet_vegas_acc <- 
  mean(test_result_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
test_result_bet_vegas_count <- sum(!is.na(test_result_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
test_result_bet_side_thresh <- 
  ifelse(test_result_home_prob > test_result_thresh, "Home",
         ifelse(test_result_away_prob > test_result_thresh, "Away", NA))

# Compare to actual outcome
test_result_bet_thresh_correct <- 
  ifelse(is.na(test_result_bet_side_thresh) | is.na(test_result_actual_cover), 
         NA, test_result_bet_side_thresh == test_result_actual_cover)

# Accuracy over placed bets
test_result_bet_thresh_acc <- 
  mean(test_result_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
test_result_bet_thresh_count <- sum(!is.na(test_result_bet_thresh_correct))

#### XGB Prediction ----
table(test_result_bet_df$xgb_correct_cover, useNA = "ifany")

test_result_acc_prob_xgb <-
  mean(test_result_bet_df$xgb_correct_cover, na.rm = TRUE)*100
test_result_acc_prob_xgb

table(train_result_bet_df$xgb_correct_cover2, useNA = "ifany")

test_result_acc_prob_xgb2 <-
  mean(test_result_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
test_result_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(test_result_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(test_result_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(test_result_bet_vegas_acc, 2), "%",
            "on", test_result_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(test_result_bet_thresh_acc, 2), "%",
            "on", test_result_bet_thresh_count, "bets",
            "with", test_result_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_result_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_result_acc_prob_xgb2, 2), "%"))

## Comparison Table ----
accuracy_metrics_result_temp <- tibble(
  Fit = rep(paste0("Fit", fitResult), 2),
  Data = rep(c("Train", "Test"), each = 1),
  Bet = rep(c("result"), 2), # "total"), times = 1),
  PostMean = c(train_result_acc_prob_posterior_mean,
               #train_total_acc_prob_posterior_mean,
               test_result_acc_prob_posterior_mean
               #test_total_acc_prob_posterior_mean
  ),
  PostFull = c(train_result_acc_prob_full_posterior,
               #train_total_acc_prob_full_posterior,
               test_result_acc_prob_full_posterior
               #test_total_acc_prob_full_posterior
  ),
  BetVegas = c(train_result_bet_vegas_acc,
               #train_total_bet_vegas_acc,
               test_result_bet_vegas_acc
               #test_total_bet_vegas_acc
  ),
  BetVegasCount = c(train_result_bet_vegas_count,
                    #train_total_bet_vegas_count,
                    test_result_bet_vegas_count
                    #test_total_bet_vegas_count
  ),
  BetThresh = c(train_result_bet_thresh_acc,
                #train_total_bet_thresh_acc,
                test_result_bet_thresh_acc
                #test_total_bet_thresh_acc
  ),
  ThreshPerc = c(train_result_thresh,
                 #train_total_thresh,
                 test_result_thresh
                 #test_total_thresh
  ),
  BetThreshCount = c(train_result_bet_thresh_count,
                     #train_total_bet_thresh_count,
                     test_result_bet_thresh_count
                     #test_total_bet_thresh_count
  ),
  XGB = c(train_result_acc_prob_xgb,
          #train_total_acc_prob_xgb,
          test_result_acc_prob_xgb
          #test_total_acc_prob_xgb
  )
)
accuracy_metrics_result <- bind_rows(
  accuracy_metrics_result_temp,
  accuracy_metrics_result
)

accuracy_metrics_result #<- accuracy_metrics_result_temp

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# TOTAL ----------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

## Model ----
formula_total <- bf(
  total|trunc(lb = 3) ~
    xgb_pred_total2 +
    # home_off_pass_epa_roll + 
    # home_off_net_epa_roll +
    # home_pass_net_epa_roll +
    # home_pass_net_epa_cum + 
    # home_off_net_epa_cum +
    # home_off_pass_epa_cum + 
    # home_off_epa_cum + 
    # away_MOV_roll_net +
    # home_off_epa_roll + 
    # home_OSRS_roll +
    # home_net_epa_cum + 
    # away_MOV_net + 
    # home_PFG_roll + 
    # home_MOV + 
    # home_offTD_roll_5 +
    # home_PFG +
    # home_def_rush_plays_cum + 
    # home_SRS +
    # away_net_epa_roll + 
    # away_SRS_roll_net + 
    # away_off_net_epa_cum + 
    # away_pass_net_epa_cum +
    # away_OSRS_roll_net + 
    # away_off_epa_cum +
    # away_off_net_epa_roll + 
    # away_off_pass_epa_cum +
    # away_MOV_roll_net +
    # away_PFG_roll +
    # away_OSRS_net + 
    # away_SRS_roll_net + 
    # away_off_epa_roll + 
    # away_pass_net_epa_roll + 
    # away_off_pass_epa_roll + 
    # home_def_pass_epa_cum + 
    # home_PAG + 
    # away_SRS_net +
    # away_OSRS_roll +
    # away_MOV_roll + 
    # away_net_epa_cum +
    # home_net_epa_cum +
    div_game + 
    roof + 
    #surface + 
    temp + 
    wind + 
    (1 | home_team) +
    (1 | away_team)
) + 
  brmsfamily(family = "student", link = "identity")
#brmsfamily(family = "shifted_lognormal", link = "identity")
#brmsfamily(family = "skew_normal", link = "identity")
#mixture(brmsfamily(family = "poisson", link = "log"), nmix = 3)
#brmsfamily(family = "negbinomial", link = "log")

default_prior(formula_total, histModelData)

priors_total <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b"),
  prior(inv_gamma(0.1, 0.1), class = "sigma"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(inv_gamma(0.1, 0.1), class = "sd")
)

system.time(
  fit_total <- brm(
    formula_total,
    data = histModelData,
    #prior = priors_Team,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)

print(fit_total, digits = 4)

#fit_Team <- fit_Team2
#pp_check(fit_total, resp = "total", ndraws = 100, type = "bars")
pp_check(fit_total, resp = "total", ndraws = 100, type = "dens_overlay")

# pp_check(fit_total, newdata = modelData,
#          resp = "total", ndraws = 100, type = "bars")
pp_check(fit_total, newdata = modelData,
         resp = "total", ndraws = 100, type = "dens_overlay")

#save(fit_total, file = "~/Desktop/NFL Analysis Data/fit_total.RData")

### Fixed Effects ----
fixedEff_total<- fixef(fit_total)
fixedEff_total <- data.frame(fixedEff_total) |>
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
print(fixedEff_total, digits = 4)
fixedSigEff_total <- fixedEff_total |> filter(p_val < 0.2)
print(fixedSigEff_total)

randEff_total <- ranef(fit_total)
randEff_total

### MAE ----
fitResiduals_total <- 
  residuals(
    fit_total,
    resp = "total",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_total$Estimate))

predResiduals_total <- 
  residuals(
    fit_total,
    resp = "total",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_total$Estimate))

fitTotal <- 5
assign(paste0("fit_total", fitTotal), fit_total)
assign(paste0("fixedEff_total", fitTotal), fixedEff_total)
assign(paste0("randEff_total", fitTotal), randEff_total)

save(fit_total, 
     file = paste0("~/Desktop/NFL Analysis Data/fit_total",
                   fitTotal,
                   ".RData")
)

# Posteriors ----
## Training ----
train_total <- histModelData$total

posteriorFit_total <- posterior_predict(
  fit_total,
  resp = "total"
)
posteriorFitMean_total <- colMeans(posteriorFit_total)
posteriorFitMed_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.975)})

## Test ----
test_total <- modelData$total

set.seed(52)
posteriorPred_total <- posterior_predict(
  fit_total,
  resp = "total",
  newdata = modelData,
  re_formula = NULL
)
posteriorPredMean_total <- colMeans(posteriorPred_total)
posteriorPredMed_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.975)})

## LOO ----
loo_total_1 <- loo(fit_total1)
loo_total_2 <- loo(fit_total2)
loo_total_3 <- loo(fit_total3)
loo_total_4 <- loo(fit_total4)
loo_total_5 <- loo(fit_total5)
loo_total_6 <- loo(fit_total6)
loo_total_7 <- loo(fit_total7)
loo_total_8 <- loo(fit_total8)
loo_total_9 <- loo(fit_total9)
loo_total_10 <- loo(fit_total10)
loo_total_11 <- loo(fit_total11)
loo_total_12 <- loo(fit_total12)
loo_total_20 <- loo(fit_total20)
loo_total_21 <- loo(fit_total21)
loo_total_22 <- loo(fit_total22)
loo_total_23 <- loo(fit_total23)
loo_total_24 <- loo(fit_total24)
loo_total_25 <- loo(fit_total25)
loo_total_26 <- loo(fit_total26)
loo_total_list <- list(
  loo_total_1,
  loo_total_2,
  loo_total_3,
  loo_total_4,
  loo_total_5
  # loo_total_6,
  # loo_total_7,
  # loo_total_8,
  # loo_total_9,
  # loo_total_10,
  # loo_total_11,
  # loo_total_12,
  # loo_total_20,
  # loo_total_21,
  # loo_total_22,
  # loo_total_23,
  # loo_total_24,
  # loo_total_25,
  # loo_total_26
)

loo_compare_total <- loo_compare(loo_total_list)
loo_compare_total

# Goodness of Fit ##########################################################
## PPC ----
set.seed(52)
sampFitID <- sample(1:sims, 200, replace = FALSE)
posteriorFitSamp_total <- posteriorFit_total[sampFitID, ]

fillPPC <- "#d1e1ec"
colorPPC <- "#b3cde0"
fill2PPC <- "#011f4b"

### Bars ----
# ppcBarsPlot_total <- ppc_bars(
#   y = train_total,
#   yrep = posteriorFitSamp_total
# ) +
#   labs(
#     # title = "Simulated density of draws from the PPD vs Observed VMAX",
#     # subtitle = "n = 1000 draws",
#     x = "total",
#     y = "Density"
#   ) +
#   #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
#   theme_bw() +
#   theme(
#     legend.position = "none"
#   )

### Density -----
ppcDensPlot_total <- ppc_dens_overlay(
  y = train_total,
  yrep = posteriorFitSamp_total
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "total",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


#### Stats ----
# Make stat functions
meanFunc <- function(y){mean(y)}
sdFunc <- function(y){sd(y)}
rangeFunc <- function(y){max(y) - min(y)}
minFunc <- function(y){min(y)}
maxFunc <- function(y){max(y)}

##### Mean ----
set.seed(52) # for reproducibility
mean_total <- meanFunc(train_total)

ppcMeanStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_total,
    meanProbHigh = value > mean_total
  )

ppcMeanPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Mean",
       subtitle = paste("p-value =", round(mean(ppcMeanStat_total$meanProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### SD ----
set.seed(52) # for reproducibility\
sd_total <- sdFunc(train_total)

ppcSDStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_total,
    sdProbHigh = value > sd_total
  )

ppcSDPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcSDStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "SD",
       subtitle = paste("p-value =", round(mean(ppcSDStat_total$sdProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Range ----
set.seed(52) # for reproducibility
range_total <- rangeFunc(train_total)

ppcRangeStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_total,
    rangeProbHigh = value > range_total
  )

ppcRangePlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Range",
       subtitle = paste("p-value =", round(mean(ppcRangeStat_total$rangeProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Min ----
set.seed(52) # for reproducibility
min_total <- minFunc(train_total)

ppcMinStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("minFunc")
) |>
  mutate(
    minProbLow = value < min_total,
    minProbHigh = value > min_total
  )

ppcMinPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMinStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMinStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Min",
       subtitle = paste("p-value =", round(mean(ppcMinStat_total$minProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Max ----
set.seed(52) # for reproducibility
max_total <- maxFunc(train_total)

ppcMaxStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("maxFunc")
) |>
  mutate(
    maxProbLow = value < max_total,
    maxProbHigh = value > max_total
  )

ppcMaxPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMaxStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMaxStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Max",
       subtitle = paste("p-value =", round(mean(ppcMaxStat_total$maxProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

#### Combined plot ----
ppcCombPlot_total <- 
  #(ppcBarsPlot_total + ppcDensPlot_total) /
  (ppcDensPlot_total) /
  (ppcMeanPlotGG_total + ppcSDPlotGG_total + ppcRangePlotGG_total) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcCombPlot_total

ppcMinMaxPlot_total <- 
  (ppcMinPlotGG_total + ppcMaxPlotGG_total) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcMinMaxPlot_total

# Performance Metrics ----
## Training ----
### MAE
MAE_fit_total <- mean(abs(posteriorFitMean_total - train_total))

### MAD
MAD_fit_total <- mean(abs(posteriorFitMed_total - train_total))

### RMSE
RMSE_fit_total <- sqrt(mean(posteriorFitMean_total - train_total)^2)

### COV
COV_fit_total <- mean(posteriorFitLCB_total < train_total &  
                        train_total < posteriorFitUCB_total)

## Test ----
### MAE
MAE_pred_total <- mean(abs(posteriorPredMean_total - test_total))

### MAD
MAD_pred_total <- mean(abs(posteriorPredMed_total - test_total))

### RMSE
RMSE_pred_total <- sqrt(mean(posteriorPredMean_total - test_total)^2)

### COV
COV_pred_total <- mean(posteriorPredLCB_total < test_total &  
                         test_total < posteriorPredUCB_total)

## Compare Table ----
performance_metrics_total <- tibble(
  Fit = rep(paste0("Fit", fitTotal), 1),
  Bet = c("total"),
  MAE_fit = c(MAE_fit_total),
  MAD_fit = c(MAD_fit_total),
  RMSE_fit = c(RMSE_fit_total),
  COV_fit = c(COV_fit_total),
  MAE_pred = c(MAE_pred_total),
  MAD_pred = c(MAD_pred_total),
  RMSE_pred = c(RMSE_pred_total),
  COV_pred = c(COV_pred_total)
)
performance_metrics_total

# Betting #######################################################
## Training ----
train_total_bet_df <- histModelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    total, total_line,
    over_prob, under_prob,
    over_odds, under_odds
  ) |>
  mutate(
    diff = total - total_line,
    actual_cover = case_when(
      total > total_line ~ "Over",
      total < total_line ~ "Under",
      .default = NA
    ),
    .after = total_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_total = posteriorFitMean_total,
    exp_total_line = total_line,
    exp_diff = exp_total - exp_total_line,
    exp_cover = case_when(
      exp_total > total_line ~ "Over",
      exp_total < total_line ~ "Under",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(home_model, newdata = histModelData),
    xgb_away_score = predict(away_model, newdata = histModelData),
    xgb_total = xgb_home_score + xgb_away_score,
    xgb_total_line = total_line,
    xgb_diff = xgb_total - xgb_total_line,
    xgb_cover = case_when(
      xgb_total > total_line ~ "Over",
      xgb_total < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_total2 = predict(xgb_total_model, newdata = histModelData),
    xgb_total_line2 = total_line,
    xgb_diff2 = xgb_total2 - xgb_total_line2,
    xgb_cover2 = case_when(
      xgb_total2 > total_line ~ "Over",
      xgb_total2 < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover2,
    .after = xgb_correct_cover
  )

### Accuracy ----
train_total_total_line <- train_total_bet_df$total_line
train_total_actual_cover <- train_total_bet_df$actual_cover
train_vegas_over_prob <- train_total_bet_df$over_prob
train_vegas_under_prob <- train_total_bet_df$under_prob
train_total_thresh <- 0.6

#### Posterior Means ----
table(train_total_bet_df$correct_cover, useNA = "ifany")

train_total_acc_prob_posterior_mean <-
  mean(train_total_bet_df$correct_cover, na.rm = TRUE)*100
train_total_acc_prob_posterior_mean

#### Full Posterior ----
train_total_bet_decision <- 
  sweep(
    posteriorFit_total, 2, train_total_total_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Over",
             ifelse(pred < line, "Under", NA))
    })

train_total_comparison_matrix <- 
  sweep(
    train_total_bet_decision, 2, train_total_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

train_total_game_accuracy <- 
  colMeans(train_total_comparison_matrix, na.rm = TRUE) 

train_total_acc_prob_full_posterior <- 
  mean(train_total_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
train_total_over_prob <- 
  colMeans(train_total_bet_decision == "Over", na.rm = TRUE)
train_total_under_prob <- 
  colMeans(train_total_bet_decision == "Under", na.rm = TRUE)

# Decision vector
train_total_bet_side_vegas <- 
  ifelse(train_total_over_prob > train_vegas_over_prob, "Over",
         ifelse(train_total_under_prob > train_vegas_under_prob, "Under", NA))

# Compare to actual outcome
train_total_bet_vegas_correct <- 
  ifelse(is.na(train_total_bet_side_vegas) | is.na(train_total_actual_cover), 
         NA, train_total_bet_side_vegas == train_total_actual_cover)

# Accuracy over placed bets
train_total_bet_vegas_acc <- 
  mean(train_total_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
train_total_bet_vegas_count <- sum(!is.na(train_total_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
train_total_bet_side_thresh <- 
  ifelse(train_total_over_prob > train_total_thresh, "Over",
         ifelse(train_total_under_prob > train_total_thresh, "Under", NA))

# Compare to actual outcome
train_total_bet_thresh_correct <- 
  ifelse(is.na(train_total_bet_side_thresh) | is.na(train_total_actual_cover), 
         NA, train_total_bet_side_thresh == train_total_actual_cover)

# Accuracy over placed bets
train_total_bet_thresh_acc <- 
  mean(train_total_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
train_total_bet_thresh_count <- sum(!is.na(train_total_bet_thresh_correct))

#### XGB Prediction ----
table(train_total_bet_df$xgb_correct_cover, useNA = "ifany")

train_total_acc_prob_xgb <-
  mean(train_total_bet_df$xgb_correct_cover, na.rm = TRUE)*100
train_total_acc_prob_xgb

table(train_total_bet_df$xgb_correct_cover2, useNA = "ifany")

train_total_acc_prob_xgb2 <-
  mean(train_total_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
train_total_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(train_total_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(train_total_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(train_total_bet_vegas_acc, 2), "%",
            "on", train_total_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(train_total_bet_thresh_acc, 2), "%",
            "on", train_total_bet_thresh_count, "bets",
            "with", train_total_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_total_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_total_acc_prob_xgb2, 2), "%"))

## Test ----
test_total_bet_df <- modelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    total, total_line,
    over_prob, under_prob,
    over_odds, under_odds
  ) |>
  mutate(
    diff = total - total_line,
    actual_cover = case_when(
      total > total_line ~ "Over",
      total < total_line ~ "Under",
      .default = NA
    ),
    .after = total_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_total = posteriorPredMean_total,
    exp_total_line = total_line,
    exp_diff = exp_total - exp_total_line,
    exp_cover = case_when(
      exp_total > total_line ~ "Over",
      exp_total < total_line ~ "Under",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(home_model, newdata = modelData),
    xgb_away_score = predict(away_model, newdata = modelData),
    xgb_total = xgb_home_score + xgb_away_score,
    xgb_total_line = total_line,
    xgb_diff = xgb_total - xgb_total_line,
    xgb_cover = case_when(
      xgb_total > total_line ~ "Over",
      xgb_total < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_total2 = predict(xgb_total_model, newdata = modelData),
    xgb_total_line2 = total_line,
    xgb_diff2 = xgb_total2 - xgb_total_line2,
    xgb_cover2 = case_when(
      xgb_total2 > total_line ~ "Over",
      xgb_total2 < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover,
    .after = xgb_correct_cover
  )

### Accuracy ----
test_total_total_line <- test_total_bet_df$total_line
test_total_actual_cover <- test_total_bet_df$actual_cover
test_vegas_over_prob <- test_total_bet_df$over_prob
test_vegas_under_prob <- test_total_bet_df$under_prob
test_total_thresh <- 0.6

#### Posterior Means ----
table(test_total_bet_df$correct_cover, useNA = "ifany")

test_total_acc_prob_posterior_mean <-
  mean(test_total_bet_df$correct_cover, na.rm = TRUE)*100
test_total_acc_prob_posterior_mean

#### Full Posterior ----
test_total_bet_decision <- 
  sweep(
    posteriorPred_total, 2, test_total_total_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Over",
             ifelse(pred < line, "Under", NA))
    })

test_total_comparison_matrix <- 
  sweep(
    test_total_bet_decision, 2, test_total_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

test_total_game_accuracy <- 
  colMeans(test_total_comparison_matrix, na.rm = TRUE) 

test_total_acc_prob_full_posterior <- 
  mean(test_total_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
test_total_over_prob <- 
  colMeans(test_total_bet_decision == "Over", na.rm = TRUE)
test_total_under_prob <- 
  colMeans(test_total_bet_decision == "Under", na.rm = TRUE)

# Decision vector
test_total_bet_side_vegas <- 
  ifelse(test_total_over_prob > test_vegas_over_prob, "Over",
         ifelse(test_total_under_prob > test_vegas_under_prob, "Under", NA))

# Compare to actual outcome
test_total_bet_vegas_correct <- 
  ifelse(is.na(test_total_bet_side_vegas) | is.na(test_total_actual_cover), 
         NA, test_total_bet_side_vegas == test_total_actual_cover)

# Accuracy over placed bets
test_total_bet_vegas_acc <- 
  mean(test_total_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
test_total_bet_vegas_count <- sum(!is.na(test_total_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
test_total_bet_side_thresh <- 
  ifelse(test_total_over_prob > test_total_thresh, "Over",
         ifelse(test_total_under_prob > test_total_thresh, "Under", NA))

# Compare to actual outcome
test_total_bet_thresh_correct <- 
  ifelse(is.na(test_total_bet_side_thresh) | is.na(test_total_actual_cover), 
         NA, test_total_bet_side_thresh == test_total_actual_cover)

# Accuracy over placed bets
test_total_bet_thresh_acc <- 
  mean(test_total_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
test_total_bet_thresh_count <- sum(!is.na(test_total_bet_thresh_correct))

#### XGB Prediction ----
table(test_total_bet_df$xgb_correct_cover, useNA = "ifany")

test_total_acc_prob_xgb <-
  mean(test_total_bet_df$xgb_correct_cover, na.rm = TRUE)*100
test_total_acc_prob_xgb

table(test_total_bet_df$xgb_correct_cover2, useNA = "ifany")

test_total_acc_prob_xgb2 <-
  mean(test_total_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
test_total_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(test_total_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(test_total_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(test_total_bet_vegas_acc, 2), "%",
            "on", test_total_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(test_total_bet_thresh_acc, 2), "%",
            "on", test_total_bet_thresh_count, "bets",
            "with", test_total_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_total_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_total_acc_prob_xgb2, 2), "%"))

## Comparison Table ----
accuracy_metrics_total_temp <- tibble(
  Fit = rep(paste0("Fit", fitTotal), 2),
  Data = rep(c("Train", "Test"), each = 1),
  Bet = rep(c("total"), times = 2),
  PostMean = c(#train_result_acc_prob_posterior_mean,
    train_total_acc_prob_posterior_mean,
    #test_result_acc_prob_posterior_mean,
    test_total_acc_prob_posterior_mean),
  PostFull = c(#train_result_acc_prob_full_posterior,
    train_total_acc_prob_full_posterior,
    #test_result_acc_prob_full_posterior,
    test_total_acc_prob_full_posterior),
  BetVegas = c(#train_result_bet_vegas_acc,
    train_total_bet_vegas_acc,
    #test_result_bet_vegas_acc,
    test_total_bet_vegas_acc),
  BetVegasCount = c(#train_result_bet_vegas_count,
    train_total_bet_vegas_count,
    #test_result_bet_vegas_count,
    test_total_bet_vegas_count),
  BetThresh = c(#train_result_bet_thresh_acc,
    train_total_bet_thresh_acc,
    #test_result_bet_thresh_acc,
    test_total_bet_thresh_acc),
  ThreshPerc = c(#train_result_thresh,
    train_total_thresh,
    #test_result_thresh,
    test_total_thresh),
  BetThreshCount = c(#train_result_bet_thresh_count,
    train_total_bet_thresh_count,
    #test_result_bet_thresh_count,
    test_total_bet_thresh_count),
  XGB = c(#train_result_acc_prob_xgb,
    train_total_acc_prob_xgb,
    #test_result_acc_prob_xgb
    test_total_acc_prob_xgb
  )
)
accuracy_metrics_total <- bind_rows(
  accuracy_metrics_total_temp,
  accuracy_metrics_total
)

accuracy_metrics_total #<- accuracy_metrics_total_temp

# All Accuracy ----
accuracy_metrics_temp <-
  bind_rows(
    accuracy_metrics_result_temp,
    accuracy_metrics_total_temp
  )

accuracy_metrics_temp

accuracy_metrics <-
  bind_rows(
    accuracy_metrics_result,
    accuracy_metrics_total
  )
accuracy_metrics

save(accuracy_metrics_result,
     accuracy_metrics_total,
     accuracy_metrics,
     file = "~/Desktop/NFL Analysis Data/accuracy_metrics.RData")




