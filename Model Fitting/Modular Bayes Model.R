# Comprehensive NFL Bayesian Model Optimization & Weekly Evaluation Code

# 0. Load Libraries -----------------------------------------------------------
library(stringr)
library(DBI)
library(RPostgres)
library(data.table)
library(smplot2)
library(patchwork)
library(zoo)
library(pracma)
library(forecast)
library(timetk)
library(elo)
library(glmnet)
library(xgboost)
library(MASS)
library(matrixStats)
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
#library(projpred)
library(cmdstanr)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)
library(Metrics)
library(nflverse)
library(tidyverse)

# 1. Data Loading & Preparation -----------------------------------------------
# Load game data and feature-engineered data.
source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
source("./app/R/clean_modData.R")

# Load XGBoost models, variable importance, and predictions.
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_home_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_away_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_result_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_total_model_final.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/varImp_final_all.rda")
load(file = "~/Desktop/NFL Analysis Data/finalXGBmodels/xgb_cv_preds.rda")

# Determine available seasons from XGBoost predictions.
xgb_seasons <- xgb_cv_preds %>% 
  filter(complete.cases(xgb_cv_preds)) %>% 
  pull(season) %>% unique()
xgb_season_start <- min(xgb_seasons)
xgb_season_end   <- max(xgb_seasons)

# Clean and prepare the main data.
model_data <- clean_modData(data = modData, season_start = xgb_season_start)

# Add a time identifier.
model_data <- model_data %>%
  mutate(time_id = as.integer(str_extract(game_id, "^\\d{4}_\\d{2}") %>% str_remove("_")),
         .before = game_id)

# Merge in XGBoost predictions.
model_data <- model_data %>%
  left_join(xgb_cv_preds %>% select(game_id, contains("xgb")), by = "game_id") %>%
  mutate(xgb_result_calc = xgb_home_score - xgb_away_score,
         xgb_total_calc  = xgb_home_score + xgb_away_score) %>%
  relocate(xgb_home_score, .after = home_score) %>%
  relocate(xgb_away_score, .after = away_score) %>%
  relocate(xgb_result, xgb_result_calc, .after = result) %>%
  relocate(xgb_total, xgb_total_calc, .after = total)

# 2. Seasonal CV Split (Rolling Window) ---------------------------------------
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

# 3. Modular Model Fitting Function -------------------------------------------
fit_seasonal_model <- function(train_data, test_data, 
                               response = "result", use_calc = FALSE,
                               use_team_RE = TRUE, use_season_FE = FALSE,
                               # Numeric predictors to center & scale.
                               preprocess_vars = c("xgb_result", "home_rest", "away_rest", "temp", "wind"),
                               iters = 4000, burn = 2000, chains = 4,
                               rstanBackend = FALSE) {
  
  # Preprocess numeric predictors.
  if (!is.null(preprocess_vars)) {
    preProc <- preProcess(train_data[, preprocess_vars], method = c("center", "scale"))
    train_data[, preprocess_vars] <- predict(preProc, train_data[, preprocess_vars])
    test_data[, preprocess_vars]  <- predict(preProc, test_data[, preprocess_vars])
  }
  
  # Choose the XGB predictor variable.
  xgb_pred_var <- switch(response,
                         "result" = if(use_calc) "xgb_result_calc" else "xgb_result",
                         "total"  = if(use_calc) "xgb_total_calc"  else "xgb_total",
                         stop("Unsupported response"))
  
  # Build formula.
  fixed_part <- paste(
    response, "~", xgb_pred_var, " + ",
    paste(c(
      #"home_rest",
      #"away_rest",
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
  
  # Define priors.
  priors <- c(
    prior(normal(0, 5), class = "b"),  # tighter prior after centering/scaling
    prior(student_t(3, 0, 10), class = "sigma"),
    prior(student_t(3, 0, 5), class = "sd")
  )
  
  # Fit the model.
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
  
  # Generate full posterior predictive draws.
  ppd <- posterior_predict(fit, newdata = test_data, re_formula = NULL, allow_new_levels = TRUE)
  
  return(list(model = fit,
              ppd = ppd, 
              train_data = train_data,
              test_data = test_data,
              formula = formula_obj))
}

# 4. Optional: Variable Selection with projpred -----------------------------
# This step uses projpred to perform projection predictive variable selection.
# It converts the brms model to a stanreg object (if needed) and then runs cv_varsel().
# (Uncomment and run for one fold to test. Be aware that this can be computationally intensive.)

# library(projpred)
# example_fold <- "2022"  # choose a fold, e.g., season 2022
# full_model <- fold_results[[example_fold]]$model
# fit_stanreg <- as_stanreg(full_model)  # Convert to stanreg (if supported)
# vs <- cv_varsel(fit_stanreg, method = "forward")
# suggested_size <- suggest_size(vs)
# cat("Suggested model size (number of predictors):", suggested_size, "\n")
# selected_vars <- names(vs$solution)[vs$solution != 0]
# cat("Selected predictors:", paste(selected_vars, collapse = ", "), "\n")
# # You could then re-fit a model using only these predictors.

# Convert your brms model to a stanreg object (if supported)
# fit_stanreg <- as_stanreg(fold_results[["2022"]]$model)
# vs <- cv_varsel(fit_stanreg, method = "forward")
# suggested_size <- suggest_size(vs)
# cat("Suggested model size (number of predictors):", suggested_size, "\n")
# selected_vars <- names(vs$solution)[vs$solution != 0]
# cat("Selected predictors:", paste(selected_vars, collapse = ", "), "\n")

# 5. Loop Over Seasonal Folds to Fit and Store Models ------------------------
fold_results <- list()
system.time(
  for (fold in names(time_slices$train)) {
    cat("\n---------- Processing Season Fold:", fold, "----------\n")
    train_data <- model_data[time_slices$train[[fold]], ]
    test_data  <- model_data[time_slices$test[[fold]], ]
    
    fold_fit <- fit_seasonal_model(
      train_data, test_data, 
      response = "result", use_calc = TRUE,
      use_team_RE = TRUE, use_season_FE = FALSE,
      preprocess_vars = c("xgb_result_calc", "home_rest", "away_rest", "temp", "wind")
    )
    
    # Compute and report fold RMSE.
    ppd_mean <- colMeans(fold_fit$ppd)
    rmse_val <- sqrt(mean((test_data$result - ppd_mean)^2, na.rm = TRUE))
    cat(sprintf("Fold (Season %s) RMSE = %.3f\n", fold, rmse_val))
    
    fold_results[[fold]] <- fold_fit
  }
)

## 5B. Model Summary Outputs -----------------------------------------------------
# Loop over each fold and print a summary.
for (fold in names(fold_results)) {
  cat("\n---------- Model Summary for Season Fold:", fold, "----------\n")
  summary_output <- summary(fold_results[[fold]]$model)
  print(summary_output)
  
  cat("\nFixed Effects for Season Fold", fold, ":\n")
  fixed_eff <- fixef(fold_results[[fold]]$model, probs = c(0.025, 0.975))
  print(fixed_eff)
}

# 6. Post-Processing: Overall & Weekly Performance ----------------------------
# Combine predictions from all folds.
all_fold_preds <- do.call(rbind, lapply(fold_results, function(fold) {
  data.frame(
    game_id  = fold$test_data$game_id,
    season   = fold$test_data$season,
    week     = fold$test_data$week,
    observed = fold$test_data$result,
    predicted = colMeans(fold$ppd),
    predictedMed = colMedians(fold$ppd)
  )
}))

overall_rmse <- sqrt(mean((all_fold_preds$observed - all_fold_preds$predicted)^2, na.rm = TRUE))
overall_mae  <- mean(abs(all_fold_preds$observed - all_fold_preds$predicted), na.rm = TRUE)
overall_mad  <- median(abs(all_fold_preds$observed - all_fold_preds$predicted), na.rm = TRUE)
overall_mad2  <- mean(abs(all_fold_preds$observed - all_fold_preds$predictedMed), na.rm = TRUE)

cat("Overall RMSE:", round(overall_rmse, 3), "\n")
cat("Overall MAE:", round(overall_mae, 3), "\n")
cat("Overall MAD:", round(overall_mad, 3), "\n")
cat("Overall MAD2:", round(overall_mad2, 3), "\n")

# Weekly performance: group by week and compute metrics.
weekly_metrics <- all_fold_preds |>
  group_by(week) |>
  summarize(
    rmse = sqrt(mean((observed - predicted)^2, na.rm = FALSE)),
    mae = mean(abs(observed - predicted), na.rm = FALSE),
    #mae2 = mae(observed, predicted),
    n = n()
  )
print(weekly_metrics, n = 30)

# 7. Compute Coverage (95% Credible Intervals) -------------------------------
compute_coverage <- function(ppd, observed, lower = 0.025, upper = 0.975) {
  lower_bound <- apply(ppd, 2, quantile, probs = lower)
  upper_bound <- apply(ppd, 2, quantile, probs = upper)
  mean(observed >= lower_bound & observed <= upper_bound, na.rm = TRUE)
}

fold_coverage <- sapply(fold_results, function(fold) {
  compute_coverage(fold$ppd, fold$test_data$result)
})
cat("Coverage across folds (proportion within 95% intervals):\n")
print(round(fold_coverage, 3))

# 8. Betting Evaluation --------------------------------------------------------
betting_eval_fold <- function(fold_result,
                              home_model, away_model, xgb_result_model,
                              threshold = 0.6) {
  test_df <- fold_result$test_data
  test_df$result_pred <- colMeans(fold_result$ppd)
  
  test_df <- test_df %>%
    mutate(
      diff = result - spread_line,
      actual_cover = case_when(
        result > spread_line ~ "Home",
        result < spread_line ~ "Away",
        TRUE ~ NA_character_
      ),
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
  
  acc_posterior_mean <- mean(test_df$correct_cover, na.rm = TRUE) * 100
  
  ppd_decision <- sweep(fold_result$ppd, 2, test_df$spread_line, 
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

# Apply the betting evaluation to each fold.
betting_evals <- lapply(fold_results, function(fold) {
  betting_eval_fold(fold, 
                    xgb_home_model_final,
                    xgb_away_model_final, 
                    xgb_result_model_final, 
                    threshold = 0.6)
})

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

print(accuracy_metrics_result_temp)

# 9. Generate PPC Plots --------------------------------------------------------
ppc_plots <- lapply(fold_results, function(fold) {
  pp_check(fold$model, newdata = fold$test_data, resp = "result", ndraws = 100, type = "dens_overlay")
})
# Display or save ppc_plots as needed.
ppc_plots
