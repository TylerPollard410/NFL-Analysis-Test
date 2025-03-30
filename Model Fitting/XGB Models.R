
# 0. Setup & Libraries ------------------------------
library(dplyr)
library(readr)
library(doParallel)
library(caret)
library(xgboost)
library(brms)
library(bayesplot)
library(lubridate)
library(Metrics)  # for MAE, RMSE
library(zoo)      # for rolling averages

set.seed(123)


# 1. Load & Prepare Data ------------------------------
# Read in the feature-engineered CSV file (ensure "modData.csv" is in your working directory)
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

source("./app/R/clean_modData.R")
nfl_data <- clean_modData(data = modData, season_start = 2007)
write_csv(nfl_data, file = "~/Desktop/nfl_data.csv")

# nfl_data <- read_csv("~/Desktop/modData.csv")
# 
# # Include both regular season and postseason games.
# # Create a postseason indicator: 0 for regular season, 1 for postseason.
# nfl_data <- nfl_data %>% 
#   mutate(postseason = ifelse(season_type == "REG", 0, 1))
# 
# # Order data by season and week (assumes week is available for both regular and postseason games)
# nfl_data <- nfl_data %>% arrange(season, week)
# 
# # Create additional net features if not already available (e.g., net_SRS and net_off_epa)
# if(!"net_SRS" %in% names(nfl_data)){
#   nfl_data <- nfl_data %>% mutate(net_SRS = home_SRS - away_SRS)
# }
# if(!"net_off_epa" %in% names(nfl_data)){
#   nfl_data <- nfl_data %>% mutate(net_off_epa = home_off_epa_cum - away_def_epa_cum)
# }

# Create an overall "time" identifier for ordering (combining season and week)
nfl_data <- nfl_data %>% mutate(time_id = as.numeric(paste0(season, sprintf("%02d", week))))


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

## 2A. Preprocessing: ----
candidate_data <- nfl_data |> select(candidate_xgb_vars)

# Keep only complete.cases
candidate_data <- candidate_data |> filter(complete.cases(candidate_data))

### 2A1. Linear Combos ----
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

### 2A2. Near-zero variance ----
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

# Remove highly correlated predictors (cutoff = 0.95)
cor_matrix <- cor(nfl_data[, candidate_xgb_vars_clean], use = "pairwise.complete.obs")
high_corr <- findCorrelation(cor_matrix, cutoff = 0.95) #0.99
candidate_xgb_vars_clean[high_corr]
if(length(high_corr) > 0) {
  candidate_xgb_vars_corr <- candidate_xgb_vars_clean[-high_corr]
}

preProcCorr <- preProcess(
  nfl_data |> select(candidate_xgb_vars_clean),
  method = c("corr")
)
preProcCorr_vars <- preProcCorr$method$remove
preProcCorr_vars
setdiff(preProcCorr_vars, candidate_xgb_vars_clean[high_corr])

candidate_xgb_vars <- candidate_xgb_vars_corr
cat("After correlation filtering:\n")
print(candidate_xgb_vars)

## 2B. Filter Data ----
#nfl_data_model <- nfl_data |> select(candidate_xgb_vars_clean)

# 3. Define CV Indicies for XGBoost ------------------------------
## 3A. Expanding Window ----
### 3A1. Per Week ----
# 1) Ensure your data is sorted chronologically by season, week, and that you have season_type = "REG" or "POST"
nfl_data_model <- nfl_data |>
  select(time_id, everything()) |>
  arrange(season, week, season_type)

# 2) Create a numeric "time_id" if not already present (e.g. 201601, 201602, etc.)
#    You said you already have something like this, but for clarity:
# nfl_data_model <- nfl_data_model %>%
#   mutate(time_id = as.numeric(paste0(season, sprintf("%02d", week))))

# 3) Define the initial training cutoff (use 2007-2010 as your first training block)
initial_train_cutoff <- 2009

# 4) Identify all seasons (beyond the cutoff) we want to iterate over
seasons_after_cutoff <- nfl_data_model %>%
  filter(season > initial_train_cutoff) %>%
  distinct(season) %>%
  pull(season) %>%
  sort()

# 5) Prepare lists to store training/test indices for each fold
train_indices <- list()
test_indices  <- list()

# 6) Loop through each season after the cutoff
for (s in seasons_after_cutoff) {
  
  # A) Handle Regular Season Weeks for Season s 
  # Extract all unique REG weeks for season s
  reg_weeks <- nfl_data_model %>%
    filter(season == s, season_type == "REG") %>%
    distinct(week, time_id) %>%
    arrange(week)
  
  # For each regular-season week, define a fold
  for (i in seq_len(nrow(reg_weeks))) {
    current_week   <- reg_weeks$week[i]
    current_timeid <- reg_weeks$time_id[i]
    
    # Training set: all data with time_id < current_timeid
    train_idx <- which(nfl_data_model$time_id < current_timeid)
    
    # Test set: rows for (season == s, week == current_week, season_type == "REG")
    test_idx  <- which(
      nfl_data_model$season == s &
        nfl_data_model$week == current_week &
        nfl_data_model$season_type == "REG"
    )
    
    if (length(train_idx) > 0 && length(test_idx) > 0) {
      train_indices[[length(train_indices) + 1]] <- train_idx
      test_indices[[length(test_indices) + 1]]  <- test_idx
    }
  }
  
  # B) Handle Postseason for Season s
  # Check if there are any POST games for season s
  post_games <- nfl_data_model %>%
    filter(season == s, season_type != "REG")
  
  if (nrow(post_games) > 0) {
    # If postseason exists, treat all POST games of season s as one fold
    first_post_timeid <- min(post_games$time_id)
    
    # Training set: all data with time_id < first_post_timeid
    train_idx <- which(nfl_data_model$time_id < first_post_timeid)
    
    # Test set: all postseason rows for season s
    test_idx <- which(
      nfl_data_model$season == s &
        nfl_data_model$season_type != "REG"
    )
    
    if (length(train_idx) > 0 && length(test_idx) > 0) {
      train_indices[[length(train_indices) + 1]] <- train_idx
      test_indices[[length(test_indices) + 1]]  <- test_idx
    }
  }
}

# 7) Package the train/test indices into the format caret expects
time_slices_expanding <- list(train = train_indices, test = test_indices)

# Check how many folds we have
cat("Total number of folds:", length(time_slices_expanding$train), "\n")

# Example of how many observations in the first fold
fn <- 274
cat("Fold", fn, ": Train obs =", length(time_slices_expanding$train[[fn]]),
    "Test obs =", length(time_slices_expanding$test[[fn]]), "\n")

# 8) Create trainControl with custom CV splits
cv_control_expanding <- trainControl(
  method = "cv",
  index = time_slices_expanding$train,
  indexOut = time_slices_expanding$test,
  summaryFunction = defaultSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

# To inspect the cv_control object:
#print(cv_control_expanding)

# Then, for example, when training your XGBoost model you might do:
# set.seed(123)
# xgb_model <- train(
#   x = nfl_data_model[, candidate_xgb_vars_clean],  # your predictors
#   y = nfl_data$target,  # replace with your actual outcome variable
#   method = "xgbTree",
#   trControl = cv_control,
#   tuneLength = 5
# )

### 3A2. Per Season ----
# Ensure data is sorted by season (and week if needed)
nfl_data_model <- nfl_data_model %>%
  arrange(season, week)

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

# Inspect the number of folds:
cat("Total Expanding CV folds:", length(time_slices_expanding_season$train), "\n")


## 3B. Rolling Window ----
### 3B1. Per Week ----
# Ensure data is sorted chronologically
nfl_data_model <- nfl_data_model %>%
  arrange(season, week, season_type)

# Define rolling window parameters:
rolling_window <- 4            # number of seasons to use for training
initial_train_cutoff <- 2009   # we start testing on seasons after 2010

# Assume nfl_data_model already has a numeric time_id column (e.g., 201001, 201002, …)

# Prepare lists to store rolling CV fold indices
train_indices_rolling <- list()
test_indices_rolling  <- list()

# Identify the seasons to test (all seasons after the initial cutoff)
seasons_to_test <- nfl_data_model %>%
  filter(season > initial_train_cutoff) %>%
  distinct(season) %>%
  arrange(season) %>%
  pull(season)

# Loop through each season in which we will create folds
for (s in seasons_to_test) {
  
  # Limit training data to the most recent 'rolling_window' seasons
  min_season_allowed <- s - rolling_window + 1
  
  # Part A: Regular Season Folds
  # Get unique regular-season weeks for season s
  reg_weeks <- nfl_data_model %>%
    filter(season == s, season_type == "REG") %>%
    distinct(week, time_id) %>%
    arrange(week)
  
  # For each regular season week, create a fold:
  for (i in seq_len(nrow(reg_weeks))) {
    current_week <- reg_weeks$week[i]
    current_timeid <- reg_weeks$time_id[i]
    
    # Training set: rows with season >= min_season_allowed and time_id < current_timeid
    train_idx <- which(nfl_data_model$season >= min_season_allowed &
                         nfl_data_model$time_id < current_timeid)
    
    # Test set: regular season games for season s and current_week
    test_idx <- which(nfl_data_model$season == s &
                        nfl_data_model$week == current_week &
                        nfl_data_model$season_type == "REG")
    
    if (length(train_idx) > 0 && length(test_idx) > 0) {
      train_indices_rolling[[length(train_indices_rolling) + 1]] <- train_idx
      test_indices_rolling[[length(test_indices_rolling) + 1]] <- test_idx
    }
  }
  
  # Part B: Postseason Fold for Season s
  # Instead of splitting postseason into multiple weeks, treat all POST games for season s as one fold.
  post_games <- nfl_data_model %>%
    filter(season == s, season_type != "REG")
  
  if (nrow(post_games) > 0) {
    # Use the earliest time_id among POST games to define the cutoff.
    first_post_timeid <- min(post_games$time_id)
    
    train_idx <- which(nfl_data_model$season >= min_season_allowed &
                         nfl_data_model$time_id < first_post_timeid)
    
    # Test set: all postseason games for season s
    test_idx <- which(nfl_data_model$season == s & nfl_data_model$season_type != "REG")
    
    if (length(train_idx) > 0 && length(test_idx) > 0) {
      train_indices_rolling[[length(train_indices_rolling) + 1]] <- train_idx
      test_indices_rolling[[length(test_indices_rolling) + 1]] <- test_idx
    }
  }
}

# Package the indices into a list for caret
time_slices_rolling <- list(train = train_indices_rolling, test = test_indices_rolling)

cat("Total rolling folds:", length(time_slices_rolling$train), "\n")

fn <- 274
cat("Fold", fn, ": Train obs =", length(time_slices_rolling$train[[fn]]),
    "Test obs =", length(time_slices_rolling$test[[fn]]), "\n")

# Set up caret's trainControl using these rolling window indices
cv_control_rolling <- trainControl(
  method = "cv",
  index = time_slices_rolling$train,
  indexOut = time_slices_rolling$test,
  summaryFunction = defaultSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

# Inspect the trainControl object
#print(cv_control_rolling)

# Then, for example, when training your XGBoost model you might do:
# set.seed(123)
# xgb_model <- train(
#   x = nfl_data_model[, candidate_xgb_vars_clean],  # your predictors
#   y = nfl_data$target,  # replace with your actual outcome variable
#   method = "xgbTree",
#   trControl = cv_control,
#   tuneLength = 5
# )

### 3B2. Per Season ----
# Ensure data is sorted chronologically
nfl_data_model <- nfl_data_model %>%
  arrange(season, week, season_type)

# Define rolling window parameters:
# We'll use only the three seasons immediately preceding the test season for training.
rolling_window <- 3
#initial_train_cutoff <- 2009   # we start testing on seasons after 2010

# Initialize lists for rolling window CV indices
train_indices_roll_season <- list()
test_indices_roll_season  <- list()

for (s in cv_seasons) {
  # Define the earliest season allowed for training for test season s.
  # For example, if s is 2012 and rolling_window is 3, then we use seasons 2009, 2010, and 2011.
  min_season_allowed <- s - rolling_window
  
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

cat("Total Rolling CV folds:", length(time_slices_rolling_season$train), "\n")


## 3C. Split Data  ------------------------------
# train_data <- nfl_data |> filter(season < 2024)
# test_data <- nfl_data |> filter(season == 2024)
# 
# which(!complete.cases(train_data))
# na_counts <- colSums(is.na(train_data))
# na_cols <- na_counts[na_counts > 0]
# print(na_cols)
# 
# train_df <- train_data |> 
#   select(c(game_id, candidate_xgb_vars))
# train_df <- train_df |> filter(complete.cases(train_df))
# train_IDs <- train_df$game_id
# 
# train_data <- train_data |> filter(game_id %in% train_IDs)
# rm(train_df)
# 
# ## 3D. Best Features ----
# system.time(
#   best_vars_home <- perform_rfe(train_data, "home_score", candidate_xgb_vars)
# )
# 
# system.time(
#   best_vars_away <- perform_rfe(train_data, "away_score", candidate_xgb_vars)
# )
# 
# cat("Selected predictors for home_score:\n")
# print(best_vars_home[1:20])
# cat("Selected predictors for away_score:\n")
# print(best_vars_away[1:20])
# 
# save(best_vars_home, file = "~/Desktop/NFL Analysis Data/best_vars_home.RData")
# save(best_vars_away, file = "~/Desktop/NFL Analysis Data/best_vars_away.RData")
# 
# # Use the union of selected predictors for both models
# best_vars <- union(best_vars_home, best_vars_away)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 4. Define Recursive (Rolling) Training/Testing Function  ---------------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#recursive_forecast <- function(train_data, test_data) {

## 4A. Feature Selection  RFE ----
# (using team-specific predictors) via
# best_vars_home <- perform_rfe(train_data, "home_score", candidate_xgb_vars)
# best_vars_away <- perform_rfe(train_data, "away_score", candidate_xgb_vars)
# 
# cat("Selected predictors for home_score:\n")
# print(best_vars_home)
# cat("Selected predictors for away_score:\n")
# print(best_vars_away)

# Use the union of selected predictors for both models
# best_vars <- union(best_vars_home, best_vars_away)

# 5. Train XGBoost Models ----
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

cl <- makeCluster(detectCores() - 1)

## A. Expanding Window ----
### 1. Home ----
# Home score model using selected team-specific features
#home_formula <- as.formula(paste("home_score ~", paste(best_vars, collapse = " + ")))
#home_formula <- as.formula(paste("home_score ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)

system.time(
  home_model_expand_season <- train(
    #home_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$home_score,
    method = "xgbTree",
    trControl = cv_control_expanding_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(home_model_expand_season, file = "~/Desktop/NFL Analysis Data/xgb_home_model_expand_season.RData")
save(home_model_expand_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_home_model_expand_season.rda")
home_model_expand_season
varImp_home_expand_season <- varImp(home_model_expand_season)
varImp_home_expand_season
best_home_vars_expand_season <- varImp_home_expand_season$importance |>
  filter(Overall > 0) |>
  row.names()

plot(home_model_expand_season)


pred_home_score_expand <- predict(home_model_expand_season, newdata = test_data)

### 2. Away ----
# Away score model using selected team-specific features
#away_formula <- as.formula(paste("away_score ~", paste(best_vars, collapse = " + ")))
#away_formula <- as.formula(paste("away_score ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  away_model_expand_season <- train(
    #away_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$away_score,
    method = "xgbTree",
    trControl = cv_control_expanding_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(away_model_expand_season, file = "~/Desktop/NFL Analysis Data/xgb_away_model_expand_season.RData")
save(away_model_expand_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_away_model_expand_season.rda")
away_model_expand_season
varImp_away_expand_season <- varImp(away_model_expand_season)
varImp_away_expand_season
best_away_vars_expand_season <- varImp_away_expand_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_away_score_expand <- predict(away_model_expand_season, newdata = test_data)

### 3. Result ----
# Spread score model using selected team-specific features
#result_formula <- as.formula(paste("result ~", paste(best_vars, collapse = " + ")))
#result_formula <- as.formula(paste("result ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  result_model_expand_season <- train(
    #result_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$result,
    method = "xgbTree",
    trControl = cv_control_expanding_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(result_model_expand_season, file = "~/Desktop/NFL Analysis Data/xgb_result_model_expand_season.RData")
save(result_model_expand_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_result_model_expand_season.rda")
result_model_expand_season
varImp_result_expand_season <- varImp(result_model_expand_season)
varImp_result_expand_season
best_result_vars_expand_season <- varImp_result_expand_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_result_expand <- predict(result_model_expand_season, newdata = test_data)

### 4. Total ----
# Spread score model using selected team-specific features
#total_formula <- as.formula(paste("total ~", paste(best_vars, collapse = " + ")))
#total_formula <- as.formula(paste("total ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  total_model_expand_season <- train(
    #total_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$total,
    method = "xgbTree",
    trControl = cv_control_expanding_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(total_model_expand_season, file = "~/Desktop/NFL Analysis Data/xgb_total_model_expand_season.RData")
save(total_model_expand_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_total_model_expand_season.rda")
total_model_expand_season
varImp_total_expand_season <- varImp(total_model_expand_season)
varImp_total_expand_season
best_total_vars_expand_season <- varImp_total_expand_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_total_expand <- predict(total_model_expand_season, newdata = test_data)

## B. Rolling Window ----
### 1. Home ----
# Home score model using selected team-specific features
#home_formula <- as.formula(paste("home_score ~", paste(best_vars, collapse = " + ")))
#home_formula <- as.formula(paste("home_score ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  home_model_roll_season <- train(
    #home_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$home_score,
    method = "xgbTree",
    trControl = cv_control_rolling_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(home_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_home_model_roll_season.RData")
save(home_model_roll_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_home_model_roll_season.rda")
home_model_roll_season
varImp_home_roll_season <- varImp(home_model_roll_season)
varImp_home_roll_season
best_home_vars_roll_season <- varImp_home_roll_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_home_score_roll <- predict(home_model_roll_season, newdata = test_data)

### 2. Away ----
# Away score model using selected team-specific features
#away_formula <- as.formula(paste("away_score ~", paste(best_vars, collapse = " + ")))
#away_formula <- as.formula(paste("away_score ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  away_model_roll_season <- train(
    #away_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$away_score,
    method = "xgbTree",
    trControl = cv_control_rolling_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(away_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_away_model_roll_season.RData")
save(away_model_roll_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_away_model_roll_season.rda")
away_model_roll_season
varImp_away_roll_season <- varImp(away_model_roll_season)
varImp_away_roll_season
best_away_vars_roll_season <- varImp_away_roll_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_away_score_roll <- predict(away_model_roll_season, newdata = test_data)

### 3. Result ----
# Spread score model using selected team-specific features
#result_formula <- as.formula(paste("result ~", paste(best_vars, collapse = " + ")))
#result_formula <- as.formula(paste("result ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  result_model_roll_season <- train(
    #result_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$result,
    method = "xgbTree",
    trControl = cv_control_rolling_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(result_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_result_model_roll_season.RData")
save(result_model_roll_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_result_model_roll_season.rda")
result_model_roll_season
varImp_result_roll_season <- varImp(result_model_roll_season)
varImp_result_roll_season
best_result_vars_roll_season <- varImp_result_roll_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_result_roll <- predict(result_model_roll_season, newdata = test_data)

### 4. Total ----
# Spread score model using selected team-specific features
#total_formula <- as.formula(paste("total ~", paste(best_vars, collapse = " + ")))
#total_formula <- as.formula(paste("total ~", paste(candidate_xgb_vars, collapse = " + ")))

#registerDoParallel(cl)
set.seed(123)
system.time(
  total_model_roll_season <- train(
    #total_formula, 
    #data = train_data,
    x = nfl_data_model[, candidate_xgb_vars_clean],
    y = nfl_data_model$total,
    method = "xgbTree",
    trControl = cv_control_rolling_season,
    tuneLength = 5
    #tuneGrid = xgb_grid
  )
)

# Once all models are trained, stop the cluster:
#stopCluster(cl)

save(total_model_roll_season, file = "~/Desktop/NFL Analysis Data/xgb_total_model_roll_season.RData")
save(total_model_roll_season, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_total_model_roll_season.rda")
total_model_roll_season
varImp_total_roll_season <- varImp(total_model_roll_season)
varImp_total_roll_season
best_total_vars_roll_season <- varImp_total_roll_season$importance |>
  filter(Overall > 0) |>
  row.names()

pred_total_roll <- predict(total_model_roll_season, newdata = test_data)

# 4C. Generate Score Predictions on Test Data
test_data <- test_data %>%
  mutate(
    pred_home_score <- predict(home_model, newdata = test_data),
    pred_away_score = predict(away_model, newdata = test_data),
    pred_spread     = pred_home_score - pred_away_score,
    pred_total      = pred_home_score + pred_away_score,
    spread          = home_score - away_score,   # observed spread
    total           = home_score + away_score      # observed total
  )

# 4D. Fit Bayesian Models to "Vet" XGBoost Predictions
# Augment training data with in-sample XGBoost predictions using team-specific features.
train_data <- train_data %>%
  mutate(
    pred_home_score = predict(home_model, newdata = train_data),
    pred_away_score = predict(away_model, newdata = train_data),
    pred_spread = pred_home_score - pred_away_score,
    pred_total  = pred_home_score + pred_away_score,
    spread = home_score - away_score,
    total = home_score + away_score
  )

# In the Bayesian models, reintroduce game-level predictors.
# These include: location, div_game, roof, surface, temp, wind, week, and postseason.
bayes_spread_formula <- bf(spread ~ pred_spread + location + div_game + roof + surface + temp + wind + week + postseason +
                             (1 | home_team) + (1 | away_team))
bayes_total_formula  <- bf(total ~ pred_total + location + div_game + roof + surface + temp + wind + week + postseason +
                             (1 | home_team) + (1 | away_team))

bayes_spread <- brm(
  formula = bayes_spread_formula,
  data = train_data,
  family = student(),
  prior = c(prior(normal(0, 5), class = "b")),
  iter = 1000, warmup = 500, chains = 2, cores = 2,
  refresh = 0, seed = 123
)

bayes_total <- brm(
  formula = bayes_total_formula,
  data = train_data,
  family = student(),
  prior = c(prior(normal(0, 5), class = "b")),
  iter = 1000, warmup = 500, chains = 2, cores = 2,
  refresh = 0, seed = 123
)

# Generate posterior predictions for test data
post_spread <- posterior_predict(bayes_spread, newdata = test_data)
post_total  <- posterior_predict(bayes_total, newdata = test_data)

test_data <- test_data %>%
  mutate(
    prob_cover_minus3 = apply(post_spread, 2, function(x) mean(x > -3)),
    prob_over_45    = apply(post_total, 2, function(x) mean(x > 45))
  )

# 4E. Compute Performance Metrics
mae_home  <- mae(test_data$home_score, test_data$pred_home_score)
mae_away  <- mae(test_data$away_score, test_data$pred_away_score)
rmse_home <- rmse(test_data$home_score, test_data$pred_home_score)
rmse_away <- rmse(test_data$away_score, test_data$pred_away_score)

mae_spread <- mae(test_data$spread, test_data$pred_spread)
rmse_spread <- rmse(test_data$spread, test_data$pred_spread)

mae_total <- mae(test_data$total, test_data$pred_total)
rmse_total <- rmse(test_data$total, test_data$pred_total)

metrics <- list(mae_home = mae_home, mae_away = mae_away,
                rmse_home = rmse_home, rmse_away = rmse_away,
                mae_spread = mae_spread, rmse_spread = rmse_spread,
                mae_total = mae_total, rmse_total = rmse_total)

return(list(test_data = test_data, metrics = metrics, 
            home_model = home_model, away_model = away_model,
            bayes_spread = bayes_spread, bayes_total = bayes_total,
            best_vars = best_vars))
#}

# ------------------------------
# 5. Rolling Forecast Simulation
# ------------------------------
# We use past seasons as initial training and then simulate forecasting the test season week-by-week.
# This recursive approach updates the training set as new weekly data arrive.

# Choose test season (e.g., maximum season in the data)
test_season <- max(nfl_data$season)
train_initial <- nfl_data %>% filter(season < test_season)
test_season_data <- nfl_data %>% filter(season == test_season) %>% arrange(week)

performance_list <- list()

# Initial training set: all seasons before the test season
train_current <- train_initial

# Loop over each week of the test season
unique_weeks <- sort(unique(test_season_data$week))

for (wk in unique_weeks) {
  cat("Forecasting for Season", test_season, "Week", wk, "\n")
  
  test_current <- test_season_data %>% filter(week == wk)
  if(nrow(test_current) == 0) next
  
  forecast_result <- recursive_forecast(train_current, test_current)
  performance_list[[paste0("Week_", wk)]] <- forecast_result$metrics
  
  cat("Selected predictors for this iteration (team-specific):\n")
  print(forecast_result$best_vars)
  
  # Update training data by appending the current week's data
  train_current <- bind_rows(train_current, test_current)
}

# ------------------------------
# 6. Aggregate & Display Performance Metrics
# ------------------------------
perf_df <- do.call(rbind, lapply(names(performance_list), function(week) {
  met <- performance_list[[week]]
  data.frame(week = week,
             mae_home = met$mae_home,
             mae_away = met$mae_away,
             rmse_home = met$rmse_home,
             rmse_away = met$rmse_away,
             mae_spread = met$mae_spread,
             rmse_spread = met$rmse_spread,
             mae_total = met$mae_total,
             rmse_total = met$rmse_total)
}))

print(perf_df)
write.csv(perf_df, "performance_metrics.csv", row.names = FALSE)
