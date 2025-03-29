
# 0. Setup & Libraries ------------------------------
library(dplyr)
library(readr)
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
nfl_data <- read_csv("~/Desktop/modData.csv")

# Include both regular season and postseason games.
# Create a postseason indicator: 0 for regular season, 1 for postseason.
nfl_data <- nfl_data %>% 
  mutate(postseason = ifelse(season_type == "REG", 0, 1))

# Order data by season and week (assumes week is available for both regular and postseason games)
nfl_data <- nfl_data %>% arrange(season, week)

# Create additional net features if not already available (e.g., net_SRS and net_off_epa)
if(!"net_SRS" %in% names(nfl_data)){
  nfl_data <- nfl_data %>% mutate(net_SRS = home_SRS - away_SRS)
}
if(!"net_off_epa" %in% names(nfl_data)){
  nfl_data <- nfl_data %>% mutate(net_off_epa = home_off_epa_cum - away_def_epa_cum)
}

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
game_level_vars <- c("weekday", "time_of_day", "location", "div_game", "roof", "surface", "temp", "wind")

# Candidate predictors for XGBoost: all numeric variables not in drop_vars and not in game_level_vars.
all_candidates <- setdiff(names(nfl_data), drop_vars)
candidate_xgb_vars <- all_candidates[ sapply(nfl_data[, all_candidates], is.numeric) ]
candidate_xgb_vars <- setdiff(candidate_xgb_vars, game_level_vars)
setdiff(all_candidates, candidate_xgb_vars)

cat("Team-specific candidate predictors for XGBoost:\n")
print(candidate_xgb_vars)

## 2A. Preprocessing: ----
# Remove Near-Zero Variance and Highly Correlated Predictors
# Remove near-zero variance predictors
nzv <- nearZeroVar(nfl_data[, candidate_xgb_vars], saveMetrics = TRUE)
candidate_xgb_vars[nzv$nzv]
candidate_xgb_vars <- candidate_xgb_vars[!nzv$nzv]

cat("After near-zero variance removal:\n")
print(candidate_xgb_vars)

# Remove highly correlated predictors (cutoff = 0.95)
cor_matrix <- cor(nfl_data[, candidate_xgb_vars], use = "pairwise.complete.obs")
high_corr <- findCorrelation(cor_matrix, cutoff = 0.95) #0.99
candidate_xgb_vars[high_corr]
if(length(high_corr) > 0) {
  candidate_xgb_vars_corr <- candidate_xgb_vars[-high_corr]
}

preProcCorr <- preProcess(
  nfl_data |> select(candidate_xgb_vars),
  method = c("corr")
)
preProcCorr_vars <- preProcCorr$method$remove
setdiff(preProcCorr_vars, candidate_xgb_vars[high_corr])

candidate_xgb_vars <- candidate_xgb_vars_corr
cat("After correlation filtering:\n")
print(candidate_xgb_vars)

# 3. Define RFE Function for XGBoost Predictors ------------------------------
# We'll use recursive feature elimination (RFE) with a random forest wrapper
perform_rfe <- function(train_df, outcome, candidate_vars) {
  rfe_control <- rfeControl(
    functions = rfFuncs, 
    method = "cv", 
    number = 5, 
    verbose = TRUE
  )
  
  sizes <- seq(10, min(50, length(candidate_vars)), by = 10)
  
  rfe_result <- rfe(x = train_df[, candidate_vars],
                    y = train_df[[outcome]],
                    sizes = sizes,
                    rfeControl = rfe_control)
  return(rfe_result$optVariables)
}

## 3A. Split Data  ------------------------------
train_data <- nfl_data |> filter(season < 2024)
test_data <- nfl_data |> filter(season == 2024)

which(!complete.cases(train_data))
na_counts <- colSums(is.na(train_data))
na_cols <- na_counts[na_counts > 0]
print(na_cols)

train_df <- train_data |> 
  select(c(game_id, candidate_xgb_vars))
train_df <- train_df |> filter(complete.cases(train_df))
train_IDs <- train_df$game_id

train_data <- train_data |> filter(game_id %in% train_IDs)
rm(train_df)

## 3B. Best Features ----
system.time(
  best_vars_home <- perform_rfe(train_data, "home_score", candidate_xgb_vars)
)

system.time(
  best_vars_away <- perform_rfe(train_data, "away_score", candidate_xgb_vars)
)

cat("Selected predictors for home_score:\n")
print(best_vars_home[1:20])
cat("Selected predictors for away_score:\n")
print(best_vars_away[1:20])

save(best_vars_home, file = "~/Desktop/NFL Analysis Data/best_vars_home.RData")
save(best_vars_away, file = "~/Desktop/NFL Analysis Data/best_vars_away.RData")

# Use the union of selected predictors for both models
best_vars <- union(best_vars_home, best_vars_away)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 4. Define Recursive (Rolling) Training/Testing Function  ---------------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#recursive_forecast <- function(train_data, test_data) {

## 4A. Feature Selection  RFE ----
# (using team-specific predictors) via
best_vars_home <- perform_rfe(train_data, "home_score", candidate_xgb_vars)
best_vars_away <- perform_rfe(train_data, "away_score", candidate_xgb_vars)

cat("Selected predictors for home_score:\n")
print(best_vars_home)
cat("Selected predictors for away_score:\n")
print(best_vars_away)

# Use the union of selected predictors for both models
best_vars <- union(best_vars_home, best_vars_away)

## 4B. Train XGBoost Models ----
# for Score Predictions (using caret) on team-specific features
train_control <- trainControl(method = "cv", number = 10)
xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

### 1. Home ----
# Home score model using selected team-specific features
home_formula <- as.formula(paste("home_score ~", paste(best_vars, collapse = " + ")))
home_formula <- as.formula(paste("home_score ~", paste(candidate_xgb_vars, collapse = " + ")))
system.time(
  home_model <- train(
    home_formula, 
    data = train_data,
    method = "xgbTree",
    trControl = train_control
    #tuneGrid = xgb_grid
  )
)
save(home_model, file = "~/Desktop/NFL Analysis Data/xgb_home_model.RData")
save(home_model, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_home_model.rda")
home_model
varImp_home <- varImp(home_model)
varImp_home
best_home_vars <- varImp_home$importance |>
  filter(Overall > 0) |>
  row.names()

pred_home_score <- predict(home_model, newdata = test_data)

### 2. Away ----
# Away score model using selected team-specific features
away_formula <- as.formula(paste("away_score ~", paste(best_vars, collapse = " + ")))
away_formula <- as.formula(paste("away_score ~", paste(candidate_xgb_vars, collapse = " + ")))

system.time(
  away_model <- train(
    away_formula, 
    data = train_data,
    method = "xgbTree",
    trControl = train_control
    #tuneGrid = xgb_grid
  )
)
save(away_model, file = "~/Desktop/NFL Analysis Data/xgb_away_model.RData")
save(away_model, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_away_model.rda")
away_model
varImp_away <- varImp(away_model)
best_away_vars <- varImp_away$importance |>
  filter(Overall > 0) |>
  row.names()

### 3. Result ----
# Spread score model using selected team-specific features
result_formula <- as.formula(paste("result ~", paste(best_vars, collapse = " + ")))
result_formula <- as.formula(paste("result ~", paste(candidate_xgb_vars, collapse = " + ")))
system.time(
  result_model <- train(
    result_formula, 
    data = train_data,
    method = "xgbTree",
    trControl = train_control
    #tuneGrid = xgb_grid
  )
)
save(result_model, file = "~/Desktop/NFL Analysis Data/xgb_result_model.RData")
save(result_model, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_result_model.rda")
result_model
varImp_result <- varImp(result_model)
best_result_vars <- varImp_result$importance |>
  filter(Overall > 0) |>
  row.names()

### 4. Total ----
# Spread score model using selected team-specific features
total_formula <- as.formula(paste("total ~", paste(best_vars, collapse = " + ")))
total_formula <- as.formula(paste("total ~", paste(candidate_xgb_vars, collapse = " + ")))
system.time(
  total_model <- train(
    total_formula, 
    data = train_data,
    method = "xgbTree",
    trControl = train_control
    #tuneGrid = xgb_grid
  )
)
save(total_model, file = "~/Desktop/NFL Analysis Data/xgb_total_model.RData")
save(total_model, file = "~/Desktop/NFLAnalysisTest/app/data/xgb_total_model.rda")
total_model
varImp_total <- varImp(total_model)
varImp_total
best_total_vars <- varImp_total$importance > 0

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
