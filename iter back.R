# brms_model_updated.R
# Backtest model 1 over 2010-current with iterative refitting

# Dependencies
library(dplyr)
library(tibble)
library(brms)
library(caret)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Load data ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modDataBase <- readRDS("./data/modDataBase.rds")
kfaData     <- readRDS("./data/kfaData.rds")  # list(train, test)

# Prepare complete KFA tables
kfa_train_all <- kfaData$train |> 
  left_join(modDataBase |> select(game_id, result), by = "game_id")
kfa_test_all  <- kfaData$test

# Define backtest weeks (2010 to current)
weeks_df <- kfa_test_all |> 
  distinct(season, week) |> 
  arrange(season, week) |> 
  filter(season >= 2010)

# Initialize training set (pre-2010)
train_set <- kfa_train_all |> filter(season < 2010)

# Container for results
backtest_results <- list()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Iterative backtesting loop ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_weeks <- nrow(weeks_df)
for (i in seq_len(n_weeks)) {
  yr <- weeks_df$season[i]
  wk <- weeks_df$week[i]
  key <- sprintf("S%d_W%d", yr, wk)
  message(sprintf("[Backtest %d/%d] Processing %s...", i, n_weeks, key))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2a. Split and Preprocess ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  test_feats <- kfa_test_all |> 
    filter(season == yr, week == wk) |> 
    select(game_id, season, week, home_team, away_team,
           net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum)
  test_full <- test_feats |> 
    left_join(modDataBase |> select(game_id, result), by = "game_id")
  
  message("  - Preprocessing data...")
  preProc <- preProcess(
    train_set |> select(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
    method = c("center", "scale")
  )
  train_pp <- train_set |> 
    mutate(across(c(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
                  ~ predict(preProc, .x))) |> 
    select(game_id, season, week, home_team, away_team,
           result, net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum)
  test_pp <- test_feats |> 
    mutate(across(c(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
                  ~ predict(preProc, .x)))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2b. Fit BRMS model ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  message("  - Fitting BRMS model...")
  fit_model <- brm(
    result ~ net_elo + net_rating + hfa_pre + home_net_off_red_zone_app_perc_cum,
    data    = train_pp,
    family  = gaussian(),
    chains  = 4,
    iter    = 2000,
    cores   = parallel::detectCores(),
    seed    = 123
  )
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2c. Posterior prediction ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  message("  - Generating posterior predictions...")
  posterior_mat <- posterior_predict(
    fit_model,
    newdata = test_pp,
    allow_new_levels = TRUE
  )
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2d. Extract effects ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  effects_mat <- fixef(fit_model)
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2e. Store iteration results ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  backtest_results[[key]] <- list(
    train     = train_pp,
    test      = test_pp,
    posterior = posterior_mat,
    effects   = effects_mat
  )
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2f. Update training set ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  train_set <- bind_rows(
    train_set,
    test_full |> select(game_id, season, week, home_team, away_team,
                        result, net_elo, net_rating, hfa_pre,
                        home_net_off_red_zone_app_perc_cum)
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Result: backtest_results list structure ----
#    $"S{YEAR}_W{WEEK}" -> list(train, test, posterior, effects)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Optionally save
# saveRDS(backtest_results, "backtest_results.rds")
# saveRDS(backtest_results, "backtest_results.rds")
