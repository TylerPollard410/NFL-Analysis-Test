# Preprocessing script for Bayesian NFL modeling

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
library(tidymodels)
library(nflverse)
library(tidyverse)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
#load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))

# Filter seasons and clean long-format
modData <- modData |> filter(season >= 2007) |>
  mutate(
    winner = case_when(
      home_team == winner ~ TRUE,
      away_team == winner ~ FALSE,
      TRUE ~ NA
    ),
    winner = factor(winner, levels = c(FALSE, TRUE), labels = c("Away", "Home"))
  ) |> 
  filter(!is.na(winner))
modDataLong <- modData |> clean_homeaway(invert = c("result", "spread_line"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. FEATURE ENGINEERING & PREPROCESSING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2.1 Define columns to drop
drop_vars <- c(
  "game_id","game_type","season_type","gameday","gametime",
  "home_team","away_team",
  "home_score","away_score","result","spread_line","spreadCover",
  "total","total_line","totalCover",
  "winner",
  "away_spread_odds","away_spread_prob",
  "home_spread_odds","home_spread_prob",
  "over_odds","over_prob","under_odds","under_prob",
  "away_moneyline","away_moneyline_prob",
  "home_moneyline","home_moneyline_prob",
  "overtime","stadium","home_coach","away_coach",
  "home_games_played","home_wins","home_losses","home_ties",
  "away_games_played","away_wins","away_losses","away_ties"
)

game_level_vars <- c(
  "season","week","weekday","time_of_day","location",
  "div_game","home_rest","away_rest","roof","surface","temp","wind"
)

base_cols <- colnames(modData)[colnames(modData) %in% c(drop_vars, game_level_vars)]

# 2.2 Subset raw feature columns
# Note: brms_data should be your modData wide-format dataframe
brms_data <- modData  # adjust if necessary

df_feats <- brms_data |>
  select(-all_of(c(drop_vars, game_level_vars)))

# 2.3 Net‐feature generator function
make_net_features <- function(df) {
  nm <- names(df)
  
  # -- Static nets (home minus away / counterpart)
  statics <- list(
    net_elo           = c("home_elo", "away_elo"),
    net_SRS_cum       = c("home_SRS_cum", "away_SRS_cum"),
    home_net_OSRS_cum = c("home_OSRS_cum", "away_DSRS_cum"),
    away_net_OSRS_cum = c("away_OSRS_cum", "home_DSRS_cum")
  )
  for (newnm in names(statics)) {
    c1 <- statics[[newnm]][1]
    c2 <- statics[[newnm]][2]
    if (all(c(c1, c2) %in% nm)) {
      df <- df |> mutate(!!newnm := .data[[c1]] - .data[[c2]])
    }
  }
  
  # -- Home offense vs away defense nets
  home_off <- nm[str_starts(nm, "home_off_")]
  away_def <- nm[str_starts(nm, "away_def_")]
  stems    <- intersect(
    str_remove(home_off, "^home_off_"),
    str_remove(away_def, "^away_def_")
  )
  for (stem in stems) {
    home_nm <- paste0("home_off_", stem)
    away_nm <- paste0("away_def_", stem)
    newnm   <- paste0("home_net_off_", stem)
    if (all(c(home_nm, away_nm) %in% nm)) {
      df <- df |> mutate(
        !!newnm := if (str_detect(stem, "epa")) {
          .data[[home_nm]] + .data[[away_nm]]
        } else {
          .data[[home_nm]] - .data[[away_nm]]
        }
      )
    }
  }
  
  # -- Away offense vs home defense nets
  away_off <- nm[str_starts(nm, "away_off_")]
  home_def <- nm[str_starts(nm, "home_def_")]
  stems    <- intersect(
    str_remove(away_off, "^away_off_"),
    str_remove(home_def, "^home_def_")
  )
  for (stem in stems) {
    away_nm <- paste0("away_off_", stem)
    home_nm <- paste0("home_def_", stem)
    newnm   <- paste0("away_net_off_", stem)
    if (all(c(away_nm, home_nm) %in% nm)) {
      df <- df |> mutate(
        !!newnm := if (str_detect(stem, "epa")) {
          .data[[away_nm]] + .data[[home_nm]]
        } else {
          .data[[away_nm]] - .data[[home_nm]]
        }
      )
    }
  }
  
  return(df)
}

# 2.4 Apply net-feature creation
feats_net <- make_net_features(df_feats)

# 2.5 Helper to order metric columns
get_ordered_metric_cols <- function(df, base) {
  variants <- c("cum", "roll", "ewma")
  roles    <- c("home_net_off", "home_off", "away_def", "away_net_off", "away_off", "home_def")
  purrr::map(variants, function(var) {
    purrr::map_chr(roles, ~ paste0(.x, "_", base, "_", var))
  }) |>
    unlist() |>
    intersect(names(df))
}

# 2.6 Dynamically extract all metric bases
metric_bases <- names(feats_net) |>
  str_subset("^home_net_off_.*_(cum|roll|ewma)$") |>
  str_remove("^home_net_off_") |>
  str_remove("_(cum|roll|ewma)$") |>
  unique()

metric_bases

# 2.7 Build ordered list for dynamic metrics
ordered_cols_list <- purrr::map(metric_bases, function(base) {
  get_ordered_metric_cols(feats_net, base)
})
ordered_cols_flat <- ordered_cols_list |> unlist() |> unique()

# 2.8 Final reordering: static first, then grouped metrics, then all remaining
feats_net_ordered_all <- feats_net |>
  select(
    "net_elo", "home_elo", "away_elo",
    "net_SRS_cum", "home_SRS_cum", "away_SRS_cum",
    "home_net_OSRS_cum", "home_OSRS_cum", "away_DSRS_cum",
    "away_net_OSRS_cum", "away_OSRS_cum", "home_DSRS_cum",
    contains("MOV"),
    contains("SOS"),
    contains("PFG"),
    contains("PAG"),
    all_of(ordered_cols_flat),
    everything()
  )
colnames(feats_net_ordered_all)
# feats_net_ordered_all now has IDs and static/nets at the front, followed by grouped metrics

# 2.9 Merge with game data 
# brms_data <- modData |>
#   select(1:44)  
# brms_data <- brms_data |>
#   bind_cols(feats_net_ordered_all |> select(-any_of(names(brms_data))))

brms_data <- modData |>
  select(all_of(base_cols))
brms_data <- brms_data |>
  bind_cols(feats_net_ordered_all |> select(-any_of(names(brms_data))))

brms_data_complete <- brms_data %>% 
  select(-any_of(drop_vars), game_id) |>
  filter(if_all(where(is.numeric), ~ !is.na(.))) %>% 
  filter(if_all(where(is.numeric), ~ is.finite(.)))

incomplete_gameIDs <- setdiff(brms_data$game_id, brms_data_complete$game_id)

brms_data <- brms_data |> filter(!(game_id %in% incomplete_gameIDs))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. INITIAL MODEL FORMULAS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Home Score
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
  ) + brmsfamily(family = "student")

# Away Score
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
  ) + brmsfamily(family = "student")

# Scores Joint
brms_formula_scores <- brms_formula_home + brms_formula_away
brms_formula_scores <- 
  bf(
    mvbind(home_score, away_score) ~
      net_elo +
      #home_elo + away_elo +
      net_SRS_cum +
      #home_SRS_cum + away_SRS_cum +
      home_off_epa_sum_cum + home_def_epa_sum_cum +
      away_off_epa_sum_cum + away_def_epa_sum_cum +
      home_off_epa_sum_roll + home_def_epa_sum_roll +
      away_off_epa_sum_roll + away_def_epa_sum_roll +
      home_turnover_won_cum + home_turnover_lost_cum +
      away_turnover_won_cum + away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + home_def_red_zone_eff_cum +
      away_off_red_zone_app_perc_cum + away_def_red_zone_app_perc_cum +
      away_off_red_zone_eff_cum + away_def_red_zone_eff_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "discrete_weibull", link = "logit")

# Result
brms_formula_result <- 
  bf(
    result ~
      net_elo +
      net_SRS +
      home_SRS_cum +
      away_SRS_cum +
      net_epa_cum +
      net_epa_roll +
      home_turnover_diff_cum + 
      away_turnover_diff_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "student")

# Total
brms_formula_total <- 
  bf(
    total ~ 
      # xgb_total + 
      # xgb_away_score +
      # xgb_home_score +
      net_elo +
      net_SRS +
      home_SRS_cum +
      away_SRS_cum +
      net_epa_cum +
      net_epa_roll +
      home_turnover_diff_cum + 
      away_turnover_diff_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "student")

# Winner
brms_formula_winner <- 
  bf(
    winner ~
      net_elo +
      #home_elo + away_elo +
      net_SRS_cum +
      #home_SRS_cum + away_SRS_cum +
      home_win_pct_cum + away_win_pct_cum +
      home_off_epa_sum_cum + home_def_epa_sum_cum +
      away_off_epa_sum_cum + away_def_epa_sum_cum +
      home_off_epa_sum_roll + home_def_epa_sum_roll +
      away_off_epa_sum_roll + away_def_epa_sum_roll +
      home_turnover_won_cum + home_turnover_lost_cum +
      away_turnover_won_cum + away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + home_def_red_zone_eff_cum +
      away_off_red_zone_app_perc_cum + away_def_red_zone_app_perc_cum +
      away_off_red_zone_eff_cum + away_def_red_zone_eff_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "bernoulli")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. PREPROCESSING STEPS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 4.1 Functions ----
# Identify predictors
extract_predictors <- function(data, brms_form){
  brms_terms <- brmsterms(brms_form)
  vars <- str_extract_all(deparse1(brms_terms$allvars), 
                          "\\b[[:alpha:]][[:alnum:]_]*\\b")[[1]]
  vars_unique <- unique(vars)
  vars_final <- intersect(colnames(data), vars_unique)
  return(vars_final)
}

# Run process steps
preprocess_data <- function(data, preds, steps = c("nzv", "lincomb", "corr"), 
                            corr_cutoff = 0.95, 
                            cor_method = "pearson",
                            cor_use = "pairwise.complete.obs") {
  # Subset only the predictor columns
  df <- data |> 
    select(all_of(preds)) |>
    select(where(is.numeric))
  
  removed <- list()
  
  # 5.1 Near-zero variance filtering
  if ("nzv" %in% steps) {
    nzv_idx <- nearZeroVar(df, names = TRUE)
    removed$nzv <- nzv_idx
    if (length(nzv_idx) > 0) df <- df |> select(-all_of(nzv_idx))
  }
  
  # 5.2 Linear combinations filtering
  if ("lincomb" %in% steps) {
    lin <- findLinearCombos(df)
    removed$lincomb <- if (!is.null(lin$remove)) names(df)[lin$remove] else character(0)
    if (!is.null(lin$remove)) df <- df |> select(-all_of(lin$remove))
  }
  
  # 5.3 High-correlation filtering
  if ("corr" %in% steps) {
    corr_mat <- cor(df, use = cor_use, method = cor_method)
    high_corr_idx <- findCorrelation(corr_mat, cutoff = corr_cutoff, names = TRUE)
    removed$corr <- high_corr_idx
    if (length(high_corr_idx) > 0) df <- df |> select(-all_of(high_corr_idx))
  }
  
  # Return a list of removed variables and the cleaned dataset
  list(
    removed = removed,
    data = df
  )
}

## 4.2 Preprocess ----
# Identify predictors for the winner model
brms_vars_winner <- extract_predictors(brms_data, brms_formula_winner)

# Preprocess
brms_data_prepped_winner <- preprocess_data(brms_data,
                                            brms_vars_winner, 
                                            corr_cutoff = 0.95,
                                            cor_method = "pearson",
                                            cor_use = "pairwise.complete.obs")

# Dropped vars
brms_vars_winner_dropped <- brms_data_prepped_winner$removed |>
  list_c() |>
  unique()

# Update Data
# brms_data_winner <- brms_data |>
#   select(all_of(base_cols)) |>
#   bind_cols(brms_data_prepped_winner$data |> select(-any_of(base_cols)))

brms_data_winner <- brms_data |>
  select(all_of(base_cols), 
         all_of(brms_vars_winner), 
         -all_of(brms_vars_winner_dropped))
colnames(brms_data_winner)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. UPDATE MODEL FORMULAS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Winner
brms_formula_winner <- 
  bf(
    winner ~
      net_elo +
      #home_elo + away_elo +
      net_SRS_cum +
      #home_SRS_cum + away_SRS_cum +
      home_win_pct_cum + away_win_pct_cum +
      home_off_epa_sum_cum + home_def_epa_sum_cum +
      away_off_epa_sum_cum + away_def_epa_sum_cum +
      home_off_epa_sum_roll + home_def_epa_sum_roll +
      away_off_epa_sum_roll + away_def_epa_sum_roll +
      home_turnover_won_cum + home_turnover_lost_cum +
      away_turnover_won_cum + away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + home_def_red_zone_eff_cum +
      away_off_red_zone_app_perc_cum + away_def_red_zone_app_perc_cum +
      away_off_red_zone_eff_cum + away_def_red_zone_eff_cum +
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "bernoulli")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. DATA SPLITTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 6.1 Create sequential week index ----
# Build a week_seq that increments with each unique season/week combination
distinct_weeks <- brms_data_winner |>
  distinct(season, week) |> 
  arrange(season, week) |> 
  mutate(week_seq = row_number())

# Merge week_seq into brms_data_winner
brms_data_winner <- brms_data_winner |> 
  left_join(distinct_weeks, by = c("season", "week")) |>
  relocate(week_seq, .after = week)

## 6.2 Train/Test Split ----
#   - Use seasons 2007–2021 for training/validation
#   - Hold out seasons 2022–2024 for final testing
train_data <- brms_data_winner |> filter(season <= 2021)
test_data  <- brms_data_winner |> filter(season >= 2022)

## 6.3 Define CV Time-Series Folds -----
## 6.3.1 Weekly Folds ----
## Define initial window of weeks (warm-up: Seasons 2007–2009)
initial_window <- distinct_weeks |> 
  filter(season %in% 2007:2009) |> 
  pull(week_seq) |> 
  max()

# Determine all weeks after warm-up
weeks <- sort(unique(train_data$week_seq))
fold_weeks <- weeks[weeks > initial_window]

### 6.3.1.1 Expanding ----
# Expanding window folds: train on all weeks <= w-1, test on week == w
expanding_splits_week <- manual_rset(
  splits = purrr::map(fold_weeks, function(w) {
    analysis_idx <- which(train_data$week_seq <= (w - 1))
    assessment_idx <- which(train_data$week_seq == w)
    rsample::make_splits(
      list(analysis = analysis_idx, assessment = assessment_idx),
      train_data
    )
  }),
  ids = paste0("week_", fold_weeks)
)
#expanding_splits_week

### 6.3.1.2 Rolling ----
# Rolling window folds: train on prior `initial_window` weeks, test on week == w
rolling_splits_week <- manual_rset(
  splits = purrr::map(fold_weeks, function(w) {
    analysis_idx <- which(
      train_data$week_seq > (w - initial_window - 1) &
        train_data$week_seq <= (w - 1)
    )
    assessment_idx <- which(train_data$week_seq == w)
    rsample::make_splits(
      list(analysis = analysis_idx, assessment = assessment_idx),
      train_data
    )
  }),
  ids = paste0("week_", fold_weeks)
)
#rolling_splits_week

## 6.3.2 Season Folds ----
# Determine seasons to fold (after warm-up seasons)
seasons <- sort(unique(train_data$season))
warmup_seasons <- 2007:2009
fold_seasons <- seasons[seasons > max(warmup_seasons)]

### 6.3.2.1 Expanding ----
# Expanding window by season: train on seasons <= s-1, test on season == s
expanding_splits_season <- manual_rset(
  splits = purrr::map(fold_seasons, function(s) {
    analysis_idx <- which(train_data$season <= (s - 1))
    assessment_idx <- which(train_data$season == s)
    rsample::make_splits(
      list(analysis = analysis_idx, assessment = assessment_idx),
      train_data
    )
  }),
  ids = paste0("season_", fold_seasons)
)
expanding_splits_season

### 6.3.2.2 Rolling ----
# Rolling window by season (fixed window of last 3 seasons):
rolling_window_seasons <- length(warmup_seasons)
rolling_splits_season <- manual_rset(
  splits = purrr::map(fold_seasons, function(s) {
    analysis_idx <- which(
      train_data$season > (s - rolling_window_seasons - 1) &
        train_data$season <= (s - 1)
    )
    assessment_idx <- which(train_data$season == s)
    rsample::make_splits(
      list(analysis = analysis_idx, assessment = assessment_idx),
      train_data
    )
  }),
  ids = paste0("season_", fold_seasons)
)
rolling_splits_season

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 7. RECIPE & WORKFLOWS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# In Tidymodels, a `recipe` defines preprocessing steps, and a `workflow` bundles
# the recipe with a model specification. When you `fit()` or `predict()`, the recipe
# is automatically `prep()`ped and `bake()`d on the appropriate data (analysis sets
# during resampling, or the full training set when fitting the final model).
# 
# We have created manual resampling objects (e.g., `expanding_splits_week`,
# `rolling_splits_week`, `expanding_splits_season`, `rolling_splits_season`) for CV.
# To evaluate via resampling, you could use `fit_resamples(workflow, resamples = ...)`
# but here we demonstrate fitting on the full `train_data` and then predicting on
# `test_data`. Note that predictions on `test_data` will automatically run through
# the same preprocessing (dummy encoding, normalization) defined in the recipe.

## 7.1 Winner Model ----
# Pre-subset training data to only the outcome and predictors needed for winner model
winner_preds <- extract_predictors(brms_data_winner, brms_formula_winner)
train_winner <- train_data |> 
  select(
    game_id, winner, 
    all_of(winner_preds),
  )

# Define recipe: encode categorical predictors and normalize numeric ones
winner_recipe <- recipe(winner ~ ., data = train_winner) |>  
  update_role(game_id, home_team, away_team, new_role = "id") |>                # preserve game_id column
  step_dummy(all_nominal_predictors(), -all_outcomes()) |> # one-hot encode factors
  step_normalize(all_numeric_predictors())                 # center and scale numerics


## 7.1.1 Model specifications ----
bayes_winner_spec <- logistic_reg(mode = "classification") |> 
  set_engine("stan")
xgb_winner_spec  <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  learn_rate = 0.01
) |>
  set_engine("xgboost") |> 
  set_mode("classification")

## 7.1.2 Build workflows ----
wf_bayes_winner <- workflow() |> 
  add_recipe(winner_recipe) |> 
  add_model(bayes_winner_spec)
wf_xgb_winner  <- workflow() |> 
  add_recipe(winner_recipe) |> 
  add_model(xgb_winner_spec)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 8. RESAMPLING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Use our manual week-level expanding splits to evaluate model performance via CV
# Metrics: accuracy, ROC AUC, and Brier score
eval_metrics <- metric_set(accuracy, roc_auc, brier_class)

## 8.1 Fit Cross-Validation  ----
### 8.1.1 Season Rolling Window ----
# Bayesian logistic CV
bayes_res_roll_season <- fit_resamples(
  wf_bayes_winner,
  resamples = rolling_splits_season,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

# XGBoost CV
xgb_res_roll_season <- fit_resamples(
  wf_xgb_winner,
  resamples = rolling_splits_season,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

### 8.1.2 Season Expanding Window ----
# Bayesian logistic CV
bayes_res_expanding_season <- fit_resamples(
  wf_bayes_winner,
  resamples = expanding_splits_season,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

# XGBoost CV
xgb_res_expanding_season <- fit_resamples(
  wf_xgb_winner,
  resamples = expanding_splits_season,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

### 8.1.3 Week Rolling Window ----
# Bayesian logistic CV
bayes_res_roll_week <- fit_resamples(
  wf_bayes_winner,
  resamples = rolling_splits_week,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

# XGBoost CV
xgb_res_roll_week <- fit_resamples(
  wf_xgb_winner,
  resamples = rolling_splits_week,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

### 8.1.4 Week Expanding Window ----
# Bayesian logistic CV
bayes_res_expanding_week <- fit_resamples(
  wf_bayes_winner,
  resamples = expanding_splits_week,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

# XGBoost CV
xgb_res_expanding_week <- fit_resamples(
  wf_xgb_winner,
  resamples = expanding_splits_week,
  metrics   = eval_metrics,
  control   = control_resamples(save_pred = TRUE)
)

## 8.2. CV-Fold Metrics ----
### 8.2.1 Collect ----
# Collect and compare CV metrics
bayes_cv_metrics_roll_season <- collect_metrics(bayes_res_roll_season) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Rolling", resamp_fold = "Season")
xgb_cv_metrics_roll_season   <- collect_metrics(xgb_res_roll_season) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Season")

bayes_cv_metrics_expanding_season <- collect_metrics(bayes_res_expanding_season) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Expanding", resamp_fold = "Season")
xgb_cv_metrics_expanding_season   <- collect_metrics(xgb_res_expanding_season) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Season")

bayes_cv_metrics_roll_week <- collect_metrics(bayes_res_roll_week) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Rolling", resamp_fold = "Week")
xgb_cv_metrics_roll_week   <- collect_metrics(xgb_res_roll_week) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Week")

bayes_cv_metrics_expanding_week <- collect_metrics(bayes_res_expanding_week) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Expanding", resamp_fold = "Week")
xgb_cv_metrics_expanding_week   <- collect_metrics(xgb_res_expanding_week) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Week")

### 8.2.2 Combine ----
cv_metrics_all   <- bind_rows(
  bayes_cv_metrics_roll_season, 
  xgb_cv_metrics_roll_season,
  bayes_cv_metrics_expanding_season, 
  xgb_cv_metrics_expanding_season,
  bayes_cv_metrics_roll_week, 
  xgb_cv_metrics_roll_week,
  bayes_cv_metrics_expanding_week, 
  xgb_cv_metrics_expanding_week
) |>
  mutate(model_full = paste(model, resamp_window, resamp_fold))
print(cv_metrics_all)
print(cv_metrics_all |> filter(str_detect(model, "Bayes")))
print(cv_metrics_all |> filter(str_detect(model, "XGBoost")))

### 8.2.3 Plot ----
ggplot(cv_metrics_all |> 
         filter(.metric %in% c("accuracy","roc_auc","brier_class")),
       aes(x = model_full, y = mean, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ .metric, scales = "free_y") +
  labs(title = "Cross-Validation Performance by Model",
       x = "Model", y = "Metric Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

## 8.3. CV-Prediction Metrics ----
### 8.3.1 Collect ----
# Collect and compare CV predictions
bayes_cv_predictions_roll_season <- collect_predictions(bayes_res_roll_season) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Rolling", resamp_fold = "Season")
xgb_cv_predictions_roll_season   <- collect_predictions(xgb_res_roll_season) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Season")

bayes_cv_predictions_expanding_season <- collect_predictions(bayes_res_expanding_season) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Expanding", resamp_fold = "Season")
xgb_cv_predictions_expanding_season   <- collect_predictions(xgb_res_expanding_season) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Season")

bayes_cv_predictions_roll_week <- collect_predictions(bayes_res_roll_week) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Rolling", resamp_fold = "Week")
xgb_cv_predictions_roll_week   <- collect_predictions(xgb_res_roll_week) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Week")

bayes_cv_predictions_expanding_week <- collect_predictions(bayes_res_expanding_week) |>
  mutate(model = "Bayesian Logistic", resamp_window = "Expanding", resamp_fold = "Week")
xgb_cv_predictions_expanding_week   <- collect_predictions(xgb_res_expanding_week) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Week")

### 8.3.2 Combine ----
# Combine into one predictions data frame
cv_predictions_all <- bind_rows(
  bayes_cv_predictions_roll_season, 
  xgb_cv_predictions_roll_season,
  bayes_cv_predictions_expanding_season,
  xgb_cv_predictions_expanding_season,
  bayes_cv_predictions_roll_week,
  xgb_cv_predictions_roll_week,
  bayes_cv_predictions_expanding_week,
  xgb_cv_predictions_expanding_week
)
cv_predictions_all <- train_winner |>
  mutate(.row = row_number()) |>
  select(.row, game_id) |>
  right_join(cv_predictions_all, by = join_by(.row)) |>
  left_join(brms_data_winner |> select(game_id, season, week, home_team, away_team),
            by = join_by(game_id))

cv_bayes_predictions <- cv_predictions_all |> 
  filter(str_detect(model, "Bayes"))
cv_xgb_predictions <- cv_predictions_all |> 
  filter(str_detect(model, "XGB"))

### 8.3.3 Group By ----
#### 8.3.3.1 Model ----
# Example: compute overall accuracy over all predictions
overall_cv_metrics <- cv_predictions_all |> 
  group_by(model, resamp_window, resamp_fold) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier_class = brier_class_vec(winner, .pred_Home),
    n = n()
  ) 
print(overall_cv_metrics)

#### 8.3.3.2 Season ----
# Example: compute season accuracy over all predictions
season_cv_metrics <- cv_predictions_all |> 
  group_by(model, resamp_window, resamp_fold, season) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(season_cv_metrics)

#### 8.3.3.3 Week ----
week_cv_metrics <- cv_predictions_all |> 
  group_by(model, resamp_window, resamp_fold, season) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(week_cv_metrics)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 9. FINAL MODELLING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Final fit on full training data & test predictions
# After inspecting CV results, refit on entire train_data and predict on test_data

## 9.1 Fit Models ----
bayes_winner_fit   <- fit(wf_bayes_winner, data = train_winner)
xgb_winner_fit    <- fit(wf_xgb_winner,  data = train_winner)

## 9.2 Predictions ----
# Predictions will undergo the same preprocessing steps defined in the recipe
# when calling `predict()`, ensuring test_data is transformed identically to train.
# Compare posteriors/implied probabilities vs. actual outcomes to evaluate test performance.
bayes_winner_preds <- bind_cols(
  test_data |> select(game_id, season, week, home_team, away_team, winner),
  predict(bayes_winner_fit, test_data, type = "class"),
  predict(bayes_winner_fit, test_data, type = "prob"),
  predict(bayes_winner_fit, test_data, type = "conf_int")
) |>
  mutate(model = "Bayesian Logistic", .before = 1)

xgb_winner_preds <- bind_cols(
  test_data |> select(game_id, season, week, home_team, away_team, winner),
  predict(xgb_winner_fit, test_data, type = "class"),
  predict(xgb_winner_fit, test_data, type = "prob")
)  |>
  mutate(model = "XGBoost", .before = 1)

winner_scores <- test_data |>
  select(game_id, home_score, away_score, result, total)

winner_preds <- bind_rows(
  left_join(bayes_winner_preds, winner_scores, by = "game_id") |>
    select(-contains("lower"), -contains("upper")),
  left_join(xgb_winner_preds, winner_scores, by = "game_id")
)
winner_preds

## 9.3 Performance ----
# Compute performance metrics with vector functions

### 9.3.1 Model ----
winner_test_metrics_model <- winner_preds |>
  group_by(model) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(winner_test_metrics_model, n = nrow(winner_test_metrics_model))

### 9.3.2 Season ----
winner_test_metrics_season <- winner_preds |>
  group_by(model, season) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(winner_test_metrics_season, n = nrow(winner_test_metrics_season))

### 9.3.3 Week ----
winner_test_metrics_week <- winner_preds |>
  group_by(model, week) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(winner_test_metrics_week, n = nrow(winner_test_metrics_week))

### 9.3.4 Season & Week ----
winner_test_metrics_season_week <- winner_preds |>
  group_by(model, season, week) |>
  summarise(
    accuracy = accuracy_vec(winner, .pred_class),
    roc_auc = roc_auc_vec(winner, .pred_Home, event_level = "second"),
    brier = brier_class_vec(winner, .pred_Home),
    .groups = "drop"
  )
print(winner_test_metrics_season_week, n = nrow(winner_test_metrics_season_week))

## 9.4 ROC Curve ----
# Use prediction tibbles: bayes_winner_preds and xgb_winner_preds
roc_bayes <- roc_curve(bayes_winner_preds, truth = winner, .pred_Home) |> 
  mutate(model = "Bayesian Hierarchical")
roc_xgb   <- roc_curve(xgb_winner_preds,   truth = winner, .pred_Home) |> 
  mutate(model = "XGBoost")

winner_roc_curve <- winner_preds |>
  group_by(model) |>
  roc_curve(truth = winner, .pred_Home, event_level = "second")

winner_roc_auc <- winner_preds |>
  group_by(model) |>
  roc_auc(truth = winner, .pred_Home, event_level = "second")

roc_plot <- winner_roc_curve |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Test Set ROC Curves",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
print(roc_plot)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 10. BETTING PERFORMANCE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

winner_betting_preds <- winner_preds |>
  left_join(
    test_data |> select(game_id, contains("moneyline")),
    by = join_by(game_id)
  ) |>
  relocate(contains("moneyline"), .after = .pred_Home)

winner_betting_return <- winner_betting_preds |>
  mutate(
    pred_bet_team = .pred_class,
    pred_bet_correct = winner == .pred_class,
    exp_pay = case_when(
      pred_bet_team == "Home" ~ ifelse(home_moneyline > 0, 
                                     home_moneyline, 
                                     100/abs(home_moneyline)*100),
      pred_bet_team == "Away" ~ ifelse(away_moneyline > 0, 
                                     away_moneyline, 
                                     100/abs(away_moneyline)*100),
      TRUE ~ NA_real_
    ),
    actual_pay = ifelse(pred_bet_correct, exp_pay, -100)
  ) |>
  mutate(
    odds_home_value = ifelse(.pred_class == "Home",
                             .pred_Home - home_moneyline_prob,
                             home_moneyline_prob - .pred_Home),
    odds_away_value = ifelse(.pred_class == "Away",
                             .pred_Away - away_moneyline_prob,
                             away_moneyline_prob - .pred_Away),
  )


bets <- winner_betting_preds |>      # or xgb_winner_preds
  mutate(
    # 1) Implied win‐prob from the moneyline
    imp_Home = if_else(home_moneyline > 0,
                       100 / (home_moneyline + 100),
                       -home_moneyline / (-home_moneyline + 100)),
    imp_Away = if_else(away_moneyline > 0,
                       100 / (away_moneyline + 100),
                       -away_moneyline / (-away_moneyline + 100)),
    # 2) Decimal odds (payout per $1 stake)
    dec_Home = if_else(home_moneyline > 0,
                       home_moneyline/100 + 1,
                       100/abs(home_moneyline) + 1),
    dec_Away = if_else(away_moneyline > 0,
                       away_moneyline/100 + 1,
                       100/abs(away_moneyline) + 1),
    # 3) Edge and EV
    edge_Home = .pred_Home - imp_Home,
    edge_Away = .pred_Away - imp_Away,
    EV_Home   = .pred_Home * (dec_Home - 1) - (1 - .pred_Home),
    EV_Away   = .pred_Away * (dec_Away - 1) - (1 - .pred_Away),
    # 4) Kelly fraction: f* = (b p - q) / b,  where b = dec - 1, q = 1-p
    kelly_Home = ((dec_Home - 1) * .pred_Home - (1 - .pred_Home)) / (dec_Home - 1),
    kelly_Away = ((dec_Away - 1) * .pred_Away - (1 - .pred_Away)) / (dec_Away - 1),
    # 5) Pick the best “positive‐EV” bet (or no bet)
    best_ev = pmax(EV_Home, EV_Away),
    bet_side = case_when(
      EV_Home > EV_Away & EV_Home > 0 ~ "Home",
      EV_Away > EV_Home & EV_Away > 0 ~ "Away",
      TRUE                            ~ "No Bet"
    ),
    bet_size = case_when(
      bet_side == "Home" ~ kelly_Home,
      bet_side == "Away" ~ kelly_Away,
      TRUE               ~ 0
    )
  )

# 7.5 Betting Strategy: Calculate Actual Returns and Summaries ----
# Using Bayesian model predictions and moneyline odds, compute implied probabilities,
# decide bets where expected value is positive, and calculate actual returns.
bets2 <- winner_betting_preds |> 
  mutate(
    # Market-implied probabilities
    imp_Home = if_else(home_moneyline > 0,
                       100/(home_moneyline + 100),
                       -home_moneyline/(-home_moneyline + 100)),
    imp_Away = if_else(away_moneyline > 0,
                       100/(away_moneyline + 100),
                       -away_moneyline/(-away_moneyline + 100)),
    # Decimal odds
    dec_Home = if_else(home_moneyline > 0,
                       home_moneyline/100 + 1,
                       100/abs(home_moneyline) + 1),
    dec_Away = if_else(away_moneyline > 0,
                       away_moneyline/100 + 1,
                       100/abs(away_moneyline) + 1),
    # Expected value for $1 bet
    EV_Home = .pred_Home * (dec_Home - 1) - (1 - .pred_Home),
    EV_Away = .pred_Away * (dec_Away - 1) - (1 - .pred_Away),
    # Choose side if positive EV
    bet_side = case_when(
      EV_Home > EV_Away & EV_Home > 0 ~ "Home",
      EV_Away > EV_Home & EV_Away > 0 ~ "Away",
      TRUE                            ~ "No Bet"
    ),
    stake = 1,
    # Actual return
    actual_return = case_when(
      bet_side == "Home" & winner == "Home" ~ stake * (dec_Home - 1),
      bet_side == "Away" & winner == "Away" ~ stake * (dec_Away - 1),
      bet_side %in% c("Home", "Away") & bet_side != winner ~ -stake,
      TRUE ~ 0
    )
  )

# Summaries
return_all <- winner_betting_return |>
  group_by(model) |>
  summarise(
    return = sum(actual_pay),
    bets = n(),
    .groups = "drop"
  )

all_return_summary <- bets2 |> 
  filter(bet_side != "No Bet") |>
  group_by(model) |>
  summarise(
    total_return = sum(actual_return),
    n_bets       = n(),
    avg_return   = mean(actual_return),
    ROI          = total_return / n_bets
  )

return_season <- winner_betting_return |>
  group_by(model, season) |>
  summarise(
    return = sum(actual_pay),
    bets = n(),
    .groups = "drop"
  )

by_season <- bets2 |> 
  filter(bet_side != "No Bet") |>
  group_by(model, season) |>
  summarise(
    total_return = sum(actual_return),
    n_bets       = n(),
    ROI          = total_return / n_bets,
    .groups = "drop"
  )

return_week <- winner_betting_return |>
  group_by(model, week) |>
  summarise(
    return = sum(actual_pay),
    bets = n(),
    .groups = "drop"
  )

by_week <- bets2 |> 
  filter(bet_side != "No Bet") |>
  group_by(model, week) |>
  summarise(
    total_return = sum(actual_return),
    n_bets       = n(),
    ROI          = total_return / n_bets,
    .groups = "drop"
  )

return_season_week <- winner_betting_return |>
  group_by(model, season, week) |>
  summarise(
    return = sum(actual_pay),
    bets = n(),
    .groups = "drop"
  )

by_season_week <- bets2 |> 
  filter(bet_side != "No Bet") |>
  group_by(model, season, week) |>
  summarise(
    total_return = sum(actual_return),
    n_bets       = n(),
    ROI          = total_return / n_bets,
    .groups = "drop"
  )

return_team_side <- winner_betting_return |>
  group_by(model, pred_bet_team) |>
  summarise(
    return = sum(actual_pay),
    bets = n(),
    .groups = "drop"
  )

by_team_side <- bets2 |> 
  filter(bet_side != "No Bet") |>
  group_by(model, bet_side) |>
  summarise(
    total_return = sum(actual_return),
    n_bets       = n(),
    ROI          = total_return / n_bets,
    .groups = "drop"
  )

# Display summaries
print(all_return_summary, n = nrow(all_return_summary))
print(return_all, n = nrow(return_all))

print(by_season, n = nrow(by_season))
print(return_season, n = nrow(return_season))

print(by_week, n = nrow(by_week))
print(return_week, n = nrow(return_week))

print(by_season_week, n = nrow(by_season_week))
print(return_season_week, n = nrow(return_season_week))

print(by_team_side, n = nrow(by_team_side))
print(return_team_side, n = nrow(return_team_side))


return_all_comb <- left_join(all_return_summary, return_all,
                             by = join_by(model))
return_season_comb <- left_join(by_season, return_season,
                             by = join_by(model, season))
return_week_comb <- left_join(by_week, return_week,
                             by = join_by(model, week))
return_season_week_comb <- left_join(by_season_week, return_season_week,
                             by = join_by(model, season, week))
return_team_side_comb <- left_join(by_team_side, return_team_side,
                             by = join_by(model, bet_side == pred_bet_team))

print(return_all_comb, n = nrow(return_all_comb))
print(return_season_comb, n = nrow(return_season_comb))
print(return_week_comb, n = nrow(return_week_comb))
print(return_season_week_comb, n = nrow(return_season_week_comb))
print(return_team_side_comb, n = nrow(return_team_side_comb))



