# Preprocessing script for Bayesian NFL modeling

library(readr)
library(tidytext)
library(MASS)
library(Metrics)
# library(tidyr)
# library(purrr)
library(plotly)
library(patchwork)
library(doParallel)
library(rBayesianOptimization)
library(xgboost)
library(caret)
library(cmdstanr)
library(rstanarm)
library(brms)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
library(broom.mixed)
library(tidybayes)
library(discrim)
library(bayesian)
library(tidymodels)
library(nflverse)
library(tidyverse)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
#load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/finalScoresData.rda"))

source(file = "./app/data-raw/gameData.R")
source(file = "./app/data-raw/eloData.R")
load(file = "./app/data/kfaData.rda")

eloData_update_list <- calc_elo_ratings(
  modData,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE
)
eloData2 <- eloData_update_list$elo_history

# Filter seasons and clean long-format
modDataBase <- modData |>
  left_join(
    finalScoresData |> 
      select(game_id, team, matches("^points\\d+$")) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id)),
    by = join_by(game_id, home_team)
  ) |>
  left_join(
    finalScoresData |> 
      select(game_id, team, matches("^points\\d+$")) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id)),
    by = join_by(game_id, away_team)
  ) |>
  # move all home_points* right after home_score
  relocate(matches("^home_points\\d+$"), .after = home_score) |>
  # then move all away_points* right after away_score
  relocate(matches("^away_points\\d+$"), .after = away_score) |>
  left_join(
    eloData2,
    by = join_by(game_id, season, week, gameday, away_team, away_score, home_team, home_score)
  ) |>
  left_join(
    kfaData$train,
    by = join_by(game_id, season, week, home_team, away_team, location)
  )
# left_join(
#   kfaData$test |> rename(home_rating_post = home_rating_pre,
#                          away_rating_post = away_rating_pre,
#                          hfa_post = hfa_pre),
#   by = join_by(game_id, season, week, home_team, away_team, location)
# )

modData <- modDataBase |> filter(season >= 2007) |>
  mutate(
    winner = case_when(
      home_team == winner ~ TRUE,
      away_team == winner ~ FALSE,
      TRUE ~ NA
    ),
    winner = factor(winner, levels = c(FALSE, TRUE), labels = c("Away", "Home"))
  ) 
# modData <- modData |> 
#   filter(!is.na(winner))

modDataLong <- modData |> clean_homeaway(invert = c("result", "spread_line"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. FEATURE ENGINEERING & PREPROCESSING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2.1 Define columns to drop
drop_vars <- c(
  "game_id","game_type","season_type","gameday","gametime",
  "home_team","away_team",
  "home_score", #"home_points8", "home_points7", "home_points6", "home_points3", "home_points2",
  "away_score", #"away_points8", "away_points7", "away_points6", "away_points3", "away_points2",
  "result","spread_line","spreadCover",
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
    net_elo_pre       = c("home_elo_pre", "away_elo_pre"),
    net_elo_post      = c("home_elo_post", "away_elo_post"),
    net_rating        = c("home_rating_pre", "away_rating_pre"),
    #net_rating_post   = c("home_rating_post", "away_rating_post"),
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
ordered_cols_flat

# 2.8 Final reordering: static first, then grouped metrics, then all remaining
feats_net_ordered_all <- feats_net |>
  select(
    "net_elo", "home_elo", "away_elo",
    "net_elo_pre", "home_elo_pre", "away_elo_pre",
    "net_elo_post", "home_elo_post", "away_elo_post",
    "net_rating", "home_rating_pre", "away_rating_pre", "hfa_pre",
    #"net_rating_post", "home_rating_post", "away_rating_post", "hfa_post",
    "net_SRS_cum", "home_SRS_cum", "away_SRS_cum",
    "home_net_OSRS_cum", "home_OSRS_cum", "away_DSRS_cum",
    "away_net_OSRS_cum", "away_OSRS_cum", "home_DSRS_cum",
    contains("MOV"),
    contains("SOS"),
    contains("PFG"),
    contains("PAG"),
    all_of(ordered_cols_flat),
    everything()
  ) |>
  mutate(
    net_rating_hfa = net_rating + hfa_pre,
    .after = net_rating
  )
colnames(feats_net_ordered_all)
# feats_net_ordered_all now has IDs and static/nets at the front, followed by grouped metrics

# 2.9 Merge with game data 
brms_data_base <- modData |>
  select(all_of(base_cols))
brms_data <- brms_data_base |>
  bind_cols(feats_net_ordered_all |> select(-any_of(names(brms_data_base))))

brms_data_complete <- brms_data %>% 
  select(-any_of(drop_vars), game_id) |>
  filter(if_all(where(is.numeric), ~ !is.na(.))) %>% 
  filter(if_all(where(is.numeric), ~ is.finite(.)))

incomplete_gameIDs <- setdiff(brms_data$game_id, brms_data_complete$game_id)

brms_data <- brms_data |> filter(!(game_id %in% incomplete_gameIDs))

distinct_weeks <- brms_data |>
  distinct(season, week) |> 
  arrange(season, week) |> 
  mutate(week_seq = row_number())

brms_data <- brms_data |>
  left_join(distinct_weeks) |>
  relocate(week_seq, .after = week)

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
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      net_rating +
      hfa_pre +
      #net_rating_hfa +
      home_net_off_red_zone_app_perc_cum
  ) + brmsfamily(family = "student", link = "identity")

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
brms_vars <- extract_predictors(brms_data, brms_formula_result)

# Preprocess
brms_data_prepped <- preprocess_data(brms_data,
                                     brms_vars, 
                                     corr_cutoff = 0.95,
                                     cor_method = "pearson",
                                     cor_use = "pairwise.complete.obs")

# Dropped vars
brms_vars_dropped <- brms_data_prepped$removed |>
  list_c() |>
  unique()

# Update Data
# brms_data_clean <- brms_data |>
#   select(all_of(base_cols)) |>
#   bind_cols(brms_data_prepped_winner$data |> select(-any_of(base_cols)))

brms_data_clean <- brms_data |>
  select(all_of(base_cols), 
         all_of(brms_vars), 
         -all_of(brms_vars_dropped)) |>
  mutate(
    result_fac = factor(result, levels = min(result):max(result), ordered = TRUE),
    .after = result
  )
colnames(brms_data_clean)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. UPDATE MODEL FORMULAS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Result
brms_formula_result <- 
  bf(
    result ~
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      net_rating +
      hfa_pre +
      #net_rating_hfa +
      home_net_off_red_zone_app_perc_cum
  ) + brmsfamily(family = "student", link = "identity")


# # Winner
# brms_formula_winner <- 
#   bf(
#     winner ~
#       net_elo +
#       #home_elo + away_elo +
#       net_SRS_cum +
#       #home_SRS_cum + away_SRS_cum +
#       home_win_pct_cum + away_win_pct_cum +
#       home_off_epa_sum_cum + home_def_epa_sum_cum +
#       away_off_epa_sum_cum + away_def_epa_sum_cum +
#       home_off_epa_sum_roll + home_def_epa_sum_roll +
#       away_off_epa_sum_roll + away_def_epa_sum_roll +
#       home_turnover_won_cum + home_turnover_lost_cum +
#       away_turnover_won_cum + away_turnover_lost_cum +
#       home_off_red_zone_app_perc_cum + home_def_red_zone_app_perc_cum +
#       home_off_red_zone_eff_cum + home_def_red_zone_eff_cum +
#       away_off_red_zone_app_perc_cum + away_def_red_zone_app_perc_cum +
#       away_off_red_zone_eff_cum + away_def_red_zone_eff_cum +
#       (1|home_team) +
#       (1|away_team)
#   ) + brmsfamily(family = "bernoulli")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. DATA SPLITTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 6.1 Create sequential week index ----
# Build a week_seq that increments with each unique season/week combination
distinct_weeks <- brms_data_clean |>
  distinct(season, week) |> 
  arrange(season, week) |> 
  mutate(week_seq = row_number())

# Merge week_seq into brms_data_clean
brms_data_clean <- brms_data_clean |> 
  left_join(distinct_weeks, by = c("season", "week")) |>
  relocate(week_seq, .after = week)

## 6.2 Train/Test Split ----
#   - Use seasons 2007–2021 for training/validation
#   - Hold out seasons 2022–2024 for final testing
train_data <- brms_data_clean |> filter(season <= 2021)
test_data  <- brms_data_clean |> filter(season >= 2022)

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


## 6.3.3 Three Season Roll week Folds ----
splits_tbl <- brms_data_clean |>
  distinct(season, week) |>
  filter(season >= 2010) |>
  arrange(season, week) |>
  mutate(
    # create a unique ID for each fold
    id = sprintf("S%04d_W%02d", season, week),
    # build the split object for each (season, week)
    split = map2(season, week, ~ {
      # analysis indices: prior 3 seasons OR same season & weeks < forecast week
      train_idx <- which(
        (brms_data_clean$season >= .x - 3 & brms_data_clean$season < .x) |
          (brms_data_clean$season == .x & brms_data_clean$week < .y)
      )
      # assessment indices: exactly this season & this week
      test_idx <- which(
        brms_data_clean$season == .x & brms_data_clean$week == .y
      )
      # build an rsplit
      make_splits(
        list(analysis   = train_idx,
             assessment = test_idx),
        data = brms_data_clean
      )
    })
  )

# assemble into an rsample object
nfl_backtest_splits <- manual_rset(
  splits = splits_tbl$split,
  ids    = splits_tbl$id
)

nfl_backtest_splits
nfl_backtest_splits$splits[[1]]
nfl_backtest_splits$splits[[23]]
tidy(nfl_backtest_splits)
class(nfl_backtest_splits)


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

# Pre-subset training data to only the outcome and predictors needed for result model
result_preds <- extract_predictors(brms_data_clean, brms_formula_result)
train_result <- train_data |> 
  select(
    game_id, result, 
    all_of(result_preds),
  )

test_result <- test_data |> 
  select(
    game_id, result, 
    all_of(result_preds),
  )

## 7.0 Model Formula ----
brms_formula_result <- 
  bf(
    result ~
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      net_rating +
      hfa_pre +
      #net_rating_hfa +
      home_net_off_red_zone_app_perc_cum
  ) + brmsfamily(family = "student", link = "identity")

## 7.1 Model Recipe ----
recipe_result <- brms_data_clean |>
  # mutate(
  #   result_fac = factor(result, levels = sort(unique(result)), ordered = TRUE)
  # ) |>
  recipe() |>
  # 1) mark the response
  update_role(result, result_fac, new_role = "outcome") |>
  # 2) mark your numeric inputs
  update_role(
    net_elo,
    net_rating,
    hfa_pre,
    home_net_off_red_zone_app_perc_cum,
    new_role = "predictor"
  ) |>
  # 3) mark your grouping factors for brms
  update_role(
    home_team,
    away_team,
    new_role = "group"
  ) |>
  update_role(
    game_id,
    new_role = "id"
  ) |>
  # 4) preprocessing *only* on those numeric predictors:
  # near-zero variance
  step_nzv(all_numeric_predictors()) |> 
  # remove linear combinations
  step_lincomb(all_numeric_predictors()) |> 
  # filter by correlation
  step_corr(all_numeric_predictors(), 
            threshold = 0.95) |>
  # center & scale
  step_normalize(all_numeric_predictors()) |>
  # Select only variables used
  step_select(
    has_role("id"),
    has_role("group"),
    has_role("outcome"),
    has_role("predictor")
  )
recipe_result

## 7.2 Model Specification ----
# Engine details
bayesian() |>
  set_engine("brms") |>
  translate()

# Model info
show_model_info("bayesian")

iters <- 2000
burn <- 1000
chains <- 4
sims <- (iters-burn)*chains

brms_result_spec <- bayesian(
  #mode = "regression",
  mode = "classification",
  engine = "brms",
  # formula = brms_formula_result,
  #family = brmsfamily(family = "student", link = "identity"),
  family = brmsfamily(family = "cumulative", link = "logit"),
  init = 0,
  chains = chains,
  iter = iters,
  warmup = burn,
  thin = 1,
  cores = parallel::detectCores(),
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all =TRUE),
  seed = 52
)
brms_result_spec |> translate()

## 7.3 Workflow ----
brms_workflow_result <- workflow() |>
  add_recipe(recipe_result) |>
  add_model(
    brms_result_spec,
    #formula = result ~ net_elo + net_rating + hfa_pre + home_net_off_red_zone_app_perc_cum,
    formula = result_fac ~ net_elo + net_rating + hfa_pre + home_net_off_red_zone_app_perc_cum
  )
brms_workflow_result


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 8. RESAMPLING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 8.1 Evaluation Metrics ----
brms_eval_metrics_reg_result <- metric_set(
  rmse, mae, mpe, mape, smape
)

brms_eval_metrics_class_result <- metric_set(
  accuracy, roc_auc, brier_class
)


# Execute resampling evaluation
system.time(
brms_resampling_reg_result <- brms_workflow_result |>
  fit_resamples(
    resamples = nfl_backtest_splits,
    metrics   = brms_eval_metrics_reg_result,
    control   = control_resamples(
      save_pred = TRUE,
      verbose   = TRUE
    )
  )
)

system.time(
  brms_resampling_class_result <- brms_workflow_result |>
    fit_resamples(
      resamples = nfl_backtest_splits,
      metrics   = brms_eval_metrics_class_result,
      control   = control_resamples(
        save_pred = TRUE,
        verbose   = TRUE
      )
    )
)

# Summarize per-fold metrics
brms_resampling_metrics_result <- collect_metrics(brms_resampling_class_result) |>
  mutate(model = "Bayesian")

# (Optional) Inspect out-of-fold predictions
brms_resampling_preds_result <- collect_predictions(brms_resampling_class_result) |>
  mutate(model = "Bayesian")
















