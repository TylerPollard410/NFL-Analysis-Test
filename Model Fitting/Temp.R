
# Libraries ----
library(tidytext)
library(MASS)
library(Metrics)
# library(tidyr)
# library(purrr)
library(plotly)
library(patchwork)
#library(doParallel)
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
library(timetk)
library(modeltime)
library(tidymodels)
library(vip)

library(nflverse)
library(tidyverse)

library(nflendzonePipeline)
library(nflendzone)

set.seed(52)
getOption("future.globals.maxSize")
options(future.globals.maxSize = 8 * 1024^3)

#library(future)
#plan(multisession, workers = 2)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Global Variables ----
all_seasons <- 2006:get_current_season()
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
github_data_repo <- "TylerPollard410/nflendzoneData"

## nflverse ----
teams_data <- load_teams(current = TRUE)

### games ----
game_data <- load_game_data()
game_data_long <- game_data |> clean_homeaway(invert = c("result", "spread_line"))

game_id_keys <- game_data |> select(
  game_id, season, game_type, season_type, week, home_team, away_team, location
)
game_long_id_keys <- game_data_long |> select(
  game_id, season, game_type, season_type, week, team, opponent, location
)

### stats ----
#### week_team_regpost ----
# nfl_stats_week_team_regpost <- calculate_stats(
#   seasons = all_seasons,
#   summary_level = "week",
#   stat_type = "team",
#   season_type = "REG+POST"
# )
# 
# piggyback::pb_new_release(repo = github_data_repo, tag = "nfl_stats_week_team_regpost")
# save_and_upload(
#   tag         = "nfl_stats_week_team_regpost",
#   full_data   = nfl_stats_week_team_regpost,
#   seasons     = all_seasons,
#   repo        = github_data_repo,
#   archive_dir = file.path("~/Desktop", "nfl_stats_week_team_regpost"),
#   upload = TRUE
# )

tag <- "nfl_stats_week_team_regpost"
nfl_stats_week_team_regpost <- rds_from_url(paste0(base_repo_url, 
                                                   tag, "/",
                                                   tag, ".rds"))

#### week_player_regpost ----
# system.time(
#   progressr::with_progress({
#     nfl_stats_week_player_regpost <- calculate_stats(
#       seasons = all_seasons,
#       summary_level = "week",
#       stat_type = "player",
#       season_type = "REG+POST"
#     )
#   })
# )
#
# piggyback::pb_new_release(repo = github_data_repo, tag = "nfl_stats_week_player_regpost")
# save_and_upload(
#   tag         = "nfl_stats_week_player_regpost",
#   full_data   = nfl_stats_week_player_regpost,
#   seasons     = all_seasons,
#   repo        = github_data_repo,
#   archive_dir = file.path("~/Desktop", "nfl_stats_week_player_regpost"),
#   upload = TRUE
# )
# 
# nfl_stats_week_player_offense <- load_player_stats(
#   seasons = all_seasons,
#   stat_type = "offense"
# )
# nfl_stats_week_player_defense <- load_player_stats(
#   seasons = all_seasons,
#   stat_type = "defense"
# )
# nfl_stats_week_player_kicking <- load_player_stats(
#   seasons = all_seasons,
#   stat_type = "kicking"
# )

tag <- "nfl_stats_week_player_regpost"
nfl_stats_week_player_regpost <- rds_from_url(paste0(base_repo_url, 
                                                     tag, "/",
                                                     tag, ".rds"))



## nflendzoneData ----
tag <- "feature_long"
team_features_data <- rds_from_url(paste0(base_repo_url, 
                                          tag, "/",
                                          tag, ".rds"))
#glimpse(team_features_data)

team_features_data <- team_features_data |>
  select(-contains("opponent"), -team_score) |>
  mutate(
    team_elo_update = team_elo_post - team_elo_pre,
    .after = team_elo_post
  ) |>
  rename(elo_pre = team_elo_pre) |>
  rename(elo_post = team_elo_post) |>
  rename(elo_update = team_elo_update)

game_features_data <- game_id_keys |>
  left_join(
    team_features_data |>
      select(-location) |>
      rename_with(~paste0("home_", .x), 
                  .cols = -c(game_id, season, week, team, gameday)),
    by = join_by(game_id, season, week, home_team == team)
  ) |>
  left_join(
    team_features_data |>
      select(-location) |>
      rename_with(~paste0("away_", .x), 
                  .cols = -c(game_id, season, week, team, gameday)),
    by = join_by(game_id, season, week, away_team == team, gameday)
  )
#glimpse(game_features_data)

## model_data ----
### team ----
team_model_data <- game_long_id_keys |> filter(!(season == 2025 & week > 1)) |>
  left_join(team_features_data) |>
  mutate(
    elo_pre = case_when(
      week == 1 ~ lag(elo_post, n = 1)*0.6 + 1500*0.4,
      week != 1 ~ elo_pre,
      TRUE ~ NA_real_
    ),
    #.after = elo_pre,
    .by = team
  ) |>
  mutate(
    elo_update_roll_5 = slider::slide_dbl(elo_update,
                                          mean,
                                          .before = 5 - 1,
                                          .complete = FALSE),
    elo_update_roll_5 = lag(elo_update_roll_5, n = 1),
    .after = elo_update,
    .by = team
  ) |>
  mutate(
    pfg = pf/games,
    pag = pa/games,
    .after = pa
  ) |>
  mutate(
    across(all_of(matches("^(pf|pfg|pa|pag|win_pct|MOV|SOS|SRS|OSRS|DSRS)($|_|[0-9])")),
           ~lag(.x, n = 1, default = NA)),
    .by = team
  ) |>
  (\(.) {
    after_cols <- names(.)[(which(names(.) == "DSRS_20") + 1):ncol(.)]
    reduce(
      after_cols,
      \(df, col) {
        df |>
          mutate(
            "{col}_cummean" := cummean(.data[[col]]),
            "{col}_roll_5"   := slider::slide_dbl(.data[[col]], 
                                                  mean, 
                                                  .before = 5 - 1, 
                                                  .complete = FALSE),
            "{col}_cummean" := lag(.data[[paste0(col, '_cummean')]], n = 1),
            "{col}_roll_5"   := lag(.data[[paste0(col, '_roll_5')]], n = 1),
            .after = col,
            .by = team
          ) |>
          select(-all_of(col))
      },
      .init = .
    )
  })()

#colnames(team_model_data)

### game ----
game_model_data <- game_id_keys |> 
  left_join(
    game_data |> select(game_id, home_team, away_team,
                        home_score, away_score, 
                        result, spread_line,
                        total, total_line)
  ) |>
  left_join(
    team_model_data |>
      select(-c(opponent, location, game_type, season_type, gameday)) |>
      rename_with(~paste0("home_", .x), 
                  .cols = -c(game_id, season, week, team)),
    by = join_by(game_id, season, week, home_team == team)
  ) |>
  left_join(
    team_model_data |>
      select(-c(opponent, location, game_type, season_type, gameday)) |>
      rename_with(~paste0("away_", .x), 
                  .cols = -c(game_id, season, week, team)),
    by = join_by(game_id, season, week, away_team == team)
  ) # |>
# (\(.) {
#   main_cols <- names(.)[1:which(names(.) == "total_line")]
#   post_cols <- names(.)[(which(names(.) == "total_line") + 1):ncol(.)]
#   feature_names <- post_cols |>
#     str_remove("^home_") |>
#     str_remove("^away_") |>
#     unique()
#   interleaved <- map(feature_names, \(f) c(paste0("home_", f), paste0("away_", f))) |> flatten_chr()
#   interleaved <- interleaved[interleaved %in% names(.)]
#   final_col_order <- c(main_cols, interleaved)
#   select(., all_of(final_col_order))
# })()


#colnames(game_model_data)

add_flexible_net_features <- function(
    df,
    var1_prefix = "home_",
    var2_prefix = "away_",
    pattern1 = ".*",
    pattern2 = ".*",
    fun = `-`,
    net_prefix = "net_"
) {
  regex1 <- paste0("^", var1_prefix, "(", pattern1, ")(?=_|$|\\b)")
  regex2 <- paste0("^", var2_prefix, "(", pattern2, ")(?=_|$|\\b)")
  var1_idx <- str_which(names(df), regex1)
  var2_idx <- str_which(names(df), regex2)
  var1_names <- names(df)[var1_idx]
  var2_names <- names(df)[var2_idx]
  
  # Must be same length and matching order!
  if (length(var1_names) == 0 || length(var2_names) == 0) return(df)
  if (length(var1_names) != length(var2_names)) {
    stop("home/away columns found do not match 1:1. Check your regex patterns.")
  }
  
  net_names <- str_replace(var1_names, paste0("^", var1_prefix), net_prefix)
  
  net_df <- map2_dfc(var1_names, var2_names, ~tibble(!!sym(str_replace(.x, paste0("^", var1_prefix), net_prefix)) := fun(df[[.x]], df[[.y]])))
  
  df2 <- bind_cols(df, net_df)
  
  # # Relocate net col after its away col
  # for (j in seq_along(net_names)) {
  #   df2 <- df2 |> relocate(all_of(net_names[j]), .after = all_of(var2_names[j]))
  # }
  df2
}

# Set up your configs
net_configs <- list(
  list(
    var1_prefix = "home_", var2_prefix = "away_",
    pattern1 = "elo|MOV|SOS|SRS", pattern2 = "elo|MOV|SOS|SRS",
    fun = `-`
  ),
  list(
    var1_prefix = "home_", var2_prefix = "away_",
    pattern1 = "pfg|OSRS", pattern2 = "pag|DSRS",
    fun = `-`
  ),
  list(
    var1_prefix = "home_", var2_prefix = "away_",
    pattern1 = "pag|DSRS", pattern2 = "pfg|OSRS",
    fun = `-`
  ),
  list(
    var1_prefix = "home_", var2_prefix = "away_",
    pattern1 = "(?=.*off)(?=.*epa).*", pattern2 = "(?=.*def)(?=.*epa).*",
    fun = `+`
  ),
  list(
    var1_prefix = "home_", var2_prefix = "away_",
    pattern1 = "(?=.*off)(?=.*red_zone).*", pattern2 = "(?=.*def)(?=.*red_zone).*",
    fun = `+`
  )
)

game_model_data <- reduce(
  net_configs,
  \(df, cfg) add_flexible_net_features(
    df,
    var1_prefix = cfg$var1_prefix,
    var2_prefix = cfg$var2_prefix,
    pattern1 = cfg$pattern1,
    pattern2 = cfg$pattern2,
    fun      = cfg$fun
  ),
  .init = game_model_data
)


game_model_data <- game_model_data |> 
  left_join(game_data) |>
  mutate(
    temp = ifelse(is.na(temp), 68, temp),
    wind = ifelse(is.na(wind), 0, wind)
  ) |>
  filter(season > 2006) |>
  select(-week_seq) |>
  add_week_seq()


tag <- "game_model"
game_model_data <- rds_from_url(paste0(base_repo_url, 
                                       tag, "/",
                                       tag, ".rds"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ---- 
# MODELING -----

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. DATA SPLITTING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 6.1 Create sequential week index ----
# Build a week_seq that increments with each unique season/week combination
# distinct_weeks <- game_model_data |>
#   distinct(season, week) |>
#   arrange(season, week) |>
#   mutate(week_seq = row_number())

# Merge week_seq into brms_data_winner
# brms_data_winner <- brms_data_winner |> 
#   left_join(distinct_weeks, by = c("season", "week")) |>
#   relocate(week_seq, .after = week)

## 6.2 Train/Test Split ----
#   - Use seasons 2007–2021 for training/validation
#   - Hold out seasons 2022–2024 for final testing
train_data <- game_model_data |> filter(season <= 2021)
test_data  <- game_model_data |> filter(season >= 2022)

## 6.3 Define CV Time-Series Folds -----
## 6.3.1 Weekly Folds ----
## Define initial window of weeks (warm-up: Seasons 2007–2009)
initial_window <- game_model_data |> 
  filter(season %in% 2007:2009) |> 
  pull(week_seq) |> 
  max()

# Determine all weeks after warm-up
weeks <- sort(unique(train_data$week_seq))
fold_weeks <- weeks[weeks > initial_window]

### 6.3.1.1 Expanding ----
# Expanding window folds: train on all weeks <= w-1, test on week == w
# expanding_splits_week <- manual_rset(
#   splits = purrr::map(fold_weeks, function(w) {
#     analysis_idx <- which(train_data$week_seq <= (w - 1))
#     assessment_idx <- which(train_data$week_seq == w)
#     rsample::make_splits(
#       list(analysis = analysis_idx, assessment = assessment_idx),
#       train_data
#     )
#   }),
#   ids = paste0("week_", fold_weeks)
# )
#expanding_splits_week

### 6.3.1.2 Rolling ----
# Rolling window folds: train on prior `initial_window` weeks, test on week == w
# rolling_splits_week <- manual_rset(
#   splits = purrr::map(fold_weeks, function(w) {
#     analysis_idx <- which(
#       train_data$week_seq > (w - initial_window - 1) &
#         train_data$week_seq <= (w - 1)
#     )
#     assessment_idx <- which(train_data$week_seq == w)
#     rsample::make_splits(
#       list(analysis = analysis_idx, assessment = assessment_idx),
#       train_data
#     )
#   }),
#   ids = paste0("week_", fold_weeks)
# )
#rolling_splits_week

## 6.3.2 Season Folds ----
# Determine seasons to fold (after warm-up seasons)
seasons <- sort(unique(train_data$season))
warmup_seasons <- 2007:2009
fold_seasons <- seasons[seasons > max(warmup_seasons)]

### 6.3.2.1 Expanding ----
# Expanding window by season: train on seasons <= s-1, test on season == s
# expanding_splits_season <- manual_rset(
#   splits = purrr::map(fold_seasons, function(s) {
#     analysis_idx <- which(train_data$season <= (s - 1))
#     assessment_idx <- which(train_data$season == s)
#     rsample::make_splits(
#       list(analysis = analysis_idx, assessment = assessment_idx),
#       train_data
#     )
#   }),
#   ids = paste0("season_", fold_seasons)
# )
#expanding_splits_season

### 6.3.2.2 Rolling ----
# Rolling window by season (fixed window of last 3 seasons):
# rolling_window_seasons <- length(warmup_seasons)
# rolling_splits_season <- manual_rset(
#   splits = purrr::map(fold_seasons, function(s) {
#     analysis_idx <- which(
#       train_data$season > (s - rolling_window_seasons - 1) &
#         train_data$season <= (s - 1)
#     )
#     assessment_idx <- which(train_data$season == s)
#     rsample::make_splits(
#       list(analysis = analysis_idx, assessment = assessment_idx),
#       train_data
#     )
#   }),
#   ids = paste0("season_", fold_seasons)
# )
# rolling_splits_season

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

## 7.1 Result Model ----
# Pre-subset training data to only the outcome and predictors needed for winner model
glimpse(game_model_data)
result_preds <- game_model_data |>
  select(
    season, week, location, 
    away_rest, home_rest, 
    div_game, 
    surface, roof, temp, wind,
    home_win_pct, away_win_pct,
    contains("net")
  ) |>
  select(-any_of(c("net_elo_post", "net_elo_update"))) |>
  colnames() 

# Define recipe: encode categorical predictors and normalize numeric ones
result_recipe <- recipe(train_data) |>  
  update_role(result, 
              new_role = "outcome") |>
  update_role(any_of(!!result_preds), 
              new_role = "predictor") |>
  update_role(game_id, home_team, away_team, 
              new_role = "id") |>   # preserve game_id column
  update_role(home_score, away_score, total, spread_line, total_line,
              new_role = "betting") |>
  step_novel(surface) |>
  step_dummy(all_nominal_predictors()) |> # one-hot encode factors
  step_lincomb(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors(), -season, -week) |>  # center and scale numerics
  step_rm(-has_role("outcome"),
          -has_role("predictor"),
          -has_role("id"),
          -has_role("betting"))
result_recipe

## 7.1.1 Model specifications ----
#cores <- parallel::detectCores()

### 7.1.1.1 Bayes ----
bayes_result_spec <- linear_reg() |> 
  set_engine("stan") |> 
  set_mode("regression")

### 7.1.1.2 XGBoost ----
#show_model_info("boost_tree")
xgb_result_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  mtry = tune(),
  min_n = tune(),
  loss_reduction = tune()
  #sample_prop = tune()         # <--- not sample_size!
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

# boost_tree() |> 
#   set_engine("xgboost") |> 
#   set_mode("regression") |>
#   translate()

## 7.1.2 Tune Grid ----
xgb_param_grid <- grid_space_filling(
  trees(range = c(200, 1200)),
  tree_depth(range = c(2, 8)),
  learn_rate(range = c(0.01, 0.3)),
  mtry(range = c(3, 129)),
  min_n(range = c(2, 12)),
  loss_reduction(range = c(0, 10)),
  size = 20
)


## 7.1.3 Build workflows ----
wf_bayes_result <- workflow() |> 
  add_recipe(result_recipe) |> 
  add_model(bayes_result_spec)
wf_xgb_result  <- workflow() |> 
  add_recipe(result_recipe) |> 
  add_model(xgb_result_spec)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 8. RESAMPLING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Use our manual week-level expanding splits to evaluate model performance via CV
# Metrics: accuracy, ROC AUC, and Brier score
eval_metrics <- metric_set(rmse, mae)

# library(future)
# plan(multisession, workers = 2)

## 8.0 Tune XGBoost Model Rolling Week -----
# Tune on your primary plan (e.g., rolling season)
### 8.0.1 Tune Splits ----
# rolling_splits_week <- manual_rset(
#   splits = purrr::map(fold_weeks, function(w) {
#     analysis_idx <- which(
#       train_data$week_seq > (w - initial_window - 1) &
#         train_data$week_seq <= (w - 1)
#     )
#     assessment_idx <- which(train_data$week_seq == w)
#     rsample::make_splits(
#       list(analysis = analysis_idx, assessment = assessment_idx),
#       train_data
#     )
#   }),
#   ids = paste0("week_", fold_weeks)
# )
#rolling_splits_week

# Check RAM
as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1024^3
sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE)
#object.size("rolling_splits_week") / 1024^3

### 8.0.2 Run Grid ----
system.time(
  tuned_xgb <- tune_grid(
    wf_xgb_result,
    resamples = rolling_splits_week,
    grid = xgb_param_grid,
    metrics = eval_metrics,
    control = control_grid(verbose = TRUE)
  )
)
tuned_xgb

### 8.0.3 Best Tune ----
xgb_show_best_rmse <- show_best(tuned_xgb, metric = "rmse", n = 20)
xgb_show_best_mae <- show_best(tuned_xgb, metric = "mae", n = 20)

# Get best params and finalize workflow
best_params <- select_best(tuned_xgb, metric = "rmse")
final_xgb_wf <- finalize_workflow(wf_xgb_result, best_params)

# save(best_params, final_xgb_wf, xgb_show_best_rmse, xgb_show_best_mae,
#      file = "~/Desktop/xgb_rolling_week_tune_results.rda")


## 8.1 Fit Cross-Validation  ----
load(file = "~/Desktop/xgb_rolling_week_tune_results.rda")

xgb_show_best_rmse
xgb_show_best_mae
best_params
final_xgb_wf

wf_xgb_tuned_result <- final_xgb_wf
rm(final_xgb_wf)

### 8.1.1 Week Rolling Window ----
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

#### Bayesian CV ----
# system.time(
# bayes_res_roll_week <- fit_resamples(
#   wf_bayes_result,
#   resamples = rolling_splits_week,
#   metrics   = eval_metrics,
#   control   = control_resamples(save_pred = TRUE)
# )
# )

#### XGBoost CV ----
system.time(
  xgb_res_roll_week <- fit_resamples(
    #wf_xgb_result,
    wf_xgb_tuned_result,
    resamples = rolling_splits_week,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(xgb_res_roll_week, file = "~/Desktop/xgb_res_roll_week.rda")

#### Remove splits object ----
# to save RAM
rm(rolling_splits_week)

### 8.1.2 Week Expanding Window ----
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

#### Bayesian CV ----
# system.time(
# bayes_res_expanding_week <- fit_resamples(
#   wf_bayes_result,
#   resamples = expanding_splits_week,
#   metrics   = eval_metrics,
#   control   = control_resamples(save_pred = TRUE)
# )
# )

#### XGBoost CV ----
system.time(
  xgb_res_expanding_week <- fit_resamples(
    #wf_xgb_result,
    wf_xgb_tuned_result,
    resamples = expanding_splits_week,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(xgb_res_expanding_week, file = "~/Desktop/xgb_res_expanding_week.rda")

#### Remove splits object ----
# to save RAM
rm(expanding_splits_week)

### 8.1.3 Season Rolling Window ----
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
#rolling_splits_season

#### Bayesian CV ----
system.time(
  bayes_res_roll_season <- fit_resamples(
    wf_bayes_result,
    resamples = rolling_splits_season,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(bayes_res_roll_season, file = "~/Desktop/bayes_res_roll_season.rda")

#### XGBoost CV ----
system.time(
  xgb_res_roll_season <- fit_resamples(
    #wf_xgb_result,
    wf_xgb_tuned_result,
    resamples = rolling_splits_season,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(xgb_res_roll_season, file = "~/Desktop/xgb_res_roll_season.rda")

#### Remove splits object ----
# to save RAM
rm(rolling_splits_season)

### 8.1.4 Season Expanding Window ----
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

#### Bayesian CV ----
system.time(
  bayes_res_expanding_season <- fit_resamples(
    wf_bayes_result,
    resamples = expanding_splits_season,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(bayes_res_expanding_season, file = "~/Desktop/bayes_res_expanding_season.rda")

#### XGBoost CV ----
system.time(
  xgb_res_expanding_season <- fit_resamples(
    #wf_xgb_result,
    wf_xgb_tuned_result,
    resamples = expanding_splits_season,
    metrics   = eval_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE)
  )
)
#save(xgb_res_expanding_season, file = "~/Desktop/xgb_res_expanding_season.rda")

#### Remove splits object ----
# to save RAM
rm(expanding_splits_season)


## 8.2. CV-Fold Metrics ----
### 8.2.1 Collect ----
# Collect and compare CV metrics
bayes_cv_metrics_roll_season <- collect_metrics(bayes_res_roll_season) |>
  mutate(model = "Bayesian", resamp_window = "Rolling", resamp_fold = "Season")
xgb_cv_metrics_roll_season   <- collect_metrics(xgb_res_roll_season) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Season")

bayes_cv_metrics_expanding_season <- collect_metrics(bayes_res_expanding_season) |>
  mutate(model = "Bayesian", resamp_window = "Expanding", resamp_fold = "Season")
xgb_cv_metrics_expanding_season   <- collect_metrics(xgb_res_expanding_season) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Season")

# bayes_cv_metrics_roll_week <- collect_metrics(bayes_res_roll_week) |>
#   mutate(model = "Bayesian Logistic", resamp_window = "Rolling", resamp_fold = "Week")
xgb_cv_metrics_roll_week   <- collect_metrics(xgb_res_roll_week) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Week")

# bayes_cv_metrics_expanding_week <- collect_metrics(bayes_res_expanding_week) |>
#   mutate(model = "Bayesian Logistic", resamp_window = "Expanding", resamp_fold = "Week")
xgb_cv_metrics_expanding_week   <- collect_metrics(xgb_res_expanding_week) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Week")

### 8.2.2 Combine ----
cv_metrics_all   <- bind_rows(
  bayes_cv_metrics_roll_season, 
  xgb_cv_metrics_roll_season,
  bayes_cv_metrics_expanding_season, 
  xgb_cv_metrics_expanding_season,
  #bayes_cv_metrics_roll_week, 
  xgb_cv_metrics_roll_week,
  #bayes_cv_metrics_expanding_week, 
  xgb_cv_metrics_expanding_week
) |>
  mutate(model_full = paste(model, resamp_window, resamp_fold))
print(cv_metrics_all)
print(cv_metrics_all |> filter(str_detect(model, "Bayes")))
print(cv_metrics_all |> filter(str_detect(model, "XGBoost")))

### 8.2.3 Plot ----
ggplot(cv_metrics_all, #|> 
       #filter(.metric %in% c("accuracy","roc_auc","brier_class")),
       aes(x = model_full, y = mean, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ .metric, scales = "free_y") +
  labs(title = "Cross-Validation Performance by Model",
       x = "Model", y = "Metric Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

autoplot(xgb_res_roll_week, metric = "rmse")

## 8.3. CV-Prediction Metrics ----
### 8.3.1 Collect ----
# Collect and compare CV predictions
bayes_cv_predictions_roll_season <- collect_predictions(bayes_res_roll_season) |>
  mutate(model = "Bayesian", resamp_window = "Rolling", resamp_fold = "Season")
xgb_cv_predictions_roll_season   <- collect_predictions(xgb_res_roll_season) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Season")

bayes_cv_predictions_expanding_season <- collect_predictions(bayes_res_expanding_season) |>
  mutate(model = "Bayesian", resamp_window = "Expanding", resamp_fold = "Season")
xgb_cv_predictions_expanding_season   <- collect_predictions(xgb_res_expanding_season) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Season")

# bayes_cv_predictions_roll_week <- collect_predictions(bayes_res_roll_week) |>
#   mutate(model = "Bayesian", resamp_window = "Rolling", resamp_fold = "Week")
xgb_cv_predictions_roll_week   <- collect_predictions(xgb_res_roll_week) |>
  mutate(model = "XGBoost", resamp_window = "Rolling", resamp_fold = "Week")

# bayes_cv_predictions_expanding_week <- collect_predictions(bayes_res_expanding_week) |>
#   mutate(model = "Bayesian", resamp_window = "Expanding", resamp_fold = "Week")
xgb_cv_predictions_expanding_week   <- collect_predictions(xgb_res_expanding_week) |>
  mutate(model = "XGBoost", resamp_window = "Expanding", resamp_fold = "Week")

### 8.3.2 Combine ----
# Combine into one predictions data frame
cv_predictions_all <- bind_rows(
  bayes_cv_predictions_roll_season, 
  xgb_cv_predictions_roll_season,
  bayes_cv_predictions_expanding_season,
  xgb_cv_predictions_expanding_season,
  #bayes_cv_predictions_roll_week,
  xgb_cv_predictions_roll_week,
  #bayes_cv_predictions_expanding_week,
  xgb_cv_predictions_expanding_week
)
cv_predictions_all <- train_data |>
  mutate(.row = row_number()) |>
  select(.row, game_id) |>
  right_join(cv_predictions_all, by = join_by(.row)) |>
  left_join(game_model_data |> select(game_id, season, week, home_team, away_team),
            by = join_by(game_id))

cv_bayes_predictions <- cv_predictions_all |> 
  filter(str_detect(model, "Bayes"))
cv_xgb_predictions <- cv_predictions_all |> 
  filter(str_detect(model, "XGB"))

#### 8.3.2.1 Plot ----
cv_predictions_all |> 
  mutate(error = .pred - result) |>
  ggplot(aes(x = error, fill = model)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 40) +
  facet_grid(resamp_fold ~ resamp_window) +
  labs(
    title = "Distribution of Prediction Errors by Model and CV Plan",
    x = "Prediction Error", y = "Count"
  ) +
  theme_bw(base_size = 14)

cv_predictions_all |>
  mutate(error = .pred - result) |>
  ggplot(aes(x = error, color = model, fill = model)) +
  geom_density(alpha = 0.3) +
  facet_grid(resamp_fold ~ resamp_window) +
  labs(
    title = "Error Distributions by Model and CV Plan",
    x = "Prediction Error", y = "Density"
  ) +
  theme_bw(base_size = 14)

ggplot(cv_predictions_all, aes(x = result, y = .pred, color = model)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_grid(resamp_fold ~ resamp_window) +
  labs(
    title = "Predicted vs. Actual by Model and CV Plan",
    x = "Actual", y = "Predicted"
  ) +
  theme_bw(base_size = 14)

### 8.3.3 Group By ----
#### 8.3.3.1 Model ----
# Example: compute overall accuracy over all predictions
overall_cv_metrics <- cv_predictions_all |> 
  group_by(model, resamp_window, resamp_fold) |>
  summarise(
    rmse = rmse_vec(result, .pred),
    mae = mae_vec(result, .pred),
    #brier_class = brier_class_vec(result, .pred_Home),
    n = n()
  ) 
print(overall_cv_metrics)


ggplot(overall_cv_metrics, aes(x = interaction(model, resamp_window, resamp_fold), y = rmse, fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title = "Overall RMSE by Model and CV Plan",
    x = "Model (CV Plan)", y = "RMSE"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(overall_cv_metrics, aes(x = interaction(model, resamp_window, resamp_fold), y = mae, fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title = "Overall MAE by Model and CV Plan",
    x = "Model (CV Plan)", y = "MAE"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### 8.3.3.2 Season ----
# Example: compute season accuracy over all predictions
season_cv_metrics <- cv_predictions_all |> 
  #separate_wider_delim(id, "_", names = c("id", "season")) |>
  group_by(model, resamp_window, resamp_fold, season) |>
  summarise(
    rmse = rmse_vec(result, .pred),
    mae = mae_vec(result, .pred),
    #brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(season_cv_metrics)

# Use your existing season_cv_metrics object
ggplot(season_cv_metrics, aes(x = season, y = rmse, color = model, linetype = resamp_window)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ resamp_fold, nrow = 1) +
  labs(
    title = "Seasonal RMSE by Model and CV Plan",
    y = "RMSE", x = "Season",
    color = "Model", linetype = "Window"
  ) +
  theme_minimal(base_size = 14)

ggplot(season_cv_metrics, aes(x = season, y = mae, color = model, linetype = resamp_window)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ resamp_fold, nrow = 1) +
  labs(
    title = "Seasonal MAE by Model and CV Plan",
    y = "MAE", x = "Season",
    color = "Model", linetype = "Window"
  ) +
  theme_minimal(base_size = 14)

#### 8.3.3.3 Week ----
week_cv_metrics <- cv_predictions_all |> 
  group_by(model, resamp_window, resamp_fold, week) |>
  summarise(
    rmse = rmse_vec(result, .pred),
    mae = mae_vec(result, .pred),
    #brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(week_cv_metrics)

ggplot(week_cv_metrics, aes(x = week, y = mae, color = model)) +
  geom_line() +
  facet_grid(resamp_fold ~ resamp_window) +
  labs(
    title = "Weekly MAE by Model and CV Plan",
    x = "Week", y = "MAE"
  ) +
  theme_minimal(base_size = 14)

ggplot(week_cv_metrics, aes(x = week, y = rmse, color = model)) +
  geom_line() +
  facet_grid(resamp_fold ~ resamp_window) +
  labs(
    title = "Weekly RMSE by Model and CV Plan",
    x = "Week", y = "RMSE"
  ) +
  theme_minimal(base_size = 14)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 9. FINAL MODELLING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Final fit on full training data & test predictions
# After inspecting CV results, refit on entire train_data and predict on test_data

## 9.1 Fit Models ----
bayes_result_fit   <- fit(wf_bayes_result, data = train_data)
xgb_result_fit    <- fit(wf_xgb_tuned_result,  data = train_data)

xgb_mod <- extract_fit_parsnip(xgb_result_fit)$fit


# Get the feature names
xgb_mod$feature_names

## 9.2 Variable Importance ----
vip(extract_fit_parsnip(xgb_result_fit), num_features = 20)
vip_tbl <- vip(extract_fit_parsnip(xgb_result_fit))
vip_tbl %>% arrange(desc(Importance)) %>% print(n = 50)

## 9.2 Predictions ----
# Predictions will undergo the same preprocessing steps defined in the recipe
# when calling `predict()`, ensuring test_data is transformed identically to train.
# Compare posteriors/implied probabilities vs. actual outcomes to evaluate test performance.
bayes_result_preds <- bind_cols(
  test_data |> select(game_id, season, week, home_team, away_team, result),
  predict(bayes_result_fit, test_data, type = "class"),
  predict(bayes_result_fit, test_data, type = "prob"),
  predict(bayes_result_fit, test_data, type = "conf_int")
) |>
  mutate(model = "Bayesian Logistic", .before = 1)

xgb_result_preds <- bind_cols(
  test_data |> select(game_id, season, week, home_team, away_team, result, spread_line),
  predict(xgb_result_fit, test_data),
  #predict(xgb_result_fit, test_data, type = "prob")
)  |>
  mutate(model = "XGBoost", .before = 1)

result_scores <- test_data |>
  select(game_id, home_score, away_score, result, total)

result_preds <- bind_rows(
  left_join(bayes_result_preds, result_scores, by = "game_id") |>
    select(-contains("lower"), -contains("upper")),
  left_join(xgb_result_preds, result_scores, by = "game_id")
)
result_preds

metrics <- metric_set(rmse, mae, rsq)
metrics(xgb_result_preds, truth = result, estimate = .pred)

## NEWWWWWWW ----
xgb_result_preds <- bind_cols(
  test_data |> 
    select(game_id, season, week, home_team, away_team, result, spread_line),
  predict(xgb_result_fit, test_data),
  #predict(xgb_result_fit, test_data, type = "prob")
) |> filter(!is.na(result))

metrics <- metric_set(rmse, mae, rsq)
metrics(xgb_result_preds, truth = result, estimate = .pred)

xgb_result_preds$error <- xgb_result_preds$.pred - xgb_result_preds$result

xgb_result_preds |>
  group_by(season) |>
  summarize(RMSE = sqrt(mean(error^2)), MAE = mean(abs(error)))

library(ggplot2)
ggplot(xgb_result_preds, aes(x = result, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual", x = "Actual", y = "Predicted") +
  theme_minimal()

ggplot(xgb_result_preds, aes(x = error)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  labs(title = "Prediction Error Histogram", x = "Error", y = "Count") +
  theme_minimal()

# For example, pick side based on predicted result
xgb_result_preds$pick <- ifelse(xgb_result_preds$.pred > xgb_result_preds$spread_line, "home", "away")
xgb_result_preds$actual_cover <- ifelse(xgb_result_preds$result > xgb_result_preds$spread_line, "home", "away")
mean(xgb_result_preds$pick == xgb_result_preds$actual_cover)  # Pick accuracy

# Calculate residuals
residuals <- xgb_result_preds$result - xgb_result_preds$.pred

# Get the 2.5% and 97.5% quantiles for a 95% interval
lower_q <- quantile(residuals, 0.025)
upper_q <- quantile(residuals, 0.975)

# Add lower and upper bounds to predictions
xgb_result_preds$pred_lower <- xgb_result_preds$.pred + lower_q
xgb_result_preds$pred_upper <- xgb_result_preds$.pred + upper_q

xgb_result_preds$in_interval <- (xgb_result_preds$result >= xgb_result_preds$pred_lower) &
  (xgb_result_preds$result <= xgb_result_preds$pred_upper)
mean(xgb_result_preds$in_interval) # Should be close to 95%

library(ggplot2)
ggplot(xgb_result_preds, aes(x = .pred, y = result)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper), alpha = 0.3) +
  labs(title = "XGBoost Predictions with Empirical 95% Intervals",
       x = "Predicted", y = "Actual") +
  theme_minimal()

# Model's betting pick: home if model predicts margin > spread_line, away otherwise
xgb_result_preds$pick <- ifelse(xgb_result_preds$.pred > xgb_result_preds$spread_line, "home", "away")
xgb_result_preds$cover <- ifelse(xgb_result_preds$result > xgb_result_preds$spread_line, "home", "away")
xgb_result_preds$win <- (xgb_result_preds$pick == xgb_result_preds$cover)

# Compute "betting accuracy"
mean(xgb_result_preds$win)  # Fraction of correct picks

# Optional: Table of wins/losses by season/week
library(dplyr)
xgb_result_preds |>
  group_by(season) |>
  summarize(win_rate = mean(win), n = n())

# 1 for correct pick, -1 for incorrect
xgb_result_preds$bet_return <- ifelse(xgb_result_preds$win, 1, -1)
cumulative_return <- cumsum(xgb_result_preds$bet_return)

# Plot cumulative returns
ggplot(xgb_result_preds, aes(x = 1:nrow(xgb_result_preds), y = cumulative_return)) +
  geom_line() +
  labs(title = "Cumulative Betting Return (1 unit per game)", x = "Game #", y = "Cumulative Return") +
  theme_minimal()

ggplot(xgb_result_preds, aes(x = .pred, y = result)) +
  geom_point(alpha = 0.4) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Margin", x = "Model Prediction", y = "Actual Result")

ggplot(xgb_result_preds, aes(x = .pred - result)) +
  geom_histogram(bins = 40, fill = "orange", alpha = 0.7) +
  labs(title = "Prediction Error Histogram", x = "Prediction Error", y = "Count")

xgb_result_preds |>
  mutate(spread_bucket = cut(spread_line, breaks = seq(-20, 20, by = 4))) |>
  group_by(spread_bucket) |>
  summarize(win_rate = mean(win), n = n()) |>
  ggplot(aes(x = spread_bucket, y = win_rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Model Betting Accuracy by Vegas Spread", x = "Spread Bucket", y = "Win Rate") +
  theme_minimal()

saveRDS(xgb_result_preds, "xgb_test_preds_with_intervals_and_bets.rds")


## 9.3 Performance ----
# Compute performance metrics with vector functions

### 9.3.1 Model ----
result_test_metrics_model <- result_preds |>
  group_by(model) |>
  summarise(
    accuracy = accuracy_vec(result, .pred_class),
    roc_auc = roc_auc_vec(result, .pred_Home, event_level = "second"),
    brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(result_test_metrics_model, n = nrow(result_test_metrics_model))

### 9.3.2 Season ----
result_test_metrics_season <- result_preds |>
  group_by(model, season) |>
  summarise(
    accuracy = accuracy_vec(result, .pred_class),
    roc_auc = roc_auc_vec(result, .pred_Home, event_level = "second"),
    brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(result_test_metrics_season, n = nrow(result_test_metrics_season))

### 9.3.3 Week ----
result_test_metrics_week <- result_preds |>
  group_by(model, week) |>
  summarise(
    accuracy = accuracy_vec(result, .pred_class),
    roc_auc = roc_auc_vec(result, .pred_Home, event_level = "second"),
    brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(result_test_metrics_week, n = nrow(result_test_metrics_week))

### 9.3.4 Season & Week ----
result_test_metrics_season_week <- result_preds |>
  group_by(model, season, week) |>
  summarise(
    accuracy = accuracy_vec(result, .pred_class),
    roc_auc = roc_auc_vec(result, .pred_Home, event_level = "second"),
    brier = brier_class_vec(result, .pred_Home),
    .groups = "drop"
  )
print(result_test_metrics_season_week, n = nrow(result_test_metrics_season_week))

## 9.4 ROC Curve ----
# Use prediction tibbles: bayes_result_preds and xgb_result_preds
roc_bayes <- roc_curve(bayes_result_preds, truth = result, .pred_Home) |> 
  mutate(model = "Bayesian Hierarchical")
roc_xgb   <- roc_curve(xgb_result_preds,   truth = result, .pred_Home) |> 
  mutate(model = "XGBoost")

result_roc_curve <- result_preds |>
  group_by(model) |>
  roc_curve(truth = result, .pred_Home, event_level = "second")

result_roc_auc <- result_preds |>
  group_by(model) |>
  roc_auc(truth = result, .pred_Home, event_level = "second")

roc_plot <- result_roc_curve |> 
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

result_betting_preds <- result_preds |>
  left_join(
    test_data |> select(game_id, contains("moneyline")),
    by = join_by(game_id)
  ) |>
  relocate(contains("moneyline"), .after = .pred_Home)

result_betting_return <- result_betting_preds |>
  mutate(
    pred_bet_team = .pred_class,
    pred_bet_correct = result == .pred_class,
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


bets <- result_betting_preds |>      # or xgb_result_preds
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

## 10.1 Betting Strategy: Calculate Actual Returns and Summaries ----
# Using Bayesian model predictions and moneyline odds, compute implied probabilities,
# decide bets where expected value is positive, and calculate actual returns.
bets2 <- result_betting_preds |> 
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
      bet_side == "Home" & result == "Home" ~ stake * (dec_Home - 1),
      bet_side == "Away" & result == "Away" ~ stake * (dec_Away - 1),
      bet_side %in% c("Home", "Away") & bet_side != result ~ -stake,
      TRUE ~ 0
    )
  )

# Summaries
return_all <- result_betting_return |>
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

return_season <- result_betting_return |>
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

return_week <- result_betting_return |>
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

return_season_week <- result_betting_return |>
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

return_team_side <- result_betting_return |>
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


## 10..2 Betting Strategy Evaluation Function ----
# This function evaluates specified betting strategies and returns summaries.
evaluate_betting <- function(preds_df,
                             stake_size = 100,
                             methods = c("favorite", "ev", "kelly"),
                             group_vars = NULL) {
  # preds_df: must contain columns result (factor "Away","Home"), .pred_Away, .pred_Home,
  # home_moneyline, away_moneyline, plus any grouping columns
  require(dplyr)
  df <- preds_df %>%
    mutate(
      # market-implied probabilities
      imp_Home = if_else(home_moneyline > 0,
                         100/(home_moneyline + 100),
                         -home_moneyline / (-home_moneyline + 100)),
      imp_Away = if_else(away_moneyline > 0,
                         100/(away_moneyline + 100),
                         -away_moneyline / (-away_moneyline + 100)),
      # decimal odds
      dec_Home = if_else(home_moneyline > 0,
                         home_moneyline/100 + 1,
                         100/abs(home_moneyline) + 1),
      dec_Away = if_else(away_moneyline > 0,
                         away_moneyline/100 + 1,
                         100/abs(away_moneyline) + 1),
      # Kelly fraction
      kelly_Home = ((dec_Home - 1) * .pred_Home - (1 - .pred_Home)) / (dec_Home - 1),
      kelly_Away = ((dec_Away - 1) * .pred_Away - (1 - .pred_Away)) / (dec_Away - 1),
      # model favorite
      favorite = if_else(.pred_Home > .pred_Away, "Home", "Away")
    )
  
  # Helper function to calculate returns and correctness
  get_results <- function(df_sub) {
    df_sub %>%
      mutate(
        correct = (bet_side == result),
        actual_return = case_when(
          bet_side == "Home" & result == "Home" ~ stake * (dec_Home - 1),
          bet_side == "Away" & result == "Away" ~ stake * (dec_Away - 1),
          bet_side %in% c("Home","Away")         ~ -stake,
          TRUE                                       ~ 0
        )
      )
  }
  
  results_list <- list()
  
  # Method: always bet on favorite
  if ("favorite" %in% methods) {
    df1 <- df %>%
      mutate(
        bet_side = favorite,
        stake    = stake_size
      ) %>%
      get_results()
    results_list[["favorite"]] <- df1
  }
  
  # Method: bet fixed stake_size on positive EV
  if ("ev" %in% methods) {
    df2 <- df %>%
      mutate(
        EV_Home = .pred_Home * (dec_Home - 1) - (1 - .pred_Home),
        EV_Away = .pred_Away * (dec_Away - 1) - (1 - .pred_Away),
        bet_side = case_when(
          EV_Home > EV_Away & EV_Home > 0 ~ "Home",
          EV_Away > EV_Home & EV_Away > 0 ~ "Away",
          TRUE                            ~ NA_character_
        ),
        stake = if_else(!is.na(bet_side), stake_size, 0)
      ) %>%
      get_results()
    results_list[["ev"]] <- df2
  }
  
  # Method: Kelly-sized bet on positive EV
  if ("kelly" %in% methods) {
    df3 <- df %>%
      mutate(
        EV_Home = .pred_Home * (dec_Home - 1) - (1 - .pred_Home),
        EV_Away = .pred_Away * (dec_Away - 1) - (1 - .pred_Away),
        bet_side = case_when(
          EV_Home > EV_Away & EV_Home > 0 ~ "Home",
          EV_Away > EV_Home & EV_Away > 0 ~ "Away",
          TRUE                            ~ NA_character_
        ),
        stake = case_when(
          bet_side == "Home" ~ stake_size * kelly_Home,
          bet_side == "Away" ~ stake_size * kelly_Away,
          TRUE               ~ 0
        )
      ) %>%
      get_results()
    results_list[["kelly"]] <- df3
  }
  
  # Combine and summarize results
  combined <- bind_rows(results_list, .id = "method")
  
  if (!is.null(group_vars) && length(group_vars) > 0) {
    summary <- combined %>%
      filter(!is.na(bet_side)) %>%
      group_by(across(all_of(group_vars)), method) %>%
      summarise(
        n_bets       = n(),
        pct_correct  = mean(correct) * 100,
        total_return = sum(actual_return),
        .groups      = "drop"
      )
  } else {
    summary <- combined %>%
      filter(!is.na(bet_side)) %>%
      group_by(method) %>%
      summarise(
        n_bets       = n(),
        pct_correct  = mean(correct) * 100,
        total_return = sum(actual_return),
        .groups      = "drop"
      )
  }
  
  return(summary)
}


bet_return_all <- evaluate_betting(
  result_betting_preds, 
  stake_size = 100,
  methods = c("favorite","ev","kelly"),
  group_vars = c("model"))
print(bet_return_all, n = nrow(bet_return_all))

bet_return_season <- evaluate_betting(
  result_betting_preds, 
  stake_size = 100,
  methods = c("favorite","ev","kelly"),
  group_vars = c("model", "season"))
print(bet_return_season, n = nrow(bet_return_season))

bet_return_week <- evaluate_betting(
  result_betting_preds, 
  stake_size = 100,
  methods = c("favorite","ev","kelly"),
  group_vars = c("model", "week"))
print(bet_return_week, n = nrow(bet_return_week))



