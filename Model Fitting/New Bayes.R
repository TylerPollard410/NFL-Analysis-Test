
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
library(nflverse)
library(tidyverse)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))


# Load the long-format data (scores model)
modData <- modData |>
  filter(season >= 2007)
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BRMS MODELING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 1. Get Best Features from XGB ---
# Extract feature importances from each model and bind into a single dataframe
feature_importance_df <- pipeline_results |>
  imap_dfr(\(res, season) {
    # Skip if model is missing (optional defensive check)
    if (is.null(res$model)) return(NULL)
    
    xgb.importance(model = res$model) |>
      as_tibble() |>
      mutate(season = as.integer(season))
  })

feature_importance_summary <- feature_importance_df |>
  group_by(Feature) |>
  summarise(
    seasons_used = n_distinct(season),
    mean_gain = mean(Gain),
    mean_cover = mean(Cover),
    mean_freq = mean(Frequency),
    .groups = "drop"
  ) |>
  arrange(desc(mean_gain))

season_feature_matrix <- pipeline_results |>
  imap_dfr(\(res, season) {
    tibble(
      season = as.integer(season),
      feature = res$tuning$best_features
    )
  }) |>
  mutate(used = 1) |>
  pivot_wider(names_from = season, values_from = used, values_fill = 0)

combined_features_df <- feature_importance_summary |>
  left_join(season_feature_matrix, by = c("Feature" = "feature"))
print(combined_features_df, n = nrow(combined_features_df))
head(combined_features_df, 20)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 2. Pre-Processing ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
brms_data_pre <- modData |>
  left_join(
    xgb_preds_df |> 
      select(game_id, home_team, away_team, contains("xgb")),
    by = join_by(game_id, home_team, away_team)
  ) |>
  relocate(xgb_home_score, .after = home_score) |>
  relocate(xgb_away_score, .after = away_score) |>
  relocate(xgb_result, .after = result) |>
  relocate(xgb_total, .after = total)

# --- 1. Exclude Non-Predictor Columns ---
# We want to remove columns that are identifiers, outcomes, or betting/odds-related,
# but we want to keep key contextual variables such as:
#   • week – a time indicator (helps the model capture season progression)
#   • locationID – which serves as a proxy for home-field advantage
#   • div_game, temp, wind – as they might influence game performance.
#
# Define a vector of columns to exclude from the candidate set:
drop_vars <- c("game_id", "season_type", "home_team", "away_team",
               "home_score", "away_score", "result", "spread_line", "spreadCover",
               "total", "total_line", "totalCover", "winner", "away_spread_odds", "away_spread_prob",
               "home_spread_odds", "home_spread_prob", "over_odds", "over_prob",
               "under_odds", "under_prob", "away_moneyline", "away_moneyline_prob",
               "home_moneyline", "home_moneyline_prob", "overtime",
               "stadium", "home_coach", "away_coach",
               "home_games_played", "home_wins", "home_losses", "home_ties",
               "away_games_played", "away_wins", "away_losses", "away_ties")

# Define game-level variables (which we will exclude from XGBoost and later reintroduce in Bayesian modeling)
game_level_vars <- c(
  "season", "week", "weekday", "time_of_day", "location", "div_game", 
  "home_rest", "away_rest",
  "roof", "surface", "temp", "wind"
)

brms_data_pre_features <- modData |>
  select(-all_of(drop_vars), -all_of(game_level_vars), -contains("plays"))
  # mutate(across(starts_with("off_"), .names = "{.col}_net") - 
  #          across(starts_with("def_"))
  # )

colnames(brms_data_pre_features)

brms_data_pre <- modData

# --- 2. Remove Near-Zero Variance Predictors ---
# Here, caret::nearZeroVar identifies variables with almost no variation.
nzv_info2 <- nearZeroVar(brms_data_pre, saveMetrics = TRUE)
nzv_cols2 <- rownames(nzv_info2)[nzv_info2$nzv]
cat("Removing near-zero variance columns:\n", paste(nzv_cols2, collapse = ", "), "\n")
data_clean <- brms_data_pre %>% select(-all_of(nzv_cols2))

# --- 3. Select Candidate Numeric Predictors ---
# Get all numeric columns first
numeric_cols2 <- data_clean %>% select_if(is.numeric)

# Now remove the excluded columns from numeric predictors.
candidate_numeric2 <- numeric_cols2 %>% select(-any_of(drop_vars))

cat("Candidate predictor columns before redundancy removal:\n",
    paste(colnames(candidate_numeric2), collapse = ", "), "\n")
candidate_numeric_cols2 <- colnames(candidate_numeric2)
candidate_numeric_cols2

# In this candidate set we expect key variables like:
#   • week (if stored as numeric)
#   • locationID (a proxy for home advantage)
#   • div_game, temp, wind (if numeric)
# These remain because they are not included in exclude_cols.

# --- 4. Clean Candidate Predictors (Handle NA/Inf) ---
# Before checking for linear combinations, we need a complete numeric dataset.
candidate_numeric_NA2 <- which(!complete.cases(candidate_numeric2))
data_clean_NA <- data_clean |> 
  select(game_id, all_of(candidate_numeric_cols2)) |>
  slice(candidate_numeric_NA2)

NAcols2 <- apply(data_clean_NA, 2, function(x) sum(is.na(x)))
NAcols2 <- NAcols2[NAcols2 != 0]
data_clean_NA <- data_clean_NA |> 
  select(game_id, names(NAcols2))

candidate_numeric_clean2 <- candidate_numeric2 %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  filter(if_all(everything(), ~ is.finite(.)))

# --- 5. Remove Redundant (Linearly Dependent) Predictors ---
# Use caret::findLinearCombos to detect and remove any exact or near-exact linear combinations.
linCombos2 <- findLinearCombos(candidate_numeric_clean2)
if (!is.null(linCombos2$remove) && length(linCombos2$remove) > 0) {
  remove_lin2 <- colnames(candidate_numeric_clean2)[linCombos2$remove]
  cat("Removing linearly dependent columns:\n", paste(remove_lin2, collapse = ", "), "\n")
  candidate_numeric_clean2 <- candidate_numeric_clean2 %>% select(-all_of(remove_lin2))
} else {
  cat("No linear combinations detected.\n")
}
remove_lin2

# --- 6. Define the Final Candidate Feature Set ---
# This is the list of predictor names you intend to use in the model training.
candidate_features2 <- colnames(candidate_numeric_clean2)
cat("Final candidate predictor columns:\n", paste(candidate_features2, collapse = ", "), "\n")
candidate_features2

# Merge XGBoost predictions into wide data by game_id.
# (Ensure that xgb_predictions exist; if not, you may need to load them or run the xgb pipeline first.)



# Add Net Features
brms_data_features <- modData |>
  select(
    1:44,
    contains("elo"),
    contains("win_pct"),
    contains("PFG"),
    contains("PAG"),
    contains("MOV"),
    contains("SOS"),
    contains("SRS"),
    contains("OSRS"),
    contains("DSRS"),
    contains("epa_sum"),
    contains("turnover"),
    contains("scr"),
    contains("red_zone")
  )

brms_data <- modData |> #brms_data_features |>
  mutate(
    net_elo = home_elo - away_elo,
    net_SRS = home_SRS_cum - away_SRS_cum,
    # net_off_epa = home_off_epa_mean_cum - away_def_epa_mean_cum,
    # net_def_epa = away_off_epa_mean_cum - home_def_epa_mean_cum,
    home_net_off_epa_cum = home_off_epa_sum_cum + away_def_epa_sum_cum,
    away_net_off_epa_cum = away_off_epa_sum_cum + home_def_epa_sum_cum,
    net_epa_cum = home_net_off_epa_cum - away_net_off_epa_cum,
    home_net_off_epa_roll = home_off_epa_sum_roll + away_def_epa_sum_roll,
    away_net_off_epa_roll = away_off_epa_sum_roll + home_def_epa_sum_roll,
    net_epa_roll = home_net_off_epa_roll - away_net_off_epa_roll,
    net_turnover_diff = home_turnover_diff_cum - away_turnover_diff_cum,
    net_redzone = home_off_red_zone_app_perc_roll - away_def_red_zone_app_perc_roll
  )

net_features <- brms_data |>
  select(contains("net")) |>
  colnames()

# Filter to complete.cases
brms_seasons <- as.numeric(names(pipeline_results))
brms_seasons <- 2007:2024
brms_data <- brms_data |> filter(season %in% brms_seasons)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Results and Total Model ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create rolling CV folds for wide data based on season:
brms_folds <- list()
for(i in seq(from = 4, to = length(brms_seasons))) {
  train_seasons <- brms_seasons[(i-3):(i-1)]
  test_season   <- brms_seasons[i]
  
  train <- brms_data |> filter(season %in% train_seasons)
  test <- brms_data |> filter(season == test_season)
  
  preProc <- preProcess(
    train |> select(all_of(colnames(feats_clean))),
    method = c("center", "scale")
  )
  train_pre <- predict(preProc, train)
  test_pre <- predict(preProc, test)
  
  brms_folds[[as.character(test_season)]] <- list(
    train = train,
    test  = test,
    train_pre = train_pre,
    test_pre = test_pre
  )
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3. Function to Fit brms Model ----
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
    cores = parallel::detectCores(),
    normalize = TRUE,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95, max_treedepth = 10),
    backend = "cmdstanr",
    seed = 52,
    ...
  )
  return(model)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 5. Fit brms Models across CV Folds ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define model formulas for the outcomes.
# Build formula.
brms_formula_result <- 
  bf(
    result ~
      # xgb_result + 
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
  ) + brmsfamily(family = "gaussian")

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
  ) + brmsfamily(family = "gaussian")

brms_formula_scores <- 
  bf(
    mvbind(home_score, away_score) ~
      home_elo + away_elo +
      home_SRS_cum + away_SRS_cum +
      home_off_epa_sum_cum + home_def_epa_sum_cum +
      away_off_epa_sum_cum + away_def_epa_sum_cum +
      home_off_epa_sum_roll + home_def_epa_sum_roll +
      away_off_epa_sum_roll + away_def_epa_sum_roll +
      home_turnover_won_cum + home_turnover_lost_cum +
      away_turnover_won_cum + away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + home_def_red_zone_eff_cum +
      away_off_red_zone_app_perc_cum + away_def_red_zone_app_perc_cum +
      away_off_red_zone_eff_cum + away_def_red_zone_eff_cum #+
      # (1|home_team) +
      # (1|away_team)
  ) + brmsfamily(family = "discrete_weibull", link = "logit")

prior_default_scores <- default_prior(brms_formula_scores, data = brms_data)
brmsterms_scores <- brmsterms(brms_formula_scores)
brms_vars_scores <- str_extract_all(deparse1(brmsterms_scores$allvars), 
                        "\\b[[:alpha:]][[:alnum:]_]*\\b")[[1]]
brms_vars_scores <- unique(brms_vars_scores)
brms_vars_scores <- intersect(colnames(brms_data), brms_vars_scores)
# net_elo = home_elo - away_elo,
# net_SRS = home_SRS_cum - away_SRS_cum,
# # net_off_epa = home_off_epa_mean_cum - away_def_epa_mean_cum,
# # net_def_epa = away_off_epa_mean_cum - home_def_epa_mean_cum,
# home_net_off_epa_cum = home_off_epa_sum_cum + away_def_epa_sum_cum,
# away_net_off_epa_cum = away_off_epa_sum_cum + home_def_epa_sum_cum,
# net_epa_cum = home_net_off_epa_cum - away_net_off_epa_cum,
# home_net_off_epa_roll = home_off_epa_sum_roll + away_def_epa_sum_roll,
# away_net_off_epa_roll = away_off_epa_sum_roll + home_def_epa_sum_roll,
# net_epa_roll = home_net_off_epa_roll - away_net_off_epa_roll,
# net_turnover_diff = home_turnover_diff_cum - away_turnover_diff_cum,
# net_redzone = home_off_red_zone_app_perc_roll - away_def_red_zone_app_perc_roll

brms_models <- list()
brms_summaries <- list()

for(season in names(brms_folds)) {
  cat("Fitting brms models for test season:", season, "\n")
  train_brms <- brms_folds[[season]]$train_pre
  #model_result <- fit_brms_model(train_brms, brms_formula_result)
  #model_total  <- fit_brms_model(train_brms, brms_formula_total)
  model_scores <- fit_brms_model(train_brms, brms_formula_scores, init = 0)
  
  brms_models[[season]] <- list(
    #result = model_result, 
    #total = model_total,
    scores = model_scores
    )
  brms_summaries[[season]] <- list(
    #result = summary(model_result),
    #total  = summary(model_total),
    scores = summary(model_scores)
  )
}

# Assuming brms_models is your nested list
brms_model_summary_df <- brms_models |>
  imap_dfr(~ {
    season <- .y
    imap_dfr(.x, ~ {
      model <- .x
      outcome <- .y
      
      tidy(model, effects = "fixed", conf.int = TRUE) |>
        mutate(
          season = season,
          outcome = outcome,
          effect_type = "fixed"
        ) |>
        bind_rows(
          tidy(model, effects = "ran_pars", conf.int = TRUE) |>
            mutate(
              season = season,
              outcome = outcome,
              effect_type = "ran_pars"
            )
        )
    })
  }) #|> filter(outcome == "result")

brms_model_summary_df <- brms_model_summary_df |> 
  mutate(across(is.numeric, ~round(.x, digits = 6)))

brms_model_summary_df_sig <- brms_model_summary_df |>
  filter(!(conf.low < 0 & conf.high > 0))

brms_model_summary_df_sig_best <- brms_model_summary_df_sig |>
  group_by(response, term) |>
  summarise(
    seasons_used = n_distinct(season),
    mean_est = mean(estimate),
    mean_low = mean(conf.low),
    mean_high = mean(conf.high)
    #.groups = "drop"
  ) |>
  arrange(desc(seasons_used), .by_group = TRUE)
print(brms_model_summary_df_sig_best, n = nrow(brms_model_summary_df_sig_best))

#print(brms_model_summary_df, n = nrow(brms_model_summary_df))

brms_models_result <- lapply(brms_models, "[[", "result")
brms_models_total <- lapply(brms_models, "[[", "total")
brms_models_scores <- lapply(brms_models, "[[", "scores")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 6. Post Processing & Model Evaluation ----
# %%%%%%%%%%%%%%%

pp <- posterior_epred(
  brms_models[["2024"]]$scores,
  newdata = brms_folds[["2024"]]$test_pre,
  re_formula = NULL,
  allow_new_levels = TRUE
)

pp_home_score <- pp[,,"homescore"]
pp_away_score <- pp[,,"awayscore"]
pp_result <- pp_home_score - pp_away_score
pp_total <- pp_home_score + pp_away_score

performance_metrics <- imap(brms_folds, \(fold, season) {
  test_brms <- fold$test_pre
  
  pp <- posterior_predict(
    brms_models[[season]]$scores,
    newdata = test_brms,
    re_formula = NULL,
    allow_new_levels = TRUE
  )
  
  pp_home_score <- pp[,,"homescore"]
  pp_away_score <- pp[,,"awayscore"]
  pp_result <- pp_home_score - pp_away_score
  pp_total <- pp_home_score + pp_away_score
  
  # Home model
  ci_home_score <- apply(pp_home_score, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_home_score <- mean(test_brms$home_score >= ci_home_score[1, ] & test_brms$home_score <= ci_home_score[2, ], na.rm = TRUE)
  rmse_home_score <- sqrt(mean((colMeans(pp_home_score) - test_brms$home_score)^2, na.rm = TRUE))
  
  # Away model
  ci_away_score <- apply(pp_away_score, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_away_score <- mean(test_brms$away_score >= ci_away_score[1, ] & test_brms$away_score <= ci_away_score[2, ], na.rm = TRUE)
  rmse_away_score <- sqrt(mean((colMeans(pp_away_score) - test_brms$away_score)^2, na.rm = TRUE))
  
  # Result model
  # pp_result <- posterior_predict(
  #   brms_models[[season]]$result,
  #   newdata = test_brms,
  #   re_formula = NULL,
  #   allow_new_levels = TRUE
  # )
  ci_result <- apply(pp_result, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_result <- mean(test_brms$result >= ci_result[1, ] & test_brms$result <= ci_result[2, ], na.rm = TRUE)
  rmse_result <- sqrt(mean((colMeans(pp_result) - test_brms$result)^2, na.rm = TRUE))
  
  # Total model
  # pp_total <- posterior_predict(
  #   brms_models[[season]]$total,
  #   newdata = test_brms,
  #   re_formula = NULL,
  #   allow_new_levels = TRUE
  # )
  ci_total <- apply(pp_total, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  coverage_total <- mean(test_brms$total >= ci_total[1, ] & test_brms$total <= ci_total[2, ], na.rm = TRUE)
  rmse_total <- sqrt(mean((colMeans(pp_total) - test_brms$total)^2, na.rm = TRUE))
  
  # Return result for this season
  list(
    home_score = list(
      coverage = coverage_home_score,
      RMSE = rmse_home_score,
      pp_home_score = pp_home_score,
      game_ids = test_brms$game_id
    ),
    away_score = list(
      coverage = coverage_away_score,
      RMSE = rmse_away_score,
      pp_away_score = pp_away_score,
      game_ids = test_brms$game_id
    ),
    result = list(
      coverage = coverage_result,
      RMSE = rmse_result,
      pp_result = pp_result,
      game_ids = test_brms$game_id
    ),
    total = list(
      coverage = coverage_total,
      RMSE = rmse_total,
      pp_total = pp_total,
      game_ids = test_brms$game_id
    )
  )
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 7. PPC Plots ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Generate PPC Plots for brms models.
ppc_plots_model <- imap(brms_models, \(model, season) {
  result_ppc <- pp_check(model$result, ndraws = 100) +
    ggtitle(season)
  
  total_ppc <- pp_check(model$total, ndraws = 100) +
    ggtitle(season)
  
  list(
    result = result_ppc,
    total = total_ppc
  )
})

wrap_plots(lapply(ppc_plots_model, "[[", "result"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of result for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

wrap_plots(lapply(ppc_plots_model, "[[", "total"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of total for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )


ppc_plots_draws <- imap(performance_metrics, \(model, season) {
  results_post <- model$result$pp_result
  results_obs <- brms_folds[[season]]$test_pre$result
  
  total_post <- model$total$pp_total
  totals_obs <- brms_folds[[season]]$test_pre$total
  
  set.seed(52)
  sampleID <- sample(1:nrow(results_post), size = 100, replace = FALSE)
  
  result_ppc <- ppc_dens_overlay(yrep = results_post[sampleID, ], y = results_obs) +
    ggtitle(season)
  
  total_ppc <- ppc_dens_overlay(yrep = total_post[sampleID, ], y = totals_obs) +
    ggtitle(season)
  
  list(
    result = result_ppc,
    total = total_ppc
  )
})

wrap_plots(lapply(ppc_plots_draws, "[[", "result"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of result for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

wrap_plots(lapply(ppc_plots_draws, "[[", "total"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of total for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

ppc_plots_draws_bars <- imap(performance_metrics, \(model, season) {
  results_post <- model$result$pp_result
  results_obs <- brms_folds[[season]]$test_pre$result
  
  total_post <- model$total$pp_total
  totals_obs <- brms_folds[[season]]$test_pre$total
  
  set.seed(52)
  sampleID <- sample(1:nrow(results_post), size = 100, replace = FALSE)
  
  result_ppc <- ppc_bars(yrep = results_post[sampleID, ], y = results_obs) +
    ggtitle(season)
  
  total_ppc <- ppc_bars(yrep = total_post[sampleID, ], y = totals_obs) +
    ggtitle(season)
  
  list(
    result = result_ppc,
    total = total_ppc
  )
})

wrap_plots(lapply(ppc_plots_draws_bars, "[[", "result"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of result for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

wrap_plots(lapply(ppc_plots_draws_bars, "[[", "total"), 
           guides = "collect") +
  plot_annotation(
    title = paste0("PPC Plots of total for Seasons ", 
                   brms_seasons[1], " - ", brms_seasons[length(brms_seasons)])
  )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BETTING EVALUATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
betting_vars <- c("spread_line", "spreadCover", 
                  "home_spread_odds", "home_spread_prob",
                  "away_spread_odds", "away_spread_prob",
                  "total_line", "totalCover",
                  "over_odds", "over_prob",
                  "under_odds", "under_prob",
                  "winner", 
                  "home_moneyline", "home_moneyline_prob",
                  "away_moneyline", "away_moneyline_prob")

aggregate_posterior_predictions <- function(performance_metrics,
                                            target = c("result", "total")) {
  target <- match.arg(target)
  posterior_col <- if (target == "result") "pp_result" else "pp_total"
  
  # Collect matrices and annotate with season + column index
  extracted <- imap(performance_metrics, \(season_data, season) {
    target_list <- season_data[[target]]
    posterior_matrix <- target_list[[posterior_col]]
    if (is.null(posterior_matrix)) return(NULL)
    
    num_games <- ncol(posterior_matrix)
    
    list(
      matrix = posterior_matrix,
      game_info = tibble(
        season = as.integer(season),
        game_index = seq_len(num_games),
        game_id = target_list[["game_ids"]]
      )
    )
  })
  
  # Filter NULLs
  extracted <- compact(extracted)
  
  # Combine matrices
  full_matrix <- map(extracted, "matrix") |>
    reduce(cbind)
  
  # Combine game info
  game_info <- map_dfr(extracted, "game_info")
  
  return(list(
    posterior = full_matrix,  # draws x all games
    game_info = game_info     # maps each column to season + index
  ))
}

compute_betting_accuracy <- function(posterior_matrix,
                                     game_data,
                                     target = c("result", "total"),
                                     vegas_line_col = NULL,
                                     vegas_prob_col1 = NULL,
                                     vegas_prob_col2 = NULL,
                                     actual_col = NULL,
                                     xgb_pred_col = NULL,
                                     prob_threshold = 0.6,
                                     group_vars = NULL) {
  target <- match.arg(target)
  
  # Target-specific label choices
  target_labels <- if (target == "result") c("Home", "Away") else c("Over", "Under")
  
  vegas_line_col <- vegas_line_col %||% ifelse(target == "result", "spread_line", "total_line")
  vegas_prob_col1 <- vegas_prob_col1 %||% ifelse(target == "result", "home_spread_prob", "over_prob")
  vegas_prob_col2 <- vegas_prob_col1 %||% ifelse(target == "result", "away_spread_prob", "under_prob")
  actual_col     <- actual_col     %||% target
  xgb_pred_col   <- xgb_pred_col   %||% paste0("xgb_", target)
  
  # Match posterior columns to game data
  df <- game_data |>
    mutate(
      posterior_mean = colMeans(posterior_matrix, na.rm = TRUE),
      actual_cover = case_when(
        .data[[actual_col]] > .data[[vegas_line_col]] ~ target_labels[1],
        .data[[actual_col]] < .data[[vegas_line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      predicted_cover_mean = case_when(
        posterior_mean > .data[[vegas_line_col]] ~ target_labels[1],
        posterior_mean < .data[[vegas_line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      correct_cover_mean = predicted_cover_mean == actual_cover
    )
  
  # Full posterior coverage decisions
  df <- df |>
    mutate(
      predicted_covers = map2(
        .x = asplit(posterior_matrix, 2),
        .y = .data[[vegas_line_col]],
        \(draws, line) {
          ifelse(draws > line, target_labels[1],
                 ifelse(draws < line, target_labels[2], NA_character_))
        }
      ),
      correct_posterior = map2_dbl(predicted_covers, actual_cover, \(preds, actual) {
        mean(preds == actual, na.rm = TRUE)
      })
    )
  
  # Vegas-based decision (only bet if confident enough)
  df <- df |>
    mutate(
      vegas_prob_side1 = map_dbl(predicted_covers, ~ mean(.x == target_labels[1], na.rm = TRUE)),
      vegas_prob_side2 = map_dbl(predicted_covers, ~ mean(.x == target_labels[2], na.rm = TRUE)),
      vegas_bet = case_when(
        vegas_prob_side1 > .data[[vegas_prob_col1]] ~ target_labels[1],
        vegas_prob_side2 > .data[[vegas_prob_col2]] ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      vegas_correct = vegas_bet == actual_cover
    )
  
  # Threshold-based decision (only bet if confident enough)
  df <- df |>
    mutate(
      threshold_prob_side1 = map_dbl(predicted_covers, ~ mean(.x == target_labels[1], na.rm = TRUE)),
      threshold_prob_side2 = map_dbl(predicted_covers, ~ mean(.x == target_labels[2], na.rm = TRUE)),
      threshold_bet = case_when(
        threshold_prob_side1 > prob_threshold ~ target_labels[1],
        threshold_prob_side2 > prob_threshold ~ target_labels[2],
        TRUE ~ NA_character_
      ),
      threshold_correct = threshold_bet == actual_cover
    )
  
  # XGBoost prediction (optional)
  if (xgb_pred_col %in% colnames(df)) {
    df <- df |>
      mutate(
        xgb_cover = case_when(
          .data[[xgb_pred_col]] > .data[[vegas_line_col]] ~ target_labels[1],
          .data[[xgb_pred_col]] < .data[[vegas_line_col]] ~ target_labels[2],
          TRUE ~ NA_character_
        ),
        xgb_correct = xgb_cover == actual_cover
      )
  }
  
  # Group if needed
  grouped_df <- if (!is.null(group_vars)) df |> group_by(across(all_of(group_vars))) else df
  
  # Flag if xgb column exists
  has_xgb <- "xgb_correct" %in% colnames(df)
  
  # Summary
  summary <- grouped_df |>
    summarise(
      target = target,
      games = n(),
      PostMean_Acc = mean(correct_cover_mean, na.rm = TRUE) * 100,
      PostFull_Acc = mean(correct_posterior, na.rm = TRUE) * 100,
      Vegas_Acc = mean(vegas_correct, na.rm = TRUE) * 100,
      Vegas_Bets = sum(!is.na(vegas_correct)),
      Thresh_Acc = mean(threshold_correct, na.rm = TRUE) * 100,
      Thresh_Bets = sum(!is.na(threshold_correct)),
      Thresh = prob_threshold,
      XGB_Acc = if (has_xgb) mean(xgb_correct, na.rm = TRUE) * 100 else NA_real_,
      XGB_Bets = if (has_xgb) sum(!is.na(xgb_correct)) else NA_integer_,
      .groups = "drop"
    )
  
  return(summary)
}

## Generate Full Posteriors ----
# Generate full-season aggregated posterior for result predictions
agg_result <- aggregate_posterior_predictions(performance_metrics, target = "result")
agg_total  <- aggregate_posterior_predictions(performance_metrics, target = "total")

# Join this with full game data (vegas lines, scores, etc.)
result_input_df <- agg_result$game_info |> left_join(brms_data, by = c("game_id", "season"))
total_input_df  <- agg_total$game_info  |> left_join(brms_data, by = c("game_id", "season"))

# Compute betting accuracy
run_betting_accuracy_for_targets <- function(performance_metrics,
                                             brms_data,
                                             targets = c("result", "total"),
                                             prob_threshold = 0.6,
                                             out_format = "long",
                                             group_vars = "season") {
  summary_df <- map_dfr(targets, \(target_type) {
    # Aggregate posterior for this target
    agg <- aggregate_posterior_predictions(performance_metrics, target = target_type)
    
    # Join game info with game-level data
    df <- agg$game_info |> 
      left_join(brms_data, by = c("game_id", "season"))
    
    # Compute betting accuracy
    compute_betting_accuracy(
      posterior_matrix = agg$posterior,
      game_data = df,
      target = target_type,
      prob_threshold = prob_threshold,
      group_vars = group_vars
    ) |> 
      mutate(target = target_type)
  })
  if(out_format == "wide"){
    summary_df <- summary_df |> pivot_wider(names_from = target, values_from = c(PostMean_Acc:XGB_Bets))
  }
  return(summary_df)
}

## Combine Output -----
#betting_summary_model_comp <- list()
model_fit <- 5
betting_summary_model_comp[[model_fit]] <- list(
  overall = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = NULL #c("season", "week")
  ), # |> mutate(Fit = 1, .before = 1),
  season = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = "season" #c("season", "week")
  ), # |> mutate(Fit = 1, .before = 1),
  week = run_betting_accuracy_for_targets(
    performance_metrics = performance_metrics,
    brms_data = brms_data,
    targets = c("result", "total"),
    prob_threshold = 0.6,
    out_format = "long", #"wide"
    group_vars = "week"
  ) # |> mutate(Fit = 1, .before = 1)
  #brms_formula_result = brms_formula_result,
  #brms_formula_total = brms_formula_total
)


# betting_summary_overall <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = NULL #c("season", "week")
# )

betting_summary_overall <- betting_summary_model_comp |>
  map("overall") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, Fit)
betting_summary_overall

best_result_bet_model <- betting_summary_overall |>
  filter(target == "result") |>
  arrange(desc(Thresh_Acc)) |>
  select(Fit, target, Games = games, Thresh_Acc, Thresh_Bets, XGB_Acc, XGB_Bets) |>
  mutate(
    Thresh_Exp_Wins = Thresh_Acc*Thresh_Bets/100,
    Thresh_Exp_Loss = (1-Thresh_Acc/100)*Thresh_Bets, 
    Thresh_Exp_Pay = (Thresh_Exp_Wins*0.91 - Thresh_Exp_Loss)*100,
    .after = Thresh_Bets
  ) |>
  mutate(
    XGB_Exp_Wins = XGB_Acc*XGB_Bets/100,
    XGB_Exp_Loss = (1-XGB_Acc/100)*XGB_Bets,
    XGB_Exp_Pay = (XGB_Exp_Wins*0.91 - XGB_Exp_Loss)*100,
  ) |>
  arrange(desc(Thresh_Exp_Pay))

best_result_bet_model

best_total_bet_model <- betting_summary_overall |>
  filter(target == "total") |>
  arrange(desc(Thresh_Acc)) |>
  select(Fit, target, Games = games, Thresh_Acc, Thresh_Bets, XGB_Acc, XGB_Bets) |>
  mutate(
    Thresh_Exp_Wins = Thresh_Acc*Thresh_Bets/100,
    Thresh_Exp_Loss = (1-Thresh_Acc/100)*Thresh_Bets, 
    Thresh_Exp_Pay = (Thresh_Exp_Wins*0.91 - Thresh_Exp_Loss)*100,
    .after = Thresh_Bets
  ) |>
  mutate(
    XGB_Exp_Wins = XGB_Acc*XGB_Bets/100,
    XGB_Exp_Loss = (1-XGB_Acc/100)*XGB_Bets,
    XGB_Exp_Pay = (XGB_Exp_Wins*0.91 - XGB_Exp_Loss)*100,
  ) |>
  arrange(desc(Thresh_Exp_Pay))
best_total_bet_model


# betting_summary_season <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = "season" #c("season", "week")
# )

betting_summary_season <- betting_summary_model_comp |>
  map("season") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, season, Fit)
print(betting_summary_season, n = nrow(betting_summary_season))

# betting_summary_week <- run_betting_accuracy_for_targets(
#   performance_metrics = performance_metrics,
#   brms_data = brms_data,
#   targets = c("result", "total"),
#   prob_threshold = 0.6,
#   out_format = "long", #"wide"
#   group_vars = "week"
# )

betting_summary_week <- betting_summary_model_comp |>
  map("week") |>
  imap_dfr(~ mutate(.x, Fit = paste0("Model", .y), .before = 1)) |>
  arrange(target, week, Fit)
print(betting_summary_week, n = nrow(betting_summary_week))

## Return on Investment -----

compute_roi_table <- function(posterior_matrix, new_data,
                              target = c("result", "total"),
                              threshold_grid = seq(0.50, 0.70, by = 0.01),
                              odds = -110) {
  target <- match.arg(target)
  
  # Define the labels and relevant columns based on target type.
  target_labels <- if (target == "result") c("Home", "Away") else c("Over", "Under")
  line_col <- if (target == "result") "spread_line" else "total_line"
  actual_col <- if (target == "result") "result" else "total"
  
  # Calculate payout ratio from American odds (for -110, win nets ~0.91 profit).
  payout_ratio <- ifelse(odds < 0, 100 / abs(odds), odds / 100)
  
  # Compute cover probabilities for each game.
  # Note: We assume posterior_matrix is organized as draws x games.
  new_data <- new_data %>%
    mutate(
      home_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] > .data[[line_col]][.x])),
      away_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] < .data[[line_col]][.x]))
    ) %>%
    mutate(
      actual_cover = case_when(
        .data[[actual_col]] > .data[[line_col]] ~ target_labels[1],
        .data[[actual_col]] < .data[[line_col]] ~ target_labels[2],
        TRUE ~ NA_character_
      )
    )
  
  # Loop over the threshold grid to determine betting performance.
  roi_table <- map_dfr(threshold_grid, function(thresh) {
    temp <- new_data %>%
      mutate(
        # Apply threshold-based decision logic:
        # If the home cover probability exceeds the threshold and is greater than the away probability, bet "Home".
        # If the away cover probability exceeds the threshold and is greater than the home probability, bet "Away".
        bet_choice = case_when(
          home_cover_prob > thresh & home_cover_prob > away_cover_prob ~ target_labels[1],
          away_cover_prob > thresh & away_cover_prob > home_cover_prob ~ target_labels[2],
          TRUE ~ NA_character_
        ),
        # Check if the bet was correct.
        bet_hit = if_else(bet_choice == actual_cover, 1, 0, missing = NA_integer_),
        # Calculate payout: win returns payout_ratio, loss returns -1, no bet returns 0.
        bet_payout = case_when(
          is.na(bet_choice) ~ 0,
          bet_hit == 1 ~ payout_ratio,
          bet_hit == 0 ~ -1
        )
      )
    
    bets_placed <- sum(!is.na(temp$bet_choice))
    roi_value <- if (bets_placed > 0) sum(temp$bet_payout, na.rm = TRUE) / bets_placed else NA_real_
    accuracy_value <- if (bets_placed > 0) mean(temp$bet_hit, na.rm = TRUE) else NA_real_
    
    tibble(
      threshold   = thresh,
      bets_placed = bets_placed,
      roi         = roi_value,
      accuracy    = accuracy_value
    )
  })
  
  return(roi_table)
}

roi_table_result <- compute_roi_table(
  posterior_matrix = agg_result$posterior, 
  new_data = brms_data,
  target = "result")
print(roi_table_result, n =nrow(roi_table_result))

# Finally, plot ROI vs. threshold
ggplot(roi_table_result, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold",
    x = "Posterior Threshold",
    y = "ROI"
  )

roi_table_total <- compute_roi_table(
  posterior_matrix = agg_total$posterior, 
  new_data = brms_data,
  target = "total")
print(roi_table_total, n =nrow(roi_table_total))

# Finally, plot ROI vs. threshold
ggplot(roi_table_total, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold",
    x = "Posterior Threshold",
    y = "ROI"
  )


















