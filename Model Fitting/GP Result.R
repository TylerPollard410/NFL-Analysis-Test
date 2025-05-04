# Preprocessing script for Bayesian NFL modeling

library(readr)
library(tidytext)
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
eloData <- eloData_update_list$elo_history

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
    eloData,
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
brms_formula_result <-
  bf(
    result ~
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      #home_elo + away_elo +
      net_rating +
      #net_rating_hfa +
      #hfa_pre +
      #net_SRS_cum +
      #home_DSRS_cum + 
      #away_OSRS_cum +
      #home_net_off_epa_sum_cum +
      #away_net_off_epa_sum_cum +
      #home_off_epa_sum_cum + 
      #home_def_epa_sum_cum +
      #away_off_epa_sum_cum + 
      #away_def_epa_sum_cum +
      #home_net_off_epa_sum_roll +
      #away_net_off_epa_sum_roll +
      # home_off_epa_sum_roll + 
      # home_def_epa_sum_roll +
      # away_off_epa_sum_roll + 
      # away_def_epa_sum_roll +
      #home_turnover_diff_cum +
      #away_turnover_diff_cum +
      # home_turnover_won_cum + 
      # home_turnover_lost_cum +
      # away_turnover_won_cum + 
      # away_turnover_lost_cum +
      home_net_off_red_zone_app_perc_cum
      #away_net_off_red_zone_app_perc_cum 
      #home_net_off_red_zone_eff_cum +
      #away_net_off_red_zone_eff_cum
      # home_off_red_zone_app_perc_cum + 
      # home_def_red_zone_app_perc_cum +
      # home_off_red_zone_eff_cum + 
      # home_def_red_zone_eff_cum +
      # away_off_red_zone_app_perc_cum + 
      # away_def_red_zone_app_perc_cum +
      # away_off_red_zone_eff_cum + 
      # away_def_red_zone_eff_cum 
      # gp(season, by = home_team) +
      # gp(season, by = away_team) 
      #(1|gr(season, by = home_team, id = "H")) +
      #(1|gr(season, by = away_team, id = "A"))
      #s(week_seq) +
      #(1|season)
  ) + 
  brmsfamily(family = "student", link = "identity") 
#brmsfamily(family = "cumulative", link = "logit") 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. PREPROCESS DATA ----
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

preprocess_data <- function(data, preds, steps = c("nzv", "lincomb", "corr"), 
                            corr_cutoff = 0.95, 
                            cor_method = "pearson",
                            cor_use = "pairwise.complete.obs") {
  # Subset only the predictor columns
  df <- data |> 
    select(all_of(preds)) |>
    select(where(is.numeric))
  
  removed <- list()
  
  # 4.1.1 Near-zero variance filtering
  if ("nzv" %in% steps) {
    nzv_idx <- nearZeroVar(df, names = TRUE)
    removed$nzv <- nzv_idx
    if (length(nzv_idx) > 0) df <- df |> select(-all_of(nzv_idx))
  }
  
  # 4.1.2 Linear combinations filtering
  if ("lincomb" %in% steps) {
    lin <- findLinearCombos(df)
    removed$lincomb <- if (!is.null(lin$remove)) names(df)[lin$remove] else character(0)
    if (!is.null(lin$remove)) df <- df |> select(-all_of(lin$remove))
  }
  
  # 4.1.3 High-correlation filtering
  if ("corr" %in% steps) {
    corr_mat <- cor(df, use = cor_use, method = cor_method)
    high_corr_idx <- findCorrelation(corr_mat, cutoff = corr_cutoff, names = TRUE,verbose = TRUE)
    removed$corr <- high_corr_idx
    if (length(high_corr_idx) > 0) df <- df |> select(-all_of(high_corr_idx))
  }
  
  # Return a list of removed variables and the cleaned dataset
  list(
    nzv_idx,
    lin,
    corr_mat,
    high_corr_idx,
    removed = removed,
    data = df
  )
}

## 4.2 Preprocess ----
# Identify predictors for the winner model

brms_vars_result <- extract_predictors(brms_data, brms_formula_result)
brms_vars_result

brms_data_prepped <- preprocess_data(brms_data,
                                     brms_vars_result, 
                                     corr_cutoff = 0.95,
                                     cor_method = "pearson",
                                     cor_use = "pairwise.complete.obs")
brms_data_prepped$removed

# Dropped vars
brms_data_prepped_dropped <- brms_data_prepped$removed |>
  list_c() |>
  unique()

brms_data_model <- brms_data |>
  select(all_of(base_cols), 
         all_of(brms_vars_result), 
         -all_of(brms_data_prepped_dropped)) 
colnames(brms_data_model)


preProc_result <- preProcess(
  brms_data_model |> 
    filter(season < 2024) |>
    select(-all_of(base_cols)),
  method = c("center", "scale")
)
train_data <- predict(preProc_result, 
                      brms_data |> filter(season < 2024))
test_data <- predict(preProc_result, 
                     brms_data |> filter(season == 2024))

default_prior(brms_formula_result, data = train_data)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. FIT MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

# priors_result <- c(
#   prior(horseshoe(1), class = "b")
#   #prior(normal(0, 10), class = "b"),
#   #prior(normal(0, 5), class = "b", dpar = "mu1"),
#   #prior(normal(0, 5), class = "b", dpar = "mu2"),
#   #prior(student_t(3, 0, 10), class = "sigma"),
#   #prior(student_t(3, 0, 10), class = "sigma1"),
#   #prior(student_t(3, 0, 10), class = "sigma2"),
#   #prior(inv_gamma(0.1, 0.1), class = "shape"),
#   #prior(student_t(3, 0, 5), class = "sd")
#   #prior(student_t(3, 0, 5), class = "sd", dpar = "mu1"),
#   #prior(student_t(3, 0, 5), class = "sd", dpar = "mu2")
# )

## 5.1 Fit ----
system.time(
  fit_result <- brm(
    brms_formula_result,
    data = train_data,
    #prior = priors_result,
    drop_unused_levels = FALSE,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 52
  )
)


#fit_list <- list()
#fit <- 1
#fit_name <- paste0("fit", fit)

fit <- fit + 1
fit_name <- paste0("fit", fit)
fit_list[[fit]] <- fit_result
#save(fit_list, file = "~/Desktop/NFL Analysis Data/fit_list_ordinal.rda")
save(fit_list, file = "~/Desktop/NFL Analysis Data/fit_list_result.rda")

#print(fit_list[[6]], digits = 4)
print(fit_result, digits = 4)
fixef(fit_result)
ranef(fit_result)

print(fit_list[[1]], digits = 4)
print(roi_table_result_historic1, n = nrow(roi_table_result_historic1))
print(fit_list[[2]], digits = 4)
print(roi_table_result_historic2, n = nrow(roi_table_result_historic2))
print(fit_list[[3]], digits = 4)
print(roi_table_result_historic3, n = nrow(roi_table_result_historic3))

loo_compare(
  loo(fit_list[[1]]),
  loo(fit_list[[2]]),
  loo(fit_list[[3]])
)

## 5.2 Posterior ----
posterior_result <- posterior_predict(
  fit_result,
  newdata = test_data,
  ndraws = sims,
  re_formula = NULL,
  allow_new_levels = TRUE
)

## 5.3 PPC ----
pp_check(fit_result, resp = "result", 
         newdata = train_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)


## 5.3 PPD ----
sampleID <- sample(1:sims, 200, replace = FALSE)
ppc_bars(
  y = as.numeric(as.character(test_data$result)),
  yrep = round(posterior_result[sampleID,])
)
ppc_dens_overlay(
  y = as.numeric(as.character(test_data$result)),
  yrep = posterior_result[sampleID,]
)


## 5.4 Conditional Effects ----
Fitsmooth <- conditional_smooths(fit_result, 
                                 resp = "result", 
                                 method = "posterior_predict"
)
plot(Fitsmooth,
     stype = "contour",
     ask = FALSE)

conditional_eff <- conditional_effects(
  fit_result,
  # effects = c(
  #   "home_OSRS_net",
  #   "home_off_epa_roll",
  #   "away_off_td",
  #   "home_def_epa_roll",
  #   "away_SRS_net",
  #   "away_off_n"
  # ),
  # method = "posterior_predict", 
  re_formula = NULL
  # robust = FALSE
)

plot(conditional_eff, 
     points = TRUE, 
     ask = FALSE)


# 6.Performance Metrics ----
### MAE
MAE_pred_result <- mean(abs(colMeans(posterior_result) - test_data$result))
MAE_pred_result

### MAD
MAD_pred_result <- mean(abs(apply(posterior_result, 2, function(x){quantile(x, 0.5)}) - test_data$result))
MAD_pred_result

### RMSE
RMSE_pred_result <- Metrics::rmse(colMeans(posterior_result), test_data$result)
RMSE_pred_result


## Betting ----
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
      vegas_prob_side1 = map_dbl(predicted_covers, ~ 
                                   sum(.x == target_labels[1], na.rm = TRUE)/length(.x)),
      vegas_prob_side2 = map_dbl(predicted_covers, ~ 
                                   sum(.x == target_labels[2], na.rm = TRUE)/length(.x)),
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
      # threshold_prob_side1 = map_dbl(predicted_covers, ~ mean(.x == target_labels[1], na.rm = TRUE)),
      # threshold_prob_side2 = map_dbl(predicted_covers, ~ mean(.x == target_labels[2], na.rm = TRUE)),
      threshold_prob_side1 = map_dbl(predicted_covers, ~ 
                                       sum(.x == target_labels[1], na.rm = TRUE)/length(.x)),
      threshold_prob_side2 = map_dbl(predicted_covers, ~ 
                                       sum(.x == target_labels[2], na.rm = TRUE)/length(.x)),
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
  
  return(list(df = df,summary = summary))
}

## Return on Investment ----
compute_roi_table <- function(posterior_matrix, new_data,
                              target = c("result", "total"),
                              group_vars = NULL,
                              threshold_grid = seq(0.50, 0.70, by = 0.01),
                              default_odds = -110,
                              use_historic_odds = TRUE,
                              stake = 1) {
  target <- match.arg(target)
  
  # Define the labels and relevant columns based on target type.
  target_labels <- if (target == "result") c("Home", "Away") else c("Over", "Under")
  line_col <- if (target == "result") "spread_line" else "total_line"
  actual_col <- if (target == "result") "result" else "total"
  prob_cols <- if (target == "result") {
    c(Home = "home_spread_prob", Away = "away_spread_prob")
  } else {
    c(Over = "over_prob",    Under = "under_prob")
  } 
  
  # which odds‐columns line up with each label
  odds_cols <- if (target == "result") {
    c(Home = "home_spread_odds", Away = "away_spread_odds")
  } else {
    c(Over = "over_odds",    Under = "under_odds")
  }
  
  # Calculate payout ratio from American odds (for -110, win nets ~0.91 profit).
  #payout_ratio <- ifelse(odds < 0, 100 / abs(odds), odds / 100)
  
  # Compute cover probabilities for each game.
  # Note: We assume posterior_matrix is organized as draws x games.
  new_data <- new_data %>%
    mutate(
      # home_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] > .data[[line_col]][.x])),
      # away_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[.x, ] < .data[[line_col]][.x]))
      home_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[, .x] > .data[[line_col]][.x])),
      away_cover_prob = map_dbl(1:n(), ~ mean(posterior_matrix[, .x] < .data[[line_col]][.x]))
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
        bet_edge = case_when(
          home_cover_prob > away_cover_prob ~ home_cover_prob - .data[[prob_cols[1]]],
          away_cover_prob > home_cover_prob ~ away_cover_prob - .data[[prob_cols[2]]],
          TRUE ~ NA_real_
        ),
        # edge_value = case_when(
        #   bet_choice == target_labels[1] ~ home_cover_prob - prob_cols[1],
        #   bet_choice == target_labels[2]  ~ away_cover_prob - prob_cols[2],
        #   TRUE ~ NA_real_
        # ),
        # EV_Home   = .pred_Home * (dec_Home - 1) - (1 - .pred_Home),
        # EV_Away   = .pred_Away * (dec_Away - 1) - (1 - .pred_Away)
        # Check if the bet was correct.
        bet_hit = if_else(bet_choice == actual_cover, 1, 0, missing = NA_integer_),
        
        # Calculate payout: win returns payout_ratio, loss returns -1, no bet returns 0.
        # bet_payout = case_when(
        #   is.na(bet_choice) ~ 0,
        #   bet_hit == 1 ~ payout_ratio,
        #   bet_hit == 0 ~ -1
        # ),
        # grab the right odds column for your bet_choice
        raw_odds = case_when(
          bet_choice == target_labels[1] ~ .data[[odds_cols[1]]],
          bet_choice == target_labels[2] ~ .data[[odds_cols[2]]],
          TRUE ~ NA_real_
        ),
        # use default if raw_odds is NA
        chosen_odds  = if(use_historic_odds) coalesce(raw_odds, default_odds) else default_odds,
        # american → decimal payout ratio (profit per $1 staked)
        payout_ratio = ifelse(chosen_odds < 0,
                              100 / abs(chosen_odds),
                              chosen_odds / 100),
        # compute your P&L
        bet_payout = case_when(
          is.na(bet_choice) ~ 0,           # no bet
          bet_hit == 1 ~ payout_ratio*stake, # win: profit
          bet_hit == 0 ~ -1*stake,            # loss: lose your stake
          TRUE ~ 0 # pushes get NA in bet_hit
        ),
        bet_payout_edge = case_when(
          bet_edge < 0 ~ 0,           # no bet
          bet_hit == 1 ~ payout_ratio*stake*(1 + bet_edge), # win: profit
          bet_hit == 0 ~ -1*stake*(1 + bet_edge),            # loss: lose your stake
          TRUE ~ 0 # pushes get NA in bet_hit
        )
      )
    
    # if grouping, do per‐group summarise; else overall
    if (!is.null(group_vars)) {
      temp %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          bets_placed = sum(!is.na(bet_choice)),
          bets_won     = sum(bet_hit == 1L, na.rm = TRUE),
          bets_lost    = sum(bet_hit == 0L, na.rm = TRUE),
          bets_pushed  = sum(is.na(bet_hit) & !is.na(bet_choice)),
          profit      = sum(bet_payout, na.rm = TRUE),
          accuracy    = ifelse(bets_placed>0, mean(bet_hit, na.rm = TRUE), NA_real_),
          roi         = ifelse(bets_placed>0, profit / bets_placed, NA_real_),
          edge_bets_placed = sum(bet_edge > 0, na.rm = TRUE),
          edge_accuracy = ifelse(edge_bets_placed>0, mean(bet_edge > 0, na.rm = TRUE), NA_real_),
          edge_profit = sum(bet_payout_edge, na.rm = TRUE),
          edge_roi = ifelse(bet_edge > 0, edge_profit / edge_bets_placed, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(
          roi    = round(roi,    4),
          profit = round(profit, 2),
          edge_roi    = round(edge_roi,    4),
          edge_profit = round(edge_profit, 2),
          threshold = thresh
        ) %>%
        select(all_of(group_vars), 
               threshold, bets_placed, bets_won, bets_lost, bets_pushed, 
               accuracy, roi, profit,
               edge_bets_placed, edge_accuracy, edge_roi, edge_profit)
      
    } else {
      bets_placed <- sum(!is.na(temp$bet_choice))
      bets_won    <- if (bets_placed>0) sum(temp$bet_hit == 1L, na.rm = TRUE) else NA_real_
      bets_lost   <- if (bets_placed>0) sum(temp$bet_hit == 0L, na.rm = TRUE) else NA_real_
      bets_push   <- if (bets_placed>0) sum(is.na(temp$bet_hit) & !is.na(temp$bet_choice)) else NA_real_
      accuracy    <- if (bets_placed>0) mean(temp$bet_hit, na.rm = TRUE) else NA_real_
      profit      <- sum(temp$bet_payout, na.rm = TRUE)
      roi_val     <- if (bets_placed>0) profit / bets_placed    else NA_real_
      edge_bets_placed <- sum(temp$bet_edge > 0, na.rm = TRUE)
      edge_accuracy <- if (edge_bets_placed>0) mean(temp$bet_hit, na.rm = TRUE) else NA_real_
      edge_profit <- sum(temp$bet_payout_edge, na.rm = TRUE)
      edge_roi <- if (edge_bets_placed > 0) edge_profit / edge_bets_placed else NA_real_
      
      tibble(
        threshold    = thresh,
        bets_placed  = bets_placed,
        bets_won     = bets_won,
        bets_lost    = bets_lost,
        bets_pushed  = bets_push,
        accuracy     = round(accuracy, 4),
        roi          = round(roi_val, 4),
        profit       = round(profit,   2), 
        edge_bets_placed = edge_bets_placed,
        edge_accuracy     = round(edge_accuracy, 4),
        edge_roi          = round(edge_roi, 4),
        edge_profit       = round(edge_profit,   2)
      )
    }
    
    # bets_placed <- sum(!is.na(temp$bet_choice))
    # roi_value <- if (bets_placed > 0) sum(temp$bet_payout, na.rm = TRUE) / bets_placed else NA_real_
    # profit_value <- if (bets_placed > 0) sum(temp$bet_payout, na.rm = TRUE) else NA_real_
    # accuracy_value <- if (bets_placed > 0) mean(temp$bet_hit, na.rm = TRUE) else NA_real_
    # 
    # tibble(
    #   threshold     = thresh,
    #   bets_placed   = bets_placed,
    #   accuracy      = accuracy_value,
    #   roi           = round(roi_value, 4),
    #   profit        = round(profit_value, 2)
    # )
  })
  
  return(roi_table)
}


## Compute Bet Results ----
betting_vars <- c(
  "game_id", "season", "week",
  "game_type","season_type",
  "home_team","away_team",
  "home_score", 
  "away_score", 
  "result","spread_line","spreadCover",
  "total","total_line","totalCover",
  "winner",
  "away_spread_odds","away_spread_prob",
  "home_spread_odds","home_spread_prob",
  "over_odds","over_prob","under_odds","under_prob",
  "away_moneyline","away_moneyline_prob",
  "home_moneyline","home_moneyline_prob"
)

betting_df <- test_data |>
  select(all_of(betting_vars))

## Result ----
betting_accuracy_result <- compute_betting_accuracy(
  posterior_result,
  #discrete_mat,
  betting_df,
  target = c("result"),
  vegas_line_col = NULL,
  vegas_prob_col1 = NULL,
  vegas_prob_col2 = NULL,
  actual_col = NULL,
  xgb_pred_col = NULL,
  prob_threshold = 0.7,
  group_vars = NULL
)
betting_accuracy_result$summary

group_col <- NULL
roi_table_result_historic <- compute_roi_table(
  posterior_matrix = posterior_result, 
  #posterior_matrix = discrete_mat, 
  new_data = betting_df,
  target = "result",
  group_vars = group_col,
  threshold_grid = seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = TRUE,
  stake = 100)
print(roi_table_result_historic, n =nrow(roi_table_result_historic))

roi_table_result_default <- compute_roi_table(
  posterior_matrix = posterior_result, 
  new_data = betting_df,
  target = "result",
  threshold_grid = seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = FALSE,
  stake = 100)
#print(roi_table_result_default, n =nrow(roi_table_result_default))

roi_table_result <-
  bind_cols(
    roi_table_result_default,
    roi_table_result_historic |> select(accuracy_hist = accuracy,
                                        roi_hist = roi, 
                                        profit_hist = profit)
  )
print(roi_table_result, n = nrow(roi_table_result))

# Finally, plot ROI vs. threshold
ggplot(roi_table_result_historic, aes(x = threshold, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Accuracy by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Accuracy"
  ) +
  theme_bw()

# Finally, plot ROI vs. threshold
ggplot(roi_table_result_historic, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "ROI"
  ) +
  theme_bw()

ggplot(roi_table_result_historic, aes(x = threshold, y = profit)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profit by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Profit"
  ) +
  theme_bw()



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. BACKTEST  ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Prepare data
backtest_data <- brms_data |>
  select(game_id, season, week, home_team, away_team,
         result,
         net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum)

# Define backtest weeks (2010 to current)
weeks_df <- backtest_data |> 
  distinct(season, week) |> 
  arrange(season, week) |> 
  filter(season >= 2010)

# Initialize training set (pre-2010)
train_set <- backtest_data |> filter(season < 2010)
test_set <- backtest_data |> filter(season >= 2010)

brms_formula_result <-
  bf(
    result ~
      #0 + Intercept +
      net_elo +
      net_rating +
      hfa_pre +
      home_net_off_red_zone_app_perc_cum
  ) + 
  brmsfamily(family = "student", link = "identity") 

iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

# Container for results
backtest_results <- list()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Iterative backtesting loop ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n_weeks <- nrow(weeks_df)
system.time(
for (i in seq_len(n_weeks)) {
  yr <- weeks_df$season[i]
  wk <- weeks_df$week[i]
  key <- sprintf("S%d_W%d", yr, wk)
  message(sprintf("[Backtest %d/%d] Processing %s...", i, n_weeks, key))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2a. Split and Preprocess ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  test_feats <- test_set |> 
    filter(season == yr, week == wk) # |> 
    # select(game_id, season, week, home_team, away_team,
    #        net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum)
  test_full <- test_feats # |> 
    #left_join(modDataBase |> select(game_id, result), by = "game_id")
  
  message("  - Preprocessing data...")
  preProc <- preProcess(
    train_set |> select(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
    method = c("center", "scale")
  )
  train_pp <- predict(preProc, train_set)
  test_pp <- predict(preProc, test_full)
  # train_pp <- train_set |> 
  #   mutate(across(c(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
  #                 ~ predict(preProc, .x))) |> 
  #   select(game_id, season, week, home_team, away_team,
  #          result, net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum)
  # test_pp <- test_feats |> 
  #   mutate(across(c(net_elo, net_rating, hfa_pre, home_net_off_red_zone_app_perc_cum),
  #                 ~ predict(preProc, .x)))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2b. Fit BRMS model ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  message("  - Fitting BRMS model...")
  # fit_model <- brm(
  #   result ~ net_elo + net_rating + hfa_pre + home_net_off_red_zone_app_perc_cum,
  #   data    = train_pp,
  #   family  = gaussian(),
  #   chains  = 4,
  #   iter    = 2000,
  #   cores   = parallel::detectCores(),
  #   seed    = 123
  # )
  
  fit_model <- brm(
    brms_formula_result,
    data = train_pp,
    #prior = priors_result,
    drop_unused_levels = FALSE,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 52, 
    refresh = 0
  )
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 2c. Posterior prediction ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  message("  - Generating posterior predictions...")
  posterior_mat <- posterior_predict(
    fit_model,
    newdata = test_pp,
    re_formula = NULL,
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
    test_full
  )
}
)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Result: backtest_results list structure ----
#    $"S{YEAR}_W{WEEK}" -> list(train, test, posterior, effects)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Optionally save
save(backtest_results, file = "~/Desktop/NFL Analysis Data/backtest_results.rda")



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Combine predictions and compute metrics ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(purrr)
library(tidyr)
library(ggplot2)

betting_vars <- c(
  "game_id", "season", "week",
  "game_type","season_type",
  "home_team","away_team",
  "home_score", 
  "away_score", 
  "result","spread_line","spreadCover",
  "total","total_line","totalCover",
  "winner",
  "away_spread_odds","away_spread_prob",
  "home_spread_odds","home_spread_prob",
  "over_odds","over_prob","under_odds","under_prob",
  "away_moneyline","away_moneyline_prob",
  "home_moneyline","home_moneyline_prob"
)

betting_df <- brms_data |>
  filter(season >= 2010) |>
  select(all_of(betting_vars))

posterior_all_backtest <- do.call(cbind, map(backtest_results, "posterior"))

group_col <- NULL
roi_table_backtest_all <- compute_roi_table(
  posterior_matrix = posterior_all_backtest, 
  #posterior_matrix = discrete_mat, 
  new_data = betting_df,
  target = "result",
  group_vars = group_col,
  threshold_grid = seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = TRUE,
  stake = 100)
print(roi_table_backtest_all, n =nrow(roi_table_backtest_all))

ggplot(roi_table_backtest_all, aes(x = threshold, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Accuracy by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Accuracy"
  ) +
  theme_bw()

# Finally, plot ROI vs. threshold
ggplot(roi_table_backtest_all, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "ROI"
  ) +
  theme_bw()

ggplot(roi_table_backtest_all, aes(x = threshold, y = profit)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profit by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Profit"
  ) +
  theme_bw()

group_col <- "season"
roi_table_backtest_season <- compute_roi_table(
  posterior_matrix = posterior_all_backtest, 
  #posterior_matrix = discrete_mat, 
  new_data = betting_df,
  target = "result",
  group_vars = group_col,
  threshold_grid = 0.58, #seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = TRUE,
  stake = 100)
print(roi_table_backtest_season, n =nrow(roi_table_backtest_season))

ggplot(roi_table_backtest_season, aes(x = season, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Accuracy by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Accuracy"
  ) +
  theme_bw()

# Finally, plot ROI vs. threshold
ggplot(roi_table_backtest_season, aes(x = season, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "ROI"
  ) +
  theme_bw()

ggplot(roi_table_backtest_season, aes(x = season, y = profit)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profit by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Profit"
  ) +
  theme_bw()

group_col <- "week"
roi_table_backtest_week <- compute_roi_table(
  posterior_matrix = posterior_all_backtest, 
  #posterior_matrix = discrete_mat, 
  new_data = betting_df,
  target = "result",
  group_vars = group_col,
  threshold_grid = 0.58, #seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = TRUE,
  stake = 100)
print(roi_table_backtest_week, n =nrow(roi_table_backtest_week))

ggplot(roi_table_backtest_week, aes(x = week, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Accuracy by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Accuracy"
  ) +
  theme_bw()

# Finally, plot ROI vs. threshold
ggplot(roi_table_backtest_week, aes(x = week, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "ROI"
  ) +
  theme_bw()

ggplot(roi_table_backtest_week, aes(x = week, y = profit)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profit by Posterior Betting Confidence Threshold - Result",
    x = "Posterior Threshold",
    y = "Profit"
  ) +
  theme_bw()

# 4a. Bind all posterior predictions into one tibble
all_preds <- imap_dfr(backtest_results, function(res, key) {
  test_df <- res$test
  post_mat <- res$posterior    # draws x games
  # calculate summaries per game
  pred_mean <- colMeans(post_mat)
  pred_lwr  <- apply(post_mat, 2, quantile, probs = 0.025)
  pred_upr  <- apply(post_mat, 2, quantile, probs = 0.975)
  tibble(
    slate      = key,
    game_id    = test_df$game_id,
    actual     = test_df$result,
    pred_mean  = pred_mean,
    pred_lwr   = pred_lwr,
    pred_upr   = pred_upr
  )
})

# 4b. Calculate overall error metrics
error_metrics <- all_preds %>%
  summarise(
    MAE      = mean(abs(pred_mean - actual)),
    MAD      = median(abs(pred_mean - actual)),
    RMSE     = sqrt(mean((pred_mean - actual)^2)),
    Bias     = mean(pred_mean - actual),
    Coverage = mean(actual >= pred_lwr & actual <= pred_upr)
  )
print(error_metrics)

# 4c. Compute ROI table across thresholds
roi_all <- compute_roi_table(
  posterior_matrix = do.call(cbind, map(backtest_results, "posterior")),
  new_data         = bind_rows(map(backtest_results, "test")),
  threshold_grid   = seq(0.50, 0.75, by = 0.01)
)
print(roi_all)

# 4d. Plot ROI and Profit vs Threshold
roi_long <- roi_all %>%
  select(threshold, roi, edge_roi, profit, edge_profit) %>%
  pivot_longer(cols = c(roi, edge_roi, profit, edge_profit),
               names_to = "metric", values_to = "value")

ggplot(roi_long, aes(x = threshold, y = value, color = metric)) +
  geom_line() + geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Betting Performance vs Confidence Threshold",
       x = "Posterior Probability Threshold",
       y = "Value") +
  theme_minimal()

# End of backtest and evaluation







