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
    eloData
  ) |>
  left_join(
    kfaData
  )

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
    net_rating        = c("home_rating", "away_rating"),
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
    "net_rating", "home_rating", "away_rating", "hfa_est",
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
# brms_data <- brms_data |>
#   mutate(
#     home_score = factor(home_score, #levels = c(0, 2:70), 
#                         ordered = TRUE),
#     away_score = factor(away_score, #levels = c(0, 2:70), 
#                         ordered = TRUE)
#   )
# nlevels(brms_data$home_score)
# nlevels(brms_data$away_score)
brms_formula_homescore <- 
  bf(
    home_score ~
      net_elo +
      net_elo_pre +
      #home_elo + away_elo +
      #net_SRS_cum +
      home_OSRS_cum +
      away_DSRS_cum +
      home_off_epa_sum_cum + 
      #home_def_epa_sum_cum +
      #away_off_epa_sum_cum + 
      away_def_epa_sum_cum +
      home_off_epa_sum_roll + 
      #home_def_epa_sum_roll +
      #away_off_epa_sum_roll + 
      away_def_epa_sum_roll +
      home_turnover_won_cum + 
      home_turnover_lost_cum +
      away_turnover_won_cum + 
      away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + 
      #home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + 
      #home_def_red_zone_eff_cum +
      #away_off_red_zone_app_perc_cum + 
      away_def_red_zone_app_perc_cum +
      #away_off_red_zone_eff_cum + 
      away_def_red_zone_eff_cum +
      # gp(season, by = home_team) +
      # gp(season, by = away_team) 
      (1|gr(season, by = home_team, id = "H")) +
      (1|gr(season, by = away_team, id = "A"))
  ) + 
  brmsfamily(family = "student", link = "identity") 
#brmsfamily(family = "cumulative", link = "logit") +
brms_formula_awayscore <-
  bf(
    away_score ~
      net_elo +
      net_elo_pre +
      #home_elo + away_elo +
      #net_SRS_cum +
      home_DSRS_cum + 
      away_OSRS_cum +
      #home_off_epa_sum_cum + 
      home_def_epa_sum_cum +
      away_off_epa_sum_cum + 
      #away_def_epa_sum_cum +
      #home_off_epa_sum_roll + 
      home_def_epa_sum_roll +
      away_off_epa_sum_roll + 
      #away_def_epa_sum_roll +
      home_turnover_won_cum + 
      home_turnover_lost_cum +
      away_turnover_won_cum + 
      away_turnover_lost_cum +
      home_off_red_zone_app_perc_cum + 
      home_def_red_zone_app_perc_cum +
      home_off_red_zone_eff_cum + 
      home_def_red_zone_eff_cum +
      away_off_red_zone_app_perc_cum + 
      away_def_red_zone_app_perc_cum +
      away_off_red_zone_eff_cum + 
      away_def_red_zone_eff_cum +
      # gp(season, by = home_team) +
      # gp(season, by = away_team) 
      (1|gr(season, by = home_team, id = "H")) +
      (1|gr(season, by = away_team, id = "A"))
  ) + 
  brmsfamily(family = "student", link = "identity") 

brms_formula_scores <-
  brms_formula_homescore +
  brms_formula_awayscore +
  set_rescor(TRUE)


brms_formula_result <-
  bf(
    result ~
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      #home_elo + away_elo +
      net_rating +
      #hfa_est +
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
    #s(week_seq)
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
brms_vars_scores <- extract_predictors(brms_data, brms_formula_scores)
brms_vars_scores

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
         all_of(brms_vars_scores), 
         -all_of(brms_data_prepped_dropped)) 
# brms_data <- brms_data |>
#   mutate(
#     result = factor(result, ordered = TRUE)
#   )
colnames(brms_data_model)

# mutate(
#   home_score = factor(home_score, levels = c(0, 2:70)),
#   .after = home_score
# ) |>
# mutate(
#   away_score = factor(away_score, levels = c(0, 2:70)),
#   .after = away_score
# )

preProc_scores <- preProcess(
  brms_data_model |> 
    filter(season < 2024) |>
    select(-all_of(base_cols)),
  method = c("center", "scale")
)
train_data <- predict(preProc_scores, 
                      brms_data |> filter(season < 2024))
test_data <- predict(preProc_scores, 
                     brms_data |> filter(season == 2024))

default_prior(brms_formula_scores, data = train_data)
default_prior(brms_formula_result, data = train_data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. FIT MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iters <- 20000
burn <- 10000
chains <- 4
sims <- (iters-burn)*chains

priors_result <- c(
  prior(horseshoe(1), class = "b")
  #prior(normal(0, 10), class = "b"),
  #prior(normal(0, 5), class = "b", dpar = "mu1"),
  #prior(normal(0, 5), class = "b", dpar = "mu2"),
  #prior(student_t(3, 0, 10), class = "sigma"),
  #prior(student_t(3, 0, 10), class = "sigma1"),
  #prior(student_t(3, 0, 10), class = "sigma2"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  #prior(student_t(3, 0, 5), class = "sd")
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu1"),
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu2")
)

## 5.1 Fit ----
system.time(
  fit_scores <- brm(
    #brms_formula_scores,
    brms_formula_result,
    data = train_data,
    #prior = priors_result,
    #drop_unused_levels = FALSE,
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
fit_list[[fit]] <- fit_scores
#save(fit_list, file = "~/Desktop/NFL Analysis Data/fit_list_ordinal.rda")
save(fit_list, file = "~/Desktop/NFL Analysis Data/fit_list_scores.rda")

#print(fit_list[[6]], digits = 4)
print(fit_scores, digits = 4)
fixef(fit_scores)
ranef(fit_scores)

loo_scores_list <- list()
loo_scores_list[[fit]] <- loo(fit_scores, model_names = paste0("fit", fit))

train_data2 <- train_data |>
  mutate(
    # home_score = droplevels(home_score),
    # away_score = droplevels(away_score),
    result = droplevels(result)
  )
test_data2 <- test_data |>
  mutate(
    # home_score = droplevels(home_score),
    # away_score = droplevels(away_score),
    result = droplevels(result)
  )

## 5.2. DISCRETIZE ----
### 5.2.1 Method 1 -----
# 1) Empirical distribution of score differences in the TRAIN set
emp_dist_homescore <- train_data %>% 
  count(home_score) %>% 
  mutate(p_emp = n / sum(n)) %>% 
  arrange(home_score)

emp_dist_awayscore <- train_data %>% 
  count(away_score) %>% 
  mutate(p_emp = n / sum(n)) %>% 
  arrange(away_score)

emp_dist_result <- train_data %>% 
  count(result) %>% 
  mutate(p_emp = n / sum(n)) %>% 
  arrange(result)

emp_dist_total <- train_data %>% 
  count(total) %>% 
  mutate(p_emp = n / sum(n)) %>% 
  arrange(total)

k_vals  <- emp_dist$result       # vector of possible integer results
p_emp   <- emp_dist$p_emp      # their empirical probabilities

# 2) Extract posterior draws of:
#    – μ_home, μ_away  (location)  
#    – σ_home, σ_away  (scale)  
#    – ρ                (residual correlation)  
#    – df               (Student‐t d.f.)
post_linpred <- posterior_linpred(
  fit_scores, 
  newdata = test_data, 
  dpar = "mu", 
  re_formula = NULL, 
  summary = FALSE
)
# dims: draws × n_games × 2 (home, away)


post_pars <- as_draws_matrix(
  fit_scores,
  # variable = c(
  #   "sigma_homescore",
  #   "sigma_awayscore",
  #   "rescor__homescore__awayscore",
  #   "nu"
  # ),
  variable = c(
    "sigma_homescore",
    "sigma_awayscore",
    "nu_homescore",
    "nu_awayscore"
  )
)

# dims: draws × 4

# 2c) Coerce to a plain numeric matrix so indexing returns scalars
post_pars <- as.matrix(post_pars)

# 2d) Sanity check that the number of draws matches
n_draws <- nrow(post_pars)
stopifnot(n_draws == dim(post_linpred)[1])

# 3) Define a helper to map one draw to integer results
map_draw_to_result <- function(mu_vec, sigma_h, sigma_a, rho, df, k_vals, p_emp) {
  # coerce scalars to plain numeric
  sigma_h <- as.vector(sigma_h)
  sigma_a <- as.vector(sigma_a)
  rho     <- as.vector(rho)
  df      <- as.vector(df)
  
  # implied Student-t sd for the difference
  s_diff  <- sqrt(sigma_h^2 + sigma_a^2 - 2 * rho * sigma_h * sigma_a)
  s_diff  <- as.vector(s_diff)
  
  # for each game, weight the t-density at each integer and sample
  map_int(mu_vec, function(mu_i) {
    mu_i   <- as.vector(mu_i)
    dens_k <- dt((k_vals - mu_i) / s_diff, df = df) / s_diff
    w      <- dens_k * p_emp
    p_k    <- w / sum(w)
    sample(k_vals, size = 1, prob = p_k)
  })
}

# 4) Loop over all draws to build a tibble of discrete results
n_games <- dim(post_linpred)[2]

discrete_res <- map(seq_len(n_draws), function(i) {
  # progress message
  cat(sprintf("\rProcessing draw %3d / %3d", i, n_draws))
  flush.console()
  
  # continuous means for draw i
  mu_home <- post_linpred[i, , 1]
  mu_away <- post_linpred[i, , 2]
  mu_diff <- mu_home - mu_away
  
  # sample integer result for each game
  map_draw_to_result(
    mu_vec  = mu_diff,
    sigma_h = post_pars[i, "sigma_homescore"],
    sigma_a = post_pars[i, "sigma_awayscore"],
    rho     = post_pars[i, "rescor__homescore__awayscore"],
    df      = post_pars[i, "nu"],
    k_vals  = k_vals,
    p_emp   = p_emp
  )
  
  # cat(sprintf("\rProcessing draw %3d / %3d", i, n_draws))
  # flush.console()
})

cat("\nAll draws processed.\n")

discrete_mat <- t(simplify2array(discrete_res))

# b) turn into a plain matrix and name the columns V1:Vn_draws
discrete_mat <- t(simplify2array(discrete_res))

# Now `discrete_res` is a [n_games × n_draws] matrix with
#   - one row per game (same order as test_data)
#   - one column per posterior draw, named V1 … V{n_draws}

# If you *really* want it as [n_draws × n_games] (draws as rows,
# games as columns), just transpose:
# discrete_res_t <- t(discrete_res)


# 5) Quick check: posterior pmf for game 1
prop.table(table(discrete_mat[, 1]))
sum(prop.table(table(discrete_mat[, 1])))


### 5.2.2 Method 2 -----
emp_dist <- train_data %>%
  count(result) %>%
  mutate(p_emp = n / sum(n)) %>%
  arrange(result)

k_vals <- emp_dist$result
p_emp  <- emp_dist$p_emp

# 5.2.2 Extract posterior draws of μ, σ, ν
post_linpred <- posterior_linpred(
  fit_scores,
  newdata    = test_data,
  dpar       = "mu",
  re_formula = NULL,
  summary    = FALSE
)
# dims: [n_draws, n_games, 2]

post_pars <- as_draws_matrix(
  fit_scores,
  variable = c(
    "sigma_homescore",
    "sigma_awayscore",
    "nu_homescore",
    "nu_awayscore"
  )
) %>% as.matrix()

n_draws <- nrow(post_pars)
stopifnot(n_draws == dim(post_linpred)[1])
n_games <- dim(post_linpred)[2]

# 5.2.3 Helper to sample discrete result per draw using approximate combined d.f. and rho=0
map_draw_to_result <- function(mu_vec, sigma_h, sigma_a, nu_h, nu_a, k_vals, p_emp) {
  sigma_h <- as.vector(sigma_h)
  sigma_a <- as.vector(sigma_a)
  # no residual correlation
  s_diff <- sqrt(sigma_h^2 + sigma_a^2)
  # approximate degrees of freedom for difference via Welch-Satterthwaite
  # ensure nu_h > 2 and nu_a > 2 to avoid division by zero
  df_h <- as.vector(nu_h)
  df_a <- as.vector(nu_a)
  var_h <- sigma_h^2
  var_a <- sigma_a^2
  num    <- (var_h + var_a)^2
  denom  <- (var_h^2 / (df_h - 2)) + (var_a^2 / (df_a - 2))
  df_comb <- num / denom
  df_comb <- as.vector(df_comb)
  
  map_int(mu_vec, function(mu_i) {
    mu_i   <- as.vector(mu_i)
    dens_k <- dt((k_vals - mu_i) / s_diff, df = df_comb) / s_diff
    w      <- dens_k * p_emp
    p_k    <- w / sum(w)
    sample(k_vals, size = 1, prob = p_k)
  })
}

# 5.2.4 Loop over draws and build discrete matrix and build discrete matrix
discrete_res <- map(seq_len(n_draws), function(i) {
  # progress message
  cat(sprintf("\rProcessing draw %3d / %3d", i, n_draws))
  flush.console()
  
  mu_diff <- post_linpred[i, , 1] - post_linpred[i, , 2]
  map_draw_to_result(
    # mu_diff,
    # post_pars[i, "sigma_homescore"],
    # post_pars[i, "sigma_awayscore"],
    # post_pars[i, "nu_homescore"],
    # post_pars[i, "nu_awayscore"],
    # k_vals,
    # p_emp
    mu_vec  = mu_diff,
    sigma_h = post_pars[i, "sigma_homescore"],
    sigma_a = post_pars[i, "sigma_awayscore"],
    nu_h     = post_pars[i, "nu_homescore"],
    nu_a      = post_pars[i, "nu_awayscore"],
    k_vals  = k_vals,
    p_emp   = p_emp
  )
})
cat("\nAll draws processed.\n")

discrete_mat <- t(simplify2array(discrete_res))
prop.table(table(discrete_mat[, 1]))
sum(prop.table(table(discrete_mat[, 1])))

## 5.3 Posteriors -----
# posterior_train_score <- posterior_predict(
#   fit_scores,
#   #newdata = train_data,
#   re_formula = NULL,
#   allow_new_levels = TRUE
# )
# posterior_train_homescore <- posterior_train_score[,,"homescore"]
# posterior_train_awayscore <- posterior_train_score[,,"awayscore"]
# posterior_train_result <- posterior_train_homescore - posterior_train_awayscore
# posterior_train_total <- posterior_train_homescore + posterior_train_awayscore
# posterior_train_winner <- posterior_train_homescore > posterior_train_awayscore

fit_scores <- fit_list[[11]]
posterior_score <- posterior_predict(
  fit_scores,
  newdata = test_data,
  ndraws = 10000,
  re_formula = NULL,
  allow_new_levels = TRUE
)
posterior_homescore <- posterior_score[,,"homescore"]
posterior_awayscore <- posterior_score[,,"awayscore"]
posterior_result <- posterior_score
posterior_result <- posterior_homescore - posterior_awayscore
posterior_total <- posterior_homescore + posterior_awayscore
posterior_winner <- posterior_homescore > posterior_awayscore

## 5.3 PPC ----
pp_check(fit_scores, resp = "homescore", 
         newdata = train_data, 
         allow_new_levels = TRUE, 
         type = "dens_overlay",
         ndraws = 100) + 
  scale_x_continuous(limits = c(-100, 100))
pp_check(fit_scores, resp = "awayscore", 
         newdata = train_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)
pp_check(fit_scores, resp = "homescore", 
         newdata = test_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)
pp_check(fit_scores, resp = "awayscore", 
         newdata = test_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)

pp_check(fit_scores, resp = "result", 
         newdata = train_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)

# pp_check(fit_scores, resp = "homescore", 
#          newdata = train_data, 
#          allow_new_levels = TRUE, 
#          type = "bars",
#          ndraws = 100)
# pp_check(fit_scores, resp = "awayscore", 
#          newdata = train_data, 
#          allow_new_levels = TRUE,  
#          type = "bars",
#          ndraws = 100)
# pp_check(fit_scores, resp = "homescore", 
#          newdata = test_data, 
#          allow_new_levels = TRUE,  
#          type = "bars",
#          ndraws = 100)
# pp_check(fit_scores, resp = "awayscore", 
#          newdata = test_data, 
#          allow_new_levels = TRUE,  
#          type = "bars",
#          ndraws = 100)


## 5.3 PPD ----

sampleID <- sample(1:sims, 200, replace = FALSE)
ppc_bars(
  y = as.numeric(as.character(test_data$home_score)),
  yrep = round(posterior_homescore[sampleID,])
)
ppc_bars(
  y = as.numeric(as.character(test_data$away_score)),
  yrep = round(posterior_awayscore[sampleID,])
)
ppc_bars(
  y = as.numeric(as.character(test_data$result)),
  yrep = round(posterior_result[sampleID,])
)
ppc_bars(
  y = as.numeric(as.character(test_data$result)),
  yrep = round(posterior_homescore[sampleID,]) - round(posterior_awayscore[sampleID,])
)
ppc_bars(
  y = as.numeric(as.character(test_data$total)),
  yrep = round(posterior_total[sampleID,])
)
ppc_bars(
  y = as.numeric(as.character(test_data$total)),
  yrep = round(posterior_homescore[sampleID,]) + round(posterior_awayscore[sampleID,])
)

ppc_dens_overlay(
  y = as.numeric(as.character(test_data$home_score)),
  yrep = posterior_homescore[sampleID,]
) +
  ppc_dens_overlay(
    y = as.numeric(as.character(test_data$away_score)),
    yrep = posterior_awayscore[sampleID,]
  ) +
  ppc_dens_overlay(
    y = as.numeric(as.character(test_data$result)),
    yrep = posterior_result[sampleID,]
  ) +
  ppc_dens_overlay(
    y = as.numeric(as.character(test_data$total)),
    yrep = posterior_total[sampleID,]
  ) 

ppc_dens_overlay(
  y = as.numeric(as.character(test_data$result)),
  yrep = posterior_result[sampleID,]
)
ppc_bars(
  y = as.numeric(as.character(test_data$result)),
  yrep = discrete_mat[sampleID,]
)

## 5.4 Conditional Effects ----
Fitsmooth <- conditional_smooths(fit_scores, 
                                 resp = "homescore", 
                                 method = "posterior_predict"
)
plot(Fitsmooth,
     stype = "contour",
     ask = FALSE)

conditional_eff <- conditional_effects(
  fit_scores,
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


# 6. Performance ----
### MAE ----
# Performance Metrics ----
## Test ----
### MAE
MAE_pred_homescore <- mean(abs(colMeans(posterior_homescore) - 
                                 as.numeric(as.character(test_data$home_score))))
MAE_pred_awayscore <- mean(abs(colMeans(posterior_awayscore) - 
                                 as.numeric(as.character(test_data$away_score))))
MAE_pred_result <- mean(abs(colMeans(posterior_result) - test_data$result))
MAE_pred_result <- mean(abs(colMeans(discrete_mat) - test_data$result))
MAE_pred_total <- mean(abs(colMeans(posterior_total) - test_data$total))

MAE_pred_homescore
MAE_pred_awayscore
MAE_pred_result
MAE_pred_total

### MAD
MAD_pred_homescore <- mean(abs(apply(posterior_homescore, 2, function(x){quantile(x, 0.5)}) - 
                                 as.numeric(as.character(test_data2$home_score))))
MAD_pred_awayscore <- mean(abs(apply(posterior_awayscore, 2, function(x){quantile(x, 0.5)}) - 
                                 as.numeric(as.character(test_data2$away_score))))
MAD_pred_result <- mean(abs(apply(posterior_result, 2, function(x){quantile(x, 0.5)}) - test_data$result))
MAD_pred_total <- mean(abs(apply(posterior_total, 2, function(x){quantile(x, 0.5)}) - test_data$total))

MAD_pred_homescore
MAD_pred_awayscore
MAD_pred_result
MAD_pred_total

### RMSE
RMSE_pred_homescore <- Metrics::rmse(colMeans(posterior_homescore),
                                     as.numeric(as.character(test_data$home_score)))
RMSE_pred_awayscore <- Metrics::rmse(colMeans(posterior_awayscore),
                                     as.numeric(as.character(test_data$away_score)))
RMSE_pred_result <- Metrics::rmse(colMeans(posterior_result), test_data$result)
Metrics::rmse(colMeans(discrete_mat), test_data$result)
RMSE_pred_total <- Metrics::rmse(colMeans(posterior_total), test_data$total)

RMSE_pred_homescore
RMSE_pred_awayscore
RMSE_pred_result
RMSE_pred_total


# Betting ----
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

# Return on Investment ----
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


# Compute Bet Results ----
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

# Result ----
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

# Total ----
betting_accuracy_total <- compute_betting_accuracy(
  posterior_total,
  betting_df,
  target = c("total"),
  vegas_line_col = NULL,
  vegas_prob_col1 = NULL,
  vegas_prob_col2 = NULL,
  actual_col = NULL,
  xgb_pred_col = NULL,
  prob_threshold = 0.6,
  group_vars = NULL
)
betting_accuracy_total$summary

roi_table_total_historic <- compute_roi_table(
  posterior_matrix = posterior_total, 
  new_data = betting_df,
  target = "total",
  threshold_grid = seq(0.50, 0.75, by = 0.001),
  default_odds = -110,
  use_historic_odds = TRUE,
  stake = 100)
#print(roi_table_total_historic, n =nrow(roi_table_total_historic))

roi_table_total_default <- compute_roi_table(
  posterior_matrix = posterior_total, 
  new_data = betting_df,
  target = "total",
  threshold_grid = seq(0.50, 0.75, by = 0.01),
  default_odds = -110,
  use_historic_odds = FALSE,
  stake = 100)
#print(roi_table_total_default, n =nrow(roi_table_total_default))

roi_table_total <-
  bind_cols(
    roi_table_total_default,
    roi_table_total_historic |> select(accuracy_hist = accuracy,
                                       roi_hist = roi, 
                                       profit_hist = profit)
  )
print(roi_table_total, n = nrow(roi_table_total))

# Finally, plot ROI vs. threshold
ggplot(roi_table_total_historic, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold - total",
    x = "Posterior Threshold",
    y = "ROI"
  )

ggplot(roi_table_total_historic, aes(x = threshold, y = profit)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profit by Posterior Betting Confidence Threshold - total",
    x = "Posterior Threshold",
    y = "Profit"
  )



