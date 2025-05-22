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
  # left_join(
  #   finalScoresData |> 
  #     select(game_id, team, matches("^points\\d+$")) |>
  #     rename_with(~paste0("home_", .x), .cols = -c(game_id)),
  #   by = join_by(game_id, home_team)
  # ) |>
  # left_join(
  #   finalScoresData |> 
  #     select(game_id, team, matches("^points\\d+$")) |>
  #     rename_with(~paste0("away_", .x), .cols = -c(game_id)),
  #   by = join_by(game_id, away_team)
  # ) |>
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

model_data <- modDataBase |> filter(season >= 2007) |>
  mutate(
    winner = case_when(
      home_team == winner ~ TRUE,
      away_team == winner ~ FALSE,
      TRUE ~ NA
    ),
    winner = factor(winner, levels = c(FALSE, TRUE), labels = c("Away", "Home"))
  ) # |>
# mutate(
#   #location = factor(location, levels = c("Neutral", "Home")),
#   location = ifelse(location == "Home", 1, 0),
#   .after = location
# )
# modData <- modData |> 
#   filter(!is.na(winner))

model_data_long <- model_data |> clean_homeaway(invert = c("result", "spread_line"))

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
  "season","week","weekday","time_of_day","location", #"is_home",
  "div_game","home_rest","away_rest","roof","surface","temp","wind"
)

base_cols <- colnames(model_data)[colnames(model_data) %in% c(drop_vars, game_level_vars)]

# 2.2 Subset raw feature columns
# Note: brms_data should be your modData wide-format dataframe
brms_data <- model_data  # adjust if necessary

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
brms_data_base <- model_data |>
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
  relocate(week_seq, .after = week) |>
  mutate(
    result_fac = factor(result, ordered = TRUE),
    .after = result
  )


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. MODEL SPECIFICATION ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Result
brms_formula_result <- 
  bf(
    result_fac ~
      #0 + Intercept +
      net_elo +
      #net_elo_pre +
      net_rating +
      #hfa_pre +
      #net_rating_hfa +
      home_net_off_red_zone_app_perc_cum 
    #is_home +
    #location +
    #(1|home_team) 
    #gp(season, by = home_team)
    #gp(week, by = home_team)
    #ar(gr = home_team)
    #(is_home|gr(home_team)) 
    #(1|away_team)
  ) + 
  brmsfamily(family = "cumulative", link = "logit", threshold = "flexible")
#brmsfamily(family = "cumulative", link = "logit")
#brmsfamily(family = "student", link = "identity")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. PREPROCESSING ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Create recipe to be called on for any model
recipe_result <- brms_data |>
  recipe() |>
  # 1) mark the response
  update_role(result, result_fac, new_role = "outcome") |>
  # 2) mark your numeric inputs
  update_role(
    home_team,
    away_team,
    season,
    week,
    week_seq,
    net_elo,
    net_rating,
    hfa_pre,
    home_net_off_red_zone_app_perc_cum,
    new_role = "predictor"
  ) |>
  # 3) mark your grouping factors for brms
  add_role(
    home_team,
    away_team,
    new_role = "group"
  ) |>
  update_role(
    game_id, season, week,
    new_role = "id"
  ) |>
  update_role(
    net_elo,
    net_rating,
    hfa_pre,
    home_net_off_red_zone_app_perc_cum,
    new_role = "predictor"
  ) |>
  # drop unused levels of result_fac
  step_mutate( 
    result_fac = droplevels(result_fac)
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
preProc_result

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. FIT MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formula_result <- 
  bf(
    result ~ 
      #0 + Intercept +
      gp(season) +
      gp(week) +
      gp(season, by = home_team) +
      gp(season, by = away_team) +
      gp(week, by = home_team) +
      gp(week, by = away_team)
  ) + brmsfamily(family = "gaussian")

iters <- 4000
burn <- 2000
chains <- 2
sims <- (iters-burn)*chains

fit_result <- brm(
  formula_result,
  data = brms_data,
  #prior = priors_result,
  #drop_unused_levels = FALSE,
  save_pars = save_pars(all = TRUE), 
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = parallel::detectCores(),
  init = 0,
  #normalize = TRUE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  algorithm = "meanfield",
  seed = 52
)

fit_list <- list()
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
variables(fit_result)

print(fit_list[[27]], digits = 4)
fixef(fit_list[[27]])
ranef(fit_list[[27]])

pp_check(
  fit_result, 
  newdata = brms_data,
  #re_formula = NULL,
  allow_new_levels = TRUE,
  ndraws = 100
)

cond_effects <- conditional_effects(
  fit_result,
  #spaghetti = TRUE,
  ndraws = 100,
  plot = FALSE
)
plot(
  cond_effects,
  ask = FALSE
)




