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
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/datafinalScoresData.rda"))

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
  relocate(matches("^away_points\\d+$"), .after = away_score)

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
  "home_score", "home_points8", "home_points7", "home_points6", "home_points3", "home_points2",
  "away_score", "away_points8", "away_points7", "away_points6", "away_points3", "away_points2",
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

## 3.1 Home Points ----
brms_home_points8 <-
  bf(
    home_points8 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_home_points7 <-
  bf(
    home_points7 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_home_points6 <-
  bf(
    home_points6 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_home_points3 <-
  bf(
    home_points3 ~ 
      (1|HFG|home_team) +
      (1|AFG|away_team)
  ) + brmsfamily(family = "poisson")

brms_home_points2 <-
  bf(
    home_points2 ~ 
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "poisson")

## 3.2 Away Points ----
brms_away_points8 <-
  bf(
    away_points8 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_away_points7 <-
  bf(
    away_points7 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_away_points6 <-
  bf(
    away_points6 ~ 
      (1|HTD|home_team) +
      (1|ATD|away_team)
  ) + brmsfamily(family = "poisson")

brms_away_points3 <-
  bf(
    away_points3 ~ 
      (1|HFG|home_team) +
      (1|AFG|away_team)
  ) + brmsfamily(family = "poisson")

brms_away_points2 <-
  bf(
    away_points2 ~ 
      (1|home_team) +
      (1|away_team)
  ) + brmsfamily(family = "poisson")


## 3.3 Combine Scores Formulas ----
brms_points <- 
  brms_home_points8 +
  brms_home_points7 +
  brms_home_points6 +
  brms_home_points3 +
  brms_home_points2 +
  brms_away_points8 +
  brms_away_points7 +
  brms_away_points6 +
  brms_away_points3 +
  brms_away_points2

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

## 4.2 Preprocess ----
# Identify predictors for the winner model
brms_vars_points <- extract_predictors(brms_data, brms_points)
brms_vars_points


preProc_points <- preProcess(
  brms_data |> 
    filter(season %in% 2021:2023) |>
    select(-all_of(base_cols)),
  method = c("center", "scale")
)
train_data <- predict(preProc_points, brms_data |> filter(season %in% 2021:2023))
test_data <- predict(preProc_points, brms_data |> filter(season == 2024))

default_prior(brms_points, data = train_data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. FIT MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

## 5.1 Fit ----
system.time(
  fit_points <- brm(
    brms_points,
    data = train_data,
    #prior = priors_Team,
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


posterior_points <- posterior_predict(
  fit_points,
  re_formula = NULL,
  allow_new_levels = TRUE
)

posterior_homepoints8 <- posterior_points[,,"homepoints8"]
posterior_homepoints7 <- posterior_points[,,"homepoints7"]
posterior_homepoints6 <- posterior_points[,,"homepoints6"]
posterior_homepoints3 <- posterior_points[,,"homepoints3"]
posterior_homepoints2 <- posterior_points[,,"homepoints2"]

posterior_awaypoints8 <- posterior_points[,,"awaypoints8"]
posterior_awaypoints7 <- posterior_points[,,"awaypoints7"]
posterior_awaypoints6 <- posterior_points[,,"awaypoints6"]
posterior_awaypoints3 <- posterior_points[,,"awaypoints3"]
posterior_awaypoints2 <- posterior_points[,,"awaypoints2"]

posterior_home_score <- 
  posterior_homepoints8*8 +
  posterior_homepoints7*7 +
  posterior_homepoints6*6 +
  posterior_homepoints3*3 +
  posterior_homepoints2*2

posterior_away_score <- 
  posterior_awaypoints8*8 +
  posterior_awaypoints7*7 +
  posterior_awaypoints6*6 +
  posterior_awaypoints3*3 +
  posterior_awaypoints2*2

posterior_result <- posterior_home_score - posterior_away_score
posterior_total <- posterior_home_score + posterior_away_score

## 5.2 PPC ----
### 5.2.1 Points ----
ppc_bars(
  train_data$home_points8,
  posterior_homepoints8
) + labs(title = "PPC Home Points8")

ppc_bars(
  train_data$home_points7,
  posterior_homepoints7
) + labs(title = "PPC Home Points7")

ppc_bars(
  train_data$home_points6,
  posterior_homepoints6
) + labs(title = "PPC Home Points6")

ppc_bars(
  train_data$home_points3,
  posterior_homepoints3
) + labs(title = "PPC Home Points3")

ppc_bars(
  train_data$home_points2,
  posterior_homepoints2
) + labs(title = "PPC Home Points2")

ppc_bars(
  train_data$away_points8,
  posterior_awaypoints8
) + labs(title = "PPC away Points8")

ppc_bars(
  train_data$away_points7,
  posterior_awaypoints7
) + labs(title = "PPC away Points7")

ppc_bars(
  train_data$away_points6,
  posterior_awaypoints6
) + labs(title = "PPC away Points6")

ppc_bars(
  train_data$away_points3,
  posterior_awaypoints3
) + labs(title = "PPC away Points3")

ppc_bars(
  train_data$away_points2,
  posterior_awaypoints2
) + labs(title = "PPC away Points2")

### 5.2.2 Home Score ----
ppc_bars(
  train_data$home_score,
  posterior_home_score,
  prob = 0
) + 
  labs(title = "PPC Home Score") +
  scale_x_continuous(limits = c(0,70))

### 5.2.3 Away Score ----
ppc_bars(
  train_data$away_score,
  posterior_away_score,
  prob = 0
) + labs(title = "PPC Away Score") +
  scale_x_continuous(limits = c(0,70))

### 5.2.4 Result ----
ppc_bars(
  train_data$result,
  posterior_result,
  prob = 0
) + labs(title = "PPC Result") +
  scale_x_continuous(limits = c(-70,70))

### 5.2.5 Total ----
ppc_bars(
  train_data$total,
  posterior_total,
  prob = 0
) + labs(title = "PPC Total")


## 5.3 PPC ----
posterior_points <- posterior_predict(
  fit_points,
  newdata = test_data,
  re_formula = NULL,
  allow_new_levels = TRUE
)

posterior_homepoints8 <- posterior_points[,,"homepoints8"]
posterior_homepoints7 <- posterior_points[,,"homepoints7"]
posterior_homepoints6 <- posterior_points[,,"homepoints6"]
posterior_homepoints3 <- posterior_points[,,"homepoints3"]
posterior_homepoints2 <- posterior_points[,,"homepoints2"]

posterior_awaypoints8 <- posterior_points[,,"awaypoints8"]
posterior_awaypoints7 <- posterior_points[,,"awaypoints7"]
posterior_awaypoints6 <- posterior_points[,,"awaypoints6"]
posterior_awaypoints3 <- posterior_points[,,"awaypoints3"]
posterior_awaypoints2 <- posterior_points[,,"awaypoints2"]

posterior_home_score <- 
  posterior_homepoints8*8 +
  posterior_homepoints7*7 +
  posterior_homepoints6*6 +
  posterior_homepoints3*3 +
  posterior_homepoints2*2

posterior_away_score <- 
  posterior_awaypoints8*8 +
  posterior_awaypoints7*7 +
  posterior_awaypoints6*6 +
  posterior_awaypoints3*3 +
  posterior_awaypoints2*2

posterior_result <- posterior_home_score - posterior_away_score
posterior_total <- posterior_home_score + posterior_away_score

### 5.3.1 Points ----
ppc_bars(
  test_data$home_points8,
  posterior_homepoints8
) + labs(title = "PPC Home Points8")

ppc_bars(
  test_data$home_points7,
  posterior_homepoints7
) + labs(title = "PPC Home Points7")

ppc_bars(
  test_data$home_points6,
  posterior_homepoints6
) + labs(title = "PPC Home Points6")

ppc_bars(
  test_data$home_points3,
  posterior_homepoints3
) + labs(title = "PPC Home Points3")

ppc_bars(
  test_data$home_points2,
  posterior_homepoints2
) + labs(title = "PPC Home Points2")

ppc_bars(
  test_data$away_points8,
  posterior_awaypoints8
) + labs(title = "PPC away Points8")

ppc_bars(
  test_data$away_points7,
  posterior_awaypoints7
) + labs(title = "PPC away Points7")

ppc_bars(
  test_data$away_points6,
  posterior_awaypoints6
) + labs(title = "PPC away Points6")

ppc_bars(
  test_data$away_points3,
  posterior_awaypoints3
) + labs(title = "PPC away Points3")

ppc_bars(
  test_data$away_points2,
  posterior_awaypoints2
) + labs(title = "PPC away Points2")

### 5.3.2 Home Score ----
ppc_bars(
  test_data$home_score,
  posterior_home_score,
  prob = 0
) + labs(title = "PPC Home Score")

### 5.3.3 Away Score ----
ppc_bars(
  test_data$away_score,
  posterior_away_score,
  prob = 0
) + labs(title = "PPC Away Score")

### 5.3.4 Result ----
ppc_bars(
  test_data$result,
  posterior_result,
  prob = 0
) + labs(title = "PPC Result")

### 5.3.5 Total ----
ppc_bars(
  test_data$total,
  posterior_total,
  prob = 0
) + labs(title = "PPC Total")








