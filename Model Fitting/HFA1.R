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

tag <- "game_model"
game_model_data <- rds_from_url(paste0(base_repo_url, 
                                       tag, "/",
                                       tag, ".rds"))

tag <- "nfl_stats_week_team_regpost"
nfl_stats_week_team_regpost <- rds_from_url(paste0(base_repo_url, 
                                                   tag, "/",
                                                   tag, ".rds"))
tag <- "nfl_stats_week_player_regpost"
nfl_stats_week_player_regpost <- rds_from_url(paste0(base_repo_url, 
                                                     tag, "/",
                                                     tag, ".rds"))


#   - Use seasons 2007–2021 for training/validation
#   - Hold out seasons 2022–2024 for final testing
game_model_data <- game_model_data |>
  mutate(
    hfa = factor(location, levels = c("Neutral", "Home")),
    hfa0 = factor(ifelse(location == "Home", 1, 0),
                  levels = c(0, 1), 
                  labels = c("Neutral", "Home"))
    )

train_start_idx <- game_model_data |>
  filter(season == 2022, week == 1) |>
  pull(week_seq) |> unique()
train_end_idx <- game_model_data |>
  filter(season == 2024, week == 8) |>
  pull(week_seq) |> unique()

test_start_idx <- game_model_data |>
  filter(season == 2024, week == 9) |>
  pull(week_seq) |> unique()
test_end_idx <- game_model_data |>
  filter(season == 2025, week == 1) |>
  pull(week_seq) |> unique()

train_data <- game_model_data |> filter(between(week_seq, train_start_idx, train_end_idx))
test_data <- game_model_data |> filter(between(week_seq, test_start_idx, test_end_idx))

## Define initial window of weeks (warm-up: Seasons 2007–2009)
initial_window <- game_model_data |> 
  filter(season %in% 2007:2009) |> 
  pull(week_seq) |> 
  max()

# Determine all weeks after warm-up
weeks <- sort(unique(train_data$week_seq))
fold_weeks <- weeks[weeks > initial_window]

# Determine seasons to fold (after warm-up seasons)
seasons <- sort(unique(train_data$season))
warmup_seasons <- 2007:2009
fold_seasons <- seasons[seasons > max(warmup_seasons)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

train_data <- train_data |> mutate(homeWeight = 1, awayWeight = -1)
test_data <- test_data |> mutate(homeWeight = 1, awayWeight = -1) |>
  filter(!is.na(result))

## 2.1 Formula ----
hfa_formula <- bf(
  result ~ 0 + Intercept +
    #hfa +
    s(week_seq) +
    (hfa|mm(home_team, away_team, 
          weights = cbind(homeWeight, awayWeight),
          scale = FALSE))
) + brmsfamily(family = "student")

default_prior(hfa_formula, train_data)

iters <- 4000
burn <- 2000
chains <- 1
sims <- (iters-burn)*chains

## Stancode
hfa_stancode <- stancode(
  hfa_formula,
  data = train_data,
  #prior = priors_result,
  drop_unused_levels = FALSE,
  save_pars = save_pars(all = TRUE), 
  chains = chains,
  iter = iters,
  warmup = burn,
  cores = parallel::detectCores(),
  #init = 0,
  #normalize = TRUE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 52
)
hfa_stancode

## 2.2 Fit ----
system.time(
  hfa_fit <- brm(
    hfa_formula,
    data = train_data,
    #prior = priors_result,
    drop_unused_levels = FALSE,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    #normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 52
  )
)

### Check Fit ----
print(hfa_fit, digits = 4)
fixef(hfa_fit)
ranef(hfa_fit)

## 2.3 Posterior ----
set.seed(52)
train_posterior <- posterior_predict(
  hfa_fit,
  newdata = train_data,
  ndraws = sims,
  re_formula = NULL,
  allow_new_levels = TRUE
)

set.seed(52)
test_posterior <- posterior_predict(
  hfa_fit,
  newdata = test_data,
  ndraws = sims,
  re_formula = NULL,
  allow_new_levels = TRUE
)

set.seed(52)
sampleID <- sample(1:sims, 200, replace = FALSE)

## 2.4 PPC ----
pp_check(hfa_fit, 
         newdata = train_data, 
         allow_new_levels = TRUE,  
         type = "dens_overlay",
         ndraws = 100)

# ppc_bars(
#   y = as.numeric(as.character(train_data$result)),
#   yrep = train_posterior[sampleID,]
# )
ppc_dens_overlay(
  y = train_data$result,
  yrep = train_posterior[sampleID,]
) +
  labs(title = "PPC")


## 2.5 PPD ----
# ppc_bars(
#   y = test_data$result,
#   yrep = test_posterior[sampleID,]
# )
ppc_dens_overlay(
  y = test_data$result,
  yrep = test_posterior[sampleID,]
) +
  labs(title = "PPD")


## 2.6 Conditional Effects ----
Fitsmooth <- conditional_smooths(fit_result, 
                                 resp = "result", 
                                 method = "posterior_predict"
)
plot(Fitsmooth,
     stype = "contour",
     ask = FALSE)

conditional_eff <- conditional_effects(
  hfa_fit,
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





























