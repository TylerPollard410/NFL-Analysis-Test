# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(tictoc)
# library(tidytext)
# library(MASS)
library(plotly)
library(smplot2)
library(patchwork)
# library(doParallel)
# library(rBayesianOptimization)
# library(xgboost)
# library(caret)
#library(rstan)
library(cmdstanr)
#library(brms)
library(posterior)
library(bayesplot)
library(Metrics) # for MAE, RMSE
#library(vip)
library(broom.mixed)
library(tidybayes)
#library(discrim)
#library(bayesian)
#library(timetk)
#library(modeltime)
#library(tidymodels)

library(nflverse)
library(tidyverse)

# detach("package:nflendzonePipeline",unload = TRUE, force = TRUE)
# install.packages(".", repos = NULL, type = "source")
# pak::pak("TylerPollard410/nflendzone")
library(nflendzonePipeline)
library(nflendzone)

set.seed(52)
options(scipen = 10)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 1. DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Global Variables ----
all_seasons <- 2002:get_current_season()
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
github_data_repo <- "TylerPollard410/nflendzoneData"

## nflverse ----
teams_data <- load_teams(current = TRUE)
teams <- teams_data$team_abbr

### games ----
# game_data <- load_game_data(seasons = all_seasons)
# game_data_long <- game_data |>
# clean_homeaway(invert = c("result", "spread_line"))

### release data ----
# tag <- "game_features"
# game_features_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

# tag <- "game_model"
# game_model_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

## Set up modeling data ----
game_data <- load_game_data(seasons = all_seasons) |>
    mutate(
        game_idx = row_number(),
        season_idx = as.integer(as.factor(season)),
        week_idx = week_seq,
        fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
        lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
        home_idx = match(home_team, teams),
        away_idx = match(away_team, teams),
        hfa = as.integer(ifelse(location == "Home", 1, 0))
    )

# game_fit_data <- game_data |>
#     select(
#         game_idx,
#         season_idx,
#         week_idx,
#         fw_season_idx,
#         lw_season_idx,
#         home_idx,
#         away_idx,
#         hfa,
#         home_score,
#         away_score,
#         result,
#         total,
#         season,
#         week,
#         home_team,
#         away_team
#     )

fit_stan_data <- game_data |>
    filter(season < 2006) |>
    select(
        season_idx,
        week_idx,
        fw_season_idx,
        lw_season_idx,
        home_team,
        away_team,
        hfa,
        home_score,
        away_score,
        result,
        total
    ) |>
    compose_data(
        .n_name = n_prefix("N"),
        N_games = N,
        N_teams = max(N_home_team, N_away_team),
        N_seasons = length(unique(season_idx)),
        N_weeks = length(unique(week_idx))
    )
glimpse(fit_stan_data)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. RUN STAN HELPERS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

source("Model Fitting/off_def_stan/stan_helpers.R")

fit_stan_data2 <- create_stan_data(before_season = 2006, verbose = TRUE)
glimpse(fit_stan_data2)

# Test creating forecast data for 2006 season using your existing fit_stan_data
forecast_data_2006 <- create_forecast_data(
    previous_stan_data = fit_stan_data,
    forecast_seasons = 2006,
    verbose = TRUE
)

# Check the structure
glimpse(forecast_data_2006)

# Test extending existing data
extended_data <- extend_stan_data_with_forecast(
    base_stan_data = fit_stan_data,
    forecast_seasons = 2006,
    max_week = 4, # Just first 4 weeks for testing
    verbose = TRUE
)

cat("\nExtended data dimensions:\n")
cat("Games:", extended_data$N_games, "\n")
cat("Original games:", fit_stan_data$N_games, "\n")
cat("Forecast games:", extended_data$N_games - fit_stan_data$N_games, "\n")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. STAN ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Compile Model ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod1.stan")

fit_mod <- cmdstan_model(
    fit_path,
    compile_model_methods = TRUE,
    force_recompile = TRUE,
    pedantic = TRUE
)
fit_mod_vars <- fit_mod$variables()

## Fit Model ----
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

{
    timer <- .print_time(start = TRUE, msg = "Fitting Model")
    fit0 <- fit_mod$sample(
        data = fit_stan_data,
        seed = 52,
        init = fit_init,
        chains = fit_chains,
        parallel_chains = min(fit_chains, parallel::detectCores() - 1),
        iter_warmup = fit_warm,
        iter_sampling = fit_samps,
        adapt_delta = fit_adapt_delta,
        max_treedepth = fit_max_treedepth
    )
    .print_time(start = FALSE, timer = timer)
}
