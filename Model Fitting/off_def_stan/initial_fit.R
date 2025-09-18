# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(magrittr)
library(tictoc)
library(plotly)
library(smplot2)
library(patchwork)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(Metrics)
library(broom.mixed)
library(tidybayes)

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

fit_stan_data <- create_stan_data(before_season = 2006, verbose = TRUE)
glimpse(fit_stan_data)

## Test function ----

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. STAN ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Compile Model ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
    fit_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
fit_mod_vars <- fit_mod$variables()

gq_mod <- cmdstan_model(
    gq_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
gq_mod_vars <- gq_mod$variables()

## Fit Model ----
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 500
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
        #output_dir = fit_root,
        #output_basename = "fit",
        sig_figs = fit_sig_figs,
        chains = fit_chains,
        parallel_chains = min(fit_chains, parallel::detectCores() - 1),
        iter_warmup = fit_warm,
        iter_sampling = fit_samps,
        adapt_delta = fit_adapt_delta,
        max_treedepth = fit_max_treedepth
    )
    .print_time(start = FALSE, timer = timer)
}
fit0$output_files()
fit0$save_output_files(
    dir = file_root,
    basename = "fit",
    random = FALSE,
    timestamp = FALSE
)

fit0$print()
fit0$diagnostic_summary()
fit0$cmdstan_diagnose()
#fit0$cmdstan_summary()

fit0 %<>% recover_types(game_data)

summ <- fit0$summary()
worst <- summ |>
    arrange(desc(rhat))
print(worst %>% filter(rhat > 1.01 | ess_bulk < 1000), n = 30)
mcmc_rhat(worst$rhat)
mcmc_neff(worst$ess_bulk)

fit0_draws <- fit0 |> tidy_draws()

fit0_meta <- fit0$metadata()
fit0_params <- names(fit_mod_vars$parameters)
fit_mod_params <- fit_mod_vars$parameters |>
    keep(~ .x$dimensions < 2) |>
    names()
# fit0_var_skel <- fit0$variable_skeleton(
#     transformed_parameters = FALSE,
#     generated_quantities = FALSE
# )
fit0_param_sum <- fit0$summary(variables = fit_mod_params)
fit0_param_sum |> print(n = Inf)

fit0_latent_draws <- fit0$draws(
    variables = c("league_hfa", "team_hfa", "team_strength"),
    format = "df"
)
fit0_latent_draws <- fit0 |>
    spread_draws(
        filtered_team_strength_last[team],
        filtered_team_hfa_last[team],
        filtered_league_hfa_last,
        predicted_team_strength_next_mean[team],
        predicted_team_strength_next_draw[team]
    )
fit0_latent_sum <- fit0_latent_draws |>
    summarize_draws()


# Create forecast data for 1 week ahead
forecast_data <- extend_for_forecast(fit_stan_data, forecast_weeks = 1)
forecast_week_idx <- max(forecast_data$week_idx)
forecast_season <- unique(forecast_data$season)

# Make forecast stan data list from forecast data
forecast_stan_data <- create_stan_data(
    specific_seasons = unique(forecast_data$season),
    min_week = min(forecast_data$week),
    max_week = max(forecast_data$week)
)
glimpse(forecast_stan_data)

# Make forecast stan data list from forecast data
fit_data <- filter_seasons(fit_stan_data)
roll_forecast_stan_data <- create_stan_data(
    specific_seasons = unique(c(fit_data$season, forecast_data$season)),
    min_week = 1,
    max_week = max(forecast_data$week)
)

roll_forecast_stan_data <- map2(
    fit_stan_data,
    forecast_stan_data,
    c
) |>
    compose_data(
        N_oos = last(N),
        N = first(N),
        N_games = sum(N_games),
        N_teams = unique(N_teams),
        N_seasons = sum(N_seasons),
        N_weeks = sum(N_weeks)
    )
# modify_at(
#     .at = c("N", "N_games", "N_seasons", "N_weeks"),
#     sum
# ) |>
# modify_at(
#     .at = "N_teams",
#     unique
# )

glimpse(fit_stan_data)
glimpse(forecast_stan_data)
glimpse(roll_forecast_stan_data)


## Generate Forecasts ----
fit_current_season <- get_last_fitted_season(
    fit_stan_data
)
fit_current_week <- get_last_fitted_week(
    fit_stan_data
)
gq_schedule_idx <- prepare_schedule_indices(
    seasons = all_seasons,
    teams = teams
)
fit_current_week_idx <- gq_schedule_idx |>
    filter(season_idx == fit_current_season) |>
    filter(week_idx == fit_current_week)
gq_schedule_subset <- build_oos_from_schedule(
    schedule_df = gq_schedule_idx |>
        filter(week_idx == fit_current_week + 1)
)
gq_future_meta <- build_future_meta(
    fit_stan_data = fit_stan_data,
    schedule_df = gq_schedule_idx,
    horizon = 1
)
gq_stan_data <- prepare_gq_data(
    fit_stan_data = fit_stan_data,
    schedule_df = gq_schedule_idx,
    targets = fit_current_week + 1
)


{
    timer <- .print_time(start = TRUE, msg = "Generating Forecasts")
    gq0 <- gq_mod$generate_quantities(
        fitted_params = fit0,
        data = gq_stan_data,
        seed = fit_seed,
        #output_dir = gq_output_dir,
        #output_basename = gq_output_basename,
        sig_figs = fit_sig_figs,
        parallel_chains = fit_parallel
    )
    .print_time(start = FALSE, timer = timer)
}
gq0$output_files()
gq0$save_output_files(
    dir = file_root,
    basename = "gq",
    random = FALSE,
    timestamp = FALSE
)

gq0$print(max_rows = 1000)

gq0 %<>% recover_types(game_data)

summ <- gq0$summary()

gq0_draws <- gq0 |> tidy_draws()

gq0_meta <- gq0$metadata()
gq_params <- names(gq_mod_vars$generated_quantities)
gq_mod_params <- gq_mod_vars$parameters |>
    keep(~ .x$dimensions < 2) |>
    names()
# gq0_var_skel <- gq0$variable_skeleton(
#     transformed_parameters = FALSE,
#     generated_quantities = FALSE
# )
gq0_param_sum <- gq0$summary(variables = gq_params)
gq0_param_sum |> print(n = Inf)

gq0_latent_draws <- gq0$draws(
    variables = c("league_hfa", "team_hfa", "team_strength"),
    format = "df"
)
gq0_latent_draws <- gq0 |>
    spread_draws(
        filtered_team_strength_last[team],
        filtered_team_hfa_last[team],
        filtered_league_hfa_last,
        predicted_team_strength_next_mean[team],
        predicted_team_strength_next_draw[team],
        predicted_league_hfa_next_mean
    ) |>
    mutate(team = teams[team])
gq0_latent_sum <- gq0_latent_draws |>
    summarize_draws()
gq0_latent_sum


## Test function ----

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. ONE STEP UPDATE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Compile Model ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
    fit_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
fit_mod_vars <- fit_mod$variables()

gq_mod <- cmdstan_model(
    gq_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
gq_mod_vars <- gq_mod$variables()

## Fit Model ----
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 500
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

str(fit_stan_data)
last_fit_indexes <- filter_seasons(fit_stan_data)
str(last_fit_indexes)
next_fit_indexes <- extend_for_forecast(last_fit_indexes, forecast_weeks = 1)
str(next_fit_indexes)
updated_fit_data <- create_stan_data(
    specific_seasons = next_fit_indexes$season,
    min_week = min(next_fit_indexes$week),
    max_week = max(next_fit_indexes$week)
)

fit_stan_data_update <- extend_stan_data_with_forecast(
    base_stan_data = fit_stan_data,
    forecast_seasons = min(next_fit_indexes$week),
    forecast_weeks = max(next_fit_indexes$week)
)

{
    timer <- .print_time(start = TRUE, msg = "Fitting Model")
    fit0 <- fit_mod$sample(
        data = fit_stan_data,
        seed = 52,
        init = fit_init,
        #output_dir = fit_root,
        #output_basename = "fit",
        sig_figs = fit_sig_figs,
        chains = fit_chains,
        parallel_chains = min(fit_chains, parallel::detectCores() - 1),
        iter_warmup = fit_warm,
        iter_sampling = fit_samps,
        adapt_delta = fit_adapt_delta,
        max_treedepth = fit_max_treedepth
    )
    .print_time(start = FALSE, timer = timer)
}
fit0$output_files()
fit0$save_output_files(
    dir = file_root,
    basename = "fit",
    random = FALSE,
    timestamp = FALSE
)

fit0$print()
fit0$diagnostic_summary()
fit0$cmdstan_diagnose()
#fit0$cmdstan_summary()

summ <- fit0$summary()
worst <- summ |>
    arrange(desc(rhat))
print(worst %>% filter(rhat > 1.01 | ess_bulk < 1000), n = 30)
mcmc_rhat(worst$rhat)
mcmc_neff(worst$ess_bulk)

fit0_draws <- fit0 |> tidy_draws()

fit0_meta <- fit0$metadata()
fit0_params <- names(fit_mod_vars$parameters)
fit_mod_params <- fit_mod_vars$parameters |>
    keep(~ .x$dimensions < 2) |>
    names()

# Inside your sequential loop after building fit_stan_data_new
prev_metric_type <- fit0$metadata()$metric
prev_inv_metric <- fit0$inv_metric()
params <- names(fit_mod$variables()$parameters)
draws_df <- fit0$draws(variables = params, format = "df")
row_ids <- sample(seq_len(nrow(draws_df)), fit_chains, replace = TRUE)
init_files <- vapply(
    seq_len(fit_chains),
    function(ch) {
        f <- tempfile(fileext = ".json")
        cmdstanr::write_stan_json(
            as.list(draws_df[row_ids[ch], params, drop = FALSE]),
            f
        )
        f
    },
    character(1)
)

sequential_step0 <- sequential_step(
    fit_mod,
    gq_mod,
    fit0,
    fit_stan_data,
    schedule_df = game_data,
    horizon = 1L,
    refit_after = TRUE,
    refit_weeks = 1L,
    seed = 52,
    chains = 4,
    parallel_chains = min(4, parallel::detectCores() - 1L),
    iter_warmup = 500,
    iter_sampling = 1000,
    adapt_delta = 0.9,
    max_treedepth = 10
)

# fit0_var_skel <- fit0$variable_skeleton(
#     transformed_parameters = FALSE,
#     generated_quantities = FALSE
# )
fit0_param_sum <- fit0$summary(variables = fit_mod_params)
fit0_param_sum |> print(n = Inf)

fit0_latent_draws <- fit0$draws(
    variables = c("league_hfa", "team_hfa", "team_strength"),
    format = "df"
)
fit0_latent_draws <- fit0 |>
    spread_draws(
        filtered_team_strength_last[team],
        filtered_team_hfa_last[team],
        filtered_league_hfa_last,
        predicted_team_strength_next_mean[team],
        predicted_team_strength_next_draw[team]
    )
fit0_latent_sum <- fit0_latent_draws |>
    summarize_draws()


# Create forecast data for 1 week ahead
forecast_data <- extend_for_forecast(fit_stan_data, forecast_weeks = 1)
forecast_week_idx <- max(forecast_data$week_idx)
forecast_season <- unique(forecast_data$season)

# Make forecast stan data list from forecast data
forecast_stan_data <- create_stan_data(
    specific_seasons = unique(forecast_data$season),
    min_week = min(forecast_data$week),
    max_week = max(forecast_data$week)
)
glimpse(forecast_stan_data)

# Make forecast stan data list from forecast data
fit_data <- filter_seasons(fit_stan_data)
roll_forecast_stan_data <- create_stan_data(
    specific_seasons = unique(c(fit_data$season, forecast_data$season)),
    min_week = 1,
    max_week = max(forecast_data$week)
)

roll_forecast_stan_data <- map2(
    fit_stan_data,
    forecast_stan_data,
    c
) |>
    compose_data(
        N_oos = last(N),
        N = first(N),
        N_games = sum(N_games),
        N_teams = unique(N_teams),
        N_seasons = sum(N_seasons),
        N_weeks = sum(N_weeks)
    )
# modify_at(
#     .at = c("N", "N_games", "N_seasons", "N_weeks"),
#     sum
# ) |>
# modify_at(
#     .at = "N_teams",
#     unique
# )

glimpse(fit_stan_data)
glimpse(forecast_stan_data)
glimpse(roll_forecast_stan_data)


## Generate Forecasts ----
fit_current_season <- get_last_fitted_season(
    fit_stan_data
)
fit_current_week <- get_last_fitted_week(
    fit_stan_data
)
gq_schedule_idx <- prepare_schedule_indices(
    seasons = all_seasons,
    teams = teams
)
fit_current_week_idx <- gq_schedule_idx |>
    filter(season_idx == fit_current_season) |>
    filter(week_idx == fit_current_week)
gq_schedule_subset <- build_oos_from_schedule(
    schedule_df = gq_schedule_idx |>
        filter(week_idx == fit_current_week + 1)
)
gq_future_meta <- build_future_meta(
    fit_stan_data = fit_stan_data,
    schedule_df = gq_schedule_idx,
    horizon = 1
)
gq_stan_data <- prepare_gq_data(
    fit_stan_data = fit_stan_data,
    schedule_df = gq_schedule_idx,
    targets = fit_current_week + 1
)


{
    timer <- .print_time(start = TRUE, msg = "Generating Forecasts")
    gq0 <- gq_mod$generate_quantities(
        fitted_params = fit0,
        data = gq_stan_data,
        seed = fit_seed,
        #output_dir = gq_output_dir,
        #output_basename = gq_output_basename,
        sig_figs = fit_sig_figs,
        parallel_chains = fit_parallel
    )
    .print_time(start = FALSE, timer = timer)
}
gq0$output_files()
gq0$save_output_files(
    dir = file_root,
    basename = "gq",
    random = FALSE,
    timestamp = FALSE
)

gq0$print(max_rows = 1000)

gq0 %<>% recover_types(game_data)

summ <- gq0$summary()

gq0_draws <- gq0 |> tidy_draws()

gq0_meta <- gq0$metadata()
gq_params <- names(gq_mod_vars$generated_quantities)
gq_mod_params <- gq_mod_vars$parameters |>
    keep(~ .x$dimensions < 2) |>
    names()
# gq0_var_skel <- gq0$variable_skeleton(
#     transformed_parameters = FALSE,
#     generated_quantities = FALSE
# )
gq0_param_sum <- gq0$summary(variables = gq_params)
gq0_param_sum |> print(n = Inf)

gq0_latent_draws <- gq0$draws(
    variables = c("league_hfa", "team_hfa", "team_strength"),
    format = "df"
)
gq0_latent_draws <- gq0 |>
    spread_draws(
        filtered_team_strength_last[team],
        filtered_team_hfa_last[team],
        filtered_league_hfa_last,
        predicted_team_strength_next_mean[team],
        predicted_team_strength_next_draw[team],
        predicted_league_hfa_next_mean
    ) |>
    mutate(team = teams[team])
gq0_latent_sum <- gq0_latent_draws |>
    summarize_draws()
gq0_latent_sum


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 5. FULL BUILD UP ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Build all data from 2002 Week 1 to 2024 Week 22
## Sequential update until no more data to recent games
## of data available

fit_stan_data_full <- create_stan_data(
    specific_seasons = 2002:2024,
    min_week = 1,
    max_week = 22,
    verbose = TRUE
)

joining_game_data <- game_data |>
    select(
        season_idx,
        week_idx,
        home_team,
        away_team,
        season,
        week,
        home_score,
        away_score,
        result
    )

## Compile Model ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
    fit_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
fit_mod_vars <- fit_mod$variables()

gq_mod <- cmdstan_model(
    gq_path,
    compile_model_methods = TRUE,
    force_recompile = FALSE,
    pedantic = TRUE
)
gq_mod_vars <- gq_mod$variables()

## Fit Model ----
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 500
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

{
    timer <- .print_time(start = TRUE, msg = "Fitting Model")
    fit_full <- fit_mod$sample(
        data = fit_stan_data_full,
        seed = 52,
        init = fit_init,
        sig_figs = fit_sig_figs,
        chains = fit_chains,
        parallel_chains = min(fit_chains, parallel::detectCores() - 1),
        iter_warmup = fit_warm,
        iter_sampling = fit_samps,
        adapt_delta = fit_adapt_delta,
        max_treedepth = fit_max_treedepth
    )
    .print_time(start = FALSE, timer = timer)
}
fit_full$output_files()
fit_full$save_output_files(
    dir = file_root,
    basename = "fit",
    random = FALSE,
    timestamp = FALSE
)

fit_full <- as_cmdstan_fit(
    paste0(
        "/Users/tylerpollard/Desktop/NFLAnalysisTest/Model Fitting/off_def_stan/fit-",
        1:4,
        ".csv"
    )
)

fit_full$print()
fit_full$diagnostic_summary()
fit_full$cmdstan_diagnose()
#fit0$cmdstan_summary()

## Build OOS Data ----
last_fitted_week <- get_last_fitted_week(
    fit_stan_data_full
)
last_fitted_week

last_fitted_season <- get_last_fitted_season(
    fit_stan_data_full
)
last_fitted_season

prep_sched_ind <- prepare_schedule_indices(
    games_data,
    teams
)
prep_sched_ind

oos_build <- build_oos_from_schedule(
    schedule_df = prep_sched_ind |>
        filter(
            season_idx == last_fitted_season,
            week_idx == last_fitted_week + 1
        )
)
str(oos_build)

prep_gq_data <- prepare_gq_data(
    fit_stan_data = fit_stan_data_full,
    schedule_df = prep_sched_ind,
    targets = last_fitted_week + 1
)
prep_gq_data

## Run GQ Fit ----
{
    timer <- .print_time(start = TRUE, msg = "Generating Forecasts")
    gq_full <- run_gq(
        gq_mod,
        fit = fit_full,
        gq_data = prep_gq_data,
        seed = 52,
        sig_figs = 10,
        parallel_chains = min(4L, parallel::detectCores())
    )
    .print_time(start = FALSE, timer = timer)
}
gq_full$print(max_rows = 1000)
gq_sum <- gq_full$summary()
gq_draws <- gq_full |> tidy_draws()

gq_latent_draws <- gq_full |>
    spread_draws(
        filtered_team_strength_last[team],
        filtered_team_hfa_last[team],
        filtered_league_hfa_last,
        predicted_team_strength_next_mean[team],
        predicted_team_strength_next_draw[team],
        predicted_league_hfa_next_mean
    ) |>
    mutate(team = teams[team])
gq_latent_sum <- gq_latent_draws |>
    summarize_draws()
gq_latent_sum


### Create smoothed data for full fit ----
full_smoothed_data <- create_stan_data(
    specific_seasons = 2006:2024,
    min_week = 1,
    max_week = 22,
    verbose = TRUE
)


smoothed_league_hfa_estimates <- fit_full |>
    spread_rvars(league_hfa[season_idx])

smoothed_team_hfa_estimates <- fit_full |>
    spread_rvars(team_hfa[season_idx, team])

smoothed_team_strength_estimates <- fit_full |>
    spread_rvars(team_strength[week_idx, team])

smoothed_estimates <- game_data |>
    select(
        season_idx,
        week_idx,
        home_idx,
        away_idx,
        season,
        week,
        home_team,
        away_team,
        hfa,
        home_score,
        away_score,
        result,
        spread_line,
        total,
        total_line
    ) |>
    left_join(
        smoothed_league_hfa_estimates
    ) |>
    left_join(
        smoothed_team_hfa_estimates,
        by = c("season_idx", "home_idx" = "team")
    ) |>
    left_join(
        smoothed_team_strength_estimates |>
            rename("home_strength" = "team_strength"),
        by = c("week_idx", "home_idx" = "team")
    ) |>
    left_join(
        smoothed_team_strength_estimates |>
            rename("away_strength" = "team_strength"),
        by = c("week_idx", "away_idx" = "team")
    ) |>
    mutate(
        mu_smoothed = home_strength - away_strength + team_hfa * hfa
    )

smooth_team_plot <- smoothed_team_strength_estimates |>
    mutate(team = teams[team]) |>
    left_join(teams_data, by = c("team" = "team_abbr")) |>
    filter(team_division == c("AFC North")) |>
    ggplot(aes(x = week_idx, color = team, fill = team)) +
    stat_lineribbon(
        aes(ydist = team_strength),
        .width = c(0.5),
        alpha = 0.5
    ) +
    scale_color_nfl() +
    scale_fill_nfl(type = "secondary")
smooth_team_plot

ggplotly(smooth_team_plot)


## Run Sequential Step ----
sequential_step_full <- sequential_step(
    fit_mod,
    gq_mod,
    fit_full,
    fit_stan_data_full,
    schedule_df = game_data,
    horizon = 1L,
    refit_after = TRUE,
    refit_weeks = 1L,
    seed = 52,
    chains = 4,
    parallel_chains = min(4, parallel::detectCores() - 1L),
    iter_warmup = 500,
    iter_sampling = 1000,
    adapt_delta = 0.9,
    max_treedepth = 10
)
