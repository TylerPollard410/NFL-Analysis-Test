#!/usr/bin/env Rscript

suppressPackageStartupMessages({
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
})

set.seed(52)
options(scipen = 10)

source("Model Fitting/off_def_stan/stan_helpers.R")

# 0) Globals
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

# 1) Prepare schedule indices once
teams <- load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:get_current_season()
schedule_idx <- prepare_schedule_indices(seasons = all_seasons, teams = teams)

# 2) Compile models
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(fit_path)
gq_mod <- cmdstan_model(gq_path)

# 3) Initial fit window: 2002..2005 weeks 1..21
fit_stan_data <- create_stan_data(
  specific_seasons = all_seasons,
  min_week = 1,
  max_week = get_current_week(),
  verbose = TRUE
)
str(fit_stan_data)

fit_stan_data <- create_stan_data(
  before_season = get_current_season()
)
fit_stan_data <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = get_current_week() - 1
)

fit0 <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit_init,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)
fit0 <- qs::qread(
  file.path(file_root, "snapshots", "fit_current.qs")
)

fit0$time()
fit0$print()
fit0$draws()
fit0$init()
fit0_meta <- fit0$metadata()
fit0_draws <- fit0$draws(format = "df")
fit0_sum <- fit0$summary()
fit_mod_params <- fit_mod$variables()$parameters |>
  keep(~ .x$dimensions < 2) |>
  names()
fit0_param_draws <- fit0$draws(
  variables = fit_mod_params,
  format = "df"
)
fit0_param_sum <- fit0$summary(
  variables = fit_mod_params
)
fit0_param_sum |> print(n = Inf)
mcmc_combo(fit0_param_draws)

fit0_gq_draws <- fit0$draws(
  variables = names(fit_mod$variables()$generated_quantities),
  format = "df"
)
fit0_gq_sum <- fit0_gq_draws |>
  spread_draws(
    filtered_team_strength_last[team],
    filtered_team_hfa_last[team],
    filtered_league_hfa_last
    #predicted_team_strength_next_mean[team], predicted_team_strength_next_draw[team]
  ) |>
  summarise_draws() |>
  mutate(team = teams[team]) |>
  mutate(
    season_idx = unique(fit0_gq_draws$last_s),
    week_idx = unique(fit0_gq_draws$last_w),
    .before = 1
  )

# 4) One-week-ahead forecast (auto targets by week_idx)
gq0 <- gq_next_weeks(
  gq_mod,
  fit0,
  fit_stan_data,
  schedule_idx,
  horizon = 1,
  seed = fit_seed,
  parallel_chains = fit_parallel
)


gq0$print()
gq0_meta <- gq0$metadata()
gq0_data <- jsonlite::read_json(gq0$data_file())

gq0_draws1 <- gq0$draws(format = "df")
rvars <- posterior::as_draws_rvars(gq0$draws())
gq0_sum1 <- gq0$summary()
gq0_sum <- gq0 |>
  summarise_draws()
gq0_gq_draws <- gq0$draws(
  variables = names(gq_mod$variables()$generated_quantities),
  format = "df"
)
gq0_gq_sum <- gq0_gq_draws |>
  spread_draws(
    filtered_team_strength_last[team],
    filtered_team_hfa_last[team],
    filtered_league_hfa_last,
    predicted_team_strength_next_mean[team],
    predicted_team_strength_next_draw[team]
  ) |>
  summarise_draws() |>
  mutate(team = teams[team]) |>
  mutate(
    season_idx = unique(gq0_gq_draws$last_s),
    week_idx = unique(gq0_gq_draws$last_w),
    .before = 1
  )

rvars |>
  pluck("mu_pred") |>
  summarise_draws()

team_pred_rvar <- gq0 |>
  spread_rvars(
    predicted_league_hfa[week_idx],
    predicted_team_strength[week_idx, team],
    predicted_team_hfa[week_idx, team]
  ) |>
  mutate(
    week_idx = unique(unlist(gq0_data$oos_week_idx)),
    team = teams[team]
  )

mu_pred_rvar <- gq0 |>
  spread_rvars(mu_pred[game_idx], y_pred[game_idx]) |>
  mutate(game_idx = oos_df$game_idx[game_idx])
sigma_obs_rvar <- fit0 |>
  spread_rvars(sigma_obs)


game_pred_rvar <- oos_df |>
  select(
    game_idx,
    season_idx,
    week_idx,
    home_idx,
    away_idx,
    home_team,
    away_team,
    hfa
  ) |>
  left_join(
    team_pred_rvar,
    by = c("week_idx", "home_team" = "team")
  ) |>
  rename(
    predicted_home_team_strength = predicted_team_strength
  ) |>
  left_join(
    team_pred_rvar |> select(week_idx, team, predicted_team_strength),
    by = c("week_idx", "away_team" = "team")
  ) |>
  rename(
    predicted_away_team_strength = predicted_team_strength
  ) |>
  relocate(
    predicted_away_team_strength,
    .after = predicted_home_team_strength
  ) |>
  cross_join(
    sigma_obs_rvar
  ) |>
  left_join(
    mu_pred_rvar,
    by = "game_idx"
  ) |>
  mutate(
    mu_pred_check = predicted_home_team_strength -
      predicted_away_team_strength +
      predicted_team_hfa * hfa,
    .after = mu_pred
  ) |>
  mutate(
    y_pred_check = rvar_rng(rnorm, n = 1, mu_pred, sigma_obs, ndraws = 20000),
    y_pred_check2 = mu_pred +
      rvar_rng(rnorm, n = 1, 0, sigma_obs, ndraws = 20000),
    .after = y_pred,
    .by = game_idx
  )


comb_rvars <- mu_pred_rvar |>
  mutate(
    sigma_obs_rvar,
    .after = mu_pred
  ) |>
  rowwise() |>
  mutate(
    y_pred2 = rvar_rng(rnorm, n = 1, mu_pred, sigma_obs, ndraws = 20000)
  )
comb_rvars |>
  summarise(
    across(c(mu_pred, y_pred, y_pred2), ~ E(.x))
  )

build_init_lists_from_fit0 <- build_init_lists_from_fit(
  fit0,
  fit_mod,
  n_chains = fit_chains,
  seed = fit_seed
)
rowSums(build_init_lists_from_fit0[[1]]$team_hfa_deviation)
fit0$con

reuse_metric_from_fit0 <- reuse_metric_from_fit(
  fit0
)
reuse_metric_from_fit0$inv_metric$`1`
mcmc_hist()


# Optional: save snapshot for warm-start + summaries
targets <- next_week_targets(fit_stan_data, horizon = 1)
oos_df <- schedule_idx |> filter(week_idx %in% targets)
dir.create(
  file.path(file_root, "snapshots"),
  recursive = TRUE,
  showWarnings = FALSE
)
save_sequential_snapshot(
  path = file.path(file_root, "snapshots"),
  fit = fit0,
  fit_mod = fit_mod,
  gq = gq0,
  fit_stan_data = fit_stan_data,
  oos_df = oos_df,
  teams = teams,
  n_init = chains,
  seed = fit_seed
)

# 5) Sequential step: predict next week and refit (warm-start+metric reuse inside)
step1 <- sequential_step(
  fit_mod,
  gq_mod,
  fit0,
  fit_stan_data,
  schedule_idx,
  horizon = 1,
  refit_after = TRUE,
  refit_weeks = 1,
  seed = fit_seed,
  parallel_chains = fit_parallel,
  sig_figs = fit_sig_figs,
  iter_warmup = 200,
  iter_sampling = 1000,
  adapt_delta = 0.9,
  max_treedepth = 10,
  warm_start = FALSE,
  reuse_metric = FALSE
)

message("Sequential demo complete. Artifacts:")
message("- Snapshot dir: snapshots")
message("- GQ (first run): use gq0")
message(
  "- After sequential step: fit -> step1$fit, gq -> step1$gq, targets -> step1$targets"
)


step1_noWarm_noReuse <- step1


fit_stan_data_new <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = 1
)

is_dim_gt1 <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    return(FALSE)
  }
  any(d > 1)
}

# Remove elements with any dimension > 1 from each sublist
build_init_lists_from_fit0_small <- map(
  build_init_lists_from_fit0,
  ~ discard(.x, is_dim_gt1)
)

fit1 <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = 4,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit1$time()
fit1$diagnostic_summary()
fit1$cmdstan_diagnose()
fit1$print()


str(build_init_lists_from_fit0)
fit1_init <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = build_init_lists_from_fit0,
  sig_figs = fit_sig_figs,
  chains = 4,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit1_init$time()
fit1_init$diagnostic_summary()
fit1_init$cmdstan_diagnose()
fit1_init$print()

mcmc_trace(fit0$draws(variables = c("sigma_obs")))
mcmc_combo(fit0$draws(variables = c("sigma_obs")))

fit0$save_object(
  file = file.path(file_root, "snapshots", "fit0_noComp.rds")
)
fit0$save_object(
  file = file.path(file_root, "snapshots", "fit0_Comp.rds"),
  compress = TRUE
)
qs::qsave(
  x = fit0,
  file = file.path(file_root, "snapshots", "fit_current.qs")
)

fit0_read <- readRDS(
  file.path(file_root, "snapshots", "fit0.rds")
)
fit0 <- qs::qread(
  file.path(file_root, "snapshots", "fit0.qs")
)

fit0_read$print()
fit0_qs$print()

try(fit0_qs$draws())


gq0_code <- gq0$code()
gq0_code <- write_stan_file(
  code = gq0_code,
  dir = file_root,
  basename = "mod_gq1"
)

## Test code -----
file_root <- "Model Fitting/off_def_stan"


fit_stan_data <- create_stan_data(
  before_season = 2006
)
fit_stan_data <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = 10
)

### Fit 01 -----
fit_path01 <- file.path(file_root, "mod_fit.stan")
gq_path01 <- file.path(file_root, "mod_gq.stan")

fit_mod01 <- cmdstan_model(
  fit_path01,
  compile_model_methods = FALSE,
  force_recompile = FALSE,
  pedantic = TRUE
)
fit_mod01_vars <- fit_mod01$variables()
fit_mod01$code()

gq_mod <- cmdstan_model(gq_path)

fit01 <- fit_mod01$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = 4,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit01$cmdstan_diagnose()
fit01$time()
fit01$diagnostic_summary()
fit01$print()

fit01_draws <- fit01$draws(format = "df")
fit01_sum <- fit01$summary()

fit01_mod_params <- fit_mod01$variables()$parameters |>
  keep(~ .x$dimensions < 2) |>
  names()
fit01_param_draws <- fit01$draws(
  variables = fit01_mod_params,
  format = "df"
)
fit01_param_sum <- fit01$summary(
  variables = fit01_mod_params
)
fit01_param_sum |> print(n = Inf)

fit01$init_model_methods()
fit01$variable_skeleton()

fit01_loo <- fit01$loo()
fit01_loo

### Fit 02 -----
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
  fit_path,
  #compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)
fit_mod_vars <- fit_mod$variables()
fit_mod$code()

#gq_mod <- cmdstan_model(gq_path)

fit01_inits <- build_init_lists_from_fit(
  fit01,
  fit_mod,
  n_chains = fit_chains,
  seed = fit_seed
)

fit02 <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit01_inits,
  sig_figs = fit_sig_figs,
  chains = 4,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit02$cmdstan_diagnose()

fit02$time()
fit02$diagnostic_summary()
fit02$print()

fit02_draws <- fit02$draws(format = "df")
fit02_sum <- fit02$summary()

fit_mod_params <- fit_mod$variables()$parameters |>
  keep(~ .x$dimensions < 2) |>
  names()

fit02_param_draws <- fit02$draws(
  variables = fit_mod_params,
  format = "df"
)
fit02_param_sum <- fit02$summary(
  variables = fit_mod_params
)
fit02_param_sum |> print(n = Inf)

fit02$init_model_methods()
fit02$variable_skeleton()

fit02_loo <- fit02$loo()
fit02_loo

loo::loo_compare(fit01_loo, fit02_loo)


### Fit 02full -----
suppressPackageStartupMessages({
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
})

set.seed(52)
options(scipen = 10)

source("Model Fitting/off_def_stan/stan_helpers.R")

# 0) Globals
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

# 1) Prepare schedule indices once
teams <- load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:get_current_season()
schedule_idx <- prepare_schedule_indices(seasons = all_seasons, teams = teams)

# 2) Compile models
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_stan_data_full <- create_stan_data(
  before_season = get_current_season()
)
fit_stan_data_full <- roll_forward_fit_stan_data(
  fit_stan_data_full,
  schedule_idx,
  weeks_ahead = get_current_week() - 1
)

#### fit ----
fit_mod_full <- cmdstan_model(
  fit_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)
fit_mod_full_vars <- fit_mod_full$variables()
fit_mod_full$code()
#gq_mod <- cmdstan_model(gq_path)

fit01_inits <- build_init_lists_from_fit(
  fit01,
  fit_mod,
  n_chains = fit_chains,
  seed = fit_seed
)

fit02full <- fit_mod_full$sample(
  data = fit_stan_data_full,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = 4,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit02full$save_object(
  file = file.path(file_root, "snapshots", "fit_current_full.rds")
)
fit02full <- readRDS(
  file.path(file_root, "snapshots", "fit_current_full.rds")
)

fit02full$cmdstan_diagnose()

fit02full$time()
fit02full$diagnostic_summary()
fit02full$print()

fit02full_meta <- fit02full$metadata()

fit02full_diagnostics <- fit02full$sampler_diagnostics(format = "df")
fit02full_draws <- fit02full$draws(format = "df") |>
  left_join(
    fit02full_diagnostics,
    by = join_by(.chain, .iteration, .draw)
  )
fit02full_sum <- fit02full$summary()

fit_mod_params <- fit_mod$variables()$parameters |>
  #keep(~ .x$dimensions < 2) |>
  names()

fit02full_param_draws <- fit02full$draws(
  variables = fit_mod_params,
  format = "df"
)
fit02full_param_sum <- fit02full$summary(
  variables = fit_mod_params
)
fit02full_param_sum |> print(n = Inf)

fit02full$init_model_methods()
fit02full_skel <- fit02full$variable_skeleton()

fit02full_loo <- fit02full$loo()
fit02full_loo

loo::loo_compare(fit01_loo, fit02full_loo)

fit02full_meta$stan_variables |>
  str_subset(pattern = "filtered|pred|mu|sigma_obs")

fit02full_gq_latent_sum <- fit02full_draws |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team],
    predicted_league_hfa,
    predicted_team_hfa[team],
    predicted_team_strength_mean[team],
    predicted_team_strength_draw[team]
  ) |>
  summarise_draws() |>
  mutate(team = teams[team]) |>
  mutate(
    Fit = "fit",
    season_idx = fit_stan_data_full$N_seasons,
    week_idx = fit_stan_data_full$N_weeks,
    .before = 1
  )

fit02full_gq_game_sum <- fit02full_draws |>
  spread_draws(
    mu[game_idx],
    sigma_obs
  ) |>
  filter(
    game_idx %in%
      which(fit_stan_data_full$week_idx == fit_stan_data_full$N_weeks)
  ) |>
  summarise_draws() |>
  mutate(
    season_idx = fit_stan_data_full$N_seasons,
    week_idx = fit_stan_data_full$N_weeks,
    hfa = fit_stan_data_full$hfa[game_idx],
    .after = game_idx
  )

#### gq ----
gq_mod_full <- cmdstan_model(
  gq_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)
gq_mod_full_vars <- gq_mod_full$variables()
gq_mod_full$code()

gq02full <- gq_next_weeks(
  gq_mod_full,
  fit02full,
  fit_stan_data_full,
  schedule_idx,
  horizon = 1,
  seed = fit_seed,
  parallel_chains = fit_parallel
)
gq02full$print()
gq02full_meta <- gq02full$metadata()

gq02full_draws <- gq02full$draws(format = "df")
gq02full_sum <- gq02full$summary()

gq02full_meta$stan_variables |>
  str_subset(pattern = "filtered|pred|mu|sigma_obs")


gq02full_gq_latent_sum <- gq02full_draws |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team],
    predicted_league_hfa[week_idx],
    predicted_team_hfa[week_idx, team],
    predicted_team_strength[week_idx, team]
  ) |>
  summarise_draws() |>
  mutate(
    Fit = "gq",
    week_idx = (next_week_targets(
      fit_stan_data_full,
      horizon = length(unique(week_idx))
    ))[week_idx] -
      1L,
    season_idx = unique(schedule_idx$season_idx[
      schedule_idx$week_idx == week_idx
    ]),
    team = teams[team],
    .before = 1
  )

gq02full_gq_game_sum <- gq02full_draws |>
  spread_draws(
    mu[game_idx],
    sigma_obs
  ) |>
  filter(
    game_idx %in%
      which(fit_stan_data_full$week_idx == fit_stan_data_full$N_weeks)
  ) |>
  summarise_draws() |>
  mutate(
    season_idx = fit_stan_data_full$N_seasons,
    week_idx = fit_stan_data_full$N_weeks,
    hfa = fit_stan_data_full$hfa[game_idx],
    .after = game_idx
  )

#### Compare fit vs gq ----
comb_gq_latent <- bind_rows(
  fit02full_gq_latent_sum,
  gq02full_gq_latent_sum
) |>
  arrange(week_idx, team, variable) |>
  relocate(Fit, .before = variable)
