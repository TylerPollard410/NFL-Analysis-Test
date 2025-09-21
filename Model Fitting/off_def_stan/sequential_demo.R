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

source("Model Fitting/off_def_stan/stan_helpers.R")

# 0) Globals
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

# 1) Prepare schedule indices once
teams <- load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:get_current_season()
schedule_idx <- prepare_schedule_indices(seasons = all_seasons, teams = teams)

# 2) Compile models
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
  fit_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)
gq_mod <- cmdstan_model(
  gq_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)

# 3) Initial fit window: 2002..2005 weeks 1..21
fit_stan_data <- create_stan_data(
  before_season = 2006,
  verbose = TRUE
)
str(fit_stan_data)

fit0 <- fit_mod$sample(
  data = fit_stan_data,
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
  warm_start = TRUE,
  reuse_metric = TRUE
)

message("Sequential demo complete. Artifacts:")
message("- Snapshot dir: snapshots")
message("- GQ (first run): use gq0")
message(
  "- After sequential step: fit -> step1$fit, gq -> step1$gq, targets -> step1$targets"
)
