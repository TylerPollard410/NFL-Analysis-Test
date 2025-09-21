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

source("Model Fitting/off_def_stan/stan_helpers.R")

# 0) Globals
set.seed(52)
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

fit_mod <- cmdstan_model(fit_path)
gq_mod <- cmdstan_model(gq_path)

# 3) Initial fit window: 2002..2005 weeks 1..21
fit_stan_data <- create_stan_data(
  specific_seasons = 2002:2005,
  min_week = 1,
  max_week = 21,
  verbose = TRUE
)
str(fit_stan_data)

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

fit0$print()
fit0_meta <- fit0$metadata()
fit0_draws <- fit0$draws(format = "df")
fit0_sum <- fit0$summary()
fit0_gq_draws <- fit0$draws(variables = names(fit_mod$variables()$generated_quantities),
                            format = "df")
fit0_gq_sum <- fit0_gq_draws |>
  spread_draws(
    filtered_team_strength_last[team], filtered_team_hfa_last[team], filtered_league_hfa_last,
    predicted_team_strength_next_mean[team], predicted_team_strength_next_draw[team]
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
gq0_draws <- gq0$draws(format = "df")
gq0_sum <- gq0$summary()
gq0_gq_draws <- gq0$draws(variables = names(gq_mod$variables()$generated_quantities),
                            format = "df")
gq0_gq_sum <- gq0_gq_draws |>
  spread_draws(
    filtered_team_strength_last[team], filtered_team_hfa_last[team], filtered_league_hfa_last,
    predicted_team_strength_next_mean[team], predicted_team_strength_next_draw[team]
  ) |>
  summarise_draws() |>
  mutate(team = teams[team]) |>
  mutate(
    season_idx = unique(gq0_gq_draws$last_s),
    week_idx = unique(gq0_gq_draws$last_w),
    .before = 1
  )

build_init_lists_from_fit0 <- build_init_lists_from_fit(
  fit0,
  fit_mod,
  n_chains = fit_chains,
  seed = fit_seed
)
reuse_metric_from_fit0 <- reuse_metric_from_fit(
  fit0
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
  if (is.null(d)) return(FALSE)
  any(d > 1)
}

# Remove elements with any dimension > 1 from each sublist
build_init_lists_from_fit0_small <- map(
  build_init_lists_from_fit0,
  ~ discard(.x, is_dim_gt1)
)

fit1 <- fit_mod$sample(
  data = fit_stan_data_new,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = 200,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit0$save_object(
  file = file.path(file_root, "snapshots", "fit0_noComp.rds")
)
fit0$save_object(
  file = file.path(file_root, "snapshots", "fit0_Comp.rds"),
  compress = TRUE
)
qs::qsave(
  x = fit0, 
  file = file.path(file_root, "snapshots", "fit0.qs")
  )

fit0_read <- readRDS(
  file.path(file_root, "snapshots", "fit0.rds")
)
fit0_qs <- qs::qread(
  file.path(file_root, "snapshots", "fit0.qs")
)

fit0_read$print()
fit0_qs$print()

try(fit0_qs$draws())












