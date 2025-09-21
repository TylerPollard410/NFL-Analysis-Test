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
options(scipen = 999)

# 0. Globals ----
source("Model Fitting/off_def_stan/stan_helpers.R")

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

# 1. Prepare schedule indices once ----
teams <- load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:get_current_season()
schedule_idx <- prepare_schedule_indices(seasons = all_seasons, teams = teams)


fit_stan_data <- create_stan_data(
  before_season = 2006
)
fit_stan_data <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = 10
)

targets <- next_week_targets(fit_stan_data, horizon = 1)
oos_df <- schedule_idx |> filter(week_idx %in% targets)

gq_stan_data <- prepare_gq_data(
  fit_stan_data,
  schedule_idx,
  targets
)

# 2. Compile models ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit_new.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(fit_path, force_recompile = TRUE)
fit_mod$check_syntax(pedantic = TRUE)

gq_mod <- cmdstan_model(gq_path)

fit_mod_vars <- fit_mod$variables()
gq_mod_vars <- gq_mod$variables()

# 3. Fit Models ----

## 3A. MCMC ----

fit0 <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

## 3B. MLE ----
fit_mle <- fit_mod$optimize(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit_init,
  sig_figs = fit_sig_figs,
  iter = fit_samps,
  jacobian = FALSE
  # save_latent_dynamics = FALSE,
  # output_dir = getOption("cmdstanr_output_dir"),
  # output_basename = NULL,
  # threads = NULL,
  # opencl_ids = NULL,
  # algorithm = NULL,
  # jacobian = FALSE,
  # init_alpha = NULL,
  # tol_obj = NULL,
  # tol_rel_obj = NULL,
  # tol_grad = NULL,
  # tol_rel_grad = NULL,
  # tol_param = NULL,
  # history_size = NULL,
  # show_messages = TRUE,
  # show_exceptions = TRUE,
  # save_cmdstan_config = NULL
)

## 3C. MAP ----
fit_map <- fit_mod$optimize(
  data = fit_stan_data,
  seed = fit_seed,
  # init = fit_init,
  sig_figs = fit_sig_figs,
  iter = 20000,
  jacobian = TRUE,
  # save_latent_dynamics = FALSE,
  # output_dir = getOption("cmdstanr_output_dir"),
  # output_basename = NULL,
  # threads = NULL,
  # opencl_ids = NULL,
  algorithm = "lbfgs",
  # jacobian = FALSE,
  # init_alpha = NULL,
  # tol_obj = NULL,
  # tol_rel_obj = NULL,
  # tol_grad = NULL,
  # tol_rel_grad = NULL,
  # tol_param = NULL,
  history_size = 5
  # show_messages = TRUE,
  # show_exceptions = TRUE,
  # save_cmdstan_config = NULL
)

## 3D. Laplace (LA) ----
fit_la <- fit_mod$laplace(
  data = fit_stan_data,
  seed = fit_seed,
  # init = fit_init,
  # save_latent_dynamics = FALSE,
  # output_dir = getOption("cmdstanr_output_dir"),
  # output_basename = NULL,
  sig_figs = fit_sig_figs,
  # threads = NULL,
  # opencl_ids = NULL,
  # mode = fit_map,
  opt_args = list(iter = 20000),
  jacobian = TRUE,
  draws = 1000
  # show_messages = TRUE,
  # show_exceptions = TRUE,
  # save_cmdstan_config = NULL
)
fit_la_mode <- fit_la$mode()$summary()
fit_la_sum <- fit_la$summary()

## 3E. Variational (ADVI) ----
fit_vi <- fit_mod$variational(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit_init,
  # save_latent_dynamics = FALSE,
  # output_dir = getOption("cmdstanr_output_dir"),
  # output_basename = NULL,
  sig_figs = fit_sig_figs,
  # threads = NULL,
  # opencl_ids = NULL,
  # algorithm = NULL,
  iter = 20000,
  # grad_samples = NULL,
  # elbo_samples = NULL,
  # eta = NULL,
  # adapt_engaged = NULL,
  # adapt_iter = NULL,
  # tol_rel_obj = NULL,
  # eval_elbo = NULL,
  # output_samples = NULL,
  draws = fit_samps
  # show_messages = TRUE,
  # show_exceptions = TRUE,
  # save_cmdstan_config = NULL
)

## 3F. Pathfinder (PF) ----
fit_pf <- fit_mod$pathfinder(
  data = fit_stan_data,
  seed = fit_seed,
  #init = 0,
  refresh = 100,
  # save_latent_dynamics = FALSE,
  # output_dir = getOption("cmdstanr_output_dir"),
  # output_basename = NULL,
  sig_figs = fit_sig_figs,
  # opencl_ids = NULL,
  # num_threads = NULL,
  # init_alpha = NULL,
  # tol_obj = NULL,
  # tol_rel_obj = NULL,
  # tol_grad = NULL,
  # tol_rel_grad = NULL,
  # tol_param = NULL,
  history_size = 5,
  single_path_draws = 250,
  draws = fit_chains * fit_samps,
  num_paths = 8,
  max_lbfgs_iters = 200,
  num_elbo_draws = 50,
  save_single_paths = TRUE,
  psis_resample = TRUE,
  calculate_lp = TRUE
  # show_messages = TRUE,
  # show_exceptions = TRUE,
  # save_cmdstan_config = NULL
)
fit_pf_meta <- fit_pf$metadata()
str(fit_pf_meta[names(fit_pf_meta)[1:25]])

# 4. Fit Diagnostics ----
fit_mod_params <- fit_mod_vars$parameters |>
  keep(~ .x$dimensions < 2) |>
  names()

## 4A. MCMC ----
fit_mcmc$print()
fit_mcmc$diagnostic_summary()

fit_mcmc_meta <- fit_mcmc$metadata()
fit_mcmc_param_draws <- fit_mcmc$draws(
  variables = fit_mod_params,
  format = "df"
)
fit_mcmc_param_sum <- fit_mcmc$summary(variables = c("lp__", fit_mod_params))
fit_mcmc_param_sum |> print(n = Inf)

fit_mcmc_latent_sum <- fit_mcmc |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team]
  ) |>
  summarise_draws()
fit_mcmc_latent_sum |> print(n = Inf)

## 4B. MLE ----
fit_mle$print()
fit_mle$diagnostic_summary()

fit_mle_meta <- fit_mle$metadata()
fit_mle_param_sum <- fit_mle$summary(variables = c("lp__", fit_mod_params))
fit_mle_param_sum |> print(n = Inf)

fit_mle_latent_sum <- fit_mle |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team]
  ) |>
  summarise_draws()
fit_mle_latent_sum |> print(n = Inf)

## 4C. MAP ----
fit_map$print()
fit_map$diagnostic_summary()

fit_map_meta <- fit_map$metadata()
fit_map_param_sum <- fit_map$summary(variables = c("lp__", fit_mod_params))
fit_map_param_sum |> print(n = Inf)

fit_map_latent_sum <- fit_map |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team]
  ) |>
  summarise_draws()
fit_map_latent_sum |> print(n = Inf)

## 4D. Laplace (LA) ----

## 4E. Variational (ADVI) ----
fit_vi$print()
fit_vi$diagnostic_summary()

fit_vi_meta <- fit_vi$metadata()
fit_vi_param_sum <- fit_vi$summary(variables = c("lp__", fit_mod_params))
fit_vi_param_sum |> print(n = Inf)

fit_vi_latent_sum <- fit_vi |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team]
  ) |>
  summarise_draws()
fit_vi_latent_sum |> print(n = Inf)

## 4F. Pathfinder (PF) ----
fit_pf$print()
fit_pf$diagnostic_summary()
fit_pf$cmdstan_diagnose()

fit_pf_meta <- fit_pf$metadata()
fit_pf_param_draws <- fit_pf$draws(variables = fit_mod_params, format = "df")
fit_pf_param_sum <- fit_pf$summary(variables = c("lp__", fit_mod_params))
fit_pf_param_sum |> print(n = Inf)

fit_pf_latent_sum <- fit_pf |>
  spread_draws(
    filtered_league_hfa_last,
    filtered_team_hfa_last[team],
    filtered_team_strength_last[team]
  ) |>
  summarise_draws()
fit_pf_latent_sum |> print(n = Inf)

## Time ----
fit_mcmc$time()
fit_mle$time()
fit_map$time()
fit_vi$time()
fit_pf$time()

## print
fit_mcmc$print()
fit_mle$print()
fit_map$print()
fit_vi$print()
fit_pf$print()

## lp__
fit_mcmc$summary("lp__")
fit_mle$summary("lp__")
fit_map$summary("lp__")
fit_vi$summary("lp__")
fit_pf$summary("lp__")

## meta
str(fit_mcmc$metadata())
str(fit_mle$metadata())
str(fit_map$metadata())
str(fit_vi$metadata())
str(fit_pf$metadata())

## params
param_sum <- fit_mcmc_param_sum |>
  select(variable, mean) |>
  rename(mcmc = mean) |>
  left_join(
    fit_mle_param_sum |>
      select(variable, estimate) |>
      rename(mle = estimate),
    by = "variable"
  ) |>
  left_join(
    fit_map_param_sum |>
      select(variable, estimate) |>
      rename(map = estimate),
    by = "variable"
  ) |>
  left_join(
    fit_vi_param_sum |>
      select(variable, mean) |>
      rename(vi = mean),
    by = "variable"
  ) |>
  left_join(
    fit_pf_param_sum |>
      select(variable, mean) |>
      rename(pf = mean),
    by = "variable"
  )
param_sum |> print(n = Inf)

# 4. Generate Quantities ----
gq_mcmc <- gq_mod$generate_quantities(
  fitted_params = fit_mcmc,
  data = gq_stan_data,
  seed = fit_seed,
  sig_figs = fit_sig_figs,
  parallel_chains = fit_parallel
)

fit_mcmc_var_skel <- fit_mcmc$variable_skeleton()

fit_mcmc2 <- fit_mod$sample(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit_mcmc,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)
