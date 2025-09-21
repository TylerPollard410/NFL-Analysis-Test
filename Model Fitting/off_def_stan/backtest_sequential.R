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

# 3) Configure backtest window (2006 W1 forward for 25 weeks)
backtest_start_season <- 2006L
backtest_start_week <- 1L
backtest_weeks <- 5L

backtest_start_week_idx <- lookup_week_idx(
  schedule_idx,
  backtest_start_season,
  backtest_start_week
)
backtest_end_week_idx <- backtest_start_week_idx + backtest_weeks - 1L

max_week_idx <- max(schedule_idx$week_idx, na.rm = TRUE)
max_completed_week_idx <- max(
  schedule_idx$week_idx[!is.na(schedule_idx$result)],
  na.rm = TRUE
)
if (!is.finite(max_completed_week_idx)) {
  max_completed_week_idx <- backtest_start_week_idx - 1L
}
if (backtest_end_week_idx > max_week_idx) {
  stop(
    "Requested backtest horizon extends past available schedule data. ",
    "Max week_idx is ",
    max_week_idx
  )
}

if (backtest_end_week_idx > max_completed_week_idx) {
  cat(
    "\nNote: weeks beyond week_idx ",
    max_completed_week_idx,
    " lack recorded results; the model will keep the previous fit and generate forecasts only for those weeks.\n",
    sep = ""
  )
}

cat(
  "\nSequential backtest window (week_idx): ",
  backtest_start_week_idx,
  " to ",
  backtest_end_week_idx,
  " (",
  backtest_weeks,
  " weeks)\n",
  sep = ""
)

fit_params <- list(
  chains = fit_chains,
  parallel_chains = fit_parallel,
  init = fit_init,
  sig_figs = fit_sig_figs,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth,
  seed = fit_seed
)

output_dir <- file.path(
  file_root,
  sprintf(
    "backtest_%d_wk%02d_%dw",
    backtest_start_season,
    backtest_start_week,
    backtest_weeks
  )
)

backtest_run <- run_sequential_backtest(
  fit_mod = fit_mod,
  gq_mod = gq_mod,
  schedule_df = schedule_idx,
  teams = teams,
  start_week = backtest_start_week_idx,
  end_week = backtest_end_week_idx,
  output_dir = output_dir,
  fit_params = fit_params,
  verbose = TRUE,
  use_simple_extraction = TRUE,
  skip_refit_if_incomplete = TRUE,
  retain_results = FALSE
)

backtest_summary <- summarise_backtest_results(backtest_run$results)

if (nrow(backtest_summary$oos)) {
  readr::write_csv(
    backtest_summary$oos,
    file.path(output_dir, "oos_predictions.csv")
  )
}

if (nrow(backtest_summary$metrics)) {
  readr::write_csv(
    backtest_summary$metrics,
    file.path(output_dir, "metrics_summary.csv")
  )
  cat("\nBacktest metrics:\n")
  print(backtest_summary$metrics, n = nrow(backtest_summary$metrics))
} else {
  cat("\nNo out-of-sample predictions captured; metrics table is empty.\n")
}

cat(
  "\nBacktest artifacts saved to: ",
  normalizePath(output_dir),
  "\n",
  sep = ""
)
