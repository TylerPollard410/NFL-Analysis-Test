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
library(rstan)
library(cmdstanr)
library(brms)
library(posterior)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
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

library(nflendzonePipeline)
library(nflendzone)

set.seed(52)
options(scipen = 9)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 1. Source Files ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
save_root <- "Model Fitting/ssm_check"
source(file.path(save_root, "data.R"))
source(file.path(save_root, "helpers.R"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. Build Stan Code ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 2.1 baseline ----
### Fit ----
#### data ----
block_data <- "
data {
  int<lower=1> N_games;
  int<lower=1> N_obs;

  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks>  week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa;
  vector[N_obs] result;

  // Optional: games to predict without contributing to likelihood
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}
"

#### transformed data ----
block_transformed_data <- "
transformed data {
  real sum_to_zero_scale = sqrt(N_teams / (N_teams - 1.0));
}
"

#### parameters ----
block_parameters <- "
parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_z;
  real              league_hfa_init;
  real<lower=0, upper=1> beta_hfa;
  real<lower=0>          sigma_hfa;
  
  // Team HFA deviations about league mean (sum-to-zero per season)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_z;
  real<lower=0>                                sigma_team_hfa;
  
  // Team strength state-space innovations (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] z_start; // season-start shocks
  array[N_weeks]   sum_to_zero_vector[N_teams] z_w;     // weekly shocks
  
  real<lower=0, upper=1> beta_w;        // within-season AR(1)
  real<lower=0>          sigma_w;       // within-season shock sd
  
  real<lower=0, upper=1> beta_s;        // between-season carryover
  real<lower=0>          sigma_s;       // between-season start shock sd
  
  real<lower=0> sigma_y;                // observation noise
}
"

#### transformed parameters ----
block_transformed_parameters <- "
transformed parameters {
  // League HFA series
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init + 2 * league_hfa_z[1];  // diffuse start (sd ≈ 2)
  for (s in 2:N_seasons) {
    league_hfa[s] = beta_hfa * league_hfa[s - 1] + sigma_hfa * league_hfa_z[s];
  }
  
  // Team HFA = league mean + scaled, sum-to-zero deviations
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1:N_seasons) {
    team_hfa[s] = league_hfa[s] + sigma_team_hfa * team_hfa_z[s];
  }
  
  // Team strength states (zero-sum preserved by construction)
  array[N_weeks] vector[N_teams] team_strength_raw
  = rep_array(rep_vector(0.0, N_teams), N_weeks);
  array[N_weeks] vector[N_teams] team_strength;
  
  // Build each season
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];
    
    // Season start: zero-sum carries over, e_start is zero-sum
    if (s == 1) {
      team_strength_raw[fw] = 5 * z_start[s];
    } else {
      int lw_prev = last_week_of_season[s - 1];
      vector[N_teams] prev = team_strength_raw[lw_prev];  // zero-sum already
      team_strength_raw[fw] = beta_s * prev + sigma_s * z_start[s];
    }
    
    // Within-season AR(1): e_w[w] is zero-sum, so property propagates
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        team_strength_raw[w] = beta_w * team_strength_raw[w - 1]
        + sigma_w * z_w[w];
      }
    }
  }
  
  // No weekly mean-subtraction needed; already zero-sum
  for (w in 1:N_weeks)
    team_strength[w] = team_strength_raw[w];
}
"

#### model ----
block_model <- "
model {
  // Priors (non-centered std normals for innovations)
  league_hfa_z ~ std_normal();
  for (s in 1:N_seasons) team_hfa_z[s] ~ normal(0, sum_to_zero_scale); //std_normal();
  for (s in 1:N_seasons) z_start[s]    ~ normal(0, sum_to_zero_scale); //std_normal();
  for (w in 1:N_weeks)   z_w[w]        ~ normal(0, sum_to_zero_scale); //std_normal();

  beta_hfa        ~ beta(8, 2);
  sigma_hfa       ~ student_t(3, 0, 2);
  league_hfa_init ~ normal(2, 2);

  sigma_team_hfa  ~ normal(0, 2);

  beta_w ~ beta(6, 2);
  sigma_w ~ student_t(3, 0, 5);

  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 5);

  sigma_y ~ student_t(3, 0, 10);

  // Likelihood over the first N_obs games
  //{
  vector[N_obs] mu_obs;
  for (g in 1:N_obs) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];

    mu_obs[g] = (team_strength[w][i] - team_strength[w][j])
                + team_hfa[s][i] * hfa[g];
  }
  // target += normal_lpdf(result[1:N_obs] | mu_obs, sigma_y);
  result[1:N_obs] ~ normal(mu_obs, sigma_y);
  //}
}
"
#### generated quantities ----
block_generated_quantities <- "
generated quantities {
  // Snapshot at end of training window (t-1)
  int last_w = week_id[N_obs];
  int last_s = season_id[N_obs];
  vector[N_teams] team_strength_last = team_strength[last_w];
  real            league_hfa_last    = league_hfa[last_s];
  vector[N_teams] team_hfa_last      = team_hfa[last_s];
  
  // Fitted means & posterior predictive for observed games
  vector[N_obs] mu_obs;
  vector[N_obs] y_rep;

  for (g in 1:N_obs) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_obs[g] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[g];
    y_rep[g]  = normal_rng(mu_obs[g], sigma_y);
  }
}
"

### GQ ----
#### data ----
block_data_gq <- "
data {
  int<lower=1> N_games;
  int<lower=1> N_obs;

  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks>  week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa;

  // Optional: games to predict without contributing to likelihood
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}
"

#### parameteers ----
block_parameters_gq <- "
parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_z;
  real              league_hfa_init;
  real<lower=0, upper=1> beta_hfa;
  real<lower=0>          sigma_hfa;

  // Team HFA deviations about league mean (sum-to-zero per season)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_z;
  real<lower=0>                                sigma_team_hfa;

  // Team strength state-space innovations (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] z_start; // season-start shocks
  array[N_weeks]   sum_to_zero_vector[N_teams] z_w;     // weekly shocks

  real<lower=0, upper=1> beta_w;        // within-season AR(1)
  real<lower=0>          sigma_w;       // within-season shock sd

  real<lower=0, upper=1> beta_s;        // between-season carryover
  real<lower=0>          sigma_s;       // between-season start shock sd

  real<lower=0> sigma_y;                // observation noise
}
"

#### transformed parameters ----
block_transformed_parameters_gq <- "
transformed parameters {
  // League HFA series
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init + 2 * league_hfa_z[1];  // diffuse start (sd ≈ 2)
  for (s in 2:N_seasons) {
    league_hfa[s] = beta_hfa * league_hfa[s - 1] + sigma_hfa * league_hfa_z[s];
  }

  // Team HFA = league mean + scaled, sum-to-zero deviations
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1:N_seasons) {
    team_hfa[s] = league_hfa[s] + sigma_team_hfa * team_hfa_z[s];
  }

  // Team strength states (zero-sum preserved by construction)
  array[N_weeks] vector[N_teams] team_strength_raw
    = rep_array(rep_vector(0.0, N_teams), N_weeks);
  array[N_weeks] vector[N_teams] team_strength;

  // Build each season
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Season start: zero-sum carries over, e_start is zero-sum
    if (s == 1) {
      team_strength_raw[fw] = 5 * z_start[s];
    } else {
      int lw_prev = last_week_of_season[s - 1];
      vector[N_teams] prev = team_strength_raw[lw_prev];  // zero-sum already
      team_strength_raw[fw] = beta_s * prev + sigma_s * z_start[s];
    }

    // Within-season AR(1): e_w[w] is zero-sum, so property propagates
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        team_strength_raw[w] = beta_w * team_strength_raw[w - 1]
                               + sigma_w * z_w[w];
      }
    }
  }

  // No weekly mean-subtraction needed; already zero-sum
  for (w in 1:N_weeks)
    team_strength[w] = team_strength_raw[w];
}
"

#### generated quantities ----
block_generated_quantities_gq <- "
generated quantities {
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  
  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_pred[k] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[k];
    y_pred[k] = normal_rng(mu_pred[k], sigma_y);
  }
}
"
## 2.2 ssm1 ----
### Fit ----
#### transformed data ----
block_transformed_data_ssm1 <- "
transformed data {
  real sum_to_zero_scale = sqrt(N_teams * inv(N_teams - 1));
}
"

## 2.3 ssm2 ----
### Fit ----
#### generated quantities ----
block_generated_quantities_ssm2 <- "
generated quantities {
  // Snapshot at end of training window (t-1)
  int last_w = week_id[N_obs];
  int last_s = season_id[N_obs];
  vector[N_teams] team_strength_last = team_strength[last_w];
  real            league_hfa_last    = league_hfa[last_s];
  vector[N_teams] team_hfa_last      = team_hfa[last_s];
  
  // Fitted means & posterior predictive for observed games
  vector[N_obs] mu_obs;
  vector[N_obs] y_rep;

  for (g in 1:N_obs) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_obs[g] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[g];
    y_rep[g]  = normal_rng(mu_obs[g], sigma_y);
  }
  
  // Optional: out-of-sample predictions by index into N_games
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  
  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_pred[k] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[k];
    y_pred[k] = normal_rng(mu_pred[k], sigma_y);
  }
}
"

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. Write Stan Files ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 3.1 baseline ----
code_baseline <- paste(
  block_data,
  block_transformed_data,
  block_parameters,
  block_transformed_parameters,
  block_model,
  block_generated_quantities
)
cat(code_baseline)
loc_baseline <- write_stan_file(code_baseline, dir = save_root, basename = "baseline")

code_baseline_gq <- paste(
  block_data_gq,
  #block_parameters_gq,
  #block_transformed_parameters_gq,
  block_generated_quantities_gq
)
cat(code_baseline_gq)
loc_baseline_gq <- write_stan_file(code_baseline_gq, dir = save_root, basename = "baseline_gq")


## 3.2 ssm1 ----
code_ssm1 <- paste(
  block_data,
  block_transformed_data_ssm1,
  block_parameters,
  block_transformed_parameters,
  block_model,
  block_generated_quantities
)
cat(code_ssm1)
loc_ssm1 <- write_stan_file(code_ssm1, dir = save_root, basename = "ssm1")

code_ssm1_gq <- paste(
  block_data_gq,
  block_parameters_gq,
  block_transformed_parameters_gq,
  block_generated_quantities_gq
)
cat(code_ssm1_gq)
loc_ssm1_gq <- write_stan_file(code_ssm1_gq, dir = save_root, basename = "ssm1_gq")

## 3.3 ssm2 ----
### fit ----
code_ssm2 <- paste(
  block_data,
  block_transformed_data_ssm1,
  block_parameters,
  block_transformed_parameters,
  block_model,
  block_generated_quantities_ssm2
)
cat(code_ssm2)
loc_ssm2 <- write_stan_file(code_ssm2, dir = save_root, basename = "ssm2")

### gq ----
code_ssm2_gq <- paste(
  block_data_gq,
  block_parameters_gq,
  block_transformed_parameters_gq,
  block_generated_quantities_gq
)
cat(code_ssm2_gq)
loc_ssm2_gq <- write_stan_file(code_ssm2_gq, dir = save_root, basename = "ssm2_gq")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. Make Stan Data ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Global Inputs ----
df              <- game_fit_data_all
start_train     <- list(season = 2023, week = 1)
end_train_init  <- list(season = 2024, week = 16)
end_train_final <- list(season = 2024, week = 22)
n_draws_gq      <- 100
save_root       <- "Model Fitting/ssm_check"
iter_warmup     <- 250
iter_sampling   <- 500
chains          <- 4
adapt_delta     <- 0.9
max_treedepth   <- 10
sig_figs        <- 16

## week_tbl ----
week_tbl <- build_week_table(df)

## idx ----
teams   <- sort(unique(c(df$home_team, df$away_team)))
seasons <- sort(unique(df$season))
weeks   <- sort(unique(df$week_idx))

team_idx <- setNames(1:32, teams)
season_idx <- setNames(unique(week_tbl$season_idx), unique(week_tbl$season))
week_idx   <- setNames(week_tbl$week_idx, 
                       paste0("[", week_tbl$season_idx, ",", week_tbl$week, "]"))

N_teams   <- length(unique(df$home_id))
N_seasons <- max(df$season_idx)
N_weeks   <- max(df$week_idx)

## stan_data ----
stan_data <- make_stan_data(
  df,
  start_train$season, start_train$week,
  end_train_init$season, end_train_init$week
)

## forecast_data ----
next_week <- next_week_after(week_tbl, end_train_init$season, end_train_init$week)
next_sched <- schedule_for(df, next_week$season, next_week$week)
stan_data_forecast <- append_sched_to_stan_data(stan_data, next_sched)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 4. Fit Models ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## 4.1 baseline ----
mod_baseline <- cmdstan_model(loc_baseline)
mod_baseline_gq <- cmdstan_model(loc_baseline_gq)

fit_baseline <- fit_state_space(
  mod_baseline, stan_data, inits = 0,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth, sig_figs = sig_figs
)
fit_baseline$time()
fit_baseline$init_model_methods()
fit_baseline$save_output_files(dir = save_root, basename = "baseline", 
                               random = FALSE, timestamp = FALSE)
fit_baseline_csv <- read_cmdstan_csv(fit_baseline$output_files())
fit_baseline_draws_csv <- fit_baseline_csv$post_warmup_draws |> as_draws_df()

gq_baseline <- mod_baseline_gq$generate_quantities(
  fit_baseline, data = stan_data_forecast, 
  seed = 52, sig_figs = sig_figs
)
gq_baseline$time()
gq_baseline$code()
gq_baseline$init_model_methods()
gq_baseline$save_output_files(dir = save_root, basename = "baseline_gq", 
                              random = FALSE, timestamp = FALSE)
gq_baseline_csv <- read_cmdstan_csv(gq_baseline$output_files())
gq_baseline_draws_csv <- gq_baseline_csv$post_warmup_draws |> as_draws_df()


## 4.2 ssm1 ----
mod_ssm1 <- cmdstan_model(loc_ssm1, compile_model_methods = TRUE)
mod_ssm1_gq <- cmdstan_model(loc_ssm1_gq)

fit_ssm1 <- fit_state_space(
  mod_ssm1, stan_data, inits = 0,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth, sig_figs = sig_figs
)
fit_ssm1$time()
fit_ssm1$code()
fit_ssm1$init_model_methods()

fit_ssm1_meta <- fit_ssm1$metadata()
fit_ssm1_params <-names(mod_ssm1$variables()$parameters)
fit_ssm1_draws <- fit_ssm1$draws()

zero_list <- setNames(rep(list(0), length(fit_ssm1_params)), fit_ssm1_params)
fit_ssm1$unconstrain_draws(draws = fit_ssm1_draws)


fit_ssm1$save_output_files(dir = save_root, basename = "ssm1", 
                               random = FALSE, timestamp = FALSE)
fit_ssm1_csv <- read_cmdstan_csv(fit_ssm1$output_files())
fit_ssm1_draws_csv <- fit_ssm1_csv$post_warmup_draws

gq_ssm1 <- mod_ssm1_gq$generate_quantities(
  fit_ssm1$output_files(), data = stan_data_forecast, 
  seed = 52, sig_figs = sig_figs
)
gq_ssm1$time()
gq_ssm1$code()
gq_ssm1$init_model_methods()
gq_ssm1$save_output_files(dir = save_root, basename = "ssm1_gq", 
                              random = FALSE, timestamp = FALSE)
gq_ssm1_csv <- read_cmdstan_csv(gq_ssm1$output_files())
gq_ssm1_draws_csv <- gq_ssm1_csv$post_warmup_draws |> as_draws_df()


## 4.3 ssm2 ----
cat(code_ssm2)
mod_ssm2 <- cmdstan_model(loc_ssm2, compile_model_methods = TRUE, pedantic = TRUE)
cat(code_ssm2_gq)
mod_ssm2_gq <- cmdstan_model(loc_ssm2_gq)

fit_ssm2 <- fit_state_space(
  mod_ssm2, stan_data_forecast, 
  inits = 0,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, chains = chains,
  adapt_delta = adapt_delta, max_treedepth = max_treedepth, sig_figs = sig_figs
)
fit_ssm2$time()
fit_ssm2$code()

fit_ssm2_meta <- fit_ssm2$metadata()
fit_ssm2_params <- names(mod_ssm2$variables()$parameters)

fit_ssm2_draws <- fit_ssm2$draws()
fit_ssm2_draws_df <- fit_ssm2_draws |> as_draws_df()
fit_ssm2_sum <- fit_ssm2_draws_df |> colMeans() |> enframe(name = "variable", value = "mean")
#fit_ssm2_sum2 <- fit_ssm2$summary()

zero_list <- setNames(rep(list(0), length(fit_ssm2_params)), fit_ssm2_params)
fit_ssm2$unconstrain_draws(draws = fit_ssm2_draws)


fit_ssm2$save_output_files(dir = save_root, basename = "ssm2", 
                           random = FALSE, timestamp = FALSE)
fit_ssm2_csv <- read_cmdstan_csv(fit_ssm2$output_files())
fit_ssm2_draws_csv <- fit_ssm2_csv$post_warmup_draws 
fit_ssm2_draws_csv_df <- fit_ssm2_draws_csv |> as_draws_df()
fit_ssm2_csv_sum <- fit_ssm2_draws_csv_df |> colMeans() |> enframe(name = "variable", value = "mean")

### 4.3.2 gq -----
gq_ssm2 <- mod_ssm2_gq$generate_quantities(
  fit_ssm2$output_files(), data = stan_data_forecast, 
  seed = 52, sig_figs = 4
)
gq_ssm2$print()
gq_ssm2$time()
gq_ssm2$output()
gq_ssm2$init_model_methods()
gq_ssm2$save_output_files(dir = save_root, basename = "ssm2_gq", 
                          random = FALSE, timestamp = FALSE)
gq_ssm2_csv <- read_cmdstan_csv(gq_ssm2$output_files())
gq_ssm2_draws_csv <- gq_ssm2_csv$post_warmup_draws |> as_draws_df()
gq_ssm2_sum <- gq_ssm2$summary()


