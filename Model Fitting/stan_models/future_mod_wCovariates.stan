// Improved Global-week state-space model for NFL team strengths
// Enhancements:
// - Student-t likelihood for robustness
// - Vectorized operations for efficiency
// - Time-varying observation variance
// - Game context covariates
// - Hierarchical priors
// - Complete prediction functionality
// - LOO-CV support

functions {
  // Helper function to create team difference vectors efficiently
  vector compute_team_diffs(array[] int home_team, array[] int away_team,
                           array[] vector team_strength, array[] int week_idx,
                           int N_games) {
    vector[N_games] diff;
    for (g in 1:N_games) {
      diff[g] = team_strength[week_idx[g]][home_team[g]] - 
                team_strength[week_idx[g]][away_team[g]];
    }
    return diff;
  }
  
  // Helper function for HFA effects
  vector compute_hfa_effects(array[] int home_team, array[] int season_idx,
                            array[] vector team_hfa, array[] int hfa_indicator,
                            int N_games) {
    vector[N_games] hfa_effect;
    for (g in 1:N_games) {
      hfa_effect[g] = hfa_indicator[g] * team_hfa[season_idx[g]][home_team[g]];
    }
    return hfa_effect;
  }
}

data {
  int<lower=1> N_games;
  int<lower=2> N_teams; // number of teams = 32
  int<lower=1> N_seasons; // number of seasons starting from 1 = 2002
  int<lower=1> N_weeks; // number of weeks starting from 1 = 2002 week 1
  
  // Indexing variables
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx; // global season index
  array[N_games] int<lower=1, upper=N_weeks> week_idx; // global week index
  
  // Indicator variables for season transitions
  array[N_games] int<lower=0, upper=1> fw_season_idx; // First week of season (by game)
  array[N_games] int<lower=0, upper=1> lw_season_idx; // Last week of season (by game)
  array[N_games] int<lower=0, upper=1> hfa; // Home-field advantage indicator
  
  // Game context covariates (optional - set to 0 if not available)
  array[N_games] int<lower=0, upper=1> playoff_game;
  array[N_games] int<lower=0, upper=1> division_game;
  array[N_games] int<lower=0, upper=1> primetime_game; // MNF, SNF, TNF
  array[N_games] real rest_days_diff; // home team rest days - away team rest days
  array[N_games] real travel_distance; // standardized travel distance for away team
  
  // Response variables
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  array[N_games] int<lower=0> total;
  array[N_games] int result; // point differential (home - away)
  
  // Missing data indicators (1 = observed, 0 = missing)
  array[N_games] int<lower=0, upper=1> is_observed;
  
  // New team indicators (for expansions/relocations)
  array[N_teams] int<lower=0, upper=1> is_new_team;
  array[N_teams] int<lower=0, upper=N_teams> parent_team; // 0 if no parent
  
  // Hyperparameter tuning (set reasonable defaults)
  real<lower=0> prior_df_nu; // suggested: 7
  real<lower=0> prior_scale_obs; // suggested: 13
}

transformed data {
  // Map global weeks to seasons and mark season boundaries
  array[N_weeks] int week_to_season = rep_array(1, N_weeks);
  array[N_weeks] int is_first_week = rep_array(0, N_weeks);
  array[N_weeks] int is_last_week = rep_array(0, N_weeks);
  
  // Count games per week for efficient computation
  array[N_weeks] int n_games_per_week = rep_array(0, N_weeks);
  
  // Aggregate game-level boundary indicators to global-week flags
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    n_games_per_week[w] += 1;
    if (s > week_to_season[w]) week_to_season[w] = s;
    if (fw_season_idx[g] == 1) is_first_week[w] = 1;
    if (lw_season_idx[g] == 1) is_last_week[w] = 1;
  }
  is_first_week[1] = 1; // Ensure first week is marked as first
  
  // Standardize continuous covariates
  real mean_rest = mean(rest_days_diff);
  real sd_rest = sd(rest_days_diff);
  real mean_travel = mean(travel_distance);
  real sd_travel = sd(travel_distance);
  
  array[N_games] real rest_days_std;
  array[N_games] real travel_distance_std;
  for (g in 1:N_games) {
    rest_days_std[g] = (rest_days_diff[g] - mean_rest) / (sd_rest + 1e-9);
    travel_distance_std[g] = (travel_distance[g] - mean_travel) / (sd_travel + 1e-9);
  }
}

parameters {
  // Observation model parameters
  real<lower=1> nu; // Student-t degrees of freedom
  real<lower=0> sigma_obs_base;
  vector<lower=0>[N_seasons] sigma_obs_multiplier; // Season-specific variance multipliers
  real<lower=0> sigma_obs_hyper; // Hyperparameter for variance multipliers
  
  // League HFA AR(1) across seasons
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation; // std_normal, scaled in TP
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  // Team HFA deviations per season (sum-to-zero around league HFA)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  // Initial team strengths at global week 1 (sum-to-zero)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  // Global weekly innovations and season-carryover innovations (sum-to-zero)
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;

  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;
  
  // Hierarchical hyperparameters for innovation variances
  real<lower=0> mu_sigma_innovation;
  real<lower=0> tau_sigma_innovation;
  
  // Game context effects
  real beta_playoff;
  real beta_division;
  real beta_primetime;
  real beta_rest;
  real beta_travel;
  
  // Hyperparameters for persistence parameters (hierarchical beta)
  real<lower=1> alpha_weekly;
  real<lower=1> beta_weekly;
  real<lower=1> alpha_season;
  real<lower=1> beta_season;
  
  // Bounds for numerical stability
  real<lower=20, upper=50> team_strength_max_bound;
}

transformed parameters {
  // League/Team HFA
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;

  // Global-week latent strengths
  array[N_weeks] vector[N_teams] team_strength;
  
  // Season-specific observation variance
  vector<lower=0>[N_seasons] sigma_obs_season;

  // League HFA AR(1) over seasons (non-centered via innovation scaling)
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  }

  // Team HFA per season
  for (s in 1:N_seasons) {
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;
  }
  
  // Season-specific variance
  sigma_obs_season = sigma_obs_base * sigma_obs_multiplier;

  // Initialize state at global week 1
  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;

  // Global-week evolution
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      // Season carryover with potential adjustments for new teams
      vector[N_teams] carryover_base = phi_season_carryover * team_strength[w - 1];
      team_strength[w] = carryover_base + 
                        to_vector(z_season_carryover[s - 1]) * sigma_season_carryover;
      
      // Handle new teams (expansions/relocations)
      for (t in 1:N_teams) {
        if (is_new_team[t] == 1 && parent_team[t] > 0) {
          // Inherit strength from parent team with some noise
          team_strength[w][t] = team_strength[w - 1][parent_team[t]] + 
                                normal_rng(0, sigma_season_carryover);
        }
      }
    } else {
      // Regular weekly evolution
      team_strength[w] = phi_weekly * team_strength[w - 1]
                        + to_vector(z_weekly_innovation[w - 1]) * sigma_weekly_innovation;
    }
    
    // Apply soft bounds for stability
    for (t in 1:N_teams) {
      if (abs(team_strength[w][t]) > team_strength_max_bound) {
        team_strength[w][t] = team_strength_max_bound * 
                              (team_strength[w][t] / abs(team_strength[w][t]));
      }
    }
  }
}

model {
  // Hierarchical priors for innovation standard deviations
  mu_sigma_innovation ~ normal(0, 1);
  tau_sigma_innovation ~ normal(0, 0.5);
  sigma_weekly_innovation ~ normal(mu_sigma_innovation, tau_sigma_innovation);
  sigma_season_carryover ~ normal(mu_sigma_innovation * 1.5, tau_sigma_innovation);
  
  // Priors for observation model
  nu ~ gamma(prior_df_nu, 1); // Prior centered around prior_df_nu
  sigma_obs_base ~ normal(prior_scale_obs, 3);
  sigma_obs_hyper ~ normal(0, 0.5);
  sigma_obs_multiplier ~ lognormal(0, sigma_obs_hyper);
  
  // League HFA priors
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 1);
  
  // Hierarchical beta priors for persistence
  alpha_weekly ~ gamma(9, 1);
  beta_weekly ~ gamma(1, 1);
  phi_weekly ~ beta(alpha_weekly, beta_weekly);
  
  alpha_season ~ gamma(6, 1);
  beta_season ~ gamma(4, 1);
  phi_season_carryover ~ beta(alpha_season, beta_season);
  phi_league_hfa ~ beta(8, 2);

  // Team-level priors
  sigma_team_hfa ~ normal(0, 1);
  sigma_strength_init ~ normal(0, 3);
  
  // Game context effect priors
  beta_playoff ~ normal(0, 3);
  beta_division ~ normal(0, 2);
  beta_primetime ~ normal(0, 1.5);
  beta_rest ~ normal(0, 1);
  beta_travel ~ normal(0, 1);
  
  // Stability prior
  team_strength_max_bound ~ normal(35, 5);

  // Non-centered innovation priors
  league_hfa_innovation ~ std_normal();
  for (s in 1:N_seasons) {
    to_vector(team_hfa_deviation[s]) ~ std_normal();
  }
  to_vector(team_strength_init) ~ std_normal();
  for (w in 1:(N_weeks - 1)) {
    to_vector(z_weekly_innovation[w]) ~ std_normal();
  }
  for (s in 1:(N_seasons - 1)) {
    to_vector(z_season_carryover[s]) ~ std_normal();
  }

  // Vectorized likelihood computation
  vector[N_games] mu = compute_team_diffs(home_team, away_team, team_strength, week_idx, N_games)
                      + compute_hfa_effects(home_team, season_idx, team_hfa, hfa, N_games);
  
  // Add game context effects
  for (g in 1:N_games) {
    mu[g] += beta_playoff * playoff_game[g]
           + beta_division * division_game[g]
           + beta_primetime * primetime_game[g]
           + beta_rest * rest_days_std[g]
           + beta_travel * travel_distance_std[g];
  }
  
  // Observation model with missing data handling
  for (g in 1:N_games) {
    if (is_observed[g] == 1) {
      result[g] ~ student_t(nu, mu[g], sigma_obs_season[season_idx[g]]);
    }
  }
}

generated quantities {
  // Last observed global week and its season
  int last_w = 1;
  for (g in 1:N_games) {
    if (week_idx[g] > last_w) last_w = week_idx[g];
  }
  int last_s = week_to_season[last_w];

  // Filtered (smoothed at final time) state snapshots
  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];
  
  // Current team rankings (1 = best)
  array[N_teams] int current_rank;
  {
    vector[N_teams] neg_strength = -filtered_team_strength_last;
    current_rank = sort_indices_asc(neg_strength);
  }

  // One-week-ahead prediction (mean and samples)
  vector[N_teams] predicted_team_strength_next_mean;
  vector[N_teams] predicted_team_strength_next_draw;
  {
    int next_is_first = is_last_week[last_w];
    
    // Compute mean prediction
    if (next_is_first == 1) {
      predicted_team_strength_next_mean = phi_season_carryover * team_strength[last_w];
    } else {
      predicted_team_strength_next_mean = phi_weekly * team_strength[last_w];
    }

    // Generate sum-to-zero innovation
    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) {
      z_raw[t] = normal_rng(0, 1);
    }
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    
    // Add appropriate innovation
    if (next_is_first == 1) {
      predicted_team_strength_next_draw = predicted_team_strength_next_mean
                                        + z0 * sigma_season_carryover;
    } else {
      predicted_team_strength_next_draw = predicted_team_strength_next_mean
                                        + z0 * sigma_weekly_innovation;
    }
  }
  
  // Log likelihood for LOO-CV and model comparison
  array[N_games] real log_lik;
  array[N_games] real result_pred;
  array[N_games] real result_pred_home_win_prob;
  
  {
    vector[N_games] mu = compute_team_diffs(home_team, away_team, team_strength, week_idx, N_games)
                        + compute_hfa_effects(home_team, season_idx, team_hfa, hfa, N_games);
    
    // Add game context effects
    for (g in 1:N_games) {
      mu[g] += beta_playoff * playoff_game[g]
             + beta_division * division_game[g]
             + beta_primetime * primetime_game[g]
             + beta_rest * rest_days_std[g]
             + beta_travel * travel_distance_std[g];
      
      // Log likelihood (only for observed games)
      if (is_observed[g] == 1) {
        log_lik[g] = student_t_lpdf(result[g] | nu, mu[g], sigma_obs_season[season_idx[g]]);
      } else {
        log_lik[g] = 0; // Placeholder for missing data
      }
      
      // Posterior predictive draws
      result_pred[g] = student_t_rng(nu, mu[g], sigma_obs_season[season_idx[g]]);
      
      // Win probability for home team
      result_pred_home_win_prob[g] = student_t_cdf(0, nu, -mu[g], sigma_obs_season[season_idx[g]]);
    }
  }
  
  // Season-level summaries
  real mean_home_advantage_current = mean(filtered_team_hfa_last);
  real sd_team_strength_current = sd(filtered_team_strength_last);
  
  // Convergence diagnostics helper
  real max_team_strength = max(abs(filtered_team_strength_last));
}