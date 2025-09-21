// Global-week state-space model - FULLY NON-CENTERED
functions {
  // Centralized computation of game expected values
  vector compute_mu(array[] int home_team, array[] int away_team,
                    array[] int week_idx, array[] int season_idx,
                    array[] int hfa, array[] vector team_strength,
                    array[] vector team_hfa, int N_games) {
    vector[N_games] mu;
    for (g in 1 : N_games) {
      int w = week_idx[g];
      int s = season_idx[g];
      int h = home_team[g];
      int a = away_team[g];
      mu[g] = team_strength[w][h] - team_strength[w][a];
      if (hfa[g] == 1) {
        mu[g] += team_hfa[s][h];
      }
    }
    return mu;
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
  
  // Response variables
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  array[N_games] int<lower=0> total;
  array[N_games] int result;
}
transformed data {
  // Map global weeks to seasons and mark season boundaries
  array[N_weeks] int week_to_season = rep_array(1, N_weeks);
  array[N_weeks] int is_first_week = rep_array(0, N_weeks);
  array[N_weeks] int is_last_week = rep_array(0, N_weeks);
  
  // Aggregate game-level boundary indicators to global-week flags
  for (g in 1 : N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    if (s > week_to_season[w]) 
      week_to_season[w] = s;
    if (fw_season_idx[g] == 1) 
      is_first_week[w] = 1;
    if (lw_season_idx[g] == 1) 
      is_last_week[w] = 1;
  }
  is_first_week[1] = 1; // Ensure first week is marked as first
}
parameters {
  // League HFA AR(1) - fully non-centered
  real league_hfa_init_z; // standardized
  real<lower=0> mu_league_hfa_init;
  real<lower=0> sigma_league_hfa_init;
  
  vector[N_seasons - 1] league_hfa_innovation_z; // standardized
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;
  
  // Team HFA deviations - raw parameters for non-centered
  array[N_seasons] vector[N_teams - 1] team_hfa_deviation_raw;
  real<lower=0> sigma_team_hfa;
  
  // Initial team strengths - raw parameters
  vector[N_teams - 1] team_strength_init_raw;
  real<lower=0> sigma_strength_init;
  
  // Weekly and seasonal innovations - raw parameters
  array[N_weeks - 1] vector[N_teams - 1] z_weekly_innovation_raw;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;
  
  array[N_seasons - 1] vector[N_teams - 1] z_season_carryover_raw;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;
  
  // Observation noise
  real<lower=0> sigma_obs;
}
transformed parameters {
  // Apply non-centered parameterization for league HFA
  real league_hfa_init = mu_league_hfa_init
                         + league_hfa_init_z * sigma_league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation = league_hfa_innovation_z
                                                * sigma_league_hfa_innovation;
  
  // Apply sum-to-zero constraints to raw parameters
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  sum_to_zero_vector[N_teams] team_strength_init;
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  
  // Constrain raw parameters to sum-to-zero
  for (s in 1 : N_seasons) {
    team_hfa_deviation[s] = sum_to_zero_constrain(team_hfa_deviation_raw[s]);
  }
  team_strength_init = sum_to_zero_constrain(team_strength_init_raw);
  for (w in 1 : (N_weeks - 1)) {
    z_weekly_innovation[w] = sum_to_zero_constrain(
                               z_weekly_innovation_raw[w]);
  }
  for (s in 1 : (N_seasons - 1)) {
    z_season_carryover[s] = sum_to_zero_constrain(z_season_carryover_raw[s]);
  }
  
  // League/Team HFA
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;
  
  // Global-week latent strengths
  array[N_weeks] vector[N_teams] team_strength;
  
  // League HFA AR(1) evolution
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1];
  }
  
  // Team HFA per season
  for (s in 1 : N_seasons) {
    team_hfa[s] = league_hfa[s] + team_hfa_deviation[s] * sigma_team_hfa;
  }
  
  // Initialize state at global week 1
  team_strength[1] = team_strength_init * sigma_strength_init;
  
  // Global-week evolution
  for (w in 2 : N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_strength[w] = phi_season_carryover * team_strength[w - 1]
                         + z_season_carryover[s - 1] * sigma_season_carryover;
    } else {
      team_strength[w] = phi_weekly * team_strength[w - 1]
                         + z_weekly_innovation[w - 1]
                           * sigma_weekly_innovation;
    }
  }
  
  // Pre-compute mu
  vector[N_games] mu = compute_mu(home_team, away_team, week_idx, season_idx,
                                  hfa, team_strength, team_hfa, N_games);
}
model {
  // Priors on location/scale parameters
  mu_league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_init ~ normal(0, 1);
  //sigma_league_hfa_innovation ~ normal(0.5, 0.25);
  //phi_league_hfa ~ beta(16, 4);  // Mean ~0.8, tighter
  
  //sigma_team_hfa ~ normal(0.5, 0.25);
  //sigma_strength_init ~ normal(2, 0.5);
  //sigma_weekly_innovation ~ normal(0.5, 0.25);
  //sigma_season_carryover ~ normal(1, 0.5);
  //phi_weekly ~ beta(18, 2);  // Mean ~0.9, tighter
  //phi_season_carryover ~ beta(9, 6);  // Mean ~0.6
  
  //sigma_obs ~ normal(14, 1);
  
  // Priors (tuneable)
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 1);
  phi_league_hfa ~ beta(8, 2); // moderately persistent
  
  sigma_team_hfa ~ normal(0, 1);
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1); // strong weekly persistence
  phi_season_carryover ~ beta(6, 4); // moderate season carryover
  
  sigma_obs ~ normal(0, 5);
  
  // All raw parameters have standard normal priors
  league_hfa_init_z ~ std_normal();
  league_hfa_innovation_z ~ std_normal();
  
  for (s in 1 : N_seasons) {
    team_hfa_deviation_raw[s] ~ std_normal();
  }
  team_strength_init_raw ~ std_normal();
  
  for (w in 1 : (N_weeks - 1)) {
    z_weekly_innovation_raw[w] ~ std_normal();
  }
  for (s in 1 : (N_seasons - 1)) {
    z_season_carryover_raw[s] ~ std_normal();
  }
  
  // Likelihood
  result ~ normal(mu, sigma_obs);
}
generated quantities {
  // Last observed global week and its season
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  // Filtered (= smoothed at final time) state snapshots
  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];
  
  // One-week-ahead predictions
  vector[N_teams] predicted_team_strength_mean;
  vector[N_teams] predicted_team_strength_draw;
  
  // For HFA: predictions for next season if at season boundary
  real predicted_league_hfa;
  vector[N_teams] predicted_team_hfa;
  
  {
    int next_is_first = is_last_week[last_w];
    
    // Team strength predictions
    if (next_is_first == 1) {
      predicted_team_strength_mean = phi_season_carryover
                                     * team_strength[last_w];
    } else {
      predicted_team_strength_mean = phi_weekly * team_strength[last_w];
    }
    
    // Generate sum-to-zero innovation for team strength
    vector[N_teams - 1] z_raw;
    for (t in 1 : (N_teams - 1)) {
      z_raw[t] = normal_rng(0, 1);
    }
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    
    if (next_is_first == 1) {
      predicted_team_strength_draw = predicted_team_strength_mean
                                     + z0 * sigma_season_carryover;
    } else {
      predicted_team_strength_draw = predicted_team_strength_mean
                                     + z0 * sigma_weekly_innovation;
    }
    
    // HFA predictions
    if (next_is_first == 1 && last_s < N_seasons) {
      // Predict next season's HFA
      predicted_league_hfa = phi_league_hfa * league_hfa[last_s]
                             + normal_rng(0, sigma_league_hfa_innovation);
      
      // Generate team HFA deviations for next season
      vector[N_teams - 1] hfa_raw;
      for (t in 1 : (N_teams - 1)) {
        hfa_raw[t] = normal_rng(0, 1);
      }
      vector[N_teams] hfa_dev = sum_to_zero_constrain(hfa_raw);
      predicted_team_hfa = predicted_league_hfa + hfa_dev * sigma_team_hfa;
    } else {
      // If not starting new season, HFA stays same
      predicted_league_hfa = league_hfa[last_s];
      predicted_team_hfa = team_hfa[last_s];
    }
  }
  
  // Log-likelihood for model comparison
  vector[N_games] log_lik;
  for (g in 1 : N_games) {
    log_lik[g] = normal_lpdf(result[g] | mu[g], sigma_obs);
  }
}
