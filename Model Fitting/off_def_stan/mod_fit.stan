// Global-week state-space model (Glickman-style) - ENHANCED
// - Uses sum_to_zero_vector for efficient identifiability
// - Non-centered innovations for better sampling
// - Leverages provided global week_idx and season boundary indicators
// - ENHANCEMENTS: Function for mu computation, complete predictions, log-likelihood

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
  array[N_weeks] int week_to_season = rep_array(1, N_weeks); // season index for each global week
  array[N_weeks] int is_first_week = rep_array(0, N_weeks); // 1 if first week of its season
  array[N_weeks] int is_last_week = rep_array(0, N_weeks); // 1 if last week of its season
  
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
  
  // Pre-compute last observed week for efficiency
  // int last_observed_week = max(week_idx);

  real sum_to_zero_scale = sqrt(N_teams * inv(N_teams - 1)); // for sum-to-zero transformation
}

parameters {
  // League HFA AR(1) across seasons
  real league_hfa_init;                         // centered initial league HFA at season 1
  
  vector[N_seasons - 1] z_league_hfa_innovation;    // standardized season innovations, scaled in TP
  real<lower=0> sigma_league_hfa_innovation;    // scale season innovations, scaled in TP
  real<lower=0, upper=1> phi_league_hfa;        // persistence of league HFA across seasons

  // Team HFA deviations per season (sum-to-zero around league HFA)
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation; // standardized team HFA deviation
  real<lower=0> sigma_team_hfa;                                      // scale team HFA deviations
  
  // Team strengths with AR(1) accross weeks and seasonal regression to mean
  sum_to_zero_vector[N_teams] z_team_strength_init;   // standardized initial team strengths at global week 1
  real<lower=0> sigma_team_strength_init;                  // scale initial team strengths
  
  // Global weekly innovations and season-carryover innovations (sum-to-zero)
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_team_strength_innovation;
  real<lower=0> sigma_weekly_team_strength_innovation;
  real<lower=0, upper=1> phi_weekly_team_strength_innovation; // persistence of team strengths across weeks
  
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_team_strength_innovation;
  real<lower=0> sigma_season_team_strength_innovation;
  real<lower=0, upper=1> phi_season_team_strength_innovation; // persistence of team strengths across seasons
  
  // Observation noise
  real<lower=0> sigma_obs;
}

transformed parameters {
  // League HFA
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + z_league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  }

  // Team HFA
  array[N_seasons] vector[N_teams] team_hfa;
  for(s in 1 : N_seasons) {
    team_hfa[s] = league_hfa[s] + z_team_hfa_deviation[s] * sigma_team_hfa;
  }
  
  // Team Strength
  array[N_weeks] vector[N_teams] team_strength;
  // Initialize state at global week 1
  team_strength[1] = z_team_strength_init * sigma_team_strength_init;
  // Global-week evolution
  for (w in 2 : N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w]; // current season of week w
      // carryover: use season index s-1 for z_season_carryover
      team_strength[w] = phi_season_team_strength_innovation * team_strength[w - 1]
                         + z_season_team_strength_innovation[s - 1] * sigma_season_team_strength_innovation;
    } else {
      team_strength[w] = phi_weekly_team_strength_innovation * team_strength[w - 1]
                         + z_weekly_team_strength_innovation[w - 1] * sigma_weekly_team_strength_innovation;
    }
  }
  
  vector[N_games] mu = compute_mu(home_team, away_team, 
                                  week_idx, season_idx,
                                  hfa, team_strength, team_hfa, 
                                  N_games);
}

model {
  // Priors (tuneable)
  // League HFA
  league_hfa_init ~ normal(3, 2);

  z_league_hfa_innovation ~ std_normal(); //normal(0, sum_to_zero_scale);
  sigma_league_hfa_innovation ~ student_t(3, 0, 2); // exponential(1); //gamma(2, 0.1); normal(0, 5);
  phi_league_hfa ~ beta(8, 2); // moderately persistent
  
  // Team HFA
  for (s in 1 : N_seasons) z_team_hfa_deviation[s] ~ std_normal(); //normal(0, sum_to_zero_scale);
  sigma_team_hfa ~ student_t(3, 0, 2); // exponential(1); //gamma(2, 0.1); normal(0, 5);

  // Team Strength
  z_team_strength_init ~ std_normal(); //normal(0, sum_to_zero_scale);
  sigma_team_strength_init ~ student_t(3, 0, 5); // exponential(1); //gamma(2, 0.1); normal(0, 5);
  
  for (w in 1 : (N_weeks - 1)) z_weekly_team_strength_innovation[w] ~ std_normal(); //normal(0, sum_to_zero_scale);
  sigma_weekly_team_strength_innovation ~ student_t(3, 0, 2); // exponential(1); //gamma(2, 0.1); normal(0, 5);
  phi_weekly_team_strength_innovation ~ beta(9, 1); // strong weekly persistence
  
  for (s in 1 : (N_seasons - 1)) z_season_team_strength_innovation[s] ~ std_normal(); //normal(0, sum_to_zero_scale);
  sigma_season_team_strength_innovation ~ student_t(3, 0, 5); // exponential(1); //gamma(2, 0.1); normal(0, 5);
  phi_season_team_strength_innovation ~ beta(6, 4); // moderate season carryover
  
  sigma_obs ~ student_t(3, 0, 10); // exponential(1); //gamma(2, 0.1); normal(0, 5);
  
  // Likelihood - using function for cleaner code
  result ~ normal(mu, sigma_obs);
}

generated quantities {
  // Last observed global week and its season
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];
  
  // Filtered (= smoothed at final time) state snapshots
  vector[N_teams] filtered_team_strength = team_strength[last_w];
  vector[N_teams] filtered_team_hfa = team_hfa[last_s];
  real filtered_league_hfa = league_hfa[last_s];
  
  // One-week-ahead predictions
  // For team strength: both mean (expected value) and draw (with innovation uncertainty)
  vector[N_teams] predicted_team_strength;
  vector[N_teams] predicted_team_hfa;
  real predicted_league_hfa;
  
  {
    int next_is_first = is_last_week[last_w];
    
    // Team strength predictions
    // Generate sum-to-zero innovation for team strength
    vector[N_teams - 1] z_raw;
    for (t in 1 : (N_teams - 1)) {
      z_raw[t] = normal_rng(0, 1);
    }
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    
    if (next_is_first == 1) {
      predicted_team_strength = phi_season_team_strength_innovation * team_strength[last_w]
                                     + z0 * sigma_season_team_strength_innovation;
    } else {
      predicted_team_strength = phi_weekly_team_strength_innovation * team_strength[last_w]
                                     + z0 * sigma_weekly_team_strength_innovation;
    }
    
    // HFA predictions
    if (next_is_first == 1) {
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
  
  // Log-likelihood for model comparison (LOO-CV, WAIC)
  vector[N_games] log_lik;
  for (g in 1 : N_games) {
    log_lik[g] = normal_lpdf(result[g] | mu[g], sigma_obs);
  }
  // {
  //   vector[N_games] mu = compute_mu(home_team, away_team, week_idx, season_idx,
  //                                   hfa, team_strength, team_hfa, N_games);
  //   for (g in 1:N_games) {
  //     log_lik[g] = normal_lpdf(result[g] | mu[g], sigma_obs);
  //   }
  // }
}
