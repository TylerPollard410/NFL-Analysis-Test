data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;
  
  // Indexing variables
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx;
  array[N_games] int<lower=1, upper=N_weeks> week_idx;

  // Indicator variables for season transitions
  array[N_games] int<lower=0, upper=1> fw_season_idx; // First week of season
  array[N_games] int<lower=0, upper=1> lw_season_idx; // Last week of season
  array[N_games] int<lower=0, upper=1> hfa;

  // Response variables
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  array[N_games] int<lower=0> total;
  array[N_games] int result;
}

parameters {
  // AR(1) League HFA process
  real league_hfa_init;
  vector[N_seasons-1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;
  
  // Team HFA deviations from league HFA (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;
  
  // Initial team strengths (sum-to-zero)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;
  
  // AR(1) Within-season weekly innovations (non-centered)
  array[N_weeks] vector[N_teams] weekly_innovation_raw;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;
  
  // AR(1) Season carryover innovations (non-centered)
  vector[N_teams] season_carryover_raw;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;
  
  // Observation model parameters
  real<lower=0> sigma_obs;
}

transformed parameters {
  // League HFA evolution (AR1 process)
  vector[N_seasons] league_hfa;
  
  // Team HFA (league HFA + sum-to-zero deviations)
  array[N_seasons] vector[N_teams] team_hfa;
  
  // Team strengths for all weeks (sum-to-zero at each time point)
  array[N_weeks] vector[N_teams] team_strength;
  
  // Initialize league HFA
  league_hfa[1] = league_hfa_init;
  
  // League HFA AR(1) evolution across seasons
  for (s in 2:N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s-1] + 
                    league_hfa_innovation[s-1] * sigma_league_hfa_innovation;
  }
  
  // Team HFA = league HFA + sum-to-zero deviations
  for (s in 1:N_seasons) {
    team_hfa[s] = rep_vector(league_hfa[s], N_teams) + 
                  to_vector(team_hfa_deviation[s]) * sigma_team_hfa;
  }
  
  // Initialize team strengths (first week overall) - already sum-to-zero
  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;
  
  // Team strength evolution using game-level indicators
  for (w in 2:N_weeks) {
    vector[N_teams] temp_strength;
    
    // Check if any game in this week is a first week of season
    int is_first_week = 0;
    for (g in 1:N_games) {
      if (week_idx[g] == w && fw_season_idx[g] == 1) {
        is_first_week = 1;
        break;
      }
    }
    
    if (is_first_week == 1) {
      // Season carryover: φ_sc * previous_week + innovation
      temp_strength = phi_season_carryover * team_strength[w-1] + 
                     season_carryover_raw * sigma_season_carryover;
    } else {
      // Weekly AR(1) evolution within season
      temp_strength = phi_weekly * team_strength[w-1] + 
                     weekly_innovation_raw[w] * sigma_weekly_innovation;
    }
    
    // Enforce sum-to-zero constraint
    team_strength[w] = sum_to_zero_vector(temp_strength);
  }
}

model {
  // Priors for league HFA AR(1) process
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 0.3);
  phi_league_hfa ~ beta(8, 2); // Favor persistence
  
  // Priors for team HFA
  sigma_team_hfa ~ normal(0, 1);
  // team_hfa_deviation has implicit sum-to-zero prior
  
  // Priors for team strength processes
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1); // Strong weekly persistence (≈0.9)
  phi_season_carryover ~ beta(6, 4); // Moderate season carryover (≈0.6)
  
  // Observation model
  sigma_obs ~ normal(0, 5);
  
  // Non-centered parameterization priors
  league_hfa_innovation ~ std_normal();
  season_carryover_raw ~ std_normal();
  // team_strength_init and team_hfa_deviation have implicit sum-to-zero priors
  
  for (w in 1:N_weeks) {
    weekly_innovation_raw[w] ~ std_normal();
  }
  
  // Likelihood
  for (g in 1:N_games) {
    real expected_result = team_strength[week_idx[g]][home_team[g]] - 
                          team_strength[week_idx[g]][away_team[g]] + 
                          hfa[g] * team_hfa[season_idx[g]][home_team[g]];
    result[g] ~ normal(expected_result, sigma_obs);
  }
}

generated quantities {

}