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
  vector[N_seasons - 1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real phi_league_hfa_raw; // Will transform to (0,1)
  
  // Team HFA deviations from league HFA (sum-to-zero constraint means centered around 0)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;
  
  // Initial team strengths (sum-to-zero at first week, so 0 = average team)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;
  
  // AR(1) Within-season weekly innovations (sum-to-zero constraint preserves sum-to-zero for team_strength)
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real phi_weekly_raw; // Will transform to (0,1)
  
  // AR(1) Season carryover innovations (sum-to-zero constraint preserves sum-to-zero for team_strength)
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real phi_season_carryover_raw; // Will transform to (0,1)
  
  // Observation model parameters
  real<lower=0> sigma_obs;
}
transformed parameters {
  // Transform phi parameters to (0,1) using logit
  real<lower=0, upper=1> phi_league_hfa = inv_logit(phi_league_hfa_raw);
  real<lower=0, upper=1> phi_weekly = inv_logit(phi_weekly_raw);
  real<lower=0, upper=1> phi_season_carryover = inv_logit(phi_season_carryover_raw);
  
  // League HFA evolution (AR1 process)
  vector[N_seasons] league_hfa;
  
  // Team HFA (league HFA + sum-to-zero deviations, so centered around league_hfa)
  array[N_seasons] vector[N_teams] team_hfa;
  
  // Team strengths for all weeks (sum-to-zero at each time point by construction)
  array[N_weeks] vector[N_teams] team_strength;
  
  // Initialize league HFA
  league_hfa[1] = league_hfa_init;
  
  // League HFA AR(1) evolution across seasons
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  }
  
  // Team HFA = league HFA + sum-to-zero deviations (centered around league_hfa)
  for (s in 1 : N_seasons) {
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;
  }
  
  // Initialize team strengths (first week) - already sum-to-zero
  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;
  
  // Team strength evolution - maintains sum-to-zero because innovations are sum-to-zero
  {
    int season_carryover_idx = 1;
    for (w in 2 : N_weeks) {
      // Check if any game in this week is a first week of season
      int is_first_week = 0;
      for (g in 1 : N_games) {
        if (week_idx[g] == w && fw_season_idx[g] == 1) {
          is_first_week = 1;
          break;
        }
      }
      
      if (is_first_week == 1) {
        // Season carryover: φ_sc * previous_week + sum-to-zero innovation
        team_strength[w] = phi_season_carryover * team_strength[w - 1]
                           + to_vector(z_season_carryover[season_carryover_idx]) * sigma_season_carryover;
        season_carryover_idx += 1;
      } else {
        // Weekly AR(1) evolution within season: φ_w * previous_week + sum-to-zero innovation
        team_strength[w] = phi_weekly * team_strength[w - 1]
                           + to_vector(z_weekly_innovation[w - 1]) * sigma_weekly_innovation;
      }
    }
  }
}
model {
  // Priors for league HFA AR(1) process
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 0.3);
  phi_league_hfa_raw ~ normal(1.5, 0.5); // logit(0.8) ≈ 1.4, favors persistence
  
  // Priors for team HFA
  sigma_team_hfa ~ normal(0, 1);
  // team_hfa_deviation has implicit sum-to-zero prior (Stan handles this)
  
  // Priors for team strength processes
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly_raw ~ normal(2.2, 0.5); // logit(0.9) ≈ 2.2, strong weekly persistence
  phi_season_carryover_raw ~ normal(0.4, 0.5); // logit(0.6) ≈ 0.4, moderate carryover
  
  // Observation model
  sigma_obs ~ normal(0, 5);
  
  // Non-centered parameterization priors
  league_hfa_innovation ~ std_normal();
  
  // Innovation terms have implicit sum-to-zero priors (Stan handles this)
  to_vector(team_strength_init) ~ std_normal();
  
  for (s in 1:(N_seasons-1)) {
    to_vector(team_hfa_deviation[s]) ~ std_normal();
    to_vector(z_season_carryover[s]) ~ std_normal();
  }
  
  for (w in 1:(N_weeks-1)) {
    // Full array, even though z_weekly_innovation is only [N_weeks-1]
    to_vector(z_weekly_innovation[w]) ~ std_normal();
  }
  
  // Likelihood
  for (g in 1:N_games) {
    real expected_result = team_strength[week_idx[g]][home_team[g]]
                           - team_strength[week_idx[g]][away_team[g]]
                           + hfa[g] * team_hfa[season_idx[g]][home_team[g]];
    result[g] ~ normal(expected_result, sigma_obs);
  }
}
generated quantities {
  // Snapshot at end of training window 
  vector[N_teams] team_strength_last = team_strength[week_idx[N_games]];
  real league_hfa_last = league_hfa[season_idx[N_games]];
  vector[N_teams] team_hfa_last = team_hfa[season_idx[N_games]];
}
