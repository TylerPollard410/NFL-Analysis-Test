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
  // League HFA AR(1) across seasons
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innov_raw;
  real<lower=0> sigma_league_hfa_innov;
  real<lower=0, upper=1> phi_league_hfa;

  // Team-specific HFA deviation by season (raw; centered in TP)
  array[N_seasons] vector[N_teams] team_hfa_dev_raw;
  real<lower=0> sigma_team_hfa;

  // Initial team strengths (raw; centered in TP)
  vector[N_teams] team_strength_init_raw;
  real<lower=0> sigma_strength_init;

  // Within-season weekly innovations (raw; centered in TP)
  array[N_seasons] array[N_weeks - 1] vector[N_teams] z_weekly_raw;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;

  // Season carryover innovations between seasons (raw; centered in TP)
  array[N_seasons - 1] vector[N_teams] z_season_raw;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;

  // Observation model parameters
  real<lower=0> sigma_obs;
}

transformed parameters {
  
  // League HFA evolution (AR1 process)
  vector[N_seasons] league_hfa;
  
  // Team HFA (league HFA + sum-to-zero deviations, so centered around league_hfa)
  array[N_seasons] vector[N_teams] team_hfa;
  
  // Team strengths by season and week (sum-to-zero at each time point)
  array[N_seasons] array[N_weeks] vector[N_teams] team_strength;
  
  // Track last observed week per season (derived from data)
  array[N_seasons] int last_week_in_season;
  
  // Initialize league HFA
  league_hfa[1] = league_hfa_init;
  
  // League HFA AR(1) evolution across seasons
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innov_raw[s - 1] * sigma_league_hfa_innov;
  }
  
  // Team HFA = league HFA + centered deviations
  for (s in 1 : N_seasons) {
    vector[N_teams] dev_c = team_hfa_dev_raw[s] - rep_vector(mean(team_hfa_dev_raw[s]), N_teams);
    team_hfa[s] = rep_vector(league_hfa[s], N_teams) + dev_c * sigma_team_hfa;
  }
  
  // Derive last observed week per season from data
  for (s in 1 : N_seasons) last_week_in_season[s] = 1;
  for (g in 1 : N_games) {
    if (lw_season_idx[g] == 1) {
      if (week_idx[g] > last_week_in_season[season_idx[g]])
        last_week_in_season[season_idx[g]] = week_idx[g];
    }
  }
  
  // Initialize team strengths for season 1, week 1 (centered)
  team_strength[1, 1] = (team_strength_init_raw - rep_vector(mean(team_strength_init_raw), N_teams)) * sigma_strength_init;
  
  // Within-season evolution for season 1
  for (w in 2 : N_weeks) {
    vector[N_teams] zc = z_weekly_raw[1, w - 1] - rep_vector(mean(z_weekly_raw[1, w - 1]), N_teams);
    team_strength[1, w] = phi_weekly * team_strength[1, w - 1]
                          + zc * sigma_weekly_innovation;
  }
  
  // Seasons 2..N: carry over from previous season then evolve within-season
  for (s in 2 : N_seasons) {
    // Carryover from last week of previous season into week 1 of current season
    vector[N_teams] zc0 = z_season_raw[s - 1] - rep_vector(mean(z_season_raw[s - 1]), N_teams);
    team_strength[s, 1] = phi_season_carryover * team_strength[s - 1, last_week_in_season[s - 1]]
                          + zc0 * sigma_season_carryover;
    // Within-season weekly evolution
    for (w in 2 : N_weeks) {
      vector[N_teams] zc = z_weekly_raw[s, w - 1] - rep_vector(mean(z_weekly_raw[s, w - 1]), N_teams);
      team_strength[s, w] = phi_weekly * team_strength[s, w - 1]
                            + zc * sigma_weekly_innovation;
    }
  }
}

model {
  // Priors for league HFA AR(1) process
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innov ~ normal(0, 0.3);
  phi_league_hfa ~ beta(8, 2); // favors persistence near 0.8
  
  // Priors for team HFA
  sigma_team_hfa ~ normal(0, 1);
  // team_hfa_deviation has implicit sum-to-zero prior (Stan handles this)
  
  // Priors for team strength processes
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1);           // strong weekly persistence ≈ 0.9
  phi_season_carryover ~ beta(6, 4); // moderate season carryover ≈ 0.6
  
  // Observation model
  sigma_obs ~ normal(0, 5);
  
  // Non-centered parameterization priors
  league_hfa_innov_raw ~ std_normal();
  for (s in 1:N_seasons) team_hfa_dev_raw[s] ~ std_normal();
  team_strength_init_raw ~ std_normal();
  for (s in 1:N_seasons) for (w in 1:(N_weeks - 1)) z_weekly_raw[s, w] ~ std_normal();
  for (s in 1:(N_seasons - 1)) z_season_raw[s] ~ std_normal();
  
  // Likelihood
  for (g in 1:N_games) {
    real expected_result = team_strength[season_idx[g], week_idx[g]][home_team[g]]
                           - team_strength[season_idx[g], week_idx[g]][away_team[g]]
                           + hfa[g] * team_hfa[season_idx[g]][home_team[g]];
    result[g] ~ normal(expected_result, sigma_obs);
  }
}

generated quantities {
  // Snapshot at end of training window 
  vector[N_teams] team_strength_last = team_strength[season_idx[N_games], week_idx[N_games]];
  real league_hfa_last = league_hfa[season_idx[N_games]];
  vector[N_teams] team_hfa_last = team_hfa[season_idx[N_games]];
}
