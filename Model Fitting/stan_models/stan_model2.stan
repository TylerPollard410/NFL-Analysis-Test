data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  int<lower=1> N_weeks;
  int<lower=1> N_seasons;
  array[N_games] int<lower=1, upper=N_weeks> game_week;
  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  vector[N_games] result;
  array[N_games] int<lower=1, upper=N_seasons> game_season;
  array[N_weeks] int<lower=1, upper=N_seasons> week_season;
  array[N_seasons] int<lower=1, upper=N_weeks> season_start_week;
  array[N_seasons] int<lower=1, upper=N_weeks> season_end_week;
  array[N_games] int<lower=0, upper=1> hfa; // Indicator: 1 if true home, 0 if neutral
}

transformed data {
  // Boolean indicator for new season at each week (1 = new season, 0 otherwise)
  array[N_weeks] int is_new_season;
  for (w in 1:N_weeks)
    is_new_season[w] = 0;
  for (s in 1:N_seasons)
    is_new_season[season_start_week[s]] = 1;
}

parameters {
  // Non-centered AR(1) for team strengths (N-1 teams for sum-to-zero, all weeks)
  matrix[N_weeks, N_teams-1] z_team_strength; // Innovations (standard normal)

  // AR(1) parameters
  real<lower=1e-6> tau_week;
  real<lower=1e-6> tau_season;
  real<lower=-0.95, upper=0.95> rho_week;
  real<lower=-0.95, upper=0.95> rho_season;

  // Initial team strengths (N-1 teams) for first modeled week
  vector[N_teams-1] mu_strength_init;

  // League HFA and team-specific HFA (non-centered)
  vector[N_seasons] hfa_league;
  matrix[N_teams-1, N_seasons] z_hfa_team; // standard normal
  real<lower=1e-6> tau_hfa;

  // Observation noise
  real<lower=1e-6> sigma;
}

transformed parameters {
  // Team strengths [week, team] with sum-to-zero constraint (N teams, all weeks)
  matrix[N_weeks, N_teams] strength_team;

  // 1. Initialize first week for N-1 teams, enforce sum-to-zero
  strength_team[season_start_week[1], 1:(N_teams-1)] = to_row_vector(mu_strength_init);
  strength_team[season_start_week[1], N_teams] = -sum(mu_strength_init);

  // 2. Loop over all weeks (AR(1)), for N-1 teams, reconstruct Nth team each week
  for (w in (season_start_week[1] + 1):N_weeks) {
    int s = week_season[w]; // season index for this week
    if (is_new_season[w]) {
      int prev_season = s - 1;
      int last_week_prev = season_end_week[prev_season];
      strength_team[w, 1:(N_teams-1)] =
        to_row_vector(rho_season * to_vector(strength_team[last_week_prev, 1:(N_teams-1)])) +
        to_row_vector(tau_season * to_vector(z_team_strength[w, 1:(N_teams-1)]));
    } else {
      strength_team[w, 1:(N_teams-1)] =
        to_row_vector(rho_week * to_vector(strength_team[w-1, 1:(N_teams-1)])) +
        to_row_vector(tau_week * to_vector(z_team_strength[w, 1:(N_teams-1)]));
    }
    // Enforce sum-to-zero for each week
    strength_team[w, N_teams] = -sum(strength_team[w, 1:(N_teams-1)]);
  }

  // Team-specific HFA, non-centered, sum-to-zero per season
  matrix[N_teams, N_seasons] hfa_team;
  for (s in 1:N_seasons) {
    hfa_team[1:(N_teams-1), s] = hfa_league[s] + tau_hfa * z_hfa_team[, s];
    hfa_team[N_teams, s] = hfa_league[s] - sum(hfa_team[1:(N_teams-1), s]);
  }
  
  // Expected value for each game
  vector[N_games] mu;
  for (i in 1:N_games) {
    mu[i] = strength_team[game_week[i], home_id[i]]
          - strength_team[game_week[i], away_id[i]]
          + hfa_team[home_id[i], game_season[i]] * hfa[i];
  }
}

model {
  // Priors for AR(1) and noise parameters
  tau_week ~ normal(0, 2);    
  tau_season ~ normal(0, 2);
  rho_week ~ normal(0, 0.4);  
  rho_season ~ normal(0, 0.4);
  sigma ~ normal(0, 7);       
  tau_hfa ~ normal(0, 1);     

  // Priors for initial strengths (centered at 0)
  mu_strength_init ~ normal(0, 7);

  // Priors for HFA
  hfa_league ~ normal(0, 3);
  to_vector(z_hfa_team) ~ normal(0, 1);

  // Non-centered AR(1) innovations for team strengths
  to_vector(z_team_strength) ~ normal(0, 1);

  // Likelihood
  result ~ normal(mu, sigma);
}

generated quantities {
  vector[N_games] fitted = mu;
  vector[N_games] y_rep;
  for (i in 1:N_games) {
    y_rep[i] = normal_rng(fitted[i], sigma);
  }
}
