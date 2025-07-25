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
parameters {
  array[N_weeks] sum_to_zero_vector[N_teams] strength_team;   // [week][team], sum-to-zero per week
  matrix[N_teams, N_seasons] hfa_team;                        // Team-specific HFA per season
  vector[N_seasons] hfa_league;                               // League-wide HFA per season
  real<lower=1e-6> tau_hfa;
  real<lower=1e-6> tau_week;
  real<lower=1e-6> tau_season;
  real<lower=0> sigma;
  real<lower=-1, upper=1> rho_week;
  real<lower=-1, upper=1> rho_season;
}
transformed parameters {
  vector[N_games] mu;
  for (i in 1:N_games) {
    mu[i] = strength_team[game_week[i], home_id[i]]
          - strength_team[game_week[i], away_id[i]]
          + hfa_team[home_id[i], game_season[i]] * hfa[i];
  }
}
model {
  // Priors for noise and evolution parameters
  sigma ~ normal(14, 3);
  tau_week ~ normal(0, 5);
  tau_season ~ normal(0, 5);
  rho_week ~ normal(0, 0.5);
  rho_season ~ normal(0, 0.5);
  hfa_league ~ normal(0, 5);
  tau_hfa ~ normal(0, 0.5);

  // Prior for team HFA: centered on season league HFA, partial pooling
  for (s in 1:N_seasons)
    hfa_team[, s] ~ normal(hfa_league[s], tau_hfa);

  // Initial prior: team strengths at first week of first season (all teams, sum to zero)
  strength_team[season_start_week[1]] ~ normal(0, 14);

  // Team strength evolution: season transitions (carryover from last week of previous season)
  for (s in 2:N_seasons) {
    int w_start = season_start_week[s];
    int w_prev_end = season_end_week[s-1];
    strength_team[w_start] ~ normal(rho_season * strength_team[w_prev_end], tau_season);
  }

  // Team strength evolution: AR(1) within each season
  for (s in 1:N_seasons) {
    int w_start = season_start_week[s];
    int w_end = season_end_week[s];
    for (w in (w_start + 1):w_end) {
      strength_team[w] ~ normal(rho_week * strength_team[w-1], tau_week);
    }
  }

  // Likelihood for observed games
  result ~ normal(mu, sigma);
}
generated quantities {
  vector[N_games] fitted = mu;
  vector[N_games] y_rep;
  for (i in 1:N_games) {
    y_rep[i] = normal_rng(fitted[i], sigma);
  }
}
