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
  array[N_games] int<lower=0, upper=1> hfa; // 1 if true home, 0 if neutral
}

transformed data {
  array[N_weeks] int is_new_season;
  for (w in 1:N_weeks) is_new_season[w] = 0;
  for (s in 1:N_seasons) is_new_season[season_start_week[s]] = 1;
}

parameters {
  array[N_weeks] sum_to_zero_vector[N_teams] z_team_strength;
  sum_to_zero_vector[N_teams] mu_strength_init;
  real<lower=1e-6> tau_week;
  real<lower=1e-6> tau_season;
  vector[N_seasons] hfa_league;
  real<lower=1e-6> sigma;
}

transformed parameters {
  array[N_weeks] sum_to_zero_vector[N_teams] strength_team;

  strength_team[season_start_week[1]] = mu_strength_init;

  for (w in (season_start_week[1] + 1):N_weeks) {
    int s = week_season[w];
    if (is_new_season[w]) {
      int prev_season = s - 1;
      int last_week_prev = season_end_week[prev_season];
      strength_team[w] = strength_team[last_week_prev] + tau_season * z_team_strength[w];
    } else {
      strength_team[w] = strength_team[w-1] + tau_week * z_team_strength[w];
    }
  }

  vector[N_games] mu;
  for (i in 1:N_games) {
    mu[i] = strength_team[game_week[i]][home_id[i]]
          - strength_team[game_week[i]][away_id[i]]
          + hfa_league[game_season[i]] * hfa[i];
  }
}

model {
  tau_week ~ normal(0, 5);
  tau_season ~ normal(0, 5);
  sigma ~ normal(0, 14);
  mu_strength_init ~ normal(0, 14);
  hfa_league ~ normal(0, 5);

  for (w in 1:N_weeks)
    z_team_strength[w] ~ normal(0, 1);

  result ~ normal(mu, sigma);
}

generated quantities {
  vector[N_games] fitted = mu;
  vector[N_games] y_rep;
  for (i in 1:N_games)
    y_rep[i] = normal_rng(fitted[i], sigma);
}
