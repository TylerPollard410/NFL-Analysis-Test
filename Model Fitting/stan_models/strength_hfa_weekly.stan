data {
  int<lower=1> N_games;        // number of games
  int<lower=1> N_teams;        // usually 32
  int<lower=1> N_weeks;        // total unique weeks in all seasons
  array[N_games] int<lower=1, upper=N_teams> home_id;   // home team indices
  array[N_games] int<lower=1, upper=N_teams> away_id;   // away team indices
  array[N_games] int<lower=1, upper=N_weeks> week_idx;  // week index for each game
  array[N_games] int<lower=0, upper=1> hfa;             // 1 if home game, 0 if neutral
  vector[N_games] result;        // home margin (home_score - away_score)
}
parameters {
  matrix[N_teams, N_weeks] raw_team_strength;      // latent ability (before centering)
  sum_to_zero_vector[N_teams] hfa_team;            // team-specific HFA (static)
  real<lower=0> tau_team;      // random walk SD for team strength
  real<lower=0> sigma;         // residual SD
}
transformed parameters {
  matrix[N_teams, N_weeks] team_strength;    // sum-to-zero for each week

  for (w in 1:N_weeks) {
    team_strength[, w] = raw_team_strength[, w] - mean(raw_team_strength[, w]);
  }
}
model {
  // Initial prior for week 1
  for (k in 1:N_teams)
    raw_team_strength[k, 1] ~ normal(0, 5);

  // Random walk for team strength over weeks
  for (w in 2:N_weeks)
    for (k in 1:N_teams)
      raw_team_strength[k, w] ~ normal(team_strength[k, w-1], tau_team);

  // Team HFA prior (mean zero by sum_to_zero_vector)
  hfa_team ~ normal(0, 3);

  // Priors for SDs
  tau_team ~ normal(0, 2);
  sigma ~ normal(0, 10);

  // Likelihood: one row per game
  for (i in 1:N_games) {
    int hw = home_id[i];
    int aw = away_id[i];
    int w  = week_idx[i];
    result[i] ~ normal(
      team_strength[hw, w] - team_strength[aw, w] + hfa[i] * hfa_team[hw],
      sigma
    );
  }
}
generated quantities {
  // Optionally output the team_strength and HFA for later plotting or forecasting
  matrix[N_teams, N_weeks] team_strength_gq = team_strength;
  vector[N_teams] hfa_team_gq = hfa_team;
}
