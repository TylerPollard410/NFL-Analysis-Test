data {
  int<lower=1> N_games;                     // Number of games
  int<lower=1> N_teams;                     // Number of teams
  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  vector[N_games] result;                   // Margin of victory: home_score - away_score
}
// parameters {
//   vector[N_teams] srs_raw;
//   real<lower=0> sigma;
// }
// transformed parameters {
//   vector[N_teams] srs = srs_raw - mean(srs_raw); // Sum-to-zero constraint
// }
parameters {
  sum_to_zero_vector[N_teams] srs;
  real<lower=0> sigma;
}
model {
  // Priors
  srs ~ normal(0, 100);
  sigma ~ normal(0, 10);
  // Likelihood
  result ~ normal(srs[home_id] - srs[away_id], sigma);
}
