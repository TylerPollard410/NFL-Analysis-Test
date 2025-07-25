data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  vector[N_games] home_score;
  vector[N_games] away_score;
  vector[N_games] result; // margin of victory
}
parameters {
  sum_to_zero_vector[N_teams] srs;  // SRS from MOV model
  sum_to_zero_vector[N_teams] osrs; // Offense rating
  //sum_to_zero_vector[N_teams] dsrs; // Defense rating
  real<lower=0> sigma_srs;
  real<lower=0> sigma_pts;
}
transformed parameters{
  vector[N_teams] dsrs = srs - osrs;
}
model {
  // Classic SRS model
  srs ~ normal(0, 100);
  sigma_srs ~ normal(0, 10);
  result ~ normal(srs[home_id] - srs[away_id], sigma_srs);

  // Off/def model for scores
  osrs ~ normal(0, 100);
  //dsrs ~ normal(0, 100);
  sigma_pts ~ normal(0, 10);

  home_score ~ normal(osrs[home_id] - dsrs[away_id], sigma_pts);
  away_score ~ normal(osrs[away_id] - dsrs[home_id], sigma_pts);
}
generated quantities {
  vector[N_teams] srs2 = osrs + dsrs;
  // "SOS" and "SOS2" must be computed in R after fitting, since they require knowledge of schedule structure (opponent mapping).
}
