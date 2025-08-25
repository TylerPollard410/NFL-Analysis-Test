
data {
  int<lower=1> N_games;
  int<lower=1> N_obs;

  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks>  week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa;

  // Optional: games to predict without contributing to likelihood
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}
 
generated quantities {
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  
  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_pred[g] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[g];
    y_pred[g] = normal_rng(mu_pred[g], sigma_y);
  }
}

