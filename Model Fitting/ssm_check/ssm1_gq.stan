
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
 
parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_z;
  real              league_hfa_init;
  real<lower=0, upper=1> beta_hfa;
  real<lower=0>          sigma_hfa;

  // Team HFA deviations about league mean (sum-to-zero per season)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_z;
  real<lower=0>                                sigma_team_hfa;

  // Team strength state-space innovations (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] z_start; // season-start shocks
  array[N_weeks]   sum_to_zero_vector[N_teams] z_w;     // weekly shocks

  real<lower=0, upper=1> beta_w;        // within-season AR(1)
  real<lower=0>          sigma_w;       // within-season shock sd

  real<lower=0, upper=1> beta_s;        // between-season carryover
  real<lower=0>          sigma_s;       // between-season start shock sd

  real<lower=0> sigma_y;                // observation noise
}
 
transformed parameters {
  // League HFA series
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init + 2 * league_hfa_z[1];  // diffuse start (sd ≈ 2)
  for (s in 2:N_seasons) {
    league_hfa[s] = beta_hfa * league_hfa[s - 1] + sigma_hfa * league_hfa_z[s];
  }

  // Team HFA = league mean + scaled, sum-to-zero deviations
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1:N_seasons) {
    team_hfa[s] = league_hfa[s] + sigma_team_hfa * team_hfa_z[s];
  }

  // Team strength states (zero-sum preserved by construction)
  array[N_weeks] vector[N_teams] team_strength_raw
    = rep_array(rep_vector(0.0, N_teams), N_weeks);
  array[N_weeks] vector[N_teams] team_strength;

  // Build each season
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Season start: zero-sum carries over, e_start is zero-sum
    if (s == 1) {
      team_strength_raw[fw] = 5 * z_start[s];
    } else {
      int lw_prev = last_week_of_season[s - 1];
      vector[N_teams] prev = team_strength_raw[lw_prev];  // zero-sum already
      team_strength_raw[fw] = beta_s * prev + sigma_s * z_start[s];
    }

    // Within-season AR(1): e_w[w] is zero-sum, so property propagates
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        team_strength_raw[w] = beta_w * team_strength_raw[w - 1]
                               + sigma_w * z_w[w];
      }
    }
  }

  // No weekly mean-subtraction needed; already zero-sum
  for (w in 1:N_weeks)
    team_strength[w] = team_strength_raw[w];
}
 
generated quantities {
  vector[N_oos] team_strength_home_pred;
  vector[N_oos] team_strength_away_pred;
  vector[N_oos] team_hfa_pred;
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  
  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    
    team_strength_home_pred[k] = team_strength[w][i];
    team_strength_away_pred[k] = team_strength[w][j];
    team_hfa_pred[k] = team_hfa[s][i];
    
    // mu_pred[k] = (team_strength[w][i] - team_strength[w][j]) + team_hfa[s][i] * hfa[k];
    mu_pred[k] = (team_strength_home_pred[k] - team_strength_away_pred[k]) + 
                  team_hfa_pred[k] * hfa[k];
    y_pred[k] = normal_rng(mu_pred[k], sigma_y);
  }
}

