
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

  array[N_seasons] int<lower=1, upper=N_weeks> fw_season;
  array[N_seasons] int<lower=1, upper=N_weeks> lw_season;

  array[N_games] int<lower=0, upper=1> hfa;
  vector[N_obs] result;

  // Optional: games to predict without contributing to likelihood
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}
 
transformed data {
  real sum_to_zero_scale = sqrt(N_teams * inv(N_teams - 1.0));
}
 
parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_raw;
  real<lower=0, upper=1> beta_league_hfa;
  real<lower=0>          sigma_league_hfa;
  
  // Team HFA deviations about league mean (sum-to-zero per season)
  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa;
  real<lower=0>                                sigma_team_hfa;
  
  // Team strength state-space innovations (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] z_s; // season-start shocks
  real<lower=0, upper=1> beta_s;                    // between-season carryover
  real<lower=0>          sigma_s;                   // between-season start shock sd
  
  array[N_weeks]   sum_to_zero_vector[N_teams] z_w; // weekly shocks
  real<lower=0, upper=1> beta_w;                    // within-season AR(1)
  real<lower=0>          sigma_w;                   // within-season shock sd
  
  real<lower=0> sigma_y;                            // observation noise
}
 
transformed parameters {
  // League HFA series - optimized AR(1) construction
  vector[N_seasons] league_hfa = sigma_league_hfa * league_hfa_raw;
  for (s in 2:N_seasons) {
    league_hfa[s] += beta_league_hfa * league_hfa[s - 1];
  }
  
  // Team HFA = league mean + scaled, sum-to-zero deviations
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1:N_seasons) {
    team_hfa[s] = league_hfa[s] + sigma_team_hfa * z_team_hfa[s];
  }
  
  // Team strength states - work directly with final array
  array[N_weeks] vector[N_teams] team_strength = rep_array(rep_vector(0.0, N_teams), N_weeks);
  
  // Build each season
  for (s in 1:N_seasons) {
    int fw = fw_season[s];
    int lw = lw_season[s];
    
    // Season start: zero-sum carries over, z_s is zero-sum
    if (s == 1) {
      team_strength[fw] = 5 * z_s[s];
    } else {
      team_strength[fw] = beta_s * team_strength[lw_season[s - 1]] + sigma_s * z_s[s];
    }
    
    // Within-season AR(1): z_w[w] is zero-sum, so property propagates
    for (w in (fw + 1):lw) {
      team_strength[w] = beta_w * team_strength[w - 1] + sigma_w * z_w[w];
    }
  }
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
    
    mu_pred[k] = (team_strength_home_pred[k] - team_strength_away_pred[k]) + 
                  team_hfa_pred[k] * hfa[k];
    y_pred[k] = normal_rng(mu_pred[k], sigma_y);
  }
}

