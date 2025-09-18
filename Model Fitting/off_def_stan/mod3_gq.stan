// Generated-quantities program for mod3.stan (sum_to_zero_vector version)
data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx;
  array[N_games] int<lower=1, upper=N_weeks> week_idx;

  array[N_games] int<lower=0, upper=1> fw_season_idx;
  array[N_games] int<lower=0, upper=1> lw_season_idx;
  array[N_games] int<lower=0, upper=1> hfa;

  // Out-of-sample games to forecast
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_teams> oos_home_team;
  array[N_oos] int<lower=1, upper=N_teams> oos_away_team;
  array[N_oos] int<lower=1, upper=N_seasons> oos_season_idx;
  array[N_oos] int<lower=1, upper=N_weeks> oos_week_idx;
  array[N_oos] int<lower=0, upper=1> oos_hfa;
}

parameters {
  // Must match mod3.stan
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  array[N_seasons] array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;

  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;

  real<lower=0> sigma_obs;
}

transformed parameters {
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;
  array[N_seasons] array[N_weeks] vector[N_teams] team_strength;
  array[N_seasons] int last_week_in_season;

  // League HFA AR(1)
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s-1]
                    + league_hfa_innovation[s-1] * sigma_league_hfa_innovation;

  // Team HFA
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;

  // Last observed week per season
  for (s in 1:N_seasons) last_week_in_season[s] = 1;
  for (g in 1:N_games)
    if (lw_season_idx[g] == 1 && week_idx[g] > last_week_in_season[season_idx[g]])
      last_week_in_season[season_idx[g]] = week_idx[g];

  // Rebuild latent states
  team_strength[1,1] = to_vector(team_strength_init) * sigma_strength_init;
  for (w in 2:N_weeks)
    team_strength[1,w] = phi_weekly * team_strength[1,w-1]
                         + to_vector(z_weekly_innovation[1,w-1]) * sigma_weekly_innovation;
  for (s in 2:N_seasons) {
    team_strength[s,1] = phi_season_carryover * team_strength[s-1, last_week_in_season[s-1]]
                         + to_vector(z_season_carryover[s-1]) * sigma_season_carryover;
    for (w in 2:N_weeks)
      team_strength[s,w] = phi_weekly * team_strength[s,w-1]
                           + to_vector(z_weekly_innovation[s,w-1]) * sigma_weekly_innovation;
  }
}

generated quantities {
  // Forecasts for OOS games
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  vector[N_oos] y_pred_one_step;

  // Latest season/week snapshot outputs (filtered at last time; equals smoothed)
  int s_last = N_seasons;
  int w_last = last_week_in_season[s_last];
  vector[N_teams] filtered_team_strength_last = team_strength[s_last, w_last];
  vector[N_teams] filtered_team_hfa_last = team_hfa[s_last];
  real filtered_league_hfa_last = league_hfa[s_last];

  // Predicted next-week state (mean and RNG draw) for last season
  vector[N_teams] predicted_team_strength_next_mean = phi_weekly * filtered_team_strength_last;
  vector[N_teams] predicted_team_strength_next_draw;
  {
    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
    predicted_team_strength_next_draw = predicted_team_strength_next_mean + z0 * sigma_weekly_innovation;
  }

  for (k in 1:N_oos) {
    int s = oos_season_idx[k];
    int w = oos_week_idx[k];
    int i = oos_home_team[k];
    int j = oos_away_team[k];
    int lw = last_week_in_season[s];

    // One-step ahead state for requested (s,w)
    vector[N_teams] ts_next;
    if (w == 1 && s > 1) {
      vector[N_teams - 1] z_raw;
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_season_carryover * team_strength[s - 1, last_week_in_season[s - 1]]
                + z0 * sigma_season_carryover;
    } else if (w > 1 && w <= N_weeks) {
      vector[N_teams - 1] z_raw;
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_weekly * team_strength[s, w - 1] + z0 * sigma_weekly_innovation;
    } else if (w == lw + 1) {
      vector[N_teams - 1] z_raw2;
      for (t in 1:(N_teams - 1)) z_raw2[t] = normal_rng(0, 1);
      vector[N_teams] z02 = sum_to_zero_vector_constrain(z_raw2);
      ts_next = phi_weekly * team_strength[s, lw] + z02 * sigma_weekly_innovation;
    } else {
      ts_next = team_strength[s, min(w, N_weeks)];
    }

    // Predictions using smoothed state if available; otherwise one-step state
    if (w >= 1 && w <= N_weeks) {
      real mu = team_strength[s, w][i] - team_strength[s, w][j] + oos_hfa[k] * team_hfa[s][i];
      mu_pred[k] = mu;
      y_pred[k] = normal_rng(mu, sigma_obs);
    } else {
      real mu = ts_next[i] - ts_next[j] + oos_hfa[k] * team_hfa[s][i];
      mu_pred[k] = mu;
      y_pred[k] = normal_rng(mu, sigma_obs);
    }
    {
      real mu1 = ts_next[i] - ts_next[j] + oos_hfa[k] * team_hfa[s][i];
      y_pred_one_step[k] = normal_rng(mu1, sigma_obs);
    }
  }
}
