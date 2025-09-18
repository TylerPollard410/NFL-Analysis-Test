// GQ program for global-week model (mod4.stan)
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

  // OOS games to score
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_teams> oos_home_team;
  array[N_oos] int<lower=1, upper=N_teams> oos_away_team;
  array[N_oos] int<lower=1, upper=N_seasons> oos_season_idx;
  array[N_oos] int<lower=1, upper=N_weeks> oos_week_idx;
  array[N_oos] int<lower=0, upper=1> oos_hfa;
}

parameters {
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
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
  array[N_weeks] vector[N_teams] team_strength;
  array[N_weeks] int week_to_season;
  array[N_weeks] int is_first_week;
  array[N_weeks] int is_last_week;

  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s-1]
                    + league_hfa_innovation[s-1] * sigma_league_hfa_innovation;
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;

  for (w in 1:N_weeks) {
    week_to_season[w] = 1;
    is_first_week[w] = 0;
    is_last_week[w] = 0;
  }
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    if (s > week_to_season[w]) week_to_season[w] = s;
    if (fw_season_idx[g] == 1) is_first_week[w] = 1;
    if (lw_season_idx[g] == 1) is_last_week[w] = 1;
  }
  is_first_week[1] = 1;

  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_strength[w] = phi_season_carryover * team_strength[w-1]
                         + to_vector(z_season_carryover[s - 1]) * sigma_season_carryover;
    } else {
      team_strength[w] = phi_weekly * team_strength[w-1]
                         + to_vector(z_weekly_innovation[w-1]) * sigma_weekly_innovation;
    }
  }
}

generated quantities {
  // Snapshot and forecasts
  int last_w = 1;
  for (g in 1:N_games) if (week_idx[g] > last_w) last_w = week_idx[g];
  int last_s = week_to_season[last_w];

  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];

  vector[N_teams] predicted_team_strength_next_mean;
  vector[N_teams] predicted_team_strength_next_draw;
  {
    int next_is_first = is_last_week[last_w];
    if (next_is_first == 1)
      predicted_team_strength_next_mean = phi_season_carryover * team_strength[last_w];
    else
      predicted_team_strength_next_mean = phi_weekly * team_strength[last_w];
    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
    predicted_team_strength_next_draw = predicted_team_strength_next_mean
                                       + z0 * (next_is_first == 1 ? sigma_season_carryover
                                                                : sigma_weekly_innovation);
  }

  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  vector[N_oos] y_pred_one_step;
  for (k in 1:N_oos) {
    int s = oos_season_idx[k];
    int w = oos_week_idx[k];
    int i = oos_home_team[k];
    int j = oos_away_team[k];

    // If inside grid, use smoothed state
    if (w >= 1 && w <= N_weeks) {
      real mu = team_strength[w][i] - team_strength[w][j] + oos_hfa[k] * team_hfa[s][i];
      mu_pred[k] = mu;
      y_pred[k] = normal_rng(mu, sigma_obs);
    } else {
      mu_pred[k] = 0; // placeholder if outside grid; see y_pred_one_step
      y_pred[k] = 0;
    }

    // One-step ahead relative to w-1 (or carry from last of previous season)
    vector[N_teams] ts_next;
    if (w == 1 && s > 1) {
      // season rollover from last week of previous season
      int w_prev = 1;
      for (g in 1:N_games) if (season_idx[g] == s - 1 && week_idx[g] > w_prev) w_prev = week_idx[g];
      vector[N_teams - 1] z_raw;
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_season_carryover * team_strength[w_prev]
                + z0 * sigma_season_carryover;
    } else if (w > 1 && w - 1 >= 1 && w - 1 <= N_weeks) {
      vector[N_teams - 1] z_raw;
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_weekly * team_strength[w - 1]
                + z0 * sigma_weekly_innovation;
    } else {
      ts_next = team_strength[min(max(w, 1), N_weeks)];
    }
    y_pred_one_step[k] = normal_rng(ts_next[i] - ts_next[j] + oos_hfa[k] * team_hfa[s][i], sigma_obs);
  }
}
