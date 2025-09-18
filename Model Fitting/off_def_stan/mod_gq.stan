// Generated-quantities for mod5.stan (global-week model)
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

  // Out-of-sample games to predict
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_teams> oos_home_team;
  array[N_oos] int<lower=1, upper=N_teams> oos_away_team;
  // Allow OOS season index to reference future seasons beyond fitted N_seasons
  array[N_oos] int<lower=1> oos_season_idx;
  // Allow OOS week index to be beyond the fitted grid for one-step-ahead use
  array[N_oos] int<lower=1> oos_week_idx;
  array[N_oos] int<lower=0, upper=1> oos_hfa;

  // Optional future-week metadata to enable multi-step forecasts beyond N_weeks.
  // These correspond to global weeks N_weeks+1, N_weeks+2, ..., N_weeks+N_future_weeks
  // and must be provided in that contiguous order for O(1) access.
  int<lower=0> N_future_weeks;
  array[N_future_weeks] int<lower=0, upper=1> future_is_first_week;
  // May reference seasons beyond fitted N_seasons during backtests
  array[N_future_weeks] int<lower=1> future_week_to_season;
}

transformed data {
  // Map global weeks to seasons and mark season boundaries
  array[N_weeks] int week_to_season = rep_array(1, N_weeks); // season index for each global week
  array[N_weeks] int is_first_week = rep_array(0, N_weeks);  // 1 if first week of its season
  array[N_weeks] int is_last_week = rep_array(0, N_weeks);   // 1 if last week of its season
  
  // Aggregate game-level boundary indicators to global-week flags
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    if (s > week_to_season[w]) week_to_season[w] = s;
    if (fw_season_idx[g] == 1) is_first_week[w] = 1;
    if (lw_season_idx[g] == 1) is_last_week[w] = 1;
  }
  is_first_week[1] = 1; // Ensure first week is marked as first
}

parameters {
  // Match mod5.stan
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

  // Rebuild HFA and states
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;

  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_strength[w] = phi_season_carryover * team_strength[w - 1]
                         + to_vector(z_season_carryover[s - 1]) * sigma_season_carryover;
    } else {
      team_strength[w] = phi_weekly * team_strength[w - 1]
                         + to_vector(z_weekly_innovation[w - 1]) * sigma_weekly_innovation;
    }
  }
}

generated quantities {
  // Last fitted week and season
  int last_w = 1;
  for (g in 1:N_games) if (week_idx[g] > last_w) last_w = week_idx[g];
  int last_s = week_to_season[last_w];

  // Filtered (=smoothed at T) states
  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];

  // Next-week state mean and draw
  vector[N_teams] predicted_team_strength_next_mean;
  vector[N_teams] predicted_team_strength_next_draw;
  real predicted_league_hfa_next_mean;
  //real predicted_league_hfa_next_draw;
  //vector[N_teams] predicted_team_hfa_next_mean;
  //vector[N_teams] predicted_team_hfa_next_draw;
  {
    int next_is_first = is_last_week[last_w];
    if (next_is_first == 1) {
      predicted_team_strength_next_mean = phi_season_carryover * team_strength[last_w];
      predicted_league_hfa_next_mean = phi_league_hfa * league_hfa[last_s];
    } else {
      predicted_team_strength_next_mean = phi_weekly * team_strength[last_w];
      predicted_league_hfa_next_mean = league_hfa[last_s];
    }

    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
    predicted_team_strength_next_draw = predicted_team_strength_next_mean
                                       + z0 * (next_is_first == 1 ? sigma_season_carryover
                                                                : sigma_weekly_innovation);
  }

  // OOS predictions
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  vector[N_oos] y_pred_one_step;
  for (k in 1:N_oos) {
    int s = oos_season_idx[k];
    int w = oos_week_idx[k];
    int i = oos_home_team[k];
    int j = oos_away_team[k];
    
    // Build state at target week (supports multi-step ahead by iterating from N_weeks)
    vector[N_teams] ts_target;
    // Track HFA state across future seasons if needed
    real league_hfa_cur = league_hfa[last_s];
    vector[N_teams] team_hfa_cur = team_hfa[last_s];
    if (w <= N_weeks) {
      ts_target = team_strength[w];
    } else {
      int steps = w - N_weeks;
      vector[N_teams] ts_cur = team_strength[N_weeks];
      int s_prev = last_s;
      for (tstep in 1:steps) {
        int is_first = (tstep <= N_future_weeks) ? future_is_first_week[tstep] : 0;
        int s_future = (tstep <= N_future_weeks) ? future_week_to_season[tstep] : s_prev;
        vector[N_teams - 1] z_raw;
        for (tt in 1:(N_teams - 1)) z_raw[tt] = normal_rng(0, 1);
        vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
        if (is_first == 1) {
          ts_cur = phi_season_carryover * ts_cur + z0 * sigma_season_carryover;
          // Update HFA state at season boundary
          if (s_future <= N_seasons) {
            league_hfa_cur = league_hfa[s_future];
            team_hfa_cur   = team_hfa[s_future];
          } else {
            league_hfa_cur = phi_league_hfa * league_hfa_cur
                              + normal_rng(0, 1) * sigma_league_hfa_innovation;
            vector[N_teams - 1] zh_raw;
            for (tt in 1:(N_teams - 1)) zh_raw[tt] = normal_rng(0, 1);
            vector[N_teams] zh0 = sum_to_zero_constrain(zh_raw);
            team_hfa_cur = rep_vector(league_hfa_cur, N_teams) + zh0 * sigma_team_hfa;
          }
          s_prev = s_future;
        } else {
          ts_cur = phi_weekly * ts_cur + z0 * sigma_weekly_innovation;
        }
      }
      ts_target = ts_cur;
    }

    // Predictions using state at target week
    {
      real hfa_home = (w <= N_weeks && s <= N_seasons) ? team_hfa[s][i]
                                                       : team_hfa_cur[i];
      real mu = ts_target[i] - ts_target[j] + oos_hfa[k] * hfa_home;
      mu_pred[k] = mu;
      y_pred[k] = normal_rng(mu, sigma_obs);
      y_pred_one_step[k] = y_pred[k];
    }
  }
}
