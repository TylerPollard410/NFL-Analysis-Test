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

  // for sum-to-zero transformation
  real sum_to_zero_scale = sqrt(N_teams * inv(N_teams - 1)); 
}

parameters {
  // Match mod5.stan
  real league_hfa_init;
  vector[N_seasons - 1] z_league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  array[N_seasons] sum_to_zero_vector[N_teams] z_team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  sum_to_zero_vector[N_teams] z_team_strength_init;
  real<lower=0> sigma_team_strength_init;

  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_team_strength_innovation;
  real<lower=0> sigma_weekly_team_strength_innovation;
  real<lower=0, upper=1> phi_weekly_team_strength_innovation;

  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_team_strength_innovation;
  real<lower=0> sigma_season_team_strength_innovation;
  real<lower=0, upper=1> phi_season_team_strength_innovation;

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
                    + z_league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(z_team_hfa_deviation[s]) * sigma_team_hfa;

  team_strength[1] = to_vector(z_team_strength_init) * sigma_team_strength_init;
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w];
      team_strength[w] = phi_season_team_strength_innovation * team_strength[w - 1]
                         + to_vector(z_season_team_strength_innovation[s - 1]) * sigma_season_team_strength_innovation;
    } else {
      team_strength[w] = phi_weekly_team_strength_innovation * team_strength[w - 1]
                         + to_vector(z_weekly_team_strength_innovation[w - 1]) * sigma_weekly_team_strength_innovation;
    }
  }
}

generated quantities {
  // Last fitted week and season
  int last_w = max(week_idx);
  int last_s = week_to_season[last_w];

  // Filtered (=smoothed at T) states
  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];

  // Store sampled latent states for each future week
  array[N_future_weeks] vector[N_teams] predicted_team_strength;
  array[N_future_weeks] vector[N_teams] predicted_team_hfa;
  vector[N_future_weeks] predicted_league_hfa;

  {
    vector[N_teams] ts_cur = filtered_team_strength_last;
    real league_hfa_cur = filtered_league_hfa_last;
    vector[N_teams] team_hfa_cur = filtered_team_hfa_last;
    int season_cur = last_s;

    for (fw in 1:N_future_weeks) {
      int is_first = future_is_first_week[fw];
      int season_target = future_week_to_season[fw];
      vector[N_teams - 1] z_raw;
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      vector[N_teams] z0 = sum_to_zero_constrain(z_raw);

      if (is_first == 1) {
        ts_cur = phi_season_team_strength_innovation * ts_cur + z0 * sigma_season_team_strength_innovation;

        if (season_target <= N_seasons) {
          league_hfa_cur = league_hfa[season_target];
          team_hfa_cur = team_hfa[season_target];
        } else {
          league_hfa_cur = phi_league_hfa * league_hfa_cur
                            + normal_rng(0, 1) * sigma_league_hfa_innovation;
          {
            vector[N_teams - 1] zh_raw;
            for (t in 1:(N_teams - 1)) zh_raw[t] = normal_rng(0, 1);
            vector[N_teams] zh0 = sum_to_zero_constrain(zh_raw);
            team_hfa_cur = rep_vector(league_hfa_cur, N_teams) + zh0 * sigma_team_hfa;
          }
        }
        season_cur = season_target;
      } else {
        ts_cur = phi_weekly_team_strength_innovation * ts_cur + z0 * sigma_weekly_team_strength_innovation;

        if (season_target != season_cur && season_target <= N_seasons) {
          // Defensive guard in case season index advances without first-week flag
          league_hfa_cur = league_hfa[season_target];
          team_hfa_cur = team_hfa[season_target];
          season_cur = season_target;
        }
      }

      predicted_team_strength[fw] = ts_cur;
      predicted_team_hfa[fw] = team_hfa_cur;
      predicted_league_hfa[fw] = league_hfa_cur;
    }
  }

  // OOS predictions
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;
  for (k in 1:N_oos) {
    int s = oos_season_idx[k];
    int w = oos_week_idx[k];
    int i = oos_home_team[k];
    int j = oos_away_team[k];
    
    // Build state at target week (supports multi-step ahead by iterating from N_weeks)
    vector[N_teams] ts_target;
    real hfa_home;

    if (w <= N_weeks) {
      ts_target = team_strength[w];
      hfa_home = (s <= N_seasons) ? team_hfa[s][i] : team_hfa[last_s][i];
    } else if (N_future_weeks > 0) {
      int ahead = w - N_weeks;
      int idx = ahead <= N_future_weeks ? ahead : N_future_weeks;
      ts_target = predicted_team_strength[idx];
      hfa_home = predicted_team_hfa[idx][i];
    } else {
      // Fallback: one-step conditional mean when no future simulation metadata provided
      if (is_last_week[last_w] == 1) {
        ts_target = phi_season_team_strength_innovation * filtered_team_strength_last;
      } else {
        ts_target = phi_weekly_team_strength_innovation * filtered_team_strength_last;
      }
      hfa_home = filtered_team_hfa_last[i];
    }

    {
      real mu = ts_target[i] - ts_target[j] + oos_hfa[k] * hfa_home;
      mu_pred[k] = mu;
      y_pred[k] = normal_rng(mu, sigma_obs);
    }
  }
}
