data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  // Indexing variables for historical data (used to rebuild latent states)
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx;
  array[N_games] int<lower=1, upper=N_weeks> week_idx;

  // Season boundary indicators
  array[N_games] int<lower=0, upper=1> fw_season_idx; // first week of season
  array[N_games] int<lower=0, upper=1> lw_season_idx; // last week of season
  array[N_games] int<lower=0, upper=1> hfa;           // home field indicator

  // Out-of-sample games to forecast (1-week-ahead or in-sample indices)
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_teams> oos_home_team;
  array[N_oos] int<lower=1, upper=N_teams> oos_away_team;
  array[N_oos] int<lower=1, upper=N_seasons> oos_season_idx;
  array[N_oos] int<lower=1, upper=N_weeks> oos_week_idx;
  array[N_oos] int<lower=0, upper=1> oos_hfa;
}

parameters {
  // AR(1) League HFA process
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real phi_league_hfa_raw;

  // Team HFA deviations from league HFA
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  // Initial team strengths (sum-to-zero at first week)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  // Weekly and season-carryover innovations (match training model dims)
  array[N_seasons] array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real phi_weekly_raw;

  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real phi_season_carryover_raw;

  // Observation noise
  real<lower=0> sigma_obs;
}

transformed parameters {
  // Transform persistence parameters
  real<lower=0, upper=1> phi_league_hfa = inv_logit(phi_league_hfa_raw);
  real<lower=0, upper=1> phi_weekly = inv_logit(phi_weekly_raw);
  real<lower=0, upper=1> phi_season_carryover = inv_logit(phi_season_carryover_raw);

  // League and team HFA
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;

  // Team strengths by season and week
  array[N_seasons] array[N_weeks] vector[N_teams] team_strength;
  
  // Track last observed week per season from data
  array[N_seasons] int last_week_in_season;

  // League HFA evolution
  league_hfa[1] = league_hfa_init;
  for (s in 2 : N_seasons) {
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;
  }
  for (s in 1 : N_seasons) {
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;
  }

  // Derive last observed week per season
  for (s in 1 : N_seasons) last_week_in_season[s] = 1;
  for (g in 1 : N_games) {
    if (lw_season_idx[g] == 1) {
      if (week_idx[g] > last_week_in_season[season_idx[g]])
        last_week_in_season[season_idx[g]] = week_idx[g];
    }
  }

  // Team strength initialization and evolution
  team_strength[1, 1] = to_vector(team_strength_init) * sigma_strength_init;
  for (w in 2 : N_weeks) {
    team_strength[1, w] = phi_weekly * team_strength[1, w - 1]
                          + to_vector(z_weekly_innovation[1, w - 1]) * sigma_weekly_innovation;
  }
  for (s in 2 : N_seasons) {
    team_strength[s, 1] = phi_season_carryover * team_strength[s - 1, last_week_in_season[s - 1]]
                          + to_vector(z_season_carryover[s - 1]) * sigma_season_carryover;
    for (w in 2 : N_weeks) {
      team_strength[s, w] = phi_weekly * team_strength[s, w - 1]
                            + to_vector(z_weekly_innovation[s, w - 1]) * sigma_weekly_innovation;
    }
  }
}

generated quantities {
  // Predicted means and outcomes for OOS games
  vector[N_oos] mu_pred;
  vector[N_oos] y_pred;             // using latent state at requested (s, w)
  vector[N_oos] y_pred_one_step;    // explicit 1-step-ahead using RNG innovations

  // Helper for RNG innovations (sum-to-zero across teams)
  for (k in 1:N_oos) {
    int s = oos_season_idx[k];
    int w = oos_week_idx[k];
    int i = oos_home_team[k];
    int j = oos_away_team[k];

    // Smoothed latent state forecast (directly from reconstructed latent state)
    real mu = team_strength[s, w][i]
              - team_strength[s, w][j]
              + oos_hfa[k] * team_hfa[s][i];
    mu_pred[k] = mu;
    y_pred[k] = normal_rng(mu, sigma_obs);

    // Explicit one-step-ahead predictive from prior state
    vector[N_teams - 1] z_raw;
    vector[N_teams] z0;
    vector[N_teams] ts_prev;
    vector[N_teams] ts_next;

    if (w == 1 && s > 1) {
      // Season rollover: previous season last week -> current season week 1
      ts_prev = team_strength[s - 1, last_week_in_season[s - 1]];
      // draw innovation that sums to zero
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_season_carryover * ts_prev + z0 * sigma_season_carryover;
    } else if (w > 1) {
      // Within-season step: week (w-1) -> week w
      ts_prev = team_strength[s, w - 1];
      for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
      z0 = sum_to_zero_vector_constrain(z_raw);
      ts_next = phi_weekly * ts_prev + z0 * sigma_weekly_innovation;
    } else {
      // No prior state available (s == 1 && w == 1); fall back to smoothed state
      ts_next = team_strength[s, w];
    }

    {
      real mu1 = ts_next[i] - ts_next[j] + oos_hfa[k] * team_hfa[s][i];
      y_pred_one_step[k] = normal_rng(mu1, sigma_obs);
    }
  }
}
