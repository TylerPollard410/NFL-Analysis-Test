// ssm1_gq.stan
// Stand-alone generated-quantities forecaster (use algorithm=fixed_param)
//
// DATA CONTRACT (per draw):
//  - Scalars: beta_w, sigma_w, beta_s, sigma_s, beta_hfa, sigma_hfa, sigma_team_hfa, sigma_y,
//             league_hfa_cur (season S_T current league HFA)
//  - Final states: team_strength_T[J], team_hfa_cur[J] (for current season S_T)
//  - Bookkeeping: current_season (S_T), current_week (W_T), first/last week per season
//  - Forecast schedule: home/away ids, week_id, season_id, hfa for N_oos games
//
// OUTPUT:
//  - mu[d, g], y_pred[d, g] for each draw d=1..N_draws and game g=1..N_oos
//  (shaped as array[N_draws] vector[N_oos] in Stan)

functions {
  // Draw a zero-sum standard normal vector of length K.
  vector zero_sum_std_normal_rng(int K) {
    vector[K] z;
    real m;
    for (k in 1:K) z[k] = normal_rng(0, 1);
    m = mean(z);
    for (k in 1:K) z[k] -= m;
    return z; // zero-sum; variance scale handled by sigma_* multipliers
  }
}

data {
  int<lower=1> N_draws;
  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  // Current "as-of" point from the fit this snapshot came from
  int<lower=1, upper=N_seasons> current_season; // S_T
  int<lower=1, upper=N_weeks>   current_week;   // W_T
  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  // Posterior scalars per draw (aligned across arrays)
  vector[N_draws] beta_w;
  vector<lower=0>[N_draws] sigma_w;
  vector[N_draws] beta_s;
  vector<lower=0>[N_draws] sigma_s;
  vector[N_draws] beta_hfa;
  vector<lower=0>[N_draws] sigma_hfa;
  vector<lower=0>[N_draws] sigma_team_hfa;
  vector<lower=0>[N_draws] sigma_y;

  // Final "as-of" states per draw (for season = current_season, week = current_week)
  array[N_draws] vector[N_teams] team_strength_T;
  array[N_draws] vector[N_teams] team_hfa_cur;   // team HFA for current season
  vector[N_draws] league_hfa_cur;                // league HFA for current season

  // Forecast schedule (games to predict)
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_teams>  home_id;
  array[N_oos] int<lower=1, upper=N_teams>  away_id;
  array[N_oos] int<lower=1, upper=N_weeks>  week_id;    // absolute week index (same convention as the fit)
  array[N_oos] int<lower=1, upper=N_seasons> season_id; // season index per game
  array[N_oos] int<lower=0, upper=1>        hfa;
}

parameters {
  // none (fixed_param)
}

generated quantities {
  // Outputs: one vector per draw containing predictions for all N_oos games
  array[N_draws] vector[N_oos] mu_oos;
  array[N_draws] vector[N_oos] y_pred;

  // For internal propagation per draw
  // We'll allocate containers for states and HFA across needed weeks/seasons
  for (d in 1:N_draws) {
    // Local copies for this draw
    real bw   = beta_w[d];
    real sw   = sigma_w[d];
    real bs   = beta_s[d];
    real ss   = sigma_s[d];
    real bhfa = beta_hfa[d];
    real shfa = sigma_hfa[d];
    real st_hfa = sigma_team_hfa[d];
    real sy   = sigma_y[d];

    // Prepare state holders
    array[N_weeks] vector[N_teams] theta;      // team strengths by week
    array[N_seasons] vector[N_teams] team_hfa_s; // team HFA by season
    vector[N_seasons] league_hfa_s;

    // Initialize with current "as-of" states for the draw
    for (w in 1:N_weeks) theta[w] = rep_vector(0.0, N_teams);
    theta[current_week] = team_strength_T[d];

    for (s in 1:N_seasons) team_hfa_s[s] = rep_vector(0.0, N_teams);
    league_hfa_s[current_season] = league_hfa_cur[d];
    team_hfa_s[current_season]   = team_hfa_cur[d];

    // Determine max week needed per season from the forecast schedule
    array[N_seasons] int max_week_needed;
    for (s in 1:N_seasons) max_week_needed[s] = 0;
    for (k in 1:N_oos) {
      int s = season_id[k];
      int w = week_id[k];
      if (w > max_week_needed[s]) max_week_needed[s] = w;
    }

    // Ensure the previous season is fully defined if we will carry over
    // If current_week < last_week_of_season[current_season], first evolve within the current season up to needed week
    if (max_week_needed[current_season] > current_week) {
      for (w in (current_week + 1):max_week_needed[current_season]) {
        vector[N_teams] z = zero_sum_std_normal_rng(N_teams);
        theta[w] = bw * theta[w - 1] + sw * z;
      }
    }

    // Propagate across future seasons as needed
    for (s in (current_season + 1):N_seasons) {
      if (max_week_needed[s] == 0)
        continue; // nothing to do for this season

      // Evolve league HFA to next season
      league_hfa_s[s] = bhfa * league_hfa_s[s - 1] + shfa * normal_rng(0, 1);

      // New season team HFA deviations (zero-sum)
      {
        vector[N_teams] z_hfa = zero_sum_std_normal_rng(N_teams);
        team_hfa_s[s] = league_hfa_s[s] + st_hfa * z_hfa;
      }

      // Season-start strengths from last week of previous season
      {
        int lw_prev = last_week_of_season[s - 1];
        int fw      = first_week_of_season[s];
        vector[N_teams] z_start = zero_sum_std_normal_rng(N_teams);
        // If needed, ensure theta[lw_prev] exists (it should if current season was propagated fully)
        theta[fw] = bs * theta[lw_prev] + ss * z_start;

        // Within-season evolution up to the maximum week needed
        if (max_week_needed[s] > fw) {
          for (w in (fw + 1):max_week_needed[s]) {
            vector[N_teams] z = zero_sum_std_normal_rng(N_teams);
            theta[w] = bw * theta[w - 1] + sw * z;
          }
        }
      }
    }

    // Build predictions for all requested forecast games for this draw
    for (k in 1:N_oos) {
      int i = home_id[k];
      int j = away_id[k];
      int w = week_id[k];
      int s = season_id[k];

      real mu = (theta[w][i] - theta[w][j]) + team_hfa_s[s][i] * hfa[k];
      mu_oos[d][k] = mu;
      y_pred[d][k] = normal_rng(mu, sy);
    }
  }
}
