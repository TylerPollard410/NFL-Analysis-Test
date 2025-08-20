// HFA + team-strength state-space (32 teams every season)
// - No active-team mask needed
// - No season_of_week needed
// - Zero-init states to avoid NaNs on unused weeks
// - Minimal GQ: set N_oos=0 during fitting; use generate_quantities() later

data {
  int<lower=1> N_games;                  // total games in window
  int<lower=1> N_obs;                    // games to condition on (<= N_games)

  int<lower=1> N_teams;                  // 32 for NFL since 2002
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  // Local indices (1..N_teams/weeks/seasons within this window)
  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks> week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  // Per-season week boundaries (LOCAL week indices)
  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  // Inputs
  array[N_games] int<lower=0, upper=1> hfa;  // 1 if true home
  vector[N_games] result;                    // home margin (all games)

  // One-step-ahead evaluation set (can be 0 to skip work during fit)
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}

parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_z;
  real              league_hfa_init;
  real<lower=0, upper=1> beta_hfa;
  real<lower=0>          sigma_hfa;

  // Team HFA deviations about league mean (non-centered)
  array[N_seasons] vector[N_teams] team_hfa_z;
  real<lower=0>              sigma_team_hfa;

  // Team strength state-space (non-centered)
  array[N_seasons] vector[N_teams] z_start;   // season-start shocks
  array[N_weeks] vector[N_teams]   z_w;       // weekly shocks

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

  // Team HFA (center across ALL teams)
  array[N_seasons] vector[N_teams] team_hfa;
  for (s in 1:N_seasons) {
    // vector[N_teams] zcol = team_hfa_z[, s];
    team_hfa[s] = league_hfa[s] + sigma_team_hfa * (team_hfa_z[s] - mean(team_hfa_z[s]));
  }

  // Team strength states 
  array[N_weeks] vector[N_teams] team_strength_raw = rep_array(rep_vector(0.0, N_teams), N_weeks);
  array[N_weeks] vector[N_teams] team_strength; // centered by week (plain mean)

  // Build each season fully before moving on
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Season start (slightly narrower diffuse scale)
    if (s == 1) {
      team_strength_raw[fw] = 5 * z_start[s];
    } else {
      int prev_s = s - 1;
      int lw_prev = last_week_of_season[prev_s];
      vector[N_teams] prev = team_strength_raw[lw_prev];
      //real m_prev = mean(prev);
      team_strength_raw[fw] = beta_s * (prev - mean(prev)) + sigma_s * z_start[s];
    }

    // Within-season AR(1)
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        team_strength_raw[w] = beta_w * team_strength_raw[w - 1]
                                 + sigma_w * z_w[w];
      }
    }
  }

  // Weekly sum-to-zero (plain mean)
  // for (w in 1:N_weeks) {
  //   vector[N_teams] colw = team_strength_raw[, w];
  //   team_strength[, w] = colw - mean(colw);
  // }
  for (w in 1:N_weeks) {
    //vector[N_teams] colw = team_strength_raw[, w];
    //real m = mean(colw);
    team_strength[w] = team_strength_raw[w] - mean(team_strength_raw[w]);
  }
}

model {
  // Priors (vectorized) 
  league_hfa_z ~ std_normal();
  for (s in 1:N_seasons) team_hfa_z[s] ~ std_normal();
  for (s in 1:N_seasons) z_start[s]    ~ std_normal();
  for (w in 1:N_weeks)   z_w[w]        ~ std_normal();

  beta_hfa ~ beta(8, 2);
  sigma_hfa ~ student_t(3, 0, 2);
  league_hfa_init ~ normal(2, 2);
  
  sigma_team_hfa ~ normal(0,2);
  // sigma_team_hfa ~ student_t(3, 0, 2);

  beta_w ~ beta(6, 2);
  sigma_w ~ student_t(3, 0, 5);

  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 5);

  sigma_y ~ student_t(3, 0, 10);

  // --- Likelihood over the first N_obs games (init + accumulate) ---
  {
    vector[N_obs] mu_obs ;
    for (g in 1:N_obs) {
      int i = home_id[g];
      int j = away_id[g];
      int w = week_id[g];
      int s = season_id[g];

      mu_obs[g] = (team_strength[w,i] - team_strength[w,j]) + team_hfa[s,i] * hfa[g];

      // DEBUG (optional): uncomment temporarily if chasing non-finites
      // if (is_nan(mu_obs[g]) || is_inf(mu_obs[g])) reject("non-finite mu at g=", g,
      //   " i=", i, " j=", j, " w=", w, " s=", s,
      //   " ts_iw=", team_strength[i,w],
      //   " ts_jw=", team_strength[j,w],
      //   " th_i_s=", team_hfa[i,s],
      //   " hfa=", hfa[g]);
    }
    target += normal_lpdf(result[1:N_obs] | mu_obs, sigma_y);
  }
}

generated quantities {
  // Snapshot at end of training window (t-1)
  int last_w = week_id[N_obs];
  int last_s = season_id[N_obs];
  vector[N_teams] team_strength_last = team_strength[last_w];
  real            league_hfa_last    = league_hfa[last_s];
  vector[N_teams] team_hfa_last      = team_hfa[last_s];

  // Minimal OOS: compute only if N_oos > 0 (set N_oos=0 during fitting)
  vector[N_oos] mu_oos;
  vector[N_oos] y_oos;
  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu_oos[k] = (team_strength[w-1, i] - team_strength[w-1,i])
                + team_hfa[s,i] * hfa[g];
    y_oos[k] = normal_rng(mu_oos[k], sigma_y);
  }
}
