data {
  int<lower=1> N_games;   // total games passed in (full 2006-2024)
  int<lower=1> N_obs;     // games to condition on this step (<= N_games)

  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks> week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa; // 1 if true home
  vector[N_games] result;                   // home margin (for all games)

  // One-step-ahead evaluation set for this fit (e.g., next week's games)
  int<lower=0> N_oos;
  array[N_oos] int<lower=1, upper=N_games> oos_idx;
}

parameters {
  // League HFA AR(1), non-centered
  vector[N_seasons] league_hfa_z;
  real               league_hfa_init;
  real<lower=0, upper=1> beta_hfa;
  real<lower=0>          sigma_hfa;

  // Team HFA deviations about league mean (non-centered)
  matrix[N_teams, N_seasons] team_hfa_z;
  real<lower=0>              sigma_team_hfa;

  // Team strength state-space (non-centered)
  matrix[N_teams, N_seasons] z_start;   // season-start shocks
  matrix[N_teams, N_weeks]   z_w;       // weekly shocks

  real<lower=0, upper=1> beta_w;        // within-season AR(1)
  real<lower=0>          sigma_w;       // within-season shock sd

  real<lower=0, upper=1> beta_s;        // between-season carryover
  real<lower=0>          sigma_s;       // between-season start shock sd

  real<lower=0> sigma_y;                // obs noise
}

transformed parameters {
  // ---------- League HFA series ----------
  vector[N_seasons] league_hfa;
  league_hfa[1] = league_hfa_init + 2 * league_hfa_z[1];  // diffuse start (sd ≈ 2)
  for (s in 2:N_seasons) {
    league_hfa[s] = beta_hfa * league_hfa[s - 1] + sigma_hfa * league_hfa_z[s];
  }

  // ---------- Team HFA (centered so seasonal mean equals league_hfa[s]) ----------
  matrix[N_teams, N_seasons] team_hfa;
  for (s in 1:N_seasons) {
    vector[N_teams] zcol = team_hfa_z[, s];
    team_hfa[, s] = league_hfa[s] + sigma_team_hfa * (zcol - mean(zcol));
  }

  // ---------- Team strength states ----------
  matrix[N_teams, N_weeks] team_strength_raw;
  matrix[N_teams, N_weeks] team_strength; // sum-to-zero by week for identifiability

  // Build each season fully before moving on
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Season start
    if (s == 1) {
      team_strength_raw[, fw] = 10 * z_start[, s];  // broad prior on first season
    } else {
      int prev_s = s - 1;
      int lw_prev = last_week_of_season[prev_s];
      vector[N_teams] prev = team_strength_raw[, lw_prev];
      real m_prev = mean(prev);
      team_strength_raw[, fw] = beta_s * (prev - m_prev) + sigma_s * z_start[, s];
    }

    // Within-season AR(1)
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        team_strength_raw[, w] = beta_w * team_strength_raw[, w - 1]
                                 + sigma_w * z_w[, w];
      }
    }
  }

  // Enforce weekly sum-to-zero without custom helper
  for (w in 1:N_weeks) {
    vector[N_teams] colw = team_strength_raw[, w];
    team_strength[, w] = colw - mean(colw);
  }
}

model {
  // --- Priors (vectorized; use std_normal where possible) ---
  league_hfa_z ~ std_normal();
  to_vector(team_hfa_z)   ~ std_normal();
  to_vector(z_start) ~ std_normal();
  to_vector(z_w)     ~ std_normal();

  // AR coefficients and scales (weakly-informative regularization)
  beta_hfa ~ beta(8, 2);
  sigma_hfa ~ student_t(3, 0, 5);
  league_hfa_init ~ normal(2, 2);

  sigma_team_hfa ~ normal(0, 2);

  beta_w ~ beta(6, 2);
  sigma_w ~ student_t(3, 0, 10);

  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 10);

  sigma_y ~ student_t(3, 0, 10);

  // --- Likelihood over the first N_obs games (simple loop + vectorized lpdf) ---
  {
    vector[N_obs] mu_obs;
    for (g in 1:N_obs) {
      int i = home_id[g];
      int j = away_id[g];
      int w = week_id[g];
      int s = season_id[g];
      mu_obs[g] = (team_strength[i, w] - team_strength[j, w])
                  + team_hfa[i, s] * hfa[g];
    }
    target += normal_lpdf(result[1:N_obs] | mu_obs, sigma_y);
  }
}

generated quantities {
  // Snapshot of "what's known" at the end of the training window (t-1)
  int last_w = week_id[N_obs];
  int last_s = season_id[N_obs];
  vector[N_teams]   team_strength_last = team_strength[, last_w];
  real league_hfa_last    = league_hfa[last_s];         // convenience copy
  vector[N_teams] team_hfa_last = team_hfa[, last_s];       // convenience copy

  // One-week-ahead predictions for evaluation games only
  vector[N_oos] mu_oos;
  vector[N_oos] y_oos;   // posterior predictive draws for OOS games

  for (k in 1:N_oos) {
    int g = oos_idx[k];
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];   // typically last_w + 1
    int s = season_id[g];

    mu_oos[k] = (team_strength[i, w] - team_strength[j, w])
                 + team_hfa[i, s] * hfa[g];

    y_oos[k] = normal_rng(mu_oos[k], sigma_y);
  }
}
