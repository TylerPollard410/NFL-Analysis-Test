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

  array[N_weeks]   int<lower=1, upper=N_seasons> week_season;         // (unused, ok)
  array[N_seasons] int<lower=1, upper=N_weeks>   first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks>   last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa; // 1 if true home
  vector[N_games] result;                   // home margin (for all games)
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
  league_hfa[1] = league_hfa_init + league_hfa_z[1] * 2;  // sd ~ 2 at start
  for (s in 2:N_seasons) {
    league_hfa[s] = beta_hfa * league_hfa[s - 1] + league_hfa_z[s] * sigma_hfa;
  }

  // ---------- Team HFA ----------
  matrix[N_teams, N_seasons] team_hfa;
  for (s in 1:N_seasons) {
    for (t in 1:N_teams) {
      team_hfa[t, s] = league_hfa[s] + team_hfa_z[t, s] * sigma_team_hfa;
    }
  }

  // ---------- Team strength states ----------
  matrix[N_teams, N_weeks] team_strength_raw;
  matrix[N_teams, N_weeks] team_strength; // zero-sum by week

  // Build each season fully before moving on
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Season start
    if (s == 1) {
      for (t in 1:N_teams) {
        team_strength_raw[t, fw] = z_start[t, s] * 10; // broad prior
      }
    } else {
      int prev_s = s - 1;
      int lw_prev = last_week_of_season[prev_s];
      real mean_prev = mean(team_strength_raw[, lw_prev]); // fully defined now
      for (t in 1:N_teams) {
        real mu = beta_s * (team_strength_raw[t, lw_prev] - mean_prev);
        team_strength_raw[t, fw] = mu + z_start[t, s] * sigma_s;
      }
    }

    // Within-season propagation
    if (fw < lw) {
      for (w in (fw + 1):lw) {
        for (t in 1:N_teams) {
          team_strength_raw[t, w] = beta_w * team_strength_raw[t, w - 1]
                                    + z_w[t, w] * sigma_w;
        }
      }
    }
  }

  // Zero-sum every week for identifiability
  for (w in 1:N_weeks) {
    real m = mean(team_strength_raw[, w]);
    for (t in 1:N_teams) {
      team_strength[t, w] = team_strength_raw[t, w] - m;
    }
  }
}

model {
  // Priors
  beta_hfa ~ beta(8, 2);
  sigma_hfa ~ student_t(3, 0, 5);
  league_hfa_init ~ normal(2, 2);
  league_hfa_z ~ normal(0, 1);

  to_vector(team_hfa_z) ~ normal(0, 1);
  sigma_team_hfa ~ normal(0, 2);

  beta_w ~ beta(6, 2);
  sigma_w ~ student_t(3, 0, 10);

  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 10);

  to_vector(z_start) ~ normal(0, 1);
  to_vector(z_w)     ~ normal(0, 1);

  sigma_y ~ student_t(3, 0, 10);

  // Likelihood only uses the first N_obs games (no leakage)
  for (g in 1:N_obs) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    real mu = (team_strength[i, w] - team_strength[j, w])
              + team_hfa[i, s] * hfa[g];
    result[g] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Predictions for ALL games in data using filtered states
  vector[N_games] mu;
  matrix[N_teams, N_seasons] team_total_hfa;
  matrix[N_teams, N_seasons] team_hfa_dev;

  for (g in 1:N_games) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    mu[g] = (team_strength[i, w] - team_strength[j, w])
            + team_hfa[i, s] * hfa[g];
  }

  for (s in 1:N_seasons) {
    for (t in 1:N_teams) {
      team_total_hfa[t, s] = team_hfa[t, s];
      team_hfa_dev[t, s]   = team_hfa[t, s] - league_hfa[s];
    }
  }
}
