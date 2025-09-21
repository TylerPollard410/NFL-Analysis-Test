// Global-week state-space model (Glickman-style)
// - Uses sum_to_zero_vector for efficient identifiability
// - Non-centered innovations for better sampling
// - Leverages provided global week_idx and season boundary indicators
functions {
  // Centralized computation of game expected values
  vector compute_mu(array[] int home_team, array[] int away_team,
                    array[] int week_idx, array[] int season_idx,
                    array[] int hfa, array[] vector team_strength,
                    array[] vector team_hfa, int N_games) {
    vector[N_games] mu;
    for (g in 1:N_games) {
      int w = week_idx[g];
      int s = season_idx[g];
      int h = home_team[g];
      int a = away_team[g];
      mu[g] = team_strength[w][h] - team_strength[w][a];
      if (hfa[g] == 1) {
        mu[g] += team_hfa[s][h];
      }
    }
    return mu;
  }
}

data {
  int<lower=1> N_games;
  int<lower=2> N_teams; // number of teams = 32
  int<lower=1> N_seasons; // number of seasons starting from 1 = 2002
  int<lower=1> N_weeks; // number of weeks starting from 1 = 2002 week 1
  
  // Indexing variables
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx; // global season index
  array[N_games] int<lower=1, upper=N_weeks> week_idx; // global week index
  
  // Indicator variables for season transitions
  array[N_games] int<lower=0, upper=1> fw_season_idx; // First week of season (by game)
  array[N_games] int<lower=0, upper=1> lw_season_idx; // Last week of season (by game)
  array[N_games] int<lower=0, upper=1> hfa; // Home-field advantage indicator
  
  // Response variables
  array[N_games] int<lower=0> home_score;
  array[N_games] int<lower=0> away_score;
  array[N_games] int<lower=0> total;
  array[N_games] int result;
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
  // League HFA AR(1) across seasons
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation;  // std_normal, scaled in TP
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  // Team HFA deviations per season (sum-to-zero around league HFA)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  // Initial team strengths at global week 1 (sum-to-zero)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  // Global weekly innovations and season-carryover innovations (sum-to-zero)
  array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;

  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;

  // Observation noise
  real<lower=0> sigma_obs;
}

transformed parameters {
  // League/Team HFA
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;

  // Global-week latent strengths
  array[N_weeks] vector[N_teams] team_strength;

  // League HFA AR(1) over seasons (non-centered via innovation scaling)
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;

  // Team HFA per season
  for (s in 1:N_seasons)
    team_hfa[s] = league_hfa[s] + team_hfa_deviation[s] * sigma_team_hfa;

  // Initialize state at global week 1
  team_strength[1] = team_strength_init * sigma_strength_init;

  // Global-week evolution
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w]; // current season of week w
      // carryover: use season index s-1 for z_season_carryover
      team_strength[w] = phi_season_carryover * team_strength[w - 1]
                         + z_season_carryover[s - 1] * sigma_season_carryover;
    } else {
      team_strength[w] = phi_weekly * team_strength[w - 1]
                         + z_weekly_innovation[w - 1] * sigma_weekly_innovation;
    }
  }
}

model {
  // Priors (tuneable)
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 1);
  phi_league_hfa ~ beta(8, 2);            // moderately persistent

  sigma_team_hfa ~ normal(0, 1);
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1);                // strong weekly persistence
  phi_season_carryover ~ beta(6, 4);      // moderate season carryover

  sigma_obs ~ normal(13, 5);

  // Non-centered innovation priors
  league_hfa_innovation ~ std_normal();
  for (s in 1:N_seasons) team_hfa_deviation[s] ~ std_normal();
  team_strength_init ~ std_normal();
  for (w in 1:(N_weeks - 1)) z_weekly_innovation[w] ~ std_normal();
  for (s in 1:(N_seasons - 1)) z_season_carryover[s] ~ std_normal();

  // Likelihood
  vector[N_games] mu;
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    int i = home_team[g];
    int j = away_team[g];
    mu[g] = team_strength[w][i] - team_strength[w][j]
              + hfa[g] * team_hfa[s][i];
    // if (is_nan(team_strength[w][i]) || is_nan(team_strength[w][j])
    //     || is_nan(team_hfa[s][i]) || is_nan(mu) || is_nan(sigma_obs)
    //     || is_inf(team_strength[w][i]) || is_inf(team_strength[w][j])
    //     || is_inf(team_hfa[s][i]) || is_inf(mu) || is_inf(sigma_obs)
    //     || sigma_obs <= 0) {
    //   print("diagnostic:: g=", g,
    //         ", w=", w,
    //         ", s=", s,
    //         ", home=", i,
    //         ", away=", j,
    //         ", mu=", mu,
    //         ", sigma_obs=", sigma_obs,
    //         ", ts_home=", team_strength[w][i],
    //         ", ts_away=", team_strength[w][j],
    //         ", hfa_indicator=", hfa[g],
    //         ", team_hfa_home=", team_hfa[s][i],
    //         ", league_hfa=", league_hfa[s]);
    // }
    // result[g] ~ normal(mu, sigma_obs);
  }
  result ~ normal(mu, sigma_obs);
}

generated quantities {
  // Last observed global week and its season
  int last_w = 1;
  for (g in 1:N_games) if (week_idx[g] > last_w) last_w = week_idx[g];
  int last_s = week_to_season[last_w];

  // Filtered (= smoothed at final time) state snapshots
  vector[N_teams] filtered_team_strength_last = team_strength[last_w];
  vector[N_teams] filtered_team_hfa_last = team_hfa[last_s];
  real filtered_league_hfa_last = league_hfa[last_s];
  
  // Log-likelihood for model comparison (LOO-CV, WAIC)
  vector[N_games] log_lik;
  {
    vector[N_games] mu = compute_mu(home_team, away_team, week_idx, season_idx,
                                    hfa, team_strength, team_hfa, N_games);
    for (g in 1:N_games) {
      log_lik[g] = normal_lpdf(result[g] | mu[g], sigma_obs);
    }
  }

  // One-week-ahead prediction (mean and RNG draw)
  // vector[N_teams] predicted_team_strength_next_mean;
  // vector[N_teams] predicted_team_strength_next_draw;
  // {
  //   int next_is_first = is_last_week[last_w]; // if last_w is season end, next is first
  //   if (next_is_first == 1)
  //     predicted_team_strength_next_mean = phi_season_carryover * team_strength[last_w];
  //   else
  //     predicted_team_strength_next_mean = phi_weekly * team_strength[last_w];

  //   // sum-to-zero innovation draw via inverse transform
  //   // Draw N_teams-1 unconstrained normals, map to a length-N_teams zero-sum vector
  //   vector[N_teams - 1] z_raw;
  //   for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
  //   vector[N_teams] z0 = sum_to_zero_constrain(z_raw);
  //   predicted_team_strength_next_draw = predicted_team_strength_next_mean
  //                                      + z0 * (next_is_first == 1 ? sigma_season_carryover
  //                                                                : sigma_weekly_innovation);
  // }
}
