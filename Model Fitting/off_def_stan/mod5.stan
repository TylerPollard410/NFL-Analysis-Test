// Global-week state-space model (Glickman-style)
// - Uses sum_to_zero_vector for efficient identifiability
// - Non-centered innovations for better sampling
// - Leverages provided global week_idx and season boundary indicators
data {
  int<lower=1> N_games;
  int<lower=2> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks; // global weeks across all seasons

  // Game-level indices
  array[N_games] int<lower=1, upper=N_teams> home_team;
  array[N_games] int<lower=1, upper=N_teams> away_team;
  array[N_games] int<lower=1, upper=N_seasons> season_idx;
  array[N_games] int<lower=1, upper=N_weeks> week_idx; // global week index
  array[N_games] int<lower=0, upper=1> fw_season_idx;  // first week of season (by game)
  array[N_games] int<lower=0, upper=1> lw_season_idx;  // last week of season (by game)
  array[N_games] int<lower=0, upper=1> hfa;            // home-field indicator

  // Outcome (e.g., home margin)
  array[N_games] int result;
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

  // Map global weeks to seasons and boundary flags
  array[N_weeks] int week_to_season;
  array[N_weeks] int is_first_week;  // 1 if first week of a season
  array[N_weeks] int is_last_week;   // 1 if last week of a season

  // League HFA AR(1) over seasons (non-centered via innovation scaling)
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s - 1]
                    + league_hfa_innovation[s - 1] * sigma_league_hfa_innovation;

  // Team HFA per season
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;

  // Initialize mapping/flags
  for (w in 1:N_weeks) {
    week_to_season[w] = 1;
    is_first_week[w] = 0;
    is_last_week[w] = 0;
  }
  // Aggregate game-level boundary indicators to global-week flags
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    if (s > week_to_season[w]) week_to_season[w] = s;
    if (fw_season_idx[g] == 1) is_first_week[w] = 1;
    if (lw_season_idx[g] == 1) is_last_week[w] = 1;
  }
  is_first_week[1] = 1; // enforce first week overall

  // Initialize state at global week 1
  team_strength[1] = to_vector(team_strength_init) * sigma_strength_init;

  // Global-week evolution
  for (w in 2:N_weeks) {
    if (is_first_week[w] == 1) {
      int s = week_to_season[w]; // current season of week w
      // carryover: use season index s-1 for z_season_carryover
      team_strength[w] = phi_season_carryover * team_strength[w - 1]
                         + to_vector(z_season_carryover[s - 1]) * sigma_season_carryover;
    } else {
      team_strength[w] = phi_weekly * team_strength[w - 1]
                         + to_vector(z_weekly_innovation[w - 1]) * sigma_weekly_innovation;
    }
  }
}

model {
  // Priors (tuneable)
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 0.3);
  phi_league_hfa ~ beta(8, 2);            // moderately persistent

  sigma_team_hfa ~ normal(0, 1);
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1);                // strong weekly persistence
  phi_season_carryover ~ beta(6, 4);      // moderate season carryover

  sigma_obs ~ normal(0, 5);

  // Non-centered innovation priors
  league_hfa_innovation ~ std_normal();
  for (s in 1:N_seasons) to_vector(team_hfa_deviation[s]) ~ std_normal();
  to_vector(team_strength_init) ~ std_normal();
  for (w in 1:(N_weeks - 1)) to_vector(z_weekly_innovation[w]) ~ std_normal();
  for (s in 1:(N_seasons - 1)) to_vector(z_season_carryover[s]) ~ std_normal();

  // Likelihood
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    int i = home_team[g];
    int j = away_team[g];
    real mu = team_strength[w][i] - team_strength[w][j]
              + hfa[g] * team_hfa[s][i];
    result[g] ~ normal(mu, sigma_obs);
  }
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

  // One-week-ahead prediction (mean and RNG draw)
  vector[N_teams] predicted_team_strength_next_mean;
  vector[N_teams] predicted_team_strength_next_draw;
  {
    int next_is_first = is_last_week[last_w]; // if last_w is season end, next is first
    if (next_is_first == 1)
      predicted_team_strength_next_mean = phi_season_carryover * team_strength[last_w];
    else
      predicted_team_strength_next_mean = phi_weekly * team_strength[last_w];

    // sum-to-zero innovation draw via inverse transform
    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
    predicted_team_strength_next_draw = predicted_team_strength_next_mean
                                       + z0 * (next_is_first == 1 ? sigma_season_carryover
                                                                 : sigma_weekly_innovation);
  }
}
