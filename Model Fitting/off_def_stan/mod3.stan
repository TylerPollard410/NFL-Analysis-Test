// Glickman-style dynamic paired-comparison with sum_to_zero_vector
// Uses built-in sum_to_zero_vector transforms for identifiability and efficiency
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

  // Outcomes (result = home_margin, etc.)
  array[N_games] int result;
}

parameters {
  // League HFA AR(1) across seasons
  real league_hfa_init;
  vector[N_seasons - 1] league_hfa_innovation;
  real<lower=0> sigma_league_hfa_innovation;
  real<lower=0, upper=1> phi_league_hfa;

  // Team-specific HFA deviations per season (sum-to-zero)
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_deviation;
  real<lower=0> sigma_team_hfa;

  // Initial team strengths (sum-to-zero)
  sum_to_zero_vector[N_teams] team_strength_init;
  real<lower=0> sigma_strength_init;

  // Within-season weekly innovations (sum-to-zero)
  array[N_seasons] array[N_weeks - 1] sum_to_zero_vector[N_teams] z_weekly_innovation;
  real<lower=0> sigma_weekly_innovation;
  real<lower=0, upper=1> phi_weekly;

  // Season carryover innovations (sum-to-zero) between seasons
  array[N_seasons - 1] sum_to_zero_vector[N_teams] z_season_carryover;
  real<lower=0> sigma_season_carryover;
  real<lower=0, upper=1> phi_season_carryover;

  // Observation model
  real<lower=0> sigma_obs;
}

transformed parameters {
  vector[N_seasons] league_hfa;
  array[N_seasons] vector[N_teams] team_hfa;
  array[N_seasons] array[N_weeks] vector[N_teams] team_strength;
  array[N_seasons] int last_week_in_season;

  // League HFA AR(1)
  league_hfa[1] = league_hfa_init;
  for (s in 2:N_seasons)
    league_hfa[s] = phi_league_hfa * league_hfa[s-1]
                    + league_hfa_innovation[s-1] * sigma_league_hfa_innovation;

  // Team HFA = league + deviation
  for (s in 1:N_seasons)
    team_hfa[s] = rep_vector(league_hfa[s], N_teams)
                  + to_vector(team_hfa_deviation[s]) * sigma_team_hfa;

  // Last observed week per season from data
  for (s in 1:N_seasons) last_week_in_season[s] = 1;
  for (g in 1:N_games)
    if (lw_season_idx[g] == 1 && week_idx[g] > last_week_in_season[season_idx[g]])
      last_week_in_season[season_idx[g]] = week_idx[g];

  // Initialize season 1, week 1
  team_strength[1,1] = to_vector(team_strength_init) * sigma_strength_init;

  // Evolve within season 1
  for (w in 2:N_weeks)
    team_strength[1,w] = phi_weekly * team_strength[1,w-1]
                         + to_vector(z_weekly_innovation[1,w-1]) * sigma_weekly_innovation;

  // Seasons 2..N: carryover + within-season evolution
  for (s in 2:N_seasons) {
    team_strength[s,1] = phi_season_carryover * team_strength[s-1, last_week_in_season[s-1]]
                         + to_vector(z_season_carryover[s-1]) * sigma_season_carryover;
    for (w in 2:N_weeks)
      team_strength[s,w] = phi_weekly * team_strength[s,w-1]
                           + to_vector(z_weekly_innovation[s,w-1]) * sigma_weekly_innovation;
  }
}

model {
  // Priors
  league_hfa_init ~ normal(3, 1);
  sigma_league_hfa_innovation ~ normal(0, 0.3);
  phi_league_hfa ~ beta(8, 2);

  sigma_team_hfa ~ normal(0, 1);
  sigma_strength_init ~ normal(0, 3);
  sigma_weekly_innovation ~ normal(0, 1);
  sigma_season_carryover ~ normal(0, 1.5);
  phi_weekly ~ beta(9, 1);
  phi_season_carryover ~ beta(6, 4);

  // Non-centered innovations
  league_hfa_innovation ~ std_normal();
  for (s in 1:N_seasons) to_vector(team_hfa_deviation[s]) ~ std_normal();
  to_vector(team_strength_init) ~ std_normal();
  for (s in 1:N_seasons) for (w in 1:(N_weeks-1)) to_vector(z_weekly_innovation[s,w]) ~ std_normal();
  for (s in 1:(N_seasons-1)) to_vector(z_season_carryover[s]) ~ std_normal();

  // Likelihood
  for (g in 1:N_games) {
    int s = season_idx[g];
    int w = week_idx[g];
    int i = home_team[g];
    int j = away_team[g];
    real mu = team_strength[s,w][i] - team_strength[s,w][j]
              + hfa[g] * team_hfa[s][i];
    result[g] ~ normal(mu, sigma_obs);
  }
}

generated quantities {
  // Latest season/week snapshot
  int s_last = N_seasons;
  int w_last = last_week_in_season[s_last];
  vector[N_teams] filtered_team_strength_last = team_strength[s_last, w_last];
  vector[N_teams] filtered_team_hfa_last = team_hfa[s_last];
  real filtered_league_hfa_last = league_hfa[s_last];

  // One-week-ahead state prediction within last season (mean and draw)
  vector[N_teams] predicted_team_strength_next_mean = phi_weekly * filtered_team_strength_last;
  vector[N_teams] predicted_team_strength_next_draw;
  {
    vector[N_teams - 1] z_raw;
    for (t in 1:(N_teams - 1)) z_raw[t] = normal_rng(0, 1);
    vector[N_teams] z0 = sum_to_zero_vector_constrain(z_raw);
    predicted_team_strength_next_draw = predicted_team_strength_next_mean + z0 * sigma_weekly_innovation;
  }
}
