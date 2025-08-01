data {
  int<lower=1> N_games;
  int<lower=1> N_teams;
  int<lower=1> N_seasons;
  int<lower=1> N_weeks;

  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks> week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  array[N_weeks] int<lower=1, upper=N_seasons> week_season;
  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;

  array[N_games] int<lower=0, upper=1> hfa; // 1 if home, 0 if neutral
  vector[N_games] result; // home margin
}

parameters {
  // Team strength
  matrix[N_teams, N_weeks] team_strength_raw;

  // League HFA by season
  vector[N_seasons] league_hfa;

  // Team-specific HFA deviation (static)
  vector[N_teams] team_hfa_dev;
  real<lower=1e-3> sigma_team_hfa_dev;

  // AR(1) for team strength: week-to-week
  real<lower=0,upper=1> beta_w;
  real<lower=1e-3> sigma_w;

  // AR(1) for league HFA: season-to-season
  real<lower=0,upper=1> beta_hfa;
  real<lower=1e-3> sigma_hfa;

  // Between-season jump for team strength
  real<lower=0,upper=1> beta_s;
  real<lower=1e-3> sigma_s;

  // Observation noise
  real<lower=1e-3> sigma_y;
}

transformed parameters {
  // Final team strengths (with sum-to-zero correction at season start)
  matrix[N_teams, N_weeks] team_strength;

  for (t in 1:N_teams)
    for (w in 1:N_weeks)
      team_strength[t, w] = team_strength_raw[t, w];

  // Enforce sum-to-zero at each season start
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    real mu = mean(team_strength_raw[, fw]);
    for (t in 1:N_teams)
      team_strength[t, fw] = team_strength_raw[t, fw] - mu;
  }
}

model {
  // Priors
  beta_w ~ beta(16, 1);
  sigma_w ~ student_t(3, 0, 10);
  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 10);

  beta_hfa ~ beta(8, 2); // Encourages persistence, can adjust
  sigma_hfa ~ student_t(3, 0, 2);

  sigma_team_hfa_dev ~ normal(0, 1); // Strong shrinkage
  team_hfa_dev ~ normal(0, sigma_team_hfa_dev);

  sigma_y ~ student_t(3, 0, 10);

  // League HFA AR(1) by season
  league_hfa[1] ~ normal(2, 2); // Prior mean 2, wide SD
  for (s in 2:N_seasons)
    league_hfa[s] ~ normal(beta_hfa * league_hfa[s-1], sigma_hfa);

  // Team strength evolution
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // First week of first season: prior N(0, 10)
    if (s == 1) {
      for (t in 1:N_teams)
        team_strength_raw[t, fw] ~ normal(0, 10);
    } else {
      // New season: AR(1) from last week prior season, mean-centered
      int prev_s = s - 1;
      int lw_prev = last_week_of_season[prev_s];
      real mean_prev = mean(team_strength[, lw_prev]);
      for (t in 1:N_teams)
        team_strength_raw[t, fw] ~ normal(
          beta_s * (team_strength[t, lw_prev] - mean_prev), sigma_s
        );
    }

    // Weeks within season: AR(1) from previous week
    for (w in (fw + 1):lw) {
      team_strength_raw[, w] ~ normal(beta_w * team_strength[, w-1], sigma_w);
    }
  }

  // Likelihood: total HFA = league + team deviation
  for (g in 1:N_games) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    real hfa_effect = (league_hfa[s] + team_hfa_dev[i]) * hfa[g];
    result[g] ~ normal(team_strength[i, w] - team_strength[j, w] + hfa_effect, sigma_y);
  }
}

generated quantities {
  // Total HFA for each team and season, for easy plotting
  matrix[N_teams, N_seasons] team_total_hfa;
  for (s in 1:N_seasons)
    for (i in 1:N_teams)
      team_total_hfa[i, s] = league_hfa[s] + team_hfa_dev[i];
}
