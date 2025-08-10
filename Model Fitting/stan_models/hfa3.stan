// data {
//   int<lower=1> N_games;
//   int<lower=1> N_teams;
//   int<lower=1> N_seasons;
//   int<lower=1> N_weeks;
//   array[N_games] int<lower=1, upper=N_teams> home_id;
//   array[N_games] int<lower=1, upper=N_teams> away_id;
//   array[N_games] int<lower=1, upper=N_weeks> week_id;
//   array[N_games] int<lower=1, upper=N_seasons> season_id;
//   array[N_weeks] int<lower=1, upper=N_seasons> week_season;
//   array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
//   array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;
//   array[N_games] int<lower=0, upper=1> hfa;
//   vector[N_games] result;
// }
// 
// parameters {
//   matrix[N_teams, N_seasons] team_hfa;        // Team-specific HFA per season
//   vector[N_seasons] league_hfa;               // League-average HFA per season
//   real<lower=1e-3> sigma_team_hfa;               // SD: team-to-team within a season
// 
//   matrix[N_teams, N_weeks] team_strength_raw; // Raw, uncentered team strengths
//   real<lower=0, upper=1> beta_w;
//   real<lower=1e-3> sigma_w;
//   real<lower=0, upper=1> beta_s;
//   real<lower=1e-3> sigma_s;
//   real<lower=1e-3> sigma_y;
// }
// 
// transformed parameters {
//   matrix[N_teams, N_weeks] team_strength;
//   for (w in 1:N_weeks) {
//     real mu = mean(team_strength_raw[, w]);
//     for (t in 1:N_teams)
//       team_strength[t, w] = team_strength_raw[t, w] - mu;
//   }
// }
// 
// model {
//   // HFA priors
//   league_hfa ~ normal(0, 3);
//   sigma_team_hfa ~ student_t(3,0,5);
//   for (s in 1:N_seasons)
//     team_hfa[, s] ~ normal(0, sigma_team_hfa); // Deviation from league HFA
// 
//   // Evolution priors
//   beta_w ~ beta(16, 1);
//   sigma_w ~ student_t(3,0,10);
//   beta_s ~ beta(2, 2);
//   sigma_s ~ student_t(3,0,10);
//   sigma_y ~ student_t(3,0,10);
// 
//   // Team strength evolution
//   for (t in 1:N_teams) {
//     team_strength[t, 1] ~ normal(0, 14);
//     for (w in 2:N_weeks) {
//       int season_w = week_season[w];
//       if (w == first_week_of_season[season_w]) {
//         int prev_season = season_w - 1;
//         if (prev_season > 0) {
//           int last_w_prev = last_week_of_season[prev_season];
//           team_strength[t, w] ~ normal(beta_s * team_strength[t, last_w_prev], sigma_s);
//         } else {
//           team_strength[t, w] ~ normal(0, 14);
//         }
//       } else {
//         team_strength[t, w] ~ normal(beta_w * team_strength[t, w - 1], sigma_w);
//       }
//     }
//   }
// 
//   // Game outcome model
//   for (g in 1:N_games) {
//     int i = home_id[g];
//     int j = away_id[g];
//     int w = week_id[g];
//     int s = season_id[g];
//     real hfa_effect = (league_hfa[s] + team_hfa[i, s]) * hfa[g];
//     result[g] ~ normal(team_strength[i, w] - team_strength[j, w] + hfa_effect, sigma_y);
//   }
// }
// 
// generated quantities {
//   matrix[N_teams, N_seasons] total_team_hfa;
//   for (s in 1:N_seasons)
//     for (i in 1:N_teams)
//       total_team_hfa[i, s] = league_hfa[s] + team_hfa[i, s];
// }
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
  array[N_games] int<lower=0, upper=1> hfa;
  vector[N_games] result;
}

parameters {
  matrix[N_teams, N_seasons] team_hfa;        // Team-specific HFA per season
  vector[N_seasons] league_hfa;               // League-average HFA per season
  real<lower=1e-3> sigma_team_hfa;

  matrix[N_teams, N_weeks] team_strength_raw; // Unconstrained team strengths (latent)
  real<lower=0, upper=1> beta_w;
  real<lower=1e-3> sigma_w;
  real<lower=0, upper=1> beta_s;
  real<lower=1e-3> sigma_s;
  real<lower=1e-3> sigma_y;
}

transformed parameters {
  matrix[N_teams, N_weeks] team_strength;

  // 1. Set unconstrained strengths as default
  for (t in 1:N_teams)
    for (w in 1:N_weeks)
      team_strength[t, w] = team_strength_raw[t, w];

  // 2. Enforce sum-to-zero at the first week of each season
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    real mu = mean(team_strength_raw[, fw]);
    for (t in 1:N_teams)
      team_strength[t, fw] = team_strength_raw[t, fw] - mu;
  }
}

model {
  // Priors for HFA
  league_hfa ~ normal(0, 3);
  sigma_team_hfa ~ student_t(3, 0, 5);
  for (s in 1:N_seasons)
    team_hfa[, s] ~ normal(0, sigma_team_hfa);

  // Priors for evolution parameters
  beta_w ~ beta(16, 1);
  sigma_w ~ student_t(3, 0, 10);
  beta_s ~ beta(2, 2);
  sigma_s ~ student_t(3, 0, 10);
  sigma_y ~ student_t(3, 0, 10);

  // TEAM STRENGTH EVOLUTION: Match Glickman exactly
  for (s in 1:N_seasons) {
    int fw = first_week_of_season[s];
    int lw = last_week_of_season[s];

    // Initial team strengths (first week of season)
    if (s == 1) {
      // First ever week: mean zero (enforced in transformed)
      for (t in 1:N_teams)
        team_strength_raw[t, fw] ~ normal(0, 14);
    } else {
      // For later seasons: evolve from *mean-centered* last week prior season, then re-center (enforced in transformed)
      int prev_s = s - 1;
      int lw_prev = last_week_of_season[prev_s];
      real mean_prev = mean(team_strength[, lw_prev]);
      for (t in 1:N_teams)
        team_strength_raw[t, fw] ~ normal(
          beta_s * (team_strength[t, lw_prev] - mean_prev), sigma_s
        );
    }

    // Subsequent weeks of current season
    for (w in (fw + 1):lw) {
      team_strength_raw[, w] ~ normal(
        beta_w * team_strength[, w - 1], sigma_w
      );
      // Note: no centering here; mean can drift during season
    }
  }

  // Likelihood for games
  for (g in 1:N_games) {
    int i = home_id[g];
    int j = away_id[g];
    int w = week_id[g];
    int s = season_id[g];
    real hfa_effect = (league_hfa[s] + team_hfa[i, s]) * hfa[g];
    result[g] ~ normal(team_strength[i, w] - team_strength[j, w] + hfa_effect, sigma_y);
  }
}

generated quantities {
  matrix[N_teams, N_seasons] total_team_hfa;
  for (s in 1:N_seasons)
    for (i in 1:N_teams)
      total_team_hfa[i, s] = league_hfa[s] + team_hfa[i, s];
}
