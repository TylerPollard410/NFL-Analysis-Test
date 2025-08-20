// nfl_state_space_result_v1.stan
// Dynamic state-space model for NFL score DIFFERENCE (home - away)
// - Latent team strengths evolve week-to-week and season-to-season
// - League HFA plus team-specific HFA deviations (centered on league HFA)
// - Sum-to-zero identification for team strengths at each (season, week)
// - Neutral-site handling (HFA applies only when truly at home)
// Stan 2.36+ (uses sum_to_zero_vector)

data {
  int<lower=1> N_games;                 // total games supplied
  int<lower=1> N_obs;                   // games to condition on (<= N_games), allows sequential fitting
  int<lower=2> N_teams;                 // number of teams
  int<lower=1> N_seasons;               // number of seasons
  int<lower=1> N_weeks;                 // total distinct week indices across seasons

  // Per-game indexing
  array[N_games] int<lower=1, upper=N_teams> home_id;
  array[N_games] int<lower=1, upper=N_teams> away_id;
  array[N_games] int<lower=1, upper=N_weeks>  week_id;
  array[N_games] int<lower=1, upper=N_seasons> season_id;

  // Week structure by season
  array[N_seasons] int<lower=1, upper=N_weeks> first_week_of_season;
  array[N_seasons] int<lower=1, upper=N_weeks> last_week_of_season;
  
  // True home indicator: 1 = listed home team actually at home; 0 = neutral site
  array[N_games] int<lower=0, upper=1> hfa;

  // Outcomes (home - away score)
  vector[N_games] result;
}

parameters {
  // Game noise (result scale)
  real<lower=0> sigma_game;

  // Evolution scales
  real<lower=0> sigma_week;             // week-to-week evolution sd within season
  real<lower=0> sigma_season;           // jump from last week of season s-1 to week 1 of season s
  real<lower=0> sigma_init;             // prior sd for the very first season's first week

  // Evolution regression (shrinkage/expansion)
  real<lower=0, upper=1> beta_week;   // usually near 1.0
  real<lower=0, upper=1> beta_season; // usually < 1.0 (shrinkage across off-season)

  // Latent team strengths by (season, week): identified via sum-to-zero constraint
  array[N_seasons] array[N_weeks] sum_to_zero_vector[N_teams] team_strength;

  // Home-field advantage
  vector[N_seasons] league_hfa;      // league-wide HFA
  real<lower=0> sigma_team_hfa;         // sd for team-specific HFA deviation
  array[N_seasons] sum_to_zero_vector[N_teams] team_hfa_dev; // deviations centered around 0 (so centered on league HFA)
}

transformed parameters {
  // Team-specific HFA level = league_hfa + deviation
  array[N_seasons] vector[N_teams] team_hfa = league_hfa + team_hfa_dev;
}

model {
  // PRIORS
  // Result noise: weakly-informative around NFL scoring variability
  sigma_game     ~ normal(14, 7);        // ~14 points sd is a common ballpark
  sigma_week     ~ normal(2.5, 2);       // small week-to-week drift
  sigma_season   ~ normal(6, 4);         // bigger off-season drift
  sigma_init     ~ normal(10, 7);

  beta_week      ~ normal(0.9, 0.10); // near-random walk in-season
  beta_season    ~ normal(0.98, 0.10) ;// shrink across seasons

  // HFA: league mean around a few points; team deviations centered and shrunk
  league_hfa     ~ normal(3, 3);                // prior mean ~3 points, weakly-informative
  sigma_team_hfa ~ normal(2, 2);
  team_hfa_dev   ~ normal(0, sigma_team_hfa);

  // EVOLUTION OF TEAM STRENGTHS
  // Season loop
  for (s in 1:N_seasons) {

    // Week 1 of season s
    {
      int w1 = first_week_of_season[s];

      if (s == 1) {
        // First season, first week: centered at 0 with sigma_init
        team_strength[s][w1] ~ normal(0, sigma_init);
      } else {
        // Link from prior season's last week -> current season's first week
        int w_prev = last_week_of_season[s - 1];
        team_strength[s][w1] ~ normal(beta_season * team_strength[s - 1][w_prev], sigma_season);
      }
    }

    // Subsequent weeks within the same season: AR(1)-like evolution
    for (w in (first_week_of_season[s] + 1):last_week_of_season[s]) {
      team_strength[s][w] ~ normal(beta_week * team_strength[s][w - 1], sigma_week);
    }
  }

  // LIKELIHOOD FOR OBSERVED GAMES
  for (n in 1:N_obs) {
    int s  = season_id[n];
    int w  = week_id[n];
    int th = home_id[n];
    int ta = away_id[n];

    // Baseline is neutral-site expected margin (home strength - away strength)
    real mu = team_strength[s][w][th] - team_strength[s][w][ta];

    // Add HFA only if truly at home (neutral sites get zero)
    if (is_true_home[n] == 1) {
      mu += team_hfa[th];
    }

    result[n] ~ normal(mu, sigma_game);
  }
}

generated quantities {
  // Per-game log-likelihood for model comparison and LOO/WAIC
  vector[N_obs] log_lik;
  // Posterior predictive draws for observed games
  vector[N_obs] result_rep;

  for (n in 1:N_obs) {
    int s  = season_id[n];
    int w  = week_id[n];
    int th = home_id[n];
    int ta = away_id[n];

    real mu = team_strength[s][w][th] - team_strength[s][w][ta];
    if (is_true_home[n] == 1) {
      mu += team_hfa[th];
    }

    log_lik[n]   = normal_lpdf(result[n] | mu, sigma_game);
    result_rep[n] = normal_rng(mu, sigma_game);
  }
}
