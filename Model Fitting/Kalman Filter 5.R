# team_strength_hfa_model.R
#
# Mimic Glicko Chapter Section 4: dynamic team strength + global home-field advantage
# State-space Kalman filter with ridge penalty and sum-to-zero constraint on strengths
# Batched by week on gameData (season, week, home_team, away_team, result, location)

library(dplyr)
library(KFAS)
library(tibble)

# 0. Initialization: teams and state dimensions
teams        <- sort(unique(c(gameData$home_team, gameData$away_team)))
n_teams      <- length(teams)
# state vector: [theta_1,...,theta_J, home_field_advantage]
state_dim    <- n_teams + 1
strength_idx <- seq_len(n_teams)
hfa_idx      <- state_dim
team_index   <- setNames(seq_len(n_teams), teams)

# 1. Noise covariances (ridge penalties) based on observed variance
var_y            <- var(gameData$result)
lambda_strength  <- var_y * 0.01   # variance penalty for team strength
lambda_hfa       <- var_y * 0.001  # variance penalty for global home-field advantage
R_obs            <- var_y * 0.5    # measurement noise variance

W_mat <- diag(c(rep(lambda_strength, n_teams), lambda_hfa))

# 2. Build SSModel for a batch of games (one week slice)
build_model <- function(df, init = NULL) {
  n <- nrow(df)
  # design array Z: 1 x state_dim x n
  Z_arr <- array(0, dim = c(1, state_dim, n))
  for (i in seq_len(n)) {
    ht    <- df$home_team[i]
    at    <- df$away_team[i]
    # team strength contributions
    Z_arr[1, strength_idx[team_index[ht]], i] <- 1
    Z_arr[1, strength_idx[team_index[at]], i] <- -1
    # global home-field advantage covariate
    Z_arr[1, hfa_idx,                    i] <- as.integer(df$location[i] == "Home")
  }
  if (is.null(init)) {
    SSModel(
      df$result ~ -1 + SSMcustom(
        Z  = Z_arr,
        T  = diag(state_dim),
        R  = diag(state_dim),
        Q  = W_mat,
        P1 = diag(1e6, state_dim)
      ),
      H = R_obs
    )
  } else {
    SSModel(
      df$result ~ -1 + SSMcustom(
        Z  = Z_arr,
        T  = diag(state_dim),
        R  = diag(state_dim),
        Q  = W_mat,
        a1 = init$m,
        P1 = init$C
      ),
      H = R_obs
    )
  }
}

# 3. Enforce sum-to-zero: recenter strengths in predicted and filtered states
recenter_states <- function(m) {
  theta_vals    <- m[strength_idx]
  mean_theta    <- mean(theta_vals)
  m[strength_idx] <- theta_vals - mean_theta
  m
}

# 4. Run KFAS for one-week batch, return pre/post ratings and updated state
run_week_kfas <- function(df, init = NULL) {
  # Seasonal carryover: reduce past state and inflate uncertainty on new season
  if (!is.null(init) && init$season_prev != df$season[1]) {
    init$m <- init$m * 0.8
    init$C <- init$C + diag(var_y * 0.1, state_dim)
  }
  model <- build_model(df, init)
  kf    <- KFS(model, filtering = "state", smoothing = "none")
  
  # Pre-game state vector
  m_pred <- if (is.null(init)) rep(0, state_dim) else init$m
  m_pred <- recenter_states(m_pred)
  
  # Filtered states per observation
  att <- kf$att
  for (i in seq_len(nrow(att))) {
    att[i, ] <- recenter_states(att[i, ])
  }
  
  # Assemble results per game
  results <- map_dfr(seq_len(nrow(df)), function(i) {
    g    <- df[i, ]
    pre  <- m_pred
    post <- att[i, ]
    ht_i <- team_index[g$home_team]
    at_i <- team_index[g$away_team]
    tibble(
      game_id      = g$game_id,
      season       = g$season,
      week         = g$week,
      home_team    = g$home_team,
      away_team    = g$away_team,
      # pre-game ratings
      home_str_pre = pre[strength_idx[ht_i]],
      away_str_pre = pre[strength_idx[at_i]],
      hfa_pre      = pre[hfa_idx],
      # post-game ratings
      home_str     = post[strength_idx[ht_i]],
      away_str     = post[strength_idx[at_i]],
      hfa          = post[hfa_idx]
    )
  })
  
  new_state <- list(
    m           = att[nrow(df), ],
    C           = kf$Ptt[,,nrow(df)],
    season_prev = df$season[1]
  )
  list(results = results, state = new_state)
}

# 5. Loop over all weeks and collect results
all_weeks   <- gameData %>% group_by(season, week) %>% group_split(.keep = TRUE)
kf_results  <- list()
init_state  <- NULL
for (wk in all_weeks) {
  out            <- run_week_kfas(wk, init_state)
  key            <- sprintf("S%d_W%02d", wk$season[1], wk$week[1])
  kf_results[[key]] <- out$results
  init_state     <- out$state
}

# 6. Combine into training and test datasets
training_data   <- bind_rows(kf_results)
forecast_state  <- recenter_states(init_state$m)
# One-step ahead forecast for each team
test_data       <- tibble(
  team              = teams,
  strength_forecast = forecast_state[strength_idx],
  hfa_forecast      = forecast_state[hfa_idx]
)

list(training_data = training_data, test_data = test_data)
