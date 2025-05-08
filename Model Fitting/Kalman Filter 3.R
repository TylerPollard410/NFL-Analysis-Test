# --- Kalman Filter via KFAS (formula interface with init) ---
# Weekly pre-ratings constant per week, optional full recompute vs incremental, plus progress messages.

# 0. Dependencies
library(dplyr)
library(KFAS)
library(tibble)

# 1. Load data and constants
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
teams      <- sort(unique(c(modData$home_team, modData$away_team)))
n_teams    <- length(teams)
team_index <- setNames(seq_len(n_teams), teams)
state_dim  <- n_teams + 1
hfa_index  <- state_dim

# Noise covariances and season carryover
Q_team <- 0.01; Q_hfa <- 0.001; R_obs <- 1
W_mat  <- diag(c(rep(Q_team, n_teams), Q_hfa))
phi                  <- 0.8
season_var_inflation <- 5

# 2. Build weekly SSModel (formula + SSMcustom), with optional init a1/P1
build_model <- function(df, init = NULL) {
  n <- nrow(df)
  Z_arr <- array(0, dim = c(1, state_dim, n))
  for (i in seq_len(n)) {
    g <- df[i, ]
    row <- numeric(state_dim)
    row[team_index[g$home_team]] <- 1
    row[team_index[g$away_team]] <- -1
    if (g$location == "Home") row[hfa_index] <- 1
    Z_arr[1, , i] <- row
  }
  # Build SSModel with or without initial state
  if (is.null(init)) {
    SSModel(
      df$result ~ -1 + SSMcustom(
        Z = Z_arr,
        T = diag(state_dim),
        R = diag(state_dim),
        Q = W_mat
      ),
      H = R_obs
    )
  } else {
    SSModel(
      df$result ~ -1 + SSMcustom(
        Z = Z_arr,
        T = diag(state_dim),
        R = diag(state_dim),
        Q = W_mat,
        a1 = init$m,
        P1 = init$C
      ),
      H  = R_obs
    )
  }
}

# 3. Filter one week, preserving weekly m_pred
run_kfas <- function(df, init = NULL) {
  n <- nrow(df)
  # Apply season carryover if new season
  if (!is.null(init) && !identical(init$season_prev, df$season[1])) {
    message("Applying season carryover for season ", df$season[1])
    init$m <- phi * init$m
    init$C <- init$C + diag(season_var_inflation, state_dim)
  }
  slice_info <- paste0("rows: ", n,
                       if (n>0) paste0(", season: ", df$season[1], " wk: ", df$week[1]) else "")
  message("[KFAS] Building SSModel for ", slice_info)
  
  # Build model with proper init
  model <- build_model(df, init)
  
  # Run filter
  message(if (is.null(init))
    "[KFAS] Running KFS (initial week)..." else
      "[KFAS] Running KFS with provided initial a1/P1...")
  kf <- KFS(model, filtering = "state", smoothing = "none")
  
  # m_pred (weekly pre-state)
  m_pred <- if (is.null(init)) rep(0, state_dim) else init$m
  post_states <- kf$att  # filtered states for each observation
  
  # Build per-game results: constant m_pred, then post_states
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    g   <- df[i, ]
    ups <- post_states[i, ]
    hfa_pre  <- ifelse(g$location == "Home", m_pred[hfa_index], 0)
    hfa_post <- ifelse(g$location == "Home", ups[hfa_index], 0)
    rows[[i]] <- tibble(
      game_id         = g$game_id,
      season          = g$season,
      week            = g$week,
      home_team       = g$home_team,
      away_team       = g$away_team,
      location        = g$location,
      home_rating_pre = m_pred[team_index[g$home_team]],
      away_rating_pre = m_pred[team_index[g$away_team]],
      hfa_pre         = hfa_pre,
      home_rating     = ups[team_index[g$home_team]],
      away_rating     = ups[team_index[g$away_team]],
      hfa             = hfa_post
    )
  }
  result <- bind_rows(rows)
  
  # New state from last filtered
  new_state <- list(
    m           = post_states[n, ],
    C           = kf$Ptt[, , n],
    season_prev = if (n>0) df$season[1] else init$season_prev
  )
  message("[KFAS] Completed filtering for ", slice_info)
  list(kfa = result, state = new_state, kf = kf)
}

# 4. Rolling forecast with configurable ranges
forecast_list <- list()
from_scratch            <- TRUE
initial_season_start    <- 2006
initial_season_end      <- 2008
forecast_season_start   <- 2024
forecast_season_end     <- 2024

# Compute total forecast points
total_pts <- sum(sapply(forecast_season_start:forecast_season_end,
                        function(y) length(unique(modData$week[modData$season==y]))))

# Initial batch per week
i <- 1
train_win  <- tibble()
current_st <- NULL

message("[Progress] Initial batch on ", initial_season_start, "-", initial_season_end)
init_weeks <- modData %>%
  filter(season >= initial_season_start, season <= initial_season_end) %>%
  group_by(season, week) %>% group_split()
for (wk in init_weeks) {
  message(sprintf("[Progress] Initial week %d_%d (%d/?)", wk$season[1], wk$week[1], i))
  out <- run_kfas(wk, current_st)
  train_win <- bind_rows(train_win, out$kfa %>%
                           select(game_id, season, week, home_team, away_team, location,
                                  home_rating_pre, away_rating_pre, hfa_pre,
                                  home_rating, away_rating, hfa))
  current_st <- out$state
  i <- i + 1
}
message("[Progress] Initial batch complete. Starting forecasts...")

# Forecast loop
message("[Progress] Beginning forecasts...")
i <- 1
train_win  <- tibble()
current_st <- NULL
for (yr in forecast_season_start:forecast_season_end) {
  weeks <- sort(unique(modData$week[modData$season==yr]))
  for (wk in weeks) {
    key <- paste0("S", yr, "_W", wk)
    message(sprintf("[Progress] (%d/%d) Forecast point %s", i, total_pts, key))
    dfw <- modData %>% filter(season==yr, week==wk)
    if (from_scratch) {
      message("[Progress] Recomputing from scratch up to ", yr, "_", wk)
      hist <- modData %>% filter(season<yr | (season==yr & week<wk))
      out_h <- run_kfas(hist, NULL)
      train_win <- out_h$kfa %>%
        select(game_id, season, week, home_team, away_team, location,
               home_rating_pre, away_rating_pre, hfa_pre,
               home_rating, away_rating, hfa)
      st_use <- out_h$state
    } else {
      st_use <- current_st
    }
    out <- run_kfas(dfw, st_use)
    test_win <- out$kfa %>%
      select(game_id, season, week, home_team, away_team, location,
             home_rating_pre, away_rating_pre, hfa_pre,
             home_rating, away_rating, hfa)
    forecast_list[[key]] <- list(train= train_win, test= test_win, state= out$state)
    if (!from_scratch) {
      train_win  <- bind_rows(train_win, test_win)
      current_st <- out$state
    }
    i <- i + 1
  }
}
message("[Progress] Forecasting complete: ", length(forecast_list), " points generated.")
