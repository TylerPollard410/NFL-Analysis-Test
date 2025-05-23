# dependencies
library(dplyr)
library(tibble)

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# --- 1. Setup global constants and indices ----------------------------------
teams      <- sort(unique(c(modData$home_team, modData$away_team)))
n_teams    <- length(teams)
team_index <- setNames(seq_len(n_teams), teams)

# state vector: team strengths (1:n_teams), global HFA (n_teams+1)
state_dim <- n_teams + 1
hfa_index <- state_dim

# noise hyperparameters
Q_team <- 0.01     # process noise for team strength
Q_hfa  <- 0.001    # process noise for HFA
R_obs  <- 1        # observation noise variance
W_mat  <- diag(c(rep(Q_team, n_teams), Q_hfa))

# season carryover parameters
phi                  <- 0.8
season_var_inflation <- 5

# --- 2. Initialization ------------------------------------------------------
init_state <- function() {
  list(
    m           = rep(0, state_dim),       # initial means
    C           = diag(1e3, state_dim),     # initial covariance
    season_prev = NA                        # track previous season
  )
}

# --- 3. One-week update function -------------------------------------------
update_week <- function(week_data, state) {
  m <- state$m
  C <- state$C
  first_game <- week_data[1, ]
  # Apply season carryover if new season begins
  if (!identical(state$season_prev, first_game$season)) {
    message("Starting season ", first_game$season, " carryover...")
    m <- phi * m
    C <- C + diag(season_var_inflation, state_dim)
  }
  # Predict step once per week
  m_pred <- m
  C_pred <- C + W_mat
  
  # Inform progress 
  message("  Processing Season ", first_game$season, " Week ", first_game$week, " with ", nrow(week_data), " games...")
  
  # Container for this week's KFA rows
  kf_rows <- vector("list", nrow(week_data))
  
  # Record pre-week ratings
  for (j in seq_len(nrow(week_data))) {
    g <- week_data[j, ]
    hfa_pre <- ifelse(g$location == "Home", m_pred[hfa_index], 0)
    kf_rows[[j]] <- tibble(
      game_id         = g$game_id,
      season          = g$season,
      week            = g$week,
      home_team       = g$home_team,
      away_team       = g$away_team,
      location        = g$location,
      home_rating_pre = m_pred[team_index[g$home_team]],
      away_rating_pre = m_pred[team_index[g$away_team]],
      hfa_pre         = hfa_pre,
      home_rating     = NA_real_,
      away_rating     = NA_real_,
      hfa             = NA_real_
    )
  }
  
  # Sequential update for each game
  m_tmp <- m_pred
  C_tmp <- C_pred
  for (j in seq_len(nrow(week_data))) {
    g <- week_data[j, ]
    Fg <- numeric(state_dim)
    Fg[team_index[g$home_team]] <- 1
    Fg[team_index[g$away_team]] <- -1
    if (g$location == "Home") Fg[hfa_index] <- 1
    
    # Update step
    y <- g$result
    v <- y - as.numeric(Fg %*% m_tmp)
    S <- as.numeric(Fg %*% C_tmp %*% Fg + R_obs)
    K <- (C_tmp %*% Fg) / S
    
    m_tmp <- as.numeric(m_tmp + K * v)
    C_tmp <- C_tmp - K %*% t(Fg) %*% C_tmp
    
    # Fill post-game ratings
    hfa_val <- ifelse(g$location == "Home", m_tmp[hfa_index], 0)
    kf_rows[[j]] <- kf_rows[[j]] |>
      mutate(
        home_rating = m_tmp[team_index[g$home_team]],
        away_rating = m_tmp[team_index[g$away_team]],
        hfa         = hfa_val
      )
  }
  
  # Return tibble and updated state
  kfa_week <- bind_rows(kf_rows)
  new_state <- list(m = m_tmp, C = C_tmp, season_prev = first_game$season)
  list(kfa = kfa_week, state = new_state)
}

# --- 4. Batch-run historical filter for any data subset -------------------
batch_run <- function(data) {
  state   <- init_state()
  kfa_acc <- list()
  weekly  <- data |>
    group_by(season, week) |>
    group_split()
  total_weeks <- length(weekly)
  
  message("Starting batch run on ", total_weeks, " week-groups...")
  for (i in seq_along(weekly)) {
    week_data <- weekly[[i]]
    message(sprintf("[%d/%d] ", i, total_weeks))
    out      <- update_week(week_data, state)
    kfa_acc  <- append(kfa_acc, list(out$kfa))
    state    <- out$state
  }
  
  final_kfa <- bind_rows(kfa_acc)
  attr(final_kfa, "state") <- state
  message("Batch run complete. Processed ", nrow(final_kfa), " games.")
  final_kfa
}

# --- 5. Rolling window forecasting with optional re-computation ----------
forecast_list <- list()
from_scratch <- TRUE  # set to TRUE to re-run filter from scratch each week
modData2 <- modData 

# Initialize training window on 2007-2009
train_init_season_start <- 2006
train_init_season_end <- 2008
train_init_data <- modData |> filter(season >= train_init_season_start, 
                                     season <= train_init_season_end)
message("Initializing with data from ", train_init_season_start, "-", train_init_season_end, " (", nrow(train_init_data), " games)...")
batch_init <- batch_run(train_init_data)
state         <- attr(batch_init, "state")
train_window  <- batch_init |>
  select(game_id, season, week, home_team, away_team, location,
         home_rating_pre, away_rating_pre, hfa_pre)

# Iterative forecasting for seasons 2010-2024
forecast_season_start <- 2009
forecast_season_end   <- 2024
i <- 1
total_forecasts <- sum(sapply(forecast_season_start:forecast_season_end,
                              function(y) length(unique(modData$week[modData$season == y]))))
message("Beginning forecasting for ", forecast_season_start, "-", forecast_season_end,
        " (", total_forecasts, " forecast points)...")

for (yr in forecast_season_start:forecast_season_end) {
  wk_list <- sort(unique(modData$week[modData$season == yr]))
  for (wk in wk_list) {
    key <- paste0("S", yr, "_W", wk)
    message(sprintf("Forecast point %d/%d: %s", i, total_forecasts, key))
    week_data <- modData |>
      filter(season == yr, week == wk)
    
    if (from_scratch) {
      # Recompute filter on all data before this week
      train_data <- modData |>
        filter(season < yr | (season == yr & week < wk))
      batch_out <- batch_run(train_data)
      train_feats <- batch_out |>
        select(game_id, season, week, home_team, away_team, location,
               home_rating_pre, away_rating_pre, hfa_pre)
      state_use <- attr(batch_out, "state")
    } else {
      train_feats <- train_window
      state_use   <- state
    }
    
    # Update filter for this week
    out      <- update_week(week_data, state_use)
    kfa_week <- out$kfa
    new_state <- out$state
    
    test_feats <- kfa_week |>
      select(game_id, season, week, home_team, away_team, location,
             home_rating_pre, away_rating_pre, hfa_pre)
    
    # Store results
    forecast_list[[key]] <- list(
      train = train_feats,
      test  = test_feats,
      state = new_state
    )
    
    if (!from_scratch) {
      # Append for incremental mode
      train_window <- bind_rows(train_window, test_feats)
      state        <- new_state
    }
    
    i <- i + 1
  }
}
