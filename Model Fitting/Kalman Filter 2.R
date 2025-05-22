# dependencies
library(dplyr)
library(tibble)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TESTING SETUP ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# 1. Prepare training data: exclude 2024 and sort
trainData <- modData #|> 
  #filter(season < 2024) |> 
  arrange(season, week, game_id)

# 2. Build team index and define state dimension
teams <- sort(unique(c(trainData$home_team, trainData$away_team)))
n_teams <- length(teams)
team_index <- setNames(seq_len(n_teams), teams)

# state dimension = team strengths + global HFA
state_dim <- n_teams + 1
hfa_index <- state_dim

# 3. Hyperparameters
Q_team <- 0.01              # process noise variance for team ratings
Q_hfa <- 0.001              # process noise variance for HFA
R_obs <- 1                  # observation noise variance
W_mat <- diag(c(rep(Q_team, n_teams), Q_hfa))

# 4. Partial season carryover parameters
phi <- 0.8                  # carryover fraction at season start
season_var_inflation <- 5   # added uncertainty at season boundary

# 5. Initialize filter state
m <- rep(0, state_dim)            # initial state means
C <- diag(1e3, state_dim)         # initial state covariance

# 6. Run Kalman filter by season-week, freezing pre-week ratings
groups_by_week <- trainData |> group_by(season, week) |> group_split()
kf_rows <- vector("list", nrow(trainData))
row_counter <- 1

for (week_data in groups_by_week) {
  first_game <- week_data[1, ]
  # season boundary carryover
  if (row_counter == 1 || first_game$season != trainData$season[row_counter - 1]) {
    m <- phi * m
    C <- C + diag(season_var_inflation, state_dim)
  }
  
  # predict step once per week
  m_pred0 <- m
  C_pred0 <- C + W_mat
  
  # store pre-week ratings for all games
  for (j in seq_len(nrow(week_data))) {
    g <- week_data[j, ]
    hfa_pre <- ifelse(g$location == "Home", m_pred0[hfa_index], 0)
    kf_rows[[row_counter]] <- tibble(
      game_id = g$game_id,
      season = g$season,
      week = g$week,
      home_team = g$home_team,
      away_team = g$away_team,
      location = g$location,
      home_rating_pre = m_pred0[team_index[g$home_team]],
      away_rating_pre = m_pred0[team_index[g$away_team]],
      hfa_pre = hfa_pre,
      home_rating = NA_real_,
      away_rating = NA_real_,
      hfa = NA_real_
    )
    row_counter <- row_counter + 1
  }
  
  # sequential updates within week
  m_temp <- m_pred0
  C_temp <- C_pred0
  for (j in seq_len(nrow(week_data))) {
    g <- week_data[j, ]
    Fg <- numeric(state_dim)
    Fg[team_index[g$home_team]] <- 1
    Fg[team_index[g$away_team]] <- -1
    if (g$location == "Home") Fg[hfa_index] <- 1
    
    y <- g$result
    v <- y - as.numeric(crossprod(Fg, m_temp))
    S <- as.numeric(Fg %*% C_temp %*% Fg + R_obs)
    K <- (C_temp %*% Fg) / S
    
    m_temp <- as.numeric(m_temp + K * v)
    C_temp <- C_temp - K %*% t(Fg) %*% C_temp
    
    idx <- row_counter - nrow(week_data) - 1 + j
    hfa_val <- ifelse(g$location == "Home", m_temp[hfa_index], 0)
    kf_rows[[idx]] <- kf_rows[[idx]] |> 
      mutate(
        home_rating = m_temp[team_index[g$home_team]],
        away_rating = m_temp[team_index[g$away_team]],
        hfa = hfa_val
      )
  }
  
  # advance state to end-of-week
  m <- m_temp
  C <- C_temp
}

# 7. Combine into finalKFA and build trainKFA
finalKFA2 <- bind_rows(kf_rows)
trainKFA2 <- finalKFA2 |> 
  select(
    game_id, season, week, home_team, away_team, location,
    home_rating_pre, away_rating_pre, hfa_pre
  )

# 8. Build forecastKFA for 2024 Week 1
forecastKFA <- modData |> 
  filter(season == 2024, week == 1) |> 
  arrange(game_id) |> 
  rowwise() |> 
  mutate(
    home_rating_pre = m[team_index[home_team]],
    away_rating_pre = m[team_index[away_team]],
    hfa_pre = ifelse(location == "Home", m[hfa_index], 0)
  ) |> 
  select(
    game_id, season, week, home_team, away_team, location,
    home_rating_pre, away_rating_pre, hfa_pre
  ) |> 
  ungroup()

finalKFA_setup <- finalKFA
trainKFA_setup <- trainKFA
forecastKFA_setup <- forecastKFA

rm(list = ls()[!(ls() %in% ls(pattern = "setup"))])


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FUNCTION SETUP ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# dependencies
library(dplyr)
library(tibble)

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
R_obs  <- 1        # observation noise
W_mat  <- diag(c(rep(Q_team, n_teams), Q_hfa))

# season carryover parameters
phi                   <- 0.8
season_var_inflation  <- 5

# --- 2. Initialization ------------------------------------------------------
init_state <- function() {
  list(
    m = rep(0, state_dim),       # initial means
    C = diag(1e3, state_dim),     # initial covariance
    season_prev = NA              # track previous season
  )
}

# --- 3. One-week update function -------------------------------------------
# Applies Kalman filter to a single week of games, producing pre- and post-
# ratings and returning a list(kfa, state).
update_week <- function(week_data, state) {
  m <- state$m
  C <- state$C
  
  # season carryover if new season
  first_game <- week_data[1, ]
  if (!identical(state$season_prev, first_game$season)) {
    m <- phi * m
    C <- C + diag(season_var_inflation, state_dim)
  }
  
  # predict step once per week
  m_pred <- m
  C_pred <- C + W_mat
  
  # prepare container for this week's KFA rows
  kf_rows <- vector("list", nrow(week_data))
  
  # 3a. Record pre-week ratings for each game
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
  
  # 3b. Sequentially update filter for each game in this week
  m_tmp <- m_pred
  C_tmp <- C_pred
  for (j in seq_len(nrow(week_data))) {
    g <- week_data[j, ]
    Fg <- numeric(state_dim)
    Fg[team_index[g$home_team]] <- 1
    Fg[team_index[g$away_team]] <- -1
    if (g$location == "Home") {
      Fg[hfa_index] <- 1
    }
    
    y <- g$result
    v <- y - as.numeric(Fg %*% m_tmp)
    S <- as.numeric(Fg %*% C_tmp %*% Fg + R_obs)
    K <- (C_tmp %*% Fg) / S
    
    m_tmp <- as.numeric(m_tmp + K * v)
    C_tmp <- C_tmp - K %*% t(Fg) %*% C_tmp
    
    # Fill post-game ratings back into kf_rows
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
  new_state <- list(
    m           = m_tmp,
    C           = C_tmp,
    season_prev = first_game$season
  )
  list(kfa = kfa_week, state = new_state)
}

# --- 4. Batch-run historical filter ----------------------------------------
batch_run <- function(data) {
  state <- init_state()
  kfa_accum <- list()
  
  weekly <- data |>
    group_by(season, week) |>
    group_split()
  
  for (week_data in weekly) {
    out <- update_week(week_data, state)
    kfa_accum <- append(kfa_accum, list(out$kfa))
    state <- out$state
  }
  
  final_kfa <- bind_rows(kfa_accum)
  attr(final_kfa, "state") <- state
  final_kfa
}

# --- 5. Prepare train and forecast tibbles --------------------------------
# (A) Historical predictor data for BRMS and final state
batch_res <- batch_run(modData |> filter(season < 2024))
finalKFA  <- batch_res
state     <- attr(batch_res, "state")
trainKFA  <- finalKFA |> select(game_id:location, home_rating_pre, away_rating_pre, hfa_pre)

# (B) Initial forecast for 2024 Week 1
week1_2024 <- modData |> filter(season == 2024, week == 1)
out_week1  <- update_week(week1_2024, state)
forecastKFA <- out_week1$kfa |> select(game_id:location, home_rating_pre, away_rating_pre, hfa_pre)

# You can now iteratively call update_week() for each new week,
# passing in the latest 'state' from out$state and appending the returned kfa to trainKFA.



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ITERATE ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  
  # Container for this week's KFA
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
  
  # Sequentially update for each game
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

# --- 5. Rolling window from 2007-2009 through 2024 -------------------------
forecast_list <- list()

# Initialize with training on 2007-2009
train_init_season_start <- 2006
train_init_season_end <- 2008
train_init_data <- modData |> filter(season >= train_init_season_start, 
                                     season <= train_init_season_end)
message("Initializing with data from ", train_init_season_start, "-", train_init_season_end, " (", nrow(train_init_data), " games)...")
batch_init_data <- batch_run(train_init_data)
state           <- attr(batch_init_data, "state")
trainKFA_window <- batch_init_data |>
  select(game_id, season, week, home_team, away_team, location,
         home_rating_pre, away_rating_pre, hfa_pre)

# Iterate seasons 2010–2024
# Total number of forecast points across seasons 2010–2024
forecast_season_start <- 2009
forecast_season_end <- 2024
total_forecasts <- sum(sapply(forecast_season_start:forecast_season_end, function(y) length(unique(modData$week[modData$season==y]))))
message("Beginning iterative forecasting for ", forecast_season_start, "-", forecast_season_end, " (", total_forecasts, " forecast points)...")

# Counter for progress
i <- 1
for (yr in forecast_season_start:forecast_season_end) {
  wk_list <- sort(unique(modData$week[modData$season == yr]))
  for (wk in wk_list) {
    key <- paste0("S", yr, "_W", wk)
    message(sprintf("Forecast point %d/%d: %s", i, total_forecasts, key))
    
    # Kalman update for this week
    week_data <- modData |> filter(season == yr, week == wk)
    out       <- update_week(week_data, state)
    kfa_week  <- out$kfa
    state     <- out$state
    
    # Extract test features
    test_feats <- kfa_week |>
      select(game_id, season, week, home_team, away_team, location,
             home_rating_pre, away_rating_pre, hfa_pre)
    
    # Store snapshot
    forecast_list[[key]] <- list(
      train = trainKFA_window,
      test  = test_feats,
      state = state
    )
    
    # Append to training window
    trainKFA_window <- bind_rows(trainKFA_window, test_feats)
    
    # Increment progress counter
    i <- i + 1
  }
}

# Final completion message
message("Forecast list construction complete: ", length(forecast_list), " entries.")





# 'forecast_list' now contains named elements like 'S2010_W1', each with:
#   - $train: the KFA training tibble up to (but not including) that week
#   - $test : the pre-game features for that week’s slate
#   - $state: the Kalman state after processing that week


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NEW ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  # season carryover if new season begins
  if (!identical(state$season_prev, first_game$season)) {
    message("Season carryover: ", first_game$season)
    m <- phi * m
    C <- C + diag(season_var_inflation, state_dim)
  }
  # predict step once per week
  m_pred <- m
  C_pred <- C + W_mat
  
  message(sprintf("Processing Season %d Wk %d (%d games)",
                  first_game$season, first_game$week, nrow(week_data)))
  
  # container for this week's KFA
  kf_rows <- vector("list", nrow(week_data))
  
  # record pre-week ratings
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
  
  # sequentially update for each game
  m_tmp <- m_pred
  C_tmp <- C_pred
  for (j in seq_len(nrow(week_data))) {
    g  <- week_data[j, ]
    Fg <- numeric(state_dim)
    Fg[team_index[g$home_team]] <- 1
    Fg[team_index[g$away_team]] <- -1
    if (g$location == "Home") Fg[hfa_index] <- 1
    
    y <- g$result
    v <- y - as.numeric(Fg %*% m_tmp)
    S <- as.numeric(Fg %*% C_tmp %*% Fg + R_obs)
    K <- (C_tmp %*% Fg) / S
    
    m_tmp <- as.numeric(m_tmp + K * v)
    C_tmp <- C_tmp - K %*% t(Fg) %*% C_tmp
    
    hfa_val <- ifelse(g$location == "Home", m_tmp[hfa_index], 0)
    kf_rows[[j]] <- kf_rows[[j]] |>
      mutate(
        home_rating = m_tmp[team_index[g$home_team]],
        away_rating = m_tmp[team_index[g$away_team]],
        hfa         = hfa_val
      )
  }
  
  # return this week's KFA and updated state
  list(
    kfa   = bind_rows(kf_rows),
    state = list(m = m_tmp, C = C_tmp, season_prev = first_game$season)
  )
}

# --- 4. Batch-run for initial window (2006-2008) ---------------------------
train_init_data <- modData |> 
  filter(season >= 2006, season <= 2008) #|> 
  #arrange(season, week, game_id)

batch_res <- {
  state <- init_state()
  kfa_acc <- list()
  weekly <- train_init_data |> group_by(season, week) |> group_split()
  for (wd in weekly) {
    out <- update_week(wd, state)
    kfa_acc <- append(kfa_acc, list(out$kfa))
    state <- out$state
  }
  final_kfa <- bind_rows(kfa_acc)
  attr(final_kfa, "state") <- state
  final_kfa
}

trainKFA_window <- batch_res |> 
  select(game_id, season, week, home_team, away_team, location,
         home_rating_pre, away_rating_pre, hfa_pre,
         home_rating, away_rating, hfa)
state <- attr(batch_res, "state")

# --- 5. Iterate from 2009 week 1 until current -----------------------------
all_tests <- list()
seasons  <- sort(unique(modData$season))
years_to_run <- seasons[seasons >= 2009]

for (yr in years_to_run) {
  wk_list <- sort(unique(modData$week[modData$season == yr]))
  for (wk in wk_list) {
    key <- paste0("S", yr, "_W", wk)
    week_data <- modData |> filter(season == yr, week == wk)
    out <- update_week(week_data, state)
    kfa_week <- out$kfa
    state <- out$state
    
    # collect test features
    test_feats <- kfa_week |> 
      select(game_id, season, week, home_team, away_team, location,
             home_rating_pre, away_rating_pre, hfa_pre,
             home_rating, away_rating, hfa)
    all_tests <- append(all_tests, list(test_feats))
    
    # append to training window
    trainKFA_window <- bind_rows(trainKFA_window, test_feats)
  }
}

# --- 6. Construct and save kfaData -----------------------------------------
# Single list with final training set and all test sets
kfaData <- list(
  train = trainKFA_window,
  test  = bind_rows(all_tests)
)

save(kfaData, file = "./app/data/kfaData.rda")
message("kfaData saved: training rows = ", nrow(kfaData$train),
        "; test rows = ", nrow(kfaData$test))


