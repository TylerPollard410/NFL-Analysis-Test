# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(tictoc)
# library(tidytext)
# library(MASS)
library(plotly)
library(smplot2)
library(patchwork)
# library(doParallel)
# library(rBayesianOptimization)
# library(xgboost)
# library(caret)
library(KFAS)
library(cmdstanr)
library(rstan)
library(brms)
library(posterior)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
#library(vip)
library(broom.mixed)
library(tidybayes)
#library(discrim)
#library(bayesian)
#library(timetk)
#library(modeltime)
#library(tidymodels)

library(nflverse)
library(tidyverse)

library(nflendzonePipeline)
library(nflendzone)

set.seed(52)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 1. DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Global Variables ----
all_seasons <- 2006:get_current_season()
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
github_data_repo <- "TylerPollard410/nflendzoneData"

## nflverse ----
teams_data <- load_teams(current = TRUE)
teams <- teams_data$team_abbr

### games ----
game_data <- load_game_data()
game_data_long <- game_data |> clean_homeaway(invert = c("result", "spread_line"))

game_id_keys <- game_data |> select(
  game_id, season, game_type, season_type, week, home_team, away_team, location
)
game_long_id_keys <- game_data_long |> select(
  game_id, season, game_type, season_type, week, team, opponent, location
)

### release data ----
tag <- "game_features"
game_features_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "game_model"
game_model_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "team_features"
team_features_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "team_model"
team_model_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "nfl_stats_week_team_regpost"
nfl_stats_week_team_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "nfl_stats_week_player_regpost"
nfl_stats_week_player_regpost <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

tag <- "srs"
srs_data <- rds_from_url(paste0(base_repo_url, tag, "/", tag, ".rds"))

## Set up modeling data ----
#   - Use seasons 2007–2023 for training/validation
#   - Hold out seasons 2024 for out of sample weekly forecats
game_model_data <- game_model_data |>
  mutate(
    hfa = ifelse(location == "Home", 1, 0)
  )

team_model_data <- team_model_data |>
  mutate(
    hfa = case_when(
      location == "home" ~ 1,
      location == "away" ~ -1,
      location == "neutral" ~ 0,
      TRUE ~ NA
    )
  )


game_fit_data_all <- game_model_data |>
  mutate(
    home_id = match(home_team, teams),
    away_id = match(away_team, teams),
    .after = away_team
  ) |>
  mutate(
    season_idx = as.integer(as.factor(season)),
    .after = season
  ) |>
  select(
    game_id, season, season_idx, week, week_idx = week_seq, 
    game_type, season_type,
    home_team, away_team, home_id, away_id,
    location, hfa,
    home_score, away_score, 
    result, spread_line, 
    home_spread_odds, away_spread_odds, 
    home_spread_prob, away_spread_prob,
    total, total_line,
    over_odds, under_odds,
    over_prob, under_prob,
    winner,
    home_moneyline, away_moneyline,
    home_moneyline_prob, away_moneyline_prob
  )

team_fit_data_all <- game_fit_data_all |>
  clean_homeaway(invert = c("result", "spread_line", "hfa")) 

game_fit_data <- game_fit_data_all |>
  filter(!is.na(result))


# Unique week table
week_tbl <- game_fit_data_all |>
  select(season, season_idx, week, week_idx) |>
  distinct() |>
  arrange(week_idx)

season_breaks <- game_model_data |>
  rename(week_idx = week_seq) |>
  group_by(season) |>
  slice_min(week_idx, with_ties = FALSE) |>
  arrange(week_idx) |>
  select(season, week_idx)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 2. MODEL KFA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# ---------- 1) Helpers ----------
# Build an orthonormal basis for the sum-to-zero subspace in R^N.
orthonormal_sumzero_basis <- function(N_teams) {
  # Start with I_N and drop the mean direction, then orthonormalize
  M <- diag(N_teams) - matrix(1 / N_teams, N_teams, N_teams)
  # Get an orthonormal basis for col(M); use QR
  Q <- qr.Q(qr(M))
  # The last column of Q spans the mean direction; drop it.
  # Depending on QR, mean direction may be anywhere; safer to explicitly remove 1-vector projection
  # Construct basis by Gram-Schmidt against 1-vector:
  one <- rep(1 / sqrt(N_teams), N_teams)
  # Project Q columns to be orthogonal to 'one'
  Qp <- apply(Q, 2, function(col) col - sum(col * one) * one)
  # Remove near-zero columns and orthonormalize again
  Q2 <- qr.Q(qr(Qp))
  # Keep first N_teams-1 columns
  Q2[, 1:(N_teams - 1), drop = FALSE]
}

# Make per-week lists: games in each week and their indices
split_games_by_week <- function(df, N_weeks) {
  split(seq_len(nrow(df)), df$week_id)[as.character(seq_len(N_weeks))]
}

# Build a block selection row for team-strength subspace: (e_i - e_j)^T * P
design_row_strength <- function(i, j, P) {
  # i, j are 1-based team IDs
  ei <- rep(0, nrow(P)); ei[i] <- 1
  ej <- rep(0, nrow(P)); ej[j] <- 1
  as.numeric((ei - ej) %*% P)  # 1 x (N_teams-1)
}

# Centering of team-HFA deviations around league HFA is handled in expectation via Z;
# stochasticity enters through a "random restart" at the first week of a season.

logit <- function(x) log(x / (1 - x))
inv_logit <- function(x) 1 / (1 + exp(-x))

# ---------- 2) Model builder given parameters ----------
# Put this near your helpers
.ensure_theta_names <- function(theta) {
  expected <- c(
    "beta_w", "log_sigma_w",
    "beta_s", "log_sigma_s",
    "beta_hfa", "log_sigma_hfa",
    "league_init", "log_sigma_y",
    "log_sigma_team_hfa"
  )
  if (is.null(names(theta)) || !all(expected %in% names(theta))) {
    # Assume the order matches your 'init' vector; reattach names
    names(theta) <- expected
  }
  theta
}

# theta = unconstrained parameter vector -> we transform to (0,1) and positive scales
theta_to_params <- function(theta) {
  #theta <- .ensure_theta_names(theta)
  list(
    beta_w        = inv_logit(theta["beta_w"]),
    sigma_w       = exp(theta["log_sigma_w"]),
    beta_s        = inv_logit(theta["beta_s"]),
    sigma_s       = exp(theta["log_sigma_s"]),
    beta_hfa      = inv_logit(theta["beta_hfa"]),
    sigma_hfa     = exp(theta["log_sigma_hfa"]),
    league_init   = theta["league_init"],
    sigma_y       = exp(theta["log_sigma_y"]),
    sigma_team_hfa= exp(theta["log_sigma_team_hfa"])
  )
}

tidy_params <- function(p) {
  tibble::tibble(
    beta_w        = as.numeric(p$beta_w),
    sigma_w       = as.numeric(p$sigma_w),
    beta_s        = as.numeric(p$beta_s),
    sigma_s       = as.numeric(p$sigma_s),
    beta_hfa      = as.numeric(p$beta_hfa),
    sigma_hfa     = as.numeric(p$sigma_hfa),
    league_init   = as.numeric(p$league_init),
    sigma_y       = as.numeric(p$sigma_y),
    sigma_team_hfa= as.numeric(p$sigma_team_hfa)
  )
}

# Build SSModel (time = week), multivariate Y (games in week)
# Inputs:
# - games: data.frame with columns home_id, away_id, week_id, season_id, hfa (0/1), result
# - first_week_of_season, last_week_of_season: integer vectors length N_seasons
# - N_teams, N_weeks, N_seasons
# - theta: named numeric vector (see theta_to_params names)
build_ssm <- function(games,
                      N_teams, N_weeks, N_seasons,
                      first_week_of_season, last_week_of_season,
                      theta) {
  
  par <- theta_to_params(theta)
  P   <- orthonormal_sumzero_basis(N_teams)     # N_teams x (N_teams-1)
  
  if (anyNA(unlist(par))) {
    stop("Parameter mapping produced NA; check theta naming.", call. = FALSE)
  }
  
  # ---------------- Dimensions ----------------
  # State vector alpha_t structure (by blocks):
  # [ team_strength_subspace (N_teams-1),
  #   team_hfa_dev (N_teams),
  #   league_hfa (1) ]
  dim_strength <- N_teams - 1
  dim_teamhfa  <- N_teams
  dim_league   <- 1
  m <- dim_strength + dim_teamhfa + dim_league
  
  # Observation layout: KFAS expects y as n x p (time x series),
  # and Z as p x m x n. We'll treat each potential game slot in a week
  # as a separate series, padding with NA when a week has fewer games.
  by_week <- split_games_by_week(games, N_weeks)
  dims_y  <- sapply(by_week, function(idx) if (is.null(idx)) 0L else length(idx))
  p <- max(dims_y)
  if (p == 0L) stop("No games per week found.")
  
  # ---------------- Build Y (n x p) ----------------
  Y <- matrix(NA_real_, nrow = N_weeks, ncol = p)
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    Y[t, seq_len(dims_y[t])] <- games$result[idx]
  }
  
  # ---------------- H (p x p or p x p x n) ----------------
  # Use time-invariant diagonal H; KFAS will handle NAs in Y.
  H <- diag(par$sigma_y^2, p)
  if (!all(is.finite(H))) stop("Non-finite values in H; check sigma_y mapping.", call. = FALSE)
  
  # ---------------- Z (p x m x n) ----------------
  Z <- array(0, dim = c(p, m, N_weeks))
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    for (k in seq_len(dims_y[t])) {
      gidx <- idx[k]
      i <- games$home_id[gidx]
      j <- games$away_id[gidx]
      h <- games$hfa[gidx]
      
      # strength block
      Z[k, 1:dim_strength, t] <- design_row_strength(i, j, P)
      
      # team_hfa_dev and league_hfa blocks only contribute for true home
      off <- dim_strength
      if (h == 1L) {
        z_dev <- rep(0, N_teams); z_dev[i] <- 1
        Z[k, (off + 1):(off + N_teams), t] <- z_dev
        Z[k, m, t] <- 1
      }
    }
  }
  
  # ---------------- State evolution (T, R, Q) ----------------
  T_arr <- array(0, dim = c(m, m, N_weeks))
  R_arr <- array(0, dim = c(m, m, N_weeks)) # identity loadings
  Q_arr <- array(0, dim = c(m, m, N_weeks))
  
  I_strength <- diag(dim_strength)
  I_teamhfa  <- diag(dim_teamhfa)
  
  # Map each week to a season index
  season_of_week <- integer(N_weeks)
  for (s in seq_len(N_seasons)) {
    fw <- first_week_of_season[s]
    lw <- last_week_of_season[s]
    season_of_week[fw:lw] <- s
  }
  
  for (t in seq_len(N_weeks)) {
    s <- season_of_week[t]
    fw <- first_week_of_season[s]
    
    # team strength block: AR(1) within season, carryover at season start
    if (t == fw) {
      T_arr[1:dim_strength, 1:dim_strength, t] <- par$beta_s * I_strength
      Q_arr[1:dim_strength, 1:dim_strength, t] <- (par$sigma_s^2) * I_strength
    } else {
      T_arr[1:dim_strength, 1:dim_strength, t] <- par$beta_w * I_strength
      Q_arr[1:dim_strength, 1:dim_strength, t] <- (par$sigma_w^2) * I_strength
    }
    
    # team HFA deviations: restart at season start, static within season
    off <- dim_strength
    if (t == fw) {
      # T block stays zero; Q injects new deviations
      Q_arr[(off + 1):(off + dim_teamhfa), (off + 1):(off + dim_teamhfa), t] <-
        (par$sigma_team_hfa^2) * I_teamhfa
    } else {
      T_arr[(off + 1):(off + dim_teamhfa), (off + 1):(off + dim_teamhfa), t] <- I_teamhfa
    }
    
    # league HFA: AR(1) at season start, identity otherwise
    if (t == fw) {
      T_arr[m, m, t] <- par$beta_hfa
      Q_arr[m, m, t] <- par$sigma_hfa^2
    } else {
      T_arr[m, m, t] <- 1
    }
    
    # R = I so that Q is the shock covariance directly
    R_arr[,,t] <- diag(m)
  }
  
  # ---------------- Initial state ----------------
  a1 <- rep(0, m)
  P1 <- diag(m) * 1e6  # diffuse for most states
  # league init more informative
  a1[m] <- par$league_init
  P1[m, m] <- 2^2
  # team HFA devs proper prior at very first time point
  off <- dim_strength
  P1[(off + 1):(off + dim_teamhfa), (off + 1):(off + dim_teamhfa)] <- par$sigma_team_hfa^2 * I_teamhfa
  
  # ---------------- Build SSModel ----------------
  model <- SSModel(
    Y ~ -1 + SSMcustom(
      Z = Z, T = T_arr, R = R_arr, Q = Q_arr,
      a1 = a1, P1 = P1
    ),
    H = H
  )
  
  attr(model, "P_basis") <- P
  attr(model, "dim_strength") <- dim_strength
  attr(model, "dim_teamhfa") <- dim_teamhfa
  model
}

build_ssm <- function(games,
                      N_teams, N_weeks, N_seasons,
                      first_week_of_season, last_week_of_season,
                      theta) {
  
  par <- theta_to_params(theta)
  stopifnot(!anyNA(unlist(par)))
  
  # ---------- Bases ----------
  # Orthonormal basis P for the sum-to-zero subspace in R^{N_teams}
  # We'll use it for BOTH: (1) team strengths and (2) team-HFA deviations.
  P <- orthonormal_sumzero_basis(N_teams)  # [N_teams x (N_teams-1)]
  
  # ---------- State layout ----------
  ds <- N_teams - 1      # team strength in sum-to-zero subspace
  dh <- N_teams - 1      # team HFA deviations in sum-to-zero subspace
  dl <- 1                # league HFA scalar
  m  <- ds + dh + dl
  
  # ---------- Observation layout ----------
  # KFAS expects y as time x series (n x p) and Z as p x m x n
  by_week <- split(seq_len(nrow(games)), games$week_id)[as.character(seq_len(N_weeks))]
  dims_y  <- sapply(by_week, function(idx) if (is.null(idx)) 0L else length(idx))
  p       <- max(dims_y)
  stopifnot(p > 0)
  
  # Y: n x p with NA padding where a week has fewer than p games
  Y <- matrix(NA_real_, nrow = N_weeks, ncol = p)
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    Y[t, seq_len(dims_y[t])] <- games$result[idx]
  }
  
  # H: constant p x p diagonal (observation noise)
  H <- diag(par$sigma_y^2, p)
  
  # Z: p x m x n
  Z <- array(0, dim = c(p, m, N_weeks))
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    for (k in seq_len(dims_y[t])) {
      gidx <- idx[k]
      i <- games$home_id[gidx]
      j <- games$away_id[gidx]
      h <- games$hfa[gidx]
      
      # --- strength block (difference in team strengths): (e_i - e_j)^T P
      Z[k, 1:ds, t] <- (c(diag(N_teams)[i, ] - diag(N_teams)[j, ]) %*% P)
      
      # --- team HFA deviation block (sum-to-zero subspace):
      # contribution is hfa * dev_i; since dev is in subspace, dev_i = e_i^T P %*% alpha_dev
      off <- ds
      if (h == 1L) {
        Z[k, (off + 1):(off + dh), t] <- (diag(N_teams)[i, ] %*% P)
        # --- league HFA scalar (added only when true home)
        Z[k, m, t] <- 1
      }
    }
  }
  
  # ---------- Robust week -> season mapping ----------
  wk_season <- dplyr::distinct(as.data.frame(games[, c("week_id","season_id")]))
  season_of_week <- rep(NA_integer_, N_weeks)
  season_of_week[wk_season$week_id] <- wk_season$season_id
  stopifnot(!any(is.na(season_of_week)))  # every modeled week must have a season
  
  # Optional consistency check against supplied first/last
  for (s in seq_len(N_seasons)) {
    fw_sup <- first_week_of_season[s]
    lw_sup <- last_week_of_season[s]
    idx_s  <- which(season_of_week == s)
    stopifnot(length(idx_s) > 0, min(idx_s) == fw_sup, max(idx_s) == lw_sup)
  }
  
  # ---------- State evolution ----------
  # T, R, Q: m x m x n
  T_arr <- array(0, dim = c(m, m, N_weeks))
  R_arr <- array(0, dim = c(m, m, N_weeks))
  Q_arr <- array(0, dim = c(m, m, N_weeks))
  
  I_s  <- diag(ds)
  I_h  <- diag(dh)
  
  for (t in seq_len(N_weeks)) {
    s  <- season_of_week[t]
    fw <- first_week_of_season[s]
    
    # Team strength: carry over across seasons with (beta_s, sigma_s),
    # within-season AR(1) with (beta_w, sigma_w)
    if (t == fw) {
      T_arr[1:ds, 1:ds, t] <- par$beta_s * I_s
      Q_arr[1:ds, 1:ds, t] <- (par$sigma_s^2) * I_s
    } else {
      T_arr[1:ds, 1:ds, t] <- par$beta_w * I_s
      Q_arr[1:ds, 1:ds, t] <- (par$sigma_w^2) * I_s
    }
    
    # Team HFA deviations (sum-to-zero subspace):
    # season start -> random restart: T=0, Q = sigma_team_hfa^2 I
    # within season -> static: T=I, Q=0
    off <- ds
    if (t == fw) {
      Q_arr[(off + 1):(off + dh), (off + 1):(off + dh), t] <- (par$sigma_team_hfa^2) * I_h
    } else {
      T_arr[(off + 1):(off + dh), (off + 1):(off + dh), t] <- I_h
    }
    
    # League HFA:
    # AR(1) at season start with (beta_hfa, sigma_hfa), identity (no noise) within season
    if (t == fw) {
      T_arr[m, m, t] <- par$beta_hfa
      Q_arr[m, m, t] <- par$sigma_hfa^2
    } else {
      T_arr[m, m, t] <- 1
    }
    
    # R identity so Q is the shock covariance directly
    R_arr[,,t] <- diag(m)
  }
  
  # ---------- Initial state ----------
  a1 <- rep(0, m)
  P1 <- diag(m) * 1e6                 # diffuse default
  a1[m]  <- par$league_init           # league prior mean
  P1[m,m] <- 2^2                      # sd ≈ 2 as in Stan's diffuse start
  # Proper prior for team-HFA dev at the very first time
  off <- ds
  P1[(off + 1):(off + dh), (off + 1):(off + dh)] <- (par$sigma_team_hfa^2) * I_h
  
  # ---------- Assertions ----------
  stopifnot(
    is.matrix(Y), nrow(Y) == N_weeks, ncol(Y) == p,
    is.array(Z), all(dim(Z) == c(p, m, N_weeks)),
    is.array(T_arr), all(dim(T_arr) == c(m, m, N_weeks)),
    is.array(R_arr), all(dim(R_arr) == c(m, m, N_weeks)),
    is.array(Q_arr), all(dim(Q_arr) == c(m, m, N_weeks)),
    is.matrix(H), all(dim(H) == c(p, p))
  )
  
  # ---------- Build model ----------
  model <- SSModel(
    Y ~ -1 + SSMcustom(
      Z = Z, T = T_arr, R = R_arr, Q = Q_arr,
      a1 = a1, P1 = P1
    ),
    H = H
  )
  
  # Keep bases/dims for downstream reconstruction
  attr(model, "P_basis_strength") <- P
  attr(model, "P_basis_hfa_dev")  <- P
  attr(model, "dim_strength")     <- ds
  attr(model, "dim_teamhfa_sub")  <- dh
  attr(model, "dim_teamhfa_full") <- N_teams
  
  # Back-compat (some of your helpers expect this)
  attr(model, "P_basis")          <- P
  attr(model, "dim_teamhfa")      <- dh
  
  model
}

# ---------- 3) Update function for EM ----------
# theta is a named vector; we rebuild the full model each time.
updatefn <- function(theta, model, ...) {
  theta <- .ensure_theta_names(theta)
  
  # Rebuild with same data attributes we stored outside
  build_ssm(
    games           = attr(model, "games_df"),
    N_teams         = attr(model, "N_teams"),
    N_weeks         = attr(model, "N_weeks"),
    N_seasons       = attr(model, "N_seasons"),
    first_week_of_season = attr(model, "first_week_of_season"),
    last_week_of_season  = attr(model, "last_week_of_season"),
    theta = theta
  )
}

# Progress-aware updatefn factory
updatefn_progress <- function(report_every = 10L) {
  env <- new.env(parent = emptyenv())
  env$eval <- 0L
  env$report_every <- as.integer(report_every)
  
  function(theta, model, ...) {
    #theta <- .ensure_theta_names(theta)
    env$eval <- env$eval + 1L
    
    # Print every N evaluations (avoid spamming)
    if (env$eval %% env$report_every == 0L) {
      p <- theta_to_params(theta)
      cat(sprintf(
        "[fitSSM] eval=%d | beta_w=%.3f beta_s=%.3f beta_hfa=%.3f | sigmas: y=%.3f w=%.3f s=%.3f team_hfa=%.3f hfa=%.3f\n",
        env$eval, p$beta_w, p$beta_s, p$beta_hfa,
        p$sigma_y, p$sigma_w, p$sigma_s, p$sigma_team_hfa, p$sigma_hfa
      ))
      flush.console()
    }
    
    build_ssm(
      games                 = attr(model, "games_df"),
      N_teams               = attr(model, "N_teams"),
      N_weeks               = attr(model, "N_weeks"),
      N_seasons             = attr(model, "N_seasons"),
      first_week_of_season  = attr(model, "first_week_of_season"),
      last_week_of_season   = attr(model, "last_week_of_season"),
      theta = theta
    )
  }
}


# ---------- 4) Top-level fit function ----------
fit_nfl_kfas <- function(
    games,
    N_teams, N_weeks, N_seasons,
    first_week_of_season, last_week_of_season,
    init = c(
      beta_w = logit(0.8),        log_sigma_w = log(3),
      beta_s = logit(0.5),        log_sigma_s = log(5),
      beta_hfa = logit(0.8),      log_sigma_hfa = log(1),
      league_init = 2,            log_sigma_y = log(10),
      log_sigma_team_hfa = log(1.5)
    ),
    em_maxit = 200
) {
  
  # Build an initial model container to carry data via attributes
  # Provide a minimal SSModel; updatefn will replace it
  dummy <- SSModel(matrix(NA, 1, 1) ~ SSMtrend(1, Q = 1), H = matrix(1))
  attr(dummy, "games_df") <- games
  attr(dummy, "N_teams") <- N_teams
  attr(dummy, "N_weeks") <- N_weeks
  attr(dummy, "N_seasons") <- N_seasons
  attr(dummy, "first_week_of_season") <- first_week_of_season
  attr(dummy, "last_week_of_season") <- last_week_of_season
  
  # Basic sanity checks before EM
  stopifnot(all(games$week_id >= 1 & games$week_id <= N_weeks))
  stopifnot(length(first_week_of_season) == N_seasons)
  stopifnot(length(last_week_of_season)  == N_seasons)
  
  # Fit by EM
  fit <- fitSSM(
    inits = init,
    model = dummy,
    updatefn = updatefn,
    nsim = 0,  # exact
    method = "BFGS",
    maxit = em_maxit
  )
  
  # Build final model with fitted params
  final_model <- updatefn(fit$optim.out$par, dummy)
  kfs <- KFS(final_model, smoothing = c("state", "mean"))
  
  list(
    optim = fit$optim.out,
    model = final_model,
    kfs   = kfs
  )
}

fit_nfl_kfas <- function(
    games,
    N_teams, N_weeks, N_seasons,
    first_week_of_season, last_week_of_season,
    init = c(
      beta_w = logit(0.8),        log_sigma_w = log(3),
      beta_s = logit(0.5),        log_sigma_s = log(5),
      beta_hfa = logit(0.8),      log_sigma_hfa = log(1),
      league_init = 2,            log_sigma_y = log(10),
      log_sigma_team_hfa = log(1.5)
    ),
    em_maxit = 200,
    trace = TRUE,           # <-- show optim iteration summaries
    report_every = 10L      # <-- print our custom line every N evaluations
) {
  
  # Container model to carry data attrs
  dummy <- SSModel(matrix(NA, 1, 1) ~ SSMtrend(1, Q = 1), H = matrix(1))
  attr(dummy, "games_df") <- games
  attr(dummy, "N_teams") <- N_teams
  attr(dummy, "N_weeks") <- N_weeks
  attr(dummy, "N_seasons") <- N_seasons
  attr(dummy, "first_week_of_season") <- first_week_of_season
  attr(dummy, "last_week_of_season") <- last_week_of_season
  
  # Sanity checks
  stopifnot(all(games$week_id >= 1 & games$week_id <= N_weeks))
  stopifnot(length(first_week_of_season) == N_seasons)
  stopifnot(length(last_week_of_season)  == N_seasons)
  
  # EM via optim (BFGS) with console output
  fit <- fitSSM(
    inits   = init,
    model   = dummy,
    updatefn = updatefn_progress(report_every = report_every),
    nsim    = 0,       # exact
    method  = "BFGS",
    control = list(
      maxit  = em_maxit,
      trace  = if (isTRUE(trace)) 1L else 0L,  # optim's console trace
      REPORT = as.integer(report_every)        # how often optim reports
    )
  )
  
  final_model <- updatefn_progress(report_every)(fit$optim.out$par, dummy)
  kfs <- KFS(final_model, smoothing = c("state", "mean"))
  
  list(
    optim = fit$optim.out,
    model = final_model,
    kfs   = kfs
  )
}


# ---------- 5) Recover fitted means per game (mu) ----------
# This matches Stan's mu = (team_i - team_j) + team_hfa_i,s * hfa
fitted_game_means <- function(fit_obj, games) {
  mod  <- fit_obj$model
  kfs  <- fit_obj$kfs
  P    <- attr(mod, "P_basis")
  ds   <- attr(mod, "dim_strength")
  dtm  <- attr(mod, "dim_teamhfa")
  
  # smoothed states alpha_t for each week t provided in kfs$alphahat (m x N_weeks)
  alpha_hat <- kfs$alphahat
  # Build a data.frame with week, season, states
  # We need, for each game g at week w, season s:
  #   x_strength_w (length N_teams-1) -> lift to full team vector via P
  #   team_dev_s (length N_teams) -> take home team element
  #   league_hfa_s (scalar)
  
  # Map each week to the corresponding column in alpha_hat
  # KFS returns columns per time t (week)
  # Extract blocks
  # get_blocks <- function(t) {
  #   a <- alpha_hat[, t]
  #   strength <- a[1:ds]
  #   dev      <- a[(ds + 1):(ds + dtm)]
  #   league   <- a[ds + dtm + 1]
  #   list(strength = strength, dev = dev, league = league)
  # }
  
  # Extract state blocks at *time t* (row t, not column t)
  get_blocks <- function(t) {
    a <- alpha_hat[t, ]                 # <-- row t (time), all states
    stopifnot(length(a) == (ds + dtm + 1))
    strength <- a[1:ds]
    dev      <- a[(ds + 1):(ds + dtm)]
    league   <- a[ds + dtm + 1]
    list(strength = strength, dev = dev, league = league)
  }
  
  # Reconstruct full team-strength vector (sum-to-zero) from subspace:
  # x_full = P %*% x_sub
  make_strength_full <- function(x_sub) as.numeric(P %*% x_sub)
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    w <- games$week_id[g]
    bl <- get_blocks(w)
    s_full <- make_strength_full(bl$strength)
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    if (games$hfa[g] == 1L) {
      mu[g] <- base + (bl$league + bl$dev[i])
    } else {
      mu[g] <- base
    }
  }
  mu
}

osa_game_means <- function(fit_obj, games) {
  mod  <- fit_obj$model
  kfs  <- fit_obj$kfs
  P    <- attr(mod, "P_basis")
  ds   <- attr(mod, "dim_strength")
  dtm  <- attr(mod, "dim_teamhfa")
  
  # Dimensions from the model
  n <- dim(mod$Z)[3]             # number of time points (weeks)
  m <- dim(mod$T)[1]             # number of states
  
  a <- kfs$a
  if (is.null(a)) stop("KFS output missing 'a' (predicted states). Re-run KFS with smoothing='state'.")
  
  # Coerce 'a' to time x state = n x m, using common KFAS shapes:
  # - m x (n+1): columns are a_t for t=1..n plus final a_{n+1} -> take 1:n, then transpose
  # - m x n:     transpose to n x m
  # - n x m:     already correct
  # - (n+1) x m: take rows 1:n
  if (is.matrix(a)) {
    if (nrow(a) == m && ncol(a) == n + 1) {
      a_mat <- t(a[, 1:n, drop = FALSE])
    } else if (nrow(a) == m && ncol(a) == n) {
      a_mat <- t(a)
    } else if (nrow(a) == n && ncol(a) == m) {
      a_mat <- a
    } else if (nrow(a) == n + 1 && ncol(a) == m) {
      a_mat <- a[1:n, , drop = FALSE]
    } else {
      stop("Unexpected 'a' dims: ", paste(dim(a), collapse = "x"),
           " (expected m x (n+1), m x n, n x m, or (n+1) x m).")
    }
  } else {
    stop("'a' is not a matrix; got class: ", paste(class(a), collapse = ", "))
  }
  
  stopifnot(nrow(a_mat) == n, ncol(a_mat) == m)
  
  # Helper to extract blocks at time t (one-step-ahead means use predicted state at time t)
  get_blocks <- function(t) {
    at <- a_mat[t, ]
    strength <- at[1:ds]
    dev      <- at[(ds + 1):(ds + dtm)]
    league   <- at[ds + dtm + 1]
    list(strength = strength, dev = dev, league = league)
  }
  
  make_strength_full <- function(x_sub) as.numeric(P %*% x_sub)
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    w <- games$week_id[g]
    bl <- get_blocks(w)
    s_full <- make_strength_full(bl$strength)
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    mu[g] <- if (games$hfa[g] == 1L) base + (bl$league + bl$dev[i]) else base
  }
  mu
}

# -------- helpers (local to these fns) --------
.coerce_to_time_state <- function(X, n, m, take_first_n_cols = FALSE) {
  # Return an n x m matrix (time x state)
  if (is.null(X)) stop("State matrix is NULL.")
  if (nrow(X) == n && ncol(X) == m) {
    X
  } else if (nrow(X) == m && ncol(X) == n) {
    t(X)
  } else if (nrow(X) == m && ncol(X) == n + 1 && take_first_n_cols) {
    t(X[, 1:n, drop = FALSE])
  } else if (nrow(X) == n + 1 && ncol(X) == m && take_first_n_cols) {
    X[1:n, , drop = FALSE]
  } else {
    stop("Unexpected state dims: ", paste(dim(X), collapse = "x"))
  }
}

.get_dims_and_bases <- function(mod) {
  list(
    P_strength = attr(mod, "P_basis_strength") %||% attr(mod, "P_basis"),
    P_hfadev  = attr(mod, "P_basis_hfa_dev")   %||% attr(mod, "P_basis"),
    ds = attr(mod, "dim_strength"),
    dh = attr(mod, "dim_teamhfa_sub") %||% attr(mod, "dim_teamhfa"),
    n  = dim(mod$Z)[3],
    m  = dim(mod$T)[1]
  )
}
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------- in-sample (smoothed) fitted means --------
fitted_game_means <- function(fit_obj, games) {
  mod <- fit_obj$model
  kfs <- fit_obj$kfs
  dims <- .get_dims_and_bases(mod)
  
  # Smoothed states: alphahat (often n x m, but coerce robustly)
  X <- .coerce_to_time_state(kfs$alphahat, n = dims$n, m = dims$m)
  
  # Indices
  ds <- dims$ds; dh <- dims$dh; idx_league <- ds + dh + 1L
  P_s <- dims$P_strength; P_h <- dims$P_hfadev
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    t <- games$week_id[g]
    if (is.na(t) || t < 1L || t > dims$n || anyNA(X[t, ])) { mu[g] <- NA_real_; next }
    
    a_t <- X[t, ]
    # Strength (sum-to-zero): full team vector = P * subspace
    s_full  <- as.numeric(P_s %*% a_t[1:ds])
    # Team HFA deviation (sum-to-zero): full vector = P * subspace
    dev_full <- as.numeric(P_h %*% a_t[(ds + 1):(ds + dh)])
    # League HFA scalar
    league  <- a_t[idx_league]
    
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    mu[g] <- if (games$hfa[g] == 1L) base + (league + dev_full[i]) else base
  }
  mu
}

# -------- one-step-ahead (predicted) means --------
osa_game_means <- function(fit_obj, games) {
  mod <- fit_obj$model
  kfs <- fit_obj$kfs
  dims <- .get_dims_and_bases(mod)
  
  # Predicted states a_t (KFAS commonly stores as m x (n+1)); use 1..n
  A <- .coerce_to_time_state(kfs$a, n = dims$n, m = dims$m, take_first_n_cols = TRUE)
  
  ds <- dims$ds; dh <- dims$dh; idx_league <- ds + dh + 1L
  P_s <- dims$P_strength; P_h <- dims$P_hfadev
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    t <- games$week_id[g]
    if (is.na(t) || t < 1L || t > dims$n || anyNA(A[t, ])) { mu[g] <- NA_real_; next }
    
    a_t <- A[t, ]
    s_full   <- as.numeric(P_s %*% a_t[1:ds])
    dev_full <- as.numeric(P_h %*% a_t[(ds + 1):(ds + dh)])
    league   <- a_t[idx_league]
    
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    mu[g] <- if (games$hfa[g] == 1L) base + (league + dev_full[i]) else base
  }
  mu
}


# --------------- 5B) Recover Estimates -----------------
## State accessors (smoothed/filtered/predicted)
.get_state_mats <- function(fit_obj, type = c("smoothed","filtered","predicted")) {
  type <- match.arg(type)
  mod <- fit_obj$model
  kfs <- fit_obj$kfs
  
  n <- dim(mod$Z)[3]          # weeks (time)
  m <- dim(mod$T)[1]          # states
  
  # pick mean & covariance by type
  if (type == "smoothed") {
    X <- kfs$alphahat   # n x m (often), but be robust
    S <- kfs$V          # m x m x n
  } else if (type == "filtered") {
    X <- kfs$att        # m x n or n x m
    S <- kfs$Ptt        # m x m x n
  } else { # predicted (one-step-ahead)
    X <- kfs$a          # m x (n+1) or variants
    S <- kfs$P          # m x m x n
  }
  
  # Coerce X to n x m (time x state)
  if (is.null(X)) stop("Requested state matrix not present in KFS output: ", type)
  if (nrow(X) == n && ncol(X) == m) {
    Xnm <- X
  } else if (nrow(X) == m && ncol(X) == n) {
    Xnm <- t(X)
  } else if (nrow(X) == m && ncol(X) == n + 1) {
    Xnm <- t(X[, 1:n, drop = FALSE])
  } else if (nrow(X) == n + 1 && ncol(X) == m) {
    Xnm <- X[1:n, , drop = FALSE]
  } else {
    stop("Unexpected state dims for ", type, ": ", paste(dim(X), collapse = "x"),
         " (need n x m after coercion).")
  }
  
  # Coerce S to m x m x n
  if (is.null(S)) {
    Smmn <- array(NA_real_, dim = c(m, m, n))
  } else {
    ds <- dim(S)
    if (length(ds) == 3 && all(ds[1:2] == c(m, m)) && ds[3] == n) {
      Smmn <- S
    } else if (length(ds) == 3 && all(ds[1:2] == c(m, m)) && ds[3] == n + 1) {
      Smmn <- S[, , 1:n, drop = FALSE]
    } else {
      stop("Unexpected covariance dims for ", type, ": ", paste(dim(S), collapse = "x"),
           " (need m x m x n after coercion).")
    }
  }
  
  list(X = Xnm, S = Smmn, n = n, m = m)
}

## Weekly team strength (sum-to-zero full team vector), with optional CIs
extract_weekly_team_strength <- function(fit_obj, team_levels,
                                         type = c("smoothed","filtered","predicted"),
                                         intervals = FALSE, level = 0.95) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  P    <- attr(mod, "P_basis")
  ds   <- attr(mod, "dim_strength")
  dtm  <- attr(mod, "dim_teamhfa")
  stopifnot(ncol(P) == ds, length(team_levels) == nrow(P))
  
  sm <- .get_state_mats(fit_obj, type)
  X  <- sm$X  # n x m
  S  <- sm$S  # m x m x n
  n  <- sm$n
  
  # block indices
  idx_strength <- 1:ds
  
  # Mean reconstruction: s_full_t = P %*% x_sub_t
  # Return tibble: week_id, team_id, team, estimate, (ci_low, ci_high)
  out <- vector("list", n)
  if (intervals) {
    alpha <- (1 - level)/2
    z <- qnorm(1 - alpha)
  }
  for (t in seq_len(n)) {
    x_sub <- X[t, idx_strength]
    s_full <- as.numeric(P %*% x_sub)
    
    if (intervals && all(is.finite(S[,,t]))) {
      V_sub <- S[idx_strength, idx_strength, t, drop = FALSE]
      # Var(P x) = P Var(x) P^T ; take diagonal for each team
      V_full <- P %*% V_sub %*% t(P)
      se <- sqrt(pmax(0, diag(V_full)))
      lo <- s_full - z * se
      hi <- s_full + z * se
      out[[t]] <- tibble::tibble(
        week_id = t,
        team_id = seq_along(team_levels),
        team    = team_levels,
        estimate = s_full,
        ci_low = lo,
        ci_high = hi
      )
    } else {
      out[[t]] <- tibble::tibble(
        week_id = t,
        team_id = seq_along(team_levels),
        team    = team_levels,
        estimate = s_full
      )
    }
  }
  dplyr::bind_rows(out)
}

## Weekly league HFA scalar, with optional CIs 
extract_weekly_league_hfa <- function(fit_obj,
                                      type = c("smoothed","filtered","predicted"),
                                      intervals = FALSE, level = 0.95) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  ds   <- attr(mod, "dim_strength")
  dtm  <- attr(mod, "dim_teamhfa")
  
  sm <- .get_state_mats(fit_obj, type)
  X  <- sm$X
  S  <- sm$S
  n  <- sm$n
  
  idx_league <- ds + dtm + 1L
  
  if (intervals && all(is.finite(S))) {
    alpha <- (1 - level)/2
    z <- qnorm(1 - alpha)
    se <- sqrt(pmax(0, S[idx_league, idx_league, ]))
    tibble::tibble(
      week_id = seq_len(n),
      league_hfa = X[, idx_league],
      ci_low = X[, idx_league] - z * se,
      ci_high = X[, idx_league] + z * se
    )
  } else {
    tibble::tibble(
      week_id = seq_len(n),
      league_hfa = X[, idx_league]
    )
  }
}

## Weekly team HFA: dev and full (league + dev), with optional CIs 
extract_weekly_team_hfa <- function(fit_obj, team_levels,
                                    type = c("smoothed","filtered","predicted"),
                                    intervals = FALSE, level = 0.95,
                                    include_full = TRUE) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  ds   <- attr(mod, "dim_strength")
  dtm  <- attr(mod, "dim_teamhfa")
  stopifnot(length(team_levels) == dtm)
  
  sm <- .get_state_mats(fit_obj, type)
  X  <- sm$X
  S  <- sm$S
  n  <- sm$n
  
  idx_dev    <- (ds + 1):(ds + dtm)
  idx_league <- ds + dtm + 1L
  
  out <- vector("list", n)
  if (intervals) {
    alpha <- (1 - level)/2
    z <- qnorm(1 - alpha)
  }
  
  for (t in seq_len(n)) {
    dev_t    <- X[t, idx_dev]
    league_t <- X[t, idx_league]
    if (intervals && all(is.finite(S[,,t]))) {
      Vt <- S[,,t]
      se_dev <- sqrt(pmax(0, diag(Vt)[idx_dev]))
      lo_dev <- dev_t - z * se_dev
      hi_dev <- dev_t + z * se_dev
      if (include_full) {
        # Var(league + dev_i) = Var(league) + Var(dev_i) + 2*Cov(league, dev_i)
        var_league <- Vt[idx_league, idx_league]
        cov_l_dev  <- Vt[idx_dev, idx_league]
        se_full <- sqrt(pmax(0, var_league + se_dev^2 - diag(Vt)[idx_dev] + 2 * as.numeric(cov_l_dev)))
        # above simplified incorrectly; compute directly:
        # Var(league + dev_i) = var_league + var_dev_i + 2*cov(league, dev_i)
        var_dev <- diag(Vt)[idx_dev]
        cov_ld  <- Vt[idx_league, idx_dev]
        se_full <- sqrt(pmax(0, var_league + var_dev + 2 * as.numeric(cov_ld)))
        lo_full <- (league_t + dev_t) - z * se_full
        hi_full <- (league_t + dev_t) + z * se_full
        out[[t]] <- tibble::tibble(
          week_id = t,
          team_id = seq_along(team_levels),
          team    = team_levels,
          dev_est = dev_t,
          dev_lo  = lo_dev,
          dev_hi  = hi_dev,
          hfa_est = league_t + dev_t,
          hfa_lo  = lo_full,
          hfa_hi  = hi_full
        )
      } else {
        out[[t]] <- tibble::tibble(
          week_id = t,
          team_id = seq_along(team_levels),
          team    = team_levels,
          dev_est = dev_t,
          dev_lo  = lo_dev,
          dev_hi  = hi_dev
        )
      }
    } else {
      if (include_full) {
        out[[t]] <- tibble::tibble(
          week_id = t,
          team_id = seq_along(team_levels),
          team    = team_levels,
          dev_est = dev_t,
          hfa_est = league_t + dev_t
        )
      } else {
        out[[t]] <- tibble::tibble(
          week_id = t,
          team_id = seq_along(team_levels),
          team    = team_levels,
          dev_est = dev_t
        )
      }
    }
  }
  
  dplyr::bind_rows(out)
}


# ---------- 6) RUN ----------
# games_df must have: home_id, away_id, week_id, season_id, hfa (0/1), result
# And you must define first/last week per season (integer vectors of length N_seasons)
# Example (pseudo; replace with your real data):

## 6.1 Make Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2006, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2008, week == 21) |> pull(week_idx) |> unique()

train_data <- game_fit_data_all |> 
  filter(!is.na(result)) |>
  filter(between(week_idx, first_train_week, last_train_week))

# Build contiguous indices for the training window
teams   <- sort(unique(c(train_data$home_team, train_data$away_team)))
seasons <- sort(unique(train_data$season))
weeks   <- sort(unique(train_data$week_idx))   # weeks *in the window*
N_teams   <- length(teams)
N_seasons <- length(seasons)
N_weeks   <- length(weeks)                     # <- IMPORTANT

# ID maps (1-based)
team_id_map   <- setNames(seq_along(teams),   teams)
season_id_map <- setNames(seq_along(seasons), seasons)
week_id_map   <- setNames(seq_along(weeks),   weeks)

train_kfas <- train_data |>
  transmute(
    home_id   = team_id_map[home_team],
    away_id   = team_id_map[away_team],
    week_id   = week_id_map[as.character(week_idx)],  # 1..N_weeks
    season_id = season_id_map[as.character(season)],  # 1..N_seasons
    hfa       = as.integer(hfa),                      # already 0/1 in your data
    result
  )

# First/last week of each season in the *contiguous* week_id space
week_tbl_train <- week_tbl |>
  filter(week_idx %in% weeks) |>
  mutate(
    season_id = season_id_map[as.character(season)],
    week_id   = week_id_map[as.character(week_idx)]
  )

first_week_of_season <- week_tbl_train |>
  group_by(season_id) |>
  summarise(first = min(week_id), .groups = "drop") |>
  arrange(season_id) |>
  pull(first)

last_week_of_season <- week_tbl_train |>
  group_by(season_id) |>
  summarise(last = max(week_id), .groups = "drop") |>
  arrange(season_id) |>
  pull(last)


## 6.2 Fit ----
fit_kfa <- fit_nfl_kfas(
  games = train_kfas,
  N_teams = N_teams,
  N_weeks = N_weeks,
  N_seasons = N_seasons,
  first_week_of_season = first_week_of_season,
  last_week_of_season  = last_week_of_season
)

## 6.3 Estimates ----
### 6.3.1 Parameters ----
params_hat <- theta_to_params(fit_kfa$optim$par)
tidy_params(params_hat)

### 6.3.2 Game Results ----
mu_hat <- fitted_game_means(fit_kfa, train_kfas)
summary(mu_hat)

mu_osa <- osa_game_means(fit_kfa, train_kfas)  # one-step-ahead for each game/week in training window
summary(mu_osa)
mean(is.na(mu_osa))

train_eval <- train_kfas |>
  mutate(mu = mu_hat,
         resid = result - mu)

summ <- train_eval |>
  summarise(
    n = sum(!is.na(mu)),
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    mae  = mean(abs(resid), na.rm = TRUE),
    mean_mu = mean(mu, na.rm = TRUE),
    mean_y  = mean(result, na.rm = TRUE)
  )
summ

hist(train_eval$resid, 40, main = "Residuals", xlab = "result - mu")
plot(train_eval$mu, train_eval$resid, pch = 16, cex = .5)
abline(h = 0)

### 6.3.3 Team Strength ----
# Team strength (sum-to-zero) — choose smoothed / filtered / predicted
strength_sm <- extract_weekly_team_strength(fit_kfa, team_levels = teams, type = "smoothed", intervals = TRUE)
league_sm   <- extract_weekly_league_hfa(fit_kfa, type = "smoothed", intervals = TRUE)
teamhfa_sm  <- extract_weekly_team_hfa(fit_kfa, team_levels = teams, type = "smoothed", intervals = TRUE)

# One-step-ahead (OSA) weekly series (before seeing week t)
strength_osa <- extract_weekly_team_strength(fit_kfa, team_levels = teams, type = "predicted")
league_osa   <- extract_weekly_league_hfa(fit_kfa, type = "predicted")
teamhfa_osa  <- extract_weekly_team_hfa(fit_kfa, team_levels = teams, type = "predicted")


# 7. Diagnostics -----
## 7.1 Helpers ----
# Assemble a per-game frame with weekly state estimates merged in
assemble_game_frame <- function(fit_obj, games_df, team_levels, type = c("predicted","smoothed","filtered")) {
  type <- match.arg(type)
  # 1) pull weekly series
  strength <- extract_weekly_team_strength(fit_obj, team_levels, type = type, intervals = FALSE) |>
    dplyr::rename(s_est = estimate)
  league   <- extract_weekly_league_hfa(fit_obj, type = type, intervals = FALSE) |>
    dplyr::rename(league_hfa_est = league_hfa)
  teamhfa  <- extract_weekly_team_hfa(fit_obj, team_levels, type = type, intervals = FALSE, include_full = TRUE) |>
    dplyr::rename(hfa_dev_est = dev_est, hfa_full_est = hfa_est)
  
  # 2) make compact join tables keyed by (week_id, team_id)
  strength_k <- strength |>
    dplyr::transmute(week_id, team_id, s_est)
  
  teamhfa_k  <- teamhfa |>
    dplyr::transmute(week_id, team_id, hfa_dev_est, hfa_full_est)
  
  # 3) join onto game rows (twice for home/away)
  gf <- games_df |>
    dplyr::select(game_id = dplyr::any_of("game_id"),
                  week_id, season_id, home_id, away_id, hfa, result) |>
    # strengths
    dplyr::left_join(strength_k, by = c("week_id","home_id" = "team_id")) |>
    dplyr::rename(s_home = s_est) |>
    dplyr::left_join(strength_k, by = c("week_id","away_id" = "team_id")) |>
    dplyr::rename(s_away = s_est) |>
    # team HFA (full = league + dev, used only if hfa==1)
    dplyr::left_join(teamhfa_k, by = c("week_id","home_id" = "team_id")) |>
    dplyr::rename(home_hfa_dev = hfa_dev_est, home_hfa_full = hfa_full_est) |>
    # league scalar (handy for plots/checks)
    dplyr::left_join(league, by = "week_id")
  
  # 4) implied mean and residual
  gf <- gf |>
    dplyr::mutate(
      base_spread = s_home - s_away,
      hfa_contrib = dplyr::if_else(hfa == 1L, home_hfa_full, 0),
      mu_est      = base_spread + hfa_contrib,
      resid       = result - mu_est
    )
  
  gf
}

# Summaries
summarise_games <- function(gf) {
  gf |>
    dplyr::summarise(
      n = dplyr::n(),
      rmse = sqrt(mean(resid^2, na.rm = TRUE)),
      mae  = mean(abs(resid), na.rm = TRUE),
      mean_mu = mean(mu_est, na.rm = TRUE),
      mean_y  = mean(result, na.rm = TRUE)
    )
}

metrics_by_week <- function(gf) {
  gf |>
    dplyr::group_by(week_id) |>
    dplyr::summarise(
      n = dplyr::n(),
      rmse = sqrt(mean(resid^2, na.rm = TRUE)),
      mae  = mean(abs(resid), na.rm = TRUE),
      mean_mu = mean(mu_est, na.rm = TRUE),
      mean_y  = mean(result, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}

# Optional: top teams each week by strength
top_strength_by_week <- function(fit_obj, team_levels, k = 5, type = c("predicted","smoothed","filtered")) {
  type <- match.arg(type)
  extract_weekly_team_strength(fit_obj, team_levels, type = type) |>
    dplyr::group_by(week_id) |>
    dplyr::slice_max(order_by = estimate, n = k, with_ties = FALSE) |>
    dplyr::ungroup()
}

## 7.2 Output ----

# 1) One-step-ahead (OSA) per-game frame
games_osa <- assemble_game_frame(fit_kfa, train_kfas, team_levels = teams, type = "predicted")
summarise_games(games_osa)
metrics_week_osa <- metrics_by_week(games_osa)

# 2) Smoothed per-game frame
games_sm <- assemble_game_frame(fit_kfa, train_kfas, team_levels = teams, type = "smoothed")
summarise_games(games_sm)
metrics_week_sm <- metrics_by_week(games_sm)

# 3) Join week/season labels (from your week_tbl_train)
week_lookup <- week_tbl_train |>
  dplyr::select(week_id, season_id, season, week, week_idx)

games_osa_labeled <- games_osa |>
  dplyr::left_join(week_lookup, by = c("week_id","season_id")) |>
  left_join(train_data) |>
  relocate(
    c(
      game_id, season, season_idx, week, week_idx,
      game_type, season_type,
      home_team, away_team, home_id, away_id, location,
      hfa, home_score, away_score, result
    ),
    .before = 1
  ) |>
  mutate(across(where(is.numeric),
         ~round(.x, 6)))

# 4) Quick checks / plots
# Residuals over time (OSA)
ggplot2::ggplot(games_osa_labeled, ggplot2::aes(week_idx, resid)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(se = FALSE, method = "loess") +
  ggplot2::labs(title = "OSA residuals over time", x = "Week index", y = "result - mu_osa")

# League HFA over time
league_osa <- extract_weekly_league_hfa(fit_kfa, type = "predicted")
league_plot <- league_osa |>
  dplyr::left_join(week_lookup, by = "week_id") |>
  ggplot2::ggplot(ggplot2::aes(week_idx, league_hfa)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "One-step-ahead league HFA", x = "Week index", y = "HFA")
league_plot

# Top-5 strengths per week (smoothed)
top5_sm <- top_strength_by_week(fit_kfa, team_levels = teams, k = 5, type = "smoothed")
head(top5_sm, 20)

# 5) Inspect a specific week with predictions vs actuals (OSA)
inspect_week <- function(week_id_val) {
  games_osa_labeled |>
    dplyr::filter(week_id == week_id_val) |>
    dplyr::transmute(
      season, week, home_id, away_id, hfa, result,
      s_home, s_away, league_hfa_est, home_hfa_full,
      base_spread, hfa_contrib, mu_est, resid
    ) |>
    dplyr::arrange(dplyr::desc(abs(resid)))
}
inspect_week(week_id_val = 10)







