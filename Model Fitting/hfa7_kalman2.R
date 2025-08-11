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

# ---------- Math helpers ----------
logit     <- function(x) log(x/(1-x))
inv_logit <- function(z) 1/(1+exp(-z))
`%||%`    <- function(a,b) if (!is.null(a)) a else b

# Orthonormal basis for the sum-to-zero subspace in R^N (N x (N-1))
orthonormal_sumzero_basis <- function(N) {
  M  <- diag(N) - matrix(1/N, N, N)
  Q  <- qr.Q(qr(M))
  one <- rep(1/sqrt(N), N)
  Qp <- apply(Q, 2, function(col) col - sum(col*one)*one)
  B  <- qr.Q(qr(Qp))
  B[, 1:(N-1), drop = FALSE]
}

# Row design for strengths: (e_i - e_j)^T P == P[i,] - P[j,]
design_row_strength <- function(i, j, P) {
  P[i, ] - P[j, ]
}

# ---------- Parameter transforms ----------
.ensure_theta_names <- function(theta) {
  expected <- c("beta_w","log_sigma_w","beta_s","log_sigma_s",
                "beta_hfa","log_sigma_hfa","league_init",
                "log_sigma_y","log_sigma_team_hfa")
  if (is.null(names(theta)) || !all(expected %in% names(theta))) {
    names(theta) <- expected
  }
  theta
}

theta_to_params <- function(theta) {
  theta <- .ensure_theta_names(theta)
  list(
    beta_w         = as.numeric(inv_logit(theta["beta_w"])),
    sigma_w        = as.numeric(exp(theta["log_sigma_w"])),
    beta_s         = as.numeric(inv_logit(theta["beta_s"])),
    sigma_s        = as.numeric(exp(theta["log_sigma_s"])),
    beta_hfa       = as.numeric(inv_logit(theta["beta_hfa"])),
    sigma_hfa      = as.numeric(exp(theta["log_sigma_hfa"])),
    league_init    = as.numeric(theta["league_init"]),
    sigma_y        = as.numeric(exp(theta["log_sigma_y"])),
    sigma_team_hfa = as.numeric(exp(theta["log_sigma_team_hfa"]))
  )
}

# ---------- Core builder (uses week_idx/season_idx) ----------
# Expects 'games' with columns: home_id, away_id, week_idx, season_idx, hfa, result
build_ssm <- function(games,
                      N_teams,
                      theta) {
  
  par <- theta_to_params(theta)
  stopifnot(!anyNA(unlist(par)))
  
  # Unique weeks in order (these are your original week_idx labels)
  week_levels   <- sort(unique(games$week_idx))
  season_levels <- sort(unique(games$season_idx))
  N_weeks       <- length(week_levels)
  N_seasons     <- length(season_levels)
  
  # Internal 1..n time index (t) and 1..S season index (s) derived from week_idx/season_idx
  wk_map <- setNames(seq_len(N_weeks), week_levels)
  ss_map <- setNames(seq_len(N_seasons), season_levels)
  g_int  <- games |>
    mutate(
      t = wk_map[as.character(week_idx)],
      s = ss_map[as.character(season_idx)]
    )
  
  # Robust season-of-week mapping (every modeled week gets exactly one season)
  wk_season <- g_int |> distinct(t, s) |> arrange(t)
  stopifnot(setequal(wk_season$t, seq_len(N_weeks)))
  season_of_week <- wk_season$s
  
  # First/last week (in internal t) per season
  first_week_t <- wk_season |> group_by(s) |> summarise(first = min(t), .groups="drop") |> arrange(s) |> pull(first)
  last_week_t  <- wk_season |> group_by(s) |> summarise(last  = max(t), .groups="drop") |> arrange(s) |> pull(last)
  
  # ----- Bases and state layout -----
  # Use the same sum-to-zero basis P for both strengths and team-HFA deviations
  P <- orthonormal_sumzero_basis(N_teams)   # [N_teams x (N_teams-1)]
  
  ds <- N_teams - 1   # strength subspace dim
  dh <- N_teams - 1   # team-HFA dev subspace dim
  dl <- 1             # league scalar
  m  <- ds + dh + dl  # total states
  
  # ----- Observation layout -----
  # Time = week (t = 1..N_weeks). p = max games in any week.
  by_week <- split(seq_len(nrow(g_int)), g_int$t)
  dims_y  <- sapply(by_week, function(idx) if (length(idx)) length(idx) else 0L)
  p       <- max(dims_y)
  stopifnot(p > 0L)
  
  # y must be n x p (time rows)
  Y <- matrix(NA_real_, nrow = N_weeks, ncol = p)
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    Y[t, seq_len(dims_y[t])] <- g_int$result[idx]
  }
  
  # H: constant p x p diagonal
  H <- diag(par$sigma_y^2, p)
  
  # Z: p x m x n
  Z <- array(0, dim = c(p, m, N_weeks))
  for (t in seq_len(N_weeks)) {
    if (dims_y[t] == 0L) next
    idx <- by_week[[t]]
    for (k in seq_len(dims_y[t])) {
      gk <- g_int[idx[k], ]
      i  <- gk$home_id
      j  <- gk$away_id
      h  <- gk$hfa
      
      # Strength block (sum-to-zero subspace)
      Z[k, 1:ds, t] <- design_row_strength(i, j, P)
      
      # Team-HFA dev block (sum-to-zero subspace) & league scalar
      off <- ds
      if (h == 1L) {
        # contribution is dev_full_i = (e_i^T P) * dev_sub  -> row = P[i, ]
        Z[k, (off + 1):(off + dh), t] <- P[i, ]
        # league scalar
        Z[k, m, t] <- 1
      }
    }
  }
  
  # ----- State evolution (T, R, Q): m x m x n -----
  T_arr <- array(0, dim = c(m, m, N_weeks))
  R_arr <- array(0, dim = c(m, m, N_weeks))
  Q_arr <- array(0, dim = c(m, m, N_weeks))
  
  I_s <- diag(ds)
  I_h <- diag(dh)
  
  for (t in seq_len(N_weeks)) {
    s  <- season_of_week[t]
    fw <- first_week_t[s]
    
    # Strengths: carryover at season start, AR(1) within season
    if (t == fw) {
      T_arr[1:ds, 1:ds, t] <- par$beta_s * I_s
      Q_arr[1:ds, 1:ds, t] <- (par$sigma_s^2) * I_s
    } else {
      T_arr[1:ds, 1:ds, t] <- par$beta_w * I_s
      Q_arr[1:ds, 1:ds, t] <- (par$sigma_w^2) * I_s
    }
    
    # Team HFA deviations: restart at season start, static within season
    off <- ds
    if (t == fw) {
      Q_arr[(off+1):(off+dh), (off+1):(off+dh), t] <- (par$sigma_team_hfa^2) * I_h
    } else {
      T_arr[(off+1):(off+dh), (off+1):(off+dh), t] <- I_h
    }
    
    # League HFA: AR(1) at season start, identity within season
    if (t == fw) {
      T_arr[m, m, t] <- par$beta_hfa
      Q_arr[m, m, t] <- par$sigma_hfa^2
    } else {
      T_arr[m, m, t] <- 1
    }
    
    # Shock loading
    R_arr[,,t] <- diag(m)
  }
  
  # ----- Initial state -----
  a1 <- rep(0, m)
  P1 <- diag(m) * 1e6
  a1[m]   <- par$league_init
  P1[m,m] <- 2^2
  # proper prior for team-HFA dev at first time
  off <- ds
  P1[(off+1):(off+dh), (off+1):(off+dh)] <- (par$sigma_team_hfa^2) * I_h
  
  # Assertions
  stopifnot(
    is.matrix(Y), nrow(Y) == N_weeks, ncol(Y) == p,
    is.array(Z), all(dim(Z) == c(p, m, N_weeks)),
    is.array(T_arr), all(dim(T_arr) == c(m, m, N_weeks)),
    is.array(R_arr), all(dim(R_arr) == c(m, m, N_weeks)),
    is.array(Q_arr), all(dim(Q_arr) == c(m, m, N_weeks)),
    is.matrix(H), all(dim(H) == c(p, p))
  )
  
  model <- SSModel(
    Y ~ -1 + SSMcustom(Z = Z, T = T_arr, R = R_arr, Q = Q_arr, a1 = a1, P1 = P1),
    H = H
  )
  
  # Keep attributes for downstream use
  attr(model, "games_df")        <- g_int
  attr(model, "week_levels")     <- week_levels     # map t -> original week_idx
  attr(model, "season_levels")   <- season_levels   # map s -> original season_idx
  attr(model, "P_basis_strength")<- P
  attr(model, "P_basis_hfa_dev") <- P
  attr(model, "dim_strength")    <- ds
  attr(model, "dim_teamhfa_sub") <- dh
  
  model
}

# ---------- Progress-aware updatefn ----------
updatefn_progress <- function(report_every = 10L) {
  env <- new.env(parent = emptyenv()); env$eval <- 0L; env$report_every <- as.integer(report_every)
  function(theta, model, ...) {
    theta <- .ensure_theta_names(theta)
    env$eval <- env$eval + 1L
    if (env$eval %% env$report_every == 0L) {
      p <- theta_to_params(theta)
      cat(sprintf("[fitSSM] eval=%d | beta_w=%.3f beta_s=%.3f beta_hfa=%.3f | sigmas: y=%.3f w=%.3f s=%.3f team_hfa=%.3f hfa=%.3f\n",
                  env$eval, p$beta_w, p$beta_s, p$beta_hfa, p$sigma_y, p$sigma_w, p$sigma_s, p$sigma_team_hfa, p$sigma_hfa))
      flush.console()
    }
    # Rebuild complete model using the games stored on the dummy
    games <- attr(model, "games_df_full")  # original (week_idx/season_idx)
    build_ssm(
      games   = games,
      N_teams = attr(model, "N_teams"),
      theta   = theta
    )
  }
}

# ---------- Top-level fit ----------
# Expects 'games' with (home_id, away_id, week_idx, season_idx, hfa, result)
fit_nfl_kfas <- function(
    games,
    N_teams,
    init = c(
      beta_w = logit(0.8),        log_sigma_w = log(3),
      beta_s = logit(0.5),        log_sigma_s = log(5),
      beta_hfa = logit(0.8),      log_sigma_hfa = log(1),
      league_init = 2,            log_sigma_y = log(10),
      log_sigma_team_hfa = log(1.5)
    ),
    em_maxit = 200,
    trace = TRUE,
    report_every = 10L
) {
  
  # minimal dummy to carry the raw games & metadata through fitSSM
  dummy <- SSModel(matrix(NA, 1, 1) ~ SSMtrend(1, Q = 1), H = matrix(1))
  attr(dummy, "games_df_full") <- games
  attr(dummy, "N_teams") <- N_teams
  
  # EM with progress
  fit <- fitSSM(
    inits    = init,
    model    = dummy,
    updatefn = updatefn_progress(report_every = report_every),
    nsim     = 0,
    method   = "BFGS",
    control  = list(maxit = em_maxit,
                    trace = if (isTRUE(trace)) 1L else 0L,
                    REPORT = as.integer(report_every))
  )
  
  # Final model (rebuild at optimum)
  final_model <- updatefn_progress(report_every)(fit$optim.out$par, dummy)
  kfs <- KFS(final_model, smoothing = c("state", "mean"))
  
  list(
    optim = fit$optim.out,
    model = final_model,
    kfs   = kfs
  )
}

# ---------- State readers & game means ----------
.coerce_to_time_state <- function(X, n, m, take_first_n_cols = FALSE) {
  if (is.null(X)) stop("State matrix is NULL.")
  if (nrow(X) == n && ncol(X) == m) return(X)
  if (nrow(X) == m && ncol(X) == n) return(t(X))
  if (take_first_n_cols && nrow(X) == m && ncol(X) == n + 1) return(t(X[, 1:n, drop = FALSE]))
  if (take_first_n_cols && nrow(X) == n + 1 && ncol(X) == m) return(X[1:n, , drop = FALSE])
  stop("Unexpected state dims: ", paste(dim(X), collapse = "x"))
}

.get_dims_and_bases <- function(mod) {
  list(
    P_strength = attr(mod, "P_basis_strength"),
    P_hfadev   = attr(mod, "P_basis_hfa_dev"),
    ds         = attr(mod, "dim_strength"),
    dh         = attr(mod, "dim_teamhfa_sub"),
    n          = dim(mod$Z)[3],
    m          = dim(mod$T)[1],
    week_levels = attr(mod, "week_levels"),
    games_int   = attr(mod, "games_df")
  )
}

# In-sample (smoothed) fitted means
fitted_game_means <- function(fit_obj, games) {
  mod  <- fit_obj$model
  kfs  <- fit_obj$kfs
  d    <- .get_dims_and_bases(mod)
  X    <- .coerce_to_time_state(kfs$alphahat, n = d$n, m = d$m)
  
  ds <- d$ds; dh <- d$dh; idx_league <- ds + dh + 1L
  P_s <- d$P_strength; P_h <- d$P_hfadev
  
  # map external week_idx -> internal t
  wk_map <- setNames(seq_len(d$n), d$week_levels)
  t_idx  <- wk_map[as.character(games$week_idx)]
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    t <- t_idx[g]
    if (is.na(t) || anyNA(X[t, ])) { mu[g] <- NA_real_; next }
    a_t <- X[t, ]
    s_full   <- as.numeric(P_s %*% a_t[1:ds])
    dev_full <- as.numeric(P_h %*% a_t[(ds+1):(ds+dh)])
    league   <- a_t[idx_league]
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    mu[g] <- if (games$hfa[g] == 1L) base + (league + dev_full[i]) else base
  }
  mu
}

# One-step-ahead (predicted) means
osa_game_means <- function(fit_obj, games) {
  mod  <- fit_obj$model
  kfs  <- fit_obj$kfs
  d    <- .get_dims_and_bases(mod)
  A    <- .coerce_to_time_state(kfs$a, n = d$n, m = d$m, take_first_n_cols = TRUE)
  
  ds <- d$ds; dh <- d$dh; idx_league <- ds + dh + 1L
  P_s <- d$P_strength; P_h <- d$P_hfadev
  
  wk_map <- setNames(seq_len(d$n), d$week_levels)
  t_idx  <- wk_map[as.character(games$week_idx)]
  
  mu <- numeric(nrow(games))
  for (g in seq_len(nrow(games))) {
    t <- t_idx[g]
    if (is.na(t) || anyNA(A[t, ])) { mu[g] <- NA_real_; next }
    a_t <- A[t, ]
    s_full   <- as.numeric(P_s %*% a_t[1:ds])
    dev_full <- as.numeric(P_h %*% a_t[(ds+1):(ds+dh)])
    league   <- a_t[idx_league]
    i <- games$home_id[g]; j <- games$away_id[g]
    base <- s_full[i] - s_full[j]
    mu[g] <- if (games$hfa[g] == 1L) base + (league + dev_full[i]) else base
  }
  mu
}

# ---------- Weekly extractors (smoothed/filtered/predicted) ----------
.get_state_mats <- function(fit_obj, type = c("smoothed","filtered","predicted")) {
  type <- match.arg(type)
  mod <- fit_obj$model; kfs <- fit_obj$kfs
  n <- dim(mod$Z)[3]; m <- dim(mod$T)[1]
  if (type=="smoothed") { X<-kfs$alphahat; S<-kfs$V }
  else if (type=="filtered") { X<-kfs$att; S<-kfs$Ptt }
  else { X<-kfs$a; S<-kfs$P }
  Xnm <- .coerce_to_time_state(X, n, m, take_first_n_cols = (type=="predicted"))
  # coerce S (m x m x n(+1)) -> m x m x n
  if (is.null(S)) Smmn <- array(NA_real_, dim = c(m,m,n))
  else {
    ds <- dim(S)
    if (length(ds)==3 && ds[3]==n) Smmn <- S
    else if (length(ds)==3 && ds[3]==n+1) Smmn <- S[,,1:n, drop=FALSE]
    else stop("Unexpected covariance dims.")
  }
  list(X=Xnm, S=Smmn, n=n, m=m)
}

extract_weekly_team_strength <- function(fit_obj, team_levels,
                                         type=c("smoothed","filtered","predicted"),
                                         intervals=FALSE, level=0.95) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  P_s  <- attr(mod, "P_basis_strength")
  ds   <- attr(mod, "dim_strength")
  wk   <- attr(mod, "week_levels")
  sm   <- .get_state_mats(fit_obj, type)
  X    <- sm$X; S <- sm$S; n <- sm$n
  out  <- vector("list", n)
  if (intervals) { alpha <- (1-level)/2; z <- qnorm(1-alpha) }
  for (t in seq_len(n)) {
    x_sub  <- X[t, 1:ds]
    s_full <- as.numeric(P_s %*% x_sub)
    if (intervals && all(is.finite(S[,,t]))) {
      V_sub <- S[1:ds, 1:ds, t, drop=FALSE]
      V_full <- P_s %*% V_sub %*% t(P_s)
      se <- sqrt(pmax(0, diag(V_full)))
      out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                         team = team_levels, estimate = s_full,
                         ci_low = s_full - z*se, ci_high = s_full + z*se)
    } else {
      out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                         team = team_levels, estimate = s_full)
    }
  }
  bind_rows(out)
}

extract_weekly_league_hfa <- function(fit_obj, type=c("smoothed","filtered","predicted"),
                                      intervals=FALSE, level=0.95) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  ds   <- attr(mod, "dim_strength")
  dh   <- attr(mod, "dim_teamhfa_sub")
  wk   <- attr(mod, "week_levels")
  sm   <- .get_state_mats(fit_obj, type)
  X    <- sm$X; S <- sm$S; n <- sm$n
  idx  <- ds + dh + 1L
  if (intervals && all(is.finite(S))) {
    alpha <- (1-level)/2; z <- qnorm(1-alpha)
    se <- sqrt(pmax(0, S[idx, idx, ]))
    tibble(week_idx = wk, league_hfa = X[, idx],
           ci_low = X[, idx] - z*se, ci_high = X[, idx] + z*se)
  } else {
    tibble(week_idx = wk, league_hfa = X[, idx])
  }
}

extract_weekly_team_hfa <- function(fit_obj, team_levels,
                                    type=c("smoothed","filtered","predicted"),
                                    intervals=FALSE, level=0.95,
                                    include_full=TRUE) {
  type <- match.arg(type)
  mod  <- fit_obj$model
  P_h  <- attr(mod, "P_basis_hfa_dev")
  ds   <- attr(mod, "dim_strength")
  dh   <- attr(mod, "dim_teamhfa_sub")
  wk   <- attr(mod, "week_levels")
  sm   <- .get_state_mats(fit_obj, type)
  X    <- sm$X; S <- sm$S; n <- sm$n
  out  <- vector("list", n)
  if (intervals) { alpha <- (1-level)/2; z <- qnorm(1-alpha) }
  idx_dev <- (ds+1):(ds+dh); idx_league <- ds+dh+1L
  for (t in seq_len(n)) {
    dev_sub  <- X[t, idx_dev]
    dev_full <- as.numeric(P_h %*% dev_sub)
    league_t <- X[t, idx_league]
    if (intervals && all(is.finite(S[,,t]))) {
      Vt <- S[,,t]; var_dev <- diag(Vt)[idx_dev]; var_league <- Vt[idx_league, idx_league]
      cov_ld <- Vt[idx_league, idx_dev]
      # project var(dev_sub) to var(dev_full)
      V_dev_full <- P_h %*% Vt[idx_dev, idx_dev, drop=FALSE] %*% t(P_h)
      se_dev <- sqrt(pmax(0, diag(V_dev_full)))
      if (include_full) {
        # Var(league + dev_full_i) = Var(league) + Var(dev_full_i) + 2*Cov(league, dev_full_i)
        cov_l_dev_full <- as.numeric(P_h %*% cov_ld)
        se_full <- sqrt(pmax(0, var_league + diag(V_dev_full) + 2*cov_l_dev_full))
        out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                           team = team_levels,
                           dev_est = dev_full,
                           dev_lo = dev_full - z*se_dev,
                           dev_hi = dev_full + z*se_dev,
                           hfa_est = league_t + dev_full,
                           hfa_lo  = league_t + dev_full - z*se_full,
                           hfa_hi  = league_t + dev_full + z*se_full)
      } else {
        out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                           team = team_levels,
                           dev_est = dev_full,
                           dev_lo = dev_full - z*se_dev,
                           dev_hi = dev_full + z*se_dev)
      }
    } else {
      if (include_full) {
        out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                           team = team_levels,
                           dev_est = dev_full,
                           hfa_est = league_t + dev_full)
      } else {
        out[[t]] <- tibble(week_idx = wk[t], team_id = seq_along(team_levels),
                           team = team_levels,
                           dev_est = dev_full)
      }
    }
  }
  bind_rows(out)
}

# ---------- Join helpers ----------
assemble_game_frame <- function(fit_obj, games_df, team_levels, type = c("predicted","smoothed","filtered")) {
  type <- match.arg(type)
  strength <- extract_weekly_team_strength(fit_obj, team_levels, type = type) |>
    rename(s_est = estimate)
  league   <- extract_weekly_league_hfa(fit_obj, type = type) |>
    rename(league_hfa_est = league_hfa)
  teamhfa  <- extract_weekly_team_hfa(fit_obj, team_levels, type = type, include_full = TRUE) |>
    rename(hfa_dev_est = dev_est, hfa_full_est = hfa_est)
  
  strength_k <- strength |> transmute(week_idx, team_id, s_est)
  teamhfa_k  <- teamhfa  |> transmute(week_idx, team_id, hfa_dev_est, hfa_full_est)
  
  gf <- games_df |>
    select(any_of(c("game_id")), 
           season, season_idx,
           week, week_idx,
           home_team, away_team, home_id, away_id, 
           hfa, 
           home_score, away_score, 
           result, spread_line) |>
    left_join(strength_k, by = c("week_idx","home_id" = "team_id")) |>
    rename(s_home = s_est) |>
    left_join(strength_k, by = c("week_idx","away_id" = "team_id")) |>
    rename(s_away = s_est) |>
    left_join(teamhfa_k, by = c("week_idx","home_id" = "team_id")) |>
    rename(home_hfa_dev = hfa_dev_est, home_hfa_full = hfa_full_est) |>
    left_join(league, by = "week_idx") |>
    mutate(
      base_spread = s_home - s_away,
      hfa_contrib = if_else(hfa == 1L, home_hfa_full, 0),
      mu_est      = base_spread + hfa_contrib,
      resid       = result - mu_est,
      resid_spread = result - spread_line
    )
  gf
}




# ---------- 6) RUN ----------
# games_df must have: home_id, away_id, week_id, season_id, hfa (0/1), result
# And you must define first/last week per season (integer vectors of length N_seasons)
# Example (pseudo; replace with your real data):

## 6.1 Make Data ----
first_train_week <- 
  game_fit_data_all |> filter(season == 2006, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

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


## 6.2 Fit ==============
fit_kfa <- fit_nfl_kfas(
  train_data, 
  N_teams, 
  trace = TRUE, 
  report_every = 10
)

## 6.3 Estimates ----
### 6.3.1 Game Means ----
mu_fit <- fitted_game_means(fit_kfa, train_data) # In-sample
mu_osa <- osa_game_means(fit_kfa, train_data) # One-step-ahead means

#### filtered
#### smoothed
#### predicted

### 6.3.2 Weekly Latent Values ----
#### filtered ----
team_strength_ft <- extract_weekly_team_strength(
  fit_kfa, team_levels = teams, 
  type = "filtered")
league_hfa_ft  <- extract_weekly_league_hfa(
  fit_kfa, 
  type = "filtered")
team_hfa_ft  <- extract_weekly_team_hfa(
  fit_kfa, team_levels = teams, 
  type = "filtered")

#### smoothed ----
team_strength_sm <- extract_weekly_team_strength(
  fit_kfa, team_levels = teams, 
  type = "smoothed")
league_hfa_sm  <- extract_weekly_league_hfa(
  fit_kfa, 
  type = "smoothed")
team_hfa_sm  <- extract_weekly_team_hfa(
  fit_kfa, team_levels = teams, 
  type = "smoothed")

#### predicted ----
team_strength_pred <- extract_weekly_team_strength(
  fit_kfa, team_levels = teams, 
  type = "predicted")
league_hfa_pred  <- extract_weekly_league_hfa(
  fit_kfa, 
  type = "predicted")
team_hfa_pred  <- extract_weekly_team_hfa(
  fit_kfa, team_levels = teams, 
  type = "predicted")

### 6.3.3 Game Estimates ----
#### filtered ----
games_ft <- assemble_game_frame(
  fit_kfa, train_data, team_levels = teams, 
  type = "filtered"
) |>
  mutate(across(where(is.numeric), ~round(.x, 6))) |>
  left_join(train_data)

#### smoothed ----
games_sm <- assemble_game_frame(
  fit_kfa, train_data, team_levels = teams, 
  type = "smoothed"
) |>
  mutate(across(where(is.numeric), ~round(.x, 6))) |>
  left_join(train_data)

#### predicted ----
games_pred <- assemble_game_frame(
  fit_kfa, train_data, team_levels = teams, 
  type = "predicted"
) |>
  mutate(across(where(is.numeric), ~round(.x, 6))) |>
  left_join(train_data)

## 6.4 Metrics ----
backtest_metric_perf <- function(games_df, group = NULL) {
  stopifnot(is.null(group) || is.character(group))
  
  df <- if (is.null(group)) {
    games_df
  } else {
    games_df |> dplyr::group_by(dplyr::across(dplyr::all_of(group)))
  }
  
  df |>
    summarise(
      MAE  = mean(abs(resid), na.rm = TRUE),
      MAE_spread  = mean(abs(resid_spread), na.rm = TRUE),
      #COV_90 = mean(result >= pred_q05 & result <= pred_q95, na.rm = TRUE),
      RMSE = sqrt(mean(resid^2, na.rm = TRUE)),
      RMSE_spread = sqrt(mean(resid_spread^2, na.rm = TRUE)),
      Bias = mean(resid, na.rm = TRUE),
      Bias_spread = mean(resid_spread, na.rm = TRUE),
      SD_Error = sd(resid, na.rm = TRUE),
      SD_Error_spread = sd(resid_spread, na.rm = TRUE)
    )
}

### 6.4.1 fitted ----
back_metrics_ft_all <- backtest_metric_perf(games_ft, group = NULL)
back_metrics_ft_season <- backtest_metric_perf(games_ft, group = c("season"))
back_metrics_ft_week <- backtest_metric_perf(games_ft, group = c("week"))
back_metrics_ft_week_idx <- backtest_metric_perf(games_ft, group = c("week_idx"))

back_metrics_ft_all |> data.frame()
back_metrics_ft_season |> data.frame()
back_metrics_ft_week |> data.frame()
back_metrics_ft_week_idx

### 6.4.2 smoothed ----
back_metrics_sm_all <- backtest_metric_perf(games_sm, group = NULL)
back_metrics_sm_season <- backtest_metric_perf(games_sm, group = c("season"))
back_metrics_sm_week <- backtest_metric_perf(games_sm, group = c("week"))
back_metrics_sm_week_idx <- backtest_metric_perf(games_sm, group = c("week_idx"))

back_metrics_sm_all |> data.frame()
back_metrics_sm_season |> data.frame()
back_metrics_sm_week |> data.frame()
back_metrics_sm_week_idx

### 6.4.3 predicted ----
back_metrics_pred_all <- backtest_metric_perf(games_pred, group = NULL)
back_metrics_pred_season <- backtest_metric_perf(games_pred, group = c("season"))
back_metrics_pred_week <- backtest_metric_perf(games_pred, group = c("week"))
back_metrics_pred_week_idx <- backtest_metric_perf(games_pred, group = c("week_idx"))

back_metrics_pred_all |> data.frame()
back_metrics_pred_season |> data.frame()
back_metrics_pred_week |> data.frame()
back_metrics_pred_week_idx

## 6.5 Plots ----
### 6.5.1 mu Estimates ----
games_ft |>
  ggplot(aes(week_idx, resid)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "OSA residuals over time", subtitle = "Fitted",
       x = "Week index", y = "result - mu")
games_sm |>
  ggplot(aes(week_idx, resid)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "OSA residuals over time", subtitle = "Smoothed",
       x = "Week index", y = "result - mu")
games_pred |>
  ggplot(aes(week_idx, resid)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "OSA residuals over time", subtitle = "Predicted",
       x = "Week index", y = "result - mu")

#### 6.5.2 Team Strength ----
plot_team_strength_time <- function(team_strength_df, 
                                    week_tbl) {
  team_strength_df |>
  left_join(week_tbl) |>
  ggplot(
    aes(x = week_idx, y = estimate, color = team, group = team,
        text = paste0(
          "team: ", team, "\n",
          sprintf("strength: %.2f", estimate), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
  ) +
  scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 3,  # Use 2, 4, or 8 depending on what fits
      byrow = TRUE,
      title.position = "top"
    )
  )
}
p <- plot_team_strength_time(team_strength_ft, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_team_strength_time(team_strength_sm, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_team_strength_time(team_strength_pred, week_tbl)
p
ggplotly(p, tooltip = "text")

#### 6.5.2 League HFA ----
plot_league_hfa_time <- function(league_hfa_df, 
                                    week_tbl) {
  league_hfa_df |>
  left_join(week_tbl |> select(season, week, week_idx)) |>
  ggplot(
    aes(x = week_idx, y = league_hfa, group = season,
        text = paste0(
          #"team: ", team, "\n",
          sprintf("strength: %.2f", league_hfa), "\n",
          "season: ", season, "\n",
          "week: ", week, "\n",
          "week_idx: ", week_idx
        )
    )
  ) +
  geom_line() +
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = team), alpha = 0.2, color = NA) +
  #facet_wrap(~team) +
  scale_x_continuous(
    name = "Season",
    breaks = first_week_of_season,  # Show season boundaries on x-axis
    labels = seasons
    # sec.axis = dup_axis(
    #   breaks = seq(1, n_weeks, by = 17),  # Show week number at regular intervals (every season start)
    #   labels = rep(1, length(unique(season_ticks$season))), # always week 1 at start
    #   name = "Week Number"
    # )
  ) +
  #scale_color_nfl(guide = guide_legend()) +
  labs(title = "Team Strength Over Time",
       x = "Week", y = "League HFA") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) 
  # guides(
  #   color = guide_legend(
  #     nrow = 3,  # Use 2, 4, or 8 depending on what fits
  #     byrow = TRUE,
  #     title.position = "top"
  #   )
  # )
}
p <- plot_league_hfa_time(league_hfa_ft, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_league_hfa_time(league_hfa_sm, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_league_hfa_time(league_hfa_pred, week_tbl)
p
ggplotly(p, tooltip = "text")


#### 6.5.2 Team HFA ----
plot_team_hfa_time <- function(team_hfa_df, 
                                 week_tbl) {
  team_hfa_df |>
    left_join(week_tbl) |>
    ggplot(
      aes(x = week_idx, y = dev_est, color = team, group = team,
          text = paste0(
            "team: ", team, "\n",
            sprintf("strength: %.2f", dev_est), "\n",
            "season: ", season, "\n",
            "week: ", week, "\n",
            "week_idx: ", week_idx
          )
      )
    ) +
    geom_line() +
    scale_x_continuous(
      name = "Season",
      breaks = first_week_of_season,  # Show season boundaries on x-axis
      labels = seasons
    ) +
    scale_color_nfl(guide = guide_legend()) +
    labs(title = "Team Strength Over Time", x = "Week", y = "Estimated Strength") +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    ) +
    guides(
      color = guide_legend(
        nrow = 3,  # Use 2, 4, or 8 depending on what fits
        byrow = TRUE,
        title.position = "top"
      )
    )
}
p <- plot_team_hfa_time(team_hfa_ft, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_team_hfa_time(team_hfa_sm, week_tbl)
p
ggplotly(p, tooltip = "text")
p <- plot_team_hfa_time(team_hfa_pred, week_tbl)
p
ggplotly(p, tooltip = "text")


games_osa |>
  ggplot(aes(week_idx, resid)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "OSA residuals over time", subtitle = "Fitted",
       x = "Week index", y = "result - mu_osa")

games_osa |>
  ggplot(aes(week_idx, )) +
  geom_line() +
  labs(title = "One-step-ahead league HFA", x = "Week index", y = "HFA")

league_hfa_ft |>
  #dplyr::left_join(week_lookup, by = "week_id") |>
  ggplot(aes(week_idx, league_hfa)) +
  geom_line() +
  labs(title = "One-step-ahead league HFA", x = "Week index", y = "HFA")

teamhfa_sm |>
  #dplyr::left_join(week_lookup, by = "week_id") |>
  ggplot(aes(week_idx, dev_est, color = team, group = team)) +
  geom_line() +
  scale_color_nfl() +
  labs(title = "One-step-ahead league HFA", x = "Week index", y = "HFA")

