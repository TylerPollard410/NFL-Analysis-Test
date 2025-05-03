

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# KFAS State-Space Model for Weekly NFL Team Ratings with Season-Varying HFA
# Dependencies
library(KFAS)
library(dplyr)

# Vary HFA by season ----
# 1) Load and prepare the data
nflBase <- modData
nfl <- nflBase %>%
  # filter(season_type == "REG") %>%
  arrange(season, week) %>%
  mutate(
    diff     = home_score - away_score,
    game_idx = row_number()
  )

# 2) Set up team & season indexing
teams     <- sort(unique(c(nfl$home_team, nfl$away_team)))
n_teams   <- length(teams)

seasons   <- sort(unique(nfl$season))
n_seasons <- length(seasons)

nfl <- nfl %>%
  mutate(
    home_i    = match(home_team, teams),
    away_i    = match(away_team, teams),
    season_i  = match(season, seasons)
  )

N_games <- nrow(nfl)

# 3) Build Z_array with one HFA slot per season
m_states <- n_teams + n_seasons
Z_array  <- array(0, dim = c(1, m_states, N_games))

for(i in seq_len(N_games)) {
  # team‐strength part
  Z_array[1, nfl$home_i[i],   i] <-  1
  Z_array[1, nfl$away_i[i],   i] <- -1
  # season‐specific HFA part
  Z_array[1, n_teams + nfl$season_i[i], i] <- 1
}

# 4) Specify the SSModel
ss_mod <- SSModel(
  nfl$diff ~ -1 +
    SSMcustom(
      Z   = Z_array,
      T   = diag(1, m_states),
      R   = diag(1, m_states),
      Q   = diag(c(rep(NA, n_teams), rep(0, n_seasons))),
      a1  = rep(0, m_states),
      P1  = diag(1e6, m_states)
    ),
  H = matrix(NA)
)

# 5) Fit by maximum likelihood, leaving team‐diffusion free but fixing all HFA‐slots at zero‐noise
update_fn <- function(pars, model) {
  sigma2_w <- exp(pars[1])  # τ² for team random‐walk
  sigma2_v <- exp(pars[2])  # σ² for observation noise
  
  # rebuild Q so first n_teams are τ², the n_seasons are 0
  Qmat <- diag(c(rep(sigma2_w, n_teams), rep(0, n_seasons)))
  Qdims <- dim(model$Q)
  for(t in seq_len(Qdims[3])) model$Q[,,t] <- Qmat
  
  # H is just σ² at every time
  Hdims <- dim(model$H)
  for(t in seq_len(Hdims[3])) model$H[,,t] <- sigma2_v
  
  model
}

inits <- log(c(0.1, 1))
fit   <- fitSSM(ss_mod, inits = inits, updatefn = update_fn, method = "BFGS")

# 6) Kalman smoothing
ks <- KFS(fit$model, smoothing = c("state", "mean"))

ks_alpha_hat <- ks$alphahat  # N_games × m_states
ks_a <- ks$a
ks_att <- ks$att

# 7) Pull out the season‐specific HFA for each game
nfl <- nfl %>%
  mutate(
    home_rating = ks_att[cbind(game_idx, home_i)],
    away_rating = ks_att[cbind(game_idx, away_i)],
    hfa_est = ks_att[cbind(game_idx, n_teams + season_i)],
    home_rating_smooth = ks_alpha_hat[cbind(game_idx, home_i)],
    away_rating_smooth = ks_alpha_hat[cbind(game_idx, away_i)],
    hfa_est_smooth = ks_alpha_hat[cbind(game_idx, n_teams + season_i)],
    home_i = home_team,
    away_i = away_team,
    season_i = season,
    season_ix = n_teams + season_i
  )

# 8) Assemble final data
kfaDataBase <- nfl %>%
  select(
    game_id, season, week,
    home_team, away_team,
    home_rating, away_rating, hfa_est,
    home_rating_smooth, away_rating_smooth, hfa_est_smooth
  )

kfaDataLong <- kfaDataBase |>
  mutate(
    hfa_est = lag(hfa_est, n = 1, default = 0),
    .by = home_team
  ) |>
  clean_homeaway() |>
  mutate(
    team_rating = lag(team_rating, n = 1, default = 0),
    .by = team
  )

kfaData <- kfaDataBase |>
  select(game_id, season, week, home_team, away_team) |>
  left_join(
    kfaDataLong |> select(game_id, home_team = team, home_rating = team_rating, hfa_est),
    by = join_by(game_id, home_team)
  ) |>
  left_join(
    kfaDataLong |> select(game_id, away_team = team, away_rating = team_rating),
    by = join_by(game_id, away_team)
  )

save(kfaData, file = "./app/data/kfaData.rda")

# Contstant HFA ----
# 
# # 2) Set up teams & indexing
# teams    <- sort(unique(c(nfl$home_team, nfl$away_team)))
# n_teams  <- length(teams)
# N_games  <- nrow(nfl)
# 
# nfl <- nfl %>%
#   mutate(home_i = match(home_team, teams),
#          away_i = match(away_team, teams))
# 
# # 3) Build the time-varying design array Z (1 × (n_teams+1) × N_games)
# #    last state is the home-field intercept γ
# m_states <- n_teams + 1
# Z_array  <- array(0, dim = c(1, m_states, N_games))
# for(i in seq_len(N_games)) {
#   Z_array[1, nfl$home_i[i], i]       <-  1
#   Z_array[1, nfl$away_i[i], i]       <- -1
#   Z_array[1, n_teams + 1,    i]      <-  1  # HFA intercept
# }
# 
# # 4) Specify the SSModel with unknown Q (team‐diffusion) and H (obs variances)
# ss_mod <- SSModel(nfl$diff ~ -1 +
#                     SSMcustom(
#                       Z = Z_array,
#                       T = diag(1, m_states),
#                       R = diag(1, m_states),
#                       Q = diag(c(rep(NA, n_teams), 0)),  # last state (HFA) fixed
#                       a1 = rep(0, m_states),
#                       P1 = diag(1e6, m_states)
#                     ),
#                   H = matrix(NA))  # observation variance to estimate
# 
# # 5) Fit by maximum likelihood
# update_fn <- function(pars, model) {
#   sigma2_w <- exp(pars[1])   # τ²
#   sigma2_v <- exp(pars[2])   # σ²
#   
#   # build the r×r “base” Q slice
#   Qmat <- diag(c(rep(sigma2_w, n_teams), 0))
#   
#   # preserve the array dims of model$Q
#   Qdims <- dim(model$Q)      # should be (r, r, time_slices)
#   for (t in seq_len(Qdims[3])) {
#     model$Q[,,t] <- Qmat
#   }
#   
#   # likewise for H (p×p×time)
#   Hdims <- dim(model$H)
#   for (t in seq_len(Hdims[3])) {
#     model$H[,,t] <- sigma2_v
#   }
#   
#   model
# }
# 
# inits <- log(c(0.1, 1))  # log(τ), log(σ)
# fit <- fitSSM(ss_mod, 
#               inits = inits, 
#               updatefn = update_fn, 
#               method = "BFGS")
# 
# # 6) Kalman smoothing to extract the posterior means of the states
# ks <- KFS(fit$model, smoothing = c("state", "mean"))
# 
# # 7) Harvest smoothed ratings and home‐field advantage
# ks_alpha_hat <- ks$alphahat  # matrix N_games × m_states
# ks_a <- ks$a
# 
# nfl <- nfl %>%
#   mutate(
#     home_rating_pre = ks_alpha_hat[cbind(game_idx, home_i)],
#     away_rating_pre = ks_alpha_hat[cbind(game_idx, away_i)],
#     hfa_pre     = ks_alpha_hat[, n_teams + 1],
#     home_rating = ks_a[cbind(game_idx, home_i)],
#     away_rating = ks_a[cbind(game_idx, away_i)],
#     hfa     = ks_a[cbind(game_idx, n_teams + 1)],
#     home_i = home_team,
#     away_i = away_team
#   )
# 
# kfaData <- nfl |>
#   select(
#     game_id, season, week,
#     home_team, away_team,
#     rating_home, rating_away,
#     hfa_est
#   )
# save(kfaData, file = "~/Test Data/BRMS/kfaData_constant_hfa.rda")
