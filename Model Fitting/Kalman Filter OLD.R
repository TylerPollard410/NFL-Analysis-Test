# install.packages(c("KFAS", "dplyr"))
library(KFAS)
library(dplyr)

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

# 1) Load and prepare the data
nfl <- modData %>%
  #filter(season_type == "REG") %>%         # regular season only
  arrange(season, week) %>%               # ensure chronological order
  mutate(diff = home_score - away_score,  # point differential
         game_idx = row_number())

# Contstant HFA ----

# 2) Set up teams & indexing
teams    <- sort(unique(c(nfl$home_team, nfl$away_team)))
n_teams  <- length(teams)
N_games  <- nrow(nfl)

nfl <- nfl %>%
  mutate(home_i = match(home_team, teams),
         away_i = match(away_team, teams))

# 3) Build the time-varying design array Z (1 × (n_teams+1) × N_games)
#    last state is the home-field intercept γ
m_states <- n_teams + 1
Z_array  <- array(0, dim = c(1, m_states, N_games))
for(i in seq_len(N_games)) {
  Z_array[1, nfl$home_i[i], i]       <-  1
  Z_array[1, nfl$away_i[i], i]       <- -1
  Z_array[1, n_teams + 1,    i]      <-  1  # HFA intercept
}

# 4) Specify the SSModel with unknown Q (team‐diffusion) and H (obs variances)
ss_mod <- SSModel(nfl$diff ~ -1 +
                    SSMcustom(
                      Z = Z_array,
                      T = diag(1, m_states),
                      R = diag(1, m_states),
                      Q = diag(c(rep(NA, n_teams), 0)),  # last state (HFA) fixed
                      a1 = rep(0, m_states),
                      P1 = diag(1e6, m_states)
                    ),
                  H = matrix(NA))  # observation variance to estimate

# 5) Fit by maximum likelihood
update_fn <- function(pars, model) {
  sigma2_w <- exp(pars[1])   # τ²
  sigma2_v <- exp(pars[2])   # σ²
  
  # build the r×r “base” Q slice
  Qmat <- diag(c(rep(sigma2_w, n_teams), 0))
  
  # preserve the array dims of model$Q
  Qdims <- dim(model$Q)      # should be (r, r, time_slices)
  for (t in seq_len(Qdims[3])) {
    model$Q[,,t] <- Qmat
  }
  
  # likewise for H (p×p×time)
  Hdims <- dim(model$H)
  for (t in seq_len(Hdims[3])) {
    model$H[,,t] <- sigma2_v
  }
  
  model
}

inits <- log(c(0.1, 1))  # log(τ), log(σ)
fit <- fitSSM(ss_mod, 
              inits = inits, 
              updatefn = update_fn, 
              method = "BFGS")
summary(fit)

# 6) Kalman smoothing to extract the posterior means of the states
ks <- KFS(fit$model, smoothing = c("state", "mean"))
ks

# 7) Harvest smoothed ratings and home‐field advantage
alpha_hat <- ks$alphahat  # matrix N_games × m_states
nfl <- nfl %>%
  mutate(
    rating_home = alpha_hat[cbind(game_idx, home_i)],
    rating_away = alpha_hat[cbind(game_idx, away_i)],
    hfa_est     = alpha_hat[, n_teams + 1],
    home_i = home_team,
    away_i = away_team
  )

kfaData <- nfl |>
  select(
    game_id, season, week,
    home_team, away_team,
    rating_home, rating_away,
    hfa_est
  )
save(kfaData, file = "~/Test Data/BRMS/kfaData_constant_hfa.rda")

# Vary HFA by season ----
# 1) Load and prepare the data
nfl <- modData %>%
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
alpha_hat <- ks$alphahat  # N_games × m_states
ks_a <- ks$a

# 7) Pull out the season‐specific HFA for each game
nfl <- nfl %>%
  mutate(
    rating_home_pre = ks_a[cbind(game_idx, home_i)],
    rating_away_pre = ks_a[cbind(game_idx, away_i)],
    rating_home = alpha_hat[cbind(game_idx, home_i)],
    rating_away = alpha_hat[cbind(game_idx, away_i)],
    hfa_est_pre = ks_a[cbind(game_idx, n_teams + season_i)],
    hfa_est = alpha_hat[cbind(game_idx, n_teams + season_i)],
    home_i = home_team,
    away_i = away_team,
    season_i = season
  )

# 8) Assemble final data
kfaData_seasonHFA <- nfl %>%
  select(
    game_id, season, week,
    home_team, away_team,
    rating_home_pre, rating_away_pre, 
    rating_home, rating_away,
    hfa_est_pre, hfa_est
  )


save(kfaData_seasonHFA, file = "~/Test Data/BRMS/kfaData_seasonHFA.rda")

# 8) Example: plot the week-by-week strength of one team (e.g. "NE")
library(ggplot2)
team_to_plot <- "BAL"
idx     <- which(teams == team_to_plot)
plot_df <- nfl %>% 
  select(season, week, game_idx) %>% 
  distinct() %>% 
  arrange(season, week) %>% 
  mutate(
    str   = alpha_hat[, idx],
    label = paste0(season, "W", week)
  )

# pick out the game_idx where each new week/season begins
ticks <- plot_df %>% 
  filter(week == 1) %>%         # e.g. label only the first week of each season
  pull(game_idx)
labs  <- plot_df %>% 
  filter(week == 1) %>% 
  pull(label)

ggplot(plot_df, aes(x = game_idx, y = str)) +
  geom_line() +
  scale_x_continuous(
    breaks = ticks,
    labels = labs
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = paste("Smoothed strength for", team_to_plot),
    x     = "Season+Week",
    y     = expression(hat(theta))
  )

ggplot(nfl %>% mutate(str = alpha_hat[, idx]), 
       aes(x = week, y = str, group = 1)) +
  geom_line() +
  facet_wrap(~ season, ncol = 1, scales = "free_x") +
  labs(
    title = paste("Smoothed strength for", team_to_plot),
    x     = "Week",
    y     = expression(hat(theta))
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.5, "lines")
  )
