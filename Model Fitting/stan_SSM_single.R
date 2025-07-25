# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 0. Libraries ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# library(tidytext)
# library(MASS)
library(plotly)
library(patchwork)
# library(doParallel)
# library(rBayesianOptimization)
# library(xgboost)
# library(caret)
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
    game_id, season, season_idx, week, week_idx = week_seq, game_type, season_type,
    home_team, away_team, home_id, away_id,
    location, hfa,
    home_score, away_score, 
    result, spread_line,
    total, total_line
  )

team_fit_data_all <- game_fit_data_all |>
  clean_homeaway(invert = c("result", "spread_line", "hfa"))

game_fit_data <- game_fit_data_all |>
  filter(!is.na(result))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# 3. MODEL ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Model Dictionary ----
# Keep running list of model description for saved .stan files
stan_model_dictionary <- tribble(
  ~model_name, ~data_format, ~description,
  "stan_model1", "game", "State-Space with varying team strengths and hfa by week for each team"
)

# These values can be set to any range you want for any experiment/backtest
first_train_week <- 
  game_fit_data_all |> filter(season == 2007, week == 1)  |> pull(week_idx) |> unique()
last_train_week <- 
  game_fit_data_all |> filter(season == 2023, week == 22) |> pull(week_idx) |> unique()
first_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 1)  |> pull(week_idx) |> unique()
last_oos_week <- 
  game_fit_data_all |> filter(season == 2024, week == 22) |> pull(week_idx) |> unique()

## Compile Stan model ----
stan_model <- cmdstan_model("Model Fitting/stan_models/stan_model3.stan")
mod_vars <- stan_model$variables()

## Stan method variables ----

mod_seed <- 52
mod_output_dir <- "Model Fitting/stan_models"

### MCMC ----
mod_iters <- 1000
mod_warmup <- 500
mod_chains <- 2
mod_thin <- 1
mod_sims <- ((mod_iters)/mod_thin)*mod_chains
mod_parallel_chains <- parallel::detectCores()
mod_adapt_delta <- 0.95
mod_max_treedeepth <- 15

## Stan Data ----

current_train_max <- last_train_week
pred_week <- first_oos_week

### Train data ----
train_data <- game_fit_data_all |>
  filter(week_idx >= first_train_week, week_idx <= current_train_max)

### Prediction Data ----
predict_data <- game_fit_data_all |>
  filter(week_idx == pred_week)

### Stan Indexing ----
fit_seasons    <- sort(unique(train_data$season))
fit_season_idx <- sort(unique(train_data$season_idx))
fit_weeks      <- sort(unique(train_data$week_idx))

week_tbl_rolling <- game_fit_data_all |>
  filter(season %in% fit_seasons) |>
  select(season, season_idx, week, week_idx) |>
  distinct() |>
  arrange(week_idx)

season_start_week <- week_tbl_rolling |>
  group_by(season_idx) |>
  summarise(start = min(week_idx), .groups = "drop") |>
  arrange(season_idx) |>
  pull(start)
season_end_week <- week_tbl_rolling |>
  group_by(season_idx) |>
  summarise(end = max(week_idx), .groups = "drop") |>
  arrange(season_idx) |>
  pull(end)

### Stan Data List ----
stan_data <- list(
  N_games = nrow(train_data),
  N_teams = length(teams),
  N_weeks = max(week_tbl_rolling$week_idx),
  N_seasons = length(fit_season_idx),
  game_week = train_data$week_idx,
  home_id = train_data$home_id,
  away_id = train_data$away_id,
  result = train_data$result,
  game_season = train_data$season_idx,
  week_season = week_tbl_rolling$season_idx,
  season_start_week = season_start_week,
  season_end_week   = season_end_week,
  hfa = as.integer(train_data$hfa)
)


## Run Models ----
### MCMC ----
fit_mcmc <- stan_model$sample(
  data = stan_data,
  output_dir = mod_output_dir,
  chains = mod_chains,
  parallel_chains = mod_parallel_chains,
  iter_sampling = mod_iters, 
  iter_warmup = mod_warmup,
  thin = mod_thin,
  adapt_delta = mod_adapt_delta, 
  max_treedepth = mod_max_treedeepth,
  seed = mod_seed
)

stan_variables <- stan_model$variables()
names(stan_variables$parameters)

# Check sampling diagnostics
fit_mcmc$diagnostic_summary()

fit_mcmc$summary(variables = names(stan_variables$parameters)[-c(1:2)]) |>
  print(n = Inf)

# Example: Team 1 across all weeks
fit_mcmc$summary(variables = paste0("strength_team[", c(1:21, 339:360), ",3]")) |>
  print(n = Inf)

fit_mcmc$cmdstan_diagnose()
fit_mcmc$sampler_diagnostics()

mcmc_draws <- fit_mcmc$draws(
  variables = names(stan_variables$parameters)[-c(1:2)]
)

mcmc_draws |>
  mcmc_trace(window = c(1,50))

fit_mcmc$draws(
  variables = paste0("strength_team[", c(1:25), ",3]")
) |>
  mcmc_trace(window = c(1,50))


### Optimize ----
#### MLE -----
fit_mle <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = FALSE,
  seed = mod_seed,
  init = 0.1
)

#### MAP -----
fit_map <- stan_model$optimize(
  data = stan_data,
  #output_dir = mod_output_dir,
  iter = 4000,
  jacobian = TRUE,
  seed = mod_seed,
  init = 0.1
)

# ---- Diagnostics for MLE and MAP Stan fits ----

# 1. Extract parameter summaries (for all parameters)
names(mod_vars$parameters)
mle_summary <- fit_mle$summary(
  variables = c(
    "sigma",
    "tau_week","tau_season",
    #"tau_hfa", 
    "hfa_league"
    #"rho_week","rho_season"
  ))
map_summary <- fit_map$summary(
  variables = c(
    "sigma",
    "tau_week","tau_season",
    #"tau_hfa", 
    "hfa_league"
    #"rho_week","rho_season"
  ))

# 2. Print main scale parameters for quick check
cat("MLE main scale parameters:\n")
print(mle_summary)
cat("\nMAP main scale parameters:\n")
print(map_summary)

# 1. Extract fitted values as a tibble for easy plotting
# Get predicted values (fitted means) as a tibble, matching your train_data order
fitted_df <- fit_map$draws("fitted", format = "df") |> as_tibble()

# If single row (MAP/optimize), convert to long
fitted_long <- fitted_df |> 
  pivot_longer(cols = everything(), 
               names_to = "game_idx", 
               values_to = "fitted") |> 
  mutate(game_idx = as.integer(str_extract(game_idx, "\\d+")))

# Join to your training data for analysis or plotting
plot_df <- train_data |> 
  mutate(game_idx = row_number()) |> 
  left_join(fitted_long, by = "game_idx")

# Quick plot: fitted vs actual
ggplot(plot_df, aes(x = fitted, y = result)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "MAP Predicted vs Actual (Game Result)",
       x = "Predicted (MAP)", y = "Actual (Result)")

# 2. Merge with actual results
plot_df <- tibble(
  game_idx = seq_along(train_data$result),
  actual = train_data$result
) %>%
  left_join(fitted_df, by = "game_idx") %>%
  mutate(residual = actual - fitted)

# 3. Fitted vs Actual plot (ggplot2)
p1 <- ggplot(plot_df, aes(x = actual, y = fitted)) +
  geom_point(color = "#2B7A77", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2, linewidth = 1) +
  labs(
    x = "Actual Result",
    y = "Fitted (MAP) Result",
    title = "Fitted vs Actual (MAP)"
  ) +
  theme_minimal()
p1

# 4. Residual histogram (ggplot2)
p2 <- ggplot(plot_df, aes(x = residual)) +
  geom_histogram(bins = 40, fill = "#FDB515", color = "white") +
  labs(
    title = "Residuals (MAP)",
    x = "Residual (Actual - Fitted)",
    y = "Count"
  ) +
  theme_minimal()

# 5. Empirical sigma (standard deviation of residuals)
emp_sigma <- sd(plot_df$residual)
cat("Empirical sigma (SD of residuals):", round(emp_sigma, 2), "\n")

# 6. Display plots
print(p1)
print(p2)

# 3. Extract fitted values
fitted_mle <- as.numeric(fit_mle$draws("fitted"))
fitted_map <- as.numeric(fit_map$draws("fitted"))

# 4. Plot fitted vs. actual for MAP
plot(train_data$result, fitted_map, 
     xlab = "Actual Result", ylab = "MAP Fitted Result",
     main = "Fitted vs Actual (MAP)", pch = 16, col = "#2B7A77")
abline(0, 1, col = "red", lty = 2)

# 5. Plot residuals for MAP
residuals_map <- train_data$result - fitted_map
hist(residuals_map, breaks = 40,
     main = "Residuals (MAP)", xlab = "Residual", col = "#FDB515")

# 6. Quick boundary check for key parameters in MAP
map_keypars <- map_summary[grepl("sigma|tau_week|tau_season|tau_hfa|rho_week|rho_season", map_summary$variable), ]
bound_warn <- function(parname, value, lower, upper, tol = 1e-4) {
  if(!is.null(lower) && value <= lower + tol) {
    cat(sprintf("WARNING: %s at lower bound (%.6f)\n", parname, value))
  }
  if(!is.null(upper) && value >= upper - tol) {
    cat(sprintf("WARNING: %s at upper bound (%.6f)\n", parname, value))
  }
}
# Parameters and their bounds
boundaries <- list(
  sigma = c(1e-6, NA),
  tau_week = c(1e-6, NA),
  tau_season = c(1e-6, NA),
  tau_hfa = c(1e-6, NA),
  rho_week = c(-0.95, 0.95),
  rho_season = c(-0.95, 0.95)
)
for(i in seq_len(nrow(map_keypars))) {
  par <- map_keypars$variable[i]
  val <- map_keypars$mean[i]
  bounds <- boundaries[[gsub("\\[.*\\]", "", par)]]
  if(!is.null(bounds)) bound_warn(par, val, bounds[1], bounds[2])
}

# 7. Compare MLE and MAP log posterior
cat(sprintf("\nMLE log posterior: %g\n", mle_summary$value[mle_summary$variable == 'lp__']))
cat(sprintf("MAP log posterior: %g\n", map_summary$value[map_summary$variable == 'lp__']))

# ---- End of diagnostics ----



team_features_data |> 
  group_by(season, week) |>
  summarise(
    mean_SRS = round(mean(SRS), 3),
    mean_elo = round(mean(elo_post), 3)
  ) |>
  ungroup() |>
  print(n = Inf)

team_features_data |> 
  group_by(season) |>
  summarise(
    mean_SRS = round(mean(SRS), 3),
    mean_elo = round(mean(elo_post), 3)
  ) |>
  ungroup() |>
  print(n = Inf)

srs_data |> 
  group_by(season, week) |>
  summarise(
    mean_SRS = round(mean(SRS), 3)
    #mean_elo = round(mean(elo_post), 3)
  ) |>
  ungroup() |>
  print(n = Inf)

srs_data |> 
  group_by(season) |>
  summarise(
    mean_SRS = round(mean(SRS), 3)
    #mean_elo = round(mean(elo_post), 3)
  ) |>
  ungroup() |>
  print(n = Inf)


# Suppose you have:
# game_data: game_id, week, home_team, away_team, result (margin), hfa (indicator)

# Build design matrix for team strengths (N_games x N_teams*N_weeks)
library(Matrix)

# Prepare index lookups
N_games  <- nrow(train_data)
N_teams  <- max(train_data$home_id)
N_weeks  <- max(train_data$week_idx)

# Function to map team-week to column index
col_idx <- function(team, week) (week - 1) * N_teams + team

# Initialize sparse design matrix: N_games x (N_teams*N_weeks + 1)
X <- Matrix(0, nrow = N_games, ncol = N_teams * N_weeks + 1, sparse = TRUE)

for (i in seq_len(N_games)) {
  home_col <- col_idx(train_data$home_id[i], train_data$week_idx[i])
  away_col <- col_idx(train_data$away_id[i], train_data$week_idx[i])
  X[i, home_col] <- 1
  X[i, away_col] <- -1
  X[i, N_teams * N_weeks + 1] <- train_data$hfa[i] # HFA column
}
X
y <- train_data$result

# Sum-to-zero constraint: for each week, add a row so sum(team_strengths) = 0
for (w in 1:N_weeks) {
  constraint_row <- rep(0, N_teams * N_weeks + 1)
  for (t in 1:N_teams) {
    constraint_row[col_idx(t, w)] <- 1
  }
  X <- rbind(X, constraint_row)
  y <- c(y, 0)
}

# Optionally add a small ridge penalty for stability (optional)
lambda <- 1 # or set to 0 if you want pure solution
ridge <- Diagonal(x = c(rep(lambda, N_teams * N_weeks), 0))
fit <- solve(crossprod(X) + ridge, crossprod(X, y))

# Extract team strength estimates
team_strength <- matrix(fit[1:(N_teams * N_weeks)], nrow = N_teams, ncol = N_weeks)
hfa_league <- fit[N_teams * N_weeks + 1]

# Add row/column names if you want
rownames(team_strength) <- sort(unique(train_data$home_id)) # or the team abbreviations if you map them
colnames(team_strength) <- 1:N_weeks

team_abbr <- setNames(unique(train_data$home_team), unique(train_data$home_id))
rownames(team_strength) <- team_abbr[as.character(1:N_teams)]

plot_data <- as_tibble(team_strength, rownames = "team") |>
  pivot_longer(
    -team,
    names_to = "week",
    values_to = "strength"
  ) |>
  mutate(
    week = as.integer(week)
  )

teams_to_plot <- c("KC", "NE", "SF", "DAL", "GB", "BUF", "BAL")
plot_data_sub <- plot_data |> filter(team %in% teams_to_plot)

team_colors <- teams_data$team_color
names(team_colors) <- teams_data$team_abbr
team_colors

ggplot(plot_data_sub, aes(x = week, y = strength, color = team)) +
  #geom_line(size = 1) +
  geom_smooth(se = F) +
  scale_color_manual(values = team_colors) +
  labs(
    title = "Weekly Team Strength (Score Margin Scale)",
    x = "Week",
    y = "Estimated Team Strength",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )



ggplot(plot_data, aes(x = week, y = strength)) +
  geom_line() +
  facet_wrap(~team) +
  theme_minimal() +
  labs(title = "Team Strength Over Time", x = "Week", y = "Strength")

team_strength_trans <- t(team_strength)
team_strength_trans <- round(team_strength_trans, 4)
team_strength_trans <- data.frame(team_strength_trans) |> tibble()
tail(team_strength_trans, 22) |> print(n = Inf)
