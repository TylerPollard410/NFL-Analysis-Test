fit <- fit_prev
snapshot <- snapshot_prev
stan_data_next <- stan_data_next

var_sizes <- fit$metadata()$stan_variable_sizes

# Get fitted dimensions from snapshot
last_s <- snapshot[["last_season_idx"]]  # Number of previously-fitted seasons
last_w <- snapshot[["last_week_idx"]]    # Number of previously-fitted weeks

# New dimensions from next fit (from stan_data)
N_seasons <- stan_data_next$N_seasons
N_weeks   <- stan_data_next$N_weeks
N_teams   <- stan_data_next$N_teams

# Helper for sum-to-zero normal
rnorm_sumzero <- function(n) {
  x <- rnorm(n)
  x - mean(x)
}

# Get previous draws
param_vars <- c("league_hfa_z", "league_hfa_init", "team_hfa_z", "z_start", "z_w")
draws <- fit$draws(format = "df", variables = param_vars)

# league_hfa_z: [N_seasons]
league_hfa_z_prev <- suppressWarnings(
  draws |> dplyr::select(contains("league_hfa_z")) |> 
  colMeans() |> 
  unname()
)
league_hfa_z <- c(
  league_hfa_z_prev[1:last_s],
  if (N_seasons > last_s) rnorm(N_seasons - last_s) else NULL
)

# league_hfa_init: [scalar]
league_hfa_init <- suppressWarnings(
  draws |> dplyr::select(contains("league_hfa_init")) |>
  colMeans() |>
  unname()
)

# team_hfa_z: [N_seasons, N_teams]
# team_hfa_z_prev <- draws |> dplyr::select(contains("team_hfa_z")) |> colMeans()
# team_hfa_z_prev_mat <- matrix(team_hfa_z_prev, nrow = var_sizes[["team_hfa_z"]][1], ncol = var_sizes[["team_hfa_z"]][2], byrow = FALSE)
team_hfa_z_prev <- suppressWarnings(
  draws |> select(contains("team_hfa_z")) |> 
  colMeans() |>
  matrix(nrow = var_sizes[["team_hfa_z"]][1], 
         ncol = var_sizes[["team_hfa_z"]][2], 
         byrow = FALSE) |>
  t() |> scale(scale = FALSE) |> t()
)
# Build full [N_seasons, N_teams]
team_hfa_z <- matrix(NA, nrow = N_seasons, ncol = N_teams)
if (last_s > 0) team_hfa_z[1:last_s, ] <- team_hfa_z_prev[1:last_s, , drop = FALSE]
if (N_seasons > last_s) {
  for (s in (last_s+1):N_seasons) team_hfa_z[s, ] <- rnorm_sumzero(N_teams)
}

# z_start: [N_seasons, N_teams]
# z_start_prev <- draws |> dplyr::select(contains("z_start")) |> colMeans()
# z_start_prev_mat <- matrix(z_start_prev, nrow = var_sizes[["z_start"]][1], ncol = var_sizes[["z_start"]][2], byrow = FALSE)
z_start_prev <- suppressWarnings(
  draws |> select(contains("z_start")) |> 
  colMeans() |>
  matrix(nrow = var_sizes[["z_start"]][1], 
         ncol = var_sizes[["z_start"]][2], 
         byrow = FALSE) |>
  t() |> scale(scale = FALSE) |> t()
)
z_start <- matrix(NA, nrow = N_seasons, ncol = N_teams)
if (last_s > 0) z_start[1:last_s, ] <- z_start_prev[1:last_s, , drop = FALSE]
if (N_seasons > last_s) {
  for (s in (last_s+1):N_seasons) z_start[s, ] <- rnorm_sumzero(N_teams)
}

# z_w: [N_weeks, N_teams]
# z_w_prev <- draws |> dplyr::select(contains("z_w")) |> colMeans()
# z_w_prev_mat <- matrix(z_w_prev, nrow = var_sizes[["z_w"]][1], ncol = var_sizes[["z_w"]][2], byrow = FALSE)
z_w_prev <- suppressWarnings(
  draws |> select(contains("z_w")) |> 
  colMeans() |>
  matrix(nrow = var_sizes[["z_w"]][1], 
         ncol = var_sizes[["z_w"]][2], 
         byrow = FALSE) |>
  t() |> scale(scale = FALSE) |> t()
)
z_w <- matrix(NA, nrow = N_weeks, ncol = N_teams)
if (last_w > 0) z_w[1:last_w, ] <- z_w_prev[1:last_w, , drop = FALSE]
if (N_weeks > last_w) {
  for (w in (last_w+1):N_weeks) z_w[w, ] <- rnorm_sumzero(N_teams)
}

# Scalar params as before
inits_test <- list(
  league_hfa_z = league_hfa_z,
  league_hfa_init = league_hfa_init,
  beta_hfa = mean(snapshot$beta_hfa),
  sigma_hfa = abs(mean(snapshot$sigma_hfa)),
  team_hfa_z = team_hfa_z,
  sigma_team_hfa = abs(mean(snapshot$sigma_team_hfa)),
  z_start = z_start,
  z_w = z_w,
  beta_w = mean(snapshot$beta_w),
  sigma_w = abs(mean(snapshot$sigma_w)),
  beta_s = mean(snapshot$beta_s),
  sigma_s = abs(mean(snapshot$sigma_s)),
  sigma_y = abs(mean(snapshot$sigma_y))
)

all(abs(rowSums(inits_test$team_hfa_z)) < 1e-8)
all(abs(rowSums(inits_test$z_start)) < 1e-8)
all(abs(rowSums(inits_test$z_w)) < 1e-8)

any(is.na(unlist(inits_test)))
any(is.nan(unlist(inits_test)))
any(is.infinite(unlist(inits_test)))


# Check inits constraint
league_hfa_z_prev
league_hfa_z

league_hfa_init

team_hfa_z_prev[seq(last_s, length(team_hfa_z_prev), N_seasons)]
team_hfa_z_prev_mat[last_s, ]
team_hfa_z[last_s, ]
sum(team_hfa_z[last_s + 1, ])

z_start_prev
z_start_prev_mat
z_start

z_w_prev
z_w_prev_mat
z_w

rm(
  fit, snapshot, 
  var_sizes,
  last_s, last_w,
  N_seasons, N_weeks, N_teams,
  rnorm_sumzero,
  param_vars, draws,
  league_hfa_z_prev, league_hfa_z,
  league_hfa_init,
  team_hfa_z_prev, team_hfa_z_prev_mat, team_hfa_z,
  z_start_prev, z_start_prev_mat, z_start,
  z_w_prev, z_w_prev_mat, z_w,
  inits_test
)

