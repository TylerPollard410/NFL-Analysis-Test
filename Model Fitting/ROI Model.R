library(readr)
library(tidytext)
# library(tidyr)
# library(purrr)
library(plotly)
library(patchwork)
library(doParallel)
library(rBayesianOptimization)
library(xgboost)
library(caret)
library(brms)
library(bayesplot)
library(Metrics)  # for MAE, RMSE
library(broom.mixed)
library(tidybayes)
library(nflverse)
library(tidyverse)

set.seed(52)

load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))


load(file = "~/Desktop/NFL Analysis Data/modDataLong.rda")
load(file = "~/Desktop/NFL Analysis Data/modData.rda")

# Step 1: Make long-form dataset with home/away rows ==========================
long_data <- modDataLong |> mutate(is_home = locationID, .after = locationID)

# Build formula.
hfa_formula <- 
  bf(
    result ~
      is_home +
      (1 + is_home|team)
  ) + brmsfamily(family = "gaussian")


hfa_model <- brm(
  formula = hfa_formula,
  data = long_data,
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  save_pars = save_pars(all = TRUE),
  chains = 4, iter = 4000, warmup = 2000,
  cores = parallel::detectCores(),
  normalize = TRUE,
  drop_unused_levels = FALSE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 52
)

# Get posterior means for each team's HFA
team_hfa <- ranef(hfa_model)$team[, , "is_home"] |>
  as.data.frame() |>
  rownames_to_column("team") |>
  rename(hfa_mean = Estimate)


# Merge HFA into wide game data
game_data <- modData |>
  left_join(team_hfa, by = c("home_team" = "team")) |>
  rename(home_hfa = hfa_mean) |>
  mutate(
    hfa = case_when(
      location == "Home" ~ home_hfa,
      TRUE ~ 0
    ),
    elo_diff = home_elo - away_elo,
    .after = location
  )


spread_model <- brm(
  formula = result ~ elo_diff + hfa,
  data = game_data,
  family = gaussian(),
  # prior = c(
  #   prior(normal(0, 5), class = "b"),
  #   prior(exponential(1), class = "sd")
  # ),
  save_pars = save_pars(all = TRUE),
  chains = 4, iter = 4000, warmup = 2000,
  cores = parallel::detectCores(),
  normalize = TRUE,
  drop_unused_levels = FALSE,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 52
)


spread_preds <- posterior_predict(spread_model, newdata = game_data)
spread_mean <- colMeans(spread_preds)

game_data <- game_data |>
  mutate(
    pred_spread = spread_mean,
    residual = result - pred_spread,
    .after = result
  )

rmse_score <- rmse(game_data$result, game_data$pred_spread)
mae_score <- mae(game_data$result, game_data$pred_spread)

# Assume game_data is a data.frame with columns:
#   spread_line : the Vegas spread line for the game (with appropriate sign: negative for home favorites)
#   spread_result : the actual point difference, computed as home_score - away_score
# and that spread_preds is a matrix with dimensions (number of games x number of posterior draws)
#
# Also assume that if spread_result > spread_line then the home team has covered,
# and if spread_result < spread_line then the away team has covered.

# Define a betting probability threshold
prob_threshold <- 0.55

# Compute the probability that each side covers (directionally correct)
game_data <- game_data %>%
  mutate(
    # Home cover probability: proportion of draws where the simulated spread exceeds the spread_line
    home_cover_prob = map_dbl(1:n(), ~ mean(spread_preds[.x, ] > spread_line[.x])),
    # Away cover probability: proportion of draws where the simulated spread is below the spread_line
    away_cover_prob = map_dbl(1:n(), ~ mean(spread_preds[.x, ] < spread_line[.x])),
    .after = pred_spread
  )

# Now decide which side to bet on (if any) using the threshold.
# We bet on the side (home or away) if its probability exceeds the threshold and is higher than the other side.
game_data <- game_data %>%
  mutate(
    bet_choice = case_when(
      home_cover_prob > prob_threshold & home_cover_prob > away_cover_prob ~ "home",
      away_cover_prob > prob_threshold & away_cover_prob > home_cover_prob ~ "away",
      TRUE ~ NA_character_
    )
  ) |>
  relocate(spread_line, .after = pred_spread)

# Determine the actual cover result for each game.
# If the actual spread_result exceeds spread_line, then home covered;
# otherwise, if it is below spread_line, the away team covered.
game_data <- game_data %>%
  mutate(
    actual_cover = case_when(
      result > spread_line ~ "home",
      result < spread_line ~ "away",
      TRUE ~ NA_character_
    ),
    .after = away_cover_prob
  )

# Now evaluate the betting decisions.
# For each bet, if the bet_choice matches the actual_cover, it’s a win.
# Here, we use a standard odds assumption: for -110 odds, a winning bet returns 0.91 profit per unit,
# while a loss costs 1 unit.
game_data <- game_data %>%
  mutate(
    bet_hit = if_else(bet_choice == actual_cover, 1, 0, missing = NA_integer_),
    # Assume: if no bet was placed, payout is zero.
    bet_payout = case_when(
      is.na(bet_choice) ~ 0,
      bet_hit == 1 ~ 0.91,
      bet_hit == 0 ~ -1
    ),
    .after = actual_cover
  )

# Now summarize overall betting performance:
total_bets <- sum(!is.na(game_data$bet_choice))
accuracy <- mean(game_data$bet_hit, na.rm = TRUE) * 100  # percentage correct
roi <- sum(game_data$bet_payout, na.rm = TRUE) / total_bets

cat("Total Bets Placed:", total_bets, "\n")
cat("Betting Accuracy (%):", round(accuracy, 2), "\n")
cat("ROI:", round(roi, 3), "\n")

# Define a grid of thresholds to test
threshold_grid <- seq(0.50, 0.70, by = 0.01)

roi_table <- map_dfr(threshold_grid, function(thresh) {
  # For each threshold, recalculate which side you'd bet on
  df_thresh <- game_data %>%
    mutate(
      bet_choice_thresh = case_when(
        # Bet home if its posterior > thresh AND it's higher than away's
        home_cover_prob > thresh & home_cover_prob > away_cover_prob ~ "home",
        # Bet away if its posterior > thresh AND it's higher than home's
        away_cover_prob > thresh & away_cover_prob > home_cover_prob ~ "away",
        # Otherwise, no bet
        TRUE ~ NA_character_
      ),
      bet_hit_thresh = if_else(bet_choice_thresh == actual_cover, 1, 0, missing = NA_integer_),
      # Standard -110 odds assumption: +0.91 on a win, -1 on a loss, 0 if no bet
      bet_payout_thresh = case_when(
        is.na(bet_choice_thresh) ~ 0,
        bet_hit_thresh == 1      ~ 0.91,
        bet_hit_thresh == 0      ~ -1
      ),
      .after = bet_payout
    )
  
  # Summarize performance for this threshold
  bets_placed    <- sum(!is.na(df_thresh$bet_choice_thresh))
  roi_thresh     <- if (bets_placed > 0) sum(df_thresh$bet_payout_thresh, na.rm = TRUE) / bets_placed else NA_real_
  accuracy_thresh <- if (bets_placed > 0) mean(df_thresh$bet_hit_thresh, na.rm = TRUE) else NA_real_
  
  tibble(
    threshold   = thresh,
    bets_placed = bets_placed,
    roi         = roi_thresh,
    accuracy    = accuracy_thresh
  )
})

# Finally, plot ROI vs. threshold
ggplot(roi_table, aes(x = threshold, y = roi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "ROI by Posterior Betting Confidence Threshold",
    x = "Posterior Threshold",
    y = "ROI"
  )

