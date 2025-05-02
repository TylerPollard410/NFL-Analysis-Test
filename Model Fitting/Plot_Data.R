# Plot data from historical season to gain insight on modeling

library(smplot2)
library(patchwork)
library(plotly)
library(tidyverse)
library(nflverse)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. LOAD & PREPARE DATA ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
#load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modDataLong.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/finalScoresData.rda"))
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/eloData.rda"))

eloData_update_list <- calc_elo_ratings(
  modData,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE
)
eloData <- eloData_update_list$elo_history

# Filter seasons and clean long-format
modDataBase <- modData |>
  left_join(
    finalScoresData |> 
      select(game_id, team, matches("^points\\d+$")) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id)),
    by = join_by(game_id, home_team)
  ) |>
  left_join(
    finalScoresData |> 
      select(game_id, team, matches("^points\\d+$")) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id)),
    by = join_by(game_id, away_team)
  ) |>
  # move all home_points* right after home_score
  relocate(matches("^home_points\\d+$"), .after = home_score) |>
  # then move all away_points* right after away_score
  relocate(matches("^away_points\\d+$"), .after = away_score) |>
  left_join(
    eloData
  )

modData <- modDataBase |> filter(season >= 2007) |>
  mutate(
    winner = case_when(
      home_team == winner ~ TRUE,
      away_team == winner ~ FALSE,
      TRUE ~ NA
    ),
    winner = factor(winner, levels = c(FALSE, TRUE), labels = c("Away", "Home"))
  )

# modData <- modData |> 
#   filter(!is.na(winner))

modDataLong <- modData |> clean_homeaway(invert = c("result", "spread_line"))

# 2.1 Define columns to drop
drop_vars <- c(
  "game_id","game_type","season_type","gameday","gametime",
  "home_team","away_team",
  "home_score", #"home_points8", "home_points7", "home_points6", "home_points3", "home_points2",
  "away_score", #"away_points8", "away_points7", "away_points6", "away_points3", "away_points2",
  "result","spread_line","spreadCover",
  "total","total_line","totalCover",
  "winner",
  "away_spread_odds","away_spread_prob",
  "home_spread_odds","home_spread_prob",
  "over_odds","over_prob","under_odds","under_prob",
  "away_moneyline","away_moneyline_prob",
  "home_moneyline","home_moneyline_prob",
  "overtime","stadium","home_coach","away_coach",
  "home_games_played","home_wins","home_losses","home_ties",
  "away_games_played","away_wins","away_losses","away_ties"
)

game_level_vars <- c(
  "season","week","weekday","time_of_day","location",
  "div_game","home_rest","away_rest","roof","surface","temp","wind"
)

base_cols <- colnames(modData)[colnames(modData) %in% c(drop_vars, game_level_vars)]

# 2.2 Subset raw feature columns
# Note: brms_data should be your modData wide-format dataframe
brms_data <- modData  # adjust if necessary

df_feats <- brms_data |>
  select(-all_of(c(drop_vars, game_level_vars)))

# 2.3 Net‐feature generator function
make_net_features <- function(df) {
  nm <- names(df)
  
  # -- Static nets (home minus away / counterpart)
  statics <- list(
    net_elo           = c("home_elo", "away_elo"),
    net_elo_pre       = c("home_elo_pre", "away_elo_pre"),
    net_elo_post      = c("home_elo_post", "away_elo_post"),
    net_SRS_cum       = c("home_SRS_cum", "away_SRS_cum"),
    home_net_OSRS_cum = c("home_OSRS_cum", "away_DSRS_cum"),
    away_net_OSRS_cum = c("away_OSRS_cum", "home_DSRS_cum")
  )
  for (newnm in names(statics)) {
    c1 <- statics[[newnm]][1]
    c2 <- statics[[newnm]][2]
    if (all(c(c1, c2) %in% nm)) {
      df <- df |> mutate(!!newnm := .data[[c1]] - .data[[c2]])
    }
  }
  
  # -- Home offense vs away defense nets
  home_off <- nm[str_starts(nm, "home_off_")]
  away_def <- nm[str_starts(nm, "away_def_")]
  stems    <- intersect(
    str_remove(home_off, "^home_off_"),
    str_remove(away_def, "^away_def_")
  )
  for (stem in stems) {
    home_nm <- paste0("home_off_", stem)
    away_nm <- paste0("away_def_", stem)
    newnm   <- paste0("home_net_off_", stem)
    if (all(c(home_nm, away_nm) %in% nm)) {
      df <- df |> mutate(
        !!newnm := if (str_detect(stem, "epa")) {
          .data[[home_nm]] + .data[[away_nm]]
        } else {
          .data[[home_nm]] - .data[[away_nm]]
        }
      )
    }
  }
  
  # -- Away offense vs home defense nets
  away_off <- nm[str_starts(nm, "away_off_")]
  home_def <- nm[str_starts(nm, "home_def_")]
  stems    <- intersect(
    str_remove(away_off, "^away_off_"),
    str_remove(home_def, "^home_def_")
  )
  for (stem in stems) {
    away_nm <- paste0("away_off_", stem)
    home_nm <- paste0("home_def_", stem)
    newnm   <- paste0("away_net_off_", stem)
    if (all(c(away_nm, home_nm) %in% nm)) {
      df <- df |> mutate(
        !!newnm := if (str_detect(stem, "epa")) {
          .data[[away_nm]] + .data[[home_nm]]
        } else {
          .data[[away_nm]] - .data[[home_nm]]
        }
      )
    }
  }
  
  return(df)
}

# 2.4 Apply net-feature creation
feats_net <- make_net_features(df_feats)

# 2.5 Helper to order metric columns
get_ordered_metric_cols <- function(df, base) {
  variants <- c("cum", "roll", "ewma")
  roles    <- c("home_net_off", "home_off", "away_def", "away_net_off", "away_off", "home_def")
  purrr::map(variants, function(var) {
    purrr::map_chr(roles, ~ paste0(.x, "_", base, "_", var))
  }) |>
    unlist() |>
    intersect(names(df))
}

# 2.6 Dynamically extract all metric bases
metric_bases <- names(feats_net) |>
  str_subset("^home_net_off_.*_(cum|roll|ewma)$") |>
  str_remove("^home_net_off_") |>
  str_remove("_(cum|roll|ewma)$") |>
  unique()

metric_bases

# 2.7 Build ordered list for dynamic metrics
ordered_cols_list <- purrr::map(metric_bases, function(base) {
  get_ordered_metric_cols(feats_net, base)
})
ordered_cols_flat <- ordered_cols_list |> unlist() |> unique()

# 2.8 Final reordering: static first, then grouped metrics, then all remaining
feats_net_ordered_all <- feats_net |>
  select(
    "net_elo", "home_elo", "away_elo",
    "net_elo_pre", "home_elo_pre", "away_elo_pre",
    "net_elo_post", "home_elo_post", "away_elo_post",
    "net_SRS_cum", "home_SRS_cum", "away_SRS_cum",
    "home_net_OSRS_cum", "home_OSRS_cum", "away_DSRS_cum",
    "away_net_OSRS_cum", "away_OSRS_cum", "home_DSRS_cum",
    contains("MOV"),
    contains("SOS"),
    contains("PFG"),
    contains("PAG"),
    all_of(ordered_cols_flat),
    everything()
  )
colnames(feats_net_ordered_all)
# feats_net_ordered_all now has IDs and static/nets at the front, followed by grouped metrics

# 2.9 Merge with game data 
# brms_data <- modData |>
#   select(1:44)  
# brms_data <- brms_data |>
#   bind_cols(feats_net_ordered_all |> select(-any_of(names(brms_data))))

brms_data <- modData |>
  select(all_of(base_cols))
brms_data <- brms_data |>
  bind_cols(feats_net_ordered_all |> select(-any_of(names(brms_data))))

brms_data_complete <- brms_data %>% 
  select(-any_of(drop_vars), game_id) |>
  filter(if_all(where(is.numeric), ~ !is.na(.))) %>% 
  filter(if_all(where(is.numeric), ~ is.finite(.)))

incomplete_gameIDs <- setdiff(brms_data$game_id, brms_data_complete$game_id)

modData <- brms_data |> filter(!(game_id %in% incomplete_gameIDs))

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. DISTRIBUTIONS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

# Team Scores ----
bin_counts_team_score <- modDataLong |>
  count(team_score) |>        # count occurrences of each score
  arrange(team_score) |>
  mutate(
    prev_n    = lag(n),          # count of previous score
    next_n    = lead(n),         # count of next score
    is_peak = case_when(
      n > prev_n & n > next_n ~ TRUE,  # TRUE if strictly greater than both neighbors
      TRUE ~ NA
    )
  )

dist_plot_team_score <- modDataLong |>
  ggplot(aes(x = team_score)) +
  geom_histogram(binwidth = 1) +
  #geom_freqpoly(binwidth = 1) +
  geom_text(
    aes(label = ifelse(..count.. > lag(..count..) & ..count.. > lead(..count..) | ..x.. == 0,  ..x.., NA),
        y = ..count..), 
    stat = "bin", binwidth = 1,
    vjust = -0.5, size = 3) +
  theme_bw()
dist_plot_team_score

dist_plot_team_score +
  facet_wrap(~ location, nrow = 2)


# Result ----
bin_counts_result <- modDataLong |>
  count(result) |>        # count occurrences of each score
  arrange(result) |>
  mutate(
    prev_n    = lag(n),          # count of previous score
    next_n    = lead(n),         # count of next score
    is_peak = case_when(
      n > prev_n & n > next_n ~ TRUE,  # TRUE if strictly greater than both neighbors
      TRUE ~ NA
    )
  )

dist_plot_result <- modData |>
  ggplot(aes(x = result)) +
  geom_histogram(binwidth = 1) +
  #geom_freqpoly(binwidth = 1) +
  geom_text(
    aes(label = ifelse(..count.. > lag(..count..) & ..count.. > lead(..count..) | ..x.. == 0,  ..x.., NA),
        y = ..count..), 
    stat = "bin", binwidth = 1,
    vjust = -0.5, size = 3) +
  theme_bw()
dist_plot_result


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. POINT SUMMARIES ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

points_season_sum <- modData |>
  group_by(season) |>
  summarise(
    home_score = mean(home_score),
    away_score = mean(away_score),
    result = mean(result),
    total = mean(total)
  ) |>
  pivot_longer(
    cols = -season
  ) |>
  mutate(name = factor(name, levels = c("home_score",
                                        "away_score",
                                        "result",
                                        "total")))

points_season_sum_plot <- points_season_sum |>
  ggplot() +
  geom_line(aes(x = season, y = value, color = name)) +
  facet_wrap(~ name, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(2007, 2024, 1)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

points_week_sum <- modData |>
  group_by(week) |>
  summarise(
    home_score = mean(home_score),
    away_score = mean(away_score),
    result = mean(result),
    total = mean(total)
  ) |>
  pivot_longer(
    cols = -week
  ) |>
  mutate(name = factor(name, levels = c("home_score",
                                        "away_score",
                                        "result",
                                        "total")))

points_week_sum_plot <- points_week_sum |>
  ggplot() +
  geom_line(aes(x = week, y = value, color = name)) +
  facet_wrap(~ name, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(modData$week), max(modData$week), 1)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


points_season_week_sum <- modData |>
  group_by(season, week) |>
  reframe(
    home_score = mean(home_score),
    away_score = mean(away_score),
    result = mean(result),
    total = mean(total)
  ) |>
  mutate(week_num = row_number()) |>
  pivot_longer(
    cols = -c(season, week, week_num)
  ) |>
  mutate(name = factor(name, levels = c("home_score",
                                        "away_score",
                                        "result",
                                        "total")))

points_season_week_sum_plot <- points_season_week_sum |>
  ggplot() +
  geom_line(aes(x = week_num, y = value, color = name)) +
  facet_wrap(~ name, nrow = 2, scales = "free_y") +
  #scale_x_continuous(breaks = seq(min(modData$week), max(modData$week), 1)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. CORRELATIONS ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

## 4.1 Result ----
xVar <- "net_elo"
yVar <- "result"

modData |>
  ggplot(aes(x = spread_line, y = result)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_elo, y = result)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_elo_pre, y = result)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_elo_pre, y = net_elo)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = result)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = net_elo)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = net_elo_pre)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

## 4.2 Total ----
modData |>
  ggplot(aes(x = net_elo, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_elo_pre, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = home_OSRS_cum + away_OSRS_cum, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = home_PFG_cum + away_PFG_cum, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = home_net_off_epa_sum_cum, y = total)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

## 4.3 Home Score ----
modData |>
  ggplot(aes(x = net_elo, y = home_score)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = home_score)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)


## 4.3 Away Score ----
modData |>
  ggplot(aes(x = net_elo, y = away_score)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)

modData |>
  ggplot(aes(x = net_SRS_cum, y = away_score)) +
  geom_point() +
  sm_statCorr() +
  facet_wrap(~ week)


