## Model home and away score

# Load Libraries ----
## Data Manipulation
require(stringr)

## Tables
# library(DBI)
# library(RPostgres)
# library(data.table)

## Plotting
# library(smplot2)
# library(patchwork)

## Modeling
# library(zoo)
require(pracma)
# library(forecast)
require(timetk)
# library(elo)
# library(MASS)
# library(bestNormalize)
# library(tictoc)
# library(caret)
# library(splines)
# library(mgcv)
# library(DescTools)
# library(car)
# library(bayesplot)
# library(BayesFactor)
# library(cmdstanr)
# library(rstanarm)
# library(tidybayes)
# library(loo)
# library(brms)
# library(performance)

## NFL Verse
require(nflverse)

## Tidyverse
require(tidyverse)

#source("./app/data-raw/gameData.R")
#source("./app/data-raw/gameDataLong.R")
#source("./app/data-raw/pbpData.R")

cat("Generating EPA Data", "\n")

# EPA ----
## Penalty Structure 1 ----
# epaOffData <- pbpData |>
#   filter(season %in% seasonsMod) |>
#   #filter(play == 1) |> 
#   filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
#   group_by(game_id, season, week, posteam, home_team, away_team) |>
#   mutate(
#     scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
#   ) |>
#   summarise(
#     # Total EPA
#     # off_plays = sum(play == 1),
#     # off_epa_sum = sum(epa[play == 1], na.rm = TRUE),
#     # off_epa_mean = mean(epa[play == 1], na.rm = TRUE),
#     off_plays = n(),
#     off_epa_sum = sum(epa, na.rm = TRUE),
#     off_epa_mean = mean(epa, na.rm = TRUE),
#     # Passing EPA
#     off_pass_plays = sum(play == 1 & pass == 1),
#     off_pass_epa_sum = sum(epa[play == 1 & pass == 1], na.rm = TRUE),
#     off_pass_epa_mean = mean(epa[play == 1 & pass == 1], na.rm = TRUE),
#     # Rushing EPA
#     off_rush_plays = sum(play == 1 & rush == 1),
#     off_rush_epa_sum = sum(epa[play == 1 & rush == 1], na.rm = TRUE),
#     off_rush_epa_mean = mean(epa[play == 1 & rush == 1], na.rm = TRUE),
#     # Pre Snap Penalty EPA
#     off_penalty_plays = sum(play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1, na.rm = TRUE),
#     off_penalty_epa_sum = sum(epa[play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
#     off_penalty_epa_mean = mean(epa[play == 1 & pass == 0 & rush == 0 & special == 0 & penalty == 1], na.rm = TRUE),
#     # Kicker EPA
#     off_kick_plays = sum(play_type %in% c("field_goal"), na.rm = TRUE),
#     off_kick_epa_sum = sum(epa[play_type %in% c("field_goal")], na.rm = TRUE),
#     off_kick_epa_mean = mean(epa[play_type %in% c("field_goal")], na.rm = TRUE),
#     # Special Teams EPA
#     off_special_plays = sum(special == 1 & play_type != "field_goal", na.rm = TRUE),
#     off_special_epa_sum = sum(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
#     off_special_epa_mean = mean(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
#   ) |>
#   mutate(
#     across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
#   ) |>
#   ungroup() |>
#   group_by(game_id) |>
#   mutate(
#     opponent = rev(posteam), .after = posteam
#   ) |>
#   rename(
#     team = posteam
#   ) |>
#   ungroup()

## Penalty Sturcture 2 ----
## STEP 1: Compute game-level offensive EPA metrics from pbpData ----
epaOffData <- pbpData |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2) |>
  summarise(
    # Total EPA
    off_plays        = sum(play == 1 | play_type == "field_goal", na.rm = TRUE),
    off_epa_sum      = sum(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    off_epa_mean     = mean(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    # Passing EPA
    off_pass_plays   = sum(play == 1 & pass == 1 & penalty == 0, na.rm = TRUE),
    off_pass_epa_sum = sum(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    off_pass_epa_mean= mean(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # Rushing EPA
    off_rush_plays   = sum(play == 1 & rush == 1 & penalty == 0, na.rm = TRUE),
    off_rush_epa_sum = sum(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    off_rush_epa_mean= mean(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    # Pre Snap Penalty EPA
    off_penalty_plays   = sum(play == 1 & penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[play == 1 & penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean= mean(epa[play == 1 & penalty == 1], na.rm = TRUE),
    # Kicker EPA
    off_kick_plays   = sum(play_type %in% c("field_goal"), na.rm = TRUE),
    off_kick_epa_sum = sum(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    off_kick_epa_mean= mean(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    # Special Teams EPA
    off_special_plays   = sum(special == 1 & play_type != "field_goal", na.rm = TRUE),
    off_special_epa_sum = sum(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    off_special_epa_mean= mean(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    # Play distribution
    off_pass_plays_perc = off_pass_plays/(off_pass_plays + off_rush_plays),
    off_rush_plays_perc = off_rush_plays/(off_pass_plays + off_rush_plays)
  ) |>
  mutate(across(contains("off"), ~ifelse(is.nan(.x), 0, .x))) |>
  ungroup() |>
  # Create opponent variable by reversing the posteam order within each game
  group_by(game_id) |>
  mutate(opponent = rev(posteam), .after = posteam) |>
  rename(team = posteam) |>
  ungroup()

## STEP 2: Merge defensive EPA metrics ----
# Here we assume that a team's defensive EPA can be represented by the opponent’s offensive metrics.
epaData <- epaOffData |>
  left_join(
    epaOffData |>
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )
#colnames(epaData)

## STEP 3: Merge EPA data with game-level ordering (from gameDataLong) ----
epaFeatures <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(
    epaData |> 
      select(game_id, team, opponent, 
             contains("epa_mean"),
             contains("plays")),
    by = join_by(game_id, team, opponent)
  )


### Define EPA metric columns for aggregation
epa_cols <- epaFeatures |>
  select(contains("epa_mean"), contains("plays")) |>
  colnames()

## STEP 4: Season-Specific Aggregates (Cumulative Mean) ----
# For each season and team, order by week, lag each EPA metric by one game, then compute cumulative mean.
epaSeason <- epaFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(epa_cols), #contains(epa_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 

## check
# epaFeatures |>
#   filter(season == 2023, team == "KC") |>
#   pull(off_epa_mean) |>
#   mean()

## STEP 5: Multi-Season Aggregates (5-Game Rolling Average) ----
# For continuity across seasons, group by team (ordered by season then week), lag, then compute a 5-game rolling average.
epaMulti <- epaFeatures |>
  mutate(
    across(all_of(epa_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(epa_cols), #c(contains(epa_cols), -contains("roll"))
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(epa_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

# check
# epaMulti |> filter(season > 2006) |> nrow()
# epaMulti |> filter(season > 2006) |> complete.cases() |> sum()

## STEP 6: Combine EPA Features ----
epaNets <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  #left_join(epaFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(epaSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(epaMulti,  by = join_by(game_id, season, week, team, opponent))

epaFinal_ordered <- epaNets |>
  mutate(across(starts_with("off_"),
                .names = "{.col}_net") -
           across(starts_with("def_"))
  ) |> 
  rename_with(~ str_replace(., "^off_(.+)_net$", "net_\\1"), ends_with("_net")) |>
  select(
    game_id, season, week, team, opponent,
    contains("cum"), contains("roll"), contains("ewma")
  )
#epaFinal |> filter(season == 2024, week == 18) |> View()

## STEP 7: Reorder EPA Features -----
# Define the identifier columns.
id_cols <- c("game_id", "season", "week", "team", "opponent")

# Select all feature columns (everything except the id columns).
epa_feature_cols <- epaFinal_ordered |>
  select(-all_of(id_cols)) |>
  colnames()

# Pivot the aggregated features from wide to long format.
epa_long <- epaFinal_ordered |>
  pivot_longer(
    cols = contains(epa_feature_cols),
    names_to = "varname",
    values_to = "value"
  ) |>
  # Extract the three components from each column name.
  # We assume names match the pattern: (net|off|def)_(base)_(cum|roll|ewma)
  extract(varname, into = c("prefix", "base", "agg"),
          regex = "^(net|off|def)_(.+)_(cum|roll|ewma)$") |>
  # Set factor levels to enforce the desired order:
  # For aggregation: cum comes first, then roll, then ewma.
  # For prefix: net comes first, then off, then def.
  mutate(
    agg = factor(agg, levels = c("cum", "roll", "ewma")),
    prefix = factor(prefix, levels = c("net", "off", "def"))
  ) |>
  arrange(base, agg, prefix) |>
  # Reassemble a new variable name from the sorted parts.
  unite("newvar", prefix, base, agg, sep = "_")

# Pivot back to wide format so that each newvar becomes a column.
epa_wide <- epa_long |>
  pivot_wider(names_from = newvar, values_from = value)

# Now, reattach the identifier columns.
epaFinal <- epaFinal_ordered |>
  select(all_of(id_cols)) |>
  left_join(epa_wide, by = id_cols)

# epaFinal_ordered now has columns ordered as:
# net_pass_epa_mean_cum, off_pass_epa_mean_cum, def_pass_epa_mean_cum,
# net_pass_epa_mean_roll, off_pass_epa_mean_roll, def_pass_epa_mean_roll,
# net_pass_epa_mean_ewma, off_pass_epa_mean_ewma, def_pass_epa_mean_ewma, etc.

# epaFinal now contains:
# • The base EPA metrics per game (from epaFeatures)
# • Season-specific cumulative aggregates (e.g., off_epa_mean_lag_season_cum_season)
# • Multi-season rolling aggregates (e.g., off_epa_mean_lag_multi_roll_multi)
# • Multi-season EWMA aggregates (e.g., off_epa_mean_lag_multi_ewma_multi)
# epaFinal

rm(list = setdiff(ls(pattern = "epa"), c("epaFinal", "epa_cols", "epa_feature_cols")))

cat("Finished EPA Data", "\n")

# SRS ----
cat("Generating SRS Data", "\n")

## STEP 1: Extract Base SRS Values from seasonWeekStandings ----
# (Assuming seasonWeekStandings already contains cumulative, week-by-week computed SRS values)
srsData <- seasonWeekStandings |>
  select(season, week, team, 
         PFG = team_PPG,   # Points For per game
         PAG = opp_PPG,    # Points Against per game
         MOV, SOS, SRS, OSRS, DSRS)

srs_cols <- c("PFG", "PAG", "MOV", "SOS", "SRS", "OSRS", 'DSRS')

## STEP 2: Merge SRS Features with Game-Level Data ----
# Use gameDataLong to enforce consistent ordering (one row per game).
srsFeatures <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(srsData, by = join_by(season, week, team))

## STEP 3: Create Season-Specific (Cum) Features by Lagging ####
# Since the raw values are already cumulative (and normalized per week),
# we simply lag them so that each game’s predictor uses only prior data.
srsSeason <- srsFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(srs_cols), #contains(epa_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 
# The lagged values here serve as your “cum” feature since the raw values are cumulative.

## STEP 4: Compute Multi-Season Rolling Averages ####
# These features are computed across seasons (grouped by team)
# They will help smooth out the volatility (especially early in the season)
srsMulti <- srsFeatures |>
  #group_by(team) |>
  #arrange(season, week) |>
  mutate(
    across(all_of(srs_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(srs_cols),
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(srs_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

## STEP 5: Combine All SRS Features ----
# Join the lagged ("cum") features, the rolling averages, and the EWMA features together.
srsFinal <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  #left_join(epaFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(srsSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(srsMulti,  by = join_by(game_id, season, week, team, opponent))

# Select all feature columns (everything except the id columns).
srs_feature_cols <- srsFinal |>
  select(-all_of(id_cols)) |>
  colnames()

#setdiff(ls(pattern = "srs"), c("srsFinal", "srs_cols", "srs_feature_cols"))
rm(list = setdiff(ls(pattern = "srs"), c("srsFinal", "srs_cols", "srs_feature_cols")))

# ELO ----
cat("Generating ELO Data", "\n")

## STEP 1: Extract Base SRS Values from seasonWeekStandings ----
# (Assuming seasonWeekStandings already contains cumulative, week-by-week computed SRS values)

# Adjust if needed instead of using all prior data
# eloDataList <- gameData |>
#   filter(!is.na(home_score), !is.na(away_score)) |>
#   calc_elo_ratings(
#     initial_elo = 1500,
#     K = 20,
#     home_advantage = 0,
#     d = 400,
#     apply_margin_multiplier = TRUE
#   )

eloDataHistory <- eloData

eloData <- eloDataHistory |>
  clean_homeaway()

### STEP 1B: Alternate restarting elo each season ----
# library(purrr)
# 
# # Assuming gameData already exists
# eloDataSeason <- allSeasons |>
#   set_names() |> # names list elements by season
#   map(~ gameData |>
#         filter(season == .x, !is.na(home_score), !is.na(away_score)) |>
#         calc_elo_ratings(
#           initial_elo = 1500,
#           K = 20,
#           home_advantage = 0,
#           d = 400,
#           apply_margin_multiplier = TRUE
#         )
#   )
# 
# # Example access
# season_elos$`2023`$elo_history
# season_elos$`2023`$final_ratings
# season_elos$`2023`$team_ratings
# 
# # Combine elo_history across seasons into one dataframe
# elo_history_all <- map_dfr(season_elos, "elo_history", .id = "season")
# 
# # Combine final ratings across seasons into one dataframe
# final_ratings_all <- map_dfr(season_elos, 
#                              ~data.frame(team = names(.x$final_ratings),
#                                          elo = .x$final_ratings),
#                              .id = "season")
# 
# # Combine weekly team ratings across seasons into one dataframe
# team_ratings_all <- map_dfr(season_elos, "team_ratings", .id = "season_calc")


#elo_cols <- c("PFG", "PAG", "MOV", "SOS", "SRS", "OSRS", 'DSRS')

## STEP 2: Merge ELO Features with Game-Level Data ----
# Use gameDataLong to enforce consistent ordering (one row per game).
eloFeatures <- eloData |>
  select(game_id, team, opponent,
         team_elo = team_elo_pre,
         opponent_elo = opponent_elo_pre)

## STEP 3: Create Season-Specific (Cum) Features by Lagging ####
# Since the raw values are already cumulative (and normalized per week),
# we simply lag them so that each game’s predictor uses only prior data.
# eloSeason <- eloFeatures |>
#   #group_by(season, team) |>
#   #arrange(week) |>
#   mutate(
#     across(all_of(elo_cols), #contains(epa_cols),
#            ~cummean(.x),
#            .names = "{.col}_cum"),
#     .by = c(season, team),
#     .keep = "unused"
#   ) |>
#   mutate(
#     across(contains("_cum"), 
#            ~lag(.x, n = 1, default = NA)),
#     .by = c(team)
#   ) 
# The lagged values here serve as your “cum” feature since the raw values are cumulative.

## STEP 4: Compute Multi-Season Rolling Averages ####
# These features are computed across seasons (grouped by team)
# They will help smooth out the volatility (especially early in the season)
# eloMulti <- eloFeatures |>
#   #group_by(team) |>
#   #arrange(season, week) |>
#   mutate(
#     across(all_of(elo_cols),
#            ~slidify_vec(
#              .x = .x,
#              .period  = 5,
#              .f       = mean,
#              .partial = TRUE,
#              .align   = "right"
#            ),
#            .names = "{.col}_roll"),
#     .by = c(team),
#     .keep = "all"
#   ) |> #arrange(season, team, week)
#   mutate(
#     across(all_of(elo_cols),
#            ~movavg(.x, n = 5, type = "e"),
#            .names = "{.col}_ewma"),
#     .by = c(team),
#     .keep = "all"
#   ) |> 
#   select(-all_of(elo_cols)) |>
#   mutate(
#     across(c(contains("_roll"), contains("_ewma")),
#            ~lag(.x, n = 1, default = NA)),
#     .by = c(team),
#     .keep = "unused"
#   ) #|> arrange(season, team, week)

## STEP 5: Combine All SRS Features ----
# Join the lagged ("cum") features, the rolling averages, and the EWMA features together.
eloFinal <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(eloFeatures, by = join_by(game_id, team, opponent)) 
# left_join(eloSeason, by = join_by(game_id, season, week, team, opponent)) |>
# left_join(eloMulti,  by = join_by(game_id, season, week, team, opponent))

# Select all feature columns (everything except the id columns).
elo_feature_cols <- eloFinal |>
  select(-all_of(id_cols)) |>
  colnames()

setdiff(ls(pattern = "elo"), 
        c("eloFinal", "elo_cols", "elo_feature_cols", "calc_elo_ratings"))
rm(list = setdiff(ls(pattern = "elo"), 
                  c("eloFinal", "elo_cols", "elo_feature_cols", "calc_elo_ratings"))
)


# Efficiency Stats ----
cat("Generating Efficiency Stats Data", "\n")

nflStatsWeek_loc <- paste0("~/Desktop/NFLAnalysisTest/scripts/UpdateData/PriorData/",
                           "nflStatsWeek.rda")

if(file.exists(nflStatsWeek_loc)){
  load(file = nflStatsWeek_loc)
  nflStatsWeek <- nflStatsWeek |>
    filter(season != get_current_season())
  nflStatsWeekTemp <- calculate_stats(seasons = get_current_season(),
                                      summary_level = "week",
                                      stat_type = "team",
                                      season_type = "REG+POST") 
  nflStatsWeek <- bind_rows(nflStatsWeek, nflStatsWeekTemp)
}else{
  nflStatsWeek <- calculate_stats(seasons = allSeasons,
                                  summary_level = "week",
                                  stat_type = "team",
                                  season_type = "REG+POST") 
}

save(nflStatsWeek, file = nflStatsWeek_loc)

# Scores Stats ----
cat("Generating Scores Data", "\n")

## STEP 1: Calculate Weekly Efficiency/Scoring Stats ----
scoresData <- nflStatsWeek |>
  select(
    season, week, team, 
    passing_tds, 
    rushing_tds, 
    td_special = special_teams_tds,
    def_tds,
    fumble_recovery_tds, 
    passing_2pt_conversions, 
    rushing_2pt_conversions,
    pat_att,
    pat_made, #pat_pct,
    fg_att,
    fg_made, #fg_pct, 
    safeties_def = def_safeties
    # interceptions_lost = passing_interceptions,
    # sack_fumbles_lost, rushing_fumbles_lost, receiving_fumbles_lost,
    # interceptions_won = def_interceptions,
    # fumbles_forced = def_fumbles_forced,
    # fumbles_won = def_fumbles
  )

### STEP 1B: Fix Wrong data ----
#scoresData |> filter(season == 2011, week == 13, team == "DET") |> View()
scoresGameFilter2011_13_DET <- 
  scoresData$season == 2011 & scoresData$week == 13 & scoresData$team == "DET"
scoresData$rushing_tds[scoresGameFilter2011_13_DET] <- 1
scoresData$pat_att[scoresGameFilter2011_13_DET] <- 2
scoresData$pat_made[scoresGameFilter2011_13_DET] <- 2

#scoresData |> filter(season == 2019, week == 15, team == "ARI") |> View()
scoresGameFilter2019_15_ARI <- 
  scoresData$season == 2019 & scoresData$week == 15 & scoresData$team == "ARI"
scoresData$rushing_tds[scoresGameFilter2019_15_ARI] <- 4
scoresData$td_special[scoresGameFilter2019_15_ARI] <- 0

#scoresData |> filter(season == 2019, week == 15, team == "CLE") |> View()
scoresGameFilter2019_15_CLE <- 
  scoresData$season == 2019 & scoresData$week == 15 & scoresData$team == "CLE"
scoresData$passing_tds[scoresGameFilter2019_15_CLE] <- 2
scoresData$pat_att[scoresGameFilter2019_15_CLE] <- 3
scoresData$pat_made[scoresGameFilter2019_15_CLE] <- 3


## STEP 2: Aggregate scores ----
scoresDataAgg <- scoresData |>
  mutate(
    td_off = passing_tds + rushing_tds,
    td_def = def_tds + fumble_recovery_tds,
    td_total = td_off + td_special + td_def
    #two_pt_made = passing_2pt_conversions + rushing_2pt_conversions,
    #two_pt_att = td_total - pat_att
    # fumbles_lost = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
    # turnovers_won = interceptions_won + fumbles_forced,
    # turnovers_won2 = interceptions_won + fumbles_won,
    # turnovers_lost = interceptions_lost + fumbles_lost,
    # turnovers_diff = turnovers_won - turnovers_lost,
    # turnovers_diff2 = turnovers_won2 - turnovers_lost
  )

scores_two_pt_df <- pbpData |> 
  select(
    game_id, season, week,
    posteam, defteam,
    two_point_attempt,
    two_point_conv_result,
    defensive_two_point_conv, 
    defensive_extra_point_conv
  ) |>
  filter(!is.na(posteam)) |> 
  #filter(two_point_attempt == 1) |> #filter(!is.na(two_point_conv_result))
  mutate(two_point_made = ifelse(two_point_conv_result == "success", 1, 0)) |>
  summarise(
    two_pt_att = sum(two_point_attempt, na.rm = TRUE),
    two_pt_made = sum(two_point_made, na.rm = TRUE),
    two_pt_def = sum(defensive_two_point_conv, na.rm = TRUE),
    .by = c(season, week, posteam)
  )

scoresDataAgg <- scoresDataAgg |>
  left_join(scores_two_pt_df, by = join_by(season, week, team == posteam)) 

## STEP 3: Join with game-level ordering from gameDataLong ----
scores_cols<- c(
  "td_off",
  "td_def",
  "td_special",
  "td_total",
  "fg_made",
  "fg_att",
  "pat_made",
  "pat_att",
  "two_pt_made",
  "two_pt_att",
  "two_pt_def",
  #"two_pt_made2",
  #"two_pt_att2",
  #"two_pt_def2",
  "safeties_def"
  #"passing_tds", 
  #"rushing_tds", 
  #"special_teams_tds", 
  #"def_tds",
  #"fumble_recovery_tds"
  # "passing_2pt_conversions", 
  # "rushing_2pt_conversions",
  # "pat_att",
  # "pat_made", 
  # "fg_att", 
  # "fg_made", 
  # "def_safeties",
  # "off_TD",
  # "totalTD",
  # "twoPtConv",
  # "twoPtAtt"
)

scoresFeatures <- gameDataLong |>
  select(all_of(id_cols)) |>
  left_join(
    scoresDataAgg |> select(season, week, team, all_of(scores_cols)),
    by = join_by(season, week, team)) |>
  mutate(
    two_pt_def = rev(two_pt_def),
    .by = c(game_id)
  ) #|>
#mutate(rowID = row_number())

# check
scores_check <- scoresFeatures |>
  left_join(gameDataLong |> select(game_id, team, team_score, opponent_score),
            by = join_by(game_id, team)) |>
  mutate(
    team_score2 = 6*td_total + 3*fg_made + pat_made + 2*two_pt_made + 2*safeties_def + 2*two_pt_def,
    .after = team_score
  ) |>
  filter(team_score != team_score2)


## STEP 4: Season-Specific Aggregates (Cumulative Mean) ----
# For each season and team, order by week, lag each EPA metric by one game, then compute cumulative mean.
scoresSeason <- scoresFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(scores_cols), #contains(scores_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 

## check
# scoresFeatures |>
#   filter(season == 2023, team == "KC") |>
#   pull(off_scores_mean) |>
#   mean()

## STEP 5: Multi-Season Aggregates (5-Game Rolling Average) ----
# For continuity across seasons, group by team (ordered by season then week), lag, then compute a 5-game rolling average.
scoresMulti <- scoresFeatures |>
  mutate(
    across(all_of(scores_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(scores_cols), #c(contains(scores_cols), -contains("roll"))
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(scores_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

# check
# scoresMulti |> filter(season > 2006) |> nrow()
# scoresMulti |> filter(season > 2006) |> complete.cases() |> sum()

## STEP 6: Combine scores Features ----
scoresFinal <- gameDataLong |>
  select(all_of(id_cols)) |>
  #left_join(scoresFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(scoresSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(scoresMulti,  by = join_by(game_id, season, week, team, opponent))

# Select all feature columns (everything except the id columns).
scores_feature_cols <- scoresFinal |>
  select(-all_of(id_cols)) |>
  colnames()

setdiff(ls(pattern = "scores"), c("scoresFinal", "scores_cols", "scores_feature_cols"))
rm(list = setdiff(ls(pattern = "scores"), c("scoresFinal", "scores_cols", "scores_feature_cols")))

# Turnovers ----
cat("Generating Turnover Data", "\n")

## STEP 1: Calculate Weekly Turnover Stats ----
turnoverData <- pbpData |>
  select(#play_id,
    game_id, season, week,
    posteam, defteam,
    interception, fumble_forced, fumble_lost, fumble
  ) |>
  filter(!is.na(posteam)) |>
  #group_by(game_id, season, week, posteam) |>
  summarise(
    across(c(interception, fumble_forced, fumble_lost, fumble),
           ~sum(.x, na.rm = TRUE)),
    .by = c(game_id, season, week, posteam, defteam),
  ) |>
  rename(interception_lost = interception) |>
  mutate(turnover_lost = interception_lost + fumble_lost) |>
  mutate(
    interception_won = rev(interception_lost),
    fumble_won = rev(fumble_lost),
    turnover_won = rev(turnover_lost),
    .by = game_id
  ) |>
  select(-fumble_forced, -fumble) |>
  mutate(turnover_diff = turnover_won - turnover_lost)

turnover_cols <- c(
  "turnover_diff",
  "turnover_won",
  "turnover_lost",
  "interception_won",
  "interception_lost",
  "fumble_won",
  "fumble_lost"
)

## STEP 2: Merge turnover Features with Game-Level Data ----
# Use gameDataLong to enforce consistent ordering (one row per game).
turnoverFeatures <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(turnoverData |> 
              select(game_id, posteam, all_of(turnover_cols)), 
            by = join_by(game_id, team == posteam))

## STEP 3: Create Season-Specific (Cum) Features by Lagging ####
# Since the raw values are already cumulative (and normalized per week),
# we simply lag them so that each game’s predictor uses only prior data.
turnoverSeason <- turnoverFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(turnover_cols), #contains(epa_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 
# The lagged values here serve as your “cum” feature since the raw values are cumulative.

## STEP 4: Compute Multi-Season Rolling Averages ####
# These features are computed across seasons (grouped by team)
# They will help smooth out the volatility (especially early in the season)
turnoverMulti <- turnoverFeatures |>
  #group_by(team) |>
  #arrange(season, week) |>
  mutate(
    across(all_of(turnover_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(turnover_cols),
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(turnover_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

## STEP 5: Combine All turnover Features ----
# Join the lagged ("cum") features, the rolling averages, and the EWMA features together.
turnoverFinal <- gameDataLong |>
  select(all_of(id_cols)) |>
  #left_join(epaFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(turnoverSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(turnoverMulti,  by = join_by(game_id, season, week, team, opponent))

# Select all feature columns (everything except the id columns).
turnover_feature_cols <- turnoverFinal |>
  select(-all_of(id_cols)) |>
  colnames()

setdiff(ls(pattern = "turnover"), 
        c("turnoverFinal", "turnover_cols", "turnover_feature_cols"))
rm(list = setdiff(ls(pattern = "turnover"),
                  c("turnoverFinal", "turnover_cols", "turnover_feature_cols")))

# check
# stats <- nfl_stats_variables
# stats_turnover <- stats |> filter(str_detect(variable, "interceptions|fumbles"))
# turnover_df2 <- scoresFeatures |> 
#   select(game_id, season, week, team, opponent,
#          contains("interceptions"), 
#          contains("fumbles"),
#          contains("turnovers")) |>
#   arrange(game_id, team)


# Series ----
cat("Generating Series Data", "\n")

## STEP 1: Calculate Weekly Series Stats ----
nflSeriesWeek_loc <- paste0("~/Desktop/NFLAnalysisTest/scripts/UpdateData/PriorData/",
                           "nflSeriesWeek.rda")

if(file.exists(nflSeriesWeek_loc)){
  load(file = nflSeriesWeek_loc)
  nflSeriesWeek <- nflSeriesWeek |>
    filter(season != get_current_season())
  pbpData_series <- pbpData |> filter(season == get_current_season())
  nflSeriesWeekTemp <- calculate_series_conversion_rates(pbpData_series, 
                                                         weekly = TRUE)
  nflSeriesWeek <- bind_rows(nflSeriesWeek, nflSeriesWeekTemp)
}else{
  nflSeriesWeek <- calculate_series_conversion_rates(pbpData, 
                                                     weekly = TRUE)
}

save(nflSeriesWeek, file = nflSeriesWeek_loc)


seriesData <- nflSeriesWeek

## STEP 2: Merge Series Features with Game-Level Data ----
# Use gameDataLong to enforce consistent ordering (one row per game).
seriesFeatures <- gameDataLong |>
  select(all_of(id_cols)) |>
  left_join(seriesData, by = join_by(season, week, team))

series_cols <- seriesFeatures |>
  select(contains("off"), contains("def")) |>
  colnames()

## STEP 3: Create Season-Specific (Cum) Features by Lagging ####
# Since the raw values are already cumulative (and normalized per week),
# we simply lag them so that each game’s predictor uses only prior data.
seriesSeason <- seriesFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(series_cols), #contains(epa_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 
# The lagged values here serve as your “cum” feature since the raw values are cumulative.

## STEP 4: Compute Multi-Season Rolling Averages ####
# These features are computed across seasons (grouped by team)
# They will help smooth out the volatility (especially early in the season)
seriesMulti <- seriesFeatures |>
  #group_by(team) |>
  #arrange(season, week) |>
  mutate(
    across(all_of(series_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(series_cols),
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(series_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

## STEP 5: Combine All series Features ----
# Join the lagged ("cum") features, the rolling averages, and the EWMA features together.
seriesFinal <- gameDataLong |>
  select(all_of(id_cols)) |>
  #left_join(epaFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(seriesSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(seriesMulti,  by = join_by(game_id, season, week, team, opponent))

series_feature_cols <- seriesFinal |>
  select(-all_of(id_cols)) |>
  colnames()

setdiff(ls(pattern = "series"), c("seriesFinal", "series_cols", "series_feature_cols"))
rm(list = setdiff(ls(pattern = "series"), c("seriesFinal", "series_cols", "series_feature_cols")))


# Red Zone -----
cat("Generating Redzone Data", "\n")

## STEP 1: Calculate zedone data ----
redzoneData <- pbpData |>
  filter(!is.na(posteam)) |>
  select(
    game_id, season, week, posteam, home_team, away_team,
    fixed_drive, fixed_drive_result, drive_inside20, drive_ended_with_score
  ) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  distinct() |>
  arrange(posteam, .by_group = TRUE) |>
  reframe(
    drives_num = n(),
    red_zone_app = sum(drive_inside20, na.rm = TRUE),
    red_zone_td = sum(fixed_drive_result == "Touchdown" & drive_inside20, na.rm = TRUE),
    red_zone_app_perc = red_zone_app/drives_num,
    red_zone_eff = red_zone_td/red_zone_app,
    red_zone_eff = ifelse(red_zone_app == 0, 0, red_zone_eff)
  )

# red_zone_sum_season <- red_zone_sum_week |> 
#   ungroup() |>
#   group_by(season, posteam) |>
#   summarise(
#     red_zone_eff = mean(red_zone_eff)
#   )

## STEP 2: Red Zone Features ----
redzoneFeatures <- redzoneData |>
  #group_by(game_id) |>
  mutate(
    opponent = rev(posteam), 
    .by = game_id,
    .after = posteam
  ) |>
  rename(team = posteam) |>
  select(-home_team, -away_team)

### STEP 2B: Separate by Offense ----
redzoneFeaturesOff <- redzoneFeatures |> 
  select(game_id, season, week, team, opponent,
         off_red_zone_app_perc = red_zone_app_perc, 
         off_red_zone_eff = red_zone_eff)

### STEP 2B: Separate by Defense ----
redzoneFeaturesDef <- redzoneFeatures |> 
  select(game_id, opponent, 
         def_red_zone_app_perc = red_zone_app_perc, 
         def_red_zone_eff = red_zone_eff)

### STEP 2C: Merge back ----
redzoneFeatures <-
  left_join(
    redzoneFeaturesOff,
    redzoneFeaturesDef,
    by = join_by(game_id, team == opponent)
  )

redzone_cols <- redzoneFeatures |>
  select(off_red_zone_app_perc, off_red_zone_eff,
         def_red_zone_app_perc, def_red_zone_eff) |>
  colnames()

## STEP 3: Season-Specific Aggregation ----
redzoneSeason <- redzoneFeatures |>
  #group_by(season, team) |>
  #arrange(week) |>
  mutate(
    across(all_of(redzone_cols), #contains(epa_cols),
           ~cummean(.x),
           .names = "{.col}_cum"),
    .by = c(season, team),
    .keep = "unused"
  ) |>
  mutate(
    across(contains("_cum"), 
           ~lag(.x, n = 1, default = NA)),
    .by = c(team)
  ) 
# The lagged values here serve as your “cum” feature since the raw values are cumulative.

## STEP 4: Compute Multi-Season Rolling Averages -----
# These features are computed across seasons (grouped by team)
# They will help smooth out the volatility (especially early in the season)
redzoneMulti <- redzoneFeatures |>
  #group_by(team) |>
  #arrange(season, week) |>
  mutate(
    across(all_of(redzone_cols),
           ~slidify_vec(
             .x = .x,
             .period  = 5,
             .f       = mean,
             .partial = TRUE,
             .align   = "right"
           ),
           .names = "{.col}_roll"),
    .by = c(team),
    .keep = "all"
  ) |> #arrange(season, team, week)
  mutate(
    across(all_of(redzone_cols),
           ~movavg(.x, n = 5, type = "e"),
           .names = "{.col}_ewma"),
    .by = c(team),
    .keep = "all"
  ) |> 
  select(-all_of(redzone_cols)) |>
  mutate(
    across(c(contains("_roll"), contains("_ewma")),
           ~lag(.x, n = 1, default = NA)),
    .by = c(team),
    .keep = "unused"
  ) #|> arrange(season, team, week)

## STEP 5: Combine All redzone Features ----
# Join the lagged ("cum") features, the rolling averages, and the EWMA features together.
redzoneFinal <- gameDataLong |>
  select(all_of(id_cols)) |>
  #left_join(epaFeatures, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(redzoneSeason, by = join_by(game_id, season, week, team, opponent)) |>
  left_join(redzoneMulti,  by = join_by(game_id, season, week, team, opponent))

# Select all feature columns (everything except the id columns).
redzone_feature_cols <- redzoneFinal |>
  select(-all_of(id_cols)) |>
  colnames()

setdiff(ls(pattern = "redzone"),
        c("redzoneFinal", "redzone_cols", "redzone_feature_cols"))
rm(list = setdiff(ls(pattern = "redzone"),
                  c("redzoneFinal", "redzone_cols", "redzone_feature_cols")))

# Weather -----
# weatherData <- pbpData |>
#   select(game_id, home_team, away_team, weather) |>
#   distinct() |>
#   left_join(
#     gameData |> select(game_id, gameday, gametime, home_team, away_team, temp, wind, roof)
#   ) |>
#   mutate(
#     rain = str_split_i(weather, "Temp", i = 1)
#   )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----
# Model Data ----
## Long Data ----
## for modeling home and away scores
# colnames(gameDataLong)
# epa_feature_cols
# srs_feature_cols
# elo_feature_cols
# scores_feature_cols
# turnover_feature_cols
# series_feature_cols
# redzone_feature_cols

cat("Generating modDataLong", "\n")
modDataLong <- gameDataLong |>
  select(-c(
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    contains("qb_id"),
    #home_qb_name, away_qb_name,
    referee,
    stadium_id,
    team_GP, team_W, team_L, team_T, team_PF, team_PFG, team_PA, team_PAG
  )) |>
  mutate(
    locationID = ifelse(location == "home", 1, 0)
  ) |>
  mutate(
    FEATURE_COLS = 1
  ) |>
  # EPA
  left_join(
    epaFinal |> select(
      all_of(id_cols), 
      all_of(epa_feature_cols),
      -contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # SRS
  left_join(
    srsFinal |> select(
      all_of(id_cols), 
      all_of(srs_feature_cols)
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # ELO
  left_join(
    eloFinal |> select(
      all_of(id_cols), 
      all_of(elo_feature_cols)
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # SCORES
  left_join(
    scoresFinal |> select(
      all_of(id_cols), 
      all_of(scores_feature_cols)
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # TURNOVERS
  left_join(
    turnoverFinal |> select(
      all_of(id_cols), 
      all_of(turnover_feature_cols),
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # SERIES
  left_join(
    seriesFinal |> select(
      all_of(id_cols), 
      all_of(series_feature_cols),
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  # REDZONE
  left_join(
    redzoneFinal |> select(
      all_of(id_cols), 
      all_of(redzone_feature_cols),
      #-contains("net")
    ),
    by = join_by(game_id, season, week, team, opponent)
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 68, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

modDataLong_features <- modDataLong |>
  select(
    (which(names(modDataLong) == "FEATURE_COLS") + 1):ncol(modDataLong)
  ) |>
  colnames()

modDataLong_team <- modDataLong |>
  relocate(contains("elo"), .before = "FEATURE_COLS") |>
  rename_with(
    .fn = ~paste0("team_", .x),
    .cols = (which(names(modDataLong) == "FEATURE_COLS") + 1):ncol(modDataLong)
  )

modDataLong_opponent <- modDataLong |>
  rename_with(
    .fn = ~paste0("opponent_", .x),
    .cols = (which(names(modDataLong) == "FEATURE_COLS") + 1):ncol(modDataLong)
  )

## Wide Data ----
cat("Generating modData", "\n")
modData <- gameData |>
  select(-c(
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    home_qb_id, away_qb_id,
    home_qb_name, away_qb_name,
    referee,
    stadium_id
  )) |>
  left_join(
    modDataLong |> 
      select(game_id, team, all_of(modDataLong_features), -contains("opponent")) |>
      rename(elo = team_elo) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    modDataLong |> 
      select(game_id, team, all_of(modDataLong_features), -contains("opponent")) |>
      rename(elo = team_elo) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 68, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

## Remove unwanted columns and Save ----
modDataLong <- modDataLong |> select(-FEATURE_COLS)

#save(modDataLong, file = "~/Desktop/NFLAnalysisTest/modDataLong.rda")
#save(modData, file = "~/Desktop/NFL Analysis Data/modData.rda")

rm(modDataLong_features,
   modDataLong_team,
   modDataLong_opponent)

# modData |> 
#   filter(season == 2024) |> 
#   select(game_id, home_team, away_team, home_elo, away_elo) |>
#   View()

# For modeling result and total directly
# modData <- gameData |>
#   select(-c(
#     old_game_id,
#     gsis,
#     nfl_detail_id,
#     pfr,
#     pff,
#     espn,
#     ftn,
#     home_qb_id, away_qb_id,
#     home_qb_name, away_qb_name,
#     referee,
#     stadium_id
#   )) |>
#   left_join(
#     epaFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     epaFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   left_join(
#     srsFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     srsFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   left_join(
#     scoresFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     scoresFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   left_join(
#     turnoverFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     turnoverFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   left_join(
#     seriesFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     seriesFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   left_join(
#     redzoneFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, home_team == team)
#   ) |>
#   left_join(
#     redzoneFinal |> 
#       select(game_id, team, contains("cum"), contains("roll"), contains("ewma")) |>
#       rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
#     by = join_by(game_id, away_team == team)
#   ) |>
#   rename_with(~str_remove(.x, "_mean"), contains("mean")) |>
#   mutate(
#     home_net_epa_cum = home_off_epa_cum - home_def_epa_cum,
#     home_net_epa_roll = home_off_epa_roll - home_def_epa_roll,
#     home_off_net_epa_cum = home_off_epa_cum + away_def_epa_cum,
#     home_off_net_epa_roll = home_off_epa_roll + away_def_epa_roll,
#     home_pass_net_epa_cum = home_off_pass_epa_cum + away_def_pass_epa_cum,
#     home_pass_net_epa_roll = home_off_pass_epa_roll + away_def_pass_epa_roll,
#     home_rush_net_epa_cum = home_off_rush_epa_cum + away_def_rush_epa_cum,
#     home_rush_net_epa_roll = home_off_rush_epa_roll + away_def_rush_epa_roll,
#     home_penalty_net_epa_cum = home_off_penalty_epa_cum + away_def_penalty_epa_cum,
#     home_penalty_net_epa_roll = home_off_penalty_epa_roll + away_def_penalty_epa_roll,
#     away_net_epa_cum = away_off_epa_cum - away_def_epa_cum,
#     away_net_epa_roll = away_off_epa_roll - away_def_epa_roll,
#     away_off_net_epa_cum = away_off_epa_cum + home_def_epa_cum,
#     away_off_net_epa_roll = away_off_epa_roll + home_def_epa_roll,
#     away_pass_net_epa_cum = away_off_pass_epa_cum + home_def_pass_epa_cum,
#     away_pass_net_epa_roll = away_off_pass_epa_roll + home_def_pass_epa_roll,
#     away_rush_net_epa_cum = away_off_rush_epa_cum + home_def_rush_epa_cum,
#     away_rush_net_epa_roll = away_off_rush_epa_roll + home_def_rush_epa_roll,
#     away_penalty_net_epa_cum = away_off_penalty_epa_cum + home_def_penalty_epa_cum,
#     away_penalty_net_epa_roll = away_off_penalty_epa_roll + home_def_penalty_epa_roll,
#     
#     home_PFG_net = home_PFG - away_PAG,
#     home_PAG_net = away_PFG - home_PAG,
#     home_MOV_net = home_MOV - away_MOV,
#     home_SOS_net = home_SOS - away_SOS,
#     home_SRS_net = home_SRS - away_SRS,
#     home_OSRS_net = home_OSRS - away_DSRS,
#     home_DSRS_net = home_DSRS - away_OSRS,
#     
#     away_PFG_net = away_PFG - home_PAG,
#     away_PAG_net = home_PFG - away_PAG,
#     away_MOV_net = away_MOV - home_MOV,
#     away_SOS_net = away_SOS - home_SOS,
#     away_SRS_net = away_SRS - home_SRS,
#     away_OSRS_net = away_OSRS - home_DSRS,
#     away_DSRS_net = away_DSRS - home_OSRS,
#     
#     home_PFG_ewma_net = home_PFG_ewma - away_PAG_ewma,
#     home_PAG_ewma_net = away_PFG_ewma - home_PAG_ewma,
#     home_MOV_ewma_net = home_MOV_ewma - away_MOV_ewma,
#     home_SOS_ewma_net = home_SOS_ewma - away_SOS_ewma,
#     home_SRS_ewma_net = home_SRS_ewma - away_SRS_ewma,
#     home_OSRS_ewma_net = home_OSRS_ewma - away_DSRS_ewma,
#     home_DSRS_ewma_net = home_DSRS_ewma - away_OSRS_ewma,
#     
#     away_PFG_ewma_net = away_PFG_ewma - home_PAG_ewma,
#     away_PAG_ewma_net = home_PFG_ewma - away_PAG_ewma,
#     away_MOV_ewma_net = away_MOV_ewma - home_MOV_ewma,
#     away_SOS_ewma_net = away_SOS_ewma - home_SOS_ewma,
#     away_SRS_ewma_net = away_SRS_ewma - home_SRS_ewma,
#     away_OSRS_ewma_net = away_OSRS_ewma - home_DSRS_ewma,
#     away_DSRS_ewma_net = away_DSRS_ewma - home_OSRS_ewma,
#     
#     home_PFG_roll_net = home_PFG_roll - away_PAG_roll,
#     home_PAG_roll_net = away_PFG_roll - home_PAG_roll,
#     home_MOV_roll_net = home_MOV_roll - away_MOV_roll,
#     home_SOS_roll_net = home_SOS_roll - away_SOS_roll,
#     home_SRS_roll_net = home_SRS_roll - away_SRS_roll,
#     home_OSRS_roll_net = home_OSRS_roll - away_DSRS_roll,
#     home_DSRS_roll_net = home_DSRS_roll - away_OSRS_roll,
#     
#     away_PFG_roll_net = away_PFG_roll - home_PAG_roll,
#     away_PAG_roll_net = home_PFG_roll - away_PAG_roll,
#     away_MOV_roll_net = away_MOV_roll - home_MOV_roll,
#     away_SOS_roll_net = away_SOS_roll - home_SOS_roll,
#     away_SRS_roll_net = away_SRS_roll - home_SRS_roll,
#     away_OSRS_roll_net = away_OSRS_roll - home_DSRS_roll,
#     away_DSRS_roll_net = away_DSRS_roll - home_OSRS_roll
#   ) |>
#   mutate(
#     temp = ifelse(is.na(temp), 68, temp),
#     wind = ifelse(is.na(wind), 0, wind)
#   )
# 
# modDataLong <- modData |>
#   clean_homeaway(invert = c("result", "spread_line"))













