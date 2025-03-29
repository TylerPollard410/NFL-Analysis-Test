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

#seasonsMod <- 2021:2024
#gameData <- gameData #|> filter(season %in% seasonsMod)
#gameData <- gameData |> filter(!(season == 2024 & week > 14))
#gameDataLongMod <- gameDataLong #|> filter(season %in% seasonsMod)
#gameDataLongMod <- gameDataLongMod |> filter(!(season == 2024 & week > 14))
#pbpData <- pbpData
#pbpData <- pbpData |> filter(!(season == 2024 & week > 14))
#load("./app/data/seasonWeekStandings.rda")
#seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

#rm(gameData, gameDataLong)

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
# dbListTables(con)
# dbDisconnect(con)


# Aggregate pbp ----
# pbpPlayTypes <- pbpData |>
#   select(
#     play,
#     play_type, 
#     play_type_nfl,
#     pass,
#     pass_attempt,
#     rush,
#     rush_attempt,
#     special_teams_play,
#     special,
#     penalty,
#     qb_dropback,
#     qb_kneel,
#     qb_spike,
#     qb_scramble,
#     penalty_type
#   ) |>
#   distinct()

# pbpPlayTypesView <- pbpPlayTypes |>
#   filter(play == 1, pass == 1)
#   filter(play == 1, pass == 0, rush == 0, penalty == 1)
# pbpData |>
#   group_by(game_id, posteam) |>
#   mutate(
#     home_epa_diff = total_home_epa - lag(total_home_epa, default = 0),
#     away_epa_diff = total_away_epa - lag(total_away_epa, default = 0)
#   ) |>
#   #filter(special == 1) |>
#   select(game_id, season, week, home_team, away_team, posteam, defteam, play_type, play, pass, rush, penalty, special,
#          qb_scramble,
#          td_prob, fg_prob, opp_td_prob, opp_fg_prob,
#          field_goal_attempt, field_goal_result, kick_distance, #extra_point_attempt, extra_point_result,
#          ep, epa, wp, wpa, vegas_wp, vegas_wpa, total_home_epa, total_away_epa, home_epa_diff, away_epa_diff,
#          posteam_score, defteam_score, posteam_score_post, defteam_score_post,
#          penalty_type, desc) |>
#   ungroup() |>
#   view()

## EPA ----
### Penalty Structure 1 ----
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

### Penalty Sturcture 2 ----
epaOffData <- pbpData |>
  #filter(season %in% seasonsMod) |>
  #filter(play == 1) |> 
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    # Total EPA
    # off_plays = sum(play == 1),
    # off_epa_sum = sum(epa[play == 1], na.rm = TRUE),
    # off_epa_mean = mean(epa[play == 1], na.rm = TRUE),
    off_plays = sum(play == 1 | play_type == "field_goal", na.rm = TRUE),
    off_epa_sum = sum(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    off_epa_mean = mean(epa[play == 1 | play_type == "field_goal"], na.rm = TRUE),
    # Passing EPA
    off_pass_plays = sum(play == 1 & pass == 1 & penalty == 0, na.rm = TRUE),
    off_pass_epa_sum = sum(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # QB EPA (includes scrambles)
    # off_pass_plays = sum(play == 1 & pass == 1 & penalty == 0, na.rm = TRUE),
    # off_pass_epa_sum = sum(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # off_pass_epa_mean = mean(epa[play == 1 & pass == 1 & penalty == 0], na.rm = TRUE),
    # Rushing EPA
    off_rush_plays = sum(play == 1 & rush == 1 & penalty == 0, na.rm = TRUE),
    off_rush_epa_sum = sum(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[play == 1 & rush == 1 & penalty == 0], na.rm = TRUE),
    # Pre Snap Penalty EPA
    off_penalty_plays = sum(play == 1 & penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[play == 1 & penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[play == 1 & penalty == 1], na.rm = TRUE),
    # Kicker EPA
    off_kick_plays = sum(play_type %in% c("field_goal"), na.rm = TRUE),
    off_kick_epa_sum = sum(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    off_kick_epa_mean = mean(epa[play_type %in% c("field_goal")], na.rm = TRUE),
    # Special Teams EPA
    off_special_plays = sum(special == 1 & play_type != "field_goal", na.rm = TRUE),
    off_special_epa_sum = sum(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    off_special_epa_mean = mean(epa[special == 1 & play_type != "field_goal"], na.rm = TRUE),
    # Play distribution
    off_pass_plays_perc = off_pass_plays/(off_pass_plays + off_rush_plays),
    off_rush_plays_perc = off_rush_plays/(off_pass_plays + off_rush_plays)
  ) |>
  mutate(
    across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
  ) |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  ) |>
  ungroup()

# epaOffData |> filter(season == 2024) |> group_by(team) |> 
#   summarise(
#     across(contains("epa"),
#            ~mean(.x, na.rm = TRUE))
#   ) |> 
#   select(team, contains("mean")) |>
#   arrange(desc(off_epa_mean)) |>
#   view()

epaData <- epaOffData |>
  left_join(
    epaOffData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

#epaData |> filter(season == 2024) |> view()

epaAvgs <- epaData |> 
  group_by(season, team) |>
  summarise(
    across(c(contains("plays"), contains("epa_mean")),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(c(contains("plays"), contains("epa_mean")),
           ~lag(.x, n = 1, default = 0),
           .names = "{.col}"
    )
  ) |>
  ungroup() |>
  mutate(week = 1)


#epaAvgs |> filter(season == 2024) |> arrange(desc(off_epa_mean)) |> view()

epaData2 <- gameDataLong |>
  select(
    game_id, season, week, team
  ) |>
  left_join(
    epaData |> select(game_id, team, opponent, contains("plays"), contains("mean"))
  ) |>
  group_by(season, team) |>
  mutate(
    across(c(contains("plays"), contains("epa_mean")),
           ~lag(.x, n = 1, default = NA),
           .names = "{.col}")
  ) |>
  ungroup()
# left_join(
#   epaAvgs,
#   by = join_by(season, team)
# )

# mutate(week = 1)
# 
# epaData3A <- gameDataLongMod |>
#   select(game_id, season, week, team, opponent) |>
#   left_join(
#     bind_rows(
#       epaAvgs,
#       epaData2 |>
#         select(season, week, team, contains("off"), contains("def")) |>
#         filter(week != 1)
#     )
#   )

epaData3A <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(
    bind_rows(
      epaAvgs,
      epaData2 |>
        select(season, week, team, contains("off"), contains("def")) |>
        filter(week != 1)
    )
  ) |>
  # select(
  #   game_id, season, week, team, contains("mean")
  # ) |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value = contains("mean"),
    .period = 5,
    .f = mean,
    .partial = TRUE,
    .align = "right"
  ) |>
  mutate(
    across(c(contains("plays"), contains("mean"), -contains("roll")),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  rename_with(~str_remove(.x, "(?<=roll).+"), contains("roll")) |>
  select(
    game_id, season, week, team, opponent, everything()
  )

epaData3 <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(epaData3A)
# arrange(row) #|>
# select(-c(
#   row,
#   old_game_id,
#   gsis,
#   nfl_detail_id,
#   pfr,
#   pff,
#   espn,
#   ftn,
#   team_qb_id,
#   team_qb_name,
#   opponent_qb_id,
#   opponent_qb_name,
#   referee,
#   stadium_id
# ))

#rm(epaData, epaData2, epaOffData, epaAvgs, pbpData, pbpPlayTypes, pbpPlayTypesView)
# mutate(
#   off_epa_mean_feat = ifelse(is.na(off_epa_mean), off_epa_mean, off_epa_mean),
#   off_pass_epa_mean_feat = ifelse(is.na(off_pass_epa_mean), off_pass_epa_mean, off_pass_epa_mean),
#   off_rush_epa_mean_feat = ifelse(is.na(off_rush_epa_mean), off_rush_epa_mean, off_rush_epa_mean),
#   off_penalty_epa_mean_feat = ifelse(is.na(off_penalty_epa_mean), off_penalty_epa_mean, off_penalty_epa_mean),
#   def_epa_mean_feat = ifelse(is.na(def_epa_mean), def_epa_mean, def_epa_mean),
#   def_pass_epa_mean_feat = ifelse(is.na(def_pass_epa_mean), def_pass_epa_mean, def_pass_epa_mean),
#   def_rush_epa_mean_feat = ifelse(is.na(def_rush_epa_mean), def_rush_epa_mean, def_rush_epa_mean),
#   def_penalty_epa_mean_feat = ifelse(is.na(def_penalty_epa_mean), def_penalty_epa_mean, def_penalty_epa_mean)
# )

## SRS ----
srsData <- epaData3 |>
  left_join(
    seasonWeekStandings |>
      select(season, week, team, PFG = team_PPG, PAG = opp_PPG, MOV, SOS, SRS, OSRS, DSRS)
  ) |>
  group_by(team) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             lag = ~lag(.x, default = 0)
           ),
           .names = "{.col}")
    # lagSRS = lag(SRS, n = 1),
    # diffSRS = SRS - lagSRS,
  ) |>
  mutate(
    across(c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
           .fns = list(
             ewma = ~ifelse(week < 6, movavg(.x, n = 2, "e"), .x)
           ),
           .names = "{.col}_ewma")
  ) |>
  #group_by(season, team) |>
  tk_augment_slidify(
    .value   = c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS),
    # Multiple rolling windows
    .period  = 5,
    .f       = mean,
    .partial = TRUE,
    .align = "right",
    .names = paste0(c("PFG", "PAG", "MOV", "SOS", "SRS", "OSRS", "DSRS"), "_roll")
  ) |>
  ungroup() 

## Efficiency Stats ----
nflStatsWeek <- calculate_stats(seasons = allSeasons,
                                summary_level = "week",
                                stat_type = "team",
                                season_type = "REG+POST")
#nflStatsWeek <- nflStatsWeek |> filter(!(season == 2024 & week > 14))
# nflStatsSeason <- calculate_stats(seasons = 2021:2024,
#                                   summary_level = "season",
#                                   stat_type = "team",
#                                   season_type = "REG+POST")

## Score Stats ----
# scoresData <- pbpData |>
#   #filter(season %in% seasonsMod) |>
#   select(game_id, season, week, posteam, home_team, away_team, td_team,
#          fixed_drive, fixed_drive_result) |>
#   distinct() |>
#   filter(!(fixed_drive_result == "Opp touchdown" & is.na(td_team))) |>
#   select(-td_team) |>
#   distinct() |>
#   filter(!is.na(posteam)) |>
#   group_by(game_id, season, week, posteam, home_team, away_team) |>
#   count(fixed_drive_result) |>
#   pivot_wider(
#     names_from = fixed_drive_result, 
#     values_from = n, 
#     values_fill = 0
#   ) |>
#   ungroup() |>
#   left_join(
#     nflStatsWeek |>
#       select(season, week, team, special_teams_tds, def_tds),
#     by = join_by(season, week, posteam == team)
#   )

scoresData <- nflStatsWeek |>
  select(season, week, team, 
         passing_tds, rushing_tds, special_teams_tds, def_tds,
         fumble_recovery_tds, 
         passing_2pt_conversions, rushing_2pt_conversions,
         pat_att, pat_made, pat_pct,
         fg_att, fg_made, fg_pct, 
         def_safeties,
         off_interceptions = passing_interceptions,
         sack_fumbles_lost, rushing_fumbles_lost, receiving_fumbles_lost,
         def_fumbles_forced,
         def_interceptions
  ) |>
  rowwise() |>
  mutate(
    offTD = passing_tds + rushing_tds,
    totalTD = offTD + special_teams_tds + def_tds + fumble_recovery_tds,
    twoPtConv = passing_2pt_conversions + rushing_2pt_conversions,
    twoPtAtt = totalTD - pat_att,
    off_fumbles = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
    def_fumbles = def_fumbles_forced
  ) |>
  ungroup() |>
  right_join(
    gameDataLong |> select(game_id, season, week, team)
  ) |>
  select(game_id, everything()) |>
  mutate(
    rowID = row_number()
  )

# scoresTeam <- scoresData |>
#   mutate(
#    score =  6*offTD + 6*special_teams_tds + 6*def_tds + 6*fumble_recovery_tds +
#       3*fg_made + 2*twoPtConv + 1*pat_made + 2*def_safeties
#   ) |>
#   select(season, week, team, score)
# 
# teamScoreComp <- gameDataLong |>
#   arrange(season, week, team) |>
#   #filter(!(season == 2024 & season_type == "POST")) |>
#   filter(!is.na(result)) |>
#   select(season, week, team, opponent, team_score, opponent_score)
# 
# teamScoreComp2 <- left_join(scoresTeam, teamScoreComp)
# diffs <- which(teamScoreComp2$score != teamScoreComp2$team_score)
# teamScoreComp2 |> 
#   mutate(
#     diff = score - team_score,
#     diffInd = diff != 0
#   ) |>
#   filter(diffInd)


# conversions <- pbpData |>
#   #filter(season %in% seasonsMod) |>
#   filter(!is.na(posteam)) |>
#   select(game_id, season, week, posteam, home_team, away_team,
#          extra_point_attempt, extra_point_result, extra_point_prob, 
#          #defensive_extra_point_attempt, defensive_extra_point_conv,
#          two_point_attempt, two_point_conv_result, two_point_conversion_prob
#          #defensive_two_point_attempt, defensive_two_point_conv
#   ) |>
#   group_by(game_id, season, week, posteam, home_team, away_team) |>
#   summarise(
#     extra_point_attempt = sum(extra_point_attempt, na.rm = TRUE),
#     extra_point_result = sum(extra_point_result == "good", na.rm = TRUE),
#     extra_point_prob = mean(extra_point_prob, na.rm = TRUE),
#     two_point_attempt = sum(two_point_attempt, na.rm = TRUE),
#     two_point_conv_result = sum(two_point_conv_result == "success", na.rm = TRUE),
#     two_point_conversion_prob = mean(two_point_conversion_prob, na.rm = TRUE)
#   ) |>
#   ungroup()
# 
# scoresData2 <- conversions |>
#   rename(team = posteam) |>
#   left_join(
#     scoresData,
#     by = join_by(season, week, team)
#   ) |>
#   mutate(
#     teamScore = 6*offTD + 6*special_teams_tds + 6*def_tds +
#       3*fg_made + 2*twoPtConv + 1*pat_made + 2*def_safeties
#     #teamPA = 6*oppTouchdown + 2*safety
#   ) |>
#   left_join(
#     gameDataLong |> select(game_id, team, team_score, opponent_score),
#     join_by(game_id, team)
#   ) |>
#   mutate(
#     scoreDiff = team_score - teamScore,
#     special_teams_tds = ifelse(scoreDiff == 6, special_teams_tds + 1, special_teams_tds)#,
#     #def_safeties = ifelse(scoreDiff == 2, def_safeties + 1, def_safeties)
#   ) |>
#   rowwise() |>
#   mutate(
#     offTD = sum(passing_tds, rushing_tds),
#     totalTD = offTD + special_teams_tds + def_tds + fumble_recovery_tds,
#     twoPtConv = sum(passing_2pt_conversions, rushing_2pt_conversions),
#     twoPtAtt = totalTD - pat_att
#   ) |>
#   mutate(
#     teamScore = 6*totalTD + #6*offTD + 6*special_teams_tds + 6*def_tds +
#       3*fg_made + 2*twoPtConv + 1*pat_made + 2*def_safeties
#     #teamPA = 6*oppTouchdown + 2*safety
#   ) |>
#   ungroup() 
# 
# diffs <- which(scoresData2$teamScore != scoresData2$team_score)
# (scoresData2$teamScore - scoresData2$team_score)[diffs]

scoresData3 <- scoresData |>
  select(rowID, everything()) |>
  # group_by(game_id) |>
  # mutate(
  #   opponentScore = rev(teamScore)
  #   #opponentPA = rev(teamPA)
  # ) |>
  # ungroup() |>
  group_by(season, team) |>
  # mutate(
  #   fg_pct_cum = cumsum(fg_made)/cumsum(fg_att),
  #   pat_pct_cum = cumsum(pat_made)/cumsum(pat_att),
  #   off_interceptions_cum = cummean(off_interceptions),
  #   def_interceptions_cum = cummean(def_interceptions),
  #   off_fumbles_cum = cummean(off_fumbles),
  #   def_fumbles_cum = cummean(def_fumbles)
  # ) |>
  # relocate(fg_pct_cum, .after = fg_pct) |>
  # relocate(pat_pct_cum, .after = pat_pct) |>
  # tk_augment_slidify(
  #   .value   = c(pat_made, pat_att, fg_made, fg_att),
  #   # Multiple rolling windows
  #   .period  = 5,
  #   .f       = sum,
  #   .partial = TRUE,
  #   .align = "right",
  #   .names = c("pat_made_roll", "pat_att_roll", "fg_made_roll", "fg_att_roll")
  # ) |>
  # mutate(pat_pct_roll = pat_made_roll/pat_att_roll, .after = pat_pct_cum) |>
  # mutate(fg_pct_roll = fg_made_roll/fg_att_roll, .after = fg_pct_cum) |>
  tk_augment_slidify(
    .value   = c(everything(), -c(1:3)), #c(-game_id, -season, -team, -rowID),
    # Multiple rolling windows
    .period  = 5,
    .f       = ~mean(., na.rm = T),
    .partial = TRUE,
    .align = "right"#,
    #.names = c("pat_att_roll", "fg_att_roll", "offTD_roll", "totalTD_roll")
  ) |>
  ungroup() |>
  mutate(
    across(where(is.numeric), ~na_if(.x, NaN))
  ) |> #arrange(rowID)
  group_by(team) |>
  mutate(
    across(contains("roll_5"), ~lag(.x, 1))
  ) |>
  mutate(
    across(contains("roll_5"), 
           ~ts_impute_vec(.x, period = 4))
  ) |>
  # fill(c(fg_att_roll, fg_pct_cum, fg_pct_roll,offTD_roll,totalTD_roll), .direction = "down") |>
  # mutate(
  #   pat_pct_cum = ifelse(is.nan(pat_pct_cum), 1, pat_pct_cum), 
  #   pat_pct_roll = ifelse(is.nan(pat_pct_roll), 1, pat_pct_roll), 
  #   fg_pct_cum = ifelse(is.nan(fg_pct_cum), 1, fg_pct_cum), 
  #   fg_pct_roll = ifelse(is.nan(fg_pct_roll), 1, fg_pct_roll), 
  #   off_interceptions = lag(off_interceptions, default = 0),
  #   def_interceptions = lag(def_interceptions, default = 0),
  #   off_fumbles = lag(off_fumbles, default = 0),
  #   def_fumbles = lag(def_fumbles, default = 0)
  # ) |>
  # mutate(
  #   totalTD_roll = lag(totalTD_roll),
  #   offTD_roll = lag(offTD_roll),
  #   off_pat_att_roll = lag(pat_att_roll),
  #   off_pat_pct_cum = lag(pat_pct_cum, default = 1),
  #   off_pat_pct_roll = lag(pat_pct_roll, default = 1),
  #   off_fg_att_roll = lag(fg_att_roll),
  #   off_fg_pct_cum = lag(fg_pct_cum),
  #   off_fg_pct_roll = lag(fg_pct_roll),
  #   
  # ) |> 
  ungroup() |>
  arrange(rowID)

scoresData <- scoresData3 |>
  select(-c(rowID, season, week))
  # select(
  #   game_id, team, 
  #   totalTD, totalTD_roll, offTD, offTD_roll,
  #   special_teams_tds, def_tds,
  #   fg_made, fg_att, 
  #   off_fg_att_roll, off_fg_pct_cum, off_fg_pct_roll,
  #   twoPtConv, twoPtAtt,
  #   safeties = def_safeties,
  #   pat_made, pat_att, 
  #   off_pat_att_roll, off_pat_pct_cum, off_pat_pct_roll,
  #   off_interceptions, 
  #   def_interceptions,
  #   off_fumbles, def_fumbles
  # )

## Series ----
seriesWeekData <- calculate_series_conversion_rates(pbpData, weekly = TRUE)
#seriesSeasonData <- calculate_series_conversion_rates(pbpData, weekly = FALSE)

seriesData <- gameDataLong |>
  select(game_id, season, week, team, opponent) |>
  left_join(seriesWeekData) |>
  group_by(season, team) |>
  mutate(
    across(-c(game_id, week, opponent),
           ~cummean(.x))
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(-c(season, game_id, week, opponent),
           ~lag(.x))
  ) |>
  ungroup()

# seriesAvgs <- seriesWeekData |> 
#   group_by(season, team) |>
#   summarise(
#     across(-week,
#            ~mean(.x, na.rm = TRUE))
#   ) |> #arrange(season, team) 
#   ungroup() |>
#   group_by(team) |>
#   mutate(
#     across(-season,
#            ~lag(.x, default = 0))
#   ) |>
#   ungroup() |>
#   mutate(week = 1)
# 
# seriesData <- gameDataLong |>
#   select(game_id, season, week, team, opponent) |>
#   left_join(
#     bind_rows(
#       seriesAvgs,
#       seriesWeekData2 |>
#         select(-c(game_id, opponent)) |>
#         filter(week != 1)
#     )
#   )

## Red Zone -----
red_zone_pbp <- pbpData |>
  #filter(season %in% seasonsMod) |>
  #filter(play == 1) |> 
  filter(!is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  select(fixed_drive, fixed_drive_result, drive_inside20, drive_ended_with_score) |>
  distinct() |>
  arrange(posteam, .by_group = TRUE)

red_zone_sum_week <- red_zone_pbp |>
  summarise(
    drives_num = n(),
    red_zone_app = sum(drive_inside20, na.rm = TRUE),
    red_zone_td = sum(fixed_drive_result == "Touchdown" & drive_inside20, na.rm = TRUE),
    red_zone_app_perc = red_zone_app/drives_num,
    # red_zone_eff = sum(fixed_drive_result == "Touchdown")/sum(drive_inside20, na.rm = TRUE),
    # red_zone_eff = ifelse(red_zone_app == 0, 0, red_zone_eff),
    red_zone_eff = red_zone_td/red_zone_app,
    red_zone_eff = ifelse(red_zone_app == 0, 0, red_zone_eff)
  ) 

red_zone_sum_season <- red_zone_sum_week |> 
  ungroup() |>
  group_by(season, posteam) |>
  summarise(
    red_zone_eff = mean(red_zone_eff)
  )
  
red_zone_data <- red_zone_sum_week |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  ) |>
  ungroup()

red_zone_data2 <-
  left_join(
    red_zone_data |> 
      select(game_id, season, week, team, opponent, home_team, away_team, 
             off_red_zone_app_perc = red_zone_app_perc, 
             off_red_zone_eff = red_zone_eff),
    red_zone_data |> 
      select(game_id, opponent, 
             def_red_zone_app_perc = red_zone_app_perc, 
             def_red_zone_eff = red_zone_eff),
      #rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

red_zone_data3 <- red_zone_data2 |>
  group_by(season, team) |>
  mutate(
    across(c(off_red_zone_app_perc, off_red_zone_eff,
             def_red_zone_app_perc, def_red_zone_eff),
           ~cummean(.x),
           .names = "{.col}_cum")
  ) |>
  ungroup() |>
  group_by(team) |>
  tk_augment_slidify(
    #.value   = c(everything(), -c(1:3)), #c(-game_id, -season, -team, -rowID),
    .value = c(off_red_zone_app_perc, off_red_zone_eff,
               def_red_zone_app_perc, def_red_zone_eff),
    # Multiple rolling windows
    .period  = 5,
    .f       = ~mean(., na.rm = T),
    .partial = TRUE,
    .align = "right"#,
    #.names = c("pat_att_roll", "fg_att_roll", "offTD_roll", "totalTD_roll")
  ) |>
  mutate(
    across(c(contains("cum"), contains("roll")),
           ~lag(.x, default = 0),
           .names = "{.col}")
  ) |>
  ungroup() |>
  select(
    game_id, team, 
    off_red_zone_app_perc_cum, off_red_zone_app_perc_roll_5,
    off_red_zone_eff_cum, off_red_zone_eff_roll_5,
    def_red_zone_app_perc_cum, def_red_zone_app_perc_roll_5,
    def_red_zone_eff_cum, def_red_zone_eff_roll_5
  )

red_zone_data <- red_zone_data3

## Weather -----
# library(openmateo)
# 
# t <- weather_history(
#   location = "Green Bay",
#   start = "2006-12-03",
#   end = "2006-12-03",
#   response_units = list(
#     temperature_units = "farenheit",
#     windspeed_unit = "mph",
#     precipitation_unit = "inch"
#   ),
#   hourly = c("temperature_2m",
#              "apparent_temperature",
#              "wind_speed_10m", 
#              "precipitation", 
#              "rain", 
#              "snowfall",
#              "weather_code")
# )
weatherData <- pbpData |>
  select(game_id, home_team, away_team, weather) |>
  distinct() |>
  left_join(
    gameData |> select(game_id, gameday, gametime, home_team, away_team, temp, wind, roof)
  ) |>
  mutate(
    rain = str_split_i(weather, "Temp", i = 1)
  )

# Model Data ----
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
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             contains("PFG"), contains("PAG"), 
             contains("MOV"), contains("SOS"), 
             contains("SRS"), contains("OSRS"), contains("DSRS")
             #c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)
             ) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    srsData |> 
      select(game_id, team, 
             contains("cum"), contains("roll"),
             contains("PFG"), contains("PAG"), 
             contains("MOV"), contains("SOS"), 
             contains("SRS"), contains("OSRS"), contains("DSRS")
             #c(PFG, PAG, MOV, SOS, SRS, OSRS, DSRS)
      ) |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    scoresData |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    scoresData |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    seriesData |>
      select(-c(season, week, opponent)) |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    seriesData |>
      select(-c(season, week, opponent))  |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    red_zone_data |>
      #select(-c(season, week, opponent))  |>
      rename_with(~paste0("home_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    red_zone_data |>
      #select(-c(season, week, opponent))  |>
      rename_with(~paste0("away_", .x), .cols = -c(game_id, team)),
    by = join_by(game_id, away_team == team)
  ) |>
  rename_with(~str_remove(.x, "_mean"), contains("mean")) |>
  mutate(
    home_net_epa_cum = home_off_epa_cum - home_def_epa_cum,
    home_net_epa_roll = home_off_epa_roll - home_def_epa_roll,
    home_off_net_epa_cum = home_off_epa_cum + away_def_epa_cum,
    home_off_net_epa_roll = home_off_epa_roll + away_def_epa_roll,
    home_pass_net_epa_cum = home_off_pass_epa_cum + away_def_pass_epa_cum,
    home_pass_net_epa_roll = home_off_pass_epa_roll + away_def_pass_epa_roll,
    home_rush_net_epa_cum = home_off_rush_epa_cum + away_def_rush_epa_cum,
    home_rush_net_epa_roll = home_off_rush_epa_roll + away_def_rush_epa_roll,
    home_penalty_net_epa_cum = home_off_penalty_epa_cum + away_def_penalty_epa_cum,
    home_penalty_net_epa_roll = home_off_penalty_epa_roll + away_def_penalty_epa_roll,
    away_net_epa_cum = away_off_epa_cum - away_def_epa_cum,
    away_net_epa_roll = away_off_epa_roll - away_def_epa_roll,
    away_off_net_epa_cum = away_off_epa_cum + home_def_epa_cum,
    away_off_net_epa_roll = away_off_epa_roll + home_def_epa_roll,
    away_pass_net_epa_cum = away_off_pass_epa_cum + home_def_pass_epa_cum,
    away_pass_net_epa_roll = away_off_pass_epa_roll + home_def_pass_epa_roll,
    away_rush_net_epa_cum = away_off_rush_epa_cum + home_def_rush_epa_cum,
    away_rush_net_epa_roll = away_off_rush_epa_roll + home_def_rush_epa_roll,
    away_penalty_net_epa_cum = away_off_penalty_epa_cum + home_def_penalty_epa_cum,
    away_penalty_net_epa_roll = away_off_penalty_epa_roll + home_def_penalty_epa_roll,
    
    home_PFG_net = home_PFG - away_PAG,
    home_PAG_net = away_PFG - home_PAG,
    home_MOV_net = home_MOV - away_MOV,
    home_SOS_net = home_SOS - away_SOS,
    home_SRS_net = home_SRS - away_SRS,
    home_OSRS_net = home_OSRS - away_DSRS,
    home_DSRS_net = home_DSRS - away_OSRS,
    
    away_PFG_net = away_PFG - home_PAG,
    away_PAG_net = home_PFG - away_PAG,
    away_MOV_net = away_MOV - home_MOV,
    away_SOS_net = away_SOS - home_SOS,
    away_SRS_net = away_SRS - home_SRS,
    away_OSRS_net = away_OSRS - home_DSRS,
    away_DSRS_net = away_DSRS - home_OSRS,
    
    home_PFG_ewma_net = home_PFG_ewma - away_PAG_ewma,
    home_PAG_ewma_net = away_PFG_ewma - home_PAG_ewma,
    home_MOV_ewma_net = home_MOV_ewma - away_MOV_ewma,
    home_SOS_ewma_net = home_SOS_ewma - away_SOS_ewma,
    home_SRS_ewma_net = home_SRS_ewma - away_SRS_ewma,
    home_OSRS_ewma_net = home_OSRS_ewma - away_DSRS_ewma,
    home_DSRS_ewma_net = home_DSRS_ewma - away_OSRS_ewma,
    
    away_PFG_ewma_net = away_PFG_ewma - home_PAG_ewma,
    away_PAG_ewma_net = home_PFG_ewma - away_PAG_ewma,
    away_MOV_ewma_net = away_MOV_ewma - home_MOV_ewma,
    away_SOS_ewma_net = away_SOS_ewma - home_SOS_ewma,
    away_SRS_ewma_net = away_SRS_ewma - home_SRS_ewma,
    away_OSRS_ewma_net = away_OSRS_ewma - home_DSRS_ewma,
    away_DSRS_ewma_net = away_DSRS_ewma - home_OSRS_ewma,
    
    home_PFG_roll_net = home_PFG_roll - away_PAG_roll,
    home_PAG_roll_net = away_PFG_roll - home_PAG_roll,
    home_MOV_roll_net = home_MOV_roll - away_MOV_roll,
    home_SOS_roll_net = home_SOS_roll - away_SOS_roll,
    home_SRS_roll_net = home_SRS_roll - away_SRS_roll,
    home_OSRS_roll_net = home_OSRS_roll - away_DSRS_roll,
    home_DSRS_roll_net = home_DSRS_roll - away_OSRS_roll,
    
    away_PFG_roll_net = away_PFG_roll - home_PAG_roll,
    away_PAG_roll_net = home_PFG_roll - away_PAG_roll,
    away_MOV_roll_net = away_MOV_roll - home_MOV_roll,
    away_SOS_roll_net = away_SOS_roll - home_SOS_roll,
    away_SRS_roll_net = away_SRS_roll - home_SRS_roll,
    away_OSRS_roll_net = away_OSRS_roll - home_DSRS_roll,
    away_DSRS_roll_net = away_DSRS_roll - home_OSRS_roll
  ) |>
  mutate(
    temp = ifelse(is.na(temp), 68, temp),
    wind = ifelse(is.na(wind), 0, wind)
  )

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))













