## Create Game Data for App
library(DBI)
library(nflverse)
library(tidyverse)

allSeasons <- 2002:most_recent_season()

## play by play
tictoc::tic()
progressr::with_progress(pbpData <- load_pbp(seasons = allSeasons))
tictoc::toc()
object.size(pbpData)

con <- dbConnect(RPostgres::Postgres(),
                    dbname = "NFLdata",
                    user = "postgre",
                    password = "NFLpass1234",
                    host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")

pbpData <- tbl(my_con, "pbp_data")
currentWeek <- get_current_week()

pbp_db <- DBI::dbConnect(RSQLite::SQLite(), "~/Desktop/NFL Analysis Data/pbp_db") |>
  tbl("nflfastR_pbp")

pbpDataSnippet <- pbpData |>
  filter(season == 2024) |>
  filter(week == (currentWeek - 2)) |>
  filter(home_team == "BAL") |>
  filter(posteam == "BAL") |>
  collect()

colnames(pbpDataSnippet)

# Create game data -----
gameData <- load_schedules(seasons = allSeasons) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

# Betting Probs ----
gameData <- gameData |>
  mutate(
    season_type = ifelse(game_type == "REG", "REG", "POST"),
    home_spread_prob = ifelse(home_spread_odds < 0, 
                              abs(home_spread_odds)/(abs(home_spread_odds) + 100),
                              100/(home_spread_odds + 100)),
    away_spread_prob = ifelse(away_spread_odds < 0, 
                              abs(away_spread_odds)/(abs(away_spread_odds) + 100),
                              100/(away_spread_odds + 100)),
    under_prob = ifelse(under_odds < 0, 
                        abs(under_odds)/(abs(under_odds) + 100),
                        100/(under_odds + 100)),
    over_prob = ifelse(over_odds < 0, 
                       abs(over_odds)/(abs(over_odds) + 100),
                       100/(over_odds + 100)),
    home_moneyline_prob = ifelse(home_moneyline < 0, 
                                 abs(home_moneyline)/(abs(home_moneyline) + 100),
                                 100/(home_moneyline + 100)),
    away_moneyline_prob = ifelse(away_moneyline < 0, 
                                 abs(away_moneyline)/(abs(away_moneyline) + 100),
                                 100/(away_moneyline + 100)),
    spreadCover = ifelse(result > spread_line, TRUE, 
                         ifelse(result < spread_line, FALSE, NA)),
    totalCover = ifelse(total > total_line, TRUE, 
                        ifelse(total < total_line, FALSE, NA)),
    winner = ifelse(result > 0, home_team, 
                    ifelse(result < 0, away_team, NA)),
    time_of_day = ifelse(hour(hm(gametime)) < 15, "Day",
                         ifelse(between(hour(hm(gametime)), 15, 18), "Evening", "Night"))
  ) |>
  relocate(season_type, .after = game_type) |>
  relocate(home_spread_prob, .after = home_spread_odds) |>
  relocate(away_spread_prob, .after = away_spread_odds) |>
  relocate(under_prob, .after = under_odds) |>
  relocate(over_prob, .after = over_odds) |>
  relocate(home_moneyline_prob, .after = home_moneyline) |>
  relocate(away_moneyline_prob, .after = away_moneyline) |>
  relocate(spreadCover, .after = spread_line) |>
  relocate(totalCover, .after = total_line) |>
  relocate(winner, .after = result) |>
  relocate(time_of_day, .after = gametime)


# Record and Points ----
gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) |>
  group_by(season, team) |>
  mutate(
    team_GP = row_number(),
    winner = ifelse(team == winner, TRUE, 
                    ifelse(opponent == winner, FALSE, NA)),
    team_W = cumsum(winner),
    team_L = cumsum(!winner),
    team_T = team_GP - team_W - team_L,
    team_PF = cumsum(team_score),
    team_PFG = team_PF/team_GP,
    team_PA = cumsum(opponent_score),
    team_PAG = team_PA/team_GP,
  ) |>
  group_by(game_id) |>
  mutate(locationID = row_number(), .after = location)

# EPA and WPA data -----
## Passing ----
## pbpData take 10.881 seconds
## pbp_db takes 6.027 seconds
#tic()
system.time(
epaPass <- pbp_db |>
  filter(season %in% allSeasons) |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "pass") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    #off_pass_epa_total = sum(epa),
    #off_pass_wpa_total = sum(vegas_wpa),
    #off_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_pass_epa = mean(epa),
    #off_pass_wpa = mean(vegas_wpa),
    off_pass_epa_adj = mean(epa*scaled_vegas_wp),
    #def_pass_epa_total = sum(epa),
    #def_pass_wpa_total = sum(vegas_wpa),
    #def_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    #def_pass_epa = mean(epa),
    #def_pass_wpa = mean(vegas_wpa),
    #def_pass_epa_adj = mean(epa*scaled_vegas_wp)
  ) |>
  ungroup() |>
  collect()
)
#toc()

## Rushing ----
#tic()
epaRush <- pbp_db |>
  filter(season %in% allSeasons) |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "run") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    #off_rush_epa_total = sum(epa),
    #off_rush_wpa_total = sum(vegas_wpa),
    #off_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_rush_epa = mean(epa),
    #off_rush_wpa = mean(vegas_wpa),
    off_rush_epa_adj = mean(epa*scaled_vegas_wp),
    #def_rush_epa_total = sum(epa),
    #def_rush_wpa_total = sum(vegas_wpa),
    #def_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    #def_rush_epa = mean(epa),
    #def_rush_wpa = mean(vegas_wpa),
    #def_rush_epa_adj = mean(epa*scaled_vegas_wp)
  ) |>
  ungroup() |>
  collect()
#toc()

## Combined ----
epaData <- left_join(
  epaPass,
  epaRush,
  by = join_by(game_id, season, week, posteam, home_team, away_team)
) |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  )
rm(epaPass, epaRush)

epaData <- epaData |>
  left_join(
    epaData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  ) |>
  group_by(season, team) |>
  mutate(
    across(c(contains("off"), contains("def")), ~cummean(.x))
  ) |>
  mutate(
    across(c(contains("off"), contains("def")), ~lag(.x))
  ) |>
  ungroup()


## Merge with Game Data ----
epaGameData <- left_join(
  gameData,
  epaData |> 
    filter(team == home_team) |>
    select(game_id, team, contains("off"), contains("def")) |>
    rename_with(~paste0("home_", .x), .cols = c(contains("off"), contains("def"))),
    by = join_by(game_id, "home_team" == "team")) |>
  left_join(
    epaData |> 
      filter(team == away_team) |>
      select(game_id, team, contains("off"), contains("def")) |>
      rename_with(~paste0("away_", .x), .cols = c(contains("off"), contains("def"))),
    by = join_by(game_id, "away_team" == "team")) |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team,
             home_GP = games_played,
             home_W = win,
             home_L = loss,
             home_T = tie,
             home_WL = win_loss_percent,
             home_PF = team_score,
             home_PFG = team_PPG,
             home_PA = opponent_score,
             home_PAG = opp_PPG,
             home_MOV = MOV, 
             home_SOS = SOS, 
             home_SRS = SRS, 
             home_OSRS = OSRS, 
             home_DSRS = DSRS),
    by = join_by(season, week, "home_team" == "team")
  ) |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team,
             away_GP = games_played,
             away_W = win,
             away_L = loss,
             away_T = tie,
             away_WL = win_loss_percent,
             away_PF = team_score,
             away_PFG = team_PPG,
             away_PA = opponent_score,
             away_PAG = opp_PPG,
             away_MOV = MOV, 
             away_SOS = SOS, 
             away_SRS = SRS, 
             away_OSRS = OSRS, 
             away_DSRS = DSRS),
    by = join_by(season, week, "away_team" == "team")
  ) 
  # select(game_id, season, week, home_team, home_score, away_team, away_score,
  #        contains("epa"), contains("home"), contains("away"))

epaGameDataLong <- epaGameData |>
  clean_homeaway()

# tic()
# epaGameData2 <- calculate_stats(seasons = allSeasons,
#                                 summary_level = "week",
#                                 stat_type = "team")
# toc()
# 
epaGameDataLong <- epaGameDataLong |>
  arrange(season, week, team)
# 
# epaGameData2 <- epaGameData2 |>
#   arrange(season, week, team)

epaGameDataLongTest <- epaGameDataLong |>
  select(team, contains("team_off"))

### Moving Avg ----
window <- 8
epaGameDataLongLag <- epaGameData |>
  clean_homeaway() |>
  group_by(team) |>
  mutate(
    team_MOV = lag(team_MOV),
    team_SOS = lag(team_SOS),
    team_SRS = lag(team_SRS),
    team_OSRS = lag(team_OSRS),
    team_DSRS = lag(team_DSRS),
    # opponent_SRS = lag(opponent_SRS),
    team_off_pass_epa_adj = lag(team_off_pass_epa_adj),
    team_off_rush_epa_adj = lag(team_off_rush_epa_adj),
    team_def_pass_epa_adj = lag(team_def_pass_epa_adj),
    team_def_rush_epa_adj = lag(team_def_rush_epa_adj)
    # opponent_off_pass_epa_adj = lag(opponent_off_pass_epa_adj),
    # opponent_off_rush_epa_adj = lag(opponent_off_rush_epa_adj),
    # opponent_def_pass_epa_adj = lag(opponent_def_pass_epa_adj),
    # opponent_def_rush_epa_adj = lag(opponent_def_rush_epa_adj)
  ) |>
  mutate(
    team_MOV_mov = ifelse(is.na(team_MOV), lag(team_MOV), team_MOV),
    team_SOS_mov = ifelse(is.na(team_SOS), lag(team_SOS), team_SOS),
    team_SRS_mov = ifelse(is.na(team_SRS), lag(team_SRS), team_SRS),
    team_OSRS_mov = ifelse(is.na(team_OSRS), lag(team_OSRS), team_OSRS),
    team_DSRS_mov = ifelse(is.na(team_DSRS), lag(team_DSRS), team_DSRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj), lag(team_off_pass_epa_adj), team_off_pass_epa_adj),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj), lag(team_off_rush_epa_adj), team_off_rush_epa_adj),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj), lag(team_def_pass_epa_adj), team_def_pass_epa_adj),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj), lag(team_def_rush_epa_adj), team_def_rush_epa_adj)
  ) |>
  mutate(
    team_MOV_mov = ifelse(is.na(team_MOV), 0, team_MOV),
    team_SOS_mov = ifelse(is.na(team_SOS), 0, team_SOS),
    team_SRS_mov = ifelse(is.na(team_SRS_mov), 0, team_SRS),
    team_OSRS_mov = ifelse(is.na(team_OSRS), 0, team_OSRS),
    team_DSRS_mov = ifelse(is.na(team_DSRS), 0, team_DSRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj_mov), 0, team_off_pass_epa_adj_mov),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj_mov), 0, team_off_rush_epa_adj_mov),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj_mov), 0, team_def_pass_epa_adj_mov),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj_mov), 0, team_def_rush_epa_adj_mov)
  ) |>
  mutate(
    team_MOV_mov = movavg(team_MOV_mov, n = window, type = "r"),
    team_SOS_mov = movavg(team_SOS_mov, n = window, type = "r"),
    team_SRS_mov = movavg(team_SRS_mov, n = window, type = "r"),
    team_OSRS_mov = movavg(team_OSRS_mov, n = window, type = "r"),
    team_DSRS_mov = movavg(team_DSRS_mov, n = window, type = "r"),
    team_off_pass_epa_adj_mov = movavg(team_off_pass_epa_adj_mov, n = window, type = "r"),
    team_off_rush_epa_adj_mov = movavg(team_off_rush_epa_adj_mov, n = window, type = "r"),
    team_def_pass_epa_adj_mov = movavg(team_def_pass_epa_adj_mov, n = window, type = "r"),
    team_def_rush_epa_adj_mov = movavg(team_def_rush_epa_adj_mov, n = window, type = "r")
  ) |>
  group_by(game_id) |>
  mutate(locationID = row_number()) |>
  ungroup()

epaGameDataLongLagTest1 <- epaGameDataLongLag |>
  #filter(location == "home") |>
  select(game_id, contains("team")) |>
  rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home"))
# select(game_id, 
#        contains("MOV"),
#        contains("SOS"),
#        contains("SRS"),
#        contains("OSRS"),
#        contains("DSRS"),
#        contains("epa"))


epaGameDataLag <- epaGameData |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(locationID == 1) |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              rename("home_team" = "home"),
            keep = FALSE) |>
  left_join(epaGameDataLongLag |>
              filter(locationID == 2) |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              rename("away_team" = "away"),
            keep = FALSE)


epaGameDataLag <- epaGameData |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(location == "home") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              select(game_id, 
                     contains("MOV"),
                     contains("SOS"),
                     contains("SRS"),
                     contains("OSRS"),
                     contains("DSRS"),
                     contains("epa")),
            by = join_by(game_id)) |>
  left_join(epaGameDataLongLag |>
              filter(location == "away") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              select(game_id, 
                     contains("MOV"),
                     contains("SOS"),
                     contains("SRS"),
                     contains("OSRS"),
                     contains("DSRS"),
                     contains("epa")),
            by = join_by(game_id)) 

t <- epaGameDataLongLag |>
  filter(location == lag(location))






