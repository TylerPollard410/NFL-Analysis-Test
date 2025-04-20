## Create Game Data for App
require(nflverse)
require(tidyverse)

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) |>
  group_by(season, team) |>
  mutate(
    team_GP = row_number(),
    winner = ifelse(team == winner, TRUE, 
                    ifelse(opponent == winner, FALSE, NA)),
    team_W = cumsum(result > 0),
    team_L = cumsum(result < 0),
    team_T = team_GP - team_W - team_L,
    team_PF = cumsum(team_score),
    team_PFG = team_PF/team_GP,
    team_PA = cumsum(opponent_score),
    team_PAG = team_PA/team_GP,
  ) |>
  mutate(
    team_W = ifelse(is.na(lag(team_W)), 0, lag(team_W)),
    team_L = ifelse(is.na(lag(team_L)), 0, lag(team_L)),
    team_T = ifelse(is.na(lag(team_T)), 0, lag(team_T))
  ) |>
  ungroup() |>
  group_by(game_id) |>
  mutate(locationID = row_number(), .after = location) |>
  ungroup()


