## Create Game Data for App
library(nflverse)
library(tidyverse)

# Create game data -----
allSeasons <- 2002:most_recent_season()

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
    gamehour = as.numeric(str_extract(gametime, "[:digit:]+(?=:)")),
    time_of_day = ifelse(gamehour < 15, "Day",
                         ifelse(between(gamehour, 15, 18), "Evening", "Night"))
  ) |>
  select(-gamehour) |>
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






