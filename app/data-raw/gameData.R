# gameData.R
# Helper script to compute and process game schedule data for the app

# Dependencies: nflverse, tidyverse (loaded externally in UpdateData.R)

#' Compute processed game schedule data with betting probabilities and related features
#'
#' @param seasons Integer vector of seasons to include (default: 2006 through most_recent_season()).
#' @return A tibble of game schedules for the specified seasons, with: clean team abbreviations, betting probabilities,
#'         cover flags for spread and total, game winner, and time-of-day classification.
compute_game_data <- function(seasons = 2006:most_recent_season()) {
  load_schedules(seasons = TRUE) |>      # load schedule for specified seasons
    filter(season >= min(seasons)) |>       # ensure only seasons at or after the minimum
    mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team),
      # season type: REG vs POST
      season_type = ifelse(game_type == "REG", "REG", "POST"),
      # betting probabilities from American odds
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
      # cover flags and winner
      spreadCover = case_when(
        result > spread_line  ~ TRUE,
        result < spread_line  ~ FALSE,
        TRUE                  ~ NA
      ),
      totalCover = case_when(
        total  > total_line  ~ TRUE,
        total  < total_line  ~ FALSE,
        TRUE                 ~ NA
      ),
      winner = case_when(
        result >  0 ~ home_team,
        result <  0 ~ away_team,
        TRUE        ~ NA_character_
      ),
      # time of day based on gametime ("HH:MM:SS" or similar)
      gamehour = as.numeric(str_extract(gametime, "[:digit:]+(?=:)")),
      time_of_day = case_when(
        gamehour < 15               ~ "Day",
        between(gamehour, 15, 18)   ~ "Evening",
        gamehour > 18               ~ "Night",
        TRUE                        ~ NA_character_
      )
    ) |>                                # remove helper and order columns
    select(-gamehour) |>                # drop intermediate field
    relocate(season_type,       .after = game_type) |>
    relocate(home_spread_prob,  .after = home_spread_odds) |>
    relocate(away_spread_prob,  .after = away_spread_odds) |>
    relocate(under_prob,        .after = under_odds) |>
    relocate(over_prob,         .after = over_odds) |>
    relocate(home_moneyline_prob, .after = home_moneyline) |>
    relocate(away_moneyline_prob, .after = away_moneyline) |>
    relocate(spreadCover,       .after = spread_line) |>
    relocate(totalCover,        .after = total_line) |>
    relocate(winner,            .after = result) |>
    relocate(time_of_day,       .after = gametime)
}

# # Auto-run when sourced in UpdateData.R ----
# if (exists("allSeasons")) {
#   gameData <- compute_game_data(allSeasons)
# }


