historicLinesData <- fread(file = "https://raw.githubusercontent.com/greerreNFL/nfelomarket_data/refs/heads/main/Data/lines.csv")

historicLinesData2 <- historicLinesData |> 
  filter(season >= 2006) |>
  select(game_id, home_spread_open, home_spread_open_source, home_spread_last, home_spread_last_source) |>
  left_join(
    modData |> select(game_id, spread_line, home_spread_odds, away_spread_odds)
  ) |>
  mutate(spread_line = -spread_line)

unique(historicLinesData2$home_spread_open_source)
