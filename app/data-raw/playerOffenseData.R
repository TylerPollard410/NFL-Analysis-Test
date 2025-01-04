## playerOffense Data
require(nflverse)
require(tidyverse)

playerOffenseData <- load_player_stats(
  seasons = allSeasons,
  stat_type = "offense"
) |>
  left_join(gameDataLong |> select(game_id, season, week, opponent),
            by = join_by(season, week, opponent_team == opponent)) |>
  relocate(game_id, .before = 1)
