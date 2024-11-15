# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(RColorBrewer)
library(fresh)
library(markdown)

## Tables ----
library(gt)
library(gtsummary)
library(gtExtras)
# library(pointblank)
# library(tfrmt)
# library(gto)
library(reactable)
library(reactablefmtr)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Game Data ====
gameData <- load_schedules(seasons = most_recent_season())

# Play-by-play Data ====
playsData <- load_pbp(seasons = most_recent_season())

# Team Data ====
teamsData <- load_teams(current = TRUE)
teamsAllData <- load_teams(current = FALSE)

## Offense ----


### Totals ----


### Passing ----


### Rushing ----


### Receiving ----


### Downs ----

# Player Data ====
# Weekly Player Stats
## Offense ----
playerOffenseData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "offense"
)
colnames(playerOffenseData)

### Passing ----
playerPassingData <- playerOffenseData |>
  filter(attempts > 0) 

# Next Gen
playerPassingNextGenData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "passing"
) |>
  filter(week != 0)
colnames(playerPassingNextGenData)

playerPassingDataComb <- left_join(playerPassingData, playerPassingNextGenData) |>
  select(-team_abbr) |>
  rename(team_abbr = recent_team)

playerPassingSumTableBase <- playerPassingDataComb |>
  select(
    player_display_name,
    team_abbr,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    passing_first_downs,
    interceptions,
    sacks,
    passer_rating
  ) |>
  group_by(
    player_display_name, team_abbr
  ) |>
  summarise(
    across(everything(), list(sum = ~sum(.x , na.rm = TRUE),
                              mean = ~mean(.x, na.rm = TRUE))),
    
    games_played = n()
  ) |>
  ungroup() |>
  mutate(
    completion_percentage = round(completions_sum/attempts_sum, 4)
  ) |>
  mutate(
    across(contains(c("mean")), ~round(.x, 2))
  ) |>
  select(
    player_display_name,
    team_abbr,
    games_played,
    contains("completions"),
    contains("attempts"),
    completion_percentage,
    everything(),
    -passer_rating_sum,
  ) |>
  arrange(desc(passing_yards_sum)) 

#### Gt Table ----
playerPassingSumTableGT <- playerPassingSumTableBase |>
  gt() |>
  sub_missing() |>
  fmt_percent(
    columns = completion_percentage,
    decimals = 2
  ) |>
  cols_merge(
    columns = contains("completions"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("attempts"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("yards"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("td"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("first_down"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("interceptions"),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = contains("sack"),
    pattern = "{1} ({2})"
  ) |>
  cols_align(
    columns = -player_display_name,
    align = "center"
  ) |>
  tab_style(
    style = cell_borders(sides = "right"),
    locations = cells_body(
      columns = c("team_abbr",
                  "completion_percentage",
                  "passing_yards_sum",
                  "passing_first_downs_sum",
                  "sacks_sum")
    )
  ) |>
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body(
      columns = player_display_name
    )
  ) |>
  cols_label(
    "player_display_name" = html("<strong>Player</strong>"),
    "team_abbr" = html("<strong>Team</strong>"),
    "games_played" = with_tooltip(html("<strong>GP</strong>"), "Games Played"),
    "completions_sum" = with_tooltip(html("<strong>CMP</strong>"), "Completions"),
    #"completions_mean" = "CMP/G",
    "attempts_sum" = with_tooltip(html("<strong>ATT</strong>"), "Attempts"),
    #"attempts_mean" = "ATT/G",
    "completion_percentage" = with_tooltip(html("<strong>CMP%</strong>"), "Completion Percent"),
    "passing_yards_sum" = with_tooltip(html("<strong>YDS</strong>"), "Passing Yards"),
    #"passing_yards_mean" = "YDS/G",
    "passing_tds_sum" = with_tooltip(html("<strong>TD</strong>"), "Passing Touchdowns"),
    #"passing_tds_mean" = "TD/G",
    "passing_first_downs_sum" = with_tooltip(html("<strong>FD</strong>"), "Passing First Downs"),
    #"passing_first_downs_mean" = "FD/G",
    "interceptions_sum" = with_tooltip(html("<strong>INT</strong>"), "Interceptions"),
    #"interceptions_mean" = "INT/G",
    "sacks_sum" = with_tooltip(html("<strong>SCK</strong>"), "Sacks"),
    #"sacks_mean" = "SCK/G",
    "passer_rating_mean" = with_tooltip(html("<strong>RTG</strong>"), "Passer Rating")
  ) |>
  tab_source_note(
    "Total (Average)"
  ) |>
  tab_footnote(
    footnote =  html("<strong>GP</strong>: Games Played"),
    locations = cells_column_labels("games_played")
  ) |>
  tab_footnote(
    footnote =  html("<strong>CMP</strong>: Completions"),
    locations = cells_column_labels("completions_sum")
  ) |>
  tab_footnote(
    footnote = html("<strong>ATT</strong>: Attempts"),
    locations = cells_column_labels("attempts_sum")
  ) |>
  tab_footnote(
    footnote =  html("<strong>CMP%</strong>: Completion Percentage"),
    locations = cells_column_labels("completion_percentage")
  ) |>
  tab_footnote(
    footnote =  html("<strong>YDS</strong>: Passing Yards"),
    locations = cells_column_labels("passing_yards_sum")
  ) |>
  tab_footnote(
    footnote =  html("<strong>TD</strong>: Passing Touchdowns"),
    locations = cells_column_labels("passing_tds_sum")
  ) |>
  tab_footnote(
    footnote =  html("<strong>fD</strong>: Passing First Downs"),
    locations = cells_column_labels("passing_first_downs_sum")
  ) |>
  tab_footnote(
    footnote =  html("<strong>INT</strong>: Interceptions"),
    locations = cells_column_labels("interceptions_sum")
  ) |>
  tab_footnote(
    footnote =  html("<strong>SCK</strong>: Sacks"),
    locations = cells_column_labels("sacks_sum")
  ) |>
  tab_footnote(
    footnote = html("<strong>RTG</strong>: Passer Rating"),
    locations = cells_column_labels("passer_rating_mean")
  ) |>
  opt_interactive(
    use_pagination = FALSE,
    use_compact_mode = TRUE,
    use_highlight = TRUE
    #use_filters = TRUE
  ) |>
  gt_nfl_logos(
    columns = "team_abbr", height = "20px"
  ) |>
  # tab_style(
  #   style = cell_text(size = px(12)),
  #   locations = cells_body()
  # ) |>
  # cols_add(
  #   RNK = row_number()
  # ) |>
  # cols_move_to_start(RNK) |>
  cols_width(
    #RNK ~ px(60),
    games_played ~ px(60),
    passing_yards_sum ~ px(120),
    player_display_name ~ px(200)
  ) |>
  tab_header(
    title = html(
      paste0(
        "<sup><i>1</i></sup> <strong>GP</strong>: Games Played; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>2</i></sup> <strong>CMP</strong>: Pass Completions; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>3</i></sup> <strong>ATT</strong>: Pass Attempts; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>4</i></sup> <strong>CMP%</strong>: Completion Percentage; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>5</i></sup> <strong>YDS</strong>: Passing Yards; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>6</i></sup> <strong>TD</strong>: Touchdowns; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>7</i></sup> <strong>FD</strong>: First Downs; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>8</i></sup> <strong>INT</strong>: Interceptions; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>9</i></sup> <strong>SCK</strong>: Sacks; &nbsp;&nbsp;&nbsp;&nbsp;",
        "<sup><i>10</i></sup> <strong>RTG</strong>: Passer Rating"
      )
    )
  ) |>
  gt_theme_espn() |>
  tab_options(
    heading.title.font.size = px(14),
    heading.align = "left",
    column_labels.font.size = px(10),
    footnotes.multiline = FALSE,
    footnotes.sep = ";  "
  )
playerPassingSumTableGT
colnames(playerPassingSumTable$`_data`)
gt(playerPassingSumTable$`_footnotes`)

#### Reactable ----
playerPassingSumTableReactData <- playerPassingSumTableBase |>
  left_join(teamsAllData |> select(team_abbr, team_logo_espn)) |>
  select(team_logo_espn, everything()) |>
  mutate()
playerPassingSumTableReact <- reactable(
  data = playerPassingSumTableReactData,
  theme = espn(),
  highlight = TRUE,
  compact = TRUE,
  pagination = FALSE,
  columns = list(
    team_logo_espn = colDef(
      name = "",
      maxWidth = 25,
      sortable = FALSE,
      style = background_img() #height = "100%", width = "100%")
    )
  )
)
playerPassingSumTableReact

### Rushing ----

### Receiving ----

### Touchdowns ----

## Defense ----
playerDefenseData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "defense"
)

## Kicking ----
playerKickingData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "kicking"
)

## NexGen ----
### Passing ----
playerPassNextGenData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "passing"
)

### Rushing ----
playerRushNextGenData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "rushing"
)

### Receiving ---
playerRecNextGenData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "receiving"
)







