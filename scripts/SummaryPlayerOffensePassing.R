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
library(reactable)
library(reactablefmtr)
library(htmltools)
# library(pointblank)
# library(tfrmt)
# library(gto)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Game Data ====
gameData <- load_schedules(seasons = 2023:2024)

# Play-by-play Data ====
#playsData <- load_pbp(seasons = most_recent_season())

# Team Data ====
#teamsData <- load_teams(current = TRUE)
teamsData <- load_teams(current = FALSE)

# Player Data ====
# Weekly Player Stats
## Offense ----
playerOffenseData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "offense"
)
colnames(playerOffenseData)

### Passing ----
summaryPlayerType <- "Offense"
summaryPlayerSeason <- 2024
summaryPlayerGameType <- "REG"
summaryPlayerTeam <- teamsData$team_name
summaryPlayerStat <- "Total"
summaryPlayerCategory <- "Passing"


playerOffensePassingData <- playerOffenseData |>
  filter(attempts > 0) |>
  rename(team_abbr = recent_team)

# Next Gen
playerOffensePassingNextGenData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "passing"
) |>
  filter(week != 0)
colnames(playerOffensePassingNextGenData)

playerOffensePassingDataComb <- left_join(playerOffensePassingData, playerOffensePassingNextGenData) |>
  select(-team_abbr) |>
  rename(team_abbr = recent_team)

#### Base Table ----
sumFunc <- "Total"
summaryPlayerOffensePassingTableBase <- playerOffensePassingData |>
  select(
    player_display_name,
    team_abbr,
    position,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    passing_first_downs,
    interceptions,
    sacks,
    sack_yards,
    sack_fumbles,
    sack_fumbles_lost#,
    #passer_rating
  ) |>
  mutate(
    a = (completions/attempts - 0.3)*5,
    a2 = ifelse(a < 0, 0, 
                ifelse(a > 2.375, 2.375, a)),
    b = (passing_yards/attempts - 3)*0.25,
    b2 = ifelse(b < 0, 0, 
                ifelse(b > 2.375, 2.375, b)),
    c = (passing_tds/attempts)*20,
    c2 = ifelse(c < 0, 0, 
                ifelse(c > 2.375, 2.375, c)),
    d = 2.375 - (interceptions/attempts)*25,
    d2 = ifelse(d < 0, 0, 
                ifelse(d > 2.375, 2.375, d)),
    passer_rating1 = ((a+b+c+d)/6)*100,
    passer_rating2 = ((a2+b2+c2+d2)/6)*100,
    passer_rating = (((completions/attempts - 0.3)*5) + ((passing_yards/attempts - 3)*0.25) + ((passing_tds/attempts)*20) + (2.375 - (interceptions/attempts)*25)/6)*100
  ) |>
  group_by(
    player_display_name, team_abbr, position
  ) %>%
  {if(sumFunc == "Total"){
    summarise(.,
              across(-c(passing_yards, passer_rating), ~round(sum(.x, na.rm = TRUE),2)),
              passing_yards = sum(passing_yards, na.rm = TRUE),
              games_played = n(),
              passing_yards_game = round(passing_yards/games_played, 2),
              passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    )
  }else{
    summarise(.,
              across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
              passing_yards = sum(passing_yards, na.rm = TRUE),
              games_played = n(),
              passing_yards_game = round(passing_yards/games_played, 2),
              passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    )
  }
  } |>
  # summarise(
  #   across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
  #   passing_yards = sum(passing_yards, na.rm = TRUE),
  #   games_played = n(),
  #   passing_yards_game = round(passing_yards/games_played, 2),
  #   passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
  # ) |>
  ungroup() |>
  mutate(
    completion_percentage = round(completions/attempts, 4)
  ) |>
  select(
    player_display_name,
    team_abbr,
    position,
    games_played,
    completions,
    attempts,
    completion_percentage,
    passing_yards,
    passing_yards_game,
    everything()
  ) |>
  arrange(desc(passing_yards)) 


# summaryPlayerOffensePassingTableBase <- playerOffensePassingDataComb |>
#   select(
#     player_display_name,
#     team_abbr,
#     completions,
#     attempts,
#     passing_yards,
#     passing_tds,
#     passing_first_downs,
#     interceptions,
#     sacks,
#     sack_yards,
#     sack_fumbles,
#     sack_fumbles_lost,
#     passer_rating
#   ) |>
#   group_by(
#     player_display_name, team_abbr
#   ) |>
#   summarise(
#     across(-passer_rating, list(sum = ~sum(.x , na.rm = TRUE),
#                               mean = ~mean(.x, na.rm = TRUE))),
#     # across(-passer_rating, list(sum = ~sum(.x , na.rm = TRUE),
#     #                             mean = ~mean(.x, na.rm = TRUE))),
#     passer_rating = round(mean(passer_rating), 2),
#     games_played = n()
#   ) |>
#   ungroup() |>
#   mutate(
#     completion_percentage = round(completions_sum/attempts_sum, 4)
#   ) |>
#   mutate(
#     across(contains(c("mean")), ~round(.x, 2))
#   ) |>
#   select(
#     player_display_name,
#     team_abbr,
#     games_played,
#     contains("completions"),
#     contains("attempts"),
#     completion_percentage,
#     everything(),
#     #-passer_rating_sum,
#   ) |>
#   arrange(desc(passing_yards_sum)) 

#### GT Table ----
# summaryPlayerOffensePassingTableGT <- summaryPlayerOffensePassingTableBase |>
#   gt() |>
#   sub_missing() |>
#   fmt_percent(
#     columns = completion_percentage,
#     decimals = 2
#   ) |>
#   # cols_merge(
#   #   columns = contains("completions"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("attempts"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("yards"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("td"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("first_down"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("interceptions"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = c("sacks_sum", "sacks_mean"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("sack_yards"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = c("sack_fumbles_sum", "sack_fumbles_mean"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   # cols_merge(
#   #   columns = contains("sack_fumbles_lost"),
#   #   pattern = "{1} ({2})"
#   # ) |>
#   cols_align(
#     columns = -player_display_name,
#     align = "center"
#   ) |>
#   tab_style(
#     style = cell_borders(sides = "right"),
#     locations = cells_body(
#       columns = c("team_abbr",
#                   "completion_percentage",
#                   "passing_yards_game", #"passing_yards_sum",
#                   "passing_first_downs", #"passing_first_downs_sum",
#                   "sack_fumbles_lost", #"sack_fumbles_lost_sum"
#                   )
#     )
#   ) |>
#   tab_style(
#     style = cell_text(whitespace = "nowrap"),
#     locations = cells_body(
#       columns = player_display_name
#     )
#   ) |>
#   cols_label(
#     "player_display_name" = html("<strong>Player</strong>"),
#     "team_abbr" = html("<strong>Team</strong>"),
#     "games_played" = with_tooltip(html("<strong>GP</strong>"), "Games Played"),
#     "completions" = with_tooltip(html("<strong>CMP</strong>"), "Completions"),
#     #"completions_mean" = "CMP/G",
#     "attempts" = with_tooltip(html("<strong>ATT</strong>"), "Attempts"),
#     #"attempts_mean" = "ATT/G",
#     "completion_percentage" = with_tooltip(html("<strong>CMP%</strong>"), "Completion Percent"),
#     "passing_yards" = with_tooltip(html("<strong>YDS</strong>"), "Passing Yards"),
#     "passing_yards_game" = with_tooltip(html("<strong>YDS/G</strong>"), "Passing Yards per Game"),
#     #"passing_yards_mean" = "YDS/G",
#     "passing_tds" = with_tooltip(html("<strong>TD</strong>"), "Passing Touchdowns"),
#     #"passing_tds_mean" = "TD/G",
#     "passing_first_downs" = with_tooltip(html("<strong>FD</strong>"), "Passing First Downs"),
#     #"passing_first_downs_mean" = "FD/G",
#     "interceptions" = with_tooltip(html("<strong>INT</strong>"), "Interceptions"),
#     #"interceptions_mean" = "INT/G",
#     "sacks" = with_tooltip(html("<strong>SCK</strong>"), "Sacks"),
#     #"sacks_mean" = "SCK/G",
#     "sack_yards" = with_tooltip(html("<strong>SYL</strong>"), "Sack Yards Lost"),
#     "sack_fumbles" = with_tooltip(html("<strong>SFM</strong>"), "Sack Fumbles"),
#     "sack_fumbles_lost" = with_tooltip(html("<strong>SFL</strong>"), "Sacks Fumbles Lost"),
#     "passer_rating" = with_tooltip(html("<strong>RTG</strong>"), "Passer Rating")
#   ) |>
#   tab_source_note(
#     "Total (Average)"
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>GP</strong>: Games Played"),
#     locations = cells_column_labels("games_played")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>CMP</strong>: Completions"),
#     locations = cells_column_labels("completions")
#   ) |>
#   tab_footnote(
#     footnote = html("<strong>ATT</strong>: Attempts"),
#     locations = cells_column_labels("attempts")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>CMP%</strong>: Completion Percentage"),
#     locations = cells_column_labels("completion_percentage")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>YDS</strong>: Passing Yards"),
#     locations = cells_column_labels("passing_yards")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>YDS/G</strong>: Passing Yards per Game"),
#     locations = cells_column_labels("passing_yards_game")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>TD</strong>: Passing Touchdowns"),
#     locations = cells_column_labels("passing_tds")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>FD</strong>: Passing First Downs"),
#     locations = cells_column_labels("passing_first_downs")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>INT</strong>: Interceptions"),
#     locations = cells_column_labels("interceptions")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>SCK</strong>: Sacks"),
#     locations = cells_column_labels("sacks")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>SYL</strong>: Sack Yards Lost"),
#     locations = cells_column_labels("sack_yards")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>SFM</strong>: Sack Fumbles"),
#     locations = cells_column_labels("sack_fumbles")
#   ) |>
#   tab_footnote(
#     footnote =  html("<strong>SFL</strong>: Sack Fumbles Lost"),
#     locations = cells_column_labels("sack_fumbles_lost")
#   ) |>
#   tab_footnote(
#     footnote = html("<strong>RTG</strong>: Passer Rating"),
#     locations = cells_column_labels("passer_rating")
#   ) |>
#   opt_interactive(
#     use_pagination = FALSE,
#     use_compact_mode = TRUE,
#     use_highlight = TRUE
#     #use_filters = TRUE
#   ) |>
#   gt_nfl_logos(
#     columns = "team_abbr", height = "20px"
#   ) |>
#   # tab_style(
#   #   style = cell_text(size = px(12)),
#   #   locations = cells_body()
#   # ) |>
#   # cols_add(
#   #   RNK = row_number()
#   # ) |>
#   # cols_move_to_start(RNK) |>
#   cols_width(
#     #RNK ~ px(60),
#     games_played ~ px(60),
#     passing_yards ~ px(120),
#     player_display_name ~ px(200)
#   ) |>
#   tab_header(
#     title = html(
#       paste0(
#         "<sup><i>1</i></sup> <strong>GP</strong>: Games Played; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>2</i></sup> <strong>CMP</strong>: Pass Completions; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>3</i></sup> <strong>ATT</strong>: Pass Attempts; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>4</i></sup> <strong>CMP%</strong>: Completion Percentage; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>5</i></sup> <strong>YDS</strong>: Passing Yards; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>5</i></sup> <strong>YDS/G</strong>: Passing Yards per Game; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>6</i></sup> <strong>TD</strong>: Touchdowns; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>7</i></sup> <strong>FD</strong>: First Downs; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>8</i></sup> <strong>INT</strong>: Interceptions; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>9</i></sup> <strong>SCK</strong>: Sacks; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>10</i></sup> <strong>SYL</strong>: Sack Yards Lost; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>11</i></sup> <strong>SFM</strong>: Sack Fumbles; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>12</i></sup> <strong>SFL</strong>: Sack Fumbles Lost; &nbsp;&nbsp;&nbsp;&nbsp;",
#         "<sup><i>13</i></sup> <strong>RTG</strong>: Passer Rating"
#       )
#     )
#   ) |>
#   gt_theme_espn() |>
#   tab_options(
#     heading.title.font.size = px(14),
#     heading.align = "left",
#     column_labels.font.size = px(10),
#     footnotes.multiline = FALSE,
#     footnotes.sep = ";  "
#   )
# summaryPlayerOffensePassingTableGT
# colnames(playerPassingSumTable$`_data`)
# gt(playerPassingSumTable$`_footnotes`)

#### Reactable ----
summaryPlayerOffensePassingTableReactData <- summaryPlayerOffensePassingTableBase |>
  left_join(teamsData |> select(team_abbr, team_logo_espn)) |>
  select(team_logo_espn, everything()) |>
  mutate()

summaryPlayerOffensePassingTableReact <- reactable(
  data = summaryPlayerOffensePassingTableReactData,
  theme = espn(),
  highlight = TRUE,
  compact = TRUE,
  pagination = FALSE,
  wrap = FALSE,
  #rownames = TRUE,
  columns = list(
    # .rownames = colDef(
    #   name = "RNK",
    #   sortable = FALSE,  # Disable sorting on the rank column
    #   cell = function(value, index) {
    #     # Rank based on the current row index (after sorting)
    #     index
    #   }
    # ),
    ##### Team Logo ----
    team_logo_espn = colDef(
      name = "Player",
      minWidth = 150,
      sortable = FALSE,
      #cell = embed_img()
      cell = function(value, index){
        player_name <- summaryPlayerOffensePassingTableReactData$player_display_name[index]
        logo <- img(src = value, style = "height: 20px;")
        team <- summaryPlayerOffensePassingTableReactData$team_abbr[index]
        div(style = "display: flex; align-items: center;",
            logo,
            span(player_name, style = "margin-left: 4px"), 
            span(",", style = "margin-right: 4px"),
            span(team, style = "font-size: 10px; color: grey")
        )
      },
      style = list(borderRight = "1px solid black")
    ),
    ##### Player ----
    player_display_name = colDef(
      show = FALSE
    ),
    ##### Team Abbr ----
    team_abbr = colDef(
      show = FALSE
    ),
    ##### Position ----
    position = colDef(
      name = "POS",
      align = "center",
      style = list(borderRight = "1px solid black")
    ),
    ##### Games Played ----
    games_played = colDef(
      name = "GP",
      minWidth = 30
    ),
    ##### Completions ----
    completions = colDef(
      name = "CMP"
    ),
    ##### Attempts ----
    attempts = colDef(
      name = "ATT"
    ),
    ##### Completion Percentage ----
    completion_percentage = colDef(
      name = "CMP%",
      format = colFormat(percent = TRUE, digits = 2),
      style = list(borderRight = "1px solid black")
    ),
    ##### Passing Yards ----
    passing_yards = colDef(
      name = "YDS"
    ),
    ##### Passing Yards ----
    passing_yards_game = colDef(
      name = "YDS/G",
      style = list(borderRight = "1px solid black")
    ),
    ##### Passing Touchdowns ----
    passing_tds = colDef(
      name = "TD"
    ),
    ##### Passing First Downs ----
    passing_first_downs = colDef(
      name = "FD",
      style = list(borderRight = "1px solid black")
    ),
    ##### Interceptions ----
    interceptions = colDef(
      name = "INT"
    ),
    ##### Sacks ----
    sacks = colDef(
      name = "SCK"
    ),
    ##### Sack Yards Lost ----
    sack_yards = colDef(
      name = "SYL"
    ),
    ##### Sack Fumbles ----
    sack_fumbles = colDef(
      name = "SFM"
    ),
    ##### Sack Fumbles Lost ----
    sack_fumbles_lost = colDef(
      name = "SFL",
      style = list(borderRight = "1px solid black")
    ),
    ##### Passer Rating ----
    passer_rating = colDef(
      name = "RTG"
    )
  ),
  defaultColDef = colDef(vAlign = "center", 
                         minWidth = 60),
  defaultSortOrder = "desc",
  defaultSorted = c("passing_yards"),
  showSortable = TRUE
)
summaryPlayerOffensePassingTableReact


### Passing with Functions ----
summaryPlayerType <- "offense"
summaryPlayerCategory <- "passing"
summaryPlayerSeason <- 2024
summaryPlayerGameType <- "REG"
summaryPlayerTeam <- teamsData$team_name
summaryPlayerStat <- "Total"

playerData <- load_player_stats(
  seasons = 2024,
  stat_type = summaryPlayerType
)

# Next Gen
playerNextGenData <- load_nextgen_stats(
  seasons = 2024,
  stat_type = summaryPlayerCategory
) |>
  filter(week != 0)

make_player_offensive_passing_table <- function(season = 2024, gameType = "REG", teams, stat = "Total"){
  # NFL verse player data
  playerData <- load_player_stats(
    seasons = season,
    stat_type = "offense"
  ) |>
    filter(season_type == gameType) |>
    filter(recent_team %in% teams) |>
    rename(team_abbr = recent_team)
  
  summaryPlayerTableBase <- playerData |>
    select(
      player_display_name,
      team_abbr,
      position,
      completions,
      attempts,
      passing_yards,
      passing_tds,
      passing_first_downs,
      interceptions,
      sacks,
      sack_yards,
      sack_fumbles,
      sack_fumbles_lost
    ) |>
    # mutate(
    #   a = (completions/attempts - 0.3)*5,
    #   a2 = ifelse(a < 0, 0,
    #               ifelse(a > 2.375, 2.375, a)),
    #   b = (passing_yards/attempts - 3)*0.25,
    #   b2 = ifelse(b < 0, 0,
    #               ifelse(b > 2.375, 2.375, b)),
    #   c = (passing_tds/attempts)*20,
    #   c2 = ifelse(c < 0, 0,
    #               ifelse(c > 2.375, 2.375, c)),
    #   d = 2.375 - (interceptions/attempts)*25,
    #   d2 = ifelse(d < 0, 0,
    #               ifelse(d > 2.375, 2.375, d)),
    #   passer_rating = ((a2+b2+c2+d2)/6)*100
    # ) |>
    # select(
    #   -c(a,a2,b,b2,c,c2,d,d2)
    # ) |>
    group_by(
      player_display_name, team_abbr, position
    ) %>%
    {if(stat == "Total"){
      summarise(.,
                across(-c(passing_yards), ~round(sum(.x, na.rm = TRUE),2)),
                passing_yards = sum(passing_yards, na.rm = TRUE),
                games_played = n(),
                passing_yards_game = round(passing_yards/games_played, 2)
                #passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
      )
    }else{
      summarise(.,
                across(-c(passing_yards), ~round(mean(.x, na.rm = TRUE),2)),
                passing_yards = sum(passing_yards, na.rm = TRUE),
                games_played = n(),
                passing_yards_game = round(passing_yards/games_played, 2)
                #passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
      )
    }
    } |>
    ungroup() |>
    mutate(
      a = (completions/attempts - 0.3)*5,
      a2 = ifelse(a < 0, 0,
                  ifelse(a > 2.375, 2.375, a)),
      b = (passing_yards/attempts - 3)*0.25,
      b2 = ifelse(b < 0, 0,
                  ifelse(b > 2.375, 2.375, b)),
      c = (passing_tds/attempts)*20,
      c2 = ifelse(c < 0, 0,
                  ifelse(c > 2.375, 2.375, c)),
      d = 2.375 - (interceptions/attempts)*25,
      d2 = ifelse(d < 0, 0,
                  ifelse(d > 2.375, 2.375, d)),
      passer_rating = round(((a2+b2+c2+d2)/6)*100, 2)
    ) |>
    select(
      -c(a,a2,b,b2,c,c2,d,d2)
    ) |>
    mutate(
      completion_percentage = round(completions/attempts, 4)
    ) |>
    select(
      player_display_name,
      team_abbr,
      position,
      games_played,
      completions,
      attempts,
      completion_percentage,
      passing_yards,
      passing_yards_game,
      everything()
    ) |>
    arrange(desc(passing_yards))
  
  summaryPlayerTableReactData <- summaryPlayerTableBase |>
    left_join(teamsData |> select(team_abbr, team_logo_espn)) |>
    select(team_logo_espn, everything())
  
  summaryPlayerTableReact <- reactable(
    data = summaryPlayerTableReactData,
    theme = espn(),
    highlight = TRUE,
    compact = TRUE,
    pagination = FALSE,
    wrap = FALSE,
    columns = list(
      ##### Team Logo 
      team_logo_espn = colDef(
        name = "Player",
        minWidth = 150,
        sortable = FALSE,
        #cell = embed_img()
        cell = function(value, index){
          player_name <- summaryPlayerTableReactData$player_display_name[index]
          logo <- img(src = value, style = "height: 20px;")
          team <- summaryPlayerTableReactData$team_abbr[index]
          div(style = "display: flex; align-items: center;",
              logo,
              span(player_name, style = "margin-left: 4px"), 
              span(",", style = "margin-right: 4px"),
              span(team, style = "font-size: 10px; color: grey")
          )
        },
        style = list(borderRight = "1px solid black")
      ),
      ##### Player 
      player_display_name = colDef(
        show = FALSE
      ),
      ##### Team Abbr 
      team_abbr = colDef(
        show = FALSE
      ),
      ##### Position
      position = colDef(
        name = "POS",
        align = "center",
        style = list(borderRight = "1px solid black")
      ),
      ##### Games Played 
      games_played = colDef(
        name = "GP",
        minWidth = 30
      ),
      ##### Completions
      completions = colDef(
        name = "CMP"
      ),
      ##### Attempts
      attempts = colDef(
        name = "ATT"
      ),
      ##### Completion Percentage
      completion_percentage = colDef(
        name = "CMP%",
        format = colFormat(percent = TRUE, digits = 2),
        style = list(borderRight = "1px solid black")
      ),
      ##### Passing Yards
      passing_yards = colDef(
        name = "YDS"
      ),
      ##### Passing Yards
      passing_yards_game = colDef(
        name = "YDS/G",
        style = list(borderRight = "1px solid black")
      ),
      ##### Passing Touchdowns
      passing_tds = colDef(
        name = "TD"
      ),
      ##### Passing First Downs
      passing_first_downs = colDef(
        name = "FD",
        style = list(borderRight = "1px solid black")
      ),
      ##### Interceptions
      interceptions = colDef(
        name = "INT"
      ),
      ##### Sacks
      sacks = colDef(
        name = "SCK"
      ),
      ##### Sack Yards Lost 
      sack_yards = colDef(
        name = "SYL"
      ),
      ##### Sack Fumbles 
      sack_fumbles = colDef(
        name = "SFM"
      ),
      ##### Sack Fumbles Lost
      sack_fumbles_lost = colDef(
        name = "SFL",
        style = list(borderRight = "1px solid black")
      ),
      ##### Passer Rating
      passer_rating = colDef(
        name = "RTG"
      )
    ),
    defaultColDef = colDef(vAlign = "center", 
                           minWidth = 60),
    defaultSortOrder = "desc",
    defaultSorted = c("passing_yards"),
    showSortable = TRUE
  )
  return(summaryPlayerTableReact)
}

##### Test Function ----
make_player_offensive_passing_table(teams = selected)

playerDataComb <- left_join(playerData, playerNextGenData) |>
  select(-team_abbr) |>
  rename(team_abbr = recent_team)

#### Base Table ----
sumFunc <- "Total"
summaryPlayerTableBase <- playerDataComb |>
  select(
    player_display_name,
    team_abbr,
    position,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    passing_first_downs,
    interceptions,
    sacks,
    sack_yards,
    sack_fumbles,
    sack_fumbles_lost
  ) |>
  mutate(
    passer_rating = (((completions/attempts - 0.3)*5) + ((passing_yards/attempts - 3)*0.25) + ((passing_tds/attempts)*20) + (2.375 - (interceptions/attempts)*25)/6)*100
  ) |>
  group_by(
    player_display_name, team_abbr, position
  ) %>%
  {if(sumFunc == "Total"){
    summarise(.,
              across(-c(passing_yards, passer_rating), ~round(sum(.x, na.rm = TRUE),2)),
              passing_yards = sum(passing_yards, na.rm = TRUE),
              games_played = n(),
              passing_yards_game = round(passing_yards/games_played, 2),
              passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    )
  }else{
    summarise(.,
              across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
              passing_yards = sum(passing_yards, na.rm = TRUE),
              games_played = n(),
              passing_yards_game = round(passing_yards/games_played, 2),
              passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    )
  }
  } |>
  ungroup() |>
  mutate(
    completion_percentage = round(completions/attempts, 4)
  ) |>
  select(
    player_display_name,
    team_abbr,
    position,
    games_played,
    completions,
    attempts,
    completion_percentage,
    passing_yards,
    passing_yards_game,
    everything()
  ) |>
  arrange(desc(passing_yards)) 

#### Reactable ----
summaryPlayerOffensePassingTableReactData <- summaryPlayerOffensePassingTableBase |>
  left_join(teamsData |> select(team_abbr, team_logo_espn)) |>
  select(team_logo_espn, everything()) |>
  mutate()

summaryPlayerOffensePassingTableReact <- reactable(
  data = summaryPlayerOffensePassingTableReactData,
  theme = espn(),
  highlight = TRUE,
  compact = TRUE,
  pagination = FALSE,
  wrap = FALSE,
  #rownames = TRUE,
  columns = list(
    # .rownames = colDef(
    #   name = "RNK",
    #   sortable = FALSE,  # Disable sorting on the rank column
    #   cell = function(value, index) {
    #     # Rank based on the current row index (after sorting)
    #     index
    #   }
    # ),
    ##### Team Logo ----
    team_logo_espn = colDef(
      name = "Player",
      minWidth = 150,
      sortable = FALSE,
      #cell = embed_img()
      cell = function(value, index){
        player_name <- summaryPlayerOffensePassingTableReactData$player_display_name[index]
        logo <- img(src = value, style = "height: 20px;")
        team <- summaryPlayerOffensePassingTableReactData$team_abbr[index]
        div(style = "display: flex; align-items: center;",
            logo,
            span(player_name, style = "margin-left: 4px"), 
            span(",", style = "margin-right: 4px"),
            span(team, style = "font-size: 10px; color: grey")
        )
      },
      style = list(borderRight = "1px solid black")
    ),
    ##### Player ----
    player_display_name = colDef(
      show = FALSE
    ),
    ##### Team Abbr ----
    team_abbr = colDef(
      show = FALSE
    ),
    ##### Position ----
    position = colDef(
      name = "POS",
      align = "center",
      style = list(borderRight = "1px solid black")
    ),
    ##### Games Played ----
    games_played = colDef(
      name = "GP",
      minWidth = 30
    ),
    ##### Completions ----
    completions = colDef(
      name = "CMP"
    ),
    ##### Attempts ----
    attempts = colDef(
      name = "ATT"
    ),
    ##### Completion Percentage ----
    completion_percentage = colDef(
      name = "CMP%",
      format = colFormat(percent = TRUE, digits = 2),
      style = list(borderRight = "1px solid black")
    ),
    ##### Passing Yards ----
    passing_yards = colDef(
      name = "YDS"
    ),
    ##### Passing Yards ----
    passing_yards_game = colDef(
      name = "YDS/G",
      style = list(borderRight = "1px solid black")
    ),
    ##### Passing Touchdowns ----
    passing_tds = colDef(
      name = "TD"
    ),
    ##### Passing First Downs ----
    passing_first_downs = colDef(
      name = "FD",
      style = list(borderRight = "1px solid black")
    ),
    ##### Interceptions ----
    interceptions = colDef(
      name = "INT"
    ),
    ##### Sacks ----
    sacks = colDef(
      name = "SCK"
    ),
    ##### Sack Yards Lost ----
    sack_yards = colDef(
      name = "SYL"
    ),
    ##### Sack Fumbles ----
    sack_fumbles = colDef(
      name = "SFM"
    ),
    ##### Sack Fumbles Lost ----
    sack_fumbles_lost = colDef(
      name = "SFL",
      style = list(borderRight = "1px solid black")
    ),
    ##### Passer Rating ----
    passer_rating = colDef(
      name = "RTG"
    )
  ),
  defaultColDef = colDef(vAlign = "center", 
                         minWidth = 60),
  defaultSortOrder = "desc",
  defaultSorted = c("passing_yards"),
  showSortable = TRUE
)
summaryPlayerOffensePassingTableReact







