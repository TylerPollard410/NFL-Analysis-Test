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

# Team Data ----
teamsData <- load_teams(current = FALSE)

# Player Data ====
# Weekly Player Stats
## Offense ----

### Passing ----
make_player_offensive_passing_table <- function(season = 2024, gameType = "REG", teams, stat = "Total"){
  # NFL verse player data
  playerData <- load_player_stats(
    seasons = season,
    stat_type = "offense"
  ) |>
    filter(season_type %in% gameType) |>
    filter(recent_team %in% teams) |>
    rename(team_abbr = recent_team) |>
    filter(attempts > 0)
  
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
    group_by(
      player_display_name, team_abbr, position
    ) |>
    mutate(
      a = (sum(completions)/sum(attempts) - 0.3)*5,
      a2 = ifelse(a < 0, 0,
                  ifelse(a > 2.375, 2.375, a)),
      b = (sum(passing_yards)/sum(attempts) - 3)*0.25,
      b2 = ifelse(b < 0, 0,
                  ifelse(b > 2.375, 2.375, b)),
      c = (sum(passing_tds)/sum(attempts))*20,
      c2 = ifelse(c < 0, 0,
                  ifelse(c > 2.375, 2.375, c)),
      d = 2.375 - (sum(interceptions)/sum(attempts))*25,
      d2 = ifelse(d < 0, 0,
                  ifelse(d > 2.375, 2.375, d)),
      passer_rating = ((a2+b2+c2+d2)/6)*100
    ) |>
    select(
      -c(a,a2,b,b2,c,c2,d,d2)
    ) %>%
    {if(stat == "Total"){
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
        minWidth = 40
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
                           minWidth = 60,
                           headerStyle = list(fontSize = "14px")
                           ),
    defaultSortOrder = "desc",
    defaultSorted = c("passing_yards"),
    showSortable = TRUE
  )
  return(summaryPlayerTableReact)
}









