

# Read in Data ----
## Game Data ----
gameData <- load_schedules(seasons = most_recent_season())

## Play-by-play Data ----
playsData <- load_pbp(seasons = most_recent_season())

## Team Data ----
teamsData <- load_teams(current = FALSE)


# Source files ----
source(file = "Testing Scripts/SummaryPlayerFunctions.R")

## Player Data ----
# Weekly Player Stats
### Offense ----
playerOffenseData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "offense"
)

#### Passing ----
# playerPassingData <- playerOffenseData |>
#   filter(attempts > 0) 
# 
# playerPassingNextGenData <- load_nextgen_stats(
#   seasons = most_recent_season(),
#   stat_type = "passing"
# ) |>
#   filter(week != 0)
# colnames(playerPassingNextGenData)
# 
# playerPassingDataComb <- left_join(playerPassingData, playerPassingNextGenData)


### Defense ----
playerDefenseData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "defense"
)

### Kicking ----
playerKickingData <- load_player_stats(
  seasons = most_recent_season(),
  stat_type = "kicking"
)

## NexGen ----
### Passing ----
playerPassData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "passing"
)

### Rushing ----
playerRushData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "rushing"
)

### Receiving ---
playerRecData <- load_nextgen_stats(
  seasons = most_recent_season(),
  stat_type = "receiving"
)

# Define server logic #########################################################
shinyServer(function(input, output, session) {
  # Homepage / Navbar ==========
  observeEvent(input$about, {
    showModal(
      modalDialog(title = "About",
                  div(strong("Created by: "), "Tyler Pollard"),
                  div(strong("Version: "), "1.0"),
                  div(strong("Release Date: "), "27 July 2021"))
    )
  })
  
  output$image <- renderImage({
    filename <- normalizePath(file.path("www/nfl_logo.jpeg"))
    list(src = filename,
         width = 800,
         height = 400,
         align = "center")
  }, deleteFile = FALSE)
  
  
  # Data Tab =====================
  ## Team Statistics ----
  ## Player Statistics ----
  ### Title ----
  # output$player_stats_title <- renderText({
  #   paste0(input$player_tabset, " ",
  #          input$offense_player_statistics, " ",
  #          "Player Statistics")
  # })
  
  ### Offense ----
  
  #### Passing ----
  output$summaryPlayerOffensePassingTable <- renderReactable({
    inputSeason <- as.numeric(input$summaryPlayerSeason)
    inputGameType <- input$summaryPlayerGameType
    inputGameType <- ifelse(inputGameType %in% c("WC", "DIV", "CON", "SB"), 
                            "POST", 
                            "REG")
    inputTeams <- input$summaryPlayerTeam
    inputStat <- input$summaryPlayerStat
    
    make_player_offensive_passing_table(
      season = inputSeason,
      gameType = inputGameType, 
      teams = inputTeams, 
      stat = inputStat
    )
    # playerOffensePassingDataComb <- playerPassingDataComb |>
    #   select(-team_abbr) |>
    #   rename(team_abbr = recent_team)
    # 
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
    #   ) %>%
    #   # {if(sumFunc == "Total"){
    #   #   summarise(.,
    #   #             across(-c(passing_yards, passer_rating), ~round(sum(.x, na.rm = TRUE),2)),
    #   #             passing_yards = sum(passing_yards, na.rm = TRUE),
    #   #             games_played = n(),
    #   #             passing_yards_game = round(passing_yards/games_played, 2),
    #   #             passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    #   #   )
    #   # }else{
    #   #   summarise(.,
    #   #             across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
    #   #             passing_yards = sum(passing_yards, na.rm = TRUE),
    #   #             games_played = n(),
    #   #             passing_yards_game = round(passing_yards/games_played, 2),
    #   #             passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    #   #   )
    #   # }
    #   # } |>
    #   summarise(
    #     across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
    #     passing_yards = sum(passing_yards, na.rm = TRUE),
    #     games_played = n(),
    #     passing_yards_game = round(passing_yards/games_played, 2),
    #     passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
    #   ) |>
    #   ungroup() |>
    #   mutate(
    #     completion_percentage = round(completions/attempts, 4)
    #   ) |>
    #   select(
    #     player_display_name,
    #     team_abbr,
    #     games_played,
    #     completions,
    #     attempts,
    #     completion_percentage,
    #     passing_yards,
    #     passing_yards_game,
    #     everything()
    #   ) |>
    #   arrange(desc(passing_yards)) 
    # 
    # summaryPlayerOffensePassingTableReactData <- summaryPlayerOffensePassingTableBase |>
    #   left_join(teamsData |> select(team_abbr, team_logo_espn)) |>
    #   select(team_logo_espn, everything()) |>
    #   mutate()
    # 
    # summaryPlayerOffensePassingTableReact <- reactable(
    #   data = summaryPlayerOffensePassingTableReactData,
    #   theme = espn(),
    #   highlight = TRUE,
    #   compact = TRUE,
    #   pagination = FALSE,
    #   wrap = FALSE,
    #   #rownames = TRUE,
    #   columns = list(
    #     # .rownames = colDef(
    #     #   name = "RNK",
    #     #   sortable = FALSE,  # Disable sorting on the rank column
    #     #   cell = function(value, index) {
    #     #     # Rank based on the current row index (after sorting)
    #     #     index
    #     #   }
    #     # ),
    #     ##### Team Logo ----
    #     team_logo_espn = colDef(
    #       name = "Player",
    #       minWidth = 150,
    #       sortable = FALSE,
    #       #cell = embed_img()
    #       cell = function(value, index){
    #         player_name <- summaryPlayerOffensePassingTableReactData$player_display_name[index]
    #         logo <- img(src = value, style = "height: 20px;")
    #         team <- summaryPlayerOffensePassingTableReactData$team_abbr[index]
    #         div(style = "display: flex; align-items: center;",
    #             logo,
    #             span(player_name, style = "margin-left: 4px"), 
    #             span(",", style = "margin-right: 4px"),
    #             span(team, style = "font-size: 10px; color: grey")
    #         )
    #       },
    #       style = list(borderRight = "1px solid black")
    #     ),
    #     ##### Player ----
    #     player_display_name = colDef(
    #       show = FALSE
    #     ),
    #     ##### Team Abbr ----
    #     team_abbr = colDef(
    #       show = FALSE
    #     ),
    #     ##### Games Played ----
    #     games_played = colDef(
    #       name = "GP",
    #       minWidth = 30
    #     ),
    #     ##### Completions ----
    #     completions = colDef(
    #       name = "CMP"
    #     ),
    #     ##### Attempts ----
    #     attempts = colDef(
    #       name = "ATT"
    #     ),
    #     ##### Completion Percentage ----
    #     completion_percentage = colDef(
    #       name = "CMP%",
    #       format = colFormat(percent = TRUE, digits = 2)
    #     ),
    #     ##### Passing Yards ----
    #     passing_yards = colDef(
    #       name = "YDS"
    #     ),
    #     ##### Passing Yards ----
    #     passing_yards_game = colDef(
    #       name = "YDS/G"
    #     ),
    #     ##### Passing Touchdowns ----
    #     passing_tds = colDef(
    #       name = "TD"
    #     ),
    #     ##### Passing First Downs ----
    #     passing_first_downs = colDef(
    #       name = "FD"
    #     ),
    #     ##### Interceptions ----
    #     interceptions = colDef(
    #       name = "INT"
    #     ),
    #     ##### Sacks ----
    #     sacks = colDef(
    #       name = "SCK"
    #     ),
    #     ##### Sack Yards Lost ----
    #     sack_yards = colDef(
    #       name = "SYL"
    #     ),
    #     ##### Sack Fumbles ----
    #     sack_fumbles = colDef(
    #       name = "SFM"
    #     ),
    #     ##### Sack Fumbles Lost ----
    #     sack_fumbles_lost = colDef(
    #       name = "SFL"
    #     ),
    #     ##### Passer Rating ----
    #     passer_rating = colDef(
    #       name = "RTG"
    #     )
    #   ),
    #   defaultColDef = colDef(vAlign = "center", 
    #                          minWidth = 60),
    #   defaultSortOrder = "desc",
    #   defaultSorted = c("passing_yards"),
    #   showSortable = TRUE
    # )
    # return(summaryPlayerOffensePassingTableReact)
  })
  
  #### Rushing ----
  
  #### Receiving ----
  
  #### Touchdowns ----
  
  ### Defense ----
  
  #### Tackles ----
  
  #### Sacks ----
  
  #### Interceptions ----
  
  ### Special Teams ----
  
  #### Returning ----
  
  #### Kicking ----
  
  #### Punting ----
  
}) # end server





