

# Source files ----
source(file = "Testing Scripts/SummaryPlayerFunctions.R")

# Read in Data ----
## Game Data ----
gameData <- load_schedules(seasons = most_recent_season())

## Play-by-play Data ----
playsData <- load_pbp(seasons = most_recent_season())

## Team Data ----
teamsData <- load_teams(current = FALSE)

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
    inputSeason <- seq(input$summaryPlayerSeason[1], input$summaryPlayerSeason[2])
    inputGameType <- input$summaryPlayerGameType
    inputTeams <- input$summaryPlayerTeam
    inputStat <- input$summaryPlayerStat
    
    make_player_offensive_passing_table(
      teams_data = teamsData,
      season = inputSeason,
      gameType = inputGameType, 
      teams = inputTeams, 
      stat = inputStat
    )
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





