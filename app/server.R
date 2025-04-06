# Read in Data ################################################################
## Amazon RDS connection ----
plan("multisession")

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")

allSeasons <- 2006:most_recent_season()

## Team Data ----
teamsData <- load_teams(current = FALSE)

## Game Data ----
source(file = "./data-raw/gameData.R")

source(file = "./data-raw/gameDataLong.R")

## Play-by-play Data ----
#pbpData <- load_pbp(seasons = 2002:most_recent_season())

## Player Data ----
### Offense ----
#playerOffenseData <- tbl(con, "playerOffenseData")
#load(file = "./data/playerOffenseData.rda")
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/playerOffenseData.rda"))

## seasonStandings ----
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/seasonStandings.rda"))
#seasonStandings <- tbl(con, "seasonStandings")

## modData ----
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Source files ============================================
#source(file = "Testing Scripts/SummaryPlayerFunctions.R")
# source("./R/calculateStandings.R", local = TRUE)

# Define server logic #########################################################
shinyServer(function(input, output, session) {
  # Navbar  #################################################
  observeEvent(input$about, {
    showModal(
      modalDialog(title = "About",
                  div(strong("Created by: "), "Tyler Pollard"),
                  div(strong("Version: "), "1.0"),
                  div(strong("Release Date: "), "27 July 2021"))
    )
  })
  
  # Sidebar #################################################
  # observeEvent(input$menu_items,{
  #   updateSidebar(id = "sidebar", session = session)
  # })
  
  # Home Tab  ###############################################
  # output$image <- renderImage({
  #   filename <- normalizePath(file.path("./www/nfl_logo.jpeg"))
  #   list(src = filename,
  #        width = "60%",
  #        height = "400px",
  #        align = "center")
  # }, deleteFile = FALSE)
  
  # Data Tab ################################################
  ## Standings Tab ##########################################
  ### Table Data ----
  standingsSeason <- reactive({
    as.numeric(input$standingsSeason)
  })
  standingsStat <- reactive({
    input$standingsStat
  })
  standingsTableData <- reactive({
    Season <- standingsSeason()
    seasonStandings |>
      filter(season == Season) |>
      collect()
  })
  
  ### AFC Table ----
  standingsTableServer("standingsTableAFC",
                       standingsSeason,
                       standingsStat,
                       teamsData,
                       standingsTableData,
                       conference = "AFC")
  
  ### NFC Table ----
  standingsTableServer("standingsTableNFC",
                       standingsSeason,
                       standingsStat,
                       teamsData,
                       standingsTableData,
                       conference = "NFC")
  
  ### AFC Playoffs Table ----
  standingsPlayoffsTableServer("standingsPlayoffsTableAFC",
                               standingsSeason,
                               teamsData,
                               standingsTableData,
                               conference = "AFC")
  
  ### NFC Playoffs Table ----
  standingsPlayoffsTableServer("standingsPlayoffsTableNFC",
                               standingsSeason,
                               teamsData,
                               standingsTableData,
                               conference = "NFC")
  
  ## Scores Tab #############################################
  ## Team Tab ###############################################
  ### Team Offense ==========================================
  #### Overview ----
  #### Passing ----
  #### Rushing ----
  #### Conversions ----
  #### Drive Averages ----
  ### Team Defense ==========================================
  #### Overview ----
  #### Passing ----
  #### Rushing ----
  #### Conversions ----
  #### Drive Averages ----
  #### Against Position ----
  ### Team Special Teams ====================================
  #### Kick/Punt Returns ----
  #### Kicking ----
  #### Punting ----
  ### Team Scoring ==========================================
  #### Scoring For ----
  #### Scoring Against ----
  ## Player Tab  ############################################
  ### Player Offense ========================================
  playerOffenseSeason <- reactive({
    input$playerOffenseSeason
  })
  playerOffenseGameType <- reactive({
    input$playerOffenseGameType
  })
  playerOffenseTeam <- reactive({
    input$playerOffenseTeam
  })
  playerOffenseStat <- reactive({
    input$playerOffenseStat
  })
  #### Scrimmage ----
  #### Passing ----
  playerOffensePassingTableServer("playerOffensePassingTable", 
                                  playerOffenseData,
                                  playerOffenseSeason,
                                  playerOffenseGameType,
                                  playerOffenseTeam,
                                  playerOffenseStat,
                                  teamsData)
  #### Rushing ----
  #### Receiving ----
  #### Conversions ----
  ### Player Defense ========================================
  #### Overview ----
  ### Player Special Teams ==================================
  #### Kick/Punt Returns ----
  #### Kicking ----
  #### Punting ----
  ### Player Scoring ========================================
  #### Overview ----
  ### Player Fantasy ========================================
  #### Ranks ----
  # Betting Tab  ############################################
  ## Games ==================================================
  ### Lines ----
  #### Inputs ----
  ##### Season ----
  # Betting Tab  ############################################
  ## Games ==================================================
  
  ### Lines ----
  # Betting Season and Week reactive values
  bettingSeasonInput <- reactive({ input$bettingSeason })
  bettingWeekInput <- reactive({ input$bettingWeek })
  
  # Update Week choices when Season changes
  observe({
    #req(bettingSeasonInput())
    
    current_season <- get_current_season()
    current_week <- get_current_week()
    
    if (bettingSeasonInput() == current_season) {
      updateSelectInput(session, "bettingWeek",
                        choices = 1:current_week,
                        selected = current_week)
    } else {
      # Allow full season for past years
      max_week <- max(gameData$week[gameData$season == bettingSeasonInput()], na.rm = TRUE)
      updateSelectInput(session, "bettingWeek",
                        choices = 1:max_week,
                        selected = max_week)
    }
  }) |> bindEvent(bettingSeasonInput())
  
  # Filter game data based on selected season & week
  filteredFutureGames <- reactive({
    req(input$bettingSeason, input$bettingWeek)
    gameData |>
      filter(season == bettingSeasonInput(), week == bettingWeekInput(), !is.na(spread_line))
    #futureGameIDs <- futureGames$game_id
    # list(
    #   ids = futureGameIDs,
    #   data = futureGames
    # )
  })
  
  filteredFutureGamesIDs <- reactive({
    filteredFutureGames()$game_id
  })
  
  # Launch betting lines module
  observe({
    filteredFutureGamesIDs <- filteredFutureGamesIDs()
    filteredFutureGames <- filteredFutureGames()
    bettingGamesLinesServer("gameLines",
                            futureGameIDs = filteredFutureGamesIDs, #games$ids,
                            futureGameData = filteredFutureGames, #games$data,
                            teamsData = teamsData,
                            gameDataLong = gameDataLong)
  }) |> bindEvent(filteredFutureGames())
  
  
  
  
  
  ## Player Props ----
  AllInputs1 <- reactive({
    x <- reactiveValuesToList(input)
    #x2 <<- x
    x2 <- unlist(x)
    x3 <- data.frame(x2) |> rownames_to_column()
    colnames(x3) <- c("name", "value")
    # data.frame(
    #   names = names(x),
    #   values = unlist(x, use.names = TRUE)
    # )
    x3 <- x3 |> add_row(name = NA, value = NA)
    return(x3)
  })
  
  output$show_inputs1 <- renderTable({
    AllInputs1()
  }, width = "100%")
  
  # AllInputs2 <- reactive({
  #   x <- reactiveValuesToList(input)
  #   data.frame(
  #     names = names(x)
  #     #values = unlist(x, use.names = TRUE)
  #   )
  # })
  # 
  # output$show_inputs2 <- renderTable({
  #   AllInputs2()
  # })
  
  ## Plot ====================================
  modPlotInputs <- modDataPlotInputServer("modDataPlotInput",
                                          teamsData,
                                          modData)
  
  modDataPlotServer("modPlot",
                    teamsData,
                    modData,
                    modPlotInputs)
  
  # Prediction Tab  #########################################
  
}) # end server





