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
load(file = "./data/playerOffenseData.RData")

# Load Historical Data ----
load(file = "./data/seasonStandings.rda")
#seasonStandings <- tbl(con, "seasonStandings")

source(file = "./data-raw/modelData.R")

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
  observeEvent(input$menu_items,{
    updateSidebar(id = "sidebar", session = session)
  })
  
  # Home Tab  ###############################################
  output$image <- renderImage({
    filename <- normalizePath(file.path("./www/nfl_logo.jpeg"))
    list(src = filename,
         width = "60%",
         height = "400px",
         align = "center")
  }, deleteFile = FALSE)
  
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
  futureGameIDs <- gameData |>
    filter(!is.na(spread_line) & is.na(result)) |>
    pull(game_id)
  
  futureGameData <- gameData |>
    filter(game_id %in% futureGameIDs)
  
  futureGameDataLong <- gameDataLong |>
    filter(game_id %in% futureGameIDs)
  
  futureGameDates <- unique(futureGameData |> select(week, gameday, weekday))
  
  # lapply(futureGameIDs, function(x){
  #   bettingGamesLinesTableServer(x, teamsData, gameDataLong, gameID = x)
  # })
  
  bettingGamesLinesServer("bettingGamesLines",
                          futureGameIDs, 
                          futureGameData,
                          teamsData,
                          gameDataLong)
  #outputOptions(output, "bettingGamesLines-bettingGamesLinesUI", suspendWhenHidden = FALSE)
  
  
  ## Player Props ----
  AllInputs1 <- reactive({
    x <- reactiveValuesToList(input)
    data.frame(
      #names = names(x)
      values = unlist(x, use.names = TRUE)
    )
  })
  
  output$show_inputs1 <- renderTable({
    AllInputs1()
  })
  
  AllInputs2 <- reactive({
    x <- reactiveValuesToList(input)
    data.frame(
      names = names(x)
      #values = unlist(x, use.names = TRUE)
    )
  })
  
  output$show_inputs2 <- renderTable({
    AllInputs2()
  })
  
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





