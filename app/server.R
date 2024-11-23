# Read in Data ################################################################
allSeasons <- 2002:most_recent_season()

## Team Data ----
teamsData <- load_teams(current = FALSE)

## Game Data ----
source(file = "./data-raw/gameData.R")

source(file = "./data-raw/gameDataLong.R")

## Play-by-play Data ----
#pbpData <- load_pbp(seasons = 2002:most_recent_season())

## Player Data ----
### Offense ----
playerOffenseData <- load_player_stats(
  seasons = allSeasons,
  stat_type = "offense"
)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "NFLdata",
                 user = "postgre",
                 password = "NFLpass1234",
                 host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")

playerOffenseData <- tbl(con, "playerOffenseData")

# Load Historical Data ----
load(file = "./data/seasonStandings.rda")

# Source files ============================================
#source(file = "Testing Scripts/SummaryPlayerFunctions.R")
source("./R/calculateStandings.R", local = TRUE)

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
    
    if(standingsSeason() == get_current_season()){
      standingsCurrent <- calculateStandings(season = get_current_season(),
                                             game_data = gameData)
      
      gameDataCurrent <- gameData |>
        filter(season == get_current_season()) |>
        filter(game_type == "REG") 
      
      standingCurrentNFLverse <- calculate_standings(
        nflverse_object = gameDataCurrent |> filter(!is.na(result)),
        tiebreaker_depth = 2
      )
      
      standingsTableData <- standingsCurrent |>
        left_join(
          standingCurrentNFLverse |>
            select(
              team,
              div_rank,
              seed,
              div_pct,
              conf_pct,
              sov,
              sos),
          by = join_by(team)
        ) |>
        select(
          team,
          team_name,
          team_conf,
          team_division,
          div_rank,
          seed,
          games_played,
          win,
          loss,
          tie,
          win_loss_percent,
          conf_pct,
          div_pct,
          everything()
        ) |>
        left_join(
          teamsData,
          by = join_by(team == team_abbr,team_name, team_conf, team_division)
        ) |>
        rename(
          "GP" = games_played,
          "W" = win,
          "L" = loss,
          "T" = tie,
          "W-L%" = win_loss_percent,
          "CON%" = conf_pct,
          "DIV%" = div_pct,
          "PF" = team_score,
          "PA" = opponent_score,
          "PD" = result
        )
    }else{
      standingsTableData <- seasonStandings |>
        filter(season == standingsSeason())
    }
    
    standingsTableData |>
      rowwise() |>
      mutate(
        PF = ifelse(standingsStat() == "Total", PF, round(PF/GP, 2)),
        PA = ifelse(standingsStat() == "Total", PA, round(PA/GP, 2)),
        PD = ifelse(standingsStat() == "Total", PD, round(PD/GP, 2)),
      )
  })
  
  ### AFC Table ----
  standingsTableServer("standingsTableAFC",
                       standingsSeason,
                       teamsData,
                       standingsTableData,
                       conference = "AFC")
  
  ### NFC Table ----
  standingsTableServer("standingsTableNFC",
                       standingsSeason,
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
  
  # Prediction Tab  #########################################
  
}) # end server





