# Read in Data ################################################################
allSeasons <- 2002:most_recent_season()

## Team Data ----
teamsData <- load_teams(current = FALSE)

## Game Data ----
source(file = "./data-raw/gameData.R")

source(file = "./data-raw/gameDataLong.R")

## Play-by-play Data ----
#playsData <- load_pbp(seasons = most_recent_season())

## Player Data ----
### Offense ----
playerOffenseData <- load_player_stats(
  seasons = allSeasons,
  stat_type = "offense"
)

### Defense ----
playerDefenseData <- load_player_stats(
  seasons = allSeasons,
  stat_type = "defense"
)

### Kicking ----
playerKickingData <- load_player_stats(
  seasons = allSeasons,
  stat_type = "kicking"
)

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
  standingsTableData <- reactive({
    standingsSeason <- as.numeric(input$standingsSeason)
    standingsStat <- input$standingsStat
    
    if(standingsSeason == get_current_season()){
      standingsCurrent <- calculateStandings(season = standingsSeason,
                                             game_data = gameData)
      
      gameDataCurrent <- gameData |>
        filter(season == standingsSeason) |>
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
        filter(season == standingsSeason)
    }
    
    standingsTableData |>
      rowwise() |>
      mutate(
        PF = ifelse(standingsStat == "Total", PF, round(PF/GP, 2)),
        PA = ifelse(standingsStat == "Total", PA, round(PA/GP, 2)),
        PD = ifelse(standingsStat == "Total", PD, round(PD/GP, 2)),
      )
  })
  
  ### AFC Table ----
  output$standingsTableAFC <- render_gt({
    standingsSeason <- as.numeric(input$standingsSeason)
    conf_logo <- teamsData |> 
      filter(team_conf == "AFC") |>
      pull(team_conference_logo) |>
      unique()
    
    standingsTableData() |>
      filter(team_conf == "AFC") |>
      select(
        "team",
        "team_name",
        "team_division",
        "div_rank",
        "GP",
        "W",
        "L",
        "T",
        "W-L%",
        "PF",
        "PA",
        "PD",
        "MOV",
        "SOS",
        "SRS",
        "OSRS",
        "DSRS"
      ) |>
      group_by(team_division) |>
      arrange(team_division, div_rank) |>
      gt() |>
      cols_hide(
        columns = "div_rank"
      ) |>
      gt_nfl_logos(
        columns = "team",
        height = "25px"
      ) |>
      fmt_percent(
        columns = "W-L%",
        decimals = 1
      ) |>
      fmt_number(
        columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
        decimals = 2
      ) |>
      data_color(
        columns = c("SRS"),
        method = "numeric",
        palette = c("red", "green")
      ) |>
      tab_header(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo, style = "height: 25px;"),
                    strong(standingsSeason, style = "margin-left: 6px"),
                    strong("Standings", style = "margin-left: 4px")
        )
      ) |>
      tab_options(
        data_row.padding = 0,
        column_labels.font.weight = "bold",
        heading.title.font.size = "150%",
        table.font.size = "90%"
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(
          columns = c("team_name", "W-L%", "PD")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "right", weight = "0.5px"),
        locations = cells_body(
          columns = c("GP")
        )
      ) |>
      cols_label(
        team = "",
        team_name = "Team"
      )
  })
  
  ### NFC Table ----
  output$standingsTableNFC <- render_gt({
    standingsSeason <- as.numeric(input$standingsSeason)
    conf_logo <- teamsData |> 
      filter(team_conf == "NFC") |>
      pull(team_conference_logo) |>
      unique()
    
    standingsTableData() |>
      filter(team_conf == "NFC") |>
      select(
        "team",
        "team_name",
        "team_division",
        "div_rank",
        "GP",
        "W",
        "L",
        "T",
        "W-L%",
        "PF",
        "PA",
        "PD",
        "MOV",
        "SOS",
        "SRS",
        "OSRS",
        "DSRS"
      ) |>
      group_by(team_division) |>
      arrange(team_division, div_rank) |>
      gt() |>
      cols_hide(
        columns = "div_rank"
      ) |>
      gt_nfl_logos(
        columns = "team",
        height = "25px"
      ) |>
      fmt_percent(
        columns = "W-L%",
        decimals = 1
      ) |>
      fmt_number(
        columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
        decimals = 2
      ) |>
      data_color(
        columns = c("SRS"),
        method = "numeric",
        palette = c("red", "green")
      ) |>
      tab_header(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo, style = "height: 25px;"),
                    strong(standingsSeason, style = "margin-left: 6px"),
                    strong("Standings", style = "margin-left: 4px")
        )
      ) |>
      tab_options(
        data_row.padding = 0,
        column_labels.font.weight = "bold",
        heading.title.font.size = "150%",
        table.font.size = "90%"
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(
          columns = c("team_name", "W-L%", "PD")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "right", weight = "0.5px"),
        locations = cells_body(
          columns = c("GP")
        )
      ) |>
      cols_label(
        team = "",
        team_name = "Team"
      )
  })
  
  ### AFC Playoffs Table ----
  output$standingsTableAFCplayoffs <- render_gt({
    standingsSeason <- as.numeric(input$standingsSeason)
    conf_logo <- teamsData |> 
      filter(team_conf == "AFC") |>
      pull(team_conference_logo) |>
      unique()
    
    standingsTableData() |>
      filter(team_conf == "AFC") |>
      select(
        "seed",
        "team",
        "team_name",
        "GP",
        "W",
        "L",
        "T",
        "W-L%",
        "CON%",
        "DIV%"
      ) |>
      arrange(seed, desc(`W-L%`), desc(`CON%`), desc(`DIV%`)) |>
      gt() |>
      sub_missing(
        columns = "seed"
      ) |>
      gt_nfl_logos(
        columns = "team",
        height = "25px"
      ) |>
      fmt_percent(
        columns = c("W-L%", "CON%", "DIV%"),
        decimals = 1
      ) |>
      tab_header(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo, style = "height: 25px;"),
                    strong(standingsSeason, style = "margin-left: 6px"),
                    strong("Playoff Standings", style = "margin-left: 4px")
        )
      ) |>
      tab_options(
        data_row.padding = 0,
        column_labels.font.weight = "bold",
        heading.title.font.size = "150%",
        table.font.size = "90%"
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(
          columns = c("team_name", "T")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "right", weight = "0.5px"),
        locations = cells_body(
          columns = c("GP")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "bottom", weight = "2px"),
        locations = cells_body(
          rows =  7
        )
      ) |>
      cols_label(
        seed = "Seed",
        team = "",
        team_name = "Team"
      )
  })
  
  ### NFC Playoffs Table ----
  output$standingsTableNFCplayoffs <- render_gt({
    standingsSeason <- as.numeric(input$standingsSeason)
    conf_logo <- teamsData |> 
      filter(team_conf == "NFC") |>
      pull(team_conference_logo) |>
      unique()
    
    standingsTableData() |>
      filter(team_conf == "NFC") |>
      select(
        "seed",
        "team",
        "team_name",
        "GP",
        "W",
        "L",
        "T",
        "W-L%",
        "CON%",
        "DIV%"
      ) |>
      arrange(seed, desc(`W-L%`), desc(`CON%`), desc(`DIV%`)) |>
      gt() |>
      sub_missing(
        columns = "seed"
      ) |>
      gt_nfl_logos(
        columns = "team",
        height = "25px"
      ) |>
      fmt_percent(
        columns = c("W-L%", "CON%", "DIV%"),
        decimals = 1
      ) |>
      tab_header(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo, style = "height: 25px;"),
                    strong(standingsSeason, style = "margin-left: 6px"),
                    strong("Playoff Standings", style = "margin-left: 4px")
        )
      ) |>
      tab_options(
        data_row.padding = 0,
        column_labels.font.weight = "bold",
        heading.title.font.size = "150%",
        table.font.size = "90%"
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(
          columns = c("team_name", "T")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "right", weight = "0.5px"),
        locations = cells_body(
          columns = c("GP")
        )
      ) |>
      tab_style(
        style = cell_borders(sides = "bottom", weight = "2px"),
        locations = cells_body(
          rows =  7
        )
      ) |>
      cols_label(
        seed = "Seed",
        team = "",
        team_name = "Team"
      )
  })
  
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
  #### Scrimmage ----
  #### Passing ----
  output$playerOffensePassingTable <- renderReactable({
    inputSeason <- seq(input$playerOffenseSeason[1], input$playerOffenseSeason[2])
    inputGameType <- input$playerOffenseGameType
    inputTeams <- input$playerOffenseTeam
    inputStat <- input$playerOffenseStat
    
    playerOffensePassingTableBase <- playerOffenseData |>
      filter(season %in% inputSeason) |>
      filter(season_type %in% inputGameType) |>
      filter(recent_team %in% inputTeams) |>
      rename(team_abbr = recent_team) |>
      filter(attempts > 0) |>
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
      {if(inputStat == "Total"){
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
    
    playerOffensePassingTableReactData <- playerOffensePassingTableBase |>
      left_join(teamsData |> select(team_abbr, team_logo_espn),
                by = join_by(team_abbr)) |>
      select(team_logo_espn, everything())
    
    playerOffensePassingTableReact <- reactable(
      data = playerOffensePassingTableReactData,
      theme = espn(),
      highlight = TRUE,
      compact = TRUE,
      pagination = FALSE,
      wrap = FALSE,
      outlined = TRUE,
      showSortable = FALSE,
      defaultColDef = colDef(vAlign = "center", 
                             minWidth = 60,
                             headerStyle = list(fontSize = "14px")
      ),
      defaultSortOrder = "desc",
      defaultSorted = c("passing_yards"),
      columns = list(
        ##### Team Logo 
        team_logo_espn = colDef(
          name = "Player",
          minWidth = 150,
          sortable = FALSE,
          #cell = embed_img()
          cell = function(value, index){
            player_name <- playerOffensePassingTableReactData$player_display_name[index]
            logo <- img(src = value, style = "height: 20px;")
            team <- playerOffensePassingTableReactData$team_abbr[index]
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
      )
    )
    return(playerOffensePassingTableReact)
  })
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
  # Prediction Tab  #########################################
  
}) # end server





