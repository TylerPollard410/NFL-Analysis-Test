# Source files ----
#source(file = "Testing Scripts/SummaryPlayerFunctions.R")

# Read in Data ################################################################
## Game Data ----
gameData <- load_schedules(seasons = 2003:most_recent_season())

## Play-by-play Data ----
#playsData <- load_pbp(seasons = most_recent_season())

## Team Data ----
teamsData <- load_teams(current = FALSE)

## Player Data ----
### Offense ----
playerOffenseData <- load_player_stats(
  seasons = 2003:most_recent_season(),
  stat_type = "offense"
)

### Defense ----
playerDefenseData <- load_player_stats(
  seasons = 2003:most_recent_season(),
  stat_type = "defense"
)

### Kicking ----
playerKickingData <- load_player_stats(
  seasons = 2003:most_recent_season(),
  stat_type = "kicking"
)


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
  
  # Home Tab  ###############################################
  output$image <- renderImage({
    filename <- normalizePath(file.path("www/nfl_logo.jpeg"))
    list(src = filename,
         width = "60%",
         height = "400px",
         align = "center")
  }, deleteFile = FALSE)
  
  # Data Tab ################################################
  ## Standings Tab ##########################################
  output$standingsTable <- renderReactable({
    standingPFRData <- read_html("https://www.pro-football-reference.com/years/2024/index.htm")
    standingPFRDataTables <- standingPFRData |> html_table()
    standingAFCDataTables <- standingPFRDataTables[[1]] |>
      slice(-c(1,6,11,16)) |>
      mutate(
        across(-Tm, as.numeric)
        
      )
    standingNFCDataTables <- standingPFRDataTables[[2]] |>
      slice(-c(1,6,11,16)) |>
      mutate(
        across(-Tm, as.numeric)
      )
    
    standingDataTable <- bind_rows(
      standingAFCDataTables,
      standingNFCDataTables
    ) |>
      rename(team_name = Tm) |>
      mutate(
        team_name = str_replace_all(team_name, "[:punct:]|[:symbol:]", "")
      ) |>
      left_join(
        teamsData |>
          select(team_name, team_conf, team_division, team_logo_espn)
      ) |>
      select(
        team_conf,
        team_division,
        team_logo_espn,
        team_name,
        everything()
      )
    standingDataTable
    
    standingTable <- reactable(
      data = standingDataTable,
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
      #defaultSortOrder = "asc",
      # defaultSorted = c("asc" = c("team_conf", "team_division"),
      #                   #"asc" = "team_division", 
      #                   #"desc" = "W-L%", 
      #                   "desc" = c("W-L%", "SRS")),
      defaultSorted = list(team_conf = "asc",
                           team_division = "asc",
                           `W-L%` = "desc",
                           SRS = "desc"),
      rowStyle = group_border_sort(columns = c("team_conf", "team_division"),),
      columns = list(
        ##### Conference
        team_conf = colDef(
          name = "CON",
          style = group_merge_sort("team_conf")
        ),
        ##### Conference
        team_division = colDef(
          name = "DIV",
          style = list(
            group_merge_sort = "team_division"#,
           # borderBottom = "1px solid black"
          )
        ),
        ##### Team Logo 
        team_logo_espn = colDef(
          name = "Team",
          minWidth = 200,
          sortable = FALSE,
          #cell = embed_img()
          cell = function(value, index){
            team <- standingDataTable$team_name[index]
            logo <- img(src = value, style = "height: 20px;")
            div(style = "display: flex; align-items: center;",
                logo,
                span(team, style = "margin-left: 4px")
            )
          },
          style = list(borderRight = "1px solid black")
        ),
        ##### Team Name 
        team_name = colDef(
          show = FALSE
        )
      )
    )
    return(standingTable)
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
    inputSeason <- seq(input$playerOffensePassingSeason[1], input$playerOffensePassingSeason[2])
    inputGameType <- input$playerOffensePassingGameType
    inputTeams <- input$playerOffensePassingTeam
    inputStat <- input$playerOffensePassingStat
    
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
      left_join(teamsData |> select(team_abbr, team_logo_espn)) |>
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





