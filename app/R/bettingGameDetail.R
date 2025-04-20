# bettingGameDetail.R --------------------------------------------------

# UI ----
bettingGameDetailUI <- function(id) {
  uiOutput(NS(id, "detail_ui"))
}

# Server ----
bettingGameDetailServer <- function(id,
                                    selectedGameID,  # reactiveVal
                                    gameDataLong,    # data.frame
                                    teamsData) {
  moduleServer(id, function(input, output, session) {
    detail_ui_df <- reactive({
      req(selectedGameID())
      cat("[DEBUG] detail_ui rendered for", selectedGameID(), "\n")
      # Pull both away and home rows
      df <- gameDataLong |>
        filter(game_id %in% selectedGameID()) |>
        left_join(
          teamsData |> select(team_abbr, team_nick, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        mutate(
          # record = ifelse(team_T == 0,
          #                 paste0("(", team_W, "-", team_L, ")"),
          #                 paste0("(", team_W, "-", team_L, "-", team_T, ")")),
          record = paste0("(", team_W, "-", team_L, "-", team_T, ")")
        )
      df
    })
    
    output$detail_ui_table <- renderTable({
      gameDataLong |> filter(season == 2024, week > 16)
    }, width = "100%")
    
    output$detail_ui <- renderUI({
      df <- detail_ui_df()
      
      home <- df |> slice(1)
      away <- df |> slice(2)
      
      tagList(
        # sticky header container: full-width inside the box
        div(
          # style = paste(
          #   "position: sticky;",
          #   "top: 57px;",
          #   "left: 0;",
          #   "width: calc(100% + 30px);",    # span full box width including padding
          #   "margin-left: -15px;",          # adjust for box padding
          #   #"margin-right: -15px;",
          #   "background: #ffffff;",
          #   "z-index: 1000;"
          #   #"padding: 10px 15px;"
          # ),
          style = paste(
            "position: sticky;",
            "top: 57px;",                # sticks at top of viewport
            "right: 0px;",
            "left: 0px;",
            "bottom: 15px;",
            "margin-bottom: 1rem;",
            "z-index: 1000;",
            "background: #ffffff;",
            "padding: 10px 0px;",
            "box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
          ),
          fixedRow(
            # Away team
            column(width = 5, align = "center",
                   tags$img(src = away$team_logo_espn, height = "100px")
                   #h4(away$team_nick),
                   #p(strong("Away Record:"), away$record)
            ),
            # Game info
            column(width = 2, align = "center"
                   #h3("vs"),
                   #p(strong("Game ID:"), selectedGameID())
            ),
            # Home team
            column(width = 5, align = "center",
                   tags$img(src = home$team_logo_espn, height = "100px")
                   #h4(home$team_nick),
                   #p(strong("Home Record:"), home$record)
            )
          ),
          fixedRow(
            # Away team
            column(width = 5, align = "center",
                   #tags$img(src = away$team_logo_espn, height = "50px"),
                   h4(away$team_nick)
                   #p(strong("Away Record:"), away$record)
            ),
            # Game info
            column(width = 2, align = "center",
                   h3("vs")
                   #p(strong("Game ID:"), selectedGameID())
            ),
            # Home team
            column(width = 5, align = "center",
                   #tags$img(src = home$team_logo_espn, height = "50px"),
                   h4(home$team_nick)
                   #p(strong("Home Record:"), home$record)
            )
          ),
          fixedRow(
            # Away team
            column(width = 5, align = "center",
                   #tags$img(src = away$team_logo_espn, height = "50px"),
                   #h4(away$team_nick),
                   p(away$record, style = "color: #BEBEBE")
            ),
            # Game info
            column(width = 2, align = "center"
                   #h3("vs"),
                   #p(strong("Game ID:"), selectedGameID())
            ),
            # Home team
            column(width = 5, align = "center",
                   #tags$img(src = home$team_logo_espn, height = "50px"),
                   #h4(home$team_nick),
                   p(home$record, style = "color: #BEBEBE")
            )
          )
        ),
        box(
          title = h5(strong("Game ID:"), selectedGameID()),
          width = 12,
          h5(strong("Game ID:"), selectedGameID())
          #style = "position: relative;",
        ),
        box(
          title = "Test Table",
          width = 12,
          style = "position: relative;",
          fluidRow(
            column(width = 12,
                   tableOutput(NS(id, "detail_ui_table")) 
            )
          )
        )
      )
    })
  })
}
