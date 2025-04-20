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
    #  Reactive helpers
    detail_ui_df <- reactive({
      req(selectedGameID())
      cat("[DEBUG] detail_ui rendered for", selectedGameID(), "\n")
      gameDataLong |>
        filter(game_id %in% selectedGameID()) |>
        left_join(
          teamsData |> select(team_abbr, team_nick, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        mutate(record = paste0("(", team_W, "-", team_L, "-", team_T, ")"))
    })
    
    output$detail_ui_table <- renderTable({
      gameDataLong |> filter(season == 2024, week > 16)
    })
    
    observeEvent(input$away_click, {
      showNotification(paste("Away team clicked:", detail_ui_df() |> slice(2) |> pull(team_nick)))
    })
    observeEvent(input$home_click, {
      showNotification(paste("Home team clicked:", detail_ui_df() |> slice(1) |> pull(team_nick)))
    })
    
    #  UI (ONLY tagList edited as requested)
    output$detail_ui <- renderUI({
      df   <- detail_ui_df()
      home <- df |> slice(1)
      away <- df |> slice(2)
      
      tagList(
        # ── Sticky banner ─────────────────────────────────────
        tags$div(
          style = paste(
            "position: sticky;",
            "top: 57px;",
            "z-index: 1000;",
            "background: #ffffff;",
            "padding: 10px 15px;",
            "box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            "display: flex;",
            "align-items: center;",          # center everything vertically
            "justify-content: space-between;",
            "flex-wrap: nowrap;",
            "overflow: hidden;",
            "margin-bottom: 1.25rem;"        # visible gap below banner
          ),
          
          # Away team block ------------------------------------------------
          tags$div(
            style = paste(
              "flex: 0 1 45%;",               # allow shrink but max ~45%
              "display: flex;",
              "flex-direction: column;",
              "justify-content: center;",
              "align-items: center;"
            ),
            actionLink(
              inputId = session$ns("away_click"),
              label   = tags$img(src = away$team_logo_espn, style = "max-height:100px;"),
              style   = "padding:0; border:none; background:none;"
            ),
            h4(
              away$team_nick,
              style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin: 0;"
            ),
            tags$div(
              away$record,
              style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color:#777; font-size:0.9em;"
            )
          ),
          
          # VS block --------------------------------------------------------
          tags$div(
            style = paste(
              "flex: 0 0 auto;",
              "display: flex;",
              "flex-direction: column;",
              "justify-content: center;",
              "align-items: center;",
              "margin: 0 1rem;"
            ),
            h3("VS", style = "margin:0;")
          ),
          
          # Home team block -------------------------------------------------
          tags$div(
            style = paste(
              "flex: 0 1 45%;",
              "display: flex;",
              "flex-direction: column;",
              "justify-content: center;",
              "align-items: center;"
            ),
            actionLink(
              inputId = session$ns("home_click"),
              label   = tags$img(src = home$team_logo_espn, style = "max-height:100px;"),
              style   = "padding:0; border:none; background:none;"
            ),
            h4(
              home$team_nick,
              style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin: 0;"
            ),
            tags$div(
              home$record,
              style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color:#777; font-size:0.9em;"
            )
          )
        ),
        
        # ── Content boxes ─────────────────────────────────────
        box(
          title = "Game Details",
          width = 12,
          style = "margin-top: 1rem;",
          p("Additional game details here...")
        ),
        box(
          title = "Test Table",
          width = 12,
          tableOutput(NS(id, "detail_ui_table"))
        )
      )
    })
  })
}



# bettingGameDetail.R
# 
# # UI
# bettingGameDetailUI <- function(id) {
#   uiOutput(NS(id, "detail_ui"))
# }
# 
# # Server
# bettingGameDetailServer <- function(id,
#                                     selectedGameID,  # reactiveVal
#                                     gameDataLong,    # data.frame
#                                     teamsData) {
#   moduleServer(id, function(input, output, session) {
#     detail_ui_df <- reactive({
#       req(selectedGameID())
#       cat("[DEBUG] detail_ui rendered for", selectedGameID(), "\n")
#       # Pull both away and home rows
#       df <- gameDataLong |>
#         filter(game_id %in% selectedGameID()) |>
#         left_join(
#           teamsData |> select(team_abbr, team_nick, team_logo_espn),
#           by = c("team" = "team_abbr")
#         ) |>
#         mutate(
#           # record = ifelse(team_T == 0,
#           #                 paste0("(", team_W, "-", team_L, ")"),
#           #                 paste0("(", team_W, "-", team_L, "-", team_T, ")")),
#           record = paste0("(", team_W, "-", team_L, "-", team_T, ")")
#         )
#       df
#     })
#     
#     output$detail_ui_table <- renderTable({
#       gameDataLong |> filter(season == 2024, week > 16)
#     }, width = "100%")
#     
#     output$detail_ui <- renderUI({
#       df <- detail_ui_df()
#       
#       home <- df |> slice(1)
#       away <- df |> slice(2)
#       
#       tagList(
#         # sticky header container: full-width inside the box
#         div(
#           # style = paste(
#           #   "position: sticky;",
#           #   "top: 57px;",
#           #   "left: 0;",
#           #   "width: calc(100% + 30px);",    # span full box width including padding
#           #   "margin-left: -15px;",          # adjust for box padding
#           #   #"margin-right: -15px;",
#           #   "background: #ffffff;",
#           #   "z-index: 1000;"
#           #   #"padding: 10px 15px;"
#           # ),
#           style = paste(
#             "position: sticky;",
#             "top: 57px;",                # sticks at top of viewport
#             "right: 0px;",
#             "left: 0px;",
#             "bottom: 15px;",
#             
#             "margin-bottom: 1rem;",
#             "z-index: 1000;",
#             "background: #ffffff;",
#             "padding: 10px 0px;",
#             "box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
#           ),
#           fixedRow(
#             # Away team
#             column(width = 5, align = "center",
#                    tags$img(src = away$team_logo_espn, height = "80px")
#                    #h4(away$team_nick),
#                    #p(strong("Away Record:"), away$record)
#             ),
#             # Game info
#             column(width = 2, align = "center"
#                    #h3("vs"),
#                    #p(strong("Game ID:"), selectedGameID())
#             ),
#             # Home team
#             column(width = 5, align = "center",
#                    tags$img(src = home$team_logo_espn, height = "80px")
#                    #h4(home$team_nick),
#                    #p(strong("Home Record:"), home$record)
#             )
#           ),
#           fixedRow(
#             # Away team
#             column(width = 5, align = "center",
#                    #tags$img(src = away$team_logo_espn, height = "50px"),
#                    h4(away$team_nick)
#                    #p(strong("Away Record:"), away$record)
#             ),
#             # Game info
#             column(width = 2, align = "center",
#                    h3("vs")
#                    #p(strong("Game ID:"), selectedGameID())
#             ),
#             # Home team
#             column(width = 5, align = "center",
#                    #tags$img(src = home$team_logo_espn, height = "50px"),
#                    h4(home$team_nick)
#                    #p(strong("Home Record:"), home$record)
#             )
#           ),
#           fixedRow(
#             # Away team
#             column(width = 5, align = "center",
#                    #tags$img(src = away$team_logo_espn, height = "50px"),
#                    #h4(away$team_nick),
#                    p(away$record, style = "color: #BEBEBE")
#             ),
#             # Game info
#             column(width = 2, align = "center"
#                    #h3("vs"),
#                    #p(strong("Game ID:"), selectedGameID())
#             ),
#             # Home team
#             column(width = 5, align = "center",
#                    #tags$img(src = home$team_logo_espn, height = "50px"),
#                    #h4(home$team_nick),
#                    p(home$record, style = "color: #BEBEBE")
#             )
#           )
#         ),
#         box(
#           title = h5(strong("Game ID:"), selectedGameID()),
#           width = 12,
#           h5(strong("Game ID:"), selectedGameID())
#           #style = "position: relative;",
#         ),
#         box(
#           title = "Test Table",
#           width = 12,
#           style = "position: relative;",
#           fluidRow(
#             column(width = 12,
#                    tableOutput(NS(id, "detail_ui_table")) 
#             )
#           )
#         )
#       )
#     })
#   })
# }
