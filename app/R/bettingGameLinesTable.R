# bettingGamesLinesTable.R ---------------------------------------------

# UI ----
bettingGamesLinesTableOutput <- function(id) {
  uiOutput(NS(id, "bettingGamesLinesTableUI"), inline = TRUE, fill = TRUE)
}

# Server ----
bettingGamesLinesTableServer <- function(id,
                                         teamsData,
                                         gameDataLong,
                                         gameID,
                                         onGameClick) {
  moduleServer(id, function(input, output, session) {
    cat("[DEBUG] calling bettingGamesLinesTableServer.", "\n")
    
    # Invoke callback when the "Full Matchup Comparison" button is clicked
    observeEvent(input$bettingGamesLinesTableBttn, {
      onGameClick(gameID)
    }, ignoreInit = TRUE)
    
    # Prepare the line data
    lineData <- gameDataLong |>
      filter(game_id == gameID) |>
      mutate(
        rowID = row_number(),
        spread_line = -spread_line,
        spread_line = ifelse(spread_line > 0, paste0("+", spread_line), spread_line),
        team_spread_odds = paste0("(", team_spread_odds, ")"),
        total_line = ifelse(rowID == 1, paste0("o", total_line), paste0("u", total_line)),
        total_odds = ifelse(rowID == 1, paste0("(", over_odds, ")"), paste0("(", under_odds, ")")),
        team_moneyline = ifelse(team_moneyline > 0, paste0("+", team_moneyline), team_moneyline),
        # record = ifelse(team_T == 0,
        #                 paste0("(", team_W, "-", team_L, ")"),
        #                 paste0("(", team_W, "-", team_L, "-", team_T, ")")),
        record = paste0("(", team_W, "-", team_L, "-", team_T, ")"),
        location = str_to_sentence(location),
        recordLocation = paste(record, location),
        gametime = ifelse(str_sub(format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"), 1, 1) == "0",
                          str_replace(format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"), "0", ""),
                          format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"))
      ) |>
      left_join(
        teamsData |> select(team_abbr, team_nick, team_logo_espn),
        by = c("team" = "team_abbr")
      ) |>
      select(
        weekday,
        gameday,
        gametime,
        team,
        team_nick,
        recordLocation,
        spread_line,
        team_spread_odds,
        total_line,
        total_odds,
        team_moneyline
      )
    
    cat("[DEBUG] ran lineData. nrow(lineData) = ", nrow(lineData), "\n")
    
    # Render the GT table
    output$bettingGamesLinesTable <- render_gt({
      lineData |>
        select(-c(weekday, gameday, gametime)) |>
        gt() |>
        gt_nfl_logos(columns = team, height = "30px") |>
        gt_merge_stack(col1 = team_nick, col2 = recordLocation,
                       font_size = c("16px", "14px"),
                       font_weight = c("bold", "normal")) |>
        gt_merge_stack(col1 = spread_line, col2 = team_spread_odds,
                       font_size = c("16px", "14px")) |>
        gt_merge_stack(col1 = total_line, col2 = total_odds,
                       font_size = c("16px", "14px")) |>
        tab_style(
          style = cell_borders(sides = "bottom", style = "hidden"),
          locations = cells_body(columns = c(team, team_nick), rows = 1)
        ) |>
        tab_style(
          locations = cells_body(columns = c(team_moneyline)),
          style = cell_text(v_align = "top")
        ) |>
        tab_options(table.border.top.style = "hidden") |>
        cols_width(team_nick ~ "100px") |>
        cols_align(columns = c(spread_line, total_line, team_moneyline), align = "center") |>
        cols_label(
          team = "",
          team_nick = "",
          spread_line = "Spread",
          total_line = "Total",
          team_moneyline = "Moneyline"
        )
    })
    
    # Render the surrounding box with button
    output$bettingGamesLinesTableUI <- renderUI({
      cat("[DEBUG] renderUI called in bettingGamesLinesTableServer.\n")
      box(
        title = div(
          style = "display:flex; justify-content:space-between",
          div(style = "display:inline-block; margin-right:50px",
              strong(lineData$gametime[1])),
          div(style = "display:inline-block; flex:1",
              actionBttn(
                inputId = session$ns("bettingGamesLinesTableBttn"),
                label   = "Full Matchup Comparison",
                style   = "material-flat",
                color   = "primary",
                size    = "xs"
              )
          )
        ),
        width = 12,
        # ← here’s the key change:
        gt_output( session$ns("bettingGamesLinesTable") )
      )
    })
  })
}
