## Create Standings Tables ----

# UI ----
bettingGamesLinesTableOutput <- function(id){
  uiOutput(NS(id, "bettingGamesLinesTableUI"))
}


# Server ----
bettingGamesLinesTableServer <- function(id,
                                         teamsData,
                                         gameDataLong,
                                         gameID){
  
  lineData <- gameDataLong |>
    # filter(season == most_recent_season()) |>
    # group_by(team) |>
    # mutate(
    #   team_W = ifelse(is.na(lag(team_W)), 0, lag(team_W)),
    #   team_L = ifelse(is.na(lag(team_L)), 0, lag(team_L)),
    #   team_T = ifelse(is.na(lag(team_T)), 0, lag(team_T))
    # ) |>
    # ungroup() |>
    filter(game_id == gameID) |>
    mutate(
      rowID = row_number(),
      spread_line = -spread_line,
      spread_line = ifelse(spread_line > 0, paste0("+", spread_line), spread_line),
      team_spread_odds = paste0("(", team_spread_odds, ")"),
      total_line = ifelse(rowID == 1, paste0("o", total_line), paste0("u", total_line)),
      total_odds = ifelse(rowID == 1, paste0("(",over_odds,")"), paste0("(",under_odds,")")),
      team_moneyline = ifelse(team_moneyline > 0, paste0("+", team_moneyline), team_moneyline),
      record = ifelse(team_T == 0, 
                      paste0("(", team_W, "-", team_L, ")"),
                      paste0("(", team_W, "-", team_L, "-", team_T, ")")),
      location = str_to_sentence(location),
      recordLocation = paste(record, location),
      gametime = ifelse(str_sub(format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"), 1, 1) == "0",
                        str_replace(format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"), "0", ""),
                        format(parse_date_time(gametime, "%#H:%M"), "%I:%M %p"))
      
    ) |>
    left_join(
      teamsData |> select(team_abbr, team_nick, team_logo_espn),
      by = join_by(team == team_abbr)
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
  
  moduleServer(id, function(input, output, session){
    
    output$bettingGamesLinesTableUI <- renderUI({
      box(
        title = div(
          style = "display:flex; justify-content:space-between",
          div(
            #style = "flex:1; display:flex; justify-content:flex-start",
            style = "display:inline-block; margin-right: 50px",
            strong(lineData$gametime[1])
          ),
          div(
            #style = "flex:1; display:inline-flex; justify-content:flex-end",
            style = "display:inline-block; flex:1",
            actionBttn(NS(id, "bettingGamesLinesTableBttn"), 
                       label = "Full Matchup Comparison",
                       style = "material-flat",
                       color = "primary",
                       size = "xs")
          )
        ),
        width = 12,
        #div(
          #style = "margin-top:-20px; padding:-10px",
          uiOutput(NS(id, "tablePlaceholder"))
        #)
      )
    })
    
    output$tablePlaceholder <- renderUI({
      withSpinner(
        gt_output(NS(id, "bettingGamesLinesTable")), type = 8
      )
    })
    
    output$bettingGamesLinesTable <- render_gt({
      lineData |>
        select(-c(weekday, gameday, gametime)) |>
        gt() |>
        gt_nfl_logos(
          columns = team,
          height = "30px"
        ) |>
        gt_merge_stack(
          col1 = team_nick,
          col2 = recordLocation,
          font_size = c("16px", "14px"),
          font_weight = c("bold", "normal")
        ) |>
        gt_merge_stack(
          col1 = spread_line,
          col2 = team_spread_odds,
          font_size = c("16px", "14px"),
          font_weight = c("normal", "normal")
        ) |>
        gt_merge_stack(
          col1 = total_line,
          col2 = total_odds,
          font_size = c("16px", "14px"),
          font_weight = c("normal", "normal")
        ) |>
        tab_style(
          style = cell_borders(sides = "bottom", style = "hidden"),
          locations = cells_body(
            columns = c(team, team_nick),
            rows = 1
          )
        ) |>
        tab_style(
          locations = cells_body(columns = c(team_moneyline)),
          style = cell_text(v_align = "top")
        ) |>
        tab_options(
          table.border.top.style = "hidden"
        ) |>
        cols_width(
          team_nick ~ "100px"
        ) |>
        cols_align(
          columns = c(spread_line, total_line, team_moneyline),
          align = "center"
        ) |>
        cols_label(
          team = "",
          team_nick = "",
          spread_line = "Spread",
          total_line = "Total",
          team_moneyline = "Moneyline"
        )
    }) # end renderGT
    
    
    output$bettingGamesLinesTableUI <- renderUI({
      box(
        title = div(
          style = "display:flex; justify-content:space-between",
          div(
            #style = "flex:1; display:flex; justify-content:flex-start",
            style = "display:inline-block; margin-right: 50px",
            strong(lineData$gametime[1])
          ),
          div(
            #style = "flex:1; display:inline-flex; justify-content:flex-end",
            style = "display:inline-block; flex:1",
            actionBttn(NS(id, "bettingGamesLinesTableBttn"), 
                       label = "Full Matchup Comparison",
                       style = "material-flat",
                       color = "primary",
                       size = "xs")
          )
        ),
        width = 12,
        div(
          style = "margin-top:-20px; padding:-10px",
          gt_output(NS(id, "bettingGamesLinesTable"))
        )
      )
    })
  }) # end module Server
} # end bettingGameLinesTable Server

# App Test ----
# bettingGamesLinesTableApp <- function() {
#   ui <- fluidPage(
#     fluidRow(
#       column(12,
#              style = "display:inline-flex; align:center",
#              uiOutput(outputId = "bettingGamesLinesUI")
#       #style = "margin-left:10%; margin-right:10%"
#       )
#     )
#   )
#   server <- function(input, output, session) {
#     allSeasons <- 2002:most_recent_season()
# 
#     ## Team Data ----
#     teamsData <- load_teams(current = FALSE)
# 
#     ## Game Data ----
#     source(file = "./app/data-raw/gameData.R")
# 
#     source(file = "./app/data-raw/gameDataLong.R")
#     
#     futureGameIDs <- gameData |>
#       filter(!is.na(spread_line) & is.na(result)) |>
#       pull(game_id)
#     
#     futureGameData <- gameData |>
#       filter(game_id %in% futureGameIDs)
#     
#     futureGameDataLong <- gameDataLong |>
#       filter(game_id %in% futureGameIDs)
#     
#     futureGameDates <- unique(futureGameData |> select(week, gameday, weekday))
#     
#     lapply(futureGameIDs, function(x){
#       bettingGamesLinesTableServer(x, teamsData, gameDataLong, gameID = x)
#     })
#     
#     output$bettingGamesLinesUI <- renderUI({
#       req(length(futureGameIDs) > 0)
#       bettingGamesLinesTagList <- tagList()
#       
#       for(i in 1:nrow(futureGameData)){
#         if(i == 1){
#           futureWeek <- h1("Week", futureGameData$week[i])
#           futureDate <- h3(format(as_date(futureGameData |>
#                                             select(gameday, weekday) |>
#                                             slice(i) |>
#                                             pull(gameday)),
#                                   "%A, %B %d"))
#           # futureGame1 <- div(bettingGamesLinesTableOutput(futureGameIDs[i]),
#           #                    style = "display: inline-flex; align-items: center; margin-right: 20px")
#           futureGame <- bettingGamesLinesTableOutput(futureGameIDs[i])
#         }else{
#           if(futureGameData$week[i] != futureGameData$week[i-1]){
#             futureWeek <- h1("Week", futureGameData$week[i])
#           }else{futureWeek <- NULL}
#           if(futureGameData$gameday[i] != futureGameData$gameday[i-1]){
#             futureDate <- h3(format(as_date(futureGameData |>
#                                               select(gameday, weekday) |>
#                                               slice(i) |>
#                                               pull(gameday)),
#                                     "%A, %B %d"))
#           }else{futureDate <- NULL}
#           futureGame <- bettingGamesLinesTableOutput(futureGameIDs[i])
#         }
#         bettingGamesLinesTagList <- tagList(
#           bettingGamesLinesTagList,
#           futureWeek,
#           futureDate,
#           div(futureGame,
#               style = "display: inline-flex; align-items: center; margin-right: 20px")
#         )
#       }
#       #lapply(futureGameIDs, function(x){bettingGamesLinesTableOutput(x)})
#       bettingGamesLinesTagList
#     })
#   }
#   shinyApp(ui, server)
# }
# 
# bettingGamesLinesTableApp()
