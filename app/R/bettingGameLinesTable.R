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
    filter(season == most_recent_season()) |>
    group_by(team) |>
    mutate(
      team_W = ifelse(is.na(lag(team_W)), 0, lag(team_W)),
      team_L = ifelse(is.na(lag(team_L)), 0, lag(team_L)),
      team_T = ifelse(is.na(lag(team_T)), 0, lag(team_T))
    ) |>
    ungroup() |>
    filter(game_id == gameID) |>
    mutate(
      rowID = row_number(),
      team_spread_odds = paste0("(", team_spread_odds, ")"),
      total_line = ifelse(rowID == 1, paste0("over ", total_line), paste0("under ", total_line)),
      total_odds = ifelse(rowID == 1, paste0("(",over_odds,")"), paste0("(",under_odds,")")),
      record = ifelse(team_T == 0, 
                      paste0("(", team_W, "-", team_L, ")"),
                      paste0("(", team_W, "-", team_L, "-", team_T, ")")),
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
      record,
      location,
      spread_line,
      team_spread_odds,
      total_line,
      total_odds,
      team_moneyline
    )
  
  moduleServer(id, function(input, output, session){
    
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
          col2 = record,
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
            columns = c(team, team_nick, location),
            rows = 1
          )
        ) |>
        tab_style(
          style = list(
            cell_borders(sides = "right", weight = "0.5px"),
            cell_text(v_align = "top",color = "grey")
          ),
          locations = cells_body(columns = c(location))
        ) |>
        tab_style(
          locations = cells_body(columns = c(team_moneyline)),
          style = cell_text(v_align = "top")
        ) |>
        tab_options(
          table.border.top.style = "hidden"
        ) |>
        cols_width(
          location ~ "100px"
        ) |>
        cols_align(
          columns = c(spread_line, total_line, team_moneyline),
          align = "center"
        ) |>
        cols_label(
          team = "",
          team_nick = "",
          location = "",
          spread_line = "Spread",
          total_line = "Total",
          team_moneyline = "Moneyline"
        )
    }, width = pct(100)) # end renderGT
    
    output$bettingGamesLinesTableUI <- renderUI({
      #fluidRow(
      #box(
      # title = div(style = "display: flex; align-items: center;",
      #             strong(lineData$weekday[1]), ",",
      #             strong(lineData$gameday[1]), #, style = "margin-left: 6px; font-size: 25px"),
      #             strong(lineData$gametime[1]) #, style = "margin-left: 4px; font-size: 25px")
      # ),
      #width = 4,
      gt_output(NS(id, "bettingGamesLinesTable"))
      #)
      #)
    })
  }) # end module Server
} # end bettingGameLinesTable Server


# bettingGamesLinesTableApp <- function() {
#   ui <- fluidPage(
#     uiOutput(outputId = "bettingGamesLinesUI")
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
#       bettingGamesLinesTableServer(x, teamsData, futureGameDataLong, gameID = x)
#     })
#     
#     output$bettingGamesLinesUI <- renderUI({
#       #format(as_date(unique(futureGameData |> select(gameday, weekday))$gameday[1]), "%A, %B %d")
#       req(length(futureGameIDs) > 0)
#       bettingGamesLinesTagList <- tagList()
#       
#       for(i in 1:nrow(futureGameData)){
#         if(i == 1){
#           futureWeek1 <- h1("Week", futureGameData$week[i])
#           futureDate1 <- h3(format(as_date(futureGameData |>
#                                              select(gameday, weekday) |>
#                                              slice(i) |>
#                                              pull(gameday)),
#                                    "%A, %B %d"))
#           futureGame1 <- div(bettingGamesLinesTableOutput(futureGameIDs[i]),
#                              style = "display: inline-flex; align-items: center; margin-right: 20px")
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
#           bettingGamesLinesTagList <-tagList(
#             bettingGamesLinesTagList,
#             futureWeek,
#             futureDate,
#             div(bettingGamesLinesTableOutput(futureGameIDs[i]), 
#                 style = "display: inline-flex; align-items: center; margin-right: 20px")
#           )
#         }
#       }
#       bettingGamesLinesTagList <- tagList(
#         futureWeek1,
#         futureDate1,
#         futureGame1,
#         bettingGamesLinesTagList
#         #lapply(futureGameIDs, function(x){bettingGamesLinesTableOutput(x)})
#       )
#       bettingGamesLinesTagList
#     })
#   }
#   shinyApp(ui, server)
# }
# 
# bettingGamesLinesTableApp()
