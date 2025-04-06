# bettingGamesLines.R --------------------------------------------------

# UI ----
bettingGamesLinesUI <- function(id) {
  uiOutput(NS(id, "bettingGamesLinesUIout"))
}

# Server ----
bettingGamesLinesServer <- function(id,
                                    futureGameIDs,
                                    futureGameData,
                                    teamsData,
                                    gameDataLong) {
  lapply(futureGameIDs, function(x) {
    bettingGamesLinesTableServer(
      #id = game_id,
      x,
      teamsData = teamsData,
      gameDataLong = gameDataLong,
      gameID = x
    )
  })
  
  moduleServer(id, function(input, output, session) {
    
    # observe({
    #   #req(futureGameIDs)
    #   lapply(futureGameIDs, function(game_id) {
    #     bettingGamesLinesTableServer(
    #       id = game_id,
    #       teamsData = teamsData,
    #       gameDataLong = gameDataLong,
    #       gameID = game_id
    #     )
    #   })
    # }) |> bindEvent(futureGameIDs)


    output$bettingGamesLinesUIout <- renderUI({
      req(nrow(futureGameData) > 0)

      splitGames <- split(futureGameData, futureGameData$gameday)

      tagList(
        h1(strong(glue::glue("Week {futureGameData$week[1]}"))),
        hr(),
        lapply(names(splitGames), function(gameday) {
          gamesOnDay <- splitGames[[gameday]]
          
          tagList(
            h3(format(as.Date(gameday), "%A, %B %d")),
            fluidRow(
              # 3 columns per row
              lapply(gamesOnDay$game_id, function(gid) {
                #cat("Parent renderUI: Adding UI for game_id", gid, "\n")
                column(
                  width = 4,
                  # Use the child module's UI output.
                  bettingGamesLinesTableOutput(gid)
                )
              })
            )
          )

          # tagList(
          #   h3(format(as.Date(gameday), "%A, %B %d")),
          #   layout_column_wrap(
          #     #style = "display: flex; justify-content: center;",
          #     align = "center",
          #     width = 1/3, # 3 cards per row
          #     fixed_width = FALSE,
          #     fill = FALSE,
          #     fillable = TRUE,
          #     heights_equal = "all",
          #     gap = "20px",
          #     !!!lapply(gamesOnDay$game_id, function(game_id) {
          #       bettingGamesLinesTableOutput(game_id)
          #     })
          #   )
          # )
        })
      )
    })
  })
}