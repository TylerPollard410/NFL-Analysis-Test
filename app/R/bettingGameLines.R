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
                                    gameDataLong,
                                    onGameClick) {
  moduleServer(id, function(input, output, session) {
    # Register each child module anytime futureGameIDs() changes
    observe({
      ids <- futureGameIDs()
      req(ids)
      cat("[DEBUG] registering child modules for IDs:", paste(ids, collapse = ", "), "\n")
      # Initialize each child module with a string ID
      lapply(ids, function(gid) {
        module_id <- paste0("game", gid)
        bettingGamesLinesTableServer(
          id           = module_id,
          teamsData    = teamsData,
          gameDataLong = gameDataLong,
          gameID       = gid,
          onGameClick  = onGameClick
        )
      })
    })
    
    # Render the parent UI container for each game card
    output$bettingGamesLinesUIout <- renderUI({
      df <- futureGameData()
      cat("[DEBUG] renderUI called in bettingGamesLinesServer. df rows =", nrow(df), "\n")
      showNotification(paste0("Debug: rendering UI with ", nrow(df), " games"), type = "message", duration = 2)
      req(df, nrow(df) > 0)
      
      splitGames <- split(df, df$gameday)
      # Build the UI tags for each day and each game
      tagList(
        h1(strong(glue::glue("Week {unique(df$week)}"))),
        hr(),
        lapply(names(splitGames), function(day) {
          gamesOnDay <- splitGames[[day]]
          tagList(
            h3(format(as.Date(day), "%A, %B %d")),
            fluidRow(
              # For each game, register its column and return it
              lapply(gamesOnDay$game_id, function(gid) {
                module_id <- paste0("game", gid)
                col <- column(
                  width = 4,
                  # Use the parent session namespace for the child UI
                  bettingGamesLinesTableOutput(
                    session$ns(module_id)
                  )
                )
                cat("[DEBUG] bettingGamesLinesTableOutput rendered for", module_id, "\n")
                col
              })
            )
          )
        })
      )
    })
  })
}
