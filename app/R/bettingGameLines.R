## Create Standings Tables ----

# UI ----
bettingGamesLinesUI <- function(id){
  uiOutput(NS(id, "bettingGamesLinesUI"))
}


# Server ----
bettingGamesLinesServer <- function(id,
                                    futureGameIDs,
                                    futureGameData,
                                    teamsData,
                                    gameDataLong){
  
  lapply(futureGameIDs, function(x){
    bettingGamesLinesTableServer(x, teamsData, gameDataLong, gameID = x)
  })
  
  moduleServer(id, function(input, output, session){
    
    output$bettingGamesLinesUI <- renderUI({
      req(length(futureGameIDs) > 0)
      
      bettingGamesLinesTagList <- tagList()
      
      for(i in 1:nrow(futureGameData)){
        if(i == 1){
          futureWeek <- h1("Week", futureGameData$week[i])
          futureDate <- h3(format(as_date(futureGameData |>
                                            select(gameday, weekday) |>
                                            slice(i) |>
                                            pull(gameday)),
                                  "%A, %B %d"))
          # futureGame1 <- div(bettingGamesLinesTableOutput(futureGameIDs[i]),
          #                    style = "display: inline-flex; align-items: center; margin-right: 20px")
          futureGame <- bettingGamesLinesTableOutput(futureGameIDs[i])
        }else{
          if(futureGameData$week[i] != futureGameData$week[i-1]){
            futureWeek <- h1("Week", futureGameData$week[i])
          }else{futureWeek <- NULL}
          if(futureGameData$gameday[i] != futureGameData$gameday[i-1]){
            futureDate <- h3(format(as_date(futureGameData |>
                                              select(gameday, weekday) |>
                                              slice(i) |>
                                              pull(gameday)),
                                    "%A, %B %d"))
          }else{futureDate <- NULL}
          futureGame <- bettingGamesLinesTableOutput(futureGameIDs[i])
        }
        bettingGamesLinesTagList <- tagList(
          bettingGamesLinesTagList,
          futureWeek,
          futureDate,
          div(futureGame,
              style = "display: inline-flex; align-items: center; margin-right: 20px")
        )
      }
      #lapply(futureGameIDs, function(x){bettingGamesLinesTableOutput(x)})
      if(is.null(futureGame)){
        return(NULL)
      }else{
        bettingGamesLinesTagList
      }
    })
  }) # end module Server
} # end bettingGamesLines Server