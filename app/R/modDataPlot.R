## Create Output table for Player Offense Passing

# UI module ----

modDataPlotOutput <- function(id){
  tagList(
    verbatimTextOutput(outputId = NS(id, "modDataPrint")),
    plotOutput(outputId = NS(id, "modDataPlot"))
  )
}


# Server Module ----
modDataPlotServer <- function(id,
                              teamsData,
                              modData,
                              modPlotInputs){
  moduleServer(id, function(input, output, session){
    seasons <- reactive(modPlotInputs$seasons())
    gameType <- reactive(modPlotInputs$gameType())
    teams <- reactive(modPlotInputs$teams())
    statType <- reactive(modPlotInputs$statType())
    xVar <- reactive(modPlotInputs$xVar())
    yVar <- reactive(modPlotInputs$yVar())
    colorVar <- reactive(modPlotInputs$colorVar())
    facetVar <- reactive(modPlotInputs$facetVar())
    fitLine <- reactive(modPlotInputs$fitLine())
    
    modPlotData <- reactive({
      modData |>
        filter(season %in% seasons(),
               season_type %in% gameType(),
               home_team %in% teams() | away_team %in% teams()) |>
        select(
          season,
          season_type,
          home_team,
          away_team,
          xVar(),
          yVar(),
          colorVar(),
          facetVar()
        )
    })
    
    output$modDataPrint <- renderPrint({
      modPlotData()
    })
    
    output$modDataPlot <- renderPlot({
      validate(
        need(xVar(), "Please select x variable to plot"),
        need(yVar(), "Please select y variable to plot")
      )
      if(!is.null(colorVar())){
        plot <- ggplot(data = modPlotData(), 
                       aes(x = !!sym(xVar()), 
                           y = !!sym(yVar()),
                           color = !!sym(colorVar()))) +
          geom_point()
      }else{
        plot <- ggplot(data = modPlotData(), 
                       aes(x = !!sym(xVar()), 
                           y = !!sym(yVar()))) +
          geom_point()
      }
      
      if(!is.null(facetVar())){
        plot2 <- plot + facet_wrap(vars(!!sym(facetVar())))
      }else{
        plot2 <- plot
      }
      
      plot2 + theme_bw()
    })
  })
}


# App Test ----
# playerOffensePassingTableApp <- function() {
#
#   # Load Libraries ----
#   library(shiny)
#   library(shinydashboard)
#   library(bs4Dash)
#   library(shinyWidgets)
#   library(shinycssloaders)
#   library(shinyjs)
#   library(waiter)
#   library(RColorBrewer)
#   library(fresh)
#   library(markdown)
#
#   ## Data Manipulation
#   library(stringr)
#   library(rvest)
#
#   ## Tables ----
#   library(DBI)
#   library(RPostgres)
#   library(data.table)
#   library(htmltools)
#   library(gt)
#   library(gtsummary)
#   library(gtExtras)
#   library(reactable)
#   library(reactablefmtr)
#
#   ## Plotting ----
#   library(smplot2)
#   # library(cowplot)
#   # library(GGally)
#   library(patchwork)
#
#   ## Modeling ----
#   library(pracma)
#   library(forecast)
#   library(elo)
#   library(MASS)
#   library(bestNormalize)
#   library(tictoc)
#   library(caret)
#   library(splines)
#   library(mgcv)
#   library(DescTools)
#   library(car)
#   library(bayesplot)
#   library(BayesFactor)
#   library(rstanarm)
#   library(tidybayes)
#   library(loo)
#   library(brms)
#   library(performance)
#
#   ## NFL Verse ----
#   library(nflverse)
#
#   ## Tidyverse ----
#   library(tidyverse)
#
#
#   teamsDataInput <- load_teams(current = TRUE) |>
#     select(team_abbr, team_name, team_conf, team_division) |>
#     arrange(team_division, team_name) |>
#     as.data.frame()
#
#   ui <- fluidPage(
#     h2("Offensive Player Data"),
#     tags$style(HTML(".vscomp-dropbox-container  {z-index:99999 !important;}")),
#     #### Inputs ----
#     fluidRow(
#       ##### Season ----
#       column(width = 3,
#              noUiSliderInput(
#                inputId = "playerOffenseSeason",
#                label = "Select seasons",
#                min = 2003,
#                max = get_current_season(),
#                step = 1,
#                value = c(get_current_season(),get_current_season())
#              )
#       ),
#       ##### Game Type ----
#       column(width = 2,
#              prettyCheckboxGroup(
#                inputId = "playerOffenseGameType",
#                label = "Game Type",
#                choices = c("Regular Season" = "REG",
#                            "Playoffs" = "POST"),
#                selected = "REG",
#                inline = FALSE,
#                status = "info",
#                fill = TRUE
#              )
#       ),
#       ##### Team ----
#       column(width = 3,
#              virtualSelectInput(
#                inputId = "playerOffenseTeam",
#                label = "Select team to analyze",
#                choices = prepare_choices(
#                  .data = teamsDataInput,
#                  label = team_name,
#                  value = team_abbr,
#                  group_by = team_division
#                ),
#                multiple = TRUE,
#                selected = teamsDataInput$team_abbr,
#                showSelectedOptionsFirst = TRUE
#              )
#       ),
#       ##### Table Stat ----
#       column(width = 2,
#              radioGroupButtons(
#                inputId = "playerOffenseStat",
#                label = "Table Statistic",
#                choices = c("Total", "Game"),
#                status = "info"
#              )
#       ) # end column
#     ), # end fluidRow
#     tabBox(
#       type = "pills",
#       width = 12,
#       tabPanel(
#         title = "Overview"
#       ),
#       tabPanel(
#         title = "Passing",
#         playerOffensePassingTableOutput("playerOffensePassingTable")
#       ),
#       tabPanel(
#         title = "Rushing"
#       ),
#       tabPanel(
#         title = "Receiving"
#       )
#     )
#   )
#
#
#   # Server functions -----
#   allSeasons <- 2002:most_recent_season()
#
#   ## Team Data ----
#   teamsData <- load_teams(current = FALSE)
#
#   # my_con <- dbConnect(RPostgres::Postgres(),
#   #                     dbname = "NFLdata",
#   #                     user = "postgre",
#   #                     password = "NFLpass1234",
#   #                     host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
#   #
#   # playerOffenseData <- tbl(my_con, "playerOffenseData")
#   playerOffenseData <- load_player_stats(seasons = allSeasons, "offense")
#
#   # Server ----
#   server <- function(input, output, session) {
#
#     playerOffenseSeason <- reactive({
#       input$playerOffenseSeason
#     })
#     playerOffenseGameType <- reactive({
#       input$playerOffenseGameType
#     })
#     playerOffenseTeam <- reactive({
#       input$playerOffenseTeam
#     })
#     playerOffenseStat <- reactive({
#       input$playerOffenseStat
#     })
#     #### Scrimmage ----
#     #### Passing ----
#     playerOffensePassingTableServer("playerOffensePassingTable",
#                                     playerOffenseData,
#                                     playerOffenseSeason,
#                                     playerOffenseGameType,
#                                     playerOffenseTeam,
#                                     playerOffenseStat,
#                                     teamsData)
#   }
#   shinyApp(ui, server)
# }
#
# playerOffensePassingTableApp()




# inputSeason <- seq(2023, 2024)
# inputGameType <- "REG"
# inputTeams <- load_teams(current = TRUE) |> pull(team_abbr)
# inputStat <- "Total"
# p2<-load_player_stats(2023:2024, "offense")
# t2 <- playerOffenseData |>
#   filter(season %in% inputSeason) |>
#   filter(season_type %in% inputGameType) |>
#   filter(recent_team %in% inputTeams) |>
#   rename(team_abbr = recent_team) |>
#   filter(attempts > 0) |>
#   select(
#     player_display_name,
#     team_abbr,
#     position,
#     completions,
#     attempts,
#     passing_yards,
#     passing_tds,
#     passing_first_downs,
#     interceptions,
#     sacks,
#     sack_yards,
#     sack_fumbles,
#     sack_fumbles_lost
#   ) |>
#   collect() |>
#   group_by(
#     player_display_name, team_abbr, position
#   ) %>%
#   # mutate(
#   #   a = (sum(completions)/sum(attempts) - 0.3)*5,
#   #   a2 = ifelse(a < 0, 0,
#   #               ifelse(a > 2.375, 2.375, a)),
#   #   b = (sum(passing_yards)/sum(attempts) - 3)*0.25,
#   #   b2 = ifelse(b < 0, 0,
#   #               ifelse(b > 2.375, 2.375, b)),
#   #   c = (sum(passing_tds)/sum(attempts))*20,
#   #   c2 = ifelse(c < 0, 0,
#   #               ifelse(c > 2.375, 2.375, c)),
#   #   d = 2.375 - (sum(interceptions)/sum(attempts))*25,
#   #   d2 = ifelse(d < 0, 0,
#   #               ifelse(d > 2.375, 2.375, d)),
#   #   passer_rating = ((a2+b2+c2+d2)/6)*100
#   # ) |>
#   mutate(
#     a = (sum(completions)/sum(attempts) - 0.3)*5,
#     a2 = ifelse(a < 0, 0,
#                 ifelse(a > 2.375, 2.375, a)),
#     b = (sum(passing_yards)/sum(attempts) - 3)*0.25,
#     b2 = ifelse(b < 0, 0,
#                 ifelse(b > 2.375, 2.375, b)),
#     c = (sum(passing_tds)/sum(attempts))*20,
#     c2 = ifelse(c < 0, 0,
#                 ifelse(c > 2.375, 2.375, c)),
#     d = 2.375 - (sum(interceptions)/sum(attempts))*25,
#     d2 = ifelse(d < 0, 0,
#                 ifelse(d > 2.375, 2.375, d)),
#     passer_rating = ((a2+b2+c2+d2)/6)*100
#   ) |>
#   select(
#     -c(a,a2,b,b2,c,c2,d,d2)
#   ) %>%
#   {if(inputStat == "Total"){
#     mutate(.,
#               across(-c(passing_yards, passer_rating), ~round(sum(.x, na.rm = TRUE),2)),
#               passing_yards = sum(passing_yards, na.rm = TRUE),
#               games_played = n(),
#               passing_yards_game = round(passing_yards/n(), 2),
#               passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
#     )
#   }else{
#     mutate(.,
#               across(-c(passing_yards, passer_rating), ~round(mean(.x, na.rm = TRUE),2)),
#               passing_yards = sum(passing_yards, na.rm = TRUE),
#               games_played = n(),
#               passing_yards_game = round(passing_yards/n(), 2),
#               passer_rating = round(mean(passer_rating, na.rm = TRUE), 2)
#     )
#   }
#   } |>
#   ungroup() |>
#   mutate(
#     completion_percentage = round(completions/attempts, 4)
#   ) |>
#   distinct() |>
#   select(
#     player_display_name,
#     team_abbr,
#     position,
#     games_played,
#     completions,
#     attempts,
#     completion_percentage,
#     passing_yards,
#     passing_yards_game,
#     everything()
#   ) |>
#   arrange(desc(passing_yards))
