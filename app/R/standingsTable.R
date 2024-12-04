## Create Standings Tables ----

# UI ----
standingsTableOutput <- function(id){
  withSpinner(uiOutput(NS(id, "standingsTableUI")), type = 8)
}


# Server ----
standingsTableServer <- function(id,
                                 standingsSeason,
                                 teamsData,
                                 standingsTableData,
                                 conference){
  moduleServer(id, function(input, output, session){
    
    output$standingsTableUI <- renderUI({
      conf_logo <- teamsData |> 
        filter(team_conf == conference) |>
        pull(team_conference_logo) |>
        unique()
      
      box(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo, style = "height: 25px;"),
                    strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
                    strong("Standings", style = "margin-left: 4px; font-size: 25px")
        ),
        width = 12,
        status = "primary",
        maximizable = TRUE,
        #withSpinner(
          uiOutput(NS(id, "tablePlaceholder"))#, type = 8
        #)
      )
    })
    
    output$tablePlaceholder <- renderUI({
      withSpinner(
        gt_output(NS(id, "standingsTable")), type = 8
      )
    })
    
    output$standingsTable <- render_gt({
      standingsTableData() |>
        filter(team_conf == conference) |>
        select(
          "team",
          "team_name",
          "team_division",
          "div_rank",
          "GP",
          "W",
          "L",
          "T",
          "W-L%",
          "PF",
          "PA",
          "PD",
          "MOV",
          "SOS",
          "SRS",
          "OSRS",
          "DSRS"
        ) |>
        group_by(team_division) |>
        arrange(team_division, div_rank) |>
        gt() |>
        cols_hide(
          columns = "div_rank"
        ) |>
        gt_nfl_logos(
          columns = "team",
          height = "25px"
        ) |>
        fmt_percent(
          columns = "W-L%",
          decimals = 1
        ) |>
        fmt_number(
          columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
          decimals = 2
        ) |>
        data_color(
          columns = c("SRS"),
          method = "numeric",
          palette = c("red", "green")
        ) |>
        # tab_header(
        #   title = div(style = "display: flex; align-items: center;",
        #               img(src = conf_logo(), style = "height: 25px;"),
        #               strong(standingsSeason, style = "margin-left: 6px"),
        #               strong("Standings", style = "margin-left: 4px")
        #   )
        # ) |>
        tab_options(
          data_row.padding = 0,
          column_labels.font.weight = "bold",
          #heading.title.font.size = "150%",
          table.font.size = "90%"
        ) |>
        tab_style(
          style = cell_borders(sides = "right"),
          locations = cells_body(
            columns = c("team_name", "W-L%", "PD")
          )
        ) |>
        tab_style(
          style = cell_borders(sides = "right", weight = "0.5px"),
          locations = cells_body(
            columns = c("GP")
          )
        ) |>
        cols_label(
          team = "",
          team_name = "Team"
        )
    }) # end renderGT
  }) # end module Server
} # end standingsTableServer


# # App Test ----
# standingsTableApp <- function() {
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
#     fluidRow(
#       ##### Inputs ----
#       ###### Season ----
#       column(width = 1,
#              virtualSelectInput(
#                inputId = "standingsSeason",
#                label = "Select season",
#                choices = seq(2006, get_current_season()),
#                selected = get_current_season()
#              )
#       ),
#       ###### Table Stat ----
#       column(width = 2,
#              radioGroupButtons(
#                inputId = "standingsStat",
#                label = "Table Statistic",
#                choices = c("Total", "Game"),
#                status = "info"
#              )
#       ) # end column
#     ), # end fluidRow
#     ##### Season Table ----
#     fluidRow(
#       column(
#         width = 6,
#         standingsTableOutput("standingsTableAFC")
#       ), # end AFC column
#       column(
#         width = 6,
#         standingsTableOutput("standingsTableNFC")
#       ) # end NFC column
#     ), # end divsion standings row
#     ##### Playoffs Table ----
#     fluidRow(
#       column(
#         width = 6,
#         standingsPlayoffsTableOutput("standingsPlayoffsTableAFC")
#       ), # end AFC column
#       column(
#         width = 6,
#         standingsPlayoffsTableOutput("standingsPlayoffsTableNFC")
#       ) # end NFC column
#     ) # end playoff standings row
#   )
# 
# 
#   # Server functions -----
#   ## Amazon RDS connection ----
#   plan("multisession")
#   
#   con <- dbConnect(RPostgres::Postgres(),
#                    dbname = "NFLdata",
#                    user = "postgre",
#                    password = "NFLpass1234",
#                    host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
#   
#   allSeasons <- 2006:most_recent_season()
#   
#   ## Team Data ----
#   teamsData <- load_teams(current = FALSE)
#   
#   ## Game Data ----
#   source(file = "./app/data-raw/gameData.R")
#   
#   source(file = "./app/data-raw/gameDataLong.R")
#   
#   ## Standings ----
#   seasonStandings <- tbl(con, "seasonStandings")
# 
#   # Server ----
#   server <- function(input, output, session) {
#     ### Table Data ----
#     standingsSeason <- reactive({
#       as.numeric(input$standingsSeason)
#     })
#     standingsStat <- reactive({
#       input$standingsStat
#     })
#     standingsTableData <- reactive({
#       Season <- standingsSeason()
#       Stat <- standingsStat()
#       seasonStandings |>
#         filter(season == Season) |>
#         mutate(
#           PF = ifelse(Stat == "Total", PF, round(PF/GP, 2)),
#           PA = ifelse(Stat == "Total", PA, round(PA/GP, 2)),
#           PD = ifelse(Stat == "Total", PD, round(PD/GP, 2)),
#         ) |> 
#         collect()
#     })
#     
#     ### AFC Table ----
#     standingsTableServer("standingsTableAFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "AFC")
#     
#     ### NFC Table ----
#     standingsTableServer("standingsTableNFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "NFC")
#     
#     ### AFC Playoffs Table ----
#     standingsPlayoffsTableServer("standingsPlayoffsTableAFC",
#                                  standingsSeason,
#                                  teamsData,
#                                  standingsTableData,
#                                  conference = "AFC")
#     
#     ### NFC Playoffs Table ----
#     standingsPlayoffsTableServer("standingsPlayoffsTableNFC",
#                                  standingsSeason,
#                                  teamsData,
#                                  standingsTableData,
#                                  conference = "NFC")
#   }
#   shinyApp(ui, server)
# }
# 
# standingsTableApp()
