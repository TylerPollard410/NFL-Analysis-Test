## Create Standings Tables ----

# UI ----
standingsTableOutput <- function(id){
  tagList(
    #withSpinner(uiOutput(NS(id, "standingsTableUI")), type = 8)
    uiOutput(NS(id, "standingsTableTitle")),
    withSpinner(
      reactableOutput(NS(id, "standingsTable")), type = 8
    )
  )
}


# Server ----
standingsTableServer <- function(id,
                                 standingsSeason,
                                 standingsStat,
                                 teamsData,
                                 standingsTableData,
                                 conference){
  moduleServer(id, function(input, output, session){
    
    # output$standingsTableUI <- renderUI({
    #   conf_logo <- teamsData |>
    #     filter(team_conf == conference) |>
    #     pull(team_conference_logo) |>
    #     unique()
    # 
    #   box(
    #     title = div(style = "display: flex; align-items: center;",
    #                 img(src = conf_logo, style = "height: 25px;"),
    #                 strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
    #                 strong("Standings", style = "margin-left: 4px; font-size: 25px")
    #     ),
    #     width = 12,
    #     status = "primary",
    #     maximizable = TRUE,
    #     #withSpinner(
    #     uiOutput(NS(id, "tablePlaceholder"))#, type = 8
    #     #)
    #   )
    # })
    # 
    # output$tablePlaceholder <- renderUI({
    #   withSpinner(
    #     reactableOutput(NS(id, "standingsTable")), type = 8
    #   )
    # })
    
    output$standingsTableTitle <- renderUI({
      conf_logo <- teamsData |>
        filter(team_conf == conference) |>
        pull(team_conference_logo) |>
        unique()
      
      title <- div(style = "display: flex; align-items: center;",
                   img(src = conf_logo, style = "height: 25px;"),
                   strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
                   strong("Standings", style = "margin-left: 4px; font-size: 25px")
      )
      title
    })
    
    output$standingsTable <- renderReactable({
      conf_logo <- teamsData |>
        filter(team_conf == conference) |>
        pull(team_conference_logo) |>
        unique()
      Stat <- standingsStat()
      Season <- standingsSeason()
      
      standingsTableDataReact <- standingsTableData() |>
        filter(team_conf == conference) |>
        select(
          "team_division",
          "team_logo_espn",
          "team_name",
          "div_rank",
          "GP",
          "W",
          "L",
          "T",
          "W-L%",
          "PF",
          "team_PPG",
          "PA",
          "opp_PPG",
          "PD",
          "MOV",
          "SOS",
          "SRS",
          "OSRS",
          "DSRS"
        ) |>
        collect() |>
        group_by(team_division) |>
        arrange(team_division, div_rank) |>
        ungroup() |>
        select(-div_rank)
      
      ## Reactable ----
      ## Total ----
      if(Stat == "Total"){
        standingsTableReact <- reactable(
          data = standingsTableDataReact |> select(-c(team_PPG, opp_PPG)),
          theme = espn(
            centered = TRUE, 
            header_font_size = 14,
            font_size = 14
          ),
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          wrap = FALSE,
          outlined = TRUE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          defaultSorted = "team_division",
          rowStyle = group_border_sort(columns = "team_division", 
                                       border_color = "black", 
                                       border_width = "1.5px",
                                       border_style = "solid"),
          defaultColDef = colDef(vAlign = "center",
                                 minWidth = 50
                                 #headerStyle = list(fontSize = "14px")
          ),
          columnGroups = list(
            colGroup(name = "Record",
                     columns = c("GP", "W", "L", "T", "W-L%")),
            colGroup(name = "Points",
                     columns = c("PF", "PA", "PD")),
            colGroup(name = "Performance",
                     columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
          ),
          columns = list(
            ### Team Division ----
            team_division = colDef(
              name = "",
              minWidth = 80, 
              style = group_merge_sort("team_division"), 
              #style = cell_style(font_color = "red")
            ),
            ### Team Logo ----
            team_logo_espn = colDef(
              name = "",
              sticky = "left",
              maxWidth = 25,
              style = background_img()
            ),
            ### Team Name ----
            team_name = colDef(
              name = "Team",
              minWidth = 175,
              style = list(borderRight = "1px solid black")
            ),
            ### Games Played ----
            GP = colDef(
              name = "GP",
              minWidth = 30,
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### Win ----
            W = colDef(
              minWidth = 30
            ),
            ### Loss ----
            L = colDef(
              minWidth = 30
            ),
            ### Tie ----
            `T` = colDef(
              minWidth = 30
            ),
            ### Win Loss Perc ----
            `W-L%` = colDef(
              minWidth = 75,
              format = colFormat(percent = TRUE, digits = 2),
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### PF ----
            ### PA ----
            ### PD ----
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### MOV ----
            MOV = colDef(
              format = colFormat(digits = 2)
            ),
            ### SOS ----
            SOS = colDef(
              format = colFormat(digits = 2)
            ),
            ### SRS ----
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = standingsTableData(),
                colors = c("red","pink", "whitesmoke", "palegreen", "green"),
                bias = 1,
                brighten_text = FALSE
              )
            ),
            ### OSRS ----
            OSRS = colDef(
              format = colFormat(digits = 2)
            ),
            ### DSRS ----
            DSRS = colDef(
              format = colFormat(digits = 2)
            )
          )
        )
      }else{
        ## Game ----
        standingsTableReact <- reactable(
          data = standingsTableDataReact |> select(-c(PF, PA)),
          theme = espn(
            centered = TRUE, 
            header_font_size = 14,
            font_size = 14
          ),
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          wrap = FALSE,
          outlined = TRUE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          defaultSorted = "team_division",
          rowStyle = group_border_sort(columns = "team_division", 
                                       border_color = "black", 
                                       border_width = "1.5px",
                                       border_style = "solid"),
          defaultColDef = colDef(vAlign = "center",
                                 minWidth = 50
                                 #headerStyle = list(fontSize = "14px")
          ),
          columnGroups = list(
            colGroup(name = "Record",
                     columns = c("GP", "W", "L", "T", "W-L%")),
            colGroup(name = "Points",
                     columns = c("team_PPG", "opp_PPG", "PD")),
            colGroup(name = "Performance",
                     columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
          ),
          columns = list(
            ### Team Division ----
            team_division = colDef(
              name = "",
              minWidth = 80, 
              style = group_merge_sort("team_division"), 
              #style = cell_style(font_color = "red")
            ),
            ### Team Logo ----
            team_logo_espn = colDef(
              name = "",
              sticky = "left",
              maxWidth = 25,
              style = background_img()
            ),
            ### Team Name ----
            team_name = colDef(
              name = "Team",
              minWidth = 175,
              style = list(borderRight = "1px solid black")
            ),
            ### Games Played ----
            GP = colDef(
              name = "GP",
              minWidth = 30,
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### Win ----
            W = colDef(
              minWidth = 30
            ),
            ### Loss ----
            L = colDef(
              minWidth = 30
            ),
            ### Tie ----
            `T` = colDef(
              minWidth = 30
            ),
            ### Win Loss Perc ----
            `W-L%` = colDef(
              minWidth = 75,
              format = colFormat(percent = TRUE, digits = 2),
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### PF ----
            team_PPG = colDef(
              name = "PF",
              format = colFormat(digits = 2)
            ),
            ### PA ----
            opp_PPG = colDef(
              name = "PA",
              format = colFormat(digits = 2)
            ), 
            ### PD ----
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### MOV ----
            MOV = colDef(
              format = colFormat(digits = 2)
            ),
            ### SOS ----
            SOS = colDef(
              format = colFormat(digits = 2)
            ),
            ### SRS ----
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = standingsTableData(),
                colors = c("red","pink", "whitesmoke", "palegreen", "green"),
                bias = 1
              )
            ),
            ### OSRS ----
            OSRS = colDef(
              format = colFormat(digits = 2)
            ),
            ### DSRS ----
            DSRS = colDef(
              format = colFormat(digits = 2)
            )
          )
        )
      }
      
      # ## Title ----
      # standingsTableReact <- standingsTableReact |>
      #   add_title(
      #     title = div(style = "display: flex; align-items: center;",
      #                 img(src = conf_logo, style = "height: 25px;"),
      #                 strong(Season, style = "margin-left: 6px; font-size: 25px"),
      #                 strong("Standings", style = "margin-left: 4px; font-size: 25px")
      #     )
      #     
      #   )
      
      return(standingsTableReact)
    })
  }) # end module Server
} # end standingsTableServer

# Test table output ----
# plan("multisession")
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "NFLdata",
#                  user = "postgre",
#                  password = "NFLpass1234",
#                  host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
# 
# 
# allSeasons <- 2006:most_recent_season()
# teamsData <- load_teams(current = FALSE)
# source(file = "./app/data-raw/gameData.R")
# source(file = "./app/data-raw/gameDataLong.R")
# seasonStandings <- tbl(con, "seasonStandings")
# 
# 
# Season <- 2024
# Stat <- c("Total", "Game")[2]
# conference <- "AFC"
# conf_logo <- teamsData |> 
#   filter(team_conf == conference) |>
#   pull(team_conference_logo) |>
#   unique()
# 
# 
# standingsTableData <- seasonStandings |>
#   filter(season == Season) |>
#   collect()
# 
# standingsTableDataReact <- standingsTableData |>
#   filter(team_conf == conference) |>
#   select(
#     "team_division",
#     "team_logo_espn",
#     "team_name",
#     "div_rank",
#     "GP",
#     "W",
#     "L",
#     "T",
#     "W-L%",
#     "PF",
#     "team_PPG",
#     "PA",
#     "opp_PPG",
#     "PD",
#     "MOV",
#     "SOS",
#     "SRS",
#     "OSRS",
#     "DSRS"
#   ) |>
#   collect() |>
#   group_by(team_division) |>
#   arrange(team_division, div_rank) |>
#   ungroup() |>
#   select(-div_rank)
# 
# if(Stat == "Total"){
#   standingsTableDataReact <- standingsTableDataReact |>
#     select(-c(team_PPG, opp_PPG))
# }else{
#   standingsTableDataReact <- standingsTableDataReact |>
#     select(-c(PF, PA))
# }
#   
# 
# standingsTableReact <- reactable(
#   data = standingsTableDataReact,
#   theme = espn(centered = TRUE),
#   highlight = TRUE,
#   compact = TRUE,
#   pagination = FALSE,
#   wrap = FALSE,
#   outlined = TRUE,
#   sortable = FALSE,
#   showSortable = FALSE,
#   #defaultSorted = "team_division",
#   rowStyle = group_border_sort("team_division"),
#   defaultColDef = colDef(vAlign = "center",
#                          minWidth = 30
#                          #headerStyle = list(fontSize = "14px")
#   ),
#   columns = list(
#     ### Team Division
#     team_division = colDef(
#       name = "Div",
#       minWidth = 100,
#       style = group_merge_sort("team_division")
#     ),
#     ### Team Logo
#     team_logo_espn = colDef(
#       name = "",
#       maxWidth = 25,
#       style = background_img()
#       # cell = function(value, index){
#       #   #player_name <- standingsTableDataReact$player_display_name[index]
#       #   logo <- img(src = value, style = "height: 20px;")
#       #   #team <- standingsTableDataReact$team_abbr[index]
#       #   div(style = "display: flex; align-items: center;",
#       #       logo,
#       #       span(player_name, style = "margin-left: 4px"),
#       #       span(",", style = "margin-right: 4px"),
#       #       span(team, style = "font-size: 10px; color: grey")
#       #   )
#       # },
#       # style = list(borderRight = "1px solid black")
#     ),
#     ### Team Name
#     team_name = colDef(
#       name = "Team",
#       minWidth = 125,
#       style = list(borderRight = "1px solid black")
#     ),
#     ### Games Played
#     GP = colDef(
#       name = "GP",
#       #maxWidth = 50,
#       #align = "center",
#       style = list(borderRight = "1px solid black")
#     ),
#     ### Win Loss
#     `W-L%` = colDef(
#       minWidth = 50,
#       format = colFormat(percent = TRUE, digits = 2)
#     ),
#     ### PF
#     team_PPG = colDef(
#       name = "PF",
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     ### PA
#     opp_PPG = colDef(
#       name = "PA",
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     ### PD
#     PD = colDef(
#       minWidth = 40
#     ),
#     MOV = colDef(
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     SOS = colDef(
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     SRS = colDef(
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     OSRS = colDef(
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     ),
#     DSRS = colDef(
#       minWidth = 40,
#       format = colFormat(digits = 2)
#     )
#   )
# )
# standingsTableReact
# 
# 
# 
# 
# 
# 
# 
# 
# 

# # App Test ----
# standingsTableApp <- function() {
#   
#   # Load Libraries
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
#   library(future)
#   
#   ## Data Manipulation
#   library(stringr)
#   library(rvest)
#   
#   ## Tables
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
#   ## Plotting
#   library(smplot2)
#   # library(cowplot)
#   # library(GGally)
#   library(patchwork)
#   
#   ## Modeling
#   # library(pracma)
#   # library(forecast)
#   # library(elo)
#   # library(MASS)
#   # library(bestNormalize)
#   # library(tictoc)
#   # library(caret)
#   # library(splines)
#   # library(mgcv)
#   # library(DescTools)
#   # library(car)
#   # library(bayesplot)
#   # library(BayesFactor)
#   # library(rstanarm)
#   # library(tidybayes)
#   # library(loo)
#   # library(brms)
#   # library(performance)
#   
#   ## NFL Verse
#   library(nflverse)
#   
#   ## Tidyverse
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
#       ##### Inputs
#       ###### Season
#       column(width = 1,
#              virtualSelectInput(
#                inputId = "standingsSeason",
#                label = "Select season",
#                choices = seq(2006, get_current_season()),
#                selected = get_current_season()
#              )
#       ),
#       ###### Table Stat
#       column(width = 2,
#              radioGroupButtons(
#                inputId = "standingsStat",
#                label = "Table Statistic",
#                choices = c("Total", "Game"),
#                status = "info"
#              )
#       ) # end column
#     ), # end fluidRow
#     ##### Season Table
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
#     ##### Playoffs Table
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
#   # Server functions
#   ## Amazon RDS connection
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
#   ## Team Data
#   teamsData <- load_teams(current = FALSE)
#   
#   ## Game Data
#   source(file = "./app/data-raw/gameData.R")
#   
#   source(file = "./app/data-raw/gameDataLong.R")
#   
#   ## Standings
#   seasonStandings <- tbl(con, "seasonStandings")
#   
#   # Server ----
#   server <- function(input, output, session) {
#     ### Table Data
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
#           # PF = ifelse(Stat == "Total", PF, round(PF/GP, 2)),
#           # PA = ifelse(Stat == "Total", PA, round(PA/GP, 2)),
#           # PD = ifelse(Stat == "Total", PD, round(PD/GP, 2))
#           PF = ifelse(Stat == "Total", PF, round(team_PPG, 2)),
#           PA = ifelse(Stat == "Total", PA, round(opp_PPG, 2)),
#           PD = ifelse(Stat == "Total", PD, round(MOV, 2))
#         ) |> 
#         collect()
#     })
#     
#     ### AFC Table
#     standingsTableServer("standingsTableAFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "AFC")
#     
#     ### NFC Table
#     standingsTableServer("standingsTableNFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "NFC")
#     
#     ### AFC Playoffs Table
#     standingsPlayoffsTableServer("standingsPlayoffsTableAFC",
#                                  standingsSeason,
#                                  teamsData,
#                                  standingsTableData,
#                                  conference = "AFC")
#     
#     ### NFC Playoffs Table
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
# 
# 
# 
# 
# 


# GT ----
# 
## UI ----
# standingsTableOutput <- function(id){
#   withSpinner(uiOutput(NS(id, "standingsTableUI")), type = 8)
# }
# 
# 
# ## Server ----
# standingsTableServer <- function(id,
#                                  standingsSeason,
#                                  teamsData,
#                                  standingsTableData,
#                                  conference){
#   moduleServer(id, function(input, output, session){
#     
#     output$standingsTableUI <- renderUI({
#       conf_logo <- teamsData |> 
#         filter(team_conf == conference) |>
#         pull(team_conference_logo) |>
#         unique()
#       
#       box(
#         title = div(style = "display: flex; align-items: center;",
#                     img(src = conf_logo, style = "height: 25px;"),
#                     strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
#                     strong("Standings", style = "margin-left: 4px; font-size: 25px")
#         ),
#         width = 12,
#         status = "primary",
#         maximizable = TRUE,
#         #withSpinner(
#           uiOutput(NS(id, "tablePlaceholder"))#, type = 8
#         #)
#       )
#     })
#     
#     output$tablePlaceholder <- renderUI({
#       withSpinner(
#         gt_output(NS(id, "standingsTable")), type = 8
#       )
#     })
#     
#     output$standingsTable <- render_gt({
#       standingsTableData() |>
#         filter(team_conf == conference) |>
#         select(
#           "team",
#           "team_name",
#           "team_division",
#           "div_rank",
#           "GP",
#           "W",
#           "L",
#           "T",
#           "W-L%",
#           "PF",
#           "PA",
#           "PD",
#           "MOV",
#           "SOS",
#           "SRS",
#           "OSRS",
#           "DSRS"
#         ) |>
#         group_by(team_division) |>
#         arrange(team_division, div_rank) |>
#         gt() |>
#         cols_hide(
#           columns = "div_rank"
#         ) |>
#         gt_nfl_logos(
#           columns = "team",
#           height = "25px"
#         ) |>
#         fmt_percent(
#           columns = "W-L%",
#           decimals = 1
#         ) |>
#         fmt_number(
#           columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
#           decimals = 2
#         ) |>
#         data_color(
#           columns = c("SRS"),
#           method = "numeric",
#           palette = c("red", "green")
#         ) |>
#         # tab_header(
#         #   title = div(style = "display: flex; align-items: center;",
#         #               img(src = conf_logo(), style = "height: 25px;"),
#         #               strong(standingsSeason, style = "margin-left: 6px"),
#         #               strong("Standings", style = "margin-left: 4px")
#         #   )
#         # ) |>
#         tab_options(
#           data_row.padding = 0,
#           column_labels.font.weight = "bold",
#           #heading.title.font.size = "150%",
#           table.font.size = "90%"
#         ) |>
#         tab_style(
#           style = cell_borders(sides = "right"),
#           locations = cells_body(
#             columns = c("team_name", "W-L%", "PD")
#           )
#         ) |>
#         tab_style(
#           style = cell_borders(sides = "right", weight = "0.5px"),
#           locations = cells_body(
#             columns = c("GP")
#           )
#         ) |>
#         cols_label(
#           team = "",
#           team_name = "Team"
#         )
#     }) # end renderGT
#   }) # end module Server
# } # end standingsTableServer


# ## App Test ----
# standingsTableApp <- function() {
# 
#   # Load Libraries 
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
#   ## Tables 
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
#   ## Plotting 
#   library(smplot2)
#   # library(cowplot)
#   # library(GGally)
#   library(patchwork)
# 
#   ## Modeling 
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
#   ## NFL Verse 
#   library(nflverse)
# 
#   ## Tidyverse 
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
#       ##### Inputs 
#       ###### Season 
#       column(width = 1,
#              virtualSelectInput(
#                inputId = "standingsSeason",
#                label = "Select season",
#                choices = seq(2006, get_current_season()),
#                selected = get_current_season()
#              )
#       ),
#       ###### Table Stat 
#       column(width = 2,
#              radioGroupButtons(
#                inputId = "standingsStat",
#                label = "Table Statistic",
#                choices = c("Total", "Game"),
#                status = "info"
#              )
#       ) # end column
#     ), # end fluidRow
#     ##### Season Table 
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
#     ##### Playoffs Table 
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
#   # Server functions -
#   ## Amazon RDS connection 
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
#   ## Team Data 
#   teamsData <- load_teams(current = FALSE)
#   
#   ## Game Data 
#   source(file = "./app/data-raw/gameData.R")
#   
#   source(file = "./app/data-raw/gameDataLong.R")
#   
#   ## Standings 
#   seasonStandings <- tbl(con, "seasonStandings")
# 
#   # Server 
#   server <- function(input, output, session) {
#     ### Table Data 
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
#     ### AFC Table 
#     standingsTableServer("standingsTableAFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "AFC")
#     
#     ### NFC Table 
#     standingsTableServer("standingsTableNFC",
#                          standingsSeason,
#                          teamsData,
#                          standingsTableData,
#                          conference = "NFC")
#     
#     ### AFC Playoffs Table 
#     standingsPlayoffsTableServer("standingsPlayoffsTableAFC",
#                                  standingsSeason,
#                                  teamsData,
#                                  standingsTableData,
#                                  conference = "AFC")
#     
#     ### NFC Playoffs Table 
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
