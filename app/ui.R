## NFL Analysis App
## Tyler Pollard
## 8 Aug 2023

# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bslib)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)
library(future)

## Data Manipulation
library(stringr)
library(rvest)

## Tables ----
library(DBI)
library(RPostgres)
library(data.table)
library(htmltools)
library(gt)
library(gtsummary)
library(gtExtras)
library(reactable)
library(reactablefmtr)

## Plotting ----
library(smplot2)
# library(cowplot)
# library(GGally)
library(patchwork)
library(glue)
library(scales)
library(plotly)

## Modeling ----
library(pracma)
library(forecast)
library(timetk)
library(elo)
library(MASS)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Input Filters ----
teamsDataPickerInput <- load_teams(current = TRUE) |>
  select(team_abbr, team_name, team_conf, team_division) |>
  arrange(team_division, team_name) |>
  as.data.frame()


# Set theme ====
my_theme <- create_theme(
  theme = "paper",
  bs4dash_sidebar_dark(
    bg = "#2d3b4d"
  ),
  bs4dash_status(
    primary = "purple", info = "#eec900"
  )
)
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8};")
#"#6399b8"


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(dark = NULL, 
                footer = dashboardFooter(left = br()), 
                freshTheme = my_theme,
                preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
                
                # Dahsboard Header ===============
                header = dashboardHeader(
                  title = dashboardBrand(
                    title = div(style = "font-size:14pt",
                                align = "center", 
                                "NFL Analysis App",
                                dashboardBadge(
                                  color = "warning",
                                  rounded = TRUE,
                                  position = "right",
                                  "v1.0")
                    ),
                    color = "primary"
                  ),
                  compact = FALSE,
                  fixed = TRUE,
                  rightUi = tags$li(
                    class = "dropdown",
                    dropdownMenu(
                      badgeStatus = NULL,
                      type = "notifications",
                      headerText = "NFL Analysis",
                      icon = icon("info-circle"),
                      notificationItem(
                        inputId = "info1",
                        text = "Developer: Tyler Pollard",
                        icon = icon("users-cog"),
                        status = "info"
                      ),
                      notificationItem(
                        inputId = "info2",
                        text = "Release Date: 20 Oct 2024",
                        icon = icon("calendar"),
                        status = "info"
                      ),
                      notificationItem(
                        inputId = "info3",
                        text = "Version: 1.0",
                        icon = icon("code"),
                        status = "info"
                      )
                    )
                  ),
                  ## Navbar Menu ------------------
                  navbarMenu(
                    id = "navMenu",
                    ### Home Tab ----
                    navbarTab(tabName = "homeTab", text = "Home")
                  ) # end navbarMenu
                ), # close header
                scrollToTop = TRUE,
                # Dashboard Sidebar =============
                sidebar = dashboardSidebar(
                  id = "sidebar",
                  skin = "dark",
                  elevation = 5,
                  fixed = TRUE,
                  minified = FALSE,
                  status = "primary",
                  compact = TRUE,
                  collapsed = TRUE,
                  width = "150px",
                  ## Sidebar Menu ---------------
                  sidebarMenu(
                    id = "menu_items",
                    ### Data Tab ----
                    h4("Data", style = "color: white"),
                    menuItem(text = "Standings", tabName = "standingsTab", icon = icon("list-ol")),
                    menuItem(text = "Scores", tabName = "scoresTab", icon = icon("football-ball")),
                    menuItem(text = "Team Statistics", icon = icon("users"),
                             menuSubItem(text = "Offense", tabName = "teamOffenseTab"),
                             menuSubItem(text = "Defense", tabName = "teamDefenseTab"),
                             menuSubItem(text = "Special Teams", tabName = "teamSpecialTeamsTab"),
                             menuSubItem(text = "Scoring", tabName = "teamScoringTab")
                    ), 
                    menuItem(text = "Player Statistics", icon = icon("user"),
                             menuSubItem(text = "Offense", tabName = "playerOffenseTab"),
                             menuSubItem(text = "Defense", tabName = "playerDefenseTab"),
                             menuSubItem(text = "Special Teams", tabName = "playerSpecialTeamsTab"),
                             menuSubItem(text = "Scoring", tabName = "playerScoringTab"),
                             menuSubItem(text = "Fantasy", tabName = "playerFantasyTab")
                    ), 
                    h4("Betting", style = "color: white"),
                    menuItem(text = "Games", tabName = "bettingGamesTab", icon = icon("dollar-sign")),
                    menuItem(text = "Player Props", tabName = "bettingPlayerPropsTab", icon = icon("user-clock")),
                    menuItem(text = "Plot", tabName = "bettingPlotTab", icon = icon("chart-line"))
                  ) # close sidebar menu
                ), # close dashboard sidebar
                # Dashboard Controlbar ==================
                controlbar = dashboardControlbar(),
                # Dashboard Body ================
                body = dashboardBody(
                  
                  useShinyjs(),
                  tabItems(
                    # Home Tab  ###############################################
                    tabItem(
                      tabName = "homeTab",
                      
                      # Welcome Jumbotron ----
                      bs4Jumbotron(
                        title = h1("Welcome to the NFL Analysis Dashboard"),
                        lead = includeMarkdown("./docs/Purpose.Rmd"),
                        #lead = "This dashboard explores historical and current NFL data using powerful visualizations and modeling tools.",
                        status = "primary",
                        btnName = NULL,
                        btnHref = NULL,
                        div(
                          style = "display: flex; justify-content: center; align-items: center; margin-top: 20px;",
                          tags$img(
                            src = "nfl_logo.jpeg",
                            style = "max-width: 90%; height: auto; max-height: 400px; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0,0,0,0.3);"
                          )
                        )
                        #img = "www/nfl_logo.jpeg",
                        #height = "400px",
                        #includeMarkdown("./docs/Purpose.Rmd")
                      ),
                      #h1("Welcome to the NFL Game Dashboard", align  = "center"),
                      #br(),
                      box(width = 12, closable = FALSE, collapsible = FALSE, #headerBorder = FALSE,
                          title = h2("App Overview", align = "center"),
                          # fluidRow(column(width = 12, align = "center",
                          #                 imageOutput("image"))
                          # ),
                          br(),
                          withMathJax(),
                          includeMarkdown("./docs/Description.Rmd")
                      ) # end box
                    ), # close Home tab Item 
                    # Data Tab ################################################
                    ## Standings Tab ##########################################
                    tabItem(
                      tabName = "standingsTab",
                      #fluidPage(
                      fluidRow(
                        ##### Inputs ----
                        ###### Season ----
                        column(width = 1,
                               virtualSelectInput(
                                 inputId = "standingsSeason",
                                 label = "Select season",
                                 choices = seq(2006, get_current_season()),
                                 selected = get_current_season()
                               )
                        ),
                        ###### Table Stat ----
                        column(width = 2,
                               radioGroupButtons(
                                 inputId = "standingsStat",
                                 label = "Table Statistic",
                                 choices = c("Total", "Game"),
                                 status = "info"
                               )
                        ) # end column
                      ), # end fluidRow
                      ##### Season Table ----
                      fluidRow(
                        column(
                          width = 6,
                          standingsTableOutput("standingsTableAFC")
                        ), # end AFC column
                        column(
                          width = 6,
                          standingsTableOutput("standingsTableNFC")
                        ) # end NFC column
                      ), # end divsion standings row
                      ##### Playoffs Table ----
                      fluidRow(
                        column(
                          width = 6,
                          standingsPlayoffsTableOutput("standingsPlayoffsTableAFC")
                        ), # end AFC column
                        column(
                          width = 6,
                          standingsPlayoffsTableOutput("standingsPlayoffsTableNFC")
                        ) # end NFC column
                      ) # end playoff standings row
                      #) # end fluidPage
                    ), # end Standings tabItem
                    ## Scores Tab #############################################
                    ## Team Tab ###############################################
                    ### Team Offense ==========================================
                    #### Overview ----
                    #### Passing ----
                    #### Rushing ----
                    #### Conversions ----
                    #### Drive Averages ----
                    ### Team Defense ==========================================
                    #### Overview ----
                    #### Passing ----
                    #### Rushing ----
                    #### Conversions ----
                    #### Drive Averages ----
                    #### Against Position ----
                    ### Team Special Teams ====================================
                    #### Kick/Punt Returns ----
                    #### Kicking ----
                    #### Punting ----
                    ### Team Scoring ==========================================
                    #### Scoring For ----
                    #### Scoring Against ----
                    ## Player Tab  ############################################
                    ### Player Offense ========================================
                    tabItem(
                      tabName = "playerOffenseTab",
                      h2("Offensive Player Data"),
                      tags$style(HTML(".vscomp-dropbox-container  {z-index:99999 !important;}")),
                      
                      #### Inputs ----
                      fluidRow(
                        ##### Season ----
                        column(width = 3,
                               noUiSliderInput(
                                 inputId = "playerOffenseSeason",
                                 label = "Select seasons",
                                 min = 2006,
                                 max = get_current_season(),
                                 step = 1,
                                 value = c(get_current_season(),get_current_season()),
                                 limit = 5,
                                 behaviour = "drag",
                                 format = wNumbFormat(decimals = 0)
                               )
                               # sliderInput(
                               #   inputId = "playerOffenseSeason",
                               #   label = "Select seasons",
                               #   min = 2003,
                               #   max = get_current_season(),
                               #   step = 1,
                               #   value = c(get_current_season(),get_current_season())
                               # ),
                        ),
                        
                        ##### Game Type ----
                        column(width = 2,
                               prettyCheckboxGroup(
                                 inputId = "playerOffenseGameType",
                                 label = "Game Type", 
                                 choices = c("Regular Season" = "REG",
                                             "Playoffs" = "POST"),
                                 selected = "REG",
                                 inline = FALSE, 
                                 status = "info",
                                 fill = TRUE
                               )
                        ),
                        ##### Team ----
                        column(width = 3,
                               virtualSelectInput(
                                 inputId = "playerOffenseTeam",
                                 label = "Select team to analyze", 
                                 choices = prepare_choices(
                                   .data = teamsDataPickerInput,
                                   label = team_name,
                                   value = team_abbr,
                                   group_by = team_division
                                 ),
                                 multiple = TRUE,
                                 selected = teamsDataPickerInput$team_abbr,
                                 showSelectedOptionsFirst = TRUE
                               )
                        ),
                        ##### Table Stat ----
                        column(width = 2,
                               radioGroupButtons(
                                 inputId = "playerOffenseStat",
                                 label = "Table Statistic",
                                 choices = c("Total", "Game"),
                                 status = "info"
                               )
                        ) # end column
                      ), # end fluidRow
                      tabBox(
                        type = "pills",
                        width = 12,
                        #### Overview ----
                        tabPanel(
                          title = "Overview"
                        ),
                        #### Passing ----
                        tabPanel(
                          title = "Passing",
                          playerOffensePassingTableOutput("playerOffensePassingTable")
                        ),
                        #### Rushing ----
                        tabPanel(
                          title = "Rushing"
                        ),
                        #### Receiving ----
                        tabPanel(
                          title = "Receiving"
                        )
                      )
                    ), # end Player Offense tabItem
                    ### Player Defense ========================================
                    #### Overview ----
                    ### Player Special Teams ==================================
                    #### Kick/Punt Returns ----
                    #### Kicking ----
                    #### Punting ----
                    ### Player Scoring ========================================
                    #### Overview ----
                    ### Player Fantasy ========================================
                    #### Ranks ----
                    # Betting Tab  ############################################
                    ## Games Tab ==============================================
                    tabItem(
                      tabName = "bettingGamesTab",
                      
                      tabsetPanel(
                        ### Lines ----
                        tabPanel(
                          title = "Lines",
                          br(),
                          h3("Betting Game Lines"),
                          fluidRow(
                            column(2,
                                   selectInput(
                                     inputId = "bettingSeason",
                                     label = "Season",
                                     choices = 2006:get_current_season(),
                                     selected = get_current_season()
                                   )
                            ),
                            column(2,
                                   selectInput(
                                     inputId = "bettingWeek",
                                     label = "Week",
                                     choices = 1:get_current_week(),
                                     selected = get_current_week()
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              width = 12,
                              withSpinner(
                                bettingGamesLinesUI("bettingGamesLines"),
                                type = 8
                              ),
                              align = "center"
                            )
                          )
                        ), # end Lines tabPanel
                        
                        ### Predictions ----
                        tabPanel(
                          title = "Predictions",
                          br(),
                          h3("Betting Predictions"),
                          br(),
                          #### Inputs ----
                          fluidRow(
                            ##### Season ----
                            column(
                              width = 1,
                              virtualSelectInput(
                                inputId = "bettingGamesPredSeason",
                                label = "Select season",
                                choices = seq(2007, get_current_season()),
                                selected = get_current_season()
                              )
                            ),
                            ##### Week ----
                            column(
                              width = 1,
                              uiOutput(outputId = "bettingGamesPredWeekUI")
                            )
                          ), # end fluidRow
                        ) # end Prediction tabPanel
                      ) # end tabsetPanel
                    ), #end bettingGamesTab
                    
                    ## Player Props =========================================
                    tabItem(
                      tabName = "bettingPlayerPropsTab",
                      fluidRow(
                        tableOutput('show_inputs1'),
                        tableOutput('show_inputs2'),
                        tableOutput('show_outputs1'),
                        tableOutput('show_outputs2')
                      )
                    ),
                    
                    ## Plot =================================================
                    tabItem(
                      tabName = "bettingPlotTab",
                      h4("Plot Data"),
                      fluidRow(
                        column(width = 3,
                               modDataPlotInputUI("modDataPlotInput", 
                                                  teamsDataPickerInput)
                        ),
                        column(width = 9,
                               modDataPlotOutput("modPlot"))
                      )
                    )
                  ) # end tab Items
                ) # end dashboard body
  ) # end dashboard Page
) # end ShinyUI
