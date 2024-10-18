#### NFL Analysis App
## Tyler Pollard
## 8 Aug 2023

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybrowser)
library(shinycssloaders)
# library(shinyalert)
# library(shinyBS)
# library(shinyjs)
# library(colourpicker)
library(bs4Dash)
library(plotly)
library(plyr)
# library(readxl)
library(readr)
# library(haven)
library(ggplot2)
library(data.table)
library(lubridate)
library(hms)
library(RColorBrewer)
# library(sp)
# library(htmltools)
library(fresh)
# library(extrafont)
library(stringr)
# library(reshape2)
# library(png)
library(ggpubr)
# library(htmlTable)
# library(tibble)
# library(EnvStats)
# library(xtable)
# library(grid)
library(DT)
# library(rhandsontable)
# library(rvest)
# library(scales)
library(caret)
library(knitr)
library(data.table)
library(markdown)
library(nflverse)
library(progressr)
library(gt)
library(dplyr)


# Set theme
my_theme <- create_theme(
  theme = "paper",
  bs4dash_sidebar_dark(
    bg = "#2d3b4d"
  ),
  bs4dash_status(
    primary = "purple", info = "gold"
  )
)
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8")

# tags$head(tags$script('
#                     // Define function to set height of "map" and "map_container"
#                     setHeight = function() {
#                       var window_height = $(window).height();
#                       var header_height = $(".main-header").height();
# 
#                       var boxHeight = window_height - header_height - 30;
# 
#                       $("#picks_popularity").height(boxHeight - 400);
#                       $("#pick_popularity_table").height(boxHeight - 400);
# 
#                       $("#picks_output").height(boxHeight - 130);
#                       $("#picks_table").height(boxHeight - 130);
#                     };
# 
#                     // Set input$box_height when the connection is established
#                     $(document).on("shiny:connected", function(event) {
#                       setHeight();
#                     });
# 
#                     // Refresh the box height on every window resize event
#                     $(window).on("resize", function(){
#                       setHeight();
#                     });
#                   '))

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(dark = NULL, footer = dashboardFooter(left = br()), freshTheme = my_theme,
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
                        text = "Release Date: 8 Aug 2023",
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
                  )
                ), # close header
                scrollToTop = TRUE,
                # Dashboard Sidebar =============
                sidebar = dashboardSidebar(
                  skin = "dark",
                  elevation = 5,
                  fixed = FALSE,
                  minified = FALSE,
                  status = "primary",
                  compact = TRUE,
                  # Sidebar Menu ---------------
                  sidebarMenu(
                    id = "menu_items",
                    menuItem(text = "Home", tabName = "home", icon = icon("home")),
                    menuItem(text = "Data", tabName = "data", icon = icon("table")),
                    menuItem(text = "Data Exploration", icon = icon("chart-area"),
                             menuSubItem(text = "Betting Statistics", tabName = "data_exploration_betting")),
                    menuItem(text = "Modeling", tabName = "modeling",icon = icon("chart-line"))
                  ) # close sidebar menu
                ), # close dashboard sidebar
                # Dashboard Body ================
                body = dashboardBody(
                  tabItems(
                    # Home Tab ===============
                    tabItem(
                      tabName = "home",
                      h1("Welcome to the NFL Game Dashboard", align  = "center"),
                      br(),
                      box(width = 12, closable = FALSE, collapsible = FALSE, headerBorder = FALSE,
                          fluidRow(column(width = 12, align = "center", plotOutput("image"))),
                          withMathJax(),
                          includeMarkdown("../RMarkdown Files/Description.Rmd")
                      )
                    ), # close Home tab Item 
                    # Data Tab ===============
                    tabItem(
                      tabName = "data",
                      fluidPage(
                        fluidRow(
                          ## Data Filters ----
                          column(width = 3,
                                 box(title = strong("Data Filters"), collapsible = FALSE, closable = FALSE, maximizable = TRUE, status = "warning", solidHeader = TRUE, width = 12,
                                     # detect(),
                                     # textOutput(outputId = "dimensions_out"))
                                     h4("Season"),
                                     uiOutput(outputId = "data_season_filter_out"),
                                     h4("game_type"),
                                     h4("week"),
                                     h4("gameday"),
                                     h4("weekday"),
                                     h4("gametime"),
                                     h4("away_team"),
                                     h4("away_score"),
                                     h4("home_team"),
                                     h4("home_score"),
                                     h4("location"),
                                     h4("result"),
                                     h4("total"),
                                     h4("overtime"),
                                     h4("away_rest"),
                                     h4("home_rest"),
                                     h4("away_moneyline"),
                                     h4("home_moneyline"),
                                     h4("spread_line"),
                                     h4("away_spread_odds"),
                                     h4("home_spread_odds"),
                                     h4("total_line"),
                                     h4("under_odds"),
                                     h4("over_odds"),
                                     h4("div_game"),
                                     h4("roof"),
                                     h4("surface"),
                                     h4("temp"),
                                     h4("wind"),
                                     h4("away_qb_name"),
                                     h4("home_qb_name"),
                                     h4("away_coach"),
                                     h4("home_coach"),
                                     h4("referee"),
                                     h4("stadium")
                                 ) # end box
                          ), # end column
                          ## Table Output ----
                          column(width = 9,
                                 box(title = strong("NFL Game Data since 2003"), 
                                     collapsible = FALSE,
                                     closable = FALSE,
                                     maximizable = TRUE, 
                                     status = "warning", 
                                     solidHeader = TRUE, 
                                     width = 12, height = "60vh",
                                     style = "overflow-y: scroll",
                                     
                                     fluidRow(
                                       column(width = 6,
                                              uiOutput(outputId = "data_column_select_out")
                                       ),
                                       column(width = 6,
                                              uiOutput(outputId = "data_column_hold_out")
                                       )
                                     ),
                                     hr(),
                                     withSpinner(dataTableOutput(outputId = "game_datatable_out"),
                                                 type = 8)
                                 ) # end box
                          ) # end column
                        ) # end fluid Row
                      ) # end fluid page
                    ), # end Data Tab Item
                    # Data Exploration Tab ===============
                    ## Betting Statistics ----------------
                    tabItem(
                      tabName = "data_exploration_betting",
                      fluidPage(
                        h1("Betting Statistics Tab"),
                        hr(),
                        fluidRow(
                          column(width = 3,
                                 box(title = "Data Filters", 
                                     solidHeader = TRUE, 
                                     elevation = 5, 
                                     width = 12,
                                     
                                     uiOutput(outputId = "betting_team_filter_out"),
                                     uiOutput(outputId = "betting_time_filter_out")
                                 ),
                                 box(
                                   title = "Betting Statistics", 
                                   solidHeader = TRUE, 
                                   elevation = 5,
                                   width = 12
                                 )
                          ),
                          column(width = 9,
                                 fluidRow(
                                   # column(width = 3,
                                          tabBox(
                                            id = "betting_tabbox",
                                            title = HTML(paste("Overall Betting", "Statistics", sep = "<br/>")),
                                            solidHeader = TRUE,
                                            elevation = 5,
                                            side = "right",
                                            #width = 12,
                                            type = "tabs",
                                            tabPanel(
                                              title = "ATS",
                                              div(style = 'overflow-y:scroll;height:70vh;',
                                                  withSpinner(gt_output(outputId = "betting_overall_spread_cover_table"))
                                              )
                                            ),
                                            tabPanel(
                                              title = "Moneyline",
                                              div(style = 'overflow-y:scroll;height:70vh;',
                                                  withSpinner(gt_output(outputId = "betting_overall_moneyline_table"))
                                              )
                                            ),
                                            tabPanel(
                                              title = "Over/Under",
                                              div(style = 'overflow-y:scroll;height:70vh;',
                                                  withSpinner(gt_output(outputId = "betting_overall_over_under_table"))
                                              )
                                            )
                                          )
                                   # ),
                                   # column(width = 9,
                                   #        box(title = "Team Betting Statistics", solidHeader = TRUE, elevation = 5, width = 12,
                                   #            style = "height:80vh; overflow-y: scroll",
                                   #            gt_output(outputId = "betting_team_spread_cover_table")
                                   #        )
                                   # )
                                 )
                          )
                        )
                      ) # end fluid page
                    ), # end Data Tab Item
                    # Modelling Tab =============
                    tabItem(
                      tabName = "modeling",
                      tabsetPanel(
                        tabPanel(title = "Model Info",
                                 fluidPage(
                                   h1("Model Info"),
                                   hr(),
                                   box(width = 12, 
                                       closable = FALSE, 
                                       collapsible = FALSE,
                                       headerBorder = FALSE,
                                       
                                       withMathJax(),
                                       includeMarkdown("../RMarkdown Files/Modeling_Info.Rmd")
                                   )
                                 ) # end fluid page
                        ), # end model info tab panel
                        #  Model Fitting =============
                        tabPanel(title = "Model Fit",
                                 fluidPage(
                                   h1("Model Fit"),
                                   hr()
                                 ) # end fluid page
                        ), # end Model Fit tab panel
                        # ========= Prediction Tab ===============
                        tabPanel(title = "Prediction",
                                 fluidPage(
                                   h1("Prediction"),
                                   hr()
                                 ) # end fluid page
                        ) # end Prediction  tab Panel
                      ) # end tabset
                    ) # end Modeling Tab
                  ) # end Tab items
                ) # end dashboard body
  ) # end dashboard Page
) # end ShinyUI
