#### NFL Analysis App
## Tyler Pollard
## 8 Aug 2023

# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)

## Tables ----
library(gt)
library(gtsummary)
library(gtExtras)
# library(pointblank)
# library(tfrmt)
# library(gto)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

#library(shinybrowser)
#library(shinycssloaders)
# library(shinyalert)
# library(shinyBS)
# library(shinyjs)
# library(colourpicker)
# library(bs4Dash)
# library(plotly)
# library(plyr)
# library(readxl)
#library(readr)
# library(haven)
# library(ggplot2)
# library(data.table)
# library(lubridate)
# library(hms)
# library(RColorBrewer)
# library(sp)
# library(htmltools)
# library(fresh)
# library(extrafont)
# library(stringr)
# library(reshape2)
# library(png)
# library(ggpubr)
# library(htmlTable)
# library(tibble)
# library(EnvStats)
# library(xtable)
# library(grid)
# library(DT)
# library(rhandsontable)
# library(rvest)
# library(scales)
# library(caret)
# library(knitr)
# library(data.table)
# library(markdown)
# library(nflverse)
# library(progressr)
# library(gt)
# library(dplyr)


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
                  ## Sidebar Menu ---------------
                  sidebarMenu(
                    id = "menu_items",
                    menuItem(text = "Home", tabName = "home", icon = icon("home")),
                    menuItem(text = "Data", tabName = "data", icon = icon("table"),
                             menuSubItem(text = "Team Statistics", tabName = "data_team_stats", icon = icon("users")),
                             menuSubItem(text = "Player Statistics", tabName = "data_player_stats", icon = icon("user"))
                    )
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
                    # Data ===============
                    ## Team Statistics ----
                    tabItem(
                      tabName = "data_team_stats",
                      fluidPage(
                        tabsetPanel(
                          id = "team_tabset",
                          type = "pills",
                          ### Offense ----
                          tabPanel(
                            title = "Offense", 
                            br(),
                            h1("Offensive Team Statistics"),
                            tabsetPanel(
                              id = "offense_team_statistics",
                              vertical = TRUE,
                              side = "left",
                              
                              #### Totals ----
                              tabPanel(
                                title = "Total Yards",
                              ),
                              
                              #### Passing ----
                              tabPanel(
                                title = "Passing",
                              ),
                              
                              #### Rushing ----
                              tabPanel(
                                title = "Rushing",
                              ),
                              
                              #### Receiving ----
                              tabPanel(
                                title = "Receiving",
                              ),
                              
                              #### Downs ----
                              tabPanel(
                                title = "Downs",
                              )
                            )
                          ),
                          
                          ### Defense ----
                          tabPanel(
                            title = "Defense",
                            br(),
                            h1("Defensive Team Statistics"),
                            tabsetPanel(
                              id = "defense_team_statistics",
                              vertical = TRUE,
                              side = "left",
                              
                              #### Totals ----
                              tabPanel(
                                title = "Yards Allowed",
                              ),
                              
                              #### Passing ----
                              tabPanel(
                                title = "Passing",
                              ),
                              
                              #### Rushing ----
                              tabPanel(
                                title = "Rushing",
                              ),
                              
                              #### Receiving ----
                              tabPanel(
                                title = "Receiving",
                              ),
                              
                              #### Downs ----
                              tabPanel(
                                title = "Downs",
                              ),
                              
                              #### Turnovers ----
                              tabPanel(
                                title = "Turnovers",
                              )
                            )
                          ),
                          
                          ### Special Teams ----
                          tabPanel(
                            title = "Special Teams",
                            br(),
                            h1("Special Teams Team Statistics"),
                            
                            tabsetPanel(
                              id = "special_team_statistics",
                              vertical = TRUE,
                              side = "left",
                              
                              #### Returning ----
                              tabPanel(
                                title = "Returning",
                              ),
                              
                              #### Kicking ----
                              tabPanel(
                                title = "Kicking",
                              ),
                              
                              #### Punting ----
                              tabPanel(
                                title = "Punting",
                              ) # end punting
                            ) # Special Team tabset
                          ) # end Speacial Team Tab
                        ) # end team tabset panel
                      ) # end fluid page
                    ), # end Team Statistics subItem
                    
                    ## Player Statistics ----
                    tabItem(
                      tabName = "data_player_stats",
                      fluidPage(
                        textOutput(outputId = "player_stats_title"),
                        tabsetPanel(
                          id = "player_tabset",
                          type = "pills",
                          ### Offense ----
                          tabPanel(
                            title = "Offense", 
                            br(),
                            # h1("Offensive Player Statistics"),
                            tabBox(
                              id = "offense_player_statistics", width = 12,
                              # vertical = FALSE,
                              # side = "left",
                              
                              #### Passing ----
                              tabPanel(
                                title = "Passing",
                                withSpinner(
                                  gt_output(outputId = "playerOffensivePassingSumTable_out"), type = 8)
                              ),
                              
                              #### Rushing ----
                              tabPanel(
                                title = "Rushing",
                              ),
                              
                              #### Receiving ----
                              tabPanel(
                                title = "Receiving",
                              ),
                              
                              #### Touchdowns ----
                              tabPanel(
                                title = "Touchdowns",
                              )
                            )
                          ),
                          
                          ### Defense ----
                          tabPanel(
                            title = "Defense",
                            br(),
                            h1("Defensive Player Statistics"),
                            tabsetPanel(
                              id = "defense_player_statistics",
                              vertical = TRUE,
                              side = "left",
                              
                              #### Tackles ----
                              tabPanel(
                                title = "Yards Allowed",
                              ),
                              
                              #### Sacks ----
                              tabPanel(
                                title = "Passing",
                              ),
                              
                              #### Interceptions ----
                              tabPanel(
                                title = "Rushing",
                              )
                            )
                          ),
                          
                          ### Special Teams ----
                          tabPanel(
                            title = "Special Teams",
                            br(),
                            h1("Special Teams Player Statistics"),
                            
                            tabsetPanel(
                              id = "special_player_statistics",
                              vertical = TRUE,
                              side = "left",
                              
                              #### Returning ----
                              tabPanel(
                                title = "Returning",
                              ),
                              
                              #### Kicking ----
                              tabPanel(
                                title = "Kicking",
                              ),
                              
                              #### Punting ----
                              tabPanel(
                                title = "Punting",
                              ) # end punting
                            ) # end Special Teams tabset panel
                          ) # end Special Teams tab
                        ) # end player tabset
                      ) # end fluid page
                    ) # end Player Statistics tabItem
                  ) # end tab Items
                ) # end dashboard body
  ) # end dashboard Page
) # end ShinyUI
