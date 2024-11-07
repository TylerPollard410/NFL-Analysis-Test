## NFL Analysis App
## Tyler Pollard
## 8 Aug 2023

# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)

## Data Manipulation
library(stringr)
library(rvest)

## Tables ----
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
# library(patchwork)

## Modeling ----
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
teamsDataInput <- load_teams(current = TRUE) |>
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
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8")

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
                  )
                  #   ### Data Tab
                  #   navbarTab(tabName = "dataTab", text = "Data",
                  #             navbarTab(tabName = "standingsTab", text = "Standing"), # end Standings
                  #             navbarTab(tabName = "scoresTab", text = "Scores"), # end Scores
                  #             navbarTab(tabName = "teamTab", text = "Team",
                  #                       dropdownHeader("Offense"),
                  #                       navbarTab(tabName = "teamOffenseOverviewTab", text = "Overview"),
                  #                       navbarTab(tabName = "teamOffensePassingTab", text = "Passing"),
                  #                       navbarTab(tabName = "teamOffenseRushingTab", text = "Rushing"),
                  #                       navbarTab(tabName = "teamOffenseConversionsTab", text = "Conversions"),
                  #                       navbarTab(tabName = "teamOffenseDriveAveragesTab", text = "Drive Averages"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Defense"),
                  #                       navbarTab(tabName = "teamDefenseOverviewTab", text = "Overview"),
                  #                       navbarTab(tabName = "teamDefensePassingTab", text = "Passing"),
                  #                       navbarTab(tabName = "teamDefenseRushingTab", text = "Rushing"),
                  #                       navbarTab(tabName = "teamDefenseConversionsTab", text = "Conversions"),
                  #                       navbarTab(tabName = "teamDefenseDriveAveragesTab", text = "Drive Averages"),
                  #                       navbarTab(tabName = "teamDefensePositionTab", text = "Against Position",
                  #                                 navbarTab(tabName = "teamDefensePositionQBTab", text = "Against QB"),
                  #                                 navbarTab(tabName = "teamDefensePositionRBTab", text = "Against RB"),
                  #                                 navbarTab(tabName = "teamDefensePositionTETab", text = "Against TE"),
                  #                                 navbarTab(tabName = "teamDefensePositionWRTab", text = "Against WR")
                  #                       ),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Special Teams"),
                  #                       navbarTab(tabName = "teamSpecialTeamsReturnsTab", text = "Kick/Punt Returns"),
                  #                       navbarTab(tabName = "teamSpecialTeamsKickingTab", text = "Kicing"),
                  #                       navbarTab(tabName = "teamSpecialTeamsPuntingTab", text = "Punting"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Scoring"),
                  #                       navbarTab(tabName = "teamScoringForTab", text = "Scoring For"),
                  #                       navbarTab(tabName = "teamScoringAgainstTab", text = "Scoring Against")
                  #             ), # end Team
                  #             navbarTab(tabName = "playerTab", text = "Player",
                  #                       dropdownHeader("Offense"),
                  #                       navbarTab(tabName = "playerOffenseScrimmageTab", text = "Scrimmage"),
                  #                       navbarTab(tabName = "playerOffensePassingTab", text = "Passing"),
                  #                       navbarTab(tabName = "playerOffenseRushingTab", text = "Rushing"),
                  #                       navbarTab(tabName = "playerOffenseReceivingTab", text = "Receiving"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Defense"),
                  #                       navbarTab(tabName = "playerDefenseOverviewTab", text = "Overview"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Special Teams"),
                  #                       navbarTab(tabName = "playerSpecialTeamsReturnsTab", text = "Kick/Punt Returns"),
                  #                       navbarTab(tabName = "playerSpecialTeamsKickingTab", text = "Kicking"),
                  #                       navbarTab(tabName = "playerSpecialTeamsPuntingTab", text = "Punting"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Scoring"),
                  #                       navbarTab(tabName = "playerScoringOverviewTab", text = "Overview"),
                  #                       dropdownDivider(),
                  #                       dropdownHeader("Fantasy"),
                  #                       navbarTab(tabName = "playerFantasyRanksTab", text = "Ranks") # end Fantasy
                  #             ) # end Player
                  #   ), # end Data Tab
                  #   ### Betting Tab
                  #   navbarTab(tabName = "bettingTab", text = "Betting",
                  #             navbarTab(tabName = "bettingGameTab", text = "Games"),
                  #             navbarTab(tabName = "bettingPlayerPropsTab", text = "Player Props")
                  #   ), # end Betting Tab
                  #   
                  #   ### Prediction Tab
                  #   navbarTab(tabName = "predictionTab", text = "Prediction Models")
                  # ) # end navbarMenu
                ), # close header
                scrollToTop = TRUE,
                # Dashboard Sidebar =============
                sidebar = dashboardSidebar(
                  id = "sidebar",
                  skin = "dark",
                  elevation = 5,
                  fixed = FALSE,
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
                    menuItem(text = "Standings", tabName = "standingsTab", icon = icon("table")),
                    menuItem(text = "Scores", tabName = "scoresTab", icon = icon("table")),
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
                    menuItem(text = "Games", tabName = "bettingGamesTab", icon = icon("user")),
                    menuItem(text = "Player Props", tabName = "bettingPlayerPropsTab", icon = icon("user"))
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
                      h1("Welcome to the NFL Game Dashboard", align  = "center"),
                      br(),
                      box(width = 12, closable = FALSE, collapsible = FALSE, headerBorder = FALSE,
                          fluidRow(column(width = 12, align = "center", 
                                          imageOutput("image"))
                          ),
                          withMathJax(),
                          includeMarkdown("../_docs/Description.Rmd")
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
                                 choices = seq(2003, get_current_season()),
                                 selected = get_current_season()
                                 # options = pickerOptions(
                                 #   container = "body",
                                 #   style = "background-color: #eec900;"
                                 # )
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
                          withSpinner(
                            gt_output(outputId = "standingsTableAFC"), type = 8
                          )
                        ), # end AFC column
                        column(
                          width = 6,
                          withSpinner(
                            gt_output(outputId = "standingsTableNFC"), type = 8
                          )
                        ) # end NFC column
                      ), # end divsion standings row
                      ##### Playoffs Table ----
                      fluidRow(
                        column(
                          width = 6,
                          withSpinner(
                            gt_output(outputId = "standingsTableAFCplayoffs"), type = 8
                          )
                        ), # end AFC column
                        column(
                          width = 6,
                          withSpinner(
                            gt_output(outputId = "standingsTableNFCplayoffs"), type = 8
                          )
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
                               sliderTextInput(
                                 inputId = "playerOffenseSeason",
                                 label = "Select seasons",
                                 choices = seq(2003, get_current_season()),
                                 selected = c(get_current_season(),get_current_season())
                               )
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
                                   .data = teamsDataInput,
                                   label = team_name,
                                   value = team_abbr,
                                   group_by = team_division
                                 ),
                                 multiple = TRUE,
                                 selected = teamsDataInput$team_abbr,
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
                      tabsetPanel(
                        tabPanel(
                          title = "Overview"
                        ),
                        tabPanel(
                          title = "Passing",
                          withSpinner(
                            reactableOutput(outputId = "playerOffensePassingTable"), type = 8
                          )
                        ),
                        tabPanel(
                          title = "Rushing"
                        ),
                        tabPanel(
                          title = "Receiving"
                        )
                      )
                    ) # end Player Offense tabItem
                    #### Rushing ----
                    #### Receiving ----
                    #### Conversions ----
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
                    
                    
                    # Prediction Tab  #########################################
                  ) # end tab Items
                ) # end dashboard body
  ) # end dashboard Page
) # end ShinyUI
