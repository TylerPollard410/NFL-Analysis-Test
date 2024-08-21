# Project-3

# Description
This shiny dashboard explores data on **National Football League (NFL)** games from 2006 to 2020 and fits three statistical models to predict the total score of games. The models can then be used, along with user input of predictor values, to predict the total score of a particular game. Based on the predicted total points, the user will know if the expected point total is going to be over or under the total point line established for that game. The [data](http://www.habitatring.com/games.csv) provides information about each game like identifcation parameters, game conditions, game results, and pregame lines (expected results of the game scores) with corresponding odds. For more information about the dataset and a data dictionary of the column variables visit [NFL Game data information](https://github.com/nflverse/nfldata/blob/master/DATASETS.md#games).

# Required R Packages
library(shiny)  
library(shinydashboard) 
library(bs4Dash)  
library(shinyWidgets) 
library(tidyverse)  
library(dplyr)  
library(ggplot2)  
library(caret)  
library(fresh)  
library(lubridate)  
library(forcats)  
library(knitr)  
library(DT) 
library(readr)  
library(plotly) 

# Install required packages
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "tidyverse", "dplyr", "ggplot2", "caret", "fresh", "lubridate", "forcats", "knitr", "DT", "readr", "plotly")); devtools::install_github("bs4Dash)

# Run Shiny app
shiny::runGitHub(repo = "TylerAPollard/Project-3", username = getOption("TylerAPollard"), ref = "main")
