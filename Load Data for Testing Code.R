### Tune xgboost parameters

#load libraries
library(tidyverse)
library(data.table)
library(xgboost)
library(mlr)
library(caret)
library(nflverse)
# library(nflfastR)
# library(nflseedR)
# library(nflreadr)
# library(nflplotR)
library(lubridate)
library(plyr)
library(stringr)
library(imputeR)
library(pracma)
library(ModelMetrics)
library(useful)
library(tictoc)
library(progressr)
library(writexl)
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

# Activate progress updates
progressr::handlers(global = TRUE)

# Parallel processing can be activated via the following line
future::plan("multisession")

# Read in current data ----
## Games data -------------------------------------------------------------
games_df2 <- load_schedules()
games_df2 <- games_df2 %>% filter(season >= 2003)
games_df <- games_df2 %>% 
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

# Get long format of games data
games_long_df <- clean_homeaway(games_df, invert = c("result", "spread_line"))

## Play-by-Play data -----------------------------------------------------------------
# Load play by play data from downloaded files (takes about 9 minutes)
pbp_data <- list()
pbp_files <- list.files("Play-by-Play rds Files")
pbp_seasons <- as.numeric(str_extract(pbp_files, "[:digit:]+"))
pbp_files <- pbp_files[pbp_seasons >= 2003]
for(i in pbp_files){
  pbp_season <- str_extract(i, "[:digit:]+")
  pbp_data[[i]] <- with_progress(readRDS(paste0("Play-by-Play rds Files/", i)))
}
pbp_df <- rbindlist(pbp_data)

## Teams data -----------------------------------------------------------------
# Load team ids
nfl_teams <- load_teams()




