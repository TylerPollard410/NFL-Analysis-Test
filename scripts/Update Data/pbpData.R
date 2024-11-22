## Create Game Data for App
library(tictoc)
library(progressr)
library(nflverse)
library(tidyverse)

allSeasons <- 2002:most_recent_season()

future::plan("multisession")
pbpData <- load_pbp(seasons = allSeasons)


