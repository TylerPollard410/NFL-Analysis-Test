## Create Game Data for App
library(tictoc)
library(progressr)
library(nflverse)
library(tidyverse)

future::plan("multisession")
with_progress(pbpData <- load_pbp(seasons = 2006:most_recent_season()))


