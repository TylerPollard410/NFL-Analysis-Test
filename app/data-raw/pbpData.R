## Create Game Data for App
library(tictoc)
library(progressr)
library(nflverse)
library(tidyverse)

future::plan("multisession")
tic()
with_progress(pbpData <- load_pbp(seasons = 2002:most_recent_season()))
toc()

