## Create Game Data for App
# require(tictoc)
require(progressr)
require(nflverse)
require(tidyverse)

future::plan("multisession")
with_progress(pbpData <- load_pbp(seasons = 2006:most_recent_season()))


