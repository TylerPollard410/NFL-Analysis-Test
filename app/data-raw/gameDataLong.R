## Create Game Data for App
library(nflverse)
library(tidyverse)

allSeasons <- 2002:most_recent_season()
gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line"))


