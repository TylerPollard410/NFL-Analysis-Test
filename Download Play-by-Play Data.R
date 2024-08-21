## Script to download data for easy use 

library(nflverse)
library(progressr)
library(tictoc)
library(tidyverse)
library(data.table)

# Download pbp data 
with_progress(nflverse_download(pbp, folder_path = "Play-by-Play rds Files"))

# Load play by play data from downloaded files (takes about 9 minutes)
tic()
pbp_data <- list()
pbp_files <- list.files("Play-by-Play rds Files/pbp")
for(i in pbp_files){
  pbp_season <- str_extract(i, "[:digit:]+")
  pbp_data[[i]] <- with_progress(readRDS(paste0("Play-by-Play rds Files/pbp/", i)))
}
pbp_df <- rbindlist(pbp_data)
toc()

save(pbp_df, file = "Play-by-play data.RData")

load("Play-by-Play data.RData")
