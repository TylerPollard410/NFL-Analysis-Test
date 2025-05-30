# pbpData.R
# Function to load play-by-play data with progress reporting

# Dependencies: progressr, future, nflverse, tidyverse (loaded externally in UpdateData.R)

#' Load and return NFL play-by-play data for a set of seasons
#'
#' @param seasons Integer vector of seasons to load (defaults to 2006 through most_recent_season())
#' @return A tibble of play-by-play data, with the usual nflverse timestamp attribute
compute_pbp_data <- function(seasons = 2006:most_recent_season()) {
  # ensure parallel backend is set
  plan("multisession")
  
  # wrap load_pbp in a progress bar
  pbpData <- with_progress({
    load_pbp(seasons = seasons)
  })
  
  return(pbpData)
}

# Uncomment to auto-run when sourced (if you still want that)
# if (exists("allSeasons")) {
#   pbpData <- compute_pbp_data(allSeasons)
#   pbpDataDate <- attributes(pbpData)$nflverse_timestamp
# }