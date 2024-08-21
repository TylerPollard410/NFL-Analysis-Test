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

# Activate progress updates
progressr::handlers(global = TRUE)

# Parallel processing can be activated via the following line
future::plan("multisession")

# Read in current data ----
## Games data ----
games_df2 <- load_schedules()
games_df2 <- games_df2 %>% filter(season >= 2003)
games_df <- games_df2 %>% 
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

# Get long format of games data
games_long_df <- clean_homeaway(games_df, invert = c("result", "spread_line"))

write_xlsx(games_df2, "app/data/NFL Game Data with old Team Names.xlsx")
write_xlsx(games_df, "app/data/NFL Game Data.xlsx")
write_xlsx(games_long_df, "app/data/NFL Game Data Long.xlsx")

## Play-by-Play data ----
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

write_xlsx(pbp_df, "app/data/Play-by-Play Data.xlsx")

## Teams data ----
# Load team ids
nfl_teams <- load_teams()


# Simulate NFL Season ----
# Test simulate function
elo_model <- function(teams, games, week_num, ...) {
  
  # round out (away from zero)
  # this way the simulator never simulates a tie
  # the simulator will still allow ties to be simulated if you want
  # ... but not on playoff games
  round_out <- function(x) {
    x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
    x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
    return(x)
  }
  
  # we're going to store elo as a new columns in the teams data
  # it won't start off there of course, so we need to determine it
  # from our arguments
  if (!("elo" %in% colnames(teams))) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull the elo info from custom arguments
      teams <- teams %>%
        dplyr::inner_join(args$elo %>% dplyr::select(team, elo), by = c("team" = "team"))
    } else {
      # error with a friendly error message if no elo data is passed in
      stop("Pass in a tibble `elo` as an argument to `simulate_nfl()`")
    }
  }
  
  # isolate the ratings data by sim and by team only
  # we will want to join to the games data later and don't want excess columns
  ratings <- teams %>% dplyr::select(sim, team, elo)
  
  # simulate game outcomes
  games <- games %>%
    # add in the away team's elo to the game data
    # note we join on both `sim` and the team
    # always join on `sim` to make sure each sim cares about only its data
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename(away_elo = elo) %>%
    # repeat for the home team as well
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
    dplyr::rename(home_elo = elo) %>%
    dplyr::mutate(
      # calculate the elo difference
      elo_diff = home_elo - away_elo,
      # add in a small HFA amount if played at home
      elo_diff = elo_diff + ifelse(location == "Home", 20, 0),
      # make an adjustment for rest
      elo_diff = elo_diff + (home_rest - away_rest) / 7 * 25,
      # playoff games swing elo more
      elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2),
      # from elo, we calculate the home team's win percentage
      wp = 1 / (10^(-elo_diff / 400) + 1),
      # we also can calculate the estimate (mean points home team wins by)
      estimate = elo_diff / 25,
      result = dplyr::case_when(
        # !!! ALWAYS DO THIS NEXT LINE IN YOUR `result` CHANGES !!!
        # you have to make sure you're only changing unfinished games in current week
        # if you don't do this, it will usually error out on a friendly error message
        is.na(result) & week == week_num ~ 
          as.integer(round_out(rnorm(n(), estimate, 13))),
        # if not this week or known result, leave as-is
        TRUE ~ as.integer(result)
      ),
      # simplify to 1 = win, 0 = loss, 0.5 = tie to help calculate elo shift
      outcome = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ 1,
        result < 0 ~ 0,
        TRUE ~ 0.5
      ),
      # calculate the amount to adjust home team's elo by
      elo_input = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ elo_diff * 0.001 + 2.2,
        result < 0 ~ -elo_diff * 0.001 + 2.2,
        TRUE ~ 1.0,
      ),
      elo_mult = log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input,
      elo_shift = 20 * elo_mult * (outcome - wp)
    ) %>%
    # we don't want these columns in `games` any more
    # remove any columns you don't need when you're done
    # otherwise the next week they'll get joined as `col.x` and `col.y`
    # which will almost certainly break your script
    dplyr::select(
      -away_elo, -home_elo, -elo_diff, -wp, -estimate,
      -outcome, -elo_input, -elo_mult
    )
  
  # apply elo shifts
  teams <- teams %>%
    # join games results from this week to away teams (within same sim!)
    # note this is a LEFT join, we don't want to remove any teams rows
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, away_team, elo_shift),
                     by = c("sim" = "sim", "team" = "away_team")
    ) %>%
    # away team's elo gets subtracted by elo amount
    # if the team wasn't an away team, do nothing
    dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    # we don't want to keep `elo_shift` in `teams` either, remove it
    dplyr::select(-elo_shift) %>%
    # repeat the above except now do it for the home team
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, home_team, elo_shift),
                     by = c("sim" = "sim", "team" = "home_team")
    ) %>%
    # note that a team on a bye will have `elo_shift` as NA for both joins
    # this means it won't change, which is what we want
    dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    dplyr::select(-elo_shift)
  
  # we need to keep `elo_shift` out of `games` too and we're done with it
  games <- games %>%
    dplyr::select(-elo_shift)
  
  # return the updated teams and games information
  # note that `teams` will now have an updated `elo` column which will
  # be used for the next week's games
  # note that starting `elo` values are the same per-team... 
  # ... but after that will differ per sim depending on that sim's results
  return(list(teams = teams, games = games))
}

initial_elo <- tibble::tibble(
  team = unique(nflseedR::divisions$team),
  elo = rnorm(length(unique(nflseedR::divisions$team)), 1500, 150)
)

sim <- simulate_nfl(
  nfl_season = 2022,
  process_games = elo_model,
  elo = initial_elo,
  fresh_season = FALSE,
  test_week = 3
)

sim$teams %>%
  dplyr::filter(team == "BAL") %>%
  utils::head() %>%
  knitr::kable()

summary(sim)

list2env(sim, .GlobalEnv)

