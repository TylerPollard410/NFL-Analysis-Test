# ELO ----
# Define the calc_elo_ratings function
calc_elo_ratings <- function(games,
                             initial_elo = 1500,
                             K = 20,
                             home_advantage = 65,
                             d = 400,
                             apply_margin_multiplier = TRUE) {
  # Ensure games are ordered by date
  games <- games[order(as.Date(games$gameday)), ]
  games <- games |> filter(!is.na(home_score), !is.na(away_score))
  
  # Get a sorted list of teams from both home and away columns
  teams <- sort(unique(c(games$home_team, games$away_team)))
  elo_ratings <- setNames(rep(initial_elo, length(teams)), teams)
  
  season_weeks <- games |> select(season, week) |> distinct()
  # team_ratings_full <- season_weeks |>
  #   cross_join(
  #     teams |> tibble()
  #   )
  
  # Initialize a dataframe to store Elo history information
  elo_history <- games[, c("game_id", "season", "week", "gameday", 
                           "home_team", "away_team", "home_score", "away_score")]
  elo_history$home_elo_pre <- NA_real_
  elo_history$away_elo_pre <- NA_real_
  elo_history$home_elo_post <- NA_real_
  elo_history$away_elo_post <- NA_real_
  
  # Helper function to calculate expected probability using a logistic formula
  expected_prob <- function(rating_A, 
                            rating_B, 
                            home_field = 0,
                            home_field_ind = 0,
                            d = 400) {
    home_field_adj <- home_field*home_field_ind
    1 / (1 + 10 ^ ((rating_B - (rating_A + home_field_adj)) / d))
  }
  
  team_ratings_full <- data.frame()
  # Loop through each game, updating ratings sequentially
  for (i in seq_len(nrow(games))) {
    game <- games[i, ]
    season_temp <- game$season
    week_temp <- game$week
    week_next <- games[i+1, ]$week
    home_team <- game$home_team
    away_team <- game$away_team
    game_loc <- game$location
    
    # Get pre-game ratings
    home_elo <- elo_ratings[home_team]
    away_elo <- elo_ratings[away_team]
    
    # Save pre-game ratings to history
    elo_history$home_elo_pre[i] <- home_elo
    elo_history$away_elo_pre[i] <- away_elo
    
    # get home field indicator
    is_home <- ifelse(game_loc == "Home", 1, 0)
    
    # Calculate expected probabilities (applying home advantage for the home team)
    exp_home <- expected_prob(home_elo, 
                              away_elo, 
                              home_field = home_advantage, 
                              home_field_ind = is_home,
                              d = d)
    exp_away <- 1 - exp_home
    
    # Determine the game outcome (1 for win, 0 for loss, 0.5 for tie)
    if (game$home_score > game$away_score) {
      outcome_home <- 1
      outcome_away <- 0
    } else if (game$home_score < game$away_score) {
      outcome_home <- 0
      outcome_away <- 1
    } else {
      outcome_home <- 0.5
      outcome_away <- 0.5
    }
    
    # Calculate margin and multiplier (if the margin multiplier adjustment is applied)
    margin <- abs(game$home_score - game$away_score)
    multiplier <- if (apply_margin_multiplier) {
      log(margin + 1) * (2.2 / ((0.001 * abs(home_elo - away_elo)) + 2.2))
    } else {
      1
    }
    
    # Calculate updated Elo ratings for home and away teams
    home_elo_new <- home_elo + K * multiplier * (outcome_home - exp_home)
    away_elo_new <- away_elo + K * multiplier * (outcome_away - exp_away)
    
    # Record post-game ratings
    elo_history$home_elo_post[i] <- home_elo_new
    elo_history$away_elo_post[i] <- away_elo_new
    
    # Update the current ratings
    elo_ratings[home_team] <- home_elo_new
    elo_ratings[away_team] <- away_elo_new
    
    elo_ratings_temp <- data.frame(elo_ratings) |> rownames_to_column() |>
      mutate(
        season = season_temp,
        week = week_temp,
        .before = 1
      )
    if(week_temp != week_next || is.na(week_next)){
      team_ratings_full <- rbind(team_ratings_full, elo_ratings_temp)
      
      cat("Finished elo for Season", season_temp, "Week", week_temp, "\n")
    }
  }
  
  # Return a list with the game-by-game Elo history and final ratings
  
  list(elo_history = elo_history, 
       final_ratings = elo_ratings,
       team_ratings = team_ratings_full)
  #return(elo_history)
}

# Compute all seasons starting fresh ----
# eloData <- unique(gameData$season) |>
#   map_dfr(~ {
#     season_data <- gameData |> filter(season == .x)
#     season_data_list <- calc_elo_ratings(
#       season_data,
#       initial_elo = 1500,
#       K = 20,
#       home_advantage = 0,
#       d = 400,
#       apply_margin_multiplier = TRUE
#     )
#     season_data_list$elo_history
#   })
# save(eloData, file = "./app/data/eloData.rda")


## Load Previously Calculated Data ----
load(file = "./app/data/eloData.rda")
eloData <- eloData |>
  filter(season != get_current_season())

newSeasons <- get_current_season()
current_season_data <- gameData |> filter(season == get_current_season())

eloData_update_list <- calc_elo_ratings(
  current_season_data,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE
)
eloData_update <- eloData_update_list$elo_history
eloData <- bind_rows(eloData,
                     eloData_update)

# Remove supp Vars ----
#allSeasons <- 2006:most_recent_season()
rm(newSeasons,
   current_season_data,
   eloData_update_list,
   eloData_update)

# 
# # Check functionality 
# gameDataTest <- gameData |>
#   mutate(
#     home_score = case_when(season == 2024 & season_type == "POST" ~ NA,
#                            .default = home_score),
#     away_score = case_when(season == 2024 & season_type == "POST" ~ NA,
#                            .default = away_score),
#     result = case_when(season == 2024 & season_type == "POST" ~ NA,
#                        .default = result),
#     total = case_when(season == 2024 & season_type == "POST" ~ NA,
#                       .default = total)
#   )
# 
# eloDataListTest <- gameDataTest |>
#   #filter(!(season == 2024 & week > 19)) |>
#   filter(!is.na(home_score), !is.na(away_score)) |>
#   calc_elo_ratings(
#     initial_elo = 1500,
#     K = 20,
#     home_advantage = 65,
#     d = 400,
#     apply_margin_multiplier = FALSE
#   )
# 
# 
# eloHistoryTest <- eloDataListTest$elo_history
# eloForecastTest <- eloDataListTest$final_ratings
# eloRatingsTest <- eloDataListTest$team_ratings
# eloRatingsTest |> filter(season == 2024, week == 18) |> arrange(desc(elo_ratings)) |> View()
# eloRatingsTest |> filter(season == 2024, week == 22) |> arrange(desc(elo_ratings)) |> View()
