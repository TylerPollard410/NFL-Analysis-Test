# Load necessary libraries
library(dplyr)
library(nflreadr)

# Step 1: Load Data for Previous Season and Current Season

# Set the seasons
previous_season_year <- 2023  # Replace with the actual previous season year
current_season_year <- 2024   # Replace with the actual current season year

# Load schedules for the previous season
previous_season_data <- load_schedules(previous_season_year)

# Load schedules for the current season
current_season_data <- load_schedules(current_season_year)

# Step 2: Prepare Data

# Determine the current week (for example purposes, we'll set it to week 2)
current_week <- 4  # Adjust as needed based on the data availability

# Filter current season data to weeks up to the current week
current_season_data <- current_season_data %>%
  filter(week <= current_week)

# Combine previous season data and current season data
# We will process each season's data separately first

# Function to prepare season data
prepare_season_data <- function(season_data){
  # Filter out games that haven't been played yet
  played_games <- season_data %>%
    filter(!is.na(home_score) & !is.na(away_score))
  
  # Create the game data frame in the required format
  game_data_formatted <- played_games %>%
    select(
      Week = week,
      Game_Date = gameday,
      Team = home_team,
      Opponent = away_team,
      Team_Score = home_score,
      Opponent_Score = away_score
    ) %>%
    mutate(
      Location = "Home"
    ) %>%
    bind_rows(
      played_games %>%
        select(
          Week = week,
          Game_Date = gameday,
          Team = away_team,
          Opponent = home_team,
          Team_Score = away_score,
          Opponent_Score = home_score
        ) %>%
        mutate(
          Location = "Home"
        )
      arrange(Game_Date)
      
      # Add Points Scored and Allowed
      game_data_formatted <- game_data_formatted %>%
        mutate(
          Points_Scored = Team_Score,
          Points_Allowed = Opponent_Score,
          Point_Differential = Team_Score - Opponent_Score
        )
      
      return(game_data_formatted)
}

# Prepare previous season data
previous_season_games <- prepare_season_data(previous_season_data)

# Prepare current season data (up to current week)
current_season_games <- prepare_season_data(current_season_data)

# Step 3: Calculate Team Statistics for Each Season

# Function to calculate team statistics
calculate_team_stats <- function(game_data) {
  team_stats <- game_data %>%
    group_by(Team) %>%
    summarise(
      Games_Played = n(),
      Total_Points_For = sum(Points_Scored),
      Total_Points_Against = sum(Points_Allowed),
      Average_Points_For = mean(Points_Scored),
      Average_Points_Against = mean(Points_Allowed),
      Average_Margin = mean(Point_Differential)
    ) %>%
    ungroup()
  
  return(team_stats)
}

# Calculate team stats for previous season
previous_season_stats <- calculate_team_stats(previous_season_games)

# Calculate team stats for current season (up to current week)
current_season_stats <- calculate_team_stats(current_season_games)

# Step 4: Initialize SRS, OSRS, and DSRS for Previous Season

# Calculate league average points per game for previous season
league_avg_pf_prev <- mean(previous_season_stats$Average_Points_For)
league_avg_pa_prev <- mean(previous_season_stats$Average_Points_Against)
league_avg_ppg_prev <- (league_avg_pf_prev + league_avg_pa_prev) / 2

# Initialize Off_Rating and Def_Rating for previous season
previous_season_stats <- previous_season_stats %>%
  mutate(
    Off_Rating = Average_Points_For - league_avg_ppg_prev,
    Def_Rating = league_avg_ppg_prev - Average_Points_Against
  )

# Initialize SRS as the sum of Off_Rating and Def_Rating
previous_season_stats <- previous_season_stats %>%
  mutate(
    SRS = Off_Rating + Def_Rating,
    Off_SRS = Off_Rating,
    Def_SRS = Def_Rating
  )

# Step 5: Prepare Combined Data with Weights

# Adjust weights for early weeks
if (current_week <= 2) {
  weight_prev <- 0.8  # 80% weight to previous season
  weight_curr <- 0.2  # 20% weight to current season
} else if (current_week <= 5) {
  weight_prev <- 0.6
  weight_curr <- 0.4
} else {
  weight_prev <- 0.4
  weight_curr <- 0.6
}

# Merge previous season stats into current season stats
combined_stats <- current_season_stats %>%
  full_join(previous_season_stats %>% select(Team, Off_SRS_prev = Off_SRS, Def_SRS_prev = Def_SRS, SRS_prev = SRS), by = "Team") %>%
  mutate(
    Off_SRS_prev = ifelse(is.na(Off_SRS_prev), 0, Off_SRS_prev),
    Def_SRS_prev = ifelse(is.na(Def_SRS_prev), 0, Def_SRS_prev),
    SRS_prev = ifelse(is.na(SRS_prev), 0, SRS_prev),
    Average_Points_For = ifelse(is.na(Average_Points_For), league_avg_pf_prev, Average_Points_For),
    Average_Points_Against = ifelse(is.na(Average_Points_Against), league_avg_pa_prev, Average_Points_Against),
    Average_Margin = ifelse(is.na(Average_Margin), 0, Average_Margin),
    Games_Played = ifelse(is.na(Games_Played), 0, Games_Played)
  )

# Step 6: Calculate Initial Ratings for Current Season

# Calculate league average points per game for current season
if (nrow(current_season_stats) > 0) {
  league_avg_pf_curr <- mean(current_season_stats$Average_Points_For)
  league_avg_pa_curr <- mean(current_season_stats$Average_Points_Against)
  league_avg_ppg_curr <- (league_avg_pf_curr + league_avg_pa_curr) / 2
} else {
  # Use previous season's league average if no current data
  league_avg_pf_curr <- league_avg_pf_prev
  league_avg_pa_curr <- league_avg_pa_prev
  league_avg_ppg_curr <- league_avg_ppg_prev
}

# Initialize Off_Rating and Def_Rating for current season
combined_stats <- combined_stats %>%
  mutate(
    Off_Rating_curr = Average_Points_For - league_avg_ppg_curr,
    Def_Rating_curr = league_avg_ppg_curr - Average_Points_Against
  )

# Combine previous and current season ratings
combined_stats <- combined_stats %>%
  mutate(
    Off_Rating = (Off_Rating_curr * weight_curr) + (Off_SRS_prev * weight_prev),
    Def_Rating = (Def_Rating_curr * weight_curr) + (Def_SRS_prev * weight_prev),
    SRS = Off_Rating + Def_Rating
  )

# Initialize Off_SRS and Def_SRS with combined ratings
combined_stats <- combined_stats %>%
  mutate(
    Off_SRS = Off_Rating,
    Def_SRS = Def_Rating
  )

# Step 7: Iterative Calculation of SRS, OSRS, and DSRS

# Check if sufficient data is available for iterative calculations
if (nrow(current_season_games) >= (32 * 2)) {  # Assuming at least 2 games per team
  # Proceed with iterative calculations
  
  # Merge combined_stats into game data
  current_season_games <- current_season_games %>%
    left_join(combined_stats %>% select(Team, Off_SRS, Def_SRS), by = "Team")
  
  # Limit iterations and adjust tolerance
  max_iterations <- 10
  tolerance <- 0.01
  
  # Iterative calculation
  for (i in 1:max_iterations) {
    previous_Off_SRS <- combined_stats$Off_SRS
    previous_Def_SRS <- combined_stats$Def_SRS
    
    # Update Offensive SOS (Opponents' average Def_SRS)
    Off_SOS_update <- current_season_games %>%
      left_join(combined_stats %>% select(Team, Def_SRS), by = c("Opponent" = "Team")) %>%
      group_by(Team) %>%
      summarise(Off_SOS = mean(Def_SRS.y, na.rm = TRUE))
    
    # Update Defensive SOS (Opponents' average Off_SRS)
    Def_SOS_update <- current_season_games %>%
      left_join(combined_stats %>% select(Team, Off_SRS), by = c("Opponent" = "Team")) %>%
      group_by(Team) %>%
      summarise(Def_SOS = mean(Off_SRS.y, na.rm = TRUE))
    
    # Merge SOS updates into combined_stats
    combined_stats <- combined_stats %>%
      left_join(Off_SOS_update, by = "Team") %>%
      left_join(Def_SOS_update, by = "Team") %>%
      mutate(
        Off_SOS = ifelse(is.na(Off_SOS), 0, Off_SOS),
        Def_SOS = ifelse(is.na(Def_SOS), 0, Def_SOS)
      )
    
    # Update Off_SRS and Def_SRS
    combined_stats <- combined_stats %>%
      mutate(
        Off_SRS = Off_Rating + Off_SOS,
        Def_SRS = Def_Rating + Def_SOS,
        SRS = Off_SRS + Def_SRS
      )
    
    # Check for convergence
    if (max(abs(combined_stats$Off_SRS - previous_Off_SRS), abs(combined_stats$Def_SRS - previous_Def_SRS), na.rm = TRUE) < tolerance) {
      cat("Converged after", i, "iterations.\n")
      break
    }
    
    # If last iteration and not converged
    if (i == max_iterations) {
      cat("Reached maximum iterations without full convergence.\n")
    }
  }
  
} else {
  # Use simplified calculations for early weeks
  
  # Calculate Opponents' Previous Season SRS
  Opponents_SRS_prev <- current_season_games %>%
    left_join(previous_season_stats %>% select(Team, SRS_prev = SRS), by = c("Opponent" = "Team")) %>%
    group_by(Team) %>%
    summarise(Opponents_Previous_SRS = mean(SRS_prev, na.rm = TRUE))
  
  combined_stats <- combined_stats %>%
    left_join(Opponents_SRS_prev, by = "Team") %>%
    mutate(
      Opponents_Previous_SRS = ifelse(is.na(Opponents_Previous_SRS), 0, Opponents_Previous_SRS),
      MOV = Average_Margin,
      SOS = Opponents_Previous_SRS,
      SRS = MOV + SOS,
      Off_SRS = Off_Rating + SOS,
      Def_SRS = Def_Rating + SOS
    )
}

# Step 8: Calculate MOV and SOS

# For early weeks, MOV is from current season games, or zero if no games played
combined_stats <- combined_stats %>%
  mutate(
    MOV = ifelse(Games_Played > 0, Average_Margin, 0)
  )

# For early weeks, SOS has been calculated above

# Step 9: Finalize Ratings

# Select and arrange columns for final output
final_ratings <- combined_stats %>%
  select(
    Team,
    Games_Played,
    Total_Points_For,
    Total_Points_Against,
    Average_Points_For,
    Average_Points_Against,
    MOV,
    SOS,
    SRS,
    Off_SRS,
    Def_SRS
  ) %>%
  arrange(desc(SRS))

# Display the Results

# Filter to show the Baltimore Ravens (example)
ravens_stats <- final_ratings %>%
  filter(Team == "BAL")  # Use team abbreviation "BAL" for Ravens

print("Baltimore Ravens Ratings:")
print(ravens_stats)

# Display all teams' Ratings
print("All Teams' Ratings:")
print(final_ratings)
