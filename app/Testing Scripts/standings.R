# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)

## Data Manipulation
library(stringr)
library(rvest)

## Tables ----
library(htmltools)
library(gt)
library(gtsummary)
library(gtExtras)
library(reactable)
library(reactablefmtr)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Read in data ----
gameData <- load_schedules(seasons = 2003:most_recent_season())
teamsData <- load_teams(current = FALSE)

# input vars ----
inputSeason <- 2024

# Filter and clean ----
gameDataLong <- gameData |>
  filter(season == inputSeason) |>
  filter(!is.na(result)) |>
  clean_homeaway(invert = c("result","spread_line")) |>
  select(
    week,
    team,
    team_score,
    opponent,
    opponent_score,
    location,
    result
  )

standingData <- gameData |>
  filter(season == inputSeason) |>
  filter(!is.na(result)) |>
  clean_homeaway(invert = c("result","spread_line")) |>
  select(
    week,
    team,
    team_score,
    opponent,
    opponent_score,
    location,
    result
  ) |>
  mutate(
    win = ifelse(result > 0, 1, 0),
    loss = ifelse(result < 0, 1, 0),
    tie = ifelse(result == 0, 1, 0)
  ) |>
  group_by(team) |>
  summarise(
    games_played = n(),
    across(c(win, 
             loss,
             tie,
             team_score,
             opponent_score,
             result),
           ~sum(.x)),
  ) |>
  mutate(
    win_loss_percent = (win + tie/2)/(win + loss + tie/2),
    MoV = result/games_played
  )

standingData2 <- standingData |>
  mutate(
    SOS = 0,
    SRS = MoV + SOS
  )

standingData |>
  filter(team == "NE") |>
  summarise(
    across(-team, ~sum(.x))
  )

standingData |>
  filter(team %in% standingData1$opponent) |>
  summarise(
    across(-team, ~sum(.x))
  )

standingData |>
  filter(team %in% standingData1$opponent) |>
  summarise(
    across(-team, ~mean(.x))
  )

standingData |>
  summarise(
    across(-team, ~sum(.x))
  )

standingData |>
  summarise(
    across(-team, ~mean(.x))
  )


# Load necessary libraries
library(dplyr)
library(nflreadr)

# Step 1: Set the Season Year to 2024
current_season <- 2024

# Step 2: Load Game Data using nflreadr
# Note: The load_schedules() function will fetch data for the specified season.
# For future seasons, data will only be available once the games have been played.
# Attempting to load data for future games may result in an empty or incomplete dataset.

# Fetch the schedule for the 2024 season
game_data <- load_schedules(current_season)
game_data_long <- clean_homeaway(game_data, invert = c("result"))
played_games <- game_data_long |> filter(!is.na(result))

# Create the game data frame in the required format
game_data_formatted <- played_games %>%
  select(
    Week = week,
    Game_Date = gameday,
    Team = home_team,
    Opponent = away_team,
    Team_Score = home_score,
    Opponent_Score = away_score,
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
        Opponent_Score = home_score,
        Location = "Away"
      )
  ) %>%
  arrange(Game_Date)

# Step 4: Calculate Point Differentials for Each Game
game_data_formatted <- played_games %>%
  mutate(Point_Differential = Team_Score - Opponent_Score)

# Step 5: Calculate Initial Average Point Differential for Each Team
team_stats <- played_games %>%
  group_by(team) %>%
  summarise(
    Games_Played = n(),
    Total_Points_For = sum(team_score),
    Total_Points_Against = sum(opponent_score),
    Average_Point_Differential = mean(result)
  ) %>%
  ungroup() %>%
  mutate(
    SRS = Average_Point_Differential, # Initial SRS (since SOS is 0 at first)
    SOS = 0 # Initialize SOS to 0
  )

# Step 6: Iterative Calculation of SRS and SOS
max_iterations <- 100
tolerance <- 0.001
for (i in 1:max_iterations) {
  previous_SRS <- team_stats$SRS
  
  # Update SOS for each team
  team_stats2 <- team_stats %>%
    select(-SOS) %>%
    left_join(
      played_games %>%
        select(team, opponent) %>%
        left_join(team_stats %>% select(team, SRS), by = c("opponent" = "team")) %>%
        group_by(team) %>%
        summarise(SOS = mean(SRS, na.rm = TRUE)),
      by = "team"
    ) %>%
    mutate(SOS = ifelse(is.na(SOS), 0, SOS))
  
  # Update SRS
  team_stats <- team_stats %>%
    mutate(SRS = Average_Point_Differential + SOS)
  
  # Check for convergence
  if (max(abs(team_stats$SRS - previous_SRS)) < tolerance) {
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # If last iteration and not converged
  if (i == max_iterations) {
    cat("Reached maximum iterations without full convergence.\n")
  }
}

# Step 7: Display the Results
# Filter to show the Baltimore Ravens
ravens_stats <- team_stats %>%
  filter(team == "KC") # Use team abbreviation "BAL" for Ravens

# Display Ravens' SRS and SOS
print(ravens_stats)

# Display all teams' SRS and SOS (optional)
print(team_stats)


# Web Scraping ----
standingPFRData <- read_html("https://www.pro-football-reference.com/years/2024/index.htm")
standingPFRDataTables <- standingPFRData |> html_table()
standingAFCDataTables <- standingPFRDataTables[[1]] |>
  slice(-c(1,6,11,16)) |>
  mutate(
    across(-Tm, as.numeric)
    
  )
standingNFCDataTables <- standingPFRDataTables[[2]] |>
  slice(-c(1,6,11,16)) |>
  mutate(
    across(-Tm, as.numeric)
  )

standingDataTable <- bind_rows(
  standingAFCDataTables,
  standingNFCDataTables
) |>
  rename(team_name = Tm) |>
  mutate(
    team_name = str_replace_all(team_name, "[:punct:]|[:symbol:]", "")
  ) |>
  left_join(
    teamsData |>
      select(team_name, team_conf, team_division, team_logo_espn)
  ) |>
  select(
    team_conf,
    team_division,
    team_logo_espn,
    team_name,
    everything()
  )
standingDataTable

standingTable <- reactable(
  data = standingDataTable,
  theme = espn(),
  highlight = TRUE,
  compact = TRUE,
  pagination = FALSE,
  wrap = FALSE,
  outlined = TRUE,
  showSortable = FALSE,
  defaultColDef = colDef(vAlign = "center", 
                         minWidth = 60,
                         headerStyle = list(fontSize = "14px")
  ),
  #defaultSortOrder = "asc",
  # defaultSorted = c("asc" = c("team_conf", "team_division"),
  #                   #"asc" = "team_division", 
  #                   #"desc" = "W-L%", 
  #                   "desc" = c("W-L%", "SRS")),
  defaultSorted = list(team_conf = "asc",
                       team_division = "asc",
                       `W-L%` = "desc",
                       SRS = "desc"),
  rowStyle = group_border_sort(columns = c("team_conf", "team_division"),),
  columns = list(
    ##### Conference
    team_conf = colDef(
      name = "CON",
      style = group_merge_sort("team_conf")
    ),
    ##### Conference
    team_conf = colDef(
      name = "DIV",
      style = group_merge_sort("team_division")
    ),
    ##### Team Logo 
    team_logo_espn = colDef(
      name = "Team",
      minWidth = 200,
      sortable = FALSE,
      #cell = embed_img()
      cell = function(value, index){
        team <- standingDataTable$team_name[index]
        logo <- img(src = value, style = "height: 20px;")
        div(style = "display: flex; align-items: center;",
            logo,
            span(team, style = "margin-left: 4px")
        )
      },
      style = list(borderRight = "1px solid black")
    ),
    ##### Team Name 
    team_name = colDef(
      show = FALSE
    )
  )
)
standingTable


