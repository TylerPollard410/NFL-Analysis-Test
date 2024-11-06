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

## Plotting ----
library(smplot2)
# library(cowplot)
# library(GGally)
# library(patchwork)

## Modeling ----
library(elo)
library(MASS)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Load Data ----
## Team info ----
#teamsData <- load_teams(current = FALSE)

## Historical Season -----
#load(file = "./Model Fitting/Data/seasonStandings.RData")

## Current Season ----
# gameDataCurrent <- load_schedules(seasons = get_current_season()) |>
#   #filter(complete.cases(result)) |>
#   filter(game_type == "REG") |>
#   mutate(
#     home_team = clean_team_abbrs(home_team),
#     away_team = clean_team_abbrs(away_team)
#   ) 
# 
# gameDataLongCurrent <- gameDataCurrent |>
#   clean_homeaway(invert = c("result", "spread_line")) 


## Create Function
calculateStandings <- function(season = 2024){
  if(!IsWhole(season)){
    stop("Please enter integer value between 2003 and 2024")
    #return(NULL)
  }
  
  if(season < 2003 | season > get_current_season()){
    stop("Please enter integer value between 2003 and 2024")
  }
  
  gameDataCurrent <- load_schedules(seasons = season) |>
    filter(complete.cases(result)) |>
    filter(game_type == "REG") |>
    mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team)
    ) 
  
  gameDataLongCurrent <- gameDataCurrent |>
    clean_homeaway(invert = c("result", "spread_line")) 
  
  standingCurrent <- gameDataLongCurrent |>
    filter(!is.na(result)) |>
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
      MOV = result/games_played
    ) |>
    mutate(team_PPG = team_score/games_played, .after = team_score) |>
    mutate(opp_PPG = opponent_score/games_played, .after = opponent_score) |>
    select(
      "team",
      "games_played",
      "win", 
      "loss",
      "tie", 
      "win_loss_percent",
      everything()
    )
  
  leaguePPGCurrent <- sum(standingCurrent$team_score)/sum(standingCurrent$games_played)
  standingCurrent <- standingCurrent |>
    mutate(SOS = 0, .after = MOV) |>
    mutate(SRS = MOV, .after = SOS) |>
    mutate(OSRS = team_PPG - leaguePPGCurrent) |>
    mutate(DSRS = SRS - OSRS)
  
  max_iterations <- 100
  tolerance <- 0.001
  for (i in 1:max_iterations) {
    previous_SRS <- standingCurrent$SRS
    previous_OSRS <- standingCurrent$OSRS
    previous_DSRS <- standingCurrent$DSRS
    
    standingCurrent <- standingCurrent |>
      left_join(
        gameDataLongCurrent |>
          select(team, opponent, result, team_score) |>
          left_join(standingCurrent |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
          mutate(
            SOS = SRS,
            SRS = result + SOS,
            OSOS = DSRS,
            OSRS = team_score + OSOS - mean(team_score)
          ) |>
          group_by(team) |>
          summarise(
            newSOS = mean(SOS, na.rm = TRUE),
            newSRS = mean(SRS, na.rm = TRUE),
            newOSRS = mean(OSRS)
          ), 
        by = join_by(team)
      ) |>
      mutate(
        SOS = ifelse(is.na(newSOS), 0, newSOS),
        SRS = ifelse(is.na(newSRS), 0, newSRS),
        OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
        DSRS = SRS - OSRS
      ) |>
      select(-newSOS, -newSRS, -newOSRS)
    
    if(max(abs(standingCurrent$SRS - previous_SRS),
           abs(standingCurrent$OSRS - previous_OSRS),
           abs(standingCurrent$DSRS - previous_DSRS)) < tolerance){
      cat("Converged after", i, "iterations.\n")
      break
    }
    
    # If last iteration and not converged
    if (i == max_iterations) {
      cat("Reached maximum iterations = ",i, "without full convergence.\n")
    }
  }
  
  standingCurrent <- standingCurrent |> arrange(desc(SRS)) |>
    left_join(teamsData |> select(team_abbr, team_name, team_conf, team_division), 
              by = c("team" = "team_abbr")) |>
    select(
      team,
      team_name,
      team_conf, 
      team_division,
      everything()
    )
  
  return(standingCurrent)
}
#standingsCurrent <- calculateStandings(season = 2024)


# # Get seeding ----
# standingCurrentNFLverse <- calculate_standings(
#   nflverse_object = gameDataCurrent |> filter(!is.na(result))
# )
# 
# # Make gt table ----
# standingsTableData <- standingsCurrent |>
#   left_join(
#     standingCurrentNFLverse |>
#       select(
#         team, 
#         div_rank, 
#         seed,
#         div_pct,
#         conf_pct,
#         sov,
#         sos),
#     by = join_by(team)
#   ) |>
#   select(
#     team,
#     team_name,
#     team_conf,
#     team_division,
#     div_rank,
#     seed,
#     games_played,
#     win,
#     loss,
#     tie,
#     win_loss_percent,
#     conf_pct,
#     div_pct,
#     everything()
#   ) |> 
#   left_join(
#     teamsData,
#     by = join_by(team == team_abbr,team_name, team_conf, team_division)
#   )
# 
# standingsTable <- standingsTableData |>
#   filter(team_conf == "AFC") |>
#   rename(
#     "GP" = games_played,
#     "W" = win,
#     "L" = loss,
#     "T" = tie,
#     "W-L%" = win_loss_percent,
#     "PF" = team_score,
#     "PA" = opponent_score,
#     "PD" = result
#   ) |>
#   select(
#     "team",
#     "team_name",
#     "team_division",
#     "div_rank",
#     "GP",
#     "W",
#     "L",
#     "T",
#     "W-L%",
#     "PF",
#     "PA",
#     "PD",
#     "MOV",
#     "SOS",
#     "SRS",
#     "OSRS",
#     "DSRS"
#   ) |>
#   group_by(team_division) |>
#   arrange(team_division, div_rank) |>
#   gt() |>
#   cols_hide(
#     columns = "div_rank"
#   ) |>
#   gt_nfl_logos(
#     columns = "team"
#   ) |>
#   fmt_percent(
#     columns = "W-L%",
#     decimals = 2
#   ) |>
#   fmt_number(
#     columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
#     decimals = 2
#   ) |>
#   data_color(
#     columns = c("SRS"),
#     method = "numeric",
#     palette = c("red", "green")
#   ) |>
#   tab_options(
#     data_row.padding = 0,
#     column_labels.font.weight = "bold"
#   ) |>
#   tab_style(
#     style = cell_borders(sides = "right"),
#     locations = cells_body(
#       columns = c("team_name", "W-L%", "PD")
#     )
#   ) |>
#   cols_label(
#     team = "",
#     team_name = "Team"
#   )
# 
# standingsTable




