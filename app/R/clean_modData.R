## Clean modData for modeling 

clean_modData <- function(data, season_start = 2007){
  if(is.null(data)){
    cat("Error: Data is NULL.\n")
    return(NULL) # Or handle the NULL case as needed
  }
  
  modData2 <- data |> 
    filter(season >= season_start) |>
    #filter(!is.na(result)) |>
    select(
      game_id,
      season,
      season_type,
      week,
      weekday,
      time_of_day,
      home_team,
      home_score,
      away_team,
      away_score,
      result,
      spread_line,
      contains("spread"),
      total,
      total_line,
      totalCover,
      contains("over_"),
      contains("under_"),
      winner,
      contains("moneyline"),
      contains("rest"),
      location,
      div_game,
      roof,
      surface,
      temp,
      wind,
      contains("coach"),
      contains("stadium"),
      contains("home"),
      contains("away"),
      -contains("pat_pct"),
      -contains("fg_pct")
      # off_n, 
      # off_scr, 
      # off_scr_1st, 
      # off_scr_2nd, 
      # off_scr_3rd, 
      # off_scr_4th, 
      # off_1st, 
      # off_td, 
      # off_fg,
      # off_punt,
      # off_to, 
      # def_n, 
      # def_scr,
      # def_scr_1st, 
      # def_scr_2nd, 
      # def_scr_3rd,
      # def_scr_4th, 
      # def_1st, 
      # def_td, 
      # def_fg, 
      # def_punt, 
      # def_to
    ) |>
    mutate(
      across(c(where(is.character), -game_id),
             ~factor(.x))
    ) |>
    mutate(
      home_totalTDScore = 6*home_totalTD,
      home_fg_madeScore = 3*home_fg_made,
      home_pat_madeScore = home_pat_made,
      home_safetiesScore = 2*home_def_safeties,
      home_twoPtConvScore = 2*home_twoPtConv,
      away_totalTDScore = 6*away_totalTD,
      away_fg_madeScore = 3*away_fg_made,
      away_pat_madeScore = away_pat_made,
      away_safetiesScore = 2*away_def_safeties,
      away_twoPtConvScore = 2*away_twoPtConv,
      home_totalTDScore2 = home_totalTDScore + home_pat_madeScore + home_twoPtConvScore,
      away_totalTDScore2 = away_totalTDScore + away_pat_madeScore + away_twoPtConvScore
    ) |>
    mutate(
      location2 = ifelse(location == "Home", 1, 0),
      .after = location
    ) |>
    mutate(
      location = factor(location, levels = c("Neutral", "Home"))
    )
  
  modData3 <- modData2 |>
    select(
      game_id,
      season,
      season_type,
      week,
      home_team,
      home_score,
      away_team,
      away_score,
      result,
      spread_line,
      contains("spread"),
      total,
      total_line,
      totalCover,
      contains("over_"),
      contains("under_"),
      winner,
      contains("moneyline"),
      contains("rest"),
      weekday,
      time_of_day,
      location,
      location2,
      div_game,
      roof,
      surface,
      temp,
      wind,
      contains("coach"),
      contains("stadium"),
      contains("PFG"),
      contains("PAG"),
      contains("MOV"),
      contains("SOS"),
      contains("SRS"),
      contains("OSRS"),
      contains("DSRS"),
      contains("epa"),
      contains("cum"),
      contains("net"),
      contains("roll"),
      contains("off_n"), 
      contains("off_scr"), 
      contains("off_scr_1st"), 
      contains("off_scr_2nd"), 
      contains("off_scr_3rd"), 
      contains("off_scr_4th"), 
      contains("off_1st"), 
      contains("off_td"), 
      contains("off_fg"),
      contains("off_punt"),
      contains("off_to"), 
      contains("def_n"), 
      contains("def_scr"),
      contains("def_scr_1st"), 
      contains("def_scr_2nd"), 
      contains("def_scr_3rd"),
      contains("def_scr_4th"), 
      contains("def_1st"), 
      contains("def_td"), 
      contains("def_fg"), 
      contains("def_punt"), 
      contains("def_to"),
      -home_def_tds,
      -away_def_tds
      # contains("off"),
      # contains("def"),
      # -contains("totalTD"),
      # -contains("offTD"),
      # -contains("special_teams_tds"),
      # -contains("fumble_recovery_tds"),
      # -contains("def_tds"),
      # -contains("pat_made"),
      # -contains("pat_att"),
      # -contains("twoPtConv"),
      # -contains("twoPtAtt"),
      # -contains("fg_made"),
      # -contains("fg_att"),
      # -contains("def_safeties")
    ) |>
    mutate(
      surface = as.character(surface),
      surface = case_when(
        surface == "" ~ NA,
        surface == "grass " ~ "grass",
        surface %in% c("a_turf", "astroplay") ~ "astroturf", 
        .default = surface
      ),
      surface = factor(surface)
    ) |>
    select(-surface)
  
  return(modData3)
}

# Test function
#nfl_data <- clean_modData(data = modData, season_start = 2007)
