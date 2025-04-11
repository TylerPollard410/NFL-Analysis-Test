## Model home and away score

# Load Libraries ----
## Data Manipulation
library(stringr)

## Tables
library(DBI)
library(RPostgres)
library(data.table)

## Plotting
library(smplot2)
library(patchwork)

## Modeling
library(zoo)
library(pracma)
library(forecast)
library(timetk)
library(elo)
library(glmnet)
library(xgboost)
library(MASS)
library(fitdistrplus)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DiscreteWeibull)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(projpred)
library(cmdstanr)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse
library(espnscrapeR)
library(nflverse)

## Tidyverse
library(tidyverse)


# Data -----
## Uploads ----
source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
source("./app/R/clean_modData.R")



## Clean ----
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# modData2 <- modData |> 
#   filter(season >= 2007) |>
#   #filter(!is.na(result)) |>
#   select(
#     game_id,
#     season,
#     season_type,
#     week,
#     weekday,
#     time_of_day,
#     home_team,
#     home_score,
#     away_team,
#     away_score,
#     result,
#     spread_line,
#     contains("spread"),
#     total,
#     total_line,
#     totalCover,
#     contains("over_"),
#     contains("under_"),
#     winner,
#     contains("moneyline"),
#     contains("rest"),
#     location,
#     div_game,
#     roof,
#     surface,
#     temp,
#     wind,
#     contains("coach"),
#     contains("stadium"),
#     contains("home"),
#     contains("away"),
#     -contains("pat_pct"),
#     -contains("fg_pct")
#     # off_n, 
#     # off_scr, 
#     # off_scr_1st, 
#     # off_scr_2nd, 
#     # off_scr_3rd, 
#     # off_scr_4th, 
#     # off_1st, 
#     # off_td, 
#     # off_fg,
#     # off_punt,
#     # off_to, 
#     # def_n, 
#     # def_scr,
#     # def_scr_1st, 
#     # def_scr_2nd, 
#     # def_scr_3rd,
#     # def_scr_4th, 
#     # def_1st, 
#     # def_td, 
#     # def_fg, 
#     # def_punt, 
#     # def_to
#   ) |>
#   mutate(
#     across(c(where(is.character), -game_id),
#            ~factor(.x))
#   ) |>
#   mutate(
#     home_totalTDScore = 6*home_totalTD,
#     home_fg_madeScore = 3*home_fg_made,
#     home_pat_madeScore = home_pat_made,
#     home_safetiesScore = 2*home_def_safeties,
#     home_twoPtConvScore = 2*home_twoPtConv,
#     away_totalTDScore = 6*away_totalTD,
#     away_fg_madeScore = 3*away_fg_made,
#     away_pat_madeScore = away_pat_made,
#     away_safetiesScore = 2*away_def_safeties,
#     away_twoPtConvScore = 2*away_twoPtConv,
#     home_totalTDScore2 = home_totalTDScore + home_pat_madeScore + home_twoPtConvScore,
#     away_totalTDScore2 = away_totalTDScore + away_pat_madeScore + away_twoPtConvScore
#   ) |>
#   mutate(
#     location2 = ifelse(location == "Home", 1, 0),
#     .after = location
#   ) |>
#   mutate(
#     location = factor(location, levels = c("Neutral", "Home"))
#   )
# 
# which(modData$home_totalTD != (modData$home_offTD +
#                                  modData$home_special_teams_tds +
#                                  modData$home_fumble_recovery_tds +
#                                  modData$home_def_tds))
# which(modData$away_totalTD != (modData$away_offTD +
#                                  modData$away_special_teams_tds +
#                                  modData$away_fumble_recovery_tds +
#                                  modData$away_def_tds))
# which(modData$home_totalTD != (modData$home_pat_att + modData$home_twoPtAtt))
# which(modData$away_totalTD != (modData$away_pat_att + modData$away_twoPtAtt))
# 
# modData3 <- modData2 |>
#   select(
#     game_id,
#     season,
#     season_type,
#     week,
#     home_team,
#     home_score,
#     away_team,
#     away_score,
#     result,
#     spread_line,
#     contains("spread"),
#     total,
#     total_line,
#     totalCover,
#     contains("over_"),
#     contains("under_"),
#     winner,
#     contains("moneyline"),
#     contains("rest"),
#     weekday,
#     time_of_day,
#     location,
#     location2,
#     div_game,
#     roof,
#     surface,
#     temp,
#     wind,
#     contains("coach"),
#     contains("stadium"),
#     contains("PFG"),
#     contains("PAG"),
#     contains("MOV"),
#     contains("SOS"),
#     contains("SRS"),
#     contains("OSRS"),
#     contains("DSRS"),
#     contains("epa"),
#     contains("cum"),
#     contains("net"),
#     contains("roll"),
#     contains("off_n"), 
#     contains("off_scr"), 
#     contains("off_scr_1st"), 
#     contains("off_scr_2nd"), 
#     contains("off_scr_3rd"), 
#     contains("off_scr_4th"), 
#     contains("off_1st"), 
#     contains("off_td"), 
#     contains("off_fg"),
#     contains("off_punt"),
#     contains("off_to"), 
#     contains("def_n"), 
#     contains("def_scr"),
#     contains("def_scr_1st"), 
#     contains("def_scr_2nd"), 
#     contains("def_scr_3rd"),
#     contains("def_scr_4th"), 
#     contains("def_1st"), 
#     contains("def_td"), 
#     contains("def_fg"), 
#     contains("def_punt"), 
#     contains("def_to"),
#     -home_def_tds,
#     -away_def_tds
#     # contains("off"),
#     # contains("def"),
#     # -contains("totalTD"),
#     # -contains("offTD"),
#     # -contains("special_teams_tds"),
#     # -contains("fumble_recovery_tds"),
#     # -contains("def_tds"),
#     # -contains("pat_made"),
#     # -contains("pat_att"),
#     # -contains("twoPtConv"),
#     # -contains("twoPtAtt"),
#     # -contains("fg_made"),
#     # -contains("fg_att"),
#     # -contains("def_safeties")
#   ) |>
#   mutate(
#     surface = as.character(surface),
#     surface = case_when(
#       surface == "" ~ NA,
#       surface == "grass " ~ "grass",
#       surface %in% c("a_turf", "astroplay") ~ "astroturf", 
#       .default = surface
#     ),
#     surface = factor(surface)
#   ) |>
#   select(-surface)

modData3 <- clean_modData(data = modData, season_start = 2007)
colnames(modData3)

table(modData3$surface, useNA = "ifany")

stadiums <- modData3 |>
  select(stadium, roof, surface) |>
  distinct() |>
  mutate(
    across(is.factor,
           ~as.character(.x))
  ) |>
  mutate(
    surface = ifelse(surface == "", NA, surface)
  )
stadiums2 <- stadiums |>
  filter(!is.na(surface))
stadiums3 <- left_join(
  stadiums |> select(-surface),
  stadiums2
)

modData3Long <- modData3 |>
  clean_homeaway(invert = c("result", "spread_line")) |>
  mutate(
    location2 = ifelse(location == "home", 1, 0)
  )

write_csv(modData3, file = "~/Desktop/modData.csv")

# ## Select ----
# modScoreData <- modData2 |>
#   select(
#     game_id,
#     season,
#     season_type,
#     week,
#     location,
#     location2,
#     div_game,
#     result,
#     spread_line,
#     total,
#     total_line,
#     home_team,
#     contains("home_SRS"),
#     contains("home_OSRS"),
#     contains("home_DSRS"),
#     home_score,
#     home_totalTD,
#     home_offTD,
#     home_special_teams_tds,
#     home_fumble_recovery_tds,
#     home_def_tds,
#     home_pat_made,
#     home_pat_att,
#     home_twoPtConv,
#     home_twoPtAtt,
#     home_fg_made,
#     home_fg_att,
#     home_def_safeties,
#     away_team,
#     contains("away_SRS"),
#     contains("away_OSRS"),
#     contains("away_DSRS"),
#     away_score,
#     away_totalTD,
#     away_offTD,
#     away_special_teams_tds,
#     away_fumble_recovery_tds,
#     away_def_tds,
#     away_pat_made,
#     away_pat_att,
#     away_twoPtConv,
#     away_twoPtAtt,
#     away_fg_made,
#     away_fg_att,
#     away_def_safeties
#   ) |>
#   mutate(home_deffTD = home_fumble_recovery_tds + home_def_tds, .after = home_special_teams_tds) |>
#   mutate(away_deffTD = away_fumble_recovery_tds + away_def_tds, .after = away_special_teams_tds)
# 
# modScoreDataLong <- modScoreData |>
#   clean_homeaway(invert = c("result", "spread_line"))
# 
# write_csv(modScoreData |> filter(season >= 2020), 
#           file = "~/Desktop/wideNFLdata.csv")
# write_csv(modScoreDataLong |> filter(season >= 2020),
#           file = "~/Desktop/longNFLdata.csv")
# 
# modScoreData2 <- modScoreData |>
#   mutate(
#     home_totalTD = 6*home_totalTD,
#     home_offTD = 6*home_offTD,
#     home_special_teams_tds = 6*home_special_teams_tds,
#     home_deffTD = 6*home_deffTD,
#     home_pat_made = home_pat_made,
#     home_twoPtConv = 2*home_twoPtConv,
#     home_extra_pts = home_pat_made + home_twoPtConv,
#     home_fg_made = 3*home_fg_made,
#     home_def_safeties = 2*home_def_safeties,
#     away_totalTD = 6*away_totalTD,
#     away_offTD = 6*away_offTD,
#     away_special_teams_tds = 6*away_special_teams_tds,
#     away_deffTD = 6*away_deffTD,
#     away_pat_made = away_pat_made,
#     away_twoPtConv = 2*away_twoPtConv,
#     away_extra_pts = away_pat_made + away_twoPtConv,
#     away_fg_made = 3*away_fg_made,
#     away_def_safeties = 2*away_def_safeties
#   ) |>
#   mutate(
#     home_score2 = (home_offTD + 
#                      home_special_teams_tds +
#                      home_deffTD +
#                      #home_pat_made + home_twoPtConv +
#                      home_extra_pts +
#                      home_fg_made +
#                      home_def_safeties),
#     .after = home_score
#   ) |>
#   mutate(
#     away_score2 = (away_offTD + 
#                      away_special_teams_tds +
#                      away_deffTD +
#                      #away_pat_made + away_twoPtConv +
#                      away_extra_pts +
#                      away_fg_made +
#                      away_def_safeties),
#     .after = away_score
#   )
# 
# home_score_diff <- which(modScoreData2$home_score != modScoreData2$home_score2)
# away_score_diff <- which(modScoreData2$away_score != modScoreData2$away_score2)
# 
# modScoreData2diff <- modScoreData2 |> slice(c(home_score_diff,away_score_diff))

## Predict XGB scores ----
load(file = "~/Desktop/NFL Analysis Data/xgb_home_model.RData")
load(file = "~/Desktop/NFL Analysis Data/xgb_away_model.RData")
load(file = "~/Desktop/NFL Analysis Data/xgb_result_model.RData")
load(file = "~/Desktop/NFL Analysis Data/xgb_total_model.RData")

xgb_home_model <- home_model
xgb_away_model <- away_model
xgb_result_model <- result_model
xgb_total_model <- total_model

varImp_home <- varImp(xgb_home_model)
varImp_home
best_home_vars <- varImp_home$importance |>
  filter(Overall > 0) |>
  row.names()

varImp_away <- varImp(xgb_away_model)
varImp_away
best_away_vars <- varImp_away$importance |>
  filter(Overall > 0) |>
  row.names()

varImp_result <- varImp(xgb_result_model)
varImp_result
best_result_vars <- varImp_result$importance |>
  filter(Overall > 0) |>
  row.names()

varImp_total <- varImp(xgb_total_model)
varImp_total
best_total_vars <- varImp_total$importance |>
  filter(Overall > 0) |>
  row.names()

modData4 <- modData3 |> 
  select(
    game_id, 
    best_home_vars,
    best_away_vars,
    best_result_vars,
    best_total_vars
  )
mod_IDs <- modData4 |>
  filter(complete.cases(modData4)) |>
  pull(game_id)

modData5 <- modData3 |>
  filter(game_id %in% mod_IDs)
modData5 <- modData5 |>
  #filter(game_id %in% mod_IDs) |>
  mutate(
    xgb_pred_home_score = predict(xgb_home_model, newdata = modData5),
    .after = home_score
  ) |>
  mutate(
    xgb_pred_away_score = predict(xgb_away_model, newdata = modData5),
    .after = away_score
  ) |>
  mutate(
    xgb_pred_result = xgb_pred_home_score - xgb_pred_away_score,
    xgb_pred_result2 = predict(xgb_result_model, newdata = modData5),
    .after = result
  ) |>
  mutate(
    xgb_pred_total = xgb_pred_home_score + xgb_pred_away_score,
    xgb_pred_total2 = predict(xgb_total_model, newdata = modData5),
    .after = total
  )

mean(abs(modData5$result - modData5$xgb_pred_result))
mean(abs(modData5$result - modData5$xgb_pred_result2))
mean(abs(modData5$total - modData5$xgb_pred_total))
mean(abs(modData5$total - modData5$xgb_pred_total2))

## Split ----
histModelData1 <- modData5 |> 
  filter(between(season, 2020, 2023) | (season == 2024 & week <= 6))
modelData1 <- modData5 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result))

# Pre-Process Data ----
## Predictors ----
predictorData <- histModelData1 |> 
  select(
    # xgb_pred_home_score,
    # xgb_pred_away_score,
    xgb_pred_result,
    xgb_pred_result2,
    xgb_pred_total,
    xgb_pred_total2,
    #union(best_home_vars, best_away_vars),
    best_home_vars,
    best_away_vars,
    best_result_vars,
    best_total_vars,
    home_rest,
    away_rest,
    #weekday,
    #time_of_day
    location2,
    div_game,
    roof,
    temp,
    wind
  )

### Center, Scale ----
preProc_CS_corr <- preProcess(predictorData,
                              method = c("center", "scale", "corr")
)
preProc_CS <- preProcess(predictorData,
                         method = c("center", "scale")
)
preProcValuesYeo <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)
preProcValuesArc <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)


preProc_CS_corr
preProc_CS_corr$method$remove
predictorData2 <- predict(preProc_CS, predictorData)
histModelData2 <- predict(preProc_CS, histModelData1)
modelData2 <- predict(preProc_CS, modelData1)

histModelData <- histModelData2
modelData <- modelData2

predictorDataYeo <- predict(preProcValuesYeo, predictorData)
histModelDataYeo <- predict(preProcValuesYeo, histModelData1)
modelDataYeo <- predict(preProcValuesYeo, modelData1)

histModelData <- histModelDataYeo
modelData <- modelDataYeo

# Plots ----
# [1] "game_id"                      "season"                       "season_type"                 
# [4] "week"                         "home_team"                    "home_score"                  
# [7] "away_team"                    "away_score"                   "result"                      
# [10] "spread_line"                  "spreadCover"                  "total"                       
# [13] "total_line"                   "totalCover"                   "overtime"                    
# [16] "over_odds"                    "over_prob"                    "under_odds"                  
# [19] "under_prob"                   "location"                     "div_game"                    
# [22] "roof"                         "surface"                      "temp"                        
# [25] "wind"                         "home_rest"                    "home_moneyline"              
# [28] "home_moneyline_prob"          "home_spread_odds"             "home_spread_prob"            
# [31] "home_coach"                   "home_off_plays_cum"           "home_off_pass_plays_cum"     
# [34] "home_off_rush_plays_cum"      "home_off_penalty_plays_cum"   "home_off_kick_plays_cum"     
# [37] "home_off_special_plays_cum"   "home_off_pass_plays_perc_cum" "home_off_rush_plays_perc_cum"
# [40] "home_def_plays_cum"           "home_def_pass_plays_cum"      "home_def_rush_plays_cum"     
# [43] "home_def_penalty_plays_cum"   "home_def_kick_plays_cum"      "home_def_special_plays_cum"  
# [46] "home_def_pass_plays_perc_cum" "home_def_rush_plays_perc_cum" "home_off_epa_cum"            
# [49] "home_off_pass_epa_cum"        "home_off_rush_epa_cum"        "home_off_penalty_epa_cum"    
# [52] "home_off_kick_epa_cum"        "home_off_special_epa_cum"     "home_def_epa_cum"            
# [55] "home_def_pass_epa_cum"        "home_def_rush_epa_cum"        "home_def_penalty_epa_cum"    
# [58] "home_def_kick_epa_cum"        "home_def_special_epa_cum"     "home_off_epa_roll"           
# [61] "home_off_pass_epa_roll"       "home_off_rush_epa_roll"       "home_off_penalty_epa_roll"   
# [64] "home_off_kick_epa_roll"       "home_off_special_epa_roll"    "home_def_epa_roll"           
# [67] "home_def_pass_epa_roll"       "home_def_rush_epa_roll"       "home_def_penalty_epa_roll"   
# [70] "home_def_kick_epa_roll"       "home_def_special_epa_roll"    "home_PFG"                    
# [73] "home_PAG"                     "home_MOV"                     "home_SOS"                    
# [76] "home_SRS"                     "home_OSRS"                    "home_DSRS"                   
# [79] "home_totalTD"                 "home_offTD"                   "home_special_teams_tds"      
# [82] "home_def_tds"                 "home_fg_made"                 "home_fg_att"                 
# [85] "home_off_fg_att_roll"         "home_off_fg_pct_cum"          "home_off_fg_pct_roll"        
# [88] "home_twoPtConv"               "home_twoPtAtt"                "home_safeties"               
# [91] "home_pat_made"                "home_pat_att"                 "home_off_pat_att_roll"       
# [94] "home_off_pat_pct_cum"         "home_off_pat_pct_roll"        "home_off_interceptions"      
# [97] "home_def_interceptions"       "home_off_fumbles"             "home_def_fumbles"            
# [100] "home_off_n"                   "home_off_scr"                 "home_off_scr_1st"            
# [103] "home_off_scr_2nd"             "home_off_scr_3rd"             "home_off_scr_4th"            
# [106] "home_off_1st"                 "home_off_td"                  "home_off_fg"                 
# [109] "home_off_punt"                "home_off_to"                  "home_def_n"                  
# [112] "home_def_scr"                 "home_def_scr_1st"             "home_def_scr_2nd"            
# [115] "home_def_scr_3rd"             "home_def_scr_4th"             "home_def_1st"                
# [118] "home_def_td"                  "home_def_fg"                  "home_def_punt"               
# [121] "home_def_to"                  "home_net_epa_cum"             "home_net_epa_roll"           
# [124] "home_off_net_epa_cum"         "home_off_net_epa_roll"        "home_pass_net_epa_cum"       
# [127] "home_pass_net_epa_roll"       "home_rush_net_epa_cum"        "home_rush_net_epa_roll"      
# [130] "home_penalty_net_epa_cum"     "home_penalty_net_epa_roll"    "home_MOV_net"                
# [133] "home_SOS_net"                 "home_SRS_net"                 "home_OSRS_net"               
# [136] "home_DSRS_net"                "away_rest"                    "away_moneyline"              
# [139] "away_moneyline_prob"          "away_spread_odds"             "away_spread_prob"            
# [142] "away_coach"                   "away_off_plays_cum"           "away_off_pass_plays_cum"     
# [145] "away_off_rush_plays_cum"      "away_off_penalty_plays_cum"   "away_off_kick_plays_cum"     
# [148] "away_off_special_plays_cum"   "away_off_pass_plays_perc_cum" "away_off_rush_plays_perc_cum"
# [151] "away_def_plays_cum"           "away_def_pass_plays_cum"      "away_def_rush_plays_cum"     
# [154] "away_def_penalty_plays_cum"   "away_def_kick_plays_cum"      "away_def_special_plays_cum"  
# [157] "away_def_pass_plays_perc_cum" "away_def_rush_plays_perc_cum" "away_off_epa_cum"            
# [160] "away_off_pass_epa_cum"        "away_off_rush_epa_cum"        "away_off_penalty_epa_cum"    
# [163] "away_off_kick_epa_cum"        "away_off_special_epa_cum"     "away_def_epa_cum"            
# [166] "away_def_pass_epa_cum"        "away_def_rush_epa_cum"        "away_def_penalty_epa_cum"    
# [169] "away_def_kick_epa_cum"        "away_def_special_epa_cum"     "away_off_epa_roll"           
# [172] "away_off_pass_epa_roll"       "away_off_rush_epa_roll"       "away_off_penalty_epa_roll"   
# [175] "away_off_kick_epa_roll"       "away_off_special_epa_roll"    "away_def_epa_roll"           
# [178] "away_def_pass_epa_roll"       "away_def_rush_epa_roll"       "away_def_penalty_epa_roll"   
# [181] "away_def_kick_epa_roll"       "away_def_special_epa_roll"    "away_PFG"                    
# [184] "away_PAG"                     "away_MOV"                     "away_SOS"                    
# [187] "away_SRS"                     "away_OSRS"                    "away_DSRS"                   
# [190] "away_totalTD"                 "away_offTD"                   "away_special_teams_tds"      
# [193] "away_def_tds"                 "away_fg_made"                 "away_fg_att"                 
# [196] "away_off_fg_att_roll"         "away_off_fg_pct_cum"          "away_off_fg_pct_roll"        
# [199] "away_twoPtConv"               "away_twoPtAtt"                "away_safeties"               
# [202] "away_pat_made"                "away_pat_att"                 "away_off_pat_att_roll"       
# [205] "away_off_pat_pct_cum"         "away_off_pat_pct_roll"        "away_off_interceptions"      
# [208] "away_def_interceptions"       "away_off_fumbles"             "away_def_fumbles"            
# [211] "away_off_n"                   "away_off_scr"                 "away_off_scr_1st"            
# [214] "away_off_scr_2nd"             "away_off_scr_3rd"             "away_off_scr_4th"            
# [217] "away_off_1st"                 "away_off_td"                  "away_off_fg"                 
# [220] "away_off_punt"                "away_off_to"                  "away_def_n"                  
# [223] "away_def_scr"                 "away_def_scr_1st"             "away_def_scr_2nd"            
# [226] "away_def_scr_3rd"             "away_def_scr_4th"             "away_def_1st"                
# [229] "away_def_td"                  "away_def_fg"                  "away_def_punt"               
# [232] "away_def_to"                  "away_net_epa_cum"             "away_net_epa_roll"           
# [235] "away_off_net_epa_cum"         "away_off_net_epa_roll"        "away_pass_net_epa_cum"       
# [238] "away_pass_net_epa_roll"       "away_rush_net_epa_cum"        "away_rush_net_epa_roll"      
# [241] "away_penalty_net_epa_cum"     "away_penalty_net_epa_roll"    "away_MOV_net"                
# [244] "away_SOS_net"                 "away_SRS_net"                 "away_OSRS_net"               
# [247] "away_DSRS_net"                "home_totalTDScore"            "home_fg_madeScore"           
# [250] "home_pat_madeScore"           "home_safetiesScore"           "home_twoPtConvScore"         
# [253] "away_totalTDScore"            "away_fg_madeScore"            "away_pat_madeScore"          
# [256] "away_safetiesScore"           "away_twoPtConvScore"          "home_totalTDScore2"          
# [259] "away_totalTDScore2" 

result_plot <- ggplot(data = modData5) +
  geom_histogram(aes(x = result, y = after_stat(density)),
                 bins = 100, fill = fillPPC, color = colorPPC) +
  geom_density(aes(x = result)) +
  theme_bw()
result_plot

total_plot <- ggplot(data = modData5) +
  geom_histogram(aes(x = total, y = after_stat(density)),
                 bins = 100, fill = fillPPC, color = colorPPC) +
  geom_density(aes(x = total)) +
  theme_bw()
total_plot

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# RESULT ----------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

#as.formula(paste(c(best_vars_home[1:20], best_vars_away[1:20]), collapse = " + "))

# > preProc_CS$method$remove
# [1] "home_off_net_epa_cum"    "home_off_epa_cum"        "home_off_epa_roll"      
# [4] "away_def_epa_cum"        "away_off_net_epa_cum"    "away_off_net_epa_roll"  
# [7] "away_off_epa_roll"       "home_def_epa_roll"       "home_def_epa_cum"       
# [10] "away_off_rush_plays_cum" "away_fg_made_roll_5"     "home_off_net_epa_roll"  
# [13] "home_net_epa_cum"        "away_MOV_roll_net"       "home_MOV_roll"          
# [16] "away_net_epa_cum"        "away_MOV_roll"           "away_off_epa_cum"       
# [19] "home_fg_made_roll_5" 
home_result_diff_vars <- setdiff(best_home_vars, best_result_vars)
home_result_diff_vars
away_result_diff_vars <- setdiff(best_away_vars, best_result_vars)
away_result_diff_vars
home_away_result_new_vars <- union(home_result_diff_vars, away_result_diff_vars)
home_away_result_new_vars

varImp_result$importance |> slice(1:30)
home_away_result_new_vars

## Model ----
# #div_game + roof + temp + wind + (1 | home_team) + (1 | away_team)
formula_result <- bf(
  result ~
    xgb_pred_result2 +
    # home_off_pass_epa_roll +
    # home_off_net_epa_roll +
    # home_pass_net_epa_roll +
    # home_pass_net_epa_cum +
    # home_off_net_epa_cum +
    # home_off_pass_epa_cum +
    # home_off_epa_cum +
    # away_MOV_roll_net +
    # home_off_epa_roll +
    # home_OSRS_roll +
    # home_net_epa_cum +
    # away_MOV_net +
    # home_PFG_roll +
    # home_MOV +
    # home_offTD_roll_5 +
    # home_PFG +
    # home_def_rush_plays_cum +
    # home_SRS +
    # away_net_epa_roll +
    # away_SRS_roll_net +
    # away_off_net_epa_cum +
    # away_pass_net_epa_cum +
    # away_OSRS_roll_net +
    # away_off_epa_cum +
    # away_off_net_epa_roll +
    # away_off_pass_epa_cum +
    # away_MOV_roll_net +
    # away_PFG_roll +
    # away_OSRS_net +
    # away_SRS_roll_net +
    # away_off_epa_roll +
    # away_pass_net_epa_roll +
    # away_off_pass_epa_roll +
    # home_def_pass_epa_cum +
    # home_PAG +
    # away_SRS_net +
    # away_OSRS_roll +
    # away_MOV_roll +
    # away_net_epa_cum +
    # home_net_epa_cum +
    #home_rest +
    #away_rest +
    #weekday +
    #time_of_day +
    #location2 +
    #div_game +
    roof +
    temp +
    #wind +
    (1 | home_team) +
    (1 | away_team)
) + 
  brmsfamily(family = "student", link = "identity")
#mixture(brmsfamily(family = "gaussian", link = "identity"), nmix = 2)
#brmsfamily(family = "negbinomial", link = "log")

default_prior(formula_result, histModelData)

priors_result <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 10), class = "b"),
  #prior(normal(0, 5), class = "b", dpar = "mu1"),
  #prior(normal(0, 5), class = "b", dpar = "mu2"),
  prior(student_t(3, 0, 10), class = "sigma"),
  #prior(student_t(3, 0, 10), class = "sigma1"),
  #prior(student_t(3, 0, 10), class = "sigma2"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(student_t(3, 0, 5), class = "sd")
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu1"),
  #prior(student_t(3, 0, 5), class = "sd", dpar = "mu2")
)

get_prior(formula_result,
          data = histModelData,
          prior = priors_result)

system.time(
  fit_result <- brm(
    formula_result,
    data = histModelData,
    #prior = priors_result,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)


print(fit_result, digits = 4)
check_collinearity(fit_result)

#fit_Team <- fit_Team2
#pp_check(fit_result, resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, resp = "result", ndraws = 100, type = "dens_overlay")

# pp_check(fit_result, newdata = modelData, 
#          resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, newdata = modelData,
         resp = "result", ndraws = 100, type = "dens_overlay")

### Fixed Effects ----
fixedEff_result <- fixef(fit_result)
fixedEff_result <- data.frame(fixedEff_result) |>
  mutate(
    p_val = dnorm(Estimate/Est.Error)
  ) |>
  mutate(
    across(everything(), function(x){round(x, 4)})
  ) |>
  mutate(
    Sig = ifelse(p_val < 0.01, "***",
                 ifelse(p_val < 0.05, "**",
                        ifelse(p_val < 0.1, "*", "")))
  )
print(fixedEff_result, digits = 4)
fixedSigEff_result <- fixedEff_result |> filter(p_val < 0.2)
print(fixedSigEff_result)

randEff_result <- ranef(fit_result)
randEff_result

### MAE ----
fitResiduals_result <- 
  residuals(
    fit_result,
    resp = "result",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_result$Estimate))

predResiduals_result <- 
  residuals(
    fit_result,
    resp = "result",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_result$Estimate))

fitResult <- 26
assign(paste0("fit_result", fitResult), fit_result)
assign(paste0("fixedEff_result", fitResult), fixedEff_result)
assign(paste0("randEff_result", fitResult), randEff_result)

save(fit_result, 
     file = paste0("~/Desktop/NFL Analysis Data/fit_result",
                   fitResult,
                   ".RData")
)

save(fit_result, 
     file = paste0("~/Desktop/NFLAnalysisTest/app/data/fit_result.rda")
)

# Posteriors ----
## Training ----
train_result <- histModelData$result

set.seed(52)
posteriorFit_result <- posterior_predict(
  fit_result,
  resp = "result"
)
posteriorFitMean_result <- colMeans(posteriorFit_result)
posteriorFitMed_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_result <- apply(posteriorFit_result, 2, function(x){quantile(x, 0.975)})

## Test ----
test_result <- modelData$result

set.seed(52)
posteriorPred_result <- posterior_predict(
  fit_result,
  resp = "result",
  newdata = modelData,
  re_formula = NULL
)
posteriorPredMean_result <- colMeans(posteriorPred_result)
posteriorPredMed_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_result <- apply(posteriorPred_result, 2, function(x){quantile(x, 0.975)})

## LOO ----
loo_result_4 <- loo(fit_result4)
loo_result_5 <- loo(fit_result5)
loo_result_6 <- loo(fit_result6)
loo_result_7 <- loo(fit_result7)
loo_result_8 <- loo(fit_result8)
loo_result_9 <- loo(fit_result9)
loo_result_10 <- loo(fit_result10)
loo_result_11 <- loo(fit_result11)
loo_result_12 <- loo(fit_result12)
loo_result_20 <- loo(fit_result20)
loo_result_21 <- loo(fit_result21)
loo_result_22 <- loo(fit_result22)
loo_result_23 <- loo(fit_result23)
loo_result_24 <- loo(fit_result24)
loo_result_25 <- loo(fit_result25)
loo_result_26 <- loo(fit_result26)
loo_result_list <- list(
  loo_result_4,
  loo_result_5,
  loo_result_6,
  loo_result_7,
  loo_result_8,
  loo_result_9,
  loo_result_10,
  loo_result_11,
  loo_result_12,
  loo_result_20,
  loo_result_21,
  loo_result_22,
  loo_result_23,
  loo_result_24,
  loo_result_25,
  loo_result_26
)

loo_compare_result <- loo_compare(loo_result_list)
loo_compare_result

# Goodness of Fit ##########################################################
## PPC ----
set.seed(52)
sampFitID <- sample(1:sims, 200, replace = FALSE)
posteriorFitSamp_result <- posteriorFit_result[sampFitID, ]

fillPPC <- "#d1e1ec"
colorPPC <- "#b3cde0"
fill2PPC <- "#011f4b"

### Bars ----
# ppcBarsPlot_result <- ppc_bars(
#   y = train_result,
#   yrep = posteriorFitSamp_result
# ) +
#   labs(
#     # title = "Simulated density of draws from the PPD vs Observed VMAX",
#     # subtitle = "n = 1000 draws",
#     x = "result",
#     y = "Density"
#   ) +
#   #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
#   theme_bw() +
#   theme(
#     legend.position = "none"
#   )

### Density -----
ppcDensPlot_result <- ppc_dens_overlay(
  y = train_result,
  yrep = posteriorFitSamp_result
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "result",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

#### Stats ----
# Make stat functions
meanFunc <- function(y){mean(y)}
sdFunc <- function(y){sd(y)}
rangeFunc <- function(y){max(y) - min(y)}

##### Mean ----
set.seed(52) # for reproducibility
mean_result <- meanFunc(train_result)

ppcMeanStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_result,
    meanProbHigh = value > mean_result
  )

ppcMeanPlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Mean",
       subtitle = paste("p-value =", round(mean(ppcMeanStat_result$meanProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### SD ----
set.seed(52) # for reproducibility
sd_result <- sdFunc(train_result)

ppcSDStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_result,
    sdProbHigh = value > sd_result
  )

ppcSDPlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcSDStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "SD",
       subtitle = paste("p-value =", round(mean(ppcSDStat_result$sdProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Range ----
set.seed(52) # for reproducibility
range_result <- rangeFunc(train_result)

ppcRangeStat_result <- ppc_stat_data(
  y = train_result,
  yrep = posteriorFit_result,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_result,
    rangeProbHigh = value > range_result
  )

ppcRangePlotGG_result <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_result |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_result |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "result"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Range",
       subtitle = paste("p-value =", round(mean(ppcRangeStat_result$rangeProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

#### Combined plot ----
ppcCombPlot_result <- 
  #(ppcBarsPlot_result + ppcDensPlot_result) /
  (ppcDensPlot_result) /
  (ppcMeanPlotGG_result + ppcSDPlotGG_result + ppcRangePlotGG_result) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcCombPlot_result

# Performance Metrics ----
## Training ----
### MAE
MAE_fit_result <- mean(abs(posteriorFitMean_result - train_result))

### MAD
MAD_fit_result <- mean(abs(posteriorFitMed_result - train_result))

### RMSE
RMSE_fit_result <- sqrt(mean(posteriorFitMean_result - train_result)^2)

### COV
COV_fit_result <- mean(posteriorFitLCB_result < train_result &  
                         train_result < posteriorFitUCB_result)

## Test ----
### MAE
MAE_pred_result <- mean(abs(posteriorPredMean_result - test_result))

### MAD
MAD_pred_result <- mean(abs(posteriorPredMed_result - test_result))

### RMSE
RMSE_pred_result <- sqrt(mean(posteriorPredMean_result - test_result)^2)

### COV
COV_pred_result <- mean(posteriorPredLCB_result < test_result &  
                          test_result < posteriorPredUCB_result)

## Compare Table ----
performance_metrics_result <- tibble(
  Fit = rep(paste0("Fit", fitResult), 1),
  Bet = c("result"), # "total"),
  MAE_fit = c(MAE_fit_result), # MAE_fit_total),
  MAD_fit = c(MAD_fit_result), # MAD_fit_total),
  RMSE_fit = c(RMSE_fit_result), # RMSE_fit_total),
  COV_fit = c(COV_fit_result), # COV_fit_total),
  MAE_pred = c(MAE_pred_result), # MAE_pred_total),
  MAD_pred = c(MAD_pred_result), # MAD_pred_total),
  RMSE_pred = c(RMSE_pred_result), # RMSE_pred_total),
  COV_pred = c(COV_pred_result) # COV_pred_total)
)
performance_metrics_result

# Betting #######################################################
## Training ----
train_result_bet_df <- histModelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    result, spread_line,
    home_spread_prob, away_spread_prob,
    home_spread_odds, away_spread_odds
  ) |>
  mutate(
    diff = result - spread_line,
    actual_cover = case_when(
      result > spread_line ~ "Home",
      result < spread_line ~ "Away",
      .default = NA
    ),
    .after = spread_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_result = posteriorFitMean_result,
    exp_spread_line = spread_line,
    exp_diff = exp_result - exp_spread_line,
    exp_cover = case_when(
      exp_result > spread_line ~ "Home",
      exp_result < spread_line ~ "Away",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(xgb_home_model, newdata = histModelData),
    xgb_away_score = predict(xgb_away_model, newdata = histModelData),
    xgb_result = xgb_home_score - xgb_away_score,
    #xgb_result2 = predict(xgb_result_model, newdata = histModelData),
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_result2 = predict(xgb_result_model, newdata = histModelData),
    xgb_spread_line2 = spread_line,
    xgb_diff2 = xgb_result2 - xgb_spread_line2,
    xgb_cover2 = case_when(
      xgb_result2 > spread_line ~ "Home",
      xgb_result2 < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover2,
    .after = xgb_correct_cover
  )

### Accuracy ----
train_result_spread_line <- train_result_bet_df$spread_line
train_result_actual_cover <- train_result_bet_df$actual_cover
train_vegas_home_prob <- train_result_bet_df$home_spread_prob
train_vegas_away_prob <- train_result_bet_df$away_spread_prob
train_result_thresh <- 0.6

#### Posterior Means ----
table(train_result_bet_df$correct_cover, useNA = "ifany")

train_result_acc_prob_posterior_mean <-
  mean(train_result_bet_df$correct_cover, na.rm = TRUE)*100
train_result_acc_prob_posterior_mean

#### Full Posterior ----
train_result_bet_decision <- 
  sweep(
    posteriorFit_result, 2, train_result_spread_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Home",
             ifelse(pred < line, "Away", NA))
    })

train_result_comparison_matrix <- 
  sweep(
    train_result_bet_decision, 2, train_result_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

train_result_game_accuracy <- 
  colMeans(train_result_comparison_matrix, na.rm = TRUE) 

train_result_acc_prob_full_posterior <- 
  mean(train_result_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
train_result_home_prob <- 
  colMeans(train_result_bet_decision == "Home", na.rm = TRUE)
train_result_away_prob <- 
  colMeans(train_result_bet_decision == "Away", na.rm = TRUE)

# Decision vector
train_result_bet_side_vegas <- 
  ifelse(train_result_home_prob > train_vegas_home_prob, "Home",
         ifelse(train_result_away_prob > train_vegas_away_prob, "Away", NA))

# Compare to actual outcome
train_result_bet_vegas_correct <- 
  ifelse(is.na(train_result_bet_side_vegas) | is.na(train_result_actual_cover), 
         NA, train_result_bet_side_vegas == train_result_actual_cover)

# Accuracy over placed bets
train_result_bet_vegas_acc <- 
  mean(train_result_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
train_result_bet_vegas_count <- sum(!is.na(train_result_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
train_result_bet_side_thresh <- 
  ifelse(train_result_home_prob > train_result_thresh, "Home",
         ifelse(train_result_away_prob > train_result_thresh, "Away", NA))

# Compare to actual outcome
train_result_bet_thresh_correct <- 
  ifelse(is.na(train_result_bet_side_thresh) | is.na(train_result_actual_cover), 
         NA, train_result_bet_side_thresh == train_result_actual_cover)

# Accuracy over placed bets
train_result_bet_thresh_acc <- 
  mean(train_result_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
train_result_bet_thresh_count <- sum(!is.na(train_result_bet_thresh_correct))

#### XGB Prediction ----
table(train_result_bet_df$xgb_correct_cover, useNA = "ifany")

train_result_acc_prob_xgb <-
  mean(train_result_bet_df$xgb_correct_cover, na.rm = TRUE)*100
train_result_acc_prob_xgb

table(train_result_bet_df$xgb_correct_cover2, useNA = "ifany")

train_result_acc_prob_xgb2 <-
  mean(train_result_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
train_result_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(train_result_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(train_result_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(train_result_bet_vegas_acc, 2), "%",
            "on", train_result_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(train_result_bet_thresh_acc, 2), "%",
            "on", train_result_bet_thresh_count, "bets",
            "with", train_result_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_result_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_result_acc_prob_xgb2, 2), "%"))

## Test ----

games24 <- which(agg_result$game_info$season == 2024)
posteriorPred_result <- agg_result$posterior[,games24]
posteriorPredMean_result <- colMeans(posteriorPred_result)

test_result_bet_df <- brms_data |> #modelData |>
  filter(season == 2024) |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    result, spread_line,
    xgb_home_score, xgb_away_score, xgb_result, xgb_total,
    home_spread_prob, away_spread_prob,
    home_spread_odds, away_spread_odds
  ) |>
  mutate(
    diff = result - spread_line,
    actual_cover = case_when(
      result > spread_line ~ "Home",
      result < spread_line ~ "Away",
      .default = NA
    ),
    .after = spread_line
  ) |>
  mutate(
    #exp_home_score = posteriorPredMean_home,
    #exp_away_score = posteriorPredMean_away,
    exp_result = posteriorPredMean_result,
    exp_spread_line = spread_line,
    exp_diff = exp_result - exp_spread_line,
    exp_cover = case_when(
      exp_result > spread_line ~ "Home",
      exp_result < spread_line ~ "Away",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    #xgb_home_score = predict(home_model, newdata = modelData),
    #xgb_away_score = predict(away_model, newdata = modelData),
    #xgb_result = xgb_home_score - xgb_away_score,
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) #|>
  # mutate(
  #   xgb_result2 = predict(xgb_result_model, newdata = modelData),
  #   xgb_spread_line2 = spread_line,
  #   xgb_diff2 = xgb_result2 - xgb_spread_line2,
  #   xgb_cover2 = case_when(
  #     xgb_result2 > spread_line ~ "Home",
  #     xgb_result2 < spread_line ~ "Away",
  #     .default = NA
  #   ),
  #   xgb_correct_cover2 = actual_cover == xgb_cover2,
  #   .after = xgb_correct_cover
  # )

### Accuracy ----
test_result_spread_line <- test_result_bet_df$spread_line
test_result_actual_cover <- test_result_bet_df$actual_cover
test_vegas_home_prob <- test_result_bet_df$home_spread_prob
test_vegas_away_prob <- test_result_bet_df$away_spread_prob
test_result_thresh <- 0.6

#### Posterior Means ----
table(test_result_bet_df$correct_cover, useNA = "ifany")

test_result_acc_prob_posterior_mean <-
  mean(test_result_bet_df$correct_cover, na.rm = TRUE)*100
test_result_acc_prob_posterior_mean

#### Full Posterior ----
test_result_bet_decision <- 
  sweep(
    posteriorPred_result, 2, test_result_spread_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Home",
             ifelse(pred < line, "Away", NA))
    })

test_result_comparison_matrix <- 
  sweep(
    test_result_bet_decision, 2, test_result_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

test_result_game_accuracy <- 
  colMeans(test_result_comparison_matrix, na.rm = TRUE) 

test_result_acc_prob_full_posterior <- 
  mean(test_result_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
test_result_home_prob <- 
  colMeans(test_result_bet_decision == "Home", na.rm = TRUE)
test_result_away_prob <- 
  colMeans(test_result_bet_decision == "Away", na.rm = TRUE)

# Decision vector
test_result_bet_side_vegas <- 
  ifelse(test_result_home_prob > test_vegas_home_prob, "Home",
         ifelse(test_result_away_prob > test_vegas_away_prob, "Away", NA))

# Compare to actual outcome
test_result_bet_vegas_correct <- 
  ifelse(is.na(test_result_bet_side_vegas) | is.na(test_result_actual_cover), 
         NA, test_result_bet_side_vegas == test_result_actual_cover)

# Accuracy over placed bets
test_result_bet_vegas_acc <- 
  mean(test_result_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
test_result_bet_vegas_count <- sum(!is.na(test_result_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
test_result_bet_side_thresh <- 
  ifelse(test_result_home_prob > test_result_thresh, "Home",
         ifelse(test_result_away_prob > test_result_thresh, "Away", NA))

# Compare to actual outcome
test_result_bet_thresh_correct <- 
  ifelse(is.na(test_result_bet_side_thresh) | is.na(test_result_actual_cover), 
         NA, test_result_bet_side_thresh == test_result_actual_cover)

# Accuracy over placed bets
test_result_bet_thresh_acc <- 
  mean(test_result_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
test_result_bet_thresh_count <- sum(!is.na(test_result_bet_thresh_correct))

#### XGB Prediction ----
table(test_result_bet_df$xgb_correct_cover, useNA = "ifany")

test_result_acc_prob_xgb <-
  mean(test_result_bet_df$xgb_correct_cover, na.rm = TRUE)*100
test_result_acc_prob_xgb

# table(test_result_bet_df$xgb_correct_cover2, useNA = "ifany")
# 
# test_result_acc_prob_xgb2 <-
#   mean(test_result_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
# test_result_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(test_result_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(test_result_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(test_result_bet_vegas_acc, 2), "%",
            "on", test_result_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(test_result_bet_thresh_acc, 2), "%",
            "on", test_result_bet_thresh_count, "bets",
            "with", test_result_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_result_acc_prob_xgb, 2), "%"))
# print(paste("Model Accuracy Percent XGBoost:", 
#             round(test_result_acc_prob_xgb2, 2), "%"))

## Comparison Table ----
accuracy_metrics_result_temp <- tibble(
  Fit = "Fit 1", # rep(paste0("Fit", fitResult), 2),
  Data = "Test", # rep(c("Train", "Test"), each = 1),
  Bet = "result", #rep(c("result"), 2), # "total"), times = 1),
  PostMean = c(#train_result_acc_prob_posterior_mean,
               #train_total_acc_prob_posterior_mean,
               test_result_acc_prob_posterior_mean
               #test_total_acc_prob_posterior_mean
  ),
  PostFull = c(#train_result_acc_prob_full_posterior,
               #train_total_acc_prob_full_posterior,
               test_result_acc_prob_full_posterior
               #test_total_acc_prob_full_posterior
  ),
  BetVegas = c(#train_result_bet_vegas_acc,
               #train_total_bet_vegas_acc,
               test_result_bet_vegas_acc
               #test_total_bet_vegas_acc
  ),
  BetVegasCount = c(#train_result_bet_vegas_count,
                    #train_total_bet_vegas_count,
                    test_result_bet_vegas_count
                    #test_total_bet_vegas_count
  ),
  BetThresh = c(#train_result_bet_thresh_acc,
                #train_total_bet_thresh_acc,
                test_result_bet_thresh_acc
                #test_total_bet_thresh_acc
  ),
  ThreshPerc = c(#train_result_thresh,
                 #train_total_thresh,
                 test_result_thresh
                 #test_total_thresh
  ),
  BetThreshCount = c(#train_result_bet_thresh_count,
                     #train_total_bet_thresh_count,
                     test_result_bet_thresh_count
                     #test_total_bet_thresh_count
  ),
  XGB = c(#train_result_acc_prob_xgb,
          #train_total_acc_prob_xgb,
          test_result_acc_prob_xgb
          #test_total_acc_prob_xgb
  )
)
accuracy_metrics_result <- bind_rows(
  accuracy_metrics_result_temp,
  accuracy_metrics_result
)

accuracy_metrics_result #<- accuracy_metrics_result_temp

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# TOTAL ----------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

## Model ----
formula_total <- bf(
  total|trunc(lb = 3) ~
    xgb_pred_total2 +
    # home_off_pass_epa_roll + 
    # home_off_net_epa_roll +
    # home_pass_net_epa_roll +
    # home_pass_net_epa_cum + 
    # home_off_net_epa_cum +
    # home_off_pass_epa_cum + 
    # home_off_epa_cum + 
    # away_MOV_roll_net +
    # home_off_epa_roll + 
    # home_OSRS_roll +
    # home_net_epa_cum + 
    # away_MOV_net + 
    # home_PFG_roll + 
    # home_MOV + 
    # home_offTD_roll_5 +
    # home_PFG +
    # home_def_rush_plays_cum + 
    # home_SRS +
    # away_net_epa_roll + 
    # away_SRS_roll_net + 
    # away_off_net_epa_cum + 
    # away_pass_net_epa_cum +
    # away_OSRS_roll_net + 
    # away_off_epa_cum +
    # away_off_net_epa_roll + 
    # away_off_pass_epa_cum +
    # away_MOV_roll_net +
    # away_PFG_roll +
    # away_OSRS_net + 
    # away_SRS_roll_net + 
    # away_off_epa_roll + 
    # away_pass_net_epa_roll + 
    # away_off_pass_epa_roll + 
    # home_def_pass_epa_cum + 
    # home_PAG + 
    # away_SRS_net +
    # away_OSRS_roll +
    # away_MOV_roll + 
    # away_net_epa_cum +
    # home_net_epa_cum +
    div_game + 
    roof + 
    #surface + 
    temp + 
    wind + 
    (1 | home_team) +
    (1 | away_team)
) + 
  brmsfamily(family = "student", link = "identity")
  #brmsfamily(family = "shifted_lognormal", link = "identity")
  #brmsfamily(family = "skew_normal", link = "identity")
  #mixture(brmsfamily(family = "poisson", link = "log"), nmix = 3)
  #brmsfamily(family = "negbinomial", link = "log")

default_prior(formula_total, histModelData)

priors_total <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b"),
  prior(inv_gamma(0.1, 0.1), class = "sigma"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(inv_gamma(0.1, 0.1), class = "sd")
)

system.time(
  fit_total <- brm(
    formula_total,
    data = histModelData,
    #prior = priors_Team,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    #init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)

print(fit_total, digits = 4)

#fit_Team <- fit_Team2
#pp_check(fit_total, resp = "total", ndraws = 100, type = "bars")
pp_check(fit_total, resp = "total", ndraws = 100, type = "dens_overlay")

# pp_check(fit_total, newdata = modelData,
#          resp = "total", ndraws = 100, type = "bars")
pp_check(fit_total, newdata = modelData,
         resp = "total", ndraws = 100, type = "dens_overlay")

#save(fit_total, file = "~/Desktop/NFL Analysis Data/fit_total.RData")

### Fixed Effects ----
fixedEff_total<- fixef(fit_total)
fixedEff_total <- data.frame(fixedEff_total) |>
  mutate(
    p_val = dnorm(Estimate/Est.Error)
  ) |>
  mutate(
    across(everything(), function(x){round(x, 4)})
  ) |>
  mutate(
    Sig = ifelse(p_val < 0.01, "***",
                 ifelse(p_val < 0.05, "**",
                        ifelse(p_val < 0.1, "*", "")))
  )
print(fixedEff_total, digits = 4)
fixedSigEff_total <- fixedEff_total |> filter(p_val < 0.2)
print(fixedSigEff_total)

randEff_total <- ranef(fit_total)
randEff_total

### MAE ----
fitResiduals_total <- 
  residuals(
    fit_total,
    resp = "total",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_total$Estimate))

predResiduals_total <- 
  residuals(
    fit_total,
    resp = "total",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_total$Estimate))

fitTotal <- 5
assign(paste0("fit_total", fitTotal), fit_total)
assign(paste0("fixedEff_total", fitTotal), fixedEff_total)
assign(paste0("randEff_total", fitTotal), randEff_total)

save(fit_total, 
     file = paste0("~/Desktop/NFL Analysis Data/fit_total",
                   fitTotal,
                   ".RData")
)

# Posteriors ----
## Training ----
train_total <- histModelData$total

posteriorFit_total <- posterior_predict(
  fit_total,
  resp = "total"
)
posteriorFitMean_total <- colMeans(posteriorFit_total)
posteriorFitMed_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_total <- apply(posteriorFit_total, 2, function(x){quantile(x, 0.975)})

## Test ----
test_total <- modelData$total

set.seed(52)
posteriorPred_total <- posterior_predict(
  fit_total,
  resp = "total",
  newdata = modelData,
  re_formula = NULL
)
posteriorPredMean_total <- colMeans(posteriorPred_total)
posteriorPredMed_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_total <- apply(posteriorPred_total, 2, function(x){quantile(x, 0.975)})

## LOO ----
loo_total_1 <- loo(fit_total1)
loo_total_2 <- loo(fit_total2)
loo_total_3 <- loo(fit_total3)
loo_total_4 <- loo(fit_total4)
loo_total_5 <- loo(fit_total5)
loo_total_6 <- loo(fit_total6)
loo_total_7 <- loo(fit_total7)
loo_total_8 <- loo(fit_total8)
loo_total_9 <- loo(fit_total9)
loo_total_10 <- loo(fit_total10)
loo_total_11 <- loo(fit_total11)
loo_total_12 <- loo(fit_total12)
loo_total_20 <- loo(fit_total20)
loo_total_21 <- loo(fit_total21)
loo_total_22 <- loo(fit_total22)
loo_total_23 <- loo(fit_total23)
loo_total_24 <- loo(fit_total24)
loo_total_25 <- loo(fit_total25)
loo_total_26 <- loo(fit_total26)
loo_total_list <- list(
  loo_total_1,
  loo_total_2,
  loo_total_3,
  loo_total_4,
  loo_total_5
  # loo_total_6,
  # loo_total_7,
  # loo_total_8,
  # loo_total_9,
  # loo_total_10,
  # loo_total_11,
  # loo_total_12,
  # loo_total_20,
  # loo_total_21,
  # loo_total_22,
  # loo_total_23,
  # loo_total_24,
  # loo_total_25,
  # loo_total_26
)

loo_compare_total <- loo_compare(loo_total_list)
loo_compare_total

# Goodness of Fit ##########################################################
## PPC ----
set.seed(52)
sampFitID <- sample(1:sims, 200, replace = FALSE)
posteriorFitSamp_total <- posteriorFit_total[sampFitID, ]

fillPPC <- "#d1e1ec"
colorPPC <- "#b3cde0"
fill2PPC <- "#011f4b"

### Bars ----
# ppcBarsPlot_total <- ppc_bars(
#   y = train_total,
#   yrep = posteriorFitSamp_total
# ) +
#   labs(
#     # title = "Simulated density of draws from the PPD vs Observed VMAX",
#     # subtitle = "n = 1000 draws",
#     x = "total",
#     y = "Density"
#   ) +
#   #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
#   theme_bw() +
#   theme(
#     legend.position = "none"
#   )

### Density -----
ppcDensPlot_total <- ppc_dens_overlay(
  y = train_total,
  yrep = posteriorFitSamp_total
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "total",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


#### Stats ----
# Make stat functions
meanFunc <- function(y){mean(y)}
sdFunc <- function(y){sd(y)}
rangeFunc <- function(y){max(y) - min(y)}
minFunc <- function(y){min(y)}
maxFunc <- function(y){max(y)}

##### Mean ----
set.seed(52) # for reproducibility
mean_total <- meanFunc(train_total)

ppcMeanStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_total,
    meanProbHigh = value > mean_total
  )

ppcMeanPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Mean",
       subtitle = paste("p-value =", round(mean(ppcMeanStat_total$meanProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### SD ----
set.seed(52) # for reproducibility\
sd_total <- sdFunc(train_total)

ppcSDStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_total,
    sdProbHigh = value > sd_total
  )

ppcSDPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcSDStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "SD",
       subtitle = paste("p-value =", round(mean(ppcSDStat_total$sdProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Range ----
set.seed(52) # for reproducibility
range_total <- rangeFunc(train_total)

ppcRangeStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_total,
    rangeProbHigh = value > range_total
  )

ppcRangePlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Range",
       subtitle = paste("p-value =", round(mean(ppcRangeStat_total$rangeProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Min ----
set.seed(52) # for reproducibility
min_total <- minFunc(train_total)

ppcMinStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("minFunc")
) |>
  mutate(
    minProbLow = value < min_total,
    minProbHigh = value > min_total
  )

ppcMinPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMinStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMinStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Min",
       subtitle = paste("p-value =", round(mean(ppcMinStat_total$minProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

##### Max ----
set.seed(52) # for reproducibility
max_total <- maxFunc(train_total)

ppcMaxStat_total <- ppc_stat_data(
  y = train_total,
  yrep = posteriorFit_total,
  group = NULL,
  stat = c("maxFunc")
) |>
  mutate(
    maxProbLow = value < max_total,
    maxProbHigh = value > max_total
  )

ppcMaxPlotGG_total <- ggplot() +
  geom_histogram(
    data = ppcMaxStat_total |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMaxStat_total |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "total"
  ) +
  scale_y_continuous(
    name = "Number of Posterior Draws",
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_color_manual(
    name = "Data",
    values = c(
      "Posterior" = colorPPC,
      "Observed" = "black"
    ),
    breaks = c("Posterior", "Observed")
  ) +
  labs(title = "Max",
       subtitle = paste("p-value =", round(mean(ppcMaxStat_total$maxProbLow[-1]), 4))
  ) +
  theme_bw() +
  guides(color = guide_legend(byrow = TRUE)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.spacing.y = unit(5, "points"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

#### Combined plot ----
ppcCombPlot_total <- 
  #(ppcBarsPlot_total + ppcDensPlot_total) /
  (ppcDensPlot_total) /
  (ppcMeanPlotGG_total + ppcSDPlotGG_total + ppcRangePlotGG_total) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcCombPlot_total

ppcMinMaxPlot_total <- 
  (ppcMinPlotGG_total + ppcMaxPlotGG_total) +
  plot_layout(
    guides = "collect",
    axes = "collect_x"
  ) +
  plot_annotation(
    #title = "Posterior Predictive Checks for Distributional Statistics",
    #subtitle = paste("Bayesian predictive p-values for", sims, "Simulations"),
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  )
ppcMinMaxPlot_total

# Performance Metrics ----
## Training ----
### MAE
MAE_fit_total <- mean(abs(posteriorFitMean_total - train_total))

### MAD
MAD_fit_total <- mean(abs(posteriorFitMed_total - train_total))

### RMSE
RMSE_fit_total <- sqrt(mean(posteriorFitMean_total - train_total)^2)

### COV
COV_fit_total <- mean(posteriorFitLCB_total < train_total &  
                        train_total < posteriorFitUCB_total)

## Test ----
### MAE
MAE_pred_total <- mean(abs(posteriorPredMean_total - test_total))

### MAD
MAD_pred_total <- mean(abs(posteriorPredMed_total - test_total))

### RMSE
RMSE_pred_total <- sqrt(mean(posteriorPredMean_total - test_total)^2)

### COV
COV_pred_total <- mean(posteriorPredLCB_total < test_total &  
                         test_total < posteriorPredUCB_total)

## Compare Table ----
performance_metrics_total <- tibble(
  Fit = rep(paste0("Fit", fitTotal), 1),
  Bet = c("total"),
  MAE_fit = c(MAE_fit_total),
  MAD_fit = c(MAD_fit_total),
  RMSE_fit = c(RMSE_fit_total),
  COV_fit = c(COV_fit_total),
  MAE_pred = c(MAE_pred_total),
  MAD_pred = c(MAD_pred_total),
  RMSE_pred = c(RMSE_pred_total),
  COV_pred = c(COV_pred_total)
)
performance_metrics_total

# Betting #######################################################
## Training ----
train_total_bet_df <- histModelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    total, total_line,
    over_prob, under_prob,
    over_odds, under_odds
  ) |>
  mutate(
    diff = total - total_line,
    actual_cover = case_when(
      total > total_line ~ "Over",
      total < total_line ~ "Under",
      .default = NA
    ),
    .after = total_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_total = posteriorFitMean_total,
    exp_total_line = total_line,
    exp_diff = exp_total - exp_total_line,
    exp_cover = case_when(
      exp_total > total_line ~ "Over",
      exp_total < total_line ~ "Under",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(home_model, newdata = histModelData),
    xgb_away_score = predict(away_model, newdata = histModelData),
    xgb_total = xgb_home_score + xgb_away_score,
    xgb_total_line = total_line,
    xgb_diff = xgb_total - xgb_total_line,
    xgb_cover = case_when(
      xgb_total > total_line ~ "Over",
      xgb_total < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_total2 = predict(xgb_total_model, newdata = histModelData),
    xgb_total_line2 = total_line,
    xgb_diff2 = xgb_total2 - xgb_total_line2,
    xgb_cover2 = case_when(
      xgb_total2 > total_line ~ "Over",
      xgb_total2 < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover2,
    .after = xgb_correct_cover
  )

### Accuracy ----
train_total_total_line <- train_total_bet_df$total_line
train_total_actual_cover <- train_total_bet_df$actual_cover
train_vegas_over_prob <- train_total_bet_df$over_prob
train_vegas_under_prob <- train_total_bet_df$under_prob
train_total_thresh <- 0.6

#### Posterior Means ----
table(train_total_bet_df$correct_cover, useNA = "ifany")

train_total_acc_prob_posterior_mean <-
  mean(train_total_bet_df$correct_cover, na.rm = TRUE)*100
train_total_acc_prob_posterior_mean

#### Full Posterior ----
train_total_bet_decision <- 
  sweep(
    posteriorFit_total, 2, train_total_total_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Over",
             ifelse(pred < line, "Under", NA))
    })

train_total_comparison_matrix <- 
  sweep(
    train_total_bet_decision, 2, train_total_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

train_total_game_accuracy <- 
  colMeans(train_total_comparison_matrix, na.rm = TRUE) 

train_total_acc_prob_full_posterior <- 
  mean(train_total_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
train_total_over_prob <- 
  colMeans(train_total_bet_decision == "Over", na.rm = TRUE)
train_total_under_prob <- 
  colMeans(train_total_bet_decision == "Under", na.rm = TRUE)

# Decision vector
train_total_bet_side_vegas <- 
  ifelse(train_total_over_prob > train_vegas_over_prob, "Over",
         ifelse(train_total_under_prob > train_vegas_under_prob, "Under", NA))

# Compare to actual outcome
train_total_bet_vegas_correct <- 
  ifelse(is.na(train_total_bet_side_vegas) | is.na(train_total_actual_cover), 
         NA, train_total_bet_side_vegas == train_total_actual_cover)

# Accuracy over placed bets
train_total_bet_vegas_acc <- 
  mean(train_total_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
train_total_bet_vegas_count <- sum(!is.na(train_total_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
train_total_bet_side_thresh <- 
  ifelse(train_total_over_prob > train_total_thresh, "Over",
         ifelse(train_total_under_prob > train_total_thresh, "Under", NA))

# Compare to actual outcome
train_total_bet_thresh_correct <- 
  ifelse(is.na(train_total_bet_side_thresh) | is.na(train_total_actual_cover), 
         NA, train_total_bet_side_thresh == train_total_actual_cover)

# Accuracy over placed bets
train_total_bet_thresh_acc <- 
  mean(train_total_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
train_total_bet_thresh_count <- sum(!is.na(train_total_bet_thresh_correct))

#### XGB Prediction ----
table(train_total_bet_df$xgb_correct_cover, useNA = "ifany")

train_total_acc_prob_xgb <-
  mean(train_total_bet_df$xgb_correct_cover, na.rm = TRUE)*100
train_total_acc_prob_xgb

table(train_total_bet_df$xgb_correct_cover2, useNA = "ifany")

train_total_acc_prob_xgb2 <-
  mean(train_total_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
train_total_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(train_total_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(train_total_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(train_total_bet_vegas_acc, 2), "%",
            "on", train_total_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(train_total_bet_thresh_acc, 2), "%",
            "on", train_total_bet_thresh_count, "bets",
            "with", train_total_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_total_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(train_total_acc_prob_xgb2, 2), "%"))

## Test ----
test_total_bet_df <- modelData |>
  select(
    game_id, season, season_type, week,
    home_team, away_team, 
    home_score, away_score,
    total, total_line,
    over_prob, under_prob,
    over_odds, under_odds
  ) |>
  mutate(
    diff = total - total_line,
    actual_cover = case_when(
      total > total_line ~ "Over",
      total < total_line ~ "Under",
      .default = NA
    ),
    .after = total_line
  ) |>
  mutate(
    #exp_home_score = posteriorFitMean_home,
    #exp_away_score = posteriorFitMean_away,
    exp_total = posteriorPredMean_total,
    exp_total_line = total_line,
    exp_diff = exp_total - exp_total_line,
    exp_cover = case_when(
      exp_total > total_line ~ "Over",
      exp_total < total_line ~ "Under",
      .default = NA
    ),
    correct_cover = actual_cover == exp_cover,
    .after = actual_cover
  ) |>
  mutate(
    xgb_home_score = predict(home_model, newdata = modelData),
    xgb_away_score = predict(away_model, newdata = modelData),
    xgb_total = xgb_home_score + xgb_away_score,
    xgb_total_line = total_line,
    xgb_diff = xgb_total - xgb_total_line,
    xgb_cover = case_when(
      xgb_total > total_line ~ "Over",
      xgb_total < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  ) |>
  mutate(
    xgb_total2 = predict(xgb_total_model, newdata = modelData),
    xgb_total_line2 = total_line,
    xgb_diff2 = xgb_total2 - xgb_total_line2,
    xgb_cover2 = case_when(
      xgb_total2 > total_line ~ "Over",
      xgb_total2 < total_line ~ "Under",
      .default = NA
    ),
    xgb_correct_cover2 = actual_cover == xgb_cover,
    .after = xgb_correct_cover
  )

### Accuracy ----
test_total_total_line <- test_total_bet_df$total_line
test_total_actual_cover <- test_total_bet_df$actual_cover
test_vegas_over_prob <- test_total_bet_df$over_prob
test_vegas_under_prob <- test_total_bet_df$under_prob
test_total_thresh <- 0.6

#### Posterior Means ----
table(test_total_bet_df$correct_cover, useNA = "ifany")

test_total_acc_prob_posterior_mean <-
  mean(test_total_bet_df$correct_cover, na.rm = TRUE)*100
test_total_acc_prob_posterior_mean

#### Full Posterior ----
test_total_bet_decision <- 
  sweep(
    posteriorPred_total, 2, test_total_total_line, 
    FUN = function(pred, line) {
      ifelse(pred > line, "Over",
             ifelse(pred < line, "Under", NA))
    })

test_total_comparison_matrix <- 
  sweep(
    test_total_bet_decision, 2, test_total_actual_cover, 
    FUN = function(pred, actual) {
      ifelse(is.na(pred) | is.na(actual), NA, pred == actual)
    })

test_total_game_accuracy <- 
  colMeans(test_total_comparison_matrix, na.rm = TRUE) 

test_total_acc_prob_full_posterior <- 
  mean(test_total_game_accuracy, na.rm = TRUE)*100

#### Vegas Prob -----
# Compute % of Home and Away bets per game from posterior
test_total_over_prob <- 
  colMeans(test_total_bet_decision == "Over", na.rm = TRUE)
test_total_under_prob <- 
  colMeans(test_total_bet_decision == "Under", na.rm = TRUE)

# Decision vector
test_total_bet_side_vegas <- 
  ifelse(test_total_over_prob > test_vegas_over_prob, "Over",
         ifelse(test_total_under_prob > test_vegas_under_prob, "Under", NA))

# Compare to actual outcome
test_total_bet_vegas_correct <- 
  ifelse(is.na(test_total_bet_side_vegas) | is.na(test_total_actual_cover), 
         NA, test_total_bet_side_vegas == test_total_actual_cover)

# Accuracy over placed bets
test_total_bet_vegas_acc <- 
  mean(test_total_bet_vegas_correct, na.rm = TRUE) * 100
# How many bets were made
test_total_bet_vegas_count <- sum(!is.na(test_total_bet_vegas_correct))

#### Threshold Prob ----
# Decision vector
test_total_bet_side_thresh <- 
  ifelse(test_total_over_prob > test_total_thresh, "Over",
         ifelse(test_total_under_prob > test_total_thresh, "Under", NA))

# Compare to actual outcome
test_total_bet_thresh_correct <- 
  ifelse(is.na(test_total_bet_side_thresh) | is.na(test_total_actual_cover), 
         NA, test_total_bet_side_thresh == test_total_actual_cover)

# Accuracy over placed bets
test_total_bet_thresh_acc <- 
  mean(test_total_bet_thresh_correct, na.rm = TRUE) * 100
# How many bets were made
test_total_bet_thresh_count <- sum(!is.na(test_total_bet_thresh_correct))

#### XGB Prediction ----
table(test_total_bet_df$xgb_correct_cover, useNA = "ifany")

test_total_acc_prob_xgb <-
  mean(test_total_bet_df$xgb_correct_cover, na.rm = TRUE)*100
test_total_acc_prob_xgb

table(test_total_bet_df$xgb_correct_cover2, useNA = "ifany")

test_total_acc_prob_xgb2 <-
  mean(test_total_bet_df$xgb_correct_cover2, na.rm = TRUE)*100
test_total_acc_prob_xgb2

### Compare ----
# Output
print(paste("Model Accuracy Percent Posterior Mean:", 
            round(test_total_acc_prob_posterior_mean, 2), "%"))
print(paste("Model Accuracy Percent Posterior Full:", 
            round(test_total_acc_prob_full_posterior, 2), "%"))
print(paste("Model Accuracy Percent Posterior Vegas:", 
            round(test_total_bet_vegas_acc, 2), "%",
            "on", test_total_bet_vegas_count, "bets"))
print(paste("Model Accuracy Percent Posterior Threshold:", 
            round(test_total_bet_thresh_acc, 2), "%",
            "on", test_total_bet_thresh_count, "bets",
            "with", test_total_thresh*100, "% threshold"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_total_acc_prob_xgb, 2), "%"))
print(paste("Model Accuracy Percent XGBoost:", 
            round(test_total_acc_prob_xgb2, 2), "%"))

## Comparison Table ----
accuracy_metrics_total_temp <- tibble(
  Fit = rep(paste0("Fit", fitTotal), 2),
  Data = rep(c("Train", "Test"), each = 1),
  Bet = rep(c("total"), times = 2),
  PostMean = c(#train_result_acc_prob_posterior_mean,
    train_total_acc_prob_posterior_mean,
    #test_result_acc_prob_posterior_mean,
    test_total_acc_prob_posterior_mean),
  PostFull = c(#train_result_acc_prob_full_posterior,
    train_total_acc_prob_full_posterior,
    #test_result_acc_prob_full_posterior,
    test_total_acc_prob_full_posterior),
  BetVegas = c(#train_result_bet_vegas_acc,
    train_total_bet_vegas_acc,
    #test_result_bet_vegas_acc,
    test_total_bet_vegas_acc),
  BetVegasCount = c(#train_result_bet_vegas_count,
    train_total_bet_vegas_count,
    #test_result_bet_vegas_count,
    test_total_bet_vegas_count),
  BetThresh = c(#train_result_bet_thresh_acc,
    train_total_bet_thresh_acc,
    #test_result_bet_thresh_acc,
    test_total_bet_thresh_acc),
  ThreshPerc = c(#train_result_thresh,
    train_total_thresh,
    #test_result_thresh,
    test_total_thresh),
  BetThreshCount = c(#train_result_bet_thresh_count,
    train_total_bet_thresh_count,
    #test_result_bet_thresh_count,
    test_total_bet_thresh_count),
  XGB = c(#train_result_acc_prob_xgb,
    train_total_acc_prob_xgb,
    #test_result_acc_prob_xgb
    test_total_acc_prob_xgb
  )
)
accuracy_metrics_total <- bind_rows(
  accuracy_metrics_total_temp,
  accuracy_metrics_total
)

accuracy_metrics_total #<- accuracy_metrics_total_temp

# All Accuracy ----
accuracy_metrics_temp <-
  bind_rows(
    accuracy_metrics_result_temp,
    accuracy_metrics_total_temp
  )

accuracy_metrics_temp

accuracy_metrics <-
  bind_rows(
    accuracy_metrics_result,
    accuracy_metrics_total
  )
accuracy_metrics

save(accuracy_metrics_result,
     accuracy_metrics_total,
     accuracy_metrics,
     file = "~/Desktop/NFL Analysis Data/accuracy_metrics.RData")




