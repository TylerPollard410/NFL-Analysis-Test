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
library(MASS)
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
library(nflverse)

## Tidyverse
library(tidyverse)

# Takes about 
# system.time(
#   source("./app/data-raw/modData.R")
# )

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2023:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
class(modData$home_totalTD)
class(modData$away_totalTD)
class(modData$home_fg_made)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Previous Data ----
modData2 <- modData |> 
  #filter(!is.na(result)) |>
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
    spreadCover,
    total,
    total_line,
    totalCover,
    contains("over"),
    contains("under"),
    location,
    div_game,
    roof,
    surface,
    temp,
    wind,
    contains("home"),
    contains("away")
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
  )

modPreProcess <- modData2 |>
  filter(season >= 2022, !is.na(result)) |>
  select(
    #-game_id
    -home_score, -away_score,
    -result, -total,
    -season, -week,
    -contains("totalTD"),
    -contains("fg_made"),
    -contains("fg_att"),
    -contains("twoPtConv"),
    -contains("twoPtAtt"),
    -contains("safeties"),
    -contains("pat_made"),
    -contains("pat_att"),
    -contains("TDs"),
    -contains("spread"),
    -contains("moneyline"),
    -contains("offTD"),
    -contains("Score"),
    contains("roll"),
    contains("cum")
    # -total_line,
    # -spread_line,
    #-location,
    #-div_game,
    #-roof
  )

modPreProcValues <- preProcess(modPreProcess,
                               method = c("center", "scale", "YeoJohnson", "corr")
)
modPreProcess2 <- predict(modPreProcValues, 
                          newdata = modData2 |>
                            filter(season >= 2021, !is.na(result)))

histModelData1 <- modData2 |> 
  filter(between(season, 2022, 2023) | (season == 2024 & week <= 6))
modelData1 <- modData2 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result),
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  )

predictorData <- histModelData1 |> 
  select(
    #-game_id
    -home_score, -away_score,
    -result, -total,
    -season, -week,
    -contains("totalTD"), 
    -contains("fg_made"), 
    -contains("fg_att"),
    -contains("twoPtConv"), 
    -contains("twoPtAtt"),
    -contains("safeties"),
    -contains("pat_made"),
    -contains("pat_att"),
    -contains("TDs"),
    -contains("spread"), 
    -contains("moneyline"),
    -contains("offTD"),
    contains("roll"),
    contains("cum"),
    -contains("conversions"),
    total_line,
    -spread_line,
    -overtime
    #-location,
    #-div_game,
    #-roof
  )
preProcValues <- preProcess(predictorData,
                            method = c("center", "scale")#, "YeoJohnson")
)
preProcValues2 <- preProcess(predictorData,
                             method = c("center", "scale", "YeoJohnson")
)
preProcValues
predictorData2 <- predict(preProcValues, predictorData)
histModelData2 <- predict(preProcValues, histModelData1)
modelData2 <- predict(preProcValues, modelData1)

predictorData3 <- predict(preProcValues2, predictorData)
histModelData3 <- predict(preProcValues2, histModelData1)
modelData3 <- predict(preProcValues2, modelData1)

histModelData <- histModelData3
modelData <- modelData3

home_totalTD_range <- range(modData |> filter(!is.na(home_totalTD)) |> pull(home_totalTD))
home_fg_att_range<- range(modData |> filter(!is.na(home_fg_att)) |> pull(home_fg_att))
home_fg_made_range <- range(modData |> filter(!is.na(home_fg_made)) |> pull(home_fg_made))

away_totalTD_range <- range(modData |> filter(!is.na(away_totalTD)) |> pull(away_totalTD))
away_fg_att_range <- range(modData |> filter(!is.na(away_fg_att)) |> pull(away_fg_att))
away_fg_made_range <- range(modData |> filter(!is.na(away_fg_made)) |> pull(away_fg_made))

range_totalTD <- c(min(home_totalTD_range,away_totalTD_range), 
                   max(home_totalTD_range,away_totalTD_range))
range_fg_att_range <- c(min(home_fg_att_range,away_fg_att_range), 
                        max(home_fg_att_range,away_fg_att_range))
range_fg_made_range <- c(min(home_fg_made_range,away_fg_made_range), 
                         max(home_fg_made_range,away_fg_made_range))


## Correlations ----
totalcor <- cor(histModelData |> select(total),
                predictorData3 |> select(c(where(is.numeric))),
                #use = "pairwise.complete.obs",
                method = "kendall"
)
totalcorT <- t(totalcor)
totalcorT2 <- totalcorT[order(abs(totalcorT)),]
totalcorT2df <- data.frame(Cor = totalcorT2[order(abs(totalcorT2), decreasing = TRUE)])
totalSigCor <- totalcorT2df |> filter(abs(Cor) > .1)
totalSigCor
totalSigCor2 <- abs(totalSigCor)
totalSigCor2 <- distinct(totalSigCor2)
totalSigCor2
totalCorVars <- row.names(totalSigCor2)
which(totalCorVars == "total_line")
totalSigCor |> slice(21:nrow(totalSigCor))
totalCorMat <- corrplot::cor.mtest(modPreProcess2 |> select(c(where(is.numeric))),
                                   method = "kendall")
totalCorMat <- corrplot::cor.mtest(modPreProcess2 |> select(total,totalCorVars),
                                   method = "kendall")
totalCorPlot <- corrplot::corrplot()
totalCorMatP <- totalCorMat$p 

totalCorVarsP <- apply(modPreProcess2 |> select(c(where(is.numeric), -total)),
                       2, 
                       FUN = function(x){
                         cor.test(modPreProcess2$total, x, method = "kendall")$p.value
                       })
totalcor2 <- cor(histModelData |> select(total, totalCorVars),
                 method = "kendall")
corrplot::corrplot.mixed(totalcor2, 
                         upper = 'ellipse', 
                         order = 'hclust')


spreadcor <- cor(histModelData |> select(result),
                 histModelData |> select(c(where(is.numeric), -result)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
spreadcorT <- t(spreadcor)
spreadcorT2 <- spreadcorT[order(abs(spreadcorT)),]
spreadcorT2df <- data.frame(Cor = sort(round(abs(spreadcorT2), 4), decreasing = TRUE))


homeTDcor <- cor(histModelData |> select(home_totalTD),
                 histModelData |> select(c(where(is.numeric), -home_totalTD)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
homeTDcorT <- t(homeTDcor)
homeTDcorT2 <- homeTDcorT[order(abs(homeTDcorT)),]
homeTDcorT2df <- data.frame(sort(abs(homeTDcorT2), decreasing = TRUE))

homeFGcor <- cor(histModelData |> select(home_fg_made),
                 histModelData |> select(c(where(is.numeric), -home_fg_made)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
homeFGcorT <- t(homeFGcor)
homeFGcorT2 <- homeFGcorT[order(abs(homeFGcorT)),]
homeFGcorT2df <- data.frame(sort(abs(homeFGcorT2), decreasing = TRUE))

homeFGAcor <- cor(histModelData |> select(home_fg_att),
                  histModelData |> select(c(where(is.numeric), -home_fg_att)),
                  use = "pairwise.complete.obs",
                  method = "kendall"
)
homeFGAcorT <- t(homeFGAcor)
homeFGAcorT2 <- homeFGAcorT[order(abs(homeFGAcorT)),]
homeFGAcorT2df <- data.frame(sort(abs(homeFGAcorT2), decreasing = TRUE))

histSRSdata <- histModelData |>
  select(
    game_id, season, season_type, week,
    home_team, home_score, away_team, away_score,
    result, spread_line, spreadCover,
    contains("SRS")
  ) #|>
# mutate(
#   spreadCoverSRS = ifelse(home_SRS_net > spread_line, TRUE, FALSE),
#   spreadCoverSRSCorrect = spreadCoverSRS == spreadCover,
#   .after = spreadCover
# )

modelSRSdata <- modelData |>
  select(
    game_id, season, season_type, week,
    home_team, home_score, away_team, away_score,
    result, spread_line, spreadCover,
    contains("SRS")
  ) #|>
# mutate(
#   spreadCoverSRS = ifelse(home_SRS_net > spread_line, TRUE, FALSE),
#   spreadCoverSRSCorrect = spreadCoverSRS == spreadCover,
#   .after = spreadCover
# )

SRSdata <- bind_rows(
  histSRSdata |> mutate(split = "Train", .before = 1),
  modelSRSdata |> mutate(split = "Test", .before = 1)
) |>
  mutate(
    home_SRS_net2 = home_SRS_net + 2, 
    .after = home_SRS_net
  ) |>
  mutate(
    spreadCoverSRS = ifelse(home_SRS_net > spread_line, TRUE, FALSE),
    spreadCoverSRSCorrect = spreadCoverSRS == spreadCover,
    spreadCoverSRS2 = ifelse(home_SRS_net2 > spread_line, TRUE, FALSE),
    spreadCoverSRSCorrect2 = spreadCoverSRS2 == spreadCover,
    .after = spreadCover
  )

SRSdata |>
  filter(!is.na(spreadCover)) |>
  group_by(split) |>
  summarise(
    obsN = n(),
    homeCoverSuccess = mean(spreadCover),
    SRSCoverSuccess = mean(spreadCoverSRS),
    CoverSuccess = mean(spreadCoverSRSCorrect),
    SRSCoverSuccess2 = mean(spreadCoverSRS2),
    CoverSuccess2 = mean(spreadCoverSRSCorrect2)
  )

## Plots ----
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
ggplot(data = histModelData1) +
  geom_histogram(aes(x = home_off_scr))

##"home_net_epa_cum"             "home_net_epa_roll"           
# [124] "home_off_net_epa_cum"         "home_off_net_epa_roll"        "home_pass_net_epa_cum"       
# [127] "home_pass_net_epa_roll"       "home_rush_net_epa_cum"        "home_rush_net_epa_roll"      
# [130] "home_penalty_net_epa_cum"     "home_penalty_net_epa_roll"
histModelDataPlot <- bind_rows(
  bind_cols(
    histModelData2,
    Model = "None"
  ),
  bind_cols(
    histModelData3,
    Model = "YeoJohnson"
  )
)
histModelDataPlot$home_off_pat_pct_cum
ggplot(data = histModelDataPlot, 
       aes(x = home_off_pat_att_roll, y = total, color = Model)) +
  geom_point() +
  sm_statCorr(legends = TRUE)

# Fit historical ----
## Formulas ----
### Total ----
#### Linear ----
# ome_PFG_roll +
#   #away_PFG_roll +
#   
#   home_OSRS_roll +
#   away_OSRS_roll +
#   home_OSRS_roll_net +
#   away_OSRS_roll_net +
#   # home_OSRS_net +
#   # away_OSRS_net +
#   home_off_epa_roll +
#   home_off_epa_cum +
#   home_off_net_epa_cum + 
#   home_off_net_epa_roll +
#   home_pass_net_epa_cum +
#   home_pass_net_epa_roll +
#   home_off_rush_epa_cum +
#   home_off_rush_epa_roll +
#   
#   home_totalTD_roll +
#   home_off_pat_att_roll +
#   home_def_special_plays_cum +
#   
#   home_off_n +
#   home_off_punt +
#   home_off_td +
#   home_off_1st +
#   home_off_scr_2nd +
#   home_off_scr +
#   wind +
#   temp,

formula_total <-
  bf(
    total ~ #0 + #Intercept +
      home_offTD_roll_5 +
      home_OSRS_ewma_net +
      home_off_punt +
      home_off_epa_cum +
      home_off_rush_epa_cum +
      # home_off_net_epa_cum +
      # home_OSRS_ewma_net +
      # home_off_epa_roll +
      # home_PFG_ewma +
      # home_OSRS_roll +
      home_pat_att_roll_5 
    # home_off_net_epa_roll +
    # home_offTD_roll_5 +
    # home_OSRS_ewma +
    # home_off_punt +
    # home_PFG_roll +
    # home_OSRS_net +
    # home_off_pass_epa_roll +
    # home_OSRS +
    # home_OSRS_roll_net +
    # home_off_epa_cum +
    # home_off_1st +
    # home_totalTD_roll_5 +
    #home_pat_made_roll_5
    # home_PFG +
    # home_off_scr
  ) + brmsfamily(family = "discrete_weibull")

#### Non-linear ----
formula_totalNL <-
  bf(
    total ~ b1*exp(eta),
    b1  ~ (1|H|home_team) + (1|away_team),
    eta ~ 0 +
      home_off_pat_att_roll +
      home_OSRS_roll_net +
      home_PFG_roll +
      home_OSRS_roll +
      home_OSRS_ewma_net +
      home_totalTD_roll +
      home_off_epa_roll +
      home_off_punt +
      home_off_scr +
      home_off_epa_cum +
      home_off_1st +
      home_off_net_epa_roll +
      home_off_td +
      home_off_rush_epa_cum +
      home_pass_net_epa_roll +
      home_off_n,
    #(1|H|home_team) + (1|A|away_team),
    shape ~ (1|H|home_team) + (1|away_team),
    nl = TRUE
  ) + brmsfamily("poisson", link = "log")

# formula_totalNL2 <-
#   bf(
#     total ~ b1 + inv_logit(eta),
#     b1  ~ (1|H|home_team) + (1|A|away_team),
#     eta ~ 0 +
#       home_OSRS_net +
#       away_OSRS_net +
#       home_off_epa_roll +
#       home_off_epa_cum +
#       home_off_net_epa_cum + 
#       home_off_net_epa_roll +
#       home_pass_net_epa_cum +
#       home_pass_net_epa_roll +
#       home_off_rush_epa_cum +
#       home_off_rush_epa_roll +
#       
#       home_totalTD_roll +
#       home_off_pat_att_roll +
#       home_def_special_plays_cum +
#       
#       home_off_n +
#       home_off_punt +
#       home_off_td +
#       home_off_1st +
#       home_off_scr_2nd +
#       home_off_scr +
#       wind +
#       temp,
#     sigma ~ 
#       home_OSRS_net +
#       away_OSRS_net +
#       home_off_epa_roll +
#       home_off_epa_cum +
#       home_off_net_epa_cum + 
#       home_off_net_epa_roll +
#       home_pass_net_epa_cum +
#       home_pass_net_epa_roll +
#       home_off_rush_epa_cum +
#       home_off_rush_epa_roll +
#       
#       home_totalTD_roll +
#       home_off_pat_att_roll +
#       home_def_special_plays_cum +
#       
#       home_off_n +
#       home_off_punt +
#       home_off_td +
#       home_off_1st +
#       home_off_scr_2nd +
#       home_off_scr +
#       wind +
#       temp +
#       (1|H|home_team) + (1|A|away_team),
#     nl = TRUE
#   )

## Fit ----
iters <- 2000
burn <- 1000
chains <- 4
sims <- (iters-burn)*chains

mean(histModelData$total)
sd(histModelData$total)

default_prior(formula_total, data = histModelData)
get_prior(formula_total, data = histModelData)

priorPoints <- c(
  prior(normal(0,5), class = "b") #, resp = "total")
  #prior(horseshoe(df = 3), class = "b")
)

# priorPointsNL <- c(
#   #prior(normal(0,5), nlpar = "b1"),
#   prior(normal(0,5), nlpar = "eta")
# )

system.time(
  model_nfl_fit <- brm(
    formula_total #+ formula_homeScore + formula_awayScore +
    #set_rescor(rescor = FALSE)
    ,
    data = histModelData,
    #family = brmsfamily("discrete_weibull", link = "identity"),
    save_pars = save_pars(all = TRUE),
    seed = 52,
    chains = chains, 
    cores = parallel::detectCores(),
    iter = iters,
    warmup = burn,
    init = 0,
    #stanvars = stanvars,
    #normalize = FALSE,
    prior = priorPoints,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
  )
)

#prior_summary(model_nfl_fit)
#posterior_summary(model_nfl_fit)

#fitnum <- 1

### Run Fit Diagnostics ----
fitnum <- fitnum + 1
fit_analysis(Fit = fit6, 
             fit = 6, 
             discrete = TRUE, 
             group = F
)


FitR2

totalPPCbars
totalPPCdens
totalPPDbars
totalPPDdens

predMetrics
TotalBetSuccessDF |> group_by(Data) |> arrange(desc(BetProb), .by_group = TRUE)
successPerf |> filter(Data == "Test") |> arrange(desc(Over)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(OddsOver)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(Under)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(OddsUnder)) |> head(5)
loo_compare(loo_fits)

fitFormulas <- list()
FitR2 <- data.frame()
predMetrics <- data.frame()
TotalBetSuccessDF <- data.frame()
successPerf <- data.frame()
loo_fits <- list()

# Fit <- model_nfl_fit
# fit <- fitnum
# discrete <- TRUE
# group <- FALSE

fit_analysis <- function(Fit, fit, discrete = TRUE, group = FALSE){
  # Fit <- model_nfl_fit
  # fit <- 1
  assign(paste0("fit", fit), Fit, envir = .GlobalEnv)
  
  #plot(Fit, ask = FALSE)
  
  # fitFormulas <- list()
  # for(i in 1:fit){
  #   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
  # }
  fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))
  
  ## Diagnostics ----
  #prior_summary(Fit)
  #posterior_summary(Fit)
  # launch_shinystan(Fit)
  print(Fit, digits = 4)
  fixedEff <- fixef(Fit)
  fixedEff <- data.frame(fixedEff) |>
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
  #print(fixedEff, digits = 4)
  fixedSigEff <- fixedEff |> filter(p_val < 0.2)
  # fixedSigEff <- fixedSigEff |> 
  #   rownames_to_column() |>
  #   mutate(
  #     response = str_split_i(rowname, "_", i = 1),
  #     param = str_remove(rowname, paste0(response,"_"))
  #   ) |> 
  #   relocate(c(response, param), .after = "rowname") |>
  #   select(-rowname)
  print(fixedSigEff)
  
  if(group){
    randEff <- ranef(Fit, summary = TRUE)
  }
  #print(randEff, digits = 4)
  #VarCorr(Fit)
  
  #postSum <- posterior_summary(Fit)
  #postSum[grepl("^sd_", rownames(postSum)), ]
  
  ### Bayes R2 -----
  FitR2temp <- bayes_R2(Fit)
  #FitR2temp
  FitR2tempDF <- FitR2temp |>
    bind_cols(
      Fit = paste0("Fit", fit),
      Response = c("total") #, "homescore", "awayscore")
      #Distribution = "NLDW(logit,id)"
    ) |>
    select(Fit, Response, #Distribution, 
           everything())
  print(FitR2tempDF)
  
  FitR2 <- bind_rows(
    FitR2tempDF,
    FitR2
  )
  assign("FitR2",FitR2, envir = .GlobalEnv)
  #FitR2 #<- FitR2tempDF
  
  # Fitsmooth <- conditional_smooths(Fit, method = "posterior_predict")
  # plot(Fitsmooth,
  #      stype = "contour",
  #      ask = FALSE)
  # 
  # Fiteffects <- conditional_effects(Fit, 
  #                                   # effects = c(
  #                                   #   "home_OSRS_net",
  #                                   #   "home_off_epa_roll",
  #                                   #   "away_off_td",
  #                                   #   "home_def_epa_roll",
  #                                   #   "away_SRS_net",
  #                                   #   "away_off_n"
  #                                   # ),
  #                                   method = "posterior_predict", 
  #                                   re_formula = NULL,
  #                                   robust = FALSE)
  # plot(Fiteffects, 
  #      points = TRUE, 
  #      ask = FALSE)
  
  ## Fitted ----
  totalfinalFit <- posterior_predict(Fit, resp = "total")
  totalfinalFitMean <- colMeans(totalfinalFit)
  totalfinalFitMed <- apply(totalfinalFit, 2, function(x){quantile(x, 0.5)})
  totalfinalFitLCB <- apply(totalfinalFit, 2, function(x){quantile(x, 0.025)})
  totalfinalFitUCB <- apply(totalfinalFit, 2, function(x){quantile(x, 0.975)})
  
  # homefinalFit <- posterior_predict(Fit, resp = "homescore")
  # homefinalFitMean <- colMeans(homefinalFit)
  # homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
  # homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
  # homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalFit <- totalfinalFit - homefinalFit
  # awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
  # awayfinalFitMean <- colMeans(awayfinalFit)
  # awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
  # awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
  # awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})
  # 
  # spreadfinalFit <- homefinalFit - awayfinalFit
  # spreadfinalFitMean <- colMeans(spreadfinalFit)
  # spreadfinalFitMed <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.5)})
  # spreadfinalFitLCB <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.025)})
  # spreadfinalFitUCB <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.975)})
  
  ### PPC ----
  #### Bars ----
  # homePPCbars <- ppc_bars(y = histModelData$home_score, 
  #                         yrep = homefinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCbars <- ppc_bars(y = histModelData$away_score, 
  #                         yrep = awayfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCbars <- ppc_bars(y = histModelData$result, 
  #                           yrep = spreadfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  if(discrete){
    totalPPCbars <- ppc_bars(y = histModelData$total, 
                             yrep = totalfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Fit", fit, " Total PPC")) +
      theme_bw()
    assign("totalPPCbars",totalPPCbars, envir = .GlobalEnv)
    print(totalPPCbars)
  }
  # totalPPCbars <- ppc_bars(y = histModelData$total, 
  #                          yrep = totalfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Total PPC")) +
  #   theme_bw()
  
  # homePPCbars
  # awayPPCbars
  # spreadPPCbars
  #totalPPCbars
  
  # homePPCbarsG <- ppc_bars_grouped(y = histModelData$home_score, 
  #                                  yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                  group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCbarsG <- ppc_bars_grouped(y = histModelData$away_score, 
  #                                  yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                  group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCbarsG <- ppc_bars_grouped(y = histModelData$result, 
  #                                    yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                    group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  if(discrete){
    totalPPCbarsG <- ppc_bars_grouped(y = histModelData$total, 
                                      yrep = totalfinalFit[sample(1:sims, 100, replace = FALSE), ],
                                      group = histModelData$home_team) + 
      labs(title = paste0("Fit", fit, " Total PPC")) +
      theme_bw()
    assign("totalPPCbarsG",totalPPCbarsG)
  }
  
  # homePPCbarsG
  # awayPPCbarsG
  # spreadPPCbarsG
  #totalPPCbarsG
  
  #### Density ----
  # homePPCdens <- ppc_dens_overlay(y = histModelData$home_score, 
  #                                 yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCdens <- ppc_dens_overlay(y = histModelData$away_score, 
  #                                 yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCdens <- ppc_dens_overlay(y = histModelData$result, 
  #                                   yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  totalPPCdens <- ppc_dens_overlay(y = histModelData$total, 
                                   yrep = totalfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Fit", fit, " Total PPC")) +
    theme_bw()
  assign("totalPPCdens",totalPPCdens, envir = .GlobalEnv)
  print(totalPPCdens)
  
  # homePPCdens
  # awayPPCdens
  # spreadPPCdens
  #totalPPCdens
  
  # homePPCdensG <- ppc_dens_overlay_grouped(y = histModelData$home_score, 
  #                                          yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCdensG <- ppc_dens_overlay_grouped(y = histModelData$away_score, 
  #                                          yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCdensG <- ppc_dens_overlay_grouped(y = histModelData$result, 
  #                                            yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                            group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  # totalPPCdensG <- ppc_dens_overlay_grouped(y = histModelData$total, 
  #                                           yrep = totalfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                           group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Total PPC")) +
  #   theme_bw()
  
  # homePPCdensG
  # awayPPCdensG
  # spreadPPCdensG
  #totalPPCdensG
  
  ## Preds ----
  totalfinalPreds <- posterior_predict(Fit,
                                       resp = "total",
                                       newdata = modelData,
                                       allow_new_levels = TRUE,
                                       re_formula = NULL
  )
  totalfinalPredsMean <- colMeans(totalfinalPreds)
  totalfinalPredsMed <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.5)})
  totalfinalPredsLCB <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.025)})
  totalfinalPredsUCB <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  
  # homefinalPreds <- posterior_predict(Fit,
  #                                     resp = "homescore",
  #                                     newdata = modelData,
  #                                     allow_new_levels = TRUE,
  #                                     re_formula = NULL
  # )
  # homefinalPredsMean <- colMeans(homefinalPreds)
  # homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5)})
  # homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025)})
  # homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalPreds <- totalfinalPreds - homefinalPreds
  # awayfinalPreds <- posterior_predict(Fit,
  #                                     resp = "awayscore",
  #                                     newdata = modelData,
  #                                     allow_new_levels = TRUE,
  #                                     re_formula = NULL
  # )
  # awayfinalPredsMean <- colMeans(awayfinalPreds)
  # awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5)})
  # awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025)})
  # awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975)})
  # 
  # spreadfinalPreds <- homefinalPreds - awayfinalPreds
  # spreadfinalPredsMean <- colMeans(spreadfinalPreds)
  # spreadfinalPredsMed <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.5)})
  # spreadfinalPredsLCB <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.025)})
  # spreadfinalPredsUCB <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  ### PPD ----
  #### Bars ----
  # homePPDbars <- ppc_bars(y = modelData$home_score, 
  #                         yrep = homefinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDbars <- ppc_bars(y = modelData$away_score, 
  #                         yrep = awayfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDbars <- ppc_bars(y = modelData$result, 
  #                           yrep = spreadfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  if(discrete){
    totalPPDbars <- ppc_bars(y = modelData$total, 
                             yrep = totalfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Preds", fit, " Total PPD")) +
      theme_bw()
    assign("totalPPDbars",totalPPDbars, envir = .GlobalEnv)
    print(totalPPDbars)
  }
  
  # homePPDbars
  # awayPPDbars
  # spreadPPDbars
  #totalPPDbars
  
  #### Density ----
  # homePPDdens <- ppc_dens_overlay(y = modelData$home_score, 
  #                                 yrep = homefinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDdens <- ppc_dens_overlay(y = modelData$away_score, 
  #                                 yrep = awayfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDdens <- ppc_dens_overlay(y = modelData$result, 
  #                                   yrep = spreadfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  totalPPDdens <- ppc_dens_overlay(y = modelData$total, 
                                   yrep = totalfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Preds", fit, " Total PPD")) +
    theme_bw()
  assign("totalPPDdens",totalPPDdens, envir = .GlobalEnv)
  print(totalPPDdens)
  
  # homePPDdens
  # awayPPDdens
  # spreadPPDdens
  #totalPPDdens
  
  ## Goodness of Fit ----
  # homeTrain <- histModelData$home_score
  # awayTrain <- histModelData$away_score
  # spreadTrain <- histModelData$result
  totalTrain <- histModelData$total
  
  # homeTest <- modelData$home_score
  # awayTest <- modelData$away_score
  # spreadTest <- modelData$result
  totalTest <- modelData$total
  
  predMetricsHA <- tibble(
    Fit = rep(paste0("Fit", fit), 1),
    Response = c(
      #"home", 
      #"away",
      #"spread", 
      "total"
    ),
    # Distribution = c(
    #   "NLDW(logit,id)"
    # ),
    MAE_fit = c(
      #mean(abs(homefinalFitMean - homeTrain)),
      #mean(abs(awayfinalFitMean - awayTrain)),
      #mean(abs(spreadfinalFitMean - spreadTrain)),
      mean(abs(totalfinalFitMean - totalTrain))
    ),
    COV_fit = c(
      #mean(homefinalFitLCB < homeTrain &  homeTrain < homefinalFitUCB),
      #mean(awayfinalFitLCB < awayTrain &  awayTrain < awayfinalFitUCB),
      #mean(spreadfinalFitLCB < spreadTrain &  spreadTrain < spreadfinalFitUCB),
      mean(totalfinalFitLCB < totalTrain &  totalTrain < totalfinalFitUCB)
    ),
    MAE_pred = c(
      #mean(abs(homefinalPredsMean - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMean - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMean - spreadTest), na.rm = TRUE),
      mean(abs(totalfinalPredsMean - totalTest), na.rm = TRUE)
    ),
    MAD_pred = c(
      #mean(abs(homefinalPredsMed - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMed - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMed - spreadTest), na.rm = TRUE),
      mean(abs(totalfinalPredsMed - totalTest), na.rm = TRUE)
    ),
    COV_pred = c(
      #mean(homefinalPredsLCB < homeTest & homeTest < homefinalPredsUCB, na.rm = TRUE),
      #mean(awayfinalPredsLCB < awayTest & awayTest < awayfinalPredsUCB, na.rm = TRUE),
      #mean(spreadfinalPredsLCB < spreadTest & spreadTest < spreadfinalPredsUCB, na.rm = TRUE),
      mean(totalfinalPredsLCB < totalTest & totalTest < totalfinalPredsUCB, na.rm = TRUE)
    )
  )
  #predMetricsHA
  
  predMetrics <- bind_rows(
    predMetricsHA,
    predMetrics
  )
  print(predMetrics) #<- predMetricsHA
  assign("predMetrics",predMetrics, envir = .GlobalEnv)
  
  ## Prob Errors ----
  ### Spread ----
  # #### Fit ----
  # spreadLineTrain <- modData |>
  #   filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   pull(spread_line)
  # 
  # spreadLineTrain <- histModelData$spread_line
  # 
  # FittedProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
  # for(j in 1:length(spreadLineTrain)){
  #   fitted <- spreadfinalFit[, j]
  #   probs <- fitted > spreadLineTrain[j]
  #   FittedProbsSpread[, j] <- probs
  # }
  # FittedBetSpread <- colMeans(FittedProbsSpread)
  # FittedBetLogicalSpread <- FittedBetSpread > 0.5
  # FittedLogicalSpread <- spreadTrain > spreadLineTrain
  # FittedProbSpread <- mean(FittedBetLogicalSpread == FittedLogicalSpread, na.rm = TRUE)
  # FittedProbSpread
  # 
  # spreadDataTrain <- modData |> filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          result, spread_line, spreadCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     spreadFit = spreadfinalFitMean,
  #     coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == spreadCover,
  #     spreadCoverProb = FittedBetSpread,
  #     spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
  #                             ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
  #     # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
  #     spreadCoverSuccess = spreadCoverBet == spreadCover
  #   )
  # sum(is.na(spreadDataTrain$spreadCoverSuccess))
  # sum(!is.na(spreadDataTrain$spreadCoverSuccess))
  # 
  # spreadSuccessTrain <- spreadDataTrain |>
  #   summarise(
  #     spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
  #     spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  #   )
  # spreadSuccessTrain
  # 
  # #### Pred ----
  # spreadLineTest <- modData |>
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_totalTD),
  #          !is.na(away_totalTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(spread_line)
  # 
  # spreadLineTest <- modelData$spread_line
  # 
  # PredsProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
  # for(j in 1:length(spreadLineTest)){
  #   fitted <- spreadfinalPreds[, j]
  #   probs <- fitted > spreadLineTest[j]
  #   PredsProbsSpread[, j] <- probs
  # }
  # PredsBetSpread <- colMeans(PredsProbsSpread)
  # PredsBetLogicalSpread <- PredsBetSpread > 0.5
  # PredsLogicalSpread <- spreadTest > spreadLineTest
  # PredsProbSpread <- mean(PredsBetLogicalSpread == PredsLogicalSpread, na.rm = TRUE)
  # PredsProbSpread
  # 
  # spreadDataTest <- modData |> filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_totalTD),
  #          !is.na(away_totalTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   select(game_id, season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          result, spread_line,spreadCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     spreadPred = spreadfinalPredsMean,
  #     coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == spreadCover,
  #     spreadCoverProb = PredsBetSpread,
  #     spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
  #                             ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
  #     # spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE, 
  #     #                         ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
  #     spreadCoverSuccess = spreadCoverBet == spreadCover
  #   )
  # sum(is.na(spreadDataTest$spreadCoverSuccess))
  # sum(!is.na(spreadDataTest$spreadCoverSuccess))
  # 
  # spreadSuccessTest <- spreadDataTest |>
  #   summarise(
  #     spreadProbTest = mean(coverSuccess, na.rm = TRUE),
  #     spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  #   )
  # spreadSuccessTest
  
  ### Total ----
  #### Fit ----
  #Fit <- fit33
  
  # totalfinalFit <- posterior_predict(Fit, resp = "total")
  # totalfinalFitMean <- colMeans(totalfinalFit)
  # totalfinalFitMed <- apply(totalfinalFit, 2, function(x){quantile(x, 0.5)})
  # totalfinalFitLCB <- apply(totalfinalFit, 2, function(x){quantile(x, 0.025)})
  # totalfinalFitUCB <- apply(totalfinalFit, 2, function(x){quantile(x, 0.975)})
  
  totalfinalEFit <- posterior_epred(Fit, resp = "total")
  totalfinalEFitMean <- colMeans(totalfinalEFit)
  totalfinalEFitMed <- apply(totalfinalEFit, 2, function(x){quantile(x, 0.5)})
  totalfinalEFitLCB <- apply(totalfinalEFit, 2, function(x){quantile(x, 0.025)})
  totalfinalEFitUCB <- apply(totalfinalEFit, 2, function(x){quantile(x, 0.975)})
  
  # totalLineTrain <- modData |>
  #   filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   pull(total_line)
  
  totalLineTrain <- histModelData1$total_line
  totalTrain <- histModelData1$total
  totalLineTrainResult <- ifelse(totalTrain > totalLineTrain, "over",
                                 ifelse(totalTrain < totalLineTrain, "under", NA))
  
  # FittedProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
  # FittedProbsTotalE <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
  # for(j in 1:length(totalLineTrain)){
  #   fitted <- totalfinalFit[, j]
  #   probs <- fitted > totalLineTrain[j]
  #   fittedE <- totalfinalEFit[, j]
  #   probsE <- fittedE > totalLineTrain[j]
  #   FittedProbsTotal[, j] <- probs
  #   FittedProbsTotalE[, j] <- probsE
  # }
  # FittedBetTotal <- colMeans(FittedProbsTotal)
  # FittedBetLogicalTotal <- FittedBetTotal > 0.5
  # FittedLogicalTotal <- totalTrain > totalLineTrain
  # FittedProbTotal <- mean(FittedBetLogicalTotal == FittedLogicalTotal, na.rm = TRUE)
  # FittedProbTotal
  # 
  # FittedBetTotalE <- colMeans(FittedProbsTotalE)
  # FittedBetLogicalTotalE <- FittedBetTotalE > 0.5
  # FittedLogicalTotalE <- totalTrain > totalLineTrain
  # FittedProbTotalE <- mean(FittedBetLogicalTotalE == FittedLogicalTotalE, na.rm = TRUE)
  # FittedProbTotalE
  
  FittedOver <- t(t(totalfinalFit) > totalLineTrain)
  FittedBetTotalOver <- colMeans(FittedOver)
  #FittedBetTotalOver <- colMeans(FittedProbsTotal)
  FittedBetLogicalTotalOver <- FittedBetTotalOver > 0.5
  FittedBetLogicalTotalOddsOver <- FittedBetTotalOver > histModelData1$over_prob
  FittedLogicalTotalOver <- totalTrain > totalLineTrain
  FittedProbTotalOver <- mean(FittedBetLogicalTotalOver == FittedLogicalTotalOver, na.rm = TRUE)
  FittedProbTotalOddsOver <- mean(FittedBetLogicalTotalOddsOver == FittedLogicalTotalOver, na.rm = TRUE)
  # FittedProbTotalOver
  # FittedProbTotalOddsOver
  
  FittedUnder <- t(t(totalfinalFit) < totalLineTrain)
  FittedBetTotalUnder <- colMeans(FittedUnder)
  #FittedBetTotalUnder <- colMeans(FittedProbsTotal)
  FittedBetLogicalTotalUnder <- FittedBetTotalUnder > 0.5
  FittedBetLogicalTotalOddsUnder <- FittedBetTotalUnder > histModelData1$under_prob
  FittedLogicalTotalUnder <- totalTrain < totalLineTrain
  FittedProbTotalUnder <- mean(FittedBetLogicalTotalUnder == FittedLogicalTotalUnder, na.rm = TRUE)
  FittedProbTotalOddsUnder <- mean(FittedBetLogicalTotalOddsUnder == FittedLogicalTotalUnder, na.rm = TRUE)
  # FittedProbTotalUnder
  # FittedProbTotalOddsUnder
  
  FittedEOver <- t(t(totalfinalEFit) > totalLineTrain)
  FittedEBetTotalOver <- colMeans(FittedEOver)
  #FittedEBetTotalOver <- colMeans(FittedEProbsTotal)
  FittedEBetLogicalTotalOver <- FittedEBetTotalOver > 0.5
  FittedEBetLogicalTotalOddsOver <- FittedEBetTotalOver > histModelData1$over_prob
  FittedELogicalTotalOver <- totalTrain > totalLineTrain
  FittedEProbTotalOver <- mean(FittedEBetLogicalTotalOver == FittedELogicalTotalOver, na.rm = TRUE)
  FittedEProbTotalOddsOver <- mean(FittedEBetLogicalTotalOddsOver == FittedELogicalTotalOver, na.rm = TRUE)
  # FittedEProbTotalOver
  # FittedEProbTotalOddsOver
  
  FittedEUnder <- t(t(totalfinalEFit) < totalLineTrain)
  FittedEBetTotalUnder <- colMeans(FittedEUnder)
  #FittedEBetTotalUnder <- colMeans(FittedEProbsTotal)
  FittedEBetLogicalTotalUnder <- FittedEBetTotalUnder > 0.5
  FittedEBetLogicalTotalOddsUnder <- FittedEBetTotalUnder > histModelData1$under_prob
  FittedELogicalTotalUnder <- totalTrain < totalLineTrain
  FittedEProbTotalUnder <- mean(FittedEBetLogicalTotalUnder == FittedELogicalTotalUnder, na.rm = TRUE)
  FittedEProbTotalOddsUnder <- mean(FittedEBetLogicalTotalOddsUnder == FittedELogicalTotalUnder, na.rm = TRUE)
  # FittedEProbTotalUnder
  # FittedEProbTotalOddsUnder
  
  totalSuccessTrain <- data.frame(
    TrainOver = FittedProbTotalOver,
    TrainOddsOver = FittedProbTotalOddsOver,
    TrainOverE = FittedEProbTotalOver,
    TrainOddsOverE = FittedEProbTotalOddsOver,
    TrainUnder = FittedProbTotalUnder,
    TrainOddsUnder = FittedProbTotalOddsUnder,
    TrainUnderE = FittedEProbTotalUnder,
    TrainOddsUnderE = FittedEProbTotalOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  FittedBetLogicalTotalOdds <- ifelse(FittedBetTotalOver > histModelData1$over_prob, "over", 
                                      ifelse(FittedBetTotalUnder > histModelData1$under_prob, "under", NA))
  FittedBetLogicalTotalOddsProb <- mean(FittedBetLogicalTotalOdds == totalLineTrainResult, na.rm = TRUE)
  FittedBetLogicalTotalOddsProbBets <- sum(!is.na(FittedBetLogicalTotalOddsProb))
  FittedEBetLogicalTotalOdds <- ifelse(FittedEBetTotalOver > histModelData1$over_prob, "over", 
                                       ifelse(FittedEBetTotalUnder > histModelData1$under_prob, "under", NA))
  FittedEBetLogicalTotalOddsProb <- mean(FittedEBetLogicalTotalOdds == totalLineTrainResult, na.rm = TRUE)
  FittedEBetLogicalTotalOddsProbBets <- sum(!is.na(FittedEBetLogicalTotalOddsProb))
  
  # totalDataTrain <- modData |> 
  #   filter(season %in% c(2022,2023) | (season == 2024 & week <= 6)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, away_OSRS_net,
  #          result, total_line, totalCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     totalFit = totalfinalFitMean,
  #     coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
  #     coverSuccess = coverBet == totalCover,
  #     totalCoverProb = FittedBetTotal,
  #     totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE,
  #                            ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
  #     # totalCoverBet = ifelse(totalCoverProb > .6, TRUE, ifelse(1 - totalCoverProb > .6, FALSE, NA)),
  #     totalCoverSuccess = totalCoverBet == totalCover,
  #     
  #     totalFitE = totalfinalEFitMean,
  #     coverBetE = ifelse(totalFitE > total_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == totalCover,
  #     totalCoverProbE = FittedBetTotalE,
  #     totalCoverBetE = ifelse(totalCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - totalCoverProbE > under_prob, FALSE, NA)),
  #     # totalCoverBet = ifelse(totalCoverProb > .6, TRUE, ifelse(1 - totalCoverProb > .6, FALSE, NA)),
  #     totalCoverSuccessE = totalCoverBetE == totalCover
  #   )
  # # sum(is.na(totalDataTrain$totalCoverSuccess))
  # # sum(!is.na(totalDataTrain$totalCoverSuccess))
  # # sum(is.na(totalDataTrain$totalCoverSuccessE))
  # # sum(!is.na(totalDataTrain$totalCoverSuccessE))
  # 
  # totalSuccessTrain <- totalDataTrain |>
  #   summarise(
  #     totalProbTrain = mean(coverSuccess, na.rm = TRUE),
  #     totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE),
  #     totalProbTrainE = mean(coverSuccessE, na.rm = TRUE),
  #     totalOddsProbTrainE = mean(totalCoverSuccessE, na.rm = TRUE)
  #  )
  #totalSuccessTrain
  
  #### Pred ----
  # totalLineTest <- modData |>
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_totalTD),
  #          !is.na(away_totalTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(total_line)
  
  
  # totalfinalPreds <- posterior_predict(Fit,
  #                                      resp = "total",
  #                                      newdata = modelData,
  #                                      allow_new_levels = TRUE,
  #                                      re_formula = NULL
  # )
  # totalfinalPredsMean <- colMeans(totalfinalPreds)
  # totalfinalPredsMed <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.5)})
  # totalfinalPredsLCB <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.025)})
  # totalfinalPredsUCB <- apply(totalfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  totalfinalEPreds <- posterior_epred(Fit,
                                      resp = "total",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
  )
  totalfinalEPredsMean <- colMeans(totalfinalEPreds)
  totalfinalEPredsMed <- apply(totalfinalEPreds, 2, function(x){quantile(x, 0.5)})
  totalfinalEPredsLCB <- apply(totalfinalEPreds, 2, function(x){quantile(x, 0.025)})
  totalfinalEPredsUCB <- apply(totalfinalEPreds, 2, function(x){quantile(x, 0.975)})
  
  totalLineTest <- modelData1$total_line
  totalTest <- modelData1$total
  totalLineTestResult <- ifelse(totalTest > totalLineTest, "over",
                                ifelse(totalTest < totalLineTest, "under", NA))
  
  # PredsProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
  # PredsProbsTotalE <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
  # for(j in 1:length(totalLineTest)){
  #   fitted <- totalfinalPreds[, j]
  #   probs <- fitted > totalLineTest[j]
  #   fittedE <- totalfinalEPreds[, j]
  #   probsE <- fittedE > totalLineTest[j]
  #   PredsProbsTotal[, j] <- probs
  #   PredsProbsTotalE[, j] <- probsE
  # }
  # PredsBetTotal <- colMeans(PredsProbsTotal)
  # PredsBetLogicalTotal <- PredsBetTotal > 0.5
  # PredsLogicalTotal <- totalTest > totalLineTest
  # PredsProbTotal <- mean(PredsBetLogicalTotal == PredsLogicalTotal, na.rm = TRUE)
  # PredsProbTotal
  # 
  # PredsBetTotalE <- colMeans(PredsProbsTotalE)
  # PredsBetLogicalTotalE <- PredsBetTotalE > 0.5
  # PredsLogicalTotalE <- totalTest > totalLineTest
  # PredsProbTotalE <- mean(PredsBetLogicalTotalE == PredsLogicalTotalE, na.rm = TRUE)
  # PredsProbTotalE
  # 
  # totalDataTest <- modData |> 
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(total)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, home_OSRS,
  #          away_OSRS_net, away_OSRS,
  #          result, total_line, totalCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     totalPreds = totalfinalPredsMean,
  #     coverBet = ifelse(totalPreds > total_line, TRUE, FALSE),
  #     coverSuccess = coverBet == totalCover,
  #     totalCoverProb = PredsBetTotal,
  #     totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE,
  #                            ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
  #     totalCoverBet2 = ifelse(totalCoverProb > .70, TRUE,
  #                             ifelse(1 - totalCoverProb > .70, FALSE, NA)),
  #     totalCoverSuccess = totalCoverBet == totalCover,
  #     totalCoverSuccess2 = totalCoverBet2 == totalCover,
  #     
  #     totalPredsE = totalfinalEPredsMean,
  #     coverBetE = ifelse(totalPredsE > total_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == totalCover,
  #     totalCoverProbE = PredsBetTotalE,
  #     totalCoverBetE = ifelse(totalCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - totalCoverProbE > under_prob, FALSE, NA)),
  #     totalCoverBet2E = ifelse(totalCoverProbE > .70, TRUE,
  #                              ifelse(1 - totalCoverProbE > .70, FALSE, NA)),
  #     totalCoverSuccessE = totalCoverBetE == totalCover,
  #     totalCoverSuccess2E = totalCoverBet2E == totalCover,
  #   )
  # sum(is.na(totalDataTest$totalCoverSuccess))
  # sum(!is.na(totalDataTest$totalCoverSuccess))
  # sum(is.na(totalDataTest$totalCoverSuccessE))
  # sum(!is.na(totalDataTest$totalCoverSuccessE))
  # 
  # totalSuccessTest <- totalDataTest |>
  #   summarise(
  #     totalProbTest = mean(coverSuccess, na.rm = TRUE),
  #     totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE),
  #     totalOddsProbTest2 = mean(totalCoverSuccess, na.rm = TRUE),
  #     totalProbTestE = mean(coverSuccessE, na.rm = TRUE),
  #     totalOddsProbTestE = mean(totalCoverSuccessE, na.rm = TRUE),
  #     totalOddsProbTest2E = mean(totalCoverSuccess2E, na.rm = TRUE)
  #   )
  # totalSuccessTest
  
  # totalfinalPredsResult <- ifelse(t(totalfinalPreds) > totalLineTest, "over",
  #                                 ifelse(t(totalfinalPreds) < totalLineTest, "under", NA))
  # totalfinalPredsResult <- t(totalfinalPredsResult)
  
  
  PredsOver <- t(t(totalfinalPreds) > totalLineTest)
  PredsBetTotalOver <- colMeans(PredsOver)
  #PredsBetTotalOver <- colMeans(PredsProbsTotal)
  PredsBetLogicalTotalOver <- PredsBetTotalOver > 0.5
  PredsBetLogicalTotalOddsOver <- PredsBetTotalOver > modelData1$over_prob
  PredsLogicalTotalOver <- totalTest > totalLineTest
  PredsProbTotalOver <- mean(PredsBetLogicalTotalOver == PredsLogicalTotalOver, na.rm = TRUE)
  PredsProbTotalOddsOver <- mean(PredsBetLogicalTotalOddsOver == PredsLogicalTotalOver, na.rm = TRUE)
  # PredsProbTotalOver
  # PredsProbTotalOddsOver
  
  PredsUnder <- t(t(totalfinalPreds) < totalLineTest)
  PredsBetTotalUnder <- colMeans(PredsUnder)
  #PredsBetTotalUnder <- colMeans(PredsProbsTotal)
  PredsBetLogicalTotalUnder <- PredsBetTotalUnder > 0.5
  PredsBetLogicalTotalOddsUnder <- PredsBetTotalUnder > modelData1$under_prob
  PredsLogicalTotalUnder <- totalTest < totalLineTest
  PredsProbTotalUnder <- mean(PredsBetLogicalTotalUnder == PredsLogicalTotalUnder, na.rm = TRUE)
  PredsProbTotalOddsUnder <- mean(PredsBetLogicalTotalOddsUnder == PredsLogicalTotalUnder, na.rm = TRUE)
  # PredsProbTotalUnder
  # PredsProbTotalOddsUnder
  
  
  PredsEOver <- t(t(totalfinalEPreds) > totalLineTest)
  PredsEBetTotalOver <- colMeans(PredsEOver)
  #PredsEBetTotalOver <- colMeans(PredsEProbsTotal)
  PredsEBetLogicalTotalOver <- PredsEBetTotalOver > 0.5
  PredsEBetLogicalTotalOddsOver <- PredsEBetTotalOver > modelData1$over_prob
  PredsELogicalTotalOver <- totalTest > totalLineTest
  PredsEProbTotalOver <- mean(PredsEBetLogicalTotalOver == PredsELogicalTotalOver, na.rm = TRUE)
  PredsEProbTotalOddsOver <- mean(PredsEBetLogicalTotalOddsOver == PredsELogicalTotalOver, na.rm = TRUE)
  # PredsEProbTotalOver
  # PredsEProbTotalOddsOver
  
  PredsEUnder <- t(t(totalfinalEPreds) < totalLineTest)
  PredsEBetTotalUnder <- colMeans(PredsEUnder)
  #PredsEBetTotalUnder <- colMeans(PredsEProbsTotal)
  PredsEBetLogicalTotalUnder <- PredsEBetTotalUnder > 0.5
  PredsEBetLogicalTotalOddsUnder <- PredsEBetTotalUnder > modelData1$under_prob
  PredsELogicalTotalUnder <- totalTest < totalLineTest
  PredsEProbTotalUnder <- mean(PredsEBetLogicalTotalUnder == PredsELogicalTotalUnder, na.rm = TRUE)
  PredsEProbTotalOddsUnder <- mean(PredsEBetLogicalTotalOddsUnder == PredsELogicalTotalUnder, na.rm = TRUE)
  # PredsEProbTotalUnder
  # PredsEProbTotalOddsUnder
  
  totalSuccessTest <- data.frame(
    TestOver = PredsProbTotalOver,
    TestOddsOver = PredsProbTotalOddsOver,
    TestOverE = PredsEProbTotalOver,
    TestOddsOverE = PredsEProbTotalOddsOver,
    TestUnder = PredsProbTotalUnder,
    TestOddsUnder = PredsProbTotalOddsUnder,
    TestUnderE = PredsEProbTotalUnder,
    TestOddsUnderE = PredsEProbTotalOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  PredsBetLogicalTotalOdds <- ifelse(PredsBetTotalOver > modelData1$over_prob, "over", 
                                     ifelse(PredsBetTotalUnder > modelData1$under_prob, "under", NA))
  PredsBetLogicalTotalOddsProb <- mean(PredsBetLogicalTotalOdds == totalLineTestResult, na.rm = TRUE)
  PredsBetLogicalTotalOddsProbBets <- sum(!is.na(PredsBetLogicalTotalOdds))
  PredsEBetLogicalTotalOdds <- ifelse(PredsEBetTotalOver > modelData1$over_prob, "over", 
                                      ifelse(PredsEBetTotalUnder > modelData1$under_prob, "under", NA))
  PredsEBetLogicalTotalOddsProb <- mean(PredsEBetLogicalTotalOdds == totalLineTestResult, na.rm = TRUE)
  PredsEBetLogicalTotalOddsProbBets <- sum(!is.na(PredsEBetLogicalTotalOdds))
  
  TotalBetSuccessDFtemp <- data.frame(
    Fit = paste0("Fit ", fit), 
    Data = c("Train", "Test"),
    BetNum = c(FittedBetLogicalTotalOddsProbBets, PredsBetLogicalTotalOddsProbBets),
    BetProb = c(FittedBetLogicalTotalOddsProb, PredsBetLogicalTotalOddsProb),
    BetNumE = c(FittedEBetLogicalTotalOddsProbBets, PredsEBetLogicalTotalOddsProbBets),
    BetProbE = c(FittedEBetLogicalTotalOddsProb, PredsEBetLogicalTotalOddsProb)
  )
  print(TotalBetSuccessDFtemp)
  
  TotalBetSuccessDF <- bind_rows(
    TotalBetSuccessDFtemp,
    TotalBetSuccessDF
  )
  assign("TotalBetSuccessDF", TotalBetSuccessDF, envir = .GlobalEnv)
  
  ## Success Perf ----
  successPerfTemp <- bind_rows( 
    totalSuccessTrain |> rename_with(~str_remove(.x, "Train"), .cols = everything()),
    totalSuccessTest |> rename_with(~str_remove(.x, "Test"), .cols = everything())
  ) |>
    mutate(
      Fit = paste0("Fit ", fit), 
      Data = c("Train", "Test"),
      .before = 1
    )
  print(successPerfTemp)
  
  successPerf <- bind_rows(
    successPerfTemp,
    successPerf
  )
  assign("successPerf", successPerf, envir = .GlobalEnv)
  #successPerf #<- successPerfTemp
  
  ### Loo ----
  loo_fits[[paste0("Fit",fit)]] <- loo(Fit)
  loo_compare(loo_fits)
  assign("loo_fits", loo_fits)
}


successPerf |> arrange(desc(ProbTest))
successPerf |> arrange(desc(OddsProbTest))

### Loo ----
assign(paste0("loo", fit), loo(Fit))

#loo_fits <- list()
loo_fits[[paste0("Fit",fit)]] <- get(paste0("loo", fit))
loo_compare(
  loo_fits
)

# Iterate ----
# Initialize values
predWeeks <- max(modelData$week)
iterFitBase <- fit15
#iterFitBaseB <- fitB8
iterFit <- iterFitBase
#iterFitB <- iterFitBaseB
homefinalIterFitTDComb2 <- list()
homefinalIterFitfgComb2 <- list()
# homefinalIterFitxpComb2 <- list()
# homefinalIterFittpComb2 <- list()
homefinalIterFitComb2 <- list()

awayfinalIterFitTDComb2 <- list()
awayfinalIterFitfgComb2 <- list()
# awayfinalIterFitxpComb2 <- list()
# awayfinalIterFittpComb2 <- list()
awayfinalIterFitComb2 <- list()

homefinalIterPredsTDComb2 <- list()
homefinalIterPredsfgComb2 <- list()
# homefinalIterPredsxpComb2 <- list()
# homefinalIterPredstpComb2 <- list()
homefinalIterPredsComb2 <- list()

awayfinalIterPredsTDComb2 <- list()
awayfinalIterPredsfgComb2 <- list()
# awayfinalIterPredsxpComb2 <- list()
# awayfinalIterPredstpComb2 <- list()
awayfinalIterPredsComb2 <- list()

prePriors <- posterior_summary(iterFitBase)
prePriorCoefs <- prior_summary(iterFitBase)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

trainingData <- modData2 |> filter(season %in% 2023:2023)
predictorIterData <- trainingData |> 
  select(-home_score, -away_score,
         -result, -total,
         -season, -week,
         -contains("totalTD"), 
         -contains("fg_made"), 
         -contains("fg_att"),
         -contains("twoPtConv"), 
         -contains("twoPtAtt"),
         -contains("safeties"),
         -contains("pat_made"),
         -contains("pat_att"),
         -contains("TDs"),
         -contains("spread"), 
         -contains("moneyline"),
         -contains("offTD"),
         -total_line,
         -location,
         -div_game,
         -roof)
preProcIterValues <- preProcess(predictorIterData,
                                method = c("center", "scale"))

# prePriorsB <- posterior_summary(iterFitBaseB)
# prePriorCoefsB <- prior_summary(iterFitBaseB)$coef
# old_priorsB <- create_updated_priors(post_summary = prePriorsB)

### Run loop  ----
# took 29 minutes
system.time(
  for(i in 1:predWeeks){
    
    predictWeekData <- modData2 |> 
      filter(season == 2024, week == i) |>
      filter(!is.na(result))
    
    predictWeekData <- predict(preProcIterValues, predictWeekData)
    
    
    # Make Prediction for week 
    homefinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "hometotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    awayfinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "awaytotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    homefinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "homefgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    awayfinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "awayfgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    # homefinalIterPredsTDMed <- apply(homefinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # awayfinalIterPredsTDMed <- apply(awayfinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # predictWeekData <- predictWeekData |>
    #   mutate(
    #     home_totalTD = homefinalIterPredsTDMed,
    #     away_totalTD = awayfinalIterPredsTDMed
    #   )
    # 
    # homefinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "homepatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # homefinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "hometwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # awayfinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "awaypatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # awayfinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "awaytwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    
    homefinalIterPreds <- 
      7*homefinalIterPredsTD + 
      3*homefinalIterPredsfg #+
    # 1*homefinalIterPredsxp +
    # 2*homefinalIterPredstp
    
    awayfinalIterPreds <- 
      7*awayfinalIterPredsTD + 
      3*awayfinalIterPredsfg #+
    # 1*awayfinalIterPredsxp +
    # 2*awayfinalIterPredstp
    
    homefinalIterPredsComb2[[i]] <- homefinalIterPreds
    awayfinalIterPredsComb2[[i]] <- awayfinalIterPreds
    
    # Update prediction matrix
    homefinalIterPredsTDComb2[[i]] <- homefinalIterPredsTD
    homefinalIterPredsfgComb2[[i]] <- homefinalIterPredsfg
    # homefinalIterPredsxpComb2[[i]] <- homefinalIterPredsxp
    # homefinalIterPredstpComb2[[i]] <- homefinalIterPredstp
    awayfinalIterPredsTDComb2[[i]] <- awayfinalIterPredsTD
    awayfinalIterPredsfgComb2[[i]] <- awayfinalIterPredsfg
    # awayfinalIterPredsxpComb2[[i]] <- awayfinalIterPredsxp
    # awayfinalIterPredstpComb2[[i]] <- awayfinalIterPredstp
    
    # iterData <- modelData |>
    #   filter(week %in% i:i)
    # 
    # new_priors <- create_updated_priors(post_summary = posterior_summary(iterFit))
    trainingData <- bind_rows(
      modData2 |> filter(season %in% 2023:2023, week > i),
      modData2 |> filter(season == 2024, week <= i) 
    )
    predictorIterData <- trainingData |> 
      select(-home_score, -away_score,
             -result, -total,
             -season, -week,
             -contains("totalTD"), 
             -contains("fg_made"), 
             -contains("fg_att"),
             -contains("twoPtConv"), 
             -contains("twoPtAtt"),
             -contains("safeties"),
             -contains("pat_made"),
             -contains("pat_att"),
             -contains("TDs"),
             -contains("spread"), 
             -contains("moneyline"),
             -contains("offTD"),
             -total_line,
             -location,
             -div_game,
             -roof)
    preProcIterValues <- preProcess(predictorIterData,
                                    method = c("center", "scale"))
    trainingData <- predict(preProcIterValues, trainingData)
    trainingWeekData <- trainingData |> filter(season == 2024, week == i)
    
    
    iterFit <- update(iterFit,
                      newdata = trainingData,
                      drop_unused_levels = FALSE,
                      cores = parallel::detectCores()
    )
    
    #new_priors <- create_updated_priors(post_summary = posterior_summary(iterFit))
    
    # iterFitB <- update(iterFitBaseB,
    #                   newdata = iterData,
    #                   prior = old_priorsB,
    #                   drop_unused_levels = FALSE
    # )
    
    # iterCoefs <- default_prior(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   #prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan",
    #   drop_unused_levels = TRUE
    # )$coef
    # 
    # iterPrePrior <- posterior_summary(iterFit)
    # iterPrePriorCoefs <- prior_summary(iterFit)$coef
    # 
    # dropCoefs <- iterPrePriorCoefs[!(iterPrePriorCoefs %in% iterCoefs)]
    # newCoefs <- iterCoefs[!(iterCoefs %in% iterPrePriorCoefs)]
    # 
    # updated_priors <- create_updated_priors(post_summary = iterPrePrior)
    # updated_priors_keep <- updated_priors |> filter(!(coef %in% dropCoefs))
    # updated_priors_new <- old_priors |> filter(coef %in% newCoefs)
    # updated_priors2 <- c(updated_priors_keep, updated_priors_new)
    # 
    # # Fit new model
    # iterFit <- brm(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan"
    # )
    
    # Get fitted values for week
    homefinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "hometotalTD")
    homefinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "homefgmade")
    # homefinalIterFitxp <- posterior_predict(iterFitB, resp = "homepatmade")
    # homefinalIterFittp <- posterior_predict(iterFitB, resp = "hometwoPtConv")
    
    
    awayfinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "awaytotalTD")
    awayfinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "awayfgmade")
    # awayfinalIterFitxp <- posterior_predict(iterFitB, resp = "awaypatmade")
    # awayfinalIterFittp <- posterior_predict(iterFitB, resp = "awaytwoPtConv")
    
    homefinalIterFit <- 
      7*homefinalIterFitTD + 
      3*homefinalIterFitfg #+
    # 1*homefinalIterFitxp +
    # 2*homefinalIterFittp
    
    awayfinalIterFit <- 
      7*awayfinalIterFitTD + 
      3*awayfinalIterFitfg #+
    # 1*awayfinalIterFitxp +
    # 2*awayfinalIterFittp
    
    homefinalIterFitComb2[[i]] <- homefinalIterFit
    awayfinalIterFitComb2[[i]] <- awayfinalIterFit
    
    # Update prediction matrix
    homefinalIterFitTDComb2[[i]] <- homefinalIterFitTD
    homefinalIterFitfgComb2[[i]] <- homefinalIterFitfg
    # homefinalIterFitxpComb2[[i]] <- homefinalIterFitxp
    # homefinalIterFittpComb2[[i]] <- homefinalIterFittp
    awayfinalIterFitTDComb2[[i]] <- awayfinalIterFitTD
    awayfinalIterFitfgComb2[[i]] <- awayfinalIterFitfg
    # awayfinalIterFitxpComb2[[i]] <- awayfinalIterFitxp
    # awayfinalIterFittpComb2[[i]] <- awayfinalIterFittp
    
    print(paste0("Finished Week ", i))
  }
)

iterFit2 <- iterFit

save(#iterFit1,
  homefinalIterFitTDComb2,
  homefinalIterFitfgComb2,
  homefinalIterFitComb2,
  
  awayfinalIterFitTDComb2,
  awayfinalIterFitfgComb2,
  awayfinalIterFitComb2,
  
  homefinalIterPredsTDComb2,
  homefinalIterPredsfgComb2,
  homefinalIterPredsComb2,
  
  awayfinalIterPredsTDComb2,
  awayfinalIterPredsfgComb2,
  awayfinalIterPredsComb2,
  file = "~/Desktop/NFL Analysis Data/iterData.RData")


# Predict new data ----
newData <- modData2 |>
  filter(season == 2024) |>
  filter(season_type == "POST") #|>
filter(!is.na(result))

newDataPre <- predict(preProcValues2, newData)

newDataPre <- newDataPre |> select(
  home_team, home_score,
  away_team, away_score,
  result, spread_line,
  total, total_line,
  home_pat_att_roll_5,
  home_offTD_roll_5,
  home_OSRS_ewma_net,
  home_off_punt,
  home_off_epa_cum,
  home_off_rush_epa_cum
)
newDataPre

totalNewPreds <- posterior_predict(fit5,
                                   resp = "total",
                                   newdata = newDataPre,
                                   allow_new_levels = TRUE,
                                   re_formula = NULL
)
totalNewPredsMean <- colMeans(totalNewPreds)
totalNewPredsMed <- apply(totalNewPreds, 2, function(x){quantile(x, 0.5)})
totalNewPredsLCB <- apply(totalNewPreds, 2, function(x){quantile(x, 0.025)})
totalNewPredsUCB <- apply(totalNewPreds, 2, function(x){quantile(x, 0.975)})

totalNewEPreds <- posterior_predict(fit5,
                                    resp = "total",
                                    newdata = newDataPre,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
totalNewEPredsMean <- colMeans(totalNewEPreds)
totalNewEPredsMed <- apply(totalNewEPreds, 2, function(x){quantile(x, 0.5)})
totalNewEPredsLCB <- apply(totalNewEPreds, 2, function(x){quantile(x, 0.025)})
totalNewEPredsUCB <- apply(totalNewEPreds, 2, function(x){quantile(x, 0.975)})

if(discrete){
  totalNewPPDbars <- ppc_bars(y = newData$total, 
                              yrep = totalNewPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
    labs(title = paste0("Preds", fit, " Total PPD")) +
    theme_bw()
  assign("totalPPDbars",totalPPDbars, envir = .GlobalEnv)
  print(totalNewPPDbars)
}

totalPPDdens <- ppc_dens_overlay(y = modelData$total, 
                                 yrep = totalfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Preds", fit, " Total PPD")) +
  theme_bw()
assign("totalPPDdens",totalPPDdens, envir = .GlobalEnv)
print(totalPPDdens)

totalNewLineTest <- newData$total_line
totalNewTest <- newData$total
totalNewLineTestResult <- ifelse(totalNewTest > totalNewLineTest, "over",
                                 ifelse(totalNewTest < totalNewLineTest, "under", NA))

# PredsProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalNewLineTest))
# PredsProbsTotalE <- matrix(NA, nrow = sims, ncol = length(totalNewLineTest))
# for(j in 1:length(totalNewLineTest)){
#   fitted <- totalNewfinalPreds[, j]
#   probs <- fitted > totalNewLineTest[j]
#   fittedE <- totalNewfinalEPreds[, j]
#   probsE <- fittedE > totalNewLineTest[j]
#   PredsProbsTotal[, j] <- probs
#   PredsProbsTotalE[, j] <- probsE
# }
# PredsBetTotal <- colMeans(PredsProbsTotal)
# PredsBetLogicalTotal <- PredsBetTotal > 0.5
# PredsLogicalTotal <- totalNewTest > totalNewLineTest
# PredsProbTotal <- mean(PredsBetLogicalTotal == PredsLogicalTotal, na.rm = TRUE)
# PredsProbTotal
# 
# PredsBetTotalE <- colMeans(PredsProbsTotalE)
# PredsBetLogicalTotalE <- PredsBetTotalE > 0.5
# PredsLogicalTotalE <- totalNewTest > totalNewLineTest
# PredsProbTotalE <- mean(PredsBetLogicalTotalE == PredsLogicalTotalE, na.rm = TRUE)
# PredsProbTotalE
# 
# totalNewDataTest <- modData |> 
#   filter(season == 2024 & week > 6) |>
#   filter(!is.na(totalNew)) |>
#   select(season, week, #game_type,
#          home_team, home_score, away_team, away_score,
#          home_OSRS_net, home_OSRS,
#          away_OSRS_net, away_OSRS,
#          result, totalNew_line, totalNewCover,
#          home_spread_odds, home_spread_prob,
#          away_spread_prob, away_spread_prob,
#          over_odds, over_prob,
#          under_odds, under_prob) |>
#   mutate(
#     totalNewPreds = totalNewfinalPredsMean,
#     coverBet = ifelse(totalNewPreds > totalNew_line, TRUE, FALSE),
#     coverSuccess = coverBet == totalNewCover,
#     totalNewCoverProb = PredsBetTotal,
#     totalNewCoverBet = ifelse(totalNewCoverProb > over_prob, TRUE,
#                            ifelse(1 - totalNewCoverProb > under_prob, FALSE, NA)),
#     totalNewCoverBet2 = ifelse(totalNewCoverProb > .70, TRUE,
#                             ifelse(1 - totalNewCoverProb > .70, FALSE, NA)),
#     totalNewCoverSuccess = totalNewCoverBet == totalNewCover,
#     totalNewCoverSuccess2 = totalNewCoverBet2 == totalNewCover,
#     
#     totalNewPredsE = totalNewfinalEPredsMean,
#     coverBetE = ifelse(totalNewPredsE > totalNew_line, TRUE, FALSE),
#     coverSuccessE = coverBetE == totalNewCover,
#     totalNewCoverProbE = PredsBetTotalE,
#     totalNewCoverBetE = ifelse(totalNewCoverProbE > over_prob, TRUE,
#                             ifelse(1 - totalNewCoverProbE > under_prob, FALSE, NA)),
#     totalNewCoverBet2E = ifelse(totalNewCoverProbE > .70, TRUE,
#                              ifelse(1 - totalNewCoverProbE > .70, FALSE, NA)),
#     totalNewCoverSuccessE = totalNewCoverBetE == totalNewCover,
#     totalNewCoverSuccess2E = totalNewCoverBet2E == totalNewCover,
#   )
# sum(is.na(totalNewDataTest$totalNewCoverSuccess))
# sum(!is.na(totalNewDataTest$totalNewCoverSuccess))
# sum(is.na(totalNewDataTest$totalNewCoverSuccessE))
# sum(!is.na(totalNewDataTest$totalNewCoverSuccessE))
# 
# totalNewSuccessTest <- totalNewDataTest |>
#   summarise(
#     totalNewProbTest = mean(coverSuccess, na.rm = TRUE),
#     totalNewOddsProbTest = mean(totalNewCoverSuccess, na.rm = TRUE),
#     totalNewOddsProbTest2 = mean(totalNewCoverSuccess, na.rm = TRUE),
#     totalNewProbTestE = mean(coverSuccessE, na.rm = TRUE),
#     totalNewOddsProbTestE = mean(totalNewCoverSuccessE, na.rm = TRUE),
#     totalNewOddsProbTest2E = mean(totalNewCoverSuccess2E, na.rm = TRUE)
#   )
# totalNewSuccessTest

# totalNewfinalPredsResult <- ifelse(t(totalNewfinalPreds) > totalNewLineTest, "over",
#                                 ifelse(t(totalNewfinalPreds) < totalNewLineTest, "under", NA))
# totalNewfinalPredsResult <- t(totalNewfinalPredsResult)


PredsOver <- t(t(totalNewPreds) > totalNewLineTest)
PredsBetTotalOver <- colMeans(PredsOver)
#PredsBetTotalOver <- colMeans(PredsProbsTotal)
PredsBetLogicalTotalOver <- PredsBetTotalOver > 0.5
PredsBetLogicalTotalOddsOver <- PredsBetTotalOver > newData$over_prob
PredsLogicalTotalOver <- totalNewTest > totalNewLineTest
PredsProbTotalOver <- mean(PredsBetLogicalTotalOver == PredsLogicalTotalOver, na.rm = TRUE)
PredsProbTotalOddsOver <- mean(PredsBetLogicalTotalOddsOver == PredsLogicalTotalOver, na.rm = TRUE)
# PredsProbTotalOver
# PredsProbTotalOddsOver

PredsUnder <- t(t(totalNewPreds) < totalNewLineTest)
PredsBetTotalUnder <- colMeans(PredsUnder)
#PredsBetTotalUnder <- colMeans(PredsProbsTotal)
PredsBetLogicalTotalUnder <- PredsBetTotalUnder > 0.5
PredsBetLogicalTotalOddsUnder <- PredsBetTotalUnder > newData$under_prob
PredsLogicalTotalUnder <- totalNewTest < totalNewLineTest
PredsProbTotalUnder <- mean(PredsBetLogicalTotalUnder == PredsLogicalTotalUnder, na.rm = TRUE)
PredsProbTotalOddsUnder <- mean(PredsBetLogicalTotalOddsUnder == PredsLogicalTotalUnder, na.rm = TRUE)
# PredsProbTotalUnder
# PredsProbTotalOddsUnder


PredsEOver <- t(t(totalNewEPreds) > totalNewLineTest)
PredsEBetTotalOver <- colMeans(PredsEOver)
#PredsEBetTotalOver <- colMeans(PredsEProbsTotal)
PredsEBetLogicalTotalOver <- PredsEBetTotalOver > 0.5
PredsEBetLogicalTotalOddsOver <- PredsEBetTotalOver > newData$over_prob
PredsELogicalTotalOver <- totalNewTest > totalNewLineTest
PredsEProbTotalOver <- mean(PredsEBetLogicalTotalOver == PredsELogicalTotalOver, na.rm = TRUE)
PredsEProbTotalOddsOver <- mean(PredsEBetLogicalTotalOddsOver == PredsELogicalTotalOver, na.rm = TRUE)
# PredsEProbTotalOver
# PredsEProbTotalOddsOver

PredsEUnder <- t(t(totalNewEPreds) < totalNewLineTest)
PredsEBetTotalUnder <- colMeans(PredsEUnder)
#PredsEBetTotalUnder <- colMeans(PredsEProbsTotal)
PredsEBetLogicalTotalUnder <- PredsEBetTotalUnder > 0.5
PredsEBetLogicalTotalOddsUnder <- PredsEBetTotalUnder > newData$under_prob
PredsELogicalTotalUnder <- totalNewTest < totalNewLineTest
PredsEProbTotalUnder <- mean(PredsEBetLogicalTotalUnder == PredsELogicalTotalUnder, na.rm = TRUE)
PredsEProbTotalOddsUnder <- mean(PredsEBetLogicalTotalOddsUnder == PredsELogicalTotalUnder, na.rm = TRUE)
# PredsEProbTotalUnder
# PredsEProbTotalOddsUnder

totalNewSuccessTest <- data.frame(
  TestOver = PredsProbTotalOver,
  TestOddsOver = PredsProbTotalOddsOver,
  #TestOverE = PredsEProbTotalOver,
  #TestOddsOverE = PredsEProbTotalOddsOver,
  TestUnder = PredsProbTotalUnder,
  TestOddsUnder = PredsProbTotalOddsUnder
  #TestUnderE = PredsEProbTotalUnder,
  #TestOddsUnderE = PredsEProbTotalOddsUnder
) |> 
  mutate(across(everything(), ~round(.x, 3)))

PredsBetLogicalTotalOdds <- ifelse(PredsBetTotalOver > newData$over_prob, "over", 
                                   ifelse(PredsBetTotalUnder > newData$under_prob, "under", NA))
PredsBetLogicalTotalOddsProb <- mean(PredsBetLogicalTotalOdds == totalNewLineTestResult, na.rm = TRUE)
PredsBetLogicalTotalOddsProbBets <- sum(!is.na(PredsBetLogicalTotalOdds))
PredsEBetLogicalTotalOdds <- ifelse(PredsEBetTotalOver > newData$over_prob, "over", 
                                    ifelse(PredsEBetTotalUnder > newData$under_prob, "under", NA))
PredsEBetLogicalTotalOddsProb <- mean(PredsEBetLogicalTotalOdds == totalNewLineTestResult, na.rm = TRUE)
PredsEBetLogicalTotalOddsProbBets <- sum(!is.na(PredsEBetLogicalTotalOdds))

TotalNewBetSuccessDFtemp <- data.frame(
  Fit = paste0("Fit ", fitnum), 
  Data = "New",
  BetNum = PredsBetLogicalTotalOddsProbBets,
  BetProb = PredsBetLogicalTotalOddsProb
  #BetNumE = c(FittedEBetLogicalTotalOddsProbBets, PredsEBetLogicalTotalOddsProbBets),
  #BetProbE = c(FittedEBetLogicalTotalOddsProb, PredsEBetLogicalTotalOddsProb)
)
TotalNewBetSuccessDFtemp

totalNewDataTest <- newData |>
  #filter(season == 2024 & week > 6) |>
  #filter(!is.na(totalNew)) |>
  select(season, week, #game_type,
         home_team, home_score, away_team, away_score,
         home_OSRS_net, home_OSRS,
         away_OSRS_net, away_OSRS,
         home_offTD_roll_5, home_OSRS_ewma_net, home_off_punt,
         home_off_epa_cum, home_off_rush_epa_cum,
         result, spread_line,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         total, total_line, totalCover,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalNewPredsMean = totalNewPredsMean,
    PredsBetLogicalTotalOdds = PredsBetLogicalTotalOdds,
    PredsBetLogicalTotalOddsSuccess = PredsBetLogicalTotalOdds == totalNewLineTestResult,
    PredsBetTotalOver = PredsBetTotalOver,
    PredsBetTotalUnder = PredsBetTotalUnder
    # coverBet = ifelse(totalNewPreds > total_line, TRUE, FALSE),
    # coverSuccess = coverBet == totalCover,
    # totalNewCoverProb = PredsBetTotal,
    # totalNewCoverBet = ifelse(totalNewCoverProb > over_prob, TRUE,
    #                        ifelse(1 - totalNewCoverProb > under_prob, FALSE, NA)),
    # totalNewCoverBet2 = ifelse(totalNewCoverProb > .70, TRUE,
    #                         ifelse(1 - totalNewCoverProb > .70, FALSE, NA)),
    # totalNewCoverSuccess = totalNewCoverBet == totalCover,
    # totalNewCoverSuccess2 = totalNewCoverBet2 == totalCover,
    # 
    # totalNewPredsE = totalNewEPredsMean,
    # coverBetE = ifelse(totalNewPredsE > total_line, TRUE, FALSE),
    # coverSuccessE = coverBetE == totalCover,
    # totalNewCoverProbE = PredsBetTotalE,
    # totalNewCoverBetE = ifelse(totalNewCoverProbE > over_prob, TRUE,
    #                         ifelse(1 - totalNewCoverProbE > under_prob, FALSE, NA)),
    # totalNewCoverBet2E = ifelse(totalNewCoverProbE > .70, TRUE,
    #                          ifelse(1 - totalNewCoverProbE > .70, FALSE, NA)),
    # totalNewCoverSuccessE = totalNewCoverBetE == totalCover,
    # totalNewCoverSuccess2E = totalNewCoverBet2E == totalCover
  )
newData$total_line[8] <- 52.5
newData$over_prob[8] <- .5349
newData$under_prob[8] <- .5349
