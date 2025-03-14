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
library(nflverse)

## Tidyverse
library(tidyverse)

# Takes about 
# system.time(
#   source("./app/data-raw/modData.R")
# )

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2020:2024
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


# Data -----
## Clean ----
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

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
  ) |>
  mutate(
    location2 = ifelse(location == "Home", 1, 0),
    .after = location
  ) |>
  mutate(
    location = factor(location, labels = c("Neutral", "Home"))
  )

which(modData$home_totalTD != (modData$home_offTD +
                                 modData$home_special_teams_tds +
                                 modData$home_fumble_recovery_tds +
                                 modData$home_def_tds))
which(modData$away_totalTD != (modData$away_offTD +
                                 modData$away_special_teams_tds +
                                 modData$away_fumble_recovery_tds +
                                 modData$away_def_tds))
which(modData$home_totalTD != (modData$home_pat_att + modData$home_twoPtAtt))
which(modData$away_totalTD != (modData$away_pat_att + modData$away_twoPtAtt))

## Select ----
modScoreData <- modData2 |>
  select(
    game_id,
    season,
    season_type,
    week,
    location,
    location2,
    div_game,
    result,
    spread_line,
    total,
    total_line,
    home_team,
    contains("home_SRS"),
    home_score,
    home_totalTD,
    home_offTD,
    home_special_teams_tds,
    home_fumble_recovery_tds,
    home_def_tds,
    home_pat_made,
    home_pat_att,
    home_twoPtConv,
    home_twoPtAtt,
    home_fg_made,
    home_fg_att,
    home_def_safeties,
    away_team,
    contains("away_SRS"),
    away_score,
    away_totalTD,
    away_offTD,
    away_special_teams_tds,
    away_fumble_recovery_tds,
    away_def_tds,
    away_pat_made,
    away_pat_att,
    away_twoPtConv,
    away_twoPtAtt,
    away_fg_made,
    away_fg_att,
    away_def_safeties
  ) |>
  mutate(home_deffTD = home_fumble_recovery_tds + home_def_tds, .after = home_special_teams_tds) |>
  mutate(away_deffTD = away_fumble_recovery_tds + away_def_tds, .after = away_special_teams_tds)

modScoreDataLong <- modScoreData |>
  clean_homeaway(invert = c("result", "spread_line"))

write_csv(modScoreData |> filter(season >= 2020), 
          file = "~/Desktop/wideNFLdata.csv")
write_csv(modScoreDataLong |> filter(season >= 2020),
          file = "~/Desktop/longNFLdata.csv")

modScoreData2 <- modScoreData |>
  mutate(
    home_totalTD = 6*home_totalTD,
    home_offTD = 6*home_offTD,
    home_special_teams_tds = 6*home_special_teams_tds,
    home_deffTD = 6*home_deffTD,
    home_pat_made = home_pat_made,
    home_twoPtConv = 2*home_twoPtConv,
    home_extra_pts = home_pat_made + home_twoPtConv,
    home_fg_made = 3*home_fg_made,
    home_def_safeties = 2*home_def_safeties,
    away_totalTD = 6*away_totalTD,
    away_offTD = 6*away_offTD,
    away_special_teams_tds = 6*away_special_teams_tds,
    away_deffTD = 6*away_deffTD,
    away_pat_made = away_pat_made,
    away_twoPtConv = 2*away_twoPtConv,
    away_extra_pts = away_pat_made + away_twoPtConv,
    away_fg_made = 3*away_fg_made,
    away_def_safeties = 2*away_def_safeties
  ) |>
  mutate(
    home_score2 = (home_offTD + 
                     home_special_teams_tds +
                     home_deffTD +
                     #home_pat_made + home_twoPtConv +
                     home_extra_pts +
                     home_fg_made +
                     home_def_safeties),
    .after = home_score
  ) |>
  mutate(
    away_score2 = (away_offTD + 
                     away_special_teams_tds +
                     away_deffTD +
                     #away_pat_made + away_twoPtConv +
                     away_extra_pts +
                     away_fg_made +
                     away_def_safeties),
    .after = away_score
  )

home_score_diff <- which(modScoreData2$home_score != modScoreData2$home_score2)
away_score_diff <- which(modScoreData2$away_score != modScoreData2$away_score2)

modScoreData2diff <- modScoreData2 |> slice(c(home_score_diff,away_score_diff))


## Split ----
histModelData1 <- modScoreData |> 
  filter(between(season, 2020, 2023) | (season == 2024 & week <= 6))
modelData1 <- modScoreData |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result))

# Pre-Process Data ----
## Responses ----
### result ----
resp_result <- histModelData1 |> pull(result)
shapiro.test(resp_result)
descdist(resp_result, discrete = TRUE, boot = 1000)

BN_obj_result <- bestNormalize(resp_result,
                               standardize = TRUE, 
                               allow_orderNorm = FALSE, 
                               out_of_sample = TRUE,
                               loo = TRUE)
BN_obj_result
plot(BN_obj_result)

truehist(resp_result, nbins = 100)
truehist(BN_obj_result$x.t, nbins = 100)

### total ----
resp_total <- histModelData1 |> pull(total)
shapiro.test(resp_total)
descdist(resp_total, discrete = TRUE, boot = 1000)

BN_obj_total <- bestNormalize(resp_total,
                              standardize = TRUE, 
                              allow_orderNorm = FALSE, 
                              out_of_sample = TRUE,
                              loo = TRUE)
BN_obj_total
plot(BN_obj_total)

truehist(resp_total, nbins = 100)
truehist(BN_obj_total$x.t, nbins = 100)

### home_score ----
resp_home_score <- histModelData1 |> pull(home_score)
shapiro.test(resp_home_score)
descdist(resp_home_score, discrete = TRUE, boot = 1000)

BN_obj_home_score <- bestNormalize(resp_home_score,
                                   standardize = TRUE, 
                                   allow_orderNorm = FALSE, 
                                   out_of_sample = TRUE,
                                   loo = TRUE)
BN_obj_home_score
plot(BN_obj_home_score)

truehist(resp_home_score, nbins = 100)
truehist(BN_obj_home_score$x.t, nbins = 100)

### away_score ----
resp_away_score <- histModelData1 |> pull(away_score)
shapiro.test(resp_away_score)
descdist(resp_away_score, discrete = TRUE, boot = 1000)
w <- fitdist(resp_away_score, "dweibull")

BN_obj_away_score <- bestNormalize(resp_away_score,
                                   standardize = TRUE, 
                                   allow_orderNorm = FALSE, 
                                   out_of_sample = TRUE,
                                   loo = TRUE)
BN_obj_away_score
plot(BN_obj_away_score)

truehist(resp_away_score, nbins = 100)
truehist(BN_obj_away_score$x.t, nbins = 100)

## Predictors ----
predictorData <- histModelData1 |> 
  select(
    contains("SRS")
  ) 

### BestNormalize ----
BN_predictors <- list

i <- 1
pred_temp_name <- colnames(predictorData)[i]
pred_temp <- predictorData |> pull(pred_temp_name)
BN_pred_temp <- bestNormalize(
  pred_temp,
  standardize = TRUE, 
  allow_orderNorm = FALSE, 
  out_of_sample = TRUE)

### Center, Scale ----


preProc_CS <- preProcess(predictorData,
                         method = c("center", "scale")
)
preProcValuesYeo <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)
preProcValuesArc <- preProcess(predictorData,
                               method = c("center", "scale", "YeoJohnson")
)


preProc_CS
predictorData2 <- predict(preProc_CS, predictorData)
histModelData2 <- predict(preProc_CS, histModelData1)
modelData2 <- predict(preProc_CS, modelData1)

predictorDataYeo <- predict(preProcValuesYeo, predictorData)
histModelDataYeo <- predict(preProcValuesYeo, histModelData1)
modelDataYeo <- predict(preProcValuesYeo, modelData1)

histModelData <- histModelData2
modelData <- modelData2

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

# Common Scores ----
common_score <- expand.grid(
  td = 0:10,
  fg = 0:10
) |>
  mutate(
    score = td*7 + fg*3,
    .before = 1
  ) |>
  filter(score <= 80) |>
  arrange(score, td, fg) |>
  mutate(
    obs_score_num = NA,
    obs_score_perc = NA,
    obs_score_fg_num = NA,
    obs_score_fg_perc = NA,
    .after = score
  )

for(i in 1:nrow(common_score)){
  score_temp <- common_score$score[i]
  score_fg <- common_score$fg[i]
  score_match <- modDataLong$team_score == score_temp
  fg_match <- modDataLong$team_fg_made == score_fg
  
  score_match_num <- sum(score_match)
  score_match_perc <- mean(score_match)
  score_fg_match_num <- sum(score_match & fg_match)
  score_fg_match_perc <- mean(score_match & fg_match)
  common_score$obs_score_num[i] <- score_match_num
  common_score$obs_score_perc[i] <- round(score_match_perc, 4)
  common_score$obs_score_fg_num[i] <- score_fg_match_num
  common_score$obs_score_fg_perc[i] <- round(score_fg_match_perc, 4)
}

common_score_unique <- common_score 



home_scores_df_base <- expand.grid(
  home_score = 0:80,
  home_totalTD = 0:10, 
  home_pat_made = 0:10, 
  home_twoPtConv = 0:5,
  home_fg_made = 0:10,
  home_def_safeties = 0:3
  #away_score = 1:100,
  #away_totalTD = 1:10, 
  #away_pat_made = 1:10, 
  #away_twoPtConv = 1:10,
  #away_fg_made = 1:10,
  #away_def_safeties = 1:10
)

home_scores_df <- home_scores_df_base |>
  mutate(
    home_score_totalTD = home_totalTD*6, 
    home_score_pat_made = home_pat_made, 
    home_score_twoPtConv = home_twoPtConv*2,
    home_score_fg_made = home_fg_made*3,
    home_score_def_safeties = home_def_safeties*2,
    .after = home_score
  ) |>
  # mutate(
  #   away_score_totalTD = away_totalTD*6, 
  #   away_score_pat_made = away_pat_made, 
  #   away_score_twoPtConv = away_twoPtConv*2,
  #   away_score_fg_made = away_fg_made*3,
  #   away_score_def_safeties = away_def_safeties*2,
  #   .after = away_score
  # ) |>
  filter(
    home_score == home_score_totalTD + home_score_pat_made + home_score_twoPtConv + home_score_fg_made + home_score_def_safeties
    #away_score == away_score_totalTD + away_score_pat_made + away_score_twoPtConv + away_score_fg_made + away_score_def_safeties
  ) |>
  filter(
    home_totalTD >= home_pat_made + home_twoPtConv
    #away_totalTD <= away_pat_made + away_twoPtConv
  ) |>
  arrange(
    home_score,
    home_totalTD, 
    home_pat_made, 
    home_twoPtConv,
    home_fg_made,
    home_def_safeties
  )
home_scores_df2 <- home_scores_df |> filter(home)


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

## result ----


# Models #####################################################################
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

## Combined Scores ----
formula_Comb <- bf(
  mvbind(home_totalTD, 
         home_pat_made, 
         home_twoPtConv,
         home_fg_made,
         home_def_safeties,
         away_totalTD, 
         away_pat_made, 
         away_twoPtConv,
         away_fg_made,
         away_def_safeties) ~
    home_SRS + away_SRS + location
) + brmsfamily(family = "discrete_weibull")

default_prior(formula_Comb, histModelData)

priors_Comb <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b"),
  prior(inv_gamma(0.1, 0.1), class = "sigma"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(inv_gamma(0.1, 0.1), class = "sd")
)

fit_Comb_stancode <- stancode(
  formula_Comb,
  data = histModelData,
  #prior = priors_Comb,
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
fit_Comb_stancode

system.time(
  fit_Comb <- brm(
    formula_Comb,
    data = histModelData,
    #prior = priors_Comb,
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
save(fit_Comb, file = "~/Desktop/NfL Analysis Data/fit_Comb_discrete_weibull.RData")

plot(fit_Comb)
print(fit_Comb, digits = 4)
pp_check(fit_Comb, ndraws = 100)

### Fixed Effects ----
fixedEff_Comb <- fixef(fit_Comb)
fixedEff_Comb <- data.frame(fixedEff_Comb) |>
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
print(fixedEff_Comb, digits = 4)
fixedSigEff_Comb <- fixedEff_Comb |> filter(p_val < 0.2)
print(fixedSigEff_Comb)

## Team Scores ----
formula_home <- bf(
  home_score ~
    home_SRS_net + (0 + location2 | home_team)
    #home_SRS + away_SRS + (0 + location2 | home_team)
) + brmsfamily(family = "negbinomial", link = "softplus")

formula_away <- bf(
  away_score ~
    home_SRS_net
    #home_SRS + away_SRS
) + brmsfamily(family = "negbinomial", link = "softplus")

formula_Team <- formula_home + formula_away

default_prior(formula_Team, histModelData)

priors_Team <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b"),
  prior(inv_gamma(0.1, 0.1), class = "sigma"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(inv_gamma(0.1, 0.1), class = "sd")
)

system.time(
  fit_Team <- brm(
    formula_Team,
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


#plot(fit_Team)
print(fit_Team, digits = 4)

fit_Team <- fit_Team2

pp_check(fit_Team, resp = "homescore", ndraws = 100, type = "bars")
pp_check(fit_Team, resp = "awayscore", ndraws = 100, type = "bars")
pp_check(fit_Team, resp = "homescore", ndraws = 100, type = "dens_overlay")
pp_check(fit_Team, resp = "awayscore", ndraws = 100, type = "dens_overlay")

pp_check(fit_Team, newdata = modelData, 
         resp = "homescore", ndraws = 100, type = "bars")
pp_check(fit_Team, newdata = modelData,
         resp = "awayscore", ndraws = 100, type = "bars")
pp_check(fit_Team, newdata = modelData,
         resp = "homescore", ndraws = 100, type = "dens_overlay")
pp_check(fit_Team, newdata = modelData,
         resp = "awayscore", ndraws = 100, type = "dens_overlay")

save(fit_Team, file = "_data/fit_Team.RData")

### Fixed Effects ----
fixedEff_Team <- fixef(fit_Team)
fixedEff_Team <- data.frame(fixedEff_Team) |>
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
print(fixedEff_Team, digits = 4)
fixedSigEff_Team <- fixedEff_Team |> filter(p_val < 0.2)
print(fixedSigEff_Team)

randEff_Team <- ranef(fit_Team)
randEff_Team

teamFit <- 3
assign(paste0("fit_Team", teamFit), fit_Team)
assign(paste0("fixedEff_Team", teamFit), fixedEff_Team)
assign(paste0("randEff_Team", teamFit), randEff_Team)

## Result ----
formula_Res <- bf(
 result ~
    home_SRS_net + (0 + location2 | home_team)
) + brmsfamily(family = "gaussian", link = "identity")

default_prior(formula_Res, histModelData)

priors_Res <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b"),
  prior(inv_gamma(0.1, 0.1), class = "sigma"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(inv_gamma(0.1, 0.1), class = "sd")
)

# fit_Res_stancode <- stancode(
#   formula_Res,
#   data = histModelData,
#   #prior = priors_Res,
#   save_pars = save_pars(all = TRUE), 
#   chains = chains,
#   iter = iters,
#   warmup = burn,
#   cores = parallel::detectCores(),
#   #init = 0,
#   normalize = TRUE,
#   control = list(adapt_delta = 0.95),
#   backend = "rstan",
#   seed = 52
# )
# fit_Res_stancode

system.time(
  fit_Res <- brm(
    formula_Res,
    data = histModelData,
    #prior = priors_Res,
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
save(fit_Res, file = "~/Desktop/NfL Analysis Data/fit_Res_discrete_weibull.RData")

#plot(fit_Res)
print(fit_Res, digits = 4)

#fit_Res <- fit_Res2

pp_check(fit_Res, ndraws = 100, type = "dens_overlay")
pp_check(fit_Res, newdata = modelData,
         ndraws = 100, type = "dens_overlay")

save(fit_Res, file = "_data/fit_Res.RData")

### Fixed Effects ----
fixedEff_Res <- fixef(fit_Res)
fixedEff_Res <- data.frame(fixedEff_Res) |>
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
print(fixedEff_Res, digits = 4)
fixedSigEff_Res <- fixedEff_Res |> filter(p_val < 0.2)
print(fixedSigEff_Res)

randEff_Res <- ranef(fit_Res)
randEff_Res

### MAE ----
fitResiduals_Res <- 
  residuals(
    fit_Res,
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_Res$Estimate))

predResiduals_Res <- 
  residuals(
    fit_Res,
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_Res$Estimate))

ResFit <- 3
assign(paste0("fit_Res", ResFit), fit_Res)
assign(paste0("fixedEff_Res", ResFit), fixedEff_Res)
assign(paste0("randEff_Res", ResFit), randEff_Res)

# Posterior ----
fit <- 1

## PPC ----
homePPCbarsTD <- pp_check(fit_Comb, resp = "hometotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("fit_Comb", fit, " Home PPC TD")) +
  theme_bw()
homePPCbarsXP <- pp_check(fit_Comb, resp = "homepatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " Home PPC xp")) +
  theme_bw()
homePPCbarsTP <- pp_check(fit_Comb, resp = "hometwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " Home PPC tp")) +
  theme_bw()
homePPCbarsFG <- pp_check(fit_Comb, resp = "homefgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("fit_Comb", fit, " Home PPC fg")) +
  theme_bw()
homePPCbarsSF <- pp_check(fit_Comb, resp = "homedefsafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " Home PPC safe")) +
  theme_bw()


awayPPCbarsTD <- pp_check(fit_Comb, resp = "awaytotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("fit_Comb", fit, " away PPC TD")) +
  theme_bw()
awayPPCbarsXP <- pp_check(fit_Comb, resp = "awaypatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " away PPC xp")) +
  theme_bw()
awayPPCbarsTP <- pp_check(fit_Comb, resp = "awaytwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " away PPC tp")) +
  theme_bw()
awayPPCbarsFG <- pp_check(fit_Comb, resp = "awayfgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("fit_Comb", fit, " away PPC fg")) +
  theme_bw()
awayPPCbarsSF <- pp_check(fit_Comb, resp = "awaydefsafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("fit_Comb", fit, " away PPC safe")) +
  theme_bw()


homePPCbarsTD
homePPCbarsFG
homePPCbarsSF
homePPCbarsXP
homePPCbarsTP

awayPPCbarsTD
awayPPCbarsFG
awayPPCbarsSF
awayPPCbarsXP
awayPPCbarsTP

## Predictions ----
### Scoring Components ----
#### Home ----
##### Train ----
post_train_hometotalTD <- posterior_predict(fit_Comb, resp = "hometotalTD") #, re_formula = NA)
post_train_homefgmade <- posterior_predict(fit_Comb, resp = "homefgmade") #, re_formula = NA)
post_train_homepatmade <- posterior_predict(fit_Comb, resp = "homepatmade")
post_train_hometwoPtConv <- posterior_predict(fit_Comb, resp = "hometwoPtConv")
post_train_homedefsafeties <- posterior_predict(fit_Comb, resp = "homedefsafeties")

##### Test ----
post_test_hometotalTD <- posterior_predict(
  fit_Comb,
  resp = "hometotalTD",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_homefgmade <- posterior_predict(
  fit_Comb,
  resp = "homefgmade",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_homepatmade <- posterior_predict(
  fit_Comb,
  resp = "homepatmade",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_hometwoPtConv <- posterior_predict(
  fit_Comb,
  resp = "hometwoPtConv",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_homedefsafeties <- posterior_predict(
  fit_Comb,
  resp = "homedefsafeties",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)

#### Away ----
##### Train ----
post_train_awaytotalTD <- posterior_predict(fit_Comb, resp = "awaytotalTD") #, re_formula = NA)
post_train_awayfgmade <- posterior_predict(fit_Comb, resp = "awayfgmade") #, re_formula = NA)
post_train_awaypatmade <- posterior_predict(fit_Comb, resp = "awaypatmade")
post_train_awaytwoPtConv <- posterior_predict(fit_Comb, resp = "awaytwoPtConv")
post_train_awaydefsafeties <- posterior_predict(fit_Comb, resp = "awaydefsafeties")

##### Test ----
post_test_awaytotalTD <- posterior_predict(
  fit_Comb,
  resp = "awaytotalTD",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_awayfgmade <- posterior_predict(
  fit_Comb,
  resp = "awayfgmade",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_awaypatmade <- posterior_predict(
  fit_Comb,
  resp = "awaypatmade",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_awaytwoPtConv <- posterior_predict(
  fit_Comb,
  resp = "awaytwoPtConv",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_test_awaydefsafeties <- posterior_predict(
  fit_Comb,
  resp = "awaydefsafeties",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)

#### PPD ----
set.seed(52)
sampFitID <- sample(1:sims, 1000, replace = FALSE)

post_samp_hometotalTD <- post_test_hometotalTD[sampFitID, ]
post_samp_homepatmade <- post_test_homepatmade[sampFitID, ]
post_samp_hometwoPtConv <- post_test_hometwoPtConv[sampFitID, ]
post_samp_homefgmade <- post_test_homefgmade[sampFitID, ]
post_samp_homedefsafeties <- post_test_homedefsafeties[sampFitID, ]

post_samp_awaytotalTD <- post_test_awaytotalTD[sampFitID, ]
post_samp_awaypatmade <- post_test_awaypatmade[sampFitID, ]
post_samp_awaytwoPtConv <- post_test_awaytwoPtConv[sampFitID, ]
post_samp_awayfgmade <- post_test_awayfgmade[sampFitID, ]
post_samp_awaydefsafeties <- post_test_awaydefsafeties[sampFitID, ]

obs_hometotalTD <- modelData |> pull(home_totalTD)
obs_homepatmade <- modelData |> pull(home_pat_made)
obs_hometwoPtConv <- modelData |> pull(home_twoPtConv)
obs_homefgmade <- modelData |> pull(home_fg_made)
obs_homedefsafeties <- modelData |> pull(home_def_safeties)

obs_awaytotalTD <- modelData |> pull(away_totalTD)
obs_awaypatmade <- modelData |> pull(away_pat_made)
obs_awaytwoPtConv <- modelData |> pull(away_twoPtConv)
obs_awayfgmade <- modelData |> pull(away_fg_made)
obs_awaydefsafeties <- modelData |> pull(away_def_safeties)

homePPDbars_hometotalTD <- 
  ppc_bars(
    y = obs_hometotalTD,
    yrep = post_samp_hometotalTD,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD hometotalTD")) +
  theme_bw()
homePPDbars_homepatmade <- 
  ppc_bars(
    y = obs_homepatmade,
    yrep = post_samp_homepatmade,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD homepatmade")) +
  theme_bw()
homePPDbars_hometwoPtConv <- 
  ppc_bars(
    y = obs_hometwoPtConv,
    yrep = post_samp_hometwoPtConv,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD hometwoPtConv")) +
  theme_bw()
homePPDbars_homefgmade <- 
  ppc_bars(
    y = obs_homefgmade,
    yrep = post_samp_homefgmade,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD homefgmade")) +
  theme_bw()
homePPDbars_homedefsafeties <- 
  ppc_bars(
    y = obs_homedefsafeties,
    yrep = post_samp_homedefsafeties,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD homedefsafeties")) +
  theme_bw()

awayPPDbars_awaytotalTD <- 
  ppc_bars(
    y = obs_awaytotalTD,
    yrep = post_samp_awaytotalTD,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD awaytotalTD")) +
  theme_bw()
awayPPDbars_awaypatmade <- 
  ppc_bars(
    y = obs_awaypatmade,
    yrep = post_samp_awaypatmade,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD awaypatmade")) +
  theme_bw()
awayPPDbars_awaytwoPtConv <- 
  ppc_bars(
    y = obs_awaytwoPtConv,
    yrep = post_samp_awaytwoPtConv,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD awaytwoPtConv")) +
  theme_bw()
awayPPDbars_awayfgmade <- 
  ppc_bars(
    y = obs_awayfgmade,
    yrep = post_samp_awayfgmade,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD awayfgmade")) +
  theme_bw()
awayPPDbars_awaydefsafeties <- 
  ppc_bars(
    y = obs_awaydefsafeties,
    yrep = post_samp_awaydefsafeties,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPD awaydefsafeties")) +
  theme_bw()



homePPDbars_hometotalTD
homePPDbars_homepatmade
homePPDbars_hometwoPtConv
homePPDbars_homefgmade
homePPDbars_homedefsafeties

awayPPDbars_awaytotalTD
awayPPDbars_awaypatmade
awayPPDbars_awaytwoPtConv
awayPPDbars_awayfgmade
awayPPDbars_awaydefsafeties


### Team Scores ----
#### Train ----
post_train_home_score <-
  post_train_hometotalTD*6 +
  post_train_homepatmade +
  post_train_hometwoPtConv*2 +
  post_train_homefgmade*3 + 
  post_train_homedefsafeties*2

post_train_away_score <-
  post_train_awaytotalTD*6 +
  post_train_awaypatmade +
  post_train_awaytwoPtConv*2 +
  post_train_awayfgmade*3 + 
  post_train_awaydefsafeties*2

set.seed(52)
sampFitID_score <- sample(1:sims, 1000, replace = FALSE)
post_train_samp_home_score <- post_train_home_score[sampFitID_score, ]
post_train_samp_away_score <- post_train_away_score[sampFitID_score, ]

obs_home_score <- histModelData |> pull(home_score)
obs_away_score <- histModelData |> pull(away_score)

homePPCbars_home_score <- 
  ppc_bars(
    y = obs_home_score,
    yrep = post_train_samp_home_score,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Home PPC home_score")) +
  theme_bw()
awayPPCbars_away_score <- 
  ppc_bars(
    y = obs_away_score,
    yrep = post_train_samp_away_score,
    freq = TRUE
  ) + 
  labs(title = paste0("fit_Comb", fit, " Away PPC away_score")) +
  theme_bw()

homePPCbars_home_score
awayPPCbars_away_score

#### Test ----
post_test_home_score <-
  post_test_hometotalTD*6 +
  post_test_homepatmade +
  post_test_hometwoPtConv*2 +
  post_test_homefgmade*3 + 
  post_test_homedefsafeties*2

post_test_away_score <-
  post_test_awaytotalTD*6 +
  post_test_awaypatmade +
  post_test_awaytwoPtConv*2 +
  post_test_awayfgmade*3 + 
  post_test_awaydefsafeties*2








