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
    home_spread_prob,
    away_spread_prob,
    total,
    total_line,
    home_team,
    contains("home_SRS"),
    contains("home_OSRS"),
    contains("home_DSRS"),
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
    contains("away_OSRS"),
    contains("away_DSRS"),
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
    contains("SRS"),
    contains("OSRS"),
    contains("DSRS")
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

## Result ----
formula_Res <- bf(
  result ~
    #home_SRS_net + (0 + location2 | home_team)
    #home_SRS_net + location2:home_team
    #home_OSRS_net + home_DSRS_net + (0 + location2 | home_team)
    home_SRS_roll_net + (0 + location2 | home_team)
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
pp_check(fit_Res, newdata = modelData, group = modelData$home_team,
         ndraws = 100, type = "dens_overlay_grouped")

## Preds ----
post_pred_result <- posterior_predict(
  fit_Res,
  resp = "result",
  newdata = modelData,
  allow_new_levels = TRUE,
  re_formula = NULL
)
post_pred_result_Mean <- colMeans(post_pred_result)
post_pred_result_Med <- apply(post_pred_result, 2, function(x){quantile(x, 0.5)})
post_pred_result_LCB <- apply(post_pred_result, 2, function(x){quantile(x, 0.025)})
post_pred_result_UCB <- apply(post_pred_result, 2, function(x){quantile(x, 0.975)})

set.seed(52)
post_pred_sampID <- sample(1:sims, 1000, replace = FALSE)
post_pred_samp <- post_pred_result[post_pred_sampID, ]

ppc_ribbon_grouped(
  y = modelData$result,
  yrep = post_pred_samp,
  group = modelData$home_team,
  x = modelData$week
)

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


##


### Run Fit Diagnostics ----
fitnum <- 1
fitnum <- fitnum + 1
fit_analysis(Fit = fit_Res, 
             fit = fitnum, 
             train_data = histModelData,
             test_data = modelData,
             discrete = FALSE, 
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
ResultBetSuccessDF <- data.frame()
successPerf <- data.frame()
loo_fits <- list()

# Fit <- model_nfl_fit
# fit <- fitnum
# discrete <- TRUE
# group <- FALSE

fit_analysis <- function(Fit, fit, train_data = NULL, test_data = NULL,
                         discrete = TRUE, group = FALSE){
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
      Response = c("result") #, "homescore", "awayscore")
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
  resultfinalFit <- posterior_predict(Fit, resp = "result")
  resultfinalFitMean <- colMeans(resultfinalFit)
  resultfinalFitMed <- apply(resultfinalFit, 2, function(x){quantile(x, 0.5)})
  resultfinalFitLCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.025)})
  resultfinalFitUCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.975)})
  
  # homefinalFit <- posterior_predict(Fit, resp = "homescore")
  # homefinalFitMean <- colMeans(homefinalFit)
  # homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
  # homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
  # homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalFit <- resultfinalFit - homefinalFit
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
    resultPPCbars <- ppc_bars(y = train_data$result, 
                              yrep = resultfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Fit", fit, " Result PPC")) +
      theme_bw()
    assign("resultPPCbars",resultPPCbars, envir = .GlobalEnv)
    print(resultPPCbars)
  }
  # resultPPCbars <- ppc_bars(y = histModelData$result, 
  #                          yrep = resultfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Result PPC")) +
  #   theme_bw()
  
  # homePPCbars
  # awayPPCbars
  # spreadPPCbars
  #resultPPCbars
  
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
    resultPPCbarsG <- ppc_bars_grouped(y = train_data$result, 
                                       yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ],
                                       group = train_data$home_team) + 
      labs(title = paste0("Fit", fit, " Result PPC")) +
      theme_bw()
    assign("resultPPCbarsG",resultPPCbarsG)
  }
  
  # homePPCbarsG
  # awayPPCbarsG
  # spreadPPCbarsG
  #resultPPCbarsG
  
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
  resultPPCdens <- ppc_dens_overlay(y = train_data$result, 
                                    yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Fit", fit, " Result PPC")) +
    theme_bw()
  assign("resultPPCdens",resultPPCdens, envir = .GlobalEnv)
  print(resultPPCdens)
  
  # homePPCdens
  # awayPPCdens
  # spreadPPCdens
  #resultPPCdens
  
  # homePPCdensG <- ppc_dens_overlay_grouped(y = train_data$home_score, 
  #                                          yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCdensG <- ppc_dens_overlay_grouped(y = train_data$away_score, 
  #                                          yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCdensG <- ppc_dens_overlay_grouped(y = train_data$result, 
  #                                            yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                            group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  # resultPPCdensG <- ppc_dens_overlay_grouped(y = train_data$result, 
  #                                           yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                           group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Result PPC")) +
  #   theme_bw()
  
  # homePPCdensG
  # awayPPCdensG
  # spreadPPCdensG
  #resultPPCdensG
  
  ## Preds ----
  resultfinalPreds <- posterior_predict(Fit,
                                        resp = "result",
                                        newdata = test_data,
                                        allow_new_levels = TRUE,
                                        re_formula = NULL
  )
  resultfinalPredsMean <- colMeans(resultfinalPreds)
  resultfinalPredsMed <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.5)})
  resultfinalPredsLCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.025)})
  resultfinalPredsUCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  
  # homefinalPreds <- posterior_predict(Fit,
  #                                     resp = "homescore",
  #                                     newdata = test_data,
  #                                     allow_new_levels = TRUE,
  #                                     re_formula = NULL
  # )
  # homefinalPredsMean <- colMeans(homefinalPreds)
  # homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5)})
  # homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025)})
  # homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalPreds <- resultfinalPreds - homefinalPreds
  # awayfinalPreds <- posterior_predict(Fit,
  #                                     resp = "awayscore",
  #                                     newdata = test_data,
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
  # homePPDbars <- ppc_bars(y = test_data$home_score, 
  #                         yrep = homefinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDbars <- ppc_bars(y = test_data$away_score, 
  #                         yrep = awayfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDbars <- ppc_bars(y = test_data$result, 
  #                           yrep = spreadfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  if(discrete){
    resultPPDbars <- ppc_bars(y = test_data$result, 
                              yrep = resultfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Preds", fit, " Result PPD")) +
      theme_bw()
    assign("resultPPDbars",resultPPDbars, envir = .GlobalEnv)
    print(resultPPDbars)
  }
  
  # homePPDbars
  # awayPPDbars
  # spreadPPDbars
  #resultPPDbars
  
  #### Density ----
  # homePPDdens <- ppc_dens_overlay(y = test_data$home_score, 
  #                                 yrep = homefinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDdens <- ppc_dens_overlay(y = test_data$away_score, 
  #                                 yrep = awayfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDdens <- ppc_dens_overlay(y = test_data$result, 
  #                                   yrep = spreadfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  resultPPDdens <- ppc_dens_overlay(y = test_data$result, 
                                    yrep = resultfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Preds", fit, " Result PPD")) +
    theme_bw()
  assign("resultPPDdens",resultPPDdens, envir = .GlobalEnv)
  print(resultPPDdens)
  
  # homePPDdens
  # awayPPDdens
  # spreadPPDdens
  #resultPPDdens
  
  ## Goodness of Fit ----
  # homeTrain <- train_data$home_score
  # awayTrain <- train_data$away_score
  # spreadTrain <- train_data$result
  resultTrain <- train_data$result
  
  # homeTest <- test_data$home_score
  # awayTest <- test_data$away_score
  # spreadTest <- test_data$result
  resultTest <- test_data$result
  
  predMetricsHA <- tibble(
    Fit = rep(paste0("Fit", fit), 1),
    Response = c(
      #"home", 
      #"away",
      #"spread", 
      "result"
    ),
    # Distribution = c(
    #   "NLDW(logit,id)"
    # ),
    MAE_fit = c(
      #mean(abs(homefinalFitMean - homeTrain)),
      #mean(abs(awayfinalFitMean - awayTrain)),
      #mean(abs(spreadfinalFitMean - spreadTrain)),
      mean(abs(resultfinalFitMean - resultTrain))
    ),
    COV_fit = c(
      #mean(homefinalFitLCB < homeTrain &  homeTrain < homefinalFitUCB),
      #mean(awayfinalFitLCB < awayTrain &  awayTrain < awayfinalFitUCB),
      #mean(spreadfinalFitLCB < spreadTrain &  spreadTrain < spreadfinalFitUCB),
      mean(resultfinalFitLCB < resultTrain &  resultTrain < resultfinalFitUCB)
    ),
    MAE_pred = c(
      #mean(abs(homefinalPredsMean - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMean - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMean - spreadTest), na.rm = TRUE),
      mean(abs(resultfinalPredsMean - resultTest), na.rm = TRUE)
    ),
    MAD_pred = c(
      #mean(abs(homefinalPredsMed - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMed - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMed - spreadTest), na.rm = TRUE),
      mean(abs(resultfinalPredsMed - resultTest), na.rm = TRUE)
    ),
    COV_pred = c(
      #mean(homefinalPredsLCB < homeTest & homeTest < homefinalPredsUCB, na.rm = TRUE),
      #mean(awayfinalPredsLCB < awayTest & awayTest < awayfinalPredsUCB, na.rm = TRUE),
      #mean(spreadfinalPredsLCB < spreadTest & spreadTest < spreadfinalPredsUCB, na.rm = TRUE),
      mean(resultfinalPredsLCB < resultTest & resultTest < resultfinalPredsUCB, na.rm = TRUE)
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
  # spreadLineTrain <- train_data$spread_line
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
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(spread_line)
  # 
  # spreadLineTest <- test_data$spread_line
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
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
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
  
  ### Result ----
  #### Fit ----
  #Fit <- fit33
  
  # resultfinalFit <- posterior_predict(Fit, resp = "result")
  # resultfinalFitMean <- colMeans(resultfinalFit)
  # resultfinalFitMed <- apply(resultfinalFit, 2, function(x){quantile(x, 0.5)})
  # resultfinalFitLCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.025)})
  # resultfinalFitUCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.975)})
  
  resultfinalEFit <- posterior_epred(Fit, resp = "result")
  resultfinalEFitMean <- colMeans(resultfinalEFit)
  resultfinalEFitMed <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.5)})
  resultfinalEFitLCB <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.025)})
  resultfinalEFitUCB <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.975)})
  
  # resultLineTrain <- modData |>
  #   filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   pull(result_line)
  
  resultLineTrain <- train_data$spread_line
  resultTrain <- train_data$result
  resultLineTrainResult <- ifelse(resultTrain > resultLineTrain, "homeCover",
                                  ifelse(resultTrain < resultLineTrain, "awayCover", NA))
  
  # FittedProbsResult <- matrix(NA, nrow = sims, ncol = length(resultLineTrain))
  # FittedProbsResultE <- matrix(NA, nrow = sims, ncol = length(resultLineTrain))
  # for(j in 1:length(resultLineTrain)){
  #   fitted <- resultfinalFit[, j]
  #   probs <- fitted > resultLineTrain[j]
  #   fittedE <- resultfinalEFit[, j]
  #   probsE <- fittedE > resultLineTrain[j]
  #   FittedProbsResult[, j] <- probs
  #   FittedProbsResultE[, j] <- probsE
  # }
  # FittedBetResult <- colMeans(FittedProbsResult)
  # FittedBetLogicalResult <- FittedBetResult > 0.5
  # FittedLogicalResult <- resultTrain > resultLineTrain
  # FittedProbResult <- mean(FittedBetLogicalResult == FittedLogicalResult, na.rm = TRUE)
  # FittedProbResult
  # 
  # FittedBetResultE <- colMeans(FittedProbsResultE)
  # FittedBetLogicalResultE <- FittedBetResultE > 0.5
  # FittedLogicalResultE <- resultTrain > resultLineTrain
  # FittedProbResultE <- mean(FittedBetLogicalResultE == FittedLogicalResultE, na.rm = TRUE)
  # FittedProbResultE
  
  FittedOver <- t(t(resultfinalFit) > resultLineTrain)
  FittedBetResultOver <- colMeans(FittedOver)
  #FittedBetResultOver <- colMeans(FittedProbsResult)
  FittedBetLogicalResultOver <- FittedBetResultOver > 0.5
  FittedBetLogicalResultOddsOver <- FittedBetResultOver > train_data$home_spread_prob
  FittedLogicalResultOver <- resultTrain > resultLineTrain
  FittedProbResultOver <- mean(FittedBetLogicalResultOver == FittedLogicalResultOver, na.rm = TRUE)
  FittedProbResultOddsOver <- mean(FittedBetLogicalResultOddsOver == FittedLogicalResultOver, na.rm = TRUE)
  # FittedProbResultOver
  # FittedProbResultOddsOver
  
  FittedUnder <- t(t(resultfinalFit) < resultLineTrain)
  FittedBetResultUnder <- colMeans(FittedUnder)
  #FittedBetResultUnder <- colMeans(FittedProbsResult)
  FittedBetLogicalResultUnder <- FittedBetResultUnder > 0.5
  FittedBetLogicalResultOddsUnder <- FittedBetResultUnder > train_data$away_spread_prob
  FittedLogicalResultUnder <- resultTrain < resultLineTrain
  FittedProbResultUnder <- mean(FittedBetLogicalResultUnder == FittedLogicalResultUnder, na.rm = TRUE)
  FittedProbResultOddsUnder <- mean(FittedBetLogicalResultOddsUnder == FittedLogicalResultUnder, na.rm = TRUE)
  # FittedProbResultUnder
  # FittedProbResultOddsUnder
  
  FittedEOver <- t(t(resultfinalEFit) > resultLineTrain)
  FittedEBetResultOver <- colMeans(FittedEOver)
  #FittedEBetResultOver <- colMeans(FittedEProbsResult)
  FittedEBetLogicalResultOver <- FittedEBetResultOver > 0.5
  FittedEBetLogicalResultOddsOver <- FittedEBetResultOver > train_data$home_spread_prob
  FittedELogicalResultOver <- resultTrain > resultLineTrain
  FittedEProbResultOver <- mean(FittedEBetLogicalResultOver == FittedELogicalResultOver, na.rm = TRUE)
  FittedEProbResultOddsOver <- mean(FittedEBetLogicalResultOddsOver == FittedELogicalResultOver, na.rm = TRUE)
  # FittedEProbResultOver
  # FittedEProbResultOddsOver
  
  FittedEUnder <- t(t(resultfinalEFit) < resultLineTrain)
  FittedEBetResultUnder <- colMeans(FittedEUnder)
  #FittedEBetResultUnder <- colMeans(FittedEProbsResult)
  FittedEBetLogicalResultUnder <- FittedEBetResultUnder > 0.5
  FittedEBetLogicalResultOddsUnder <- FittedEBetResultUnder > train_data$away_spread_prob
  FittedELogicalResultUnder <- resultTrain < resultLineTrain
  FittedEProbResultUnder <- mean(FittedEBetLogicalResultUnder == FittedELogicalResultUnder, na.rm = TRUE)
  FittedEProbResultOddsUnder <- mean(FittedEBetLogicalResultOddsUnder == FittedELogicalResultUnder, na.rm = TRUE)
  # FittedEProbResultUnder
  # FittedEProbResultOddsUnder
  
  resultSuccessTrain <- data.frame(
    TrainOver = FittedProbResultOver,
    TrainOddsOver = FittedProbResultOddsOver,
    TrainOverE = FittedEProbResultOver,
    TrainOddsOverE = FittedEProbResultOddsOver,
    TrainUnder = FittedProbResultUnder,
    TrainOddsUnder = FittedProbResultOddsUnder,
    TrainUnderE = FittedEProbResultUnder,
    TrainOddsUnderE = FittedEProbResultOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  FittedBetLogicalResultOdds <- ifelse(FittedBetResultOver > train_data$home_spread_prob, "homeCover", 
                                       ifelse(FittedBetResultUnder > train_data$away_spread_prob, "awayCover", NA))
  FittedBetLogicalResultOddsProb <- mean(FittedBetLogicalResultOdds == resultLineTrainResult, na.rm = TRUE)
  FittedBetLogicalResultOddsProbBets <- sum(!is.na(FittedBetLogicalResultOddsProb))
  FittedEBetLogicalResultOdds <- ifelse(FittedEBetResultOver > train_data$home_spread_prob, "homeCover", 
                                        ifelse(FittedEBetResultUnder > train_data$away_spread_prob, "awayCover", NA))
  FittedEBetLogicalResultOddsProb <- mean(FittedEBetLogicalResultOdds == resultLineTrainResult, na.rm = TRUE)
  FittedEBetLogicalResultOddsProbBets <- sum(!is.na(FittedEBetLogicalResultOddsProb))
  
  # resultDataTrain <- modData |> 
  #   filter(season %in% c(2022,2023) | (season == 2024 & week <= 6)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, away_OSRS_net,
  #          result, spread_line, resultCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     resultFit = resultfinalFitMean,
  #     coverBet = ifelse(resultFit > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == resultCover,
  #     resultCoverProb = FittedBetResult,
  #     resultCoverBet = ifelse(resultCoverProb > over_prob, TRUE,
  #                            ifelse(1 - resultCoverProb > under_prob, FALSE, NA)),
  #     # resultCoverBet = ifelse(resultCoverProb > .6, TRUE, ifelse(1 - resultCoverProb > .6, FALSE, NA)),
  #     resultCoverSuccess = resultCoverBet == resultCover,
  #     
  #     resultFitE = resultfinalEFitMean,
  #     coverBetE = ifelse(resultFitE > spread_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == resultCover,
  #     resultCoverProbE = FittedBetResultE,
  #     resultCoverBetE = ifelse(resultCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - resultCoverProbE > under_prob, FALSE, NA)),
  #     # resultCoverBet = ifelse(resultCoverProb > .6, TRUE, ifelse(1 - resultCoverProb > .6, FALSE, NA)),
  #     resultCoverSuccessE = resultCoverBetE == resultCover
  #   )
  # # sum(is.na(resultDataTrain$resultCoverSuccess))
  # # sum(!is.na(resultDataTrain$resultCoverSuccess))
  # # sum(is.na(resultDataTrain$resultCoverSuccessE))
  # # sum(!is.na(resultDataTrain$resultCoverSuccessE))
  # 
  # resultSuccessTrain <- resultDataTrain |>
  #   summarise(
  #     resultProbTrain = mean(coverSuccess, na.rm = TRUE),
  #     resultOddsProbTrain = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultProbTrainE = mean(coverSuccessE, na.rm = TRUE),
  #     resultOddsProbTrainE = mean(resultCoverSuccessE, na.rm = TRUE)
  #  )
  #resultSuccessTrain
  
  #### Pred ----
  # resultLineTest <- modData |>
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(spread_line)
  
  
  # resultfinalPreds <- posterior_predict(Fit,
  #                                      resp = "result",
  #                                      newdata = test_data,
  #                                      allow_new_levels = TRUE,
  #                                      re_formula = NULL
  # )
  # resultfinalPredsMean <- colMeans(resultfinalPreds)
  # resultfinalPredsMed <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.5)})
  # resultfinalPredsLCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.025)})
  # resultfinalPredsUCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  resultfinalEPreds <- posterior_epred(Fit,
                                       resp = "result",
                                       newdata = test_data,
                                       allow_new_levels = TRUE,
                                       re_formula = NULL
  )
  resultfinalEPredsMean <- colMeans(resultfinalEPreds)
  resultfinalEPredsMed <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.5)})
  resultfinalEPredsLCB <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.025)})
  resultfinalEPredsUCB <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.975)})
  
  resultLineTest <- test_data$spread_line
  resultTest <- test_data$result
  resultLineTestResult <- ifelse(resultTest > resultLineTest, "homeCover",
                                 ifelse(resultTest < resultLineTest, "awayCover", NA))
  
  # PredsProbsResult <- matrix(NA, nrow = sims, ncol = length(resultLineTest))
  # PredsProbsResultE <- matrix(NA, nrow = sims, ncol = length(resultLineTest))
  # for(j in 1:length(resultLineTest)){
  #   fitted <- resultfinalPreds[, j]
  #   probs <- fitted > resultLineTest[j]
  #   fittedE <- resultfinalEPreds[, j]
  #   probsE <- fittedE > resultLineTest[j]
  #   PredsProbsResult[, j] <- probs
  #   PredsProbsResultE[, j] <- probsE
  # }
  # PredsBetResult <- colMeans(PredsProbsResult)
  # PredsBetLogicalResult <- PredsBetResult > 0.5
  # PredsLogicalResult <- resultTest > resultLineTest
  # PredsProbResult <- mean(PredsBetLogicalResult == PredsLogicalResult, na.rm = TRUE)
  # PredsProbResult
  # 
  # PredsBetResultE <- colMeans(PredsProbsResultE)
  # PredsBetLogicalResultE <- PredsBetResultE > 0.5
  # PredsLogicalResultE <- resultTest > resultLineTest
  # PredsProbResultE <- mean(PredsBetLogicalResultE == PredsLogicalResultE, na.rm = TRUE)
  # PredsProbResultE
  # 
  # resultDataTest <- modData |> 
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, home_OSRS,
  #          away_OSRS_net, away_OSRS,
  #          result, spread_line, resultCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     resultPreds = resultfinalPredsMean,
  #     coverBet = ifelse(resultPreds > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == resultCover,
  #     resultCoverProb = PredsBetResult,
  #     resultCoverBet = ifelse(resultCoverProb > over_prob, TRUE,
  #                            ifelse(1 - resultCoverProb > under_prob, FALSE, NA)),
  #     resultCoverBet2 = ifelse(resultCoverProb > .70, TRUE,
  #                             ifelse(1 - resultCoverProb > .70, FALSE, NA)),
  #     resultCoverSuccess = resultCoverBet == resultCover,
  #     resultCoverSuccess2 = resultCoverBet2 == resultCover,
  #     
  #     resultPredsE = resultfinalEPredsMean,
  #     coverBetE = ifelse(resultPredsE > spread_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == resultCover,
  #     resultCoverProbE = PredsBetResultE,
  #     resultCoverBetE = ifelse(resultCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - resultCoverProbE > under_prob, FALSE, NA)),
  #     resultCoverBet2E = ifelse(resultCoverProbE > .70, TRUE,
  #                              ifelse(1 - resultCoverProbE > .70, FALSE, NA)),
  #     resultCoverSuccessE = resultCoverBetE == resultCover,
  #     resultCoverSuccess2E = resultCoverBet2E == resultCover,
  #   )
  # sum(is.na(resultDataTest$resultCoverSuccess))
  # sum(!is.na(resultDataTest$resultCoverSuccess))
  # sum(is.na(resultDataTest$resultCoverSuccessE))
  # sum(!is.na(resultDataTest$resultCoverSuccessE))
  # 
  # resultSuccessTest <- resultDataTest |>
  #   summarise(
  #     resultProbTest = mean(coverSuccess, na.rm = TRUE),
  #     resultOddsProbTest = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultOddsProbTest2 = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultProbTestE = mean(coverSuccessE, na.rm = TRUE),
  #     resultOddsProbTestE = mean(resultCoverSuccessE, na.rm = TRUE),
  #     resultOddsProbTest2E = mean(resultCoverSuccess2E, na.rm = TRUE)
  #   )
  # resultSuccessTest
  
  # resultfinalPredsResult <- ifelse(t(resultfinalPreds) > resultLineTest, "over",
  #                                 ifelse(t(resultfinalPreds) < resultLineTest, "under", NA))
  # resultfinalPredsResult <- t(resultfinalPredsResult)
  
  
  PredsOver <- t(t(resultfinalPreds) > resultLineTest)
  PredsBetResultOver <- colMeans(PredsOver)
  #PredsBetResultOver <- colMeans(PredsProbsResult)
  PredsBetLogicalResultOver <- PredsBetResultOver > 0.5
  PredsBetLogicalResultOddsOver <- PredsBetResultOver > test_data$home_spread_prob
  PredsLogicalResultOver <- resultTest > resultLineTest
  PredsProbResultOver <- mean(PredsBetLogicalResultOver == PredsLogicalResultOver, na.rm = TRUE)
  PredsProbResultOddsOver <- mean(PredsBetLogicalResultOddsOver == PredsLogicalResultOver, na.rm = TRUE)
  # PredsProbResultOver
  # PredsProbResultOddsOver
  
  PredsUnder <- t(t(resultfinalPreds) < resultLineTest)
  PredsBetResultUnder <- colMeans(PredsUnder)
  #PredsBetResultUnder <- colMeans(PredsProbsResult)
  PredsBetLogicalResultUnder <- PredsBetResultUnder > 0.5
  PredsBetLogicalResultOddsUnder <- PredsBetResultUnder > test_data$away_spread_prob
  PredsLogicalResultUnder <- resultTest < resultLineTest
  PredsProbResultUnder <- mean(PredsBetLogicalResultUnder == PredsLogicalResultUnder, na.rm = TRUE)
  PredsProbResultOddsUnder <- mean(PredsBetLogicalResultOddsUnder == PredsLogicalResultUnder, na.rm = TRUE)
  # PredsProbResultUnder
  # PredsProbResultOddsUnder
  
  
  PredsEOver <- t(t(resultfinalEPreds) > resultLineTest)
  PredsEBetResultOver <- colMeans(PredsEOver)
  #PredsEBetResultOver <- colMeans(PredsEProbsResult)
  PredsEBetLogicalResultOver <- PredsEBetResultOver > 0.5
  PredsEBetLogicalResultOddsOver <- PredsEBetResultOver > test_data$home_spread_prob
  PredsELogicalResultOver <- resultTest > resultLineTest
  PredsEProbResultOver <- mean(PredsEBetLogicalResultOver == PredsELogicalResultOver, na.rm = TRUE)
  PredsEProbResultOddsOver <- mean(PredsEBetLogicalResultOddsOver == PredsELogicalResultOver, na.rm = TRUE)
  # PredsEProbResultOver
  # PredsEProbResultOddsOver
  
  PredsEUnder <- t(t(resultfinalEPreds) < resultLineTest)
  PredsEBetResultUnder <- colMeans(PredsEUnder)
  #PredsEBetResultUnder <- colMeans(PredsEProbsResult)
  PredsEBetLogicalResultUnder <- PredsEBetResultUnder > 0.5
  PredsEBetLogicalResultOddsUnder <- PredsEBetResultUnder > test_data$away_spread_prob
  PredsELogicalResultUnder <- resultTest < resultLineTest
  PredsEProbResultUnder <- mean(PredsEBetLogicalResultUnder == PredsELogicalResultUnder, na.rm = TRUE)
  PredsEProbResultOddsUnder <- mean(PredsEBetLogicalResultOddsUnder == PredsELogicalResultUnder, na.rm = TRUE)
  # PredsEProbResultUnder
  # PredsEProbResultOddsUnder
  
  resultSuccessTest <- data.frame(
    TestOver = PredsProbResultOver,
    TestOddsOver = PredsProbResultOddsOver,
    TestOverE = PredsEProbResultOver,
    TestOddsOverE = PredsEProbResultOddsOver,
    TestUnder = PredsProbResultUnder,
    TestOddsUnder = PredsProbResultOddsUnder,
    TestUnderE = PredsEProbResultUnder,
    TestOddsUnderE = PredsEProbResultOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  PredsBetLogicalResultOdds <- ifelse(PredsBetResultOver > test_data$home_spread_prob, "homeCover", 
                                      ifelse(PredsBetResultUnder > test_data$away_spread_prob, "awayCover", NA))
  PredsBetLogicalResultOddsProb <- mean(PredsBetLogicalResultOdds == resultLineTestResult, na.rm = TRUE)
  PredsBetLogicalResultOddsProbBets <- sum(!is.na(PredsBetLogicalResultOdds))
  PredsEBetLogicalResultOdds <- ifelse(PredsEBetResultOver > test_data$home_spread_prob, "homeCover", 
                                       ifelse(PredsEBetResultUnder > test_data$away_spread_prob, "awayCover", NA))
  PredsEBetLogicalResultOddsProb <- mean(PredsEBetLogicalResultOdds == resultLineTestResult, na.rm = TRUE)
  PredsEBetLogicalResultOddsProbBets <- sum(!is.na(PredsEBetLogicalResultOdds))
  
  ResultBetSuccessDFtemp <- data.frame(
    Fit = paste0("Fit ", fit), 
    Data = c("Train", "Test"),
    BetNum = c(FittedBetLogicalResultOddsProbBets, PredsBetLogicalResultOddsProbBets),
    BetProb = c(FittedBetLogicalResultOddsProb, PredsBetLogicalResultOddsProb),
    BetNumE = c(FittedEBetLogicalResultOddsProbBets, PredsEBetLogicalResultOddsProbBets),
    BetProbE = c(FittedEBetLogicalResultOddsProb, PredsEBetLogicalResultOddsProb)
  )
  print(ResultBetSuccessDFtemp)
  
  ResultBetSuccessDF <- bind_rows(
    ResultBetSuccessDFtemp,
    ResultBetSuccessDF
  )
  assign("ResultBetSuccessDF", ResultBetSuccessDF, envir = .GlobalEnv)
  
  ## Success Perf ----
  successPerfTemp <- bind_rows( 
    resultSuccessTrain |> rename_with(~str_remove(.x, "Train"), .cols = everything()),
    resultSuccessTest |> rename_with(~str_remove(.x, "Test"), .cols = everything())
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
  looComp <- loo_compare(loo_fits)
  print(looComp)
  assign("loo_fits", loo_fits, envir = .GlobalEnv)
}








