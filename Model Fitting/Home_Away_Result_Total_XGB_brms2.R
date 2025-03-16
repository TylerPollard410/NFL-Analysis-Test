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

# Takes about 
# system.time(
#   source("./app/data-raw/modData.R")
# )

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

# seasonsMod <- 2020:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))


# Data -----
## Clean ----
modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

modData2 <- modData |> 
  filter(season >= 2007) |>
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

## Predict XGB scores ----
load(file = "~/Desktop/NFL Analysis Data/xgb_home_model.RData")
load(file = "~/Desktop/NFL Analysis Data/xgb_away_model.RData")

modData4 <- modData3 |> 
  select(c(game_id, union(best_vars_home, best_vars_away)))
mod_IDs <- modData4 |>
  filter(complete.cases(modData4)) |>
  pull(game_id)

modData5 <- modData3 |>
  filter(game_id %in% mod_IDs)
modData5 <- modData5 |>
  #filter(game_id %in% mod_IDs) |>
  mutate(
    xgb_pred_home_score = predict(home_model, newdata = modData5),
    .after = home_score
  ) |>
  mutate(
    xgb_pred_away_score = predict(away_model, newdata = modData5),
    .after = away_score
  ) |>
  mutate(
    xgb_pred_result = xgb_pred_home_score - xgb_pred_away_score,
    .after = result
  ) |>
  mutate(
    xgb_pred_total = xgb_pred_home_score + xgb_pred_away_score,
    .after = total
  )

mean(abs(modData5$result - modData5$xgb_pred_result))
mean(abs(modData5$total - modData5$xgb_pred_total))

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
    # xgb_pred_result,
    # xgb_pred_total,
    union(best_vars_home, best_vars_away)
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

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# HOME & AWAY --------------------------------
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
iters <- 4000
burn <- 2000
chains <- 4
sims <- (iters-burn)*chains

## Model  ----
formula_home <- bf(
  home_score ~
    xgb_pred_home_score +
    home_rest +
    away_rest +
    weekday +
    time_of_day +
    location2 +
    div_game + 
    roof + 
    temp + 
    wind + 
    (1 | home_team) +
    (1 | away_team)
) + #mixture(brmsfamily(family = "poisson", link = "log"), nmix = 3)
  brmsfamily(family = "negbinomial", link = "log")

formula_away <- bf(
  away_score ~
    xgb_pred_away_score +
    home_rest +
    away_rest +
    weekday +
    time_of_day +
    location2 +
    div_game + 
    roof + 
    temp + 
    wind + 
    (1 | home_team) +
    (1 | away_team)
) + #mixture(brmsfamily(family = "poisson", link = "log"), nmix = 3)
  brmsfamily(family = "negbinomial", link = "log")

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
    init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)


#plot(fit_Team)
print(fit_Team, digits = 4)

#fit_Team <- fit_Team2
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

save(fit_Team, file = "~/Desktop/NFLAnalysisTest/app/data/fit_Team.rda")

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

### MAE ----
fitResiduals_Team_home <- 
  residuals(
    fit_Team,
    resp = "homescore",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_Team_home$Estimate))

predResiduals_Team_home <- 
  residuals(
    fit_Team,
    resp = "homescore",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_Team_home$Estimate))

fitResiduals_Team_away <- 
  residuals(
    fit_Team,
    resp = "awayscore",
    #Fit2,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(fitResiduals_Team_away$Estimate))

predResiduals_Team_away <- 
  residuals(
    fit_Team,
    resp = "awayscore",
    newdata = modelData,
    method = "posterior_predict",
    re_formula = NULL,
    robust = FALSE,
    probs = c(0.025, 0.975)) |>
  data.frame()
mean(abs(predResiduals_Team_away$Estimate))

teamFit <- 3
assign(paste0("fit_Team", teamFit), fit_Team)
assign(paste0("fixedEff_Team", teamFit), fixedEff_Team)
assign(paste0("randEff_Team", teamFit), randEff_Team)

# Posteriors ----
## Training ----
train_home_score <- histModelData$home_score
train_away_score <- histModelData$away_score

set.seed(52)
posteriorFit_home <- posterior_predict(fit_Team, resp = "homescore")
posteriorFitMean_home <- colMeans(posteriorFit_home)
posteriorFitMed_home <- apply(posteriorFit_home, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_home <- apply(posteriorFit_home, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_home <- apply(posteriorFit_home, 2, function(x){quantile(x, 0.975)})

posteriorFit_away <- posterior_predict(fit_Team, resp = "awayscore")
posteriorFitMean_away <- colMeans(posteriorFit_away)
posteriorFitMed_away <- apply(posteriorFit_away, 2, function(x){quantile(x, 0.5)})
posteriorFitLCB_away <- apply(posteriorFit_away, 2, function(x){quantile(x, 0.025)})
posteriorFitUCB_away <- apply(posteriorFit_away, 2, function(x){quantile(x, 0.975)})

## Test ----
test_home_score <- modelData$home_score
test_away_score <- modelData$away_score

set.seed(52)
posteriorPred_home <- posterior_predict(fit_Team, 
                                        newdata = modelData,
                                        resp = "homescore",
                                        re_formula = NULL)
posteriorPredMean_home <- colMeans(posteriorPred_home)
posteriorPredMed_home <- apply(posteriorPred_home, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_home <- apply(posteriorPred_home, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_home <- apply(posteriorPred_home, 2, function(x){quantile(x, 0.975)})

set.seed(52)
posteriorPred_away <- posterior_predict(fit_Team, 
                                        newdata = modelData,
                                        resp = "awayscore",
                                        re_formula = NULL)
posteriorPredMean_away <- colMeans(posteriorPred_away)
posteriorPredMed_away <- apply(posteriorPred_away, 2, function(x){quantile(x, 0.5)})
posteriorPredLCB_away <- apply(posteriorPred_away, 2, function(x){quantile(x, 0.025)})
posteriorPredUCB_away <- apply(posteriorPred_away, 2, function(x){quantile(x, 0.975)})


# Goodness of Fit ##########################################################
## PPC ----
set.seed(52)
sampFitID <- sample(1:sims, 200, replace = FALSE)
posteriorFitSamp_home <- posteriorFit_home[sampFitID, ]
posteriorFitSamp_away <- posteriorFit_away[sampFitID, ]

fillPPC <- "#d1e1ec"
colorPPC <- "#b3cde0"
fill2PPC <- "#011f4b"

### Bars ----
ppcBarsPlot_home <- ppc_bars(
  y = train_home_score,
  yrep = posteriorFitSamp_home
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "home_score",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

ppcBarsPlot_away <- ppc_bars(
  y = train_away_score,
  yrep = posteriorFitSamp_away
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "away_score",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

### Density -----
ppcDensPlot_home <- ppc_dens_overlay(
  y = train_home_score,
  yrep = posteriorFitSamp_home
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "home_score",
    y = "Density"
  ) +
  #scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

ppcDensPlot_away <- ppc_dens_overlay(
  y = train_away_score,
  yrep = posteriorFitSamp_away
) +
  labs(
    # title = "Simulated density of draws from the PPD vs Observed VMAX",
    # subtitle = "n = 1000 draws",
    x = "away_score",
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
mean_home_score <- meanFunc(train_home_score)
mean_away_score <- meanFunc(train_away_score)

###### Home ----
ppcMeanStat_home <- ppc_stat_data(
  y = train_home_score,
  yrep = posteriorFit_home,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_home_score,
    meanProbHigh = value > mean_home_score
  )

ppcMeanPlotGG_home <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_home |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_home |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "home_score"
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
       subtitle = paste("p-value =", round(mean(ppcMeanStat_home$meanProbLow[-1]), 4))
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

###### Away ----
ppcMeanStat_away <- ppc_stat_data(
  y = train_away_score,
  yrep = posteriorFit_away,
  group = NULL,
  stat = c("meanFunc")
) |>
  mutate(
    meanProbLow = value < mean_away_score,
    meanProbHigh = value > mean_away_score
  )

ppcMeanPlotGG_away <- ggplot() +
  geom_histogram(
    data = ppcMeanStat_away |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcMeanStat_away |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "away_score"
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
       subtitle = paste("p-value =", round(mean(ppcMeanStat_away$meanProbLow[-1]), 4))
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
sd_home_score <- sdFunc(train_home_score)
sd_away_score <- sdFunc(train_away_score)

###### Home ----
ppcSDStat_home <- ppc_stat_data(
  y = train_home_score,
  yrep = posteriorFit_home,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_home_score,
    sdProbHigh = value > sd_home_score
  )

ppcSDPlotGG_home <- ggplot() +
  geom_histogram(
    data = ppcSDStat_home |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_home |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "home_score"
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
       subtitle = paste("p-value =", round(mean(ppcSDStat_home$sdProbLow[-1]), 4))
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

###### Away ----
ppcSDStat_away <- ppc_stat_data(
  y = train_away_score,
  yrep = posteriorFit_away,
  group = NULL,
  stat = c("sdFunc")
) |>
  mutate(
    sdProbLow = value < sd_away_score,
    sdProbHigh = value > sd_away_score
  )

ppcSDPlotGG_away <- ggplot() +
  geom_histogram(
    data = ppcSDStat_away |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcSDStat_away |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "away_score"
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
       subtitle = paste("p-value =", round(mean(ppcSDStat_away$sdProbLow[-1]), 4))
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
range_home_score <- rangeFunc(train_home_score)
range_away_score <- rangeFunc(train_away_score)

###### Home ----
ppcRangeStat_home <- ppc_stat_data(
  y = train_home_score,
  yrep = posteriorFit_home,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_home_score,
    rangeProbHigh = value > range_home_score
  )

ppcRangePlotGG_home <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_home |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_home |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "home_score"
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
       subtitle = paste("p-value =", round(mean(ppcRangeStat_home$rangeProbLow[-1]), 4))
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

###### Away ----
ppcRangeStat_away <- ppc_stat_data(
  y = train_away_score,
  yrep = posteriorFit_away,
  group = NULL,
  stat = c("rangeFunc")
) |>
  mutate(
    rangeProbLow = value < range_away_score,
    rangeProbHigh = value > range_away_score
  )

ppcRangePlotGG_away <- ggplot() +
  geom_histogram(
    data = ppcRangeStat_away |> filter(variable != "y"),
    aes(x = value, color = "Posterior"),
    fill = fillPPC
  ) +
  geom_vline(
    data = ppcRangeStat_away |> filter(variable == "y"),
    aes(xintercept = value, color = "Observed"),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = "away_score"
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
       subtitle = paste("p-value =", round(mean(ppcRangeStat_away$rangeProbLow[-1]), 4))
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
##### Home ----
ppcCombPlot_home <- 
  (ppcBarsPlot_home + ppcDensPlot_home) /
  (ppcMeanPlotGG_home + ppcSDPlotGG_home + ppcRangePlotGG_home) +
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
ppcCombPlot_home

##### Away ----
ppcCombPlot_away <- 
  (ppcBarsPlot_away + ppcDensPlot_away) /
  (ppcMeanPlotGG_away + ppcSDPlotGG_away + ppcRangePlotGG_away) +
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
ppcCombPlot_away

# Performance Metrics ----
## Training ----
### MAE ----
MAE_fit_home <- mean(abs(posteriorFitMean_home - train_home_score))
MAE_fit_away <- mean(abs(posteriorFitMean_away - train_away_score))

### MAD ----
MAD_fit_home <- mean(abs(posteriorFitMed_home - train_home_score))
MAD_fit_away <- mean(abs(posteriorFitMed_away - train_away_score))

### RMSE ----
RMSE_fit_home <- sqrt(mean(posteriorFitMean_home - train_home_score)^2)
RMSE_fit_away <- sqrt(mean(posteriorFitMean_away - train_away_score)^2)

### COV ----
COV_fit_home <- mean(posteriorFitLCB_home < train_home_score &  
                       train_home_score < posteriorFitUCB_home)
COV_fit_away <- mean(posteriorFitLCB_away < train_away_score &  
                       train_away_score < posteriorFitUCB_away)

## Test ----
### MAE ----
MAE_pred_home <- mean(abs(posteriorPredMean_home - test_home_score))
MAE_pred_away <- mean(abs(posteriorPredMean_away - test_away_score))

### MAD ----
MAD_pred_home <- mean(abs(posteriorPredMed_home - test_home_score))
MAD_pred_away <- mean(abs(posteriorPredMed_away - test_away_score))

### RMSE ----
RMSE_pred_home <- sqrt(mean(posteriorPredMean_home - test_home_score)^2)
RMSE_pred_away <- sqrt(mean(posteriorPredMean_away - test_away_score)^2)

### COV ----
COV_pred_home <- mean(posteriorPredLCB_home < test_home_score &  
                        test_home_score < posteriorPredUCB_home)
COV_pred_away <- mean(posteriorPredLCB_away < test_away_score &  
                        test_away_score < posteriorPredUCB_away)

performance_metrics_HA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("home", "away"),
  MAE_fit = c(MAE_fit_home, MAE_fit_away),
  MAD_fit = c(MAD_fit_home, MAD_fit_away),
  RMSE_fit = c(RMSE_fit_home, RMSE_fit_away),
  COV_fit = c(COV_fit_home, COV_fit_away),
  MAE_pred = c(MAE_pred_home, MAE_pred_away),
  MAD_pred = c(MAD_pred_home, MAD_pred_away),
  RMSE_pred = c(RMSE_pred_home, RMSE_pred_away),
  COV_pred = c(COV_pred_home, COV_pred_away)
)
performance_metrics_HA

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

## Model ----
formula_result <- bf(
  result ~
    xgb_pred_result +
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
  #brmsfamily(family = "gaussian", link = "identity")
mixture(brmsfamily(family = "gaussian", link = "identity"), nmix = 2)
#brmsfamily(family = "negbinomial", link = "log")

default_prior(formula_result, histModelData)

priors_result <- c(
  #prior(horseshoe(1), class = "b")
  prior(normal(0, 5), class = "b", dpar = "mu1"),
  prior(normal(0, 5), class = "b", dpar = "mu2"),
  prior(student_t(3, 0, 10), class = "sigma1"),
  prior(student_t(3, 0, 10), class = "sigma2"),
  #prior(inv_gamma(0.1, 0.1), class = "shape"),
  prior(student_t(3, 0, 5), class = "sd", dpar = "mu1"),
  prior(student_t(3, 0, 5), class = "sd", dpar = "mu2")
)

get_prior(formula_result,
          data = histModelData,
          prior = priors_result)

system.time(
  fit_result <- brm(
    formula_result,
    data = histModelData,
    prior = priors_result,
    save_pars = save_pars(all = TRUE), 
    chains = chains,
    iter = iters,
    warmup = burn,
    cores = parallel::detectCores(),
    init = 0,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "rstan",
    seed = 52
  )
)


print(fit_result, digits = 4)

#fit_Team <- fit_Team2
pp_check(fit_result, resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, resp = "result", ndraws = 100, type = "dens_overlay")

pp_check(fit_result, newdata = modelData, 
         resp = "result", ndraws = 100, type = "bars")
pp_check(fit_result, newdata = modelData,
         resp = "result", ndraws = 100, type = "dens_overlay")

save(fit_Team, file = "~/Desktop/NFL Analysis Data/fit_result.RData")

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

fitResult <- 3
assign(paste0("fit_result", fitResult), fit_result)
assign(paste0("fixedEff_result", fitResult), fixedEff_result)
assign(paste0("randEff_result", fitResult), randEff_result)

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
    xgb_home_score = predict(home_model, newdata = histModelData),
    xgb_away_score = predict(away_model, newdata = histModelData),
    xgb_result = xgb_home_score - xgb_away_score,
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
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

## Test ----
test_result_bet_df <- modelData |>
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
    xgb_home_score = predict(home_model, newdata = modelData),
    xgb_away_score = predict(away_model, newdata = modelData),
    xgb_result = xgb_home_score - xgb_away_score,
    xgb_spread_line = spread_line,
    xgb_diff = xgb_result - xgb_spread_line,
    xgb_cover = case_when(
      xgb_result > spread_line ~ "Home",
      xgb_result < spread_line ~ "Away",
      .default = NA
    ),
    xgb_correct_cover = actual_cover == xgb_cover,
    .after = correct_cover
  )

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

## Comparison Table ----
accuracy_metrics_result_temp <- tibble(
  Fit = rep(paste0("Fit", fitResult), 2),
  Data = rep(c("Train", "Test"), each = 1),
  Bet = rep(c("result"), 2), # "total"), times = 1),
  PostMean = c(train_result_acc_prob_posterior_mean,
               #train_total_acc_prob_posterior_mean,
               test_result_acc_prob_posterior_mean
               #test_total_acc_prob_posterior_mean
  ),
  PostFull = c(train_result_acc_prob_full_posterior,
               #train_total_acc_prob_full_posterior,
               test_result_acc_prob_full_posterior
               #test_total_acc_prob_full_posterior
  ),
  BetVegas = c(train_result_bet_vegas_acc,
               #train_total_bet_vegas_acc,
               test_result_bet_vegas_acc
               #test_total_bet_vegas_acc
  ),
  BetVegasCount = c(train_result_bet_vegas_count,
                    #train_total_bet_vegas_count,
                    test_result_bet_vegas_count
                    #test_total_bet_vegas_count
  ),
  BetThresh = c(train_result_bet_thresh_acc,
                #train_total_bet_thresh_acc,
                test_result_bet_thresh_acc
                #test_total_bet_thresh_acc
  ),
  ThreshPerc = c(train_result_thresh,
                 #train_total_thresh,
                 test_result_thresh
                 #test_total_thresh
  ),
  BetThreshCount = c(train_result_bet_thresh_count,
                     #train_total_bet_thresh_count,
                     test_result_bet_thresh_count
                     #test_total_bet_thresh_count
  ),
  XGB = c(train_result_acc_prob_xgb,
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
  total ~
    xgb_pred_total +
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
  #brmsfamily(family = "student", link = "identity")
#mixture(brmsfamily(family = "poisson", link = "log"), nmix = 3)
brmsfamily(family = "negbinomial", link = "log")

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
    init = 0,
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

fitTotal <- 2
assign(paste0("fit_total", fitTotal), fit_total)
assign(paste0("fixedEff_total", fitTotal), fixedEff_total)
assign(paste0("randEff_total", fitTotal), randEff_total)

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

accuracy_metrics_total #<- accuracy_metrics_result_temp

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




