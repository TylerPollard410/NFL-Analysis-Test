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
  filter(!is.na(result)) |>
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
    home_safetiesScore = 2*home_safeties,
    home_twoPtConvScore = 2*home_twoPtConv,
    away_totalTDScore = 6*away_totalTD,
    away_fg_madeScore = 3*away_fg_made,
    away_pat_madeScore = away_pat_made,
    away_safetiesScore = 2*away_safeties,
    away_twoPtConvScore = 2*away_twoPtConv,
    home_totalTDScore2 = home_totalTDScore + home_pat_madeScore + home_twoPtConvScore,
    away_totalTDScore2 = away_totalTDScore + away_pat_madeScore + away_twoPtConvScore
  )

modPreProcess <- modData2 |>
  filter(season >= 2021, !is.na(result)) |>
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
    -total_line,
    -spread_line#,
    #-location,
    #-div_game,
    #-roof
  )

modPreProcValues <- preProcess(modPreProcess,
                               method = c("center", "scale")#, "YeoJohnson")
)
modPreProcess2 <- predict(modPreProcValues, newdata = modPreProcess)

histModelData1 <- modData2 |> 
  filter(between(season, 2023, 2023) | (season == 2024 & week <= 6))
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
    #-contains("totalTD"), 
    -contains("fg_made"), 
    -contains("fg_att"),
    -contains("twoPtConv"), 
    -contains("twoPtAtt"),
    -contains("safeties"),
    -contains("pat_made"),
    #-contains("pat_att"),
    -contains("TDs"),
    -contains("spread"), 
    -contains("moneyline"),
    -contains("offTD"),
    -total_line,
    -spread_line#,
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
                histModelData |> select(c(where(is.numeric), -total)),
                use = "pairwise.complete.obs",
                method = "kendall"
)
totalcorT <- t(totalcor)
totalcorT2 <- totalcorT[order(abs(totalcorT)),]
totalcorT2df <- data.frame(sort(round(abs(totalcorT2), 4), decreasing = TRUE))

spreadcor <- cor(histModelData |> select(result),
                 histModelData |> select(c(where(is.numeric), -result)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
spreadcorT <- t(spreadcor)
spreadcorT2 <- spreadcorT[order(abs(spreadcorT)),]
spreadcorT2df <- data.frame(sort(round(abs(spreadcorT2), 4), decreasing = TRUE))


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
formula_total <-
  bf(
    total ~ #0 + #Intercept +
      home_OSRS_net +
      away_OSRS_net +
      home_off_epa_roll +
      home_off_epa_cum +
      home_off_net_epa_cum + 
      home_off_net_epa_roll +
      home_pass_net_epa_cum +
      home_pass_net_epa_roll +
      home_off_rush_epa_cum +
      home_off_rush_epa_roll +
      
      home_totalTD_roll +
      home_off_pat_att_roll +
      home_def_special_plays_cum +
      
      home_off_n +
      home_off_punt +
      home_off_td +
      home_off_1st +
      home_off_scr_2nd +
      home_off_scr +
      wind +
      temp +
      (1|H|home_team) + (1|away_team),
    shape ~ #0 + Intercept +
      1 + (1|H|home_team) #+ (1|A| away_team)
  ) + brmsfamily(family = "discrete_weibull")#, link = "probit")

#### Non-linear ----
formula_totalNL <-
  bf(
    total ~ b1*inv_logit(eta),
    b1  ~ (1|home_team) + (1|away_team),
    eta ~ 0 +
      home_OSRS_net +
      away_OSRS_net +
      home_off_epa_roll +
      home_off_epa_cum +
      home_off_net_epa_cum + 
      home_off_net_epa_roll +
      home_pass_net_epa_cum +
      home_pass_net_epa_roll +
      home_off_rush_epa_cum +
      home_off_rush_epa_roll +
      
      home_totalTD_roll +
      home_off_pat_att_roll +
      home_def_special_plays_cum +
      
      home_off_n +
      home_off_punt +
      home_off_td +
      home_off_1st +
      home_off_scr_2nd +
      home_off_scr +
      wind +
      temp,
    shape ~ (1|home_team) + (1|away_team),
    nl = TRUE
  )

## Fit ----
iters <- 2000
burn <- 1000
chains <- 4
sims <- (iters-burn)*chains

priorPoints <- c(
  prior(normal(0,5), class = "b") #, resp = "total")
)

priorPointsNL <- c(
  #prior(normal(0,5), nlpar = "b1"),
  prior(normal(0,5), nlpar = "eta")
)

system.time(
  model_nfl_fit <- brm(
    formula_totalNL #+ formula_homeScore + formula_awayScore +
    #set_rescor(rescor = FALSE)
    ,
    data = histModelData,
    family = brmsfamily("discrete_weibull", link = "logit"),
    save_pars = save_pars(all = TRUE),
    seed = 52,
    chains = chains, 
    cores = parallel::detectCores(),
    iter = iters,
    warmup = burn,
    init = 0,
    #stanvars = stanvars,
    #normalize = FALSE,
    prior = priorPointsNL,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
  )
)

Fit <- model_nfl_fit
fit <- 32
assign(paste0("fit", fit), Fit)

plot(Fit, ask = FALSE)

fitFormulas <- list()
for(i in 1:fit){
  fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
}
fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
prior_summary(Fit)
posterior_summary(Fit)
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
print(fixedEff, digits = 4)
fixedSigEff <- fixedEff |> filter(p_val < 0.2)
# fixedSigEff <- fixedSigEff |> 
#   rownames_to_column() |>
#   mutate(
#     response = str_split_i(rowname, "_", i = 1),
#     param = str_remove(rowname, paste0(response,"_"))
#   ) |> 
#   relocate(c(response, param), .after = "rowname") |>
#   select(-rowname)
fixedSigEff

randEff <- ranef(Fit, summary = TRUE)
print(randEff, digits = 4)
VarCorr(Fit)

hypothesis(Fit, "age < Intercept",
           class = "sd", group  = "patient")

## test the amount of random intercept variance on all variance
h <- paste(
  "sd_away_team__eta_Intercept^2 / (",
  "sd_away_team__b1_Intercept^2 +",
  "sd_home_team__b1_Intercept^2 + ",
  "sd_away_team__eta_Intercept^2 + ",
  "sd_home_team__eta_Intercept^2 + ",
  "sigma^2",
  ") = 0")
(hyp2 <- hypothesis(Fit, h, class = NULL))
plot(hyp2)

#plot(Fit, ask = FALSE)

postSum <- posterior_summary(Fit)
#postSum[grepl("^sd_", rownames(postSum)), ]

### Bayes R2 -----
FitR2temp <- bayes_R2(Fit)
FitR2temp
FitR2tempDF <- FitR2temp |>
  bind_cols(
    Fit = paste0("Fit", fit),
    Response = c("total"), #, "homescore", "awayscore")
    Distribution = "NLDW(logit,id)"
  ) |>
  select(Fit, Response,Distribution, everything())
FitR2tempDF

FitR2 <- bind_rows(
  FitR2tempDF,
  FitR2
)
FitR2 #<- FitR2tempDF

Fitsmooth <- conditional_smooths(Fit, method = "posterior_predict")
plot(Fitsmooth,
     stype = "contour",
     ask = FALSE)

Fiteffects <- conditional_effects(Fit, 
                                  # effects = c(
                                  #   "home_OSRS_net",
                                  #   "home_off_epa_roll",
                                  #   "away_off_td",
                                  #   "home_def_epa_roll",
                                  #   "away_SRS_net",
                                  #   "away_off_n"
                                  # ),
                                  method = "posterior_predict", 
                                  re_formula = NULL,
                                  robust = FALSE)
plot(Fiteffects, 
     points = TRUE, 
     ask = FALSE)

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
totalPPCbars <- ppc_bars(y = histModelData$total, 
                         yrep = totalfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Total PPC")) +
  theme_bw()

# homePPCbars
# awayPPCbars
# spreadPPCbars
totalPPCbars

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
totalPPCbarsG <- ppc_bars_grouped(y = histModelData$total, 
                                  yrep = totalfinalFit[sample(1:sims, 100, replace = FALSE), ],
                                  group = histModelData$home_team) + 
  labs(title = paste0("Fit", fit, " Total PPC")) +
  theme_bw()

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

# homePPCdens
# awayPPCdens
# spreadPPCdens
totalPPCdens

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
totalPPCdensG <- ppc_dens_overlay_grouped(y = histModelData$total, 
                                          yrep = totalfinalFit[sample(1:sims, 100, replace = FALSE), ],
                                          group = histModelData$home_team) + 
  labs(title = paste0("Fit", fit, " Total PPC")) +
  theme_bw()

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
totalPPDbars <- ppc_bars(y = modelData$total, 
                         yrep = totalfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  labs(title = paste0("Preds", fit, " Total PPD")) +
  theme_bw()

# homePPDbars
# awayPPDbars
# spreadPPDbars
totalPPDbars

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

# homePPDdens
# awayPPDdens
# spreadPPDdens
totalPPDdens

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
  Distribution = c(
    "NLDW(logit,id)"
  ),
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
predMetricsHA

predMetrics <- bind_rows(
  predMetricsHA,
  predMetrics
)
predMetrics #<- predMetricsHA

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
totalLineTrain <- modData |>
  filter(season == 2023 | (season == 2024 & week <= 6)) |>
  pull(total_line)

totalLineTrain <- histModelData$total_line

FittedProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- totalfinalFit[, j]
  probs <- fitted > totalLineTrain[j]
  FittedProbsTotal[, j] <- probs
}
FittedBetTotal <- colMeans(FittedProbsTotal)
FittedBetLogicalTotal <- FittedBetTotal > 0.5
FittedLogicalTotal <- totalTrain > totalLineTrain
FittedProbTotal <- mean(FittedBetLogicalTotal == FittedLogicalTotal, na.rm = TRUE)
FittedProbTotal

totalDataTrain <- modData |> filter(season == 2023 | (season == 2024 & week <= 6)) |>
  select(season, week, #game_type,
         home_team, home_score, away_team, away_score,
         home_OSRS_net, away_OSRS_net,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = totalfinalFitMean,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = FittedBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE,
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    # totalCoverBet = ifelse(totalCoverProb > .6, TRUE, ifelse(1 - totalCoverProb > .6, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )
sum(is.na(totalDataTrain$totalCoverSuccess))
sum(!is.na(totalDataTrain$totalCoverSuccess))

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

#### Pred ----
totalLineTest <- modData |>
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  pull(total_line)

totalLineTest <- modelData$total_line

PredsProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- totalfinalPreds[, j]
  probs <- fitted > totalLineTest[j]
  PredsProbsTotal[, j] <- probs
}
PredsBetTotal <- colMeans(PredsProbsTotal)
PredsBetLogicalTotal <- PredsBetTotal > 0.5
PredsLogicalTotal <- totalTest > totalLineTest
PredsProbTotal <- mean(PredsBetLogicalTotal == PredsLogicalTotal, na.rm = TRUE)
PredsProbTotal

totalDataTest <- modData |> filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score, result,
         home_PFG, away_PFG, home_PAG, away_PAG,
         home_OSRS_net, away_OSRS_net,
         total, total_line,totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = totalfinalPredsMean,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = PredsBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE,
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    # totalCoverBet = ifelse(totalCoverProb > .7, TRUE, 
    #                         ifelse(1 - totalCoverProb > .7, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  ) |>
  relocate(
    total, total_line, totalCover,
    .before = totalPred
  )
sum(is.na(totalDataTest$totalCoverSuccess))
sum(!is.na(totalDataTest$totalCoverSuccess))

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

## Success Perf ----
successPerfTemp <- bind_rows( 
  # bind_cols(
  #   Response = "Spread",
  #   spreadSuccessTrain,
  #   spreadSuccessTest
  # ) |> rename_with(~str_remove(.x, "spread"), .cols = contains("spread")),
  bind_cols(
    Response = "Total",
    totalSuccessTrain,
    totalSuccessTest
  ) |> rename_with(~str_remove(.x, "total"), .cols = contains("total"))
)|>
  mutate(
    Fit = paste0("Fit ", fit), .before = 1
  ) |>
  mutate(
    Distribution = "NLDW(logit,id)", .after = Response
  )
successPerfTemp
successPerf <- bind_rows(
  successPerfTemp,
  successPerf
)
successPerf #<- successPerfTemp

### Loo ----
assign(paste0("loo", fit), loo(Fit))

#loo_fits <- list()
loo_fits[[paste0("Fit",fit)]] <- get(paste0("loo", fit))
loo_compare(
  loo_fits
)




