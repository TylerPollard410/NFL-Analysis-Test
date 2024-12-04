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
library(pracma)
library(forecast)
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

source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2021:2024
gameDataMod <- gameData |> filter(season %in% seasonsMod)
gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
pbpDataMod <- load_pbp(seasons = seasonsMod)
load("./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "NFLdata",
                 user = "postgre",
                 password = "NFLpass1234",
                 host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
dbListTables(con)
dbDisconnect(con)


# EPA ========
## Total -----
teamWeekStats <- calculate_stats(
  seasons = seasonsMod, 
  summary_level = "week", 
  stat_type = "team", 
  season_type = "REG+POST"
)

teamSeasonStats <- calculate_stats(
  seasons = seasonsMod, 
  summary_level = "season", 
  stat_type = "team", 
  season_type = "REG+POST"
)

epaPlay <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play == 1) |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_plays = n(),
    #off_pass_epa_total = sum(epa),
    #off_pass_wpa_total = sum(vegas_wpa),
    #off_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_epa = mean(epa, na.rm = TRUE),
    #off_pass_wpa = mean(vegas_wpa),
    off_epa_adj = mean(epa*vegas_wpa, na.rm = TRUE),
    #def_pass_epa_total = sum(epa),
    #def_pass_wpa_total = sum(vegas_wpa),
    #def_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    #def_pass_epa = mean(epa),
    #def_pass_wpa = mean(vegas_wpa),
    #def_pass_epa_adj = mean(epa*scaled_vegas_wp)
  ) |>
  ungroup()

## Passing ------
epaPass <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "pass") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_pass_plays = n(),
    #off_pass_epa_total = sum(epa),
    #off_pass_wpa_total = sum(vegas_wpa),
    #off_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_pass_epa = mean(epa, na.rm = TRUE),
    #off_pass_wpa = mean(vegas_wpa),
    off_pass_epa_adj = mean(epa*vegas_wpa, na.rm = TRUE),
    #def_pass_epa_total = sum(epa),
    #def_pass_wpa_total = sum(vegas_wpa),
    #def_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    #def_pass_epa = mean(epa),
    #def_pass_wpa = mean(vegas_wpa),
    #def_pass_epa_adj = mean(epa*scaled_vegas_wp)
  ) |>
  ungroup()

## Rushing ------
epaRush <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "run") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_rush_plays = n(),
    #off_rush_epa_total = sum(epa),
    #off_rush_wpa_total = sum(vegas_wpa),
    #off_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_rush_epa = mean(epa, na.rm = TRUE),
    #off_rush_wpa = mean(vegas_wpa),
    off_rush_epa_adj = mean(epa*vegas_wpa, na.rm = TRUE),
    #def_rush_epa_total = sum(epa),
    #def_rush_wpa_total = sum(vegas_wpa),
    #def_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    #def_rush_epa = mean(epa),
    #def_rush_wpa = mean(vegas_wpa),
    #def_rush_epa_adj = mean(epa*scaled_vegas_wp)
  ) |>
  ungroup()


## Combined ----
epaData <- epaPlay |>
  left_join(
    epaPass,
    #epaRush,
    by = join_by(game_id, season, week, posteam, home_team, away_team)
  ) |>
  left_join(
    #epaPass,
    epaRush,
    by = join_by(game_id, season, week, posteam, home_team, away_team)
  ) |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  )

epaData2 <- epaData |>
  left_join(
    epaData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  ) #|>
# group_by(season, team) |>
# mutate(
#   across(c(contains("off"), contains("def")), ~cummean(.x))
# ) |>
# mutate(
#   across(c(contains("off"), contains("def")), ~lag(.x))
# ) |>
# ungroup()

epaData3 <- gameDataLongMod |>
  left_join(
    epaData2 |> 
      select(game_id, #season, week,
             team,
             #off_plays,
             off_epa, off_epa_adj,
             #off_pass_plays,
             off_pass_epa, off_pass_epa_adj,
             #off_rush_plays,
             off_rush_epa, off_rush_epa_adj,
             #def_plays,
             def_epa, def_epa_adj,
             #def_pass_plays,
             def_pass_epa, def_pass_epa_adj,
             #def_rush_plays,
             def_rush_epa, def_rush_epa_adj
      ),
    by = join_by(game_id, team)
  ) |>
  left_join(
    seasonWeekStandings |>
      select(
        season,
        week,
        team,
        PFG = team_PPG,
        PAG = opp_PPG,
        MOV,
        SOS,
        SRS,
        OSRS,
        DSRS
      ),
    by = join_by(season, week, team)
  ) |>
  select(
    game_id,
    season,
    week,
    season_type,
    location,
    team, team_score,
    opponent, opponent_score,
    result, spread_line, total,
    PFG, PAG,
    #off_plays,
    off_epa, off_epa_adj,
    #off_pass_plays,
    off_pass_epa, off_pass_epa_adj,
    #off_rush_plays,
    off_rush_epa, off_rush_epa_adj,
    #def_plays,
    def_epa, def_epa_adj,
    #def_pass_plays,
    def_pass_epa, def_pass_epa_adj,
    #def_rush_plays,
    def_rush_epa, def_rush_epa_adj,
    MOV,
    SOS,
    SRS,
    OSRS,
    DSRS,
    team_rest, opponent_rest,
    div_game, roof, surface, temp, wind
  )

## Add SRS ----
epaSeasonAvgs <- epaData3 |>
  filter(complete.cases(team_score)) |>
  group_by(season, team) |>
  summarise(
    PF = sum(team_score),
    PA = sum(opponent_score),
    PD = PF - PA,
    #off_plays = sum(off_plays),
    avg_off_epa_adj = mean(off_epa_adj),
    #off_pass_plays = sum(off_pass_plays),
    avg_off_pass_epa_adj = mean(off_pass_epa_adj),
    #off_rush_plays = sum(off_rush_plays),
    avg_off_rush_epa_adj = mean(off_rush_epa_adj),
    #def_plays = sum(def_plays),
    avg_def_epa_adj = mean(def_epa_adj),
    #def_pass_plays = sum(def_pass_plays),
    avg_def_pass_epa_adj = mean(def_pass_epa_adj),
    #def_rush_plays = sum(def_rush_plays),
    avg_def_rush_epa_adj = mean(def_rush_epa_adj)
  ) |>
  ungroup() |> 
  left_join(
    seasonWeekStandings |>
      group_by(season) |>
      filter(week == max(week)) |>
      ungroup() |>
      select(
        season,
        team,
        avg_PFG = team_PPG,
        avg_PAG = opp_PPG,
        avg_MOV = MOV, 
        avg_SOS = SOS,
        avg_SRS = SRS, 
        avg_OSRS = OSRS, 
        avg_DSRS = DSRS
      ),
    by = join_by(season, team)
  ) |>
  mutate(season_new = season + 1, .after = season) |>
  relocate(avg_PFG, .after = PF) |>
  relocate(avg_PAG, .after = PA)

# srsSeasonAvgs <- seasonWeekStandings |>
#   group_by(season) |>
#   filter(week == max(week))


epaData4 <- epaData3 |>
  group_by(season, team) |>
  mutate(
    cummean_off_epa_adj = lag(cummean(off_epa_adj)),
    cummean_off_pass_epa_adj = lag(cummean(off_pass_epa_adj)),
    cummean_off_rush_epa_adj = lag(cummean(off_rush_epa_adj)),
    cummean_def_epa_adj = lag(cummean(def_epa_adj)),
    cummean_def_pass_epa_adj = lag(cummean(def_pass_epa_adj)),
    cummean_def_rush_epa_adj = lag(cummean(def_rush_epa_adj)),
    PFG = lag(PFG),
    PAG = lag(PAG),
    MOV = lag(MOV),
    SOS = lag(SOS),
    SRS = lag(SRS),
    OSRS = lag(OSRS),
    DSRS = lag(DSRS)
  ) |>
  ungroup() 

epaData5 <- epaData4 |>
  left_join(
    epaSeasonAvgs |> select(-c(season,PF, PA, PD)),
    by = join_by(team, season == season_new)
  ) |>
  mutate(
    cummean_off_epa_adj = ifelse(is.na(cummean_off_epa_adj), avg_off_epa_adj, cummean_off_epa_adj),
    cummean_off_pass_epa_adj = ifelse(is.na(cummean_off_pass_epa_adj), avg_off_pass_epa_adj, cummean_off_pass_epa_adj),
    cummean_off_rush_epa_adj = ifelse(is.na(cummean_off_rush_epa_adj), avg_off_rush_epa_adj, cummean_off_rush_epa_adj),
    cummean_def_epa_adj = ifelse(is.na(cummean_def_epa_adj), avg_def_epa_adj, cummean_def_epa_adj),
    cummean_def_pass_epa_adj = ifelse(is.na(cummean_def_pass_epa_adj), avg_def_pass_epa_adj, cummean_def_pass_epa_adj),
    cummean_def_rush_epa_adj = ifelse(is.na(cummean_def_rush_epa_adj), avg_def_rush_epa_adj, cummean_def_rush_epa_adj),
    PFG = ifelse(is.na(PFG), 0, PFG),
    PAG = ifelse(is.na(PAG), 0, PAG),
    MOV = ifelse(is.na(MOV), 0, MOV),
    SOS = ifelse(is.na(SOS), 0, SOS),
    SRS = ifelse(is.na(SRS), 0, SRS),
    OSRS = ifelse(is.na(OSRS), 0, OSRS),
    DSRS = ifelse(is.na(DSRS), 0, DSRS)
  ) |>
  select(-contains("avg"))


modelData <- gameDataMod |>
  # mutate(
  #   season = factor(season),
  #   week = factor(week)
  # ) |>
  select(
    game_id, season, week, season_type,
    home_team, home_score,
    away_team, away_score,
    location,
    home_rest, away_rest,
    div_game, 
    roof,
    surface,
    temp,
    wind,
    # For preds
    winner, 
    result, spread_line, spreadCover,
    home_spread_odds, away_spread_odds,
    home_spread_prob, away_spread_prob,
    total, total_line, totalCover,
    under_odds, over_odds,
    under_prob, over_prob,
    home_moneyline, away_moneyline,
    home_moneyline_prob, away_moneyline_prob
  ) |>
  left_join(
    epaData5 |> 
      select(game_id, team, contains("epa_adj")) |>
      rename_with(.cols = contains("epa_adj"), ~paste0("home_", .x)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    epaData5 |> 
      select(game_id, team, contains("epa_adj")) |>
      rename_with(.cols = contains("epa_adj"), ~paste0("away_", .x)),
    by = join_by(game_id, away_team == team)
  ) |>
  left_join(
    epaData5 |> 
      select(game_id, team, contains("epa_adj")) |>
      rename_with(.cols = contains("epa_adj"), ~paste0("home_", .x)),
    by = join_by(game_id, home_team == team)
  ) |>
  left_join(
    epaData5 |> 
      select(game_id, team, contains("epa_adj")) |>
      rename_with(.cols = contains("epa_adj"), ~paste0("away_", .x)),
    by = join_by(game_id, away_team == team)
  )

  
trainingData <- modelData |>
  mutate(
    home_pass_epa_adj_diff = home_off_pass_epa_adj + away_def_pass_epa_adj,
    home_rush_epa_adj_diff = home_off_rush_epa_adj + away_def_rush_epa_adj,
    away_pass_epa_adj_diff = away_off_pass_epa_adj + home_def_pass_epa_adj,
    away_rush_epa_adj_diff = away_off_rush_epa_adj + home_def_rush_epa_adj
  ) |> 
  select(
    -contains("off"),
    -contains("def")
  )

testData <- modelData |>
  mutate(
    home_pass_epa_adj_diff = home_cummean_off_pass_epa_adj + away_cummean_def_pass_epa_adj,
    home_rush_epa_adj_diff = home_cummean_off_rush_epa_adj + away_cummean_def_rush_epa_adj,
    away_pass_epa_adj_diff = away_cummean_off_pass_epa_adj + home_cummean_def_pass_epa_adj,
    away_rush_epa_adj_diff = away_cummean_off_rush_epa_adj + home_cummean_def_rush_epa_adj
  ) |> 
  select(
    -contains("off"),
    -contains("def")
  )

rmList <- ls()[!(ls() %in% c("gameDataMod", "gameDataLongMod", "pbpDataMod", "modelData", "trainingData", "testData"))]
rm(list = rmList)


# Fit ----
## Data ----
modelDataTrain <- trainingData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week <= 8) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x)})
  )

modelDataTest <- testData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week == 9) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x,
                             center = attr(modelDataTrain |> pull(x), "scaled:center"),
                             scale = attr(modelDataTrain |> pull(x), "scaled:scale"))})
  )

## Model ----
iters <- 3000
burn <- 1000
chains <- 2
sims <- (iters-burn)*chains

Fit <- brm(
  bf(mvbind(home_score, away_score) ~ 
       home_pass_epa_adj_diff +
       home_rush_epa_adj_diff +
       away_pass_epa_adj_diff +
       away_rush_epa_adj_diff +
       home_rest +
       away_rest +
       location +
       div_game +
       roof +
       #surface +
       (1 | gr(home_team, by = season)) +
       (1 | gr(away_team, by = season)) +
       (1 | week)
  ),
  data = modelDataTrain,
  family = brmsfamily(family = "gaussian"),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = TRUE,
  control = list(adapt_delta = 0.95)
)

fit <- 2
assign(paste0("fit", fit), Fit)

#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
# prior_summary(Fit)
# posterior_summary(Fit)
# launch_shinystan(Fit)
print(Fit, digits = 4)
fixedEff <- fixef(Fit)
fixedEff2 <- data.frame(fixedEff) |>
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
fixedEff2

icc(Fit)
variance_decomposition(Fit)

## PPC Plot ----
homePPC <- pp_check(Fit, resp = "homescore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Home PPC")) +
  theme_bw()

awayPPC <- pp_check(Fit, resp = "awayscore", ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Away PPC")) +
  theme_bw()

homePPC
awayPPC

PPCplot <- pp_check(Fit, ndraws = 100, 
                    type = "dens_overlay") + 
  labs(title = paste0("Fit", fit, " PPC")) +
  theme_bw()
PPCplot

PPCplotSeason <- pp_check(Fit, ndraws = 100, 
                          type = "dens_overlay_grouped",
                          group = "season") + 
  labs(title = paste0("Fit", fit, " PPC")) +
  theme_bw()
PPCplotSeason

PPCplotWeekHome <- pp_check(Fit, ndraws = 100, resp = "homescore",
                        type = "dens_overlay_grouped",
                        group = "week") + 
  labs(title = paste0("Fit", fit, " PPC Home")) +
  theme_bw()
PPCplotWeek + ylim(c(0,.05))

PPCplotWeekAway <- pp_check(Fit, ndraws = 100, resp = "awayscore",
                        type = "dens_overlay_grouped",
                        group = "week") + 
  labs(title = paste0("Fit", fit, " PPC Away")) +
  theme_bw()
PPCplotWeekAway + ylim(c(0,.05))

performance::check_distribution(Fit)
performance::check_outliers(Fit)
performance::check_heteroskedasticity(Fit)
performance_rmse(Fit)
performance_mae(Fit)
model_performance(Fit)

variance_decomposition(Fit)
ranef(Fit)

fit <- 2
Fit <- fit2
FitR2 <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())
FitR2

bayes_factor(Fit, fit19)
bayes_factor(fit15, fit13)
bayes_factor(fit13, fit15)
bayes_factor(fit9, Fit)
bayes_factor(fit12, fit8)
bayes_factor(fit8, fit7)

Fitloo <- loo(Fit)
Fitloo2 <- loo(fit13)
loo_compare(Fitloo, Fitloo2)

Fitsmooths <- conditional_smooths(Fit,
                                  method = "posterior_predict")
plot(Fitsmooths, 
     stype = "raster", 
     ask = FALSE,
     theme = theme(legend.position = "bottom"))
plot(Fitsmooths, 
     stype = "contour", 
     ask = FALSE,
     theme = theme(legend.position = "bottom"))

Fiteffects <- conditional_effects(Fit, 
                                  method = "posterior_predict",
                                  robust = FALSE)
plot(Fiteffects, 
     points = TRUE, 
     ask = FALSE)

## Prediction ----
### Datasets ----
modelDataTrainNA <- modelDataTrain |>
  filter(!is.na(home_SRS))

modelDataTestNA <- modelDataTest |>
  filter(!is.na(home_SRS))

### Errors ----
# Fit <- fit4
# fit <- 4
#### Home Score ----
## Fitted
homefinalFit <- posterior_predict(Fit, resp = "homescore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- posterior_predict(Fit, 
                                    resp = "homescore",
                                    newdata = modelDataTest,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
## Fitted
awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <- posterior_predict(Fit, 
                                    resp = "awayscore",
                                    newdata = modelDataTest,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = paste0("Fit", fit),
  home_MAE_fit = mean(abs(homefinalFitMean - modelDataTrain$home_score)),
  home_COV_fit = mean(homefinalFitLCB < modelDataTrain$home_score &  modelDataTrain$home_score < homefinalFitUCB),
  away_MAE_fit = mean(abs(awayfinalFitMean - modelDataTrain$away_score)),
  away_COV_fit = mean(awayfinalFitLCB < modelDataTrain$away_score &  modelDataTrain$away_score < awayfinalFitUCB),
  home_MAE_pred = mean(abs(homefinalPredsMean - modelDataTest$home_score), na.rm = TRUE),
  home_MAD_pred = mean(abs(homefinalPredsMed - modelDataTest$home_score), na.rm = TRUE),
  home_COV_pred = mean(homefinalPredsLCB < modelDataTest$home_score & modelDataTest$home_score < homefinalPredsUCB),
  away_MAE_pred = mean(abs(awayfinalPredsMean - modelDataTest$away_score), na.rm = TRUE),
  away_MAD_pred = mean(abs(awayfinalPredsMed - modelDataTest$away_score), na.rm = TRUE),
  away_COV_pred = mean(awayfinalPredsLCB < modelDataTest$away_score & modelDataTest$away_score < awayfinalPredsUCB)
)
predMetricsHA

#### Spread ----
Fitted <- homefinalFit - awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMean <- colMeans(Fitted)
FittedMed <- apply(Fitted, 2, function(x){quantile(x, 0.5)})
FittedLCB <- apply(Fitted, 2, function(x){quantile(x, 0.025)})
FittedUCB <- apply(Fitted, 2, function(x){quantile(x, 0.975)})

# Prediction
Preds <- homefinalPreds - awayfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMean <- colMeans(Preds)
PredsMed <- apply(Preds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCB <- apply(Preds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCB <- apply(Preds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- modelDataTrain$result
spreadTest <- modelDataTest$result
predMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_fit = mean(abs(FittedMean - spreadTrain)),
  MAD_fit = mean(abs(FittedMed - spreadTrain)),
  COV_fit = mean(FittedLCB < spreadTrain & spreadTrain < FittedUCB),
  MAE_pred = mean(abs(PredsMean - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMed - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCB < spreadTest & spreadTest < PredsUCB)
)
predMetrics

### Prob Errors ----
#### Fit ----
spreadLineTrain <- modelDataTrain$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbs <- matrix(, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- Fitted[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbs[, j] <- probs
}
FittedBet <- colMeans(FittedProbs)
FittedBetLogical <- FittedBet > 0.5
FittedResultLogical <- spreadTrain > spreadLineTrain
FittedResultProb <- mean(FittedBetLogical == FittedResultLogical, na.rm = TRUE)
FittedResultProb

spreadDataTrain <- modelDataTrain |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMean,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

#### Pred ----
spreadLineTest <- modelDataTest$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbs <- matrix(, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- Preds[, j]
  probs <- fitted > spreadLineTest[j]
  PredsProbs[, j] <- probs
}
PredsBet <- colMeans(PredsProbs)
PredsBetLogical <- PredsBet > 0.5
PredsResultLogical <- spreadTest > spreadLineTest
PredsResultProb <- mean(PredsBetLogical == PredsResultLogical, na.rm = TRUE)
PredsResultProb

spreadDataTest <- modelDataTest |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMean,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Combined Success ----
spreadSuccess <- bind_cols(
  Fit = paste0("Fit", fit),
  spreadSuccessTrain,
  spreadSuccessTest
)
spreadSuccess

#predMetricsComb <- data.frame()
predMetricsComb <- bind_rows(
  predMetricsComb,
  predMetrics
) |>
  arrange(MAE_pred)
predMetricsComb

#spreadSuccessComb <- data.frame()
spreadSuccessComb <- bind_rows(
  spreadSuccessComb,
  spreadSuccess
) |>
  arrange(desc(spreadOddsProbTest))
spreadSuccessComb


#looFits <- list()
# for(i in 1:fit){
#   looFits[[paste0("Fit",i)]] <- loo(get(paste0("fit", i)))
# }
looFits[[paste0("Fit",fit)]] <- loo(get(paste0("fit", fit)))
looComb <- loo_compare(looFits)
looComb

save(predMetricsComb, totalSuccessComb, looComb, fitFormulas,
     file = "./Data/resultFitDiagnostics.RData")




# Fit Iterative ----

## Data ----
modelWeeks <- modelData |> 
  filter(season == 2024) |>
  filter(complete.cases(result)) |>
  pull(week) |>
  unique()

fixedEffs <- list()
FitR2s <- data.frame()
predMetricsHAs <- data.frame()
PredsAll <- matrix(nrow = 4000)
predMetricsAll <- data.frame()
predMetricsComb <- data.frame()
spreadSuccessComb <- data.frame()

head(modelWeeks, n = length(modelWeeks)-1)
modelWeeks2 <- 2:11
library(cmdstanr)
## FOR LOOP ----
tic()
for(i in modelWeeks2){
modelDataTrain <- trainingData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week <= i) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x)})
  )

modelDataTest <- testData |>
  filter(complete.cases(result)) |>
  filter(season >= 2024) |>
  filter(week == (i+1)) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  season, week,
                                  winner, 
                                  result, spread_line, spreadCover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, totalCover,
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob),
           function(x){scale(x,
                             center = attr(modelDataTrain |> pull(x), "scaled:center"),
                             scale = attr(modelDataTrain |> pull(x), "scaled:scale"))})
  )

## Model ----
iters <- 3000
burn <- 1000
chains <- 2
sims <- (iters-burn)*chains

Fit <- brm(
  bf(mvbind(home_score, away_score) ~ 
       home_pass_epa_adj_diff +
       home_rush_epa_adj_diff +
       away_pass_epa_adj_diff +
       away_rush_epa_adj_diff +
       home_rest +
       away_rest +
       location +
       div_game +
       roof +
       #surface +
       (1 | gr(home_team, by = season)) +
       (1 | gr(away_team, by = season)) +
       (1 | week)
  ),
  data = modelDataTrain,
  family = brmsfamily(family = "gaussian"),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = TRUE,
  backend = "cmdstanr",
  # prior = c(
  #   set_prior("normal(0, 10)", class = "b"),  # Coefficients
  #   set_prior("normal(20, 10)", class = "Intercept")
  # ),
  control = list(adapt_delta = 0.95)
)

fit <- i
assign(paste0("fit", fit), Fit)

#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
#fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
# prior_summary(Fit)
# posterior_summary(Fit)
# launch_shinystan(Fit)
#print(Fit, digits = 4)
fixedEff <- fixef(Fit)
fixedEff2 <- data.frame(fixedEff) |>
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
print(fixedEff2)

fixedEffs[[paste0("Week ", (i + 1))]] <- fixedEff2


FitR2 <- bayes_R2(Fit, resp = c("homescore", "awayscore")) |>
  bind_cols(Fit = paste0("Week", (fit + 1)),
            Score = c("homescore", "awayscore")) |>
  select(Fit, Score, everything())
print(FitR2)
FitR2s <- bind_rows(FitR2s, FitR2)



### Errors ----
# Fit <- fit4
# fit <- 4
#### Home Score ----
## Fitted
homefinalFit <- posterior_predict(Fit, resp = "homescore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- posterior_predict(Fit, 
                                    resp = "homescore",
                                    newdata = modelDataTest,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
## Fitted
awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <- posterior_predict(Fit, 
                                    resp = "awayscore",
                                    newdata = modelDataTest,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = paste0("Week", (fit+1)),
  home_MAE_fit = mean(abs(homefinalFitMean - modelDataTrain$home_score)),
  home_COV_fit = mean(homefinalFitLCB < modelDataTrain$home_score &  modelDataTrain$home_score < homefinalFitUCB),
  away_MAE_fit = mean(abs(awayfinalFitMean - modelDataTrain$away_score)),
  away_COV_fit = mean(awayfinalFitLCB < modelDataTrain$away_score &  modelDataTrain$away_score < awayfinalFitUCB),
  home_MAE_pred = mean(abs(homefinalPredsMean - modelDataTest$home_score), na.rm = TRUE),
  home_MAD_pred = mean(abs(homefinalPredsMed - modelDataTest$home_score), na.rm = TRUE),
  home_COV_pred = mean(homefinalPredsLCB < modelDataTest$home_score & modelDataTest$home_score < homefinalPredsUCB),
  away_MAE_pred = mean(abs(awayfinalPredsMean - modelDataTest$away_score), na.rm = TRUE),
  away_MAD_pred = mean(abs(awayfinalPredsMed - modelDataTest$away_score), na.rm = TRUE),
  away_COV_pred = mean(awayfinalPredsLCB < modelDataTest$away_score & modelDataTest$away_score < awayfinalPredsUCB)
)
predMetricsHAs <- bind_rows(predMetricsHAs, predMetricsHA)

#### Spread ----
Fitted <- homefinalFit - awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMean <- colMeans(Fitted)
FittedMed <- apply(Fitted, 2, function(x){quantile(x, 0.5)})
FittedLCB <- apply(Fitted, 2, function(x){quantile(x, 0.025)})
FittedUCB <- apply(Fitted, 2, function(x){quantile(x, 0.975)})

# Prediction
Preds <- homefinalPreds - awayfinalPreds
PredsAll <- cbind(PredsAll, Preds)
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMean <- colMeans(Preds)
PredsMed <- apply(Preds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCB <- apply(Preds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCB <- apply(Preds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- modelDataTrain$result
spreadTest <- modelDataTest$result
predMetrics <- tibble(
  Fit = paste0("Week", (fit+1)),
  MAE_fit = mean(abs(FittedMean - spreadTrain)),
  MAD_fit = mean(abs(FittedMed - spreadTrain)),
  COV_fit = mean(FittedLCB < spreadTrain & spreadTrain < FittedUCB),
  MAE_pred = mean(abs(PredsMean - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMed - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCB < spreadTest & spreadTest < PredsUCB)
)
predMetricsAll <- bind_rows(predMetricsAll, predMetrics)

### Prob Errors ----
#### Fit ----
spreadLineTrain <- modelDataTrain$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbs <- matrix(, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- Fitted[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbs[, j] <- probs
}
FittedBet <- colMeans(FittedProbs)
FittedBetLogical <- FittedBet > 0.5
FittedResultLogical <- spreadTrain > spreadLineTrain
FittedResultProb <- mean(FittedBetLogical == FittedResultLogical, na.rm = TRUE)
FittedResultProb

spreadDataTrain <- modelDataTrain |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMean,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
#spreadSuccessTrain

#### Pred ----
spreadLineTest <- modelDataTest$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbs <- matrix(, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- Preds[, j]
  probs <- fitted > spreadLineTest[j]
  PredsProbs[, j] <- probs
}
PredsBet <- colMeans(PredsProbs)
PredsBetLogical <- PredsBet > 0.5
PredsResultLogical <- spreadTest > spreadLineTest
PredsResultProb <- mean(PredsBetLogical == PredsResultLogical, na.rm = TRUE)
#PredsResultProb

spreadDataTest <- modelDataTest |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMean,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Combined Success ----
spreadSuccess <- bind_cols(
  Fit = paste0("Week", (fit+1)),
  spreadSuccessTrain,
  spreadSuccessTest
)
#spreadSuccess

#predMetricsComb <- data.frame()
predMetricsComb <- bind_rows(
  predMetricsComb,
  predMetrics
) 
print(predMetricsComb)

#spreadSuccessComb <- data.frame()
spreadSuccessComb <- bind_rows(
  spreadSuccessComb,
  spreadSuccess
) 
print(spreadSuccessComb)

}
toc()

## Check Performance ----
fixedEffs
FitR2s 
predMetricsHAs
PredsAll
predMetricsComb
spreadSuccessComb

save(fixedEffs,
     FitR2s,
     predMetricsHAs,
     PredsAll,
     predMetricsComb,
     spreadSuccessComb,
     file = "./Model Fitting/Data/Non Adj EPA Performance.RData")



#### Fit ----
spreadLineTrain <- modelDataTrain$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbs <- matrix(, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- Fitted[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbs[, j] <- probs
}
FittedBet <- colMeans(FittedProbs)
FittedBetLogical <- FittedBet > 0.5
FittedResultLogical <- spreadTrain > spreadLineTrain
FittedResultProb <- mean(FittedBetLogical == FittedResultLogical, na.rm = TRUE)
FittedResultProb

spreadDataTrain <- modelDataTrain |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMean,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBet,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
#spreadSuccessTrain

#### Pred ----
modelDataTest2 <- testData |>
  filter(complete.cases(result)) |>
  filter(season == 2024) |>
  filter(week >= 3)

Preds2 <- PredsAll[,-1]

PredsMean2 <- colMeans(Preds2)
PredsMed2 <- apply(Preds2, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCB2 <- apply(Preds2, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCB2 <- apply(Preds2, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTest2 <- modelDataTest2$result
predMetrics2 <- tibble(
  #Fit = paste0("Week", (fit+1)),
  #MAE_fit = mean(abs(FittedMean - spreadTrain)),
  #MAD_fit = mean(abs(FittedMed - spreadTrain)),
  #COV_fit = mean(FittedLCB < spreadTrain & spreadTrain < FittedUCB),
  MAE_pred = mean(abs(PredsMean2 - spreadTest2), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMed2 - spreadTest2), na.rm = TRUE),
  COV_pred = mean(PredsLCB2 < spreadTest2 & spreadTest2 < PredsUCB2)
)
predMetrics2

spreadLineTest2 <- modelDataTest2$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbs2 <- matrix(, nrow = sims, ncol = length(spreadLineTest2))
for(j in 1:length(spreadLineTest2)){
  fitted <- Preds2[, j]
  probs <- fitted > spreadLineTest2[j]
  PredsProbs2[, j] <- probs
}
PredsBet2 <- colMeans(PredsProbs2)
PredsBetLogical2 <- PredsBet2 > 0.5
PredsResultLogical2 <- spreadTest2 > spreadLineTest2
PredsResultProb2 <- mean(PredsBetLogical2 == PredsResultLogical2, na.rm = TRUE)
PredsResultProb2

spreadDataTest2 <- modelDataTest2 |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMean2,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBet2,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE, 
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )

spreadSuccessTest2 <- spreadDataTest2 |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest2


week3 <- posterior_epred(fit2, 
                         newdata = testData |>
                           filter(complete.cases(result)) |>
                           filter(season == 2024) |>
                           filter(week == 3),
                         allow_new_levels = TRUE)
week3home <- colMeans(week3[,,1])
week3away <- colMeans(week3[,,2])

predsE <- data.frame()
for(j in 2:11){
  new_fit <- get(paste0("fit",j))
  new_epred <- posterior_predict(new_fit, 
                               newdata = testData |>
                                 filter(complete.cases(result)) |>
                                 filter(season == 2024) |>
                                 filter(week == (j+1)),
                               allow_new_levels = TRUE)
  new_preds <- data.frame(
    "home_score_pred" = colMeans(new_epred[,,1]),
    "away_score_pred" = colMeans(new_epred[,,2]),
    week = j+1
  )
  predsE <- bind_rows(predsE, new_preds)
}

new_preds2 <- data.frame(
  "home_score_pred" = mean(new_epred[,,1]),
  "away_score_pred" = mean(new_epred[,,2]),
  week = j+1
)

predsE <- bind_rows(predsE, new_preds2)
predsE <- predsE |>
  mutate(result_pred = home_score_pred - away_score_pred)

predsE2 <- testData |>
  filter(complete.cases(result)) |>
  filter(season == 2024) |>
  filter(week %in% 3:12) |>
  bind_cols(
    predsE |> select(-week)
  ) |>
  select(
    game_id, season, week,
    home_team, home_score, home_score_pred,
    away_team, away_score, away_score_pred,
    result, result_pred, spread_line, spreadCover
  ) |>
  mutate(
    spreadBet = ifelse(result_pred > spread_line, TRUE, FALSE)
  )

weekPerf <- predsE2 |>
  group_by(week) |>
  summarise(
    success = mean(spreadBet == spreadCover, na.rm = TRUE)
  )
weekPerf

totalPerf <- predsE2 |>
  summarise(
    success = mean(spreadBet == spreadCover, na.rm = TRUE)
  )
totalPerf


### PPC
Preds2LCB50 <- apply(Preds2, 2, function(x){quantile(x, 0.25, na.rm = TRUE)})
Preds2UCB50 <- apply(Preds2, 2, function(x){quantile(x, 0.75, na.rm = TRUE)})
PPDdf2 <- modelDataTest2 |>
  mutate(
    Index = 1:length(PredsMean2),
    Mean = PredsMean2,
    Median = PredsMed2,
    LCB = PredsLCB2,
    UCB = PredsUCB2,
    LCB50 = Preds2LCB50,
    UCB50 = Preds2UCB50
  )

PPDplot2 <- ppc_dens_overlay(
  y = modelDataTest2$result, 
  yrep = Preds2) +
  labs(title = paste0("Fit", " PPD")) +
  theme_bw()
PPDplot2

ggplot(data = PPDdf2, aes(x = Index)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), fill = "lightblue3", alpha = 0.2) +
  geom_ribbon(aes(ymin = LCB50, ymax = UCB50), fill = "lightblue3", alpha = 0.4) +
  geom_line(aes(y = result, color = "Observed")) +
  geom_line(aes(y = Mean, color = "PPD Mean")) +
  #facet_wrap(vars(StormID))+#, ncol = 6)+
  labs(title = paste0("Fit", fit, "PPD")) +
  scale_color_manual(name = NULL, values = c("black","red")) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  theme_bw()



install.packages("cm")
library(cmdstanr)
cmdstanr::install_cmdstan()

