# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
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
library(patchwork)

## Modeling ----
library(pracma)
library(zoo)
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
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)

## NFL Verse ----
library(nflverse)

## Tidyverse ----
library(tidyverse)

# Setwd
setwd("Model Fitting")

# Load Data ----
## Teams Data 
teamsData <- load_teams(current = FALSE)

##  Game/Schedule Data
gameData <- load_schedules(seasons = TRUE) |>
  filter(season >= 2002) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

gameDataLong <- gameData |>
  clean_homeaway(invert = c("result", "spread_line")) 

## Standings
#load(file = "./_data/seasonStandings.RData")
load(file = "../app/_data/seasonWeekStandings.RData")

## Rosters
rostersWeekData <- load_rosters_weekly(seasons = TRUE) |>
  filter(season >= 2002) 

## Injuries
injuryData <- load_injuries(seasons = TRUE) |>
  filter(season >= 2002) 

injurySnippet <- injuryData |>
  filter(season == 2024) |>
  filter(week == 1)

## Player Stats 
playerOffenseData <- load_player_stats(seasons = TRUE, stat_type = "offense") |>
  filter(season >= 2002) 

playerOffenseSnippet <- playerOffenseData |>
  filter(season == 2024) |>
  filter(week == 1)

playerDefenseData <- load_player_stats(seasons = TRUE, stat_type = "defense") |>
  filter(season >= 2002) 

playerDefenseSnippet <- playerDefenseData |>
  filter(season == 2024) |>
  filter(week == 1)

playerKickingData <- load_player_stats(seasons = TRUE, stat_type = "kicking") |>
  filter(season >= 2002) 

playerKickingSnippet <- playerKickingData |>
  filter(season == 2024) |>
  filter(week == 1)

## Next Gen
nextGenData <- load_nextgen_stats(seasons = TRUE) |>
  filter(season >= 2002)

nextGenSnippet <- nextGenData |>
  filter(season == 2024) |>
  filter(week == 1)

## play by play
pbpData <- load_pbp(seasons = TRUE) |>
  filter(season >= 2002)

# EPA ----
## Player level calc ----
epaCalc <- calculate_player_stats(pbpDataSnippet, weekly = TRUE)
epaCalc2 <- playerOffenseData |>
  filter(season == 2024) |>
  filter(week <= 2) |>
  filter(opponent_team == "BAL")

epaCalc3 <- pbpDataSnippet |>
  filter(play_type == "pass") |>
  filter(posteam == "KC") |>
  summarise(pass_epa = sum(epa, na.rm = TRUE))

## Game Level calc ----
unique(pbpData$play_type)
unique(pbpData$play_type_nfl)

### Passing ----
tic()
epaPass <- pbpData |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "pass") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_pass_epa_total = sum(epa),
    off_pass_epa = mean(epa),
    off_pass_wpa_total = sum(vegas_wpa),
    off_pass_wpa = mean(vegas_wpa),
    off_pass_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_pass_epa_adj = mean(epa*scaled_vegas_wp),
    def_pass_epa_total = sum(-epa),
    def_pass_epa = mean(-epa),
    def_pass_wpa_total = sum(-vegas_wpa),
    def_pass_wpa = mean(-vegas_wpa),
    def_pass_epa_adj_total = sum(-epa*scaled_vegas_wp),
    def_pass_epa_adj = mean(-epa*scaled_vegas_wp)
  ) |>
  ungroup()
toc()

### Rushing ----
tic()
epaRush <- pbpData |>
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  filter(play_type == "run") |> 
  group_by(game_id,season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_rush_epa_total = sum(epa),
    off_rush_epa = mean(epa),
    off_rush_wpa_total = sum(vegas_wpa),
    off_rush_wpa = mean(vegas_wpa),
    off_rush_epa_adj_total = sum(epa*scaled_vegas_wp),
    off_rush_epa_adj = mean(epa*scaled_vegas_wp),
    def_rush_epa_total = sum(-epa),
    def_rush_epa = mean(-epa),
    def_rush_wpa_total = sum(-vegas_wpa),
    def_rush_wpa = mean(-vegas_wpa),
    def_rush_epa_adj_total = sum(-epa*scaled_vegas_wp),
    def_rush_epa_adj = mean(-epa*scaled_vegas_wp)
  ) |>
  ungroup()
toc()

### Combined ----
epaData <- left_join(
  epaPass,
  epaRush,
  by = join_by(game_id, season, week, posteam, home_team, away_team)
)

### Merge with Game Data ----
epaGameData <- left_join(
  gameData,
  epaData |> 
    # select(game_id, season, week, posteam,-home_team, -away_team,
    #                 contains("epa_adj")) |>
    select(-home_team, -away_team) |>
    rename_with(~paste0("home_", .x), .cols = contains("off")) |>
    rename_with(~paste0("away_", .x), .cols = contains("def")),
  by = join_by(game_id, season, week, "home_team" == "posteam")) |>
  left_join(
    epaData |> 
      # select(game_id, season, week, posteam,-home_team, -away_team,
      #                 contains("epa_adj")) |>
      select(-home_team, -away_team) |>
      rename_with(~paste0("away_", .x), .cols = contains("off")) |>
      rename_with(~paste0("home_", .x), .cols = contains("def")),
    by = join_by(game_id, season, week, "away_team" == "posteam"))

### Merge SRS data ----
epaGameData <- epaGameData |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team, 
             home_MOV = MOV, 
             home_SOS = SOS, 
             home_SRS = SRS, 
             home_OSRS = OSRS, 
             home_DSRS = DSRS),
    by = join_by(season, week, "home_team" == "team")
  ) |>
  left_join(
    seasonWeekStandings |> 
      select(season, week, team, 
             away_MOV = MOV, 
             away_SOS = SOS, 
             away_SRS = SRS, 
             away_OSRS = OSRS, 
             away_DSRS = DSRS),
    by = join_by(season, week, "away_team" == "team")
  )

epaGameDataLong <- epaGameData |>
  clean_homeaway()


## EPA trends ----
epaGameDataLag <- epaGameData |>
  select(
    game_id, season, week, game_type, 
    home_team, home_score,
    away_team, away_score,
    home_rest, away_rest,
    div_game, 
    roof, 
    surface, 
    temp, 
    wind,
    home_SRS,
    away_SRS,
    home_off_pass_epa_adj,
    home_off_rush_epa_adj,
    home_def_pass_epa_adj,
    home_def_rush_epa_adj,
    away_off_pass_epa_adj,
    away_off_rush_epa_adj,
    away_def_pass_epa_adj,
    away_def_rush_epa_adj
  )

### No Moving Avg ----
epaGameDataLongLag <- epaGameDataLag |>
  clean_homeaway() |>
  group_by(season, team) |>
  mutate(
    team_SRS = lag(team_SRS),
    # opponent_SRS = lag(opponent_SRS),
    team_off_pass_epa_adj = lag(team_off_pass_epa_adj),
    team_off_rush_epa_adj = lag(team_off_rush_epa_adj),
    team_def_pass_epa_adj = lag(team_def_pass_epa_adj),
    team_def_rush_epa_adj = lag(team_def_rush_epa_adj)
    # opponent_off_pass_epa_adj = lag(opponent_off_pass_epa_adj),
    # opponent_off_rush_epa_adj = lag(opponent_off_rush_epa_adj),
    # opponent_def_pass_epa_adj = lag(opponent_def_pass_epa_adj),
    # opponent_def_rush_epa_adj = lag(opponent_def_rush_epa_adj)
  ) |>
  ungroup()

epaGameDataLag <- epaGameDataLag |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(location == "home") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) |>
  left_join(epaGameDataLongLag |>
              filter(location == "away") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) 

### Moving Avg ----
window <- 8
epaGameDataLongLag <- epaGameDataLag |>
  clean_homeaway() |>
  group_by(team) |>
  mutate(
    team_SRS = lag(team_SRS),
    # opponent_SRS = lag(opponent_SRS),
    team_off_pass_epa_adj = lag(team_off_pass_epa_adj),
    team_off_rush_epa_adj = lag(team_off_rush_epa_adj),
    team_def_pass_epa_adj = lag(team_def_pass_epa_adj),
    team_def_rush_epa_adj = lag(team_def_rush_epa_adj)
    # opponent_off_pass_epa_adj = lag(opponent_off_pass_epa_adj),
    # opponent_off_rush_epa_adj = lag(opponent_off_rush_epa_adj),
    # opponent_def_pass_epa_adj = lag(opponent_def_pass_epa_adj),
    # opponent_def_rush_epa_adj = lag(opponent_def_rush_epa_adj)
  ) |>
  mutate(
    team_SRS_mov = ifelse(is.na(team_SRS), lag(team_SRS), team_SRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj), lag(team_off_pass_epa_adj), team_off_pass_epa_adj),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj), lag(team_off_rush_epa_adj), team_off_rush_epa_adj),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj), lag(team_def_pass_epa_adj), team_def_pass_epa_adj),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj), lag(team_def_rush_epa_adj), team_def_rush_epa_adj)
  ) |>
  mutate(
    team_SRS_mov = ifelse(is.na(team_SRS_mov), 0, team_SRS),
    team_off_pass_epa_adj_mov = ifelse(is.na(team_off_pass_epa_adj_mov), 0, team_off_pass_epa_adj_mov),
    team_off_rush_epa_adj_mov = ifelse(is.na(team_off_rush_epa_adj_mov), 0, team_off_rush_epa_adj_mov),
    team_def_pass_epa_adj_mov = ifelse(is.na(team_def_pass_epa_adj_mov), 0, team_def_pass_epa_adj_mov),
    team_def_rush_epa_adj_mov = ifelse(is.na(team_def_rush_epa_adj_mov), 0, team_def_rush_epa_adj_mov)
  ) |>
  mutate(
    team_SRS_mov = movavg(team_SRS_mov, n = window, type = "r"),
    team_off_pass_epa_adj_mov = movavg(team_off_pass_epa_adj_mov, n = window, type = "r"),
    team_off_rush_epa_adj_mov = movavg(team_off_rush_epa_adj_mov, n = window, type = "r"),
    team_def_pass_epa_adj_mov = movavg(team_def_pass_epa_adj_mov, n = window, type = "r"),
    team_def_rush_epa_adj_mov = movavg(team_def_rush_epa_adj_mov, n = window, type = "r")
  ) |>
  ungroup()

# window <- 8
# BALsrs <- epaGameDataLongLag |> 
#   filter(season %in% 2022:2024) |>
#   filter(team == "BAL") |>
#   select(game_id, season, week, game_type, team, team_SRS) |>
#   mutate(team_SRS_mov = ifelse(is.na(team_SRS), lag(team_SRS, n = 1), team_SRS)) |>
#   filter(season %in% 2023:2024) |>
#   mutate(movS = movavg(team_SRS_mov, 8, type = "s")) |>
#   mutate(movT = movavg(team_SRS_mov, 8, type = "t")) |>
#   mutate(movW = movavg(team_SRS_mov, 8, type = "w")) |>
#   mutate(movM = movavg(team_SRS_mov, 8, type = "m")) |>
#   mutate(movE = movavg(team_SRS_mov, 8, type = "e")) |>
#   mutate(movR = movavg(team_SRS_mov, 8, type = "r"))
# BALsrs

epaGameDataLag <- epaGameDataLag |>
  select(
    game_id, season, week, game_type,
    home_team, home_score, away_team, away_score, home_rest, away_rest,
    div_game, roof, surface, temp, wind
  ) |>
  left_join(epaGameDataLongLag |>
              filter(location == "home") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "home")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) |>
  left_join(epaGameDataLongLag |>
              filter(location == "away") |>
              select(game_id, contains("team")) |>
              rename_with(.cols = contains("team"), ~str_replace(.x, "team", "away")) |>
              select(game_id, contains("SRS"), contains("epa")),
            by = join_by(game_id)) 



# Modelling -----
## Clean Data ----
colnames(epaGameDataLag)
modelDataFull <- epaGameDataLag |>
  select(
    game_id, season, week, game_type, 
    home_team, home_score,
    away_team, away_score,
    home_rest, away_rest,
    div_game, 
    roof, 
    surface, 
    temp, 
    wind,
    home_SRS, home_SRS_mov,
    away_SRS, away_SRS_mov,
    home_off_pass_epa_adj, home_off_pass_epa_adj_mov,
    home_off_rush_epa_adj, home_off_rush_epa_adj_mov,
    home_def_pass_epa_adj, home_def_pass_epa_adj_mov,
    home_def_rush_epa_adj, home_def_rush_epa_adj_mov,
    away_off_pass_epa_adj, away_off_pass_epa_adj_mov,
    away_off_rush_epa_adj, away_off_rush_epa_adj_mov,
    away_def_pass_epa_adj, away_def_pass_epa_adj_mov,
    away_def_rush_epa_adj, away_def_rush_epa_adj_mov
  ) |>
  left_join(
    gameData |> 
      select(
        game_id,
        result,
        spread_line, home_spread_odds, away_spread_odds,
        total, 
        total_line, under_odds, over_odds,
        home_moneyline, away_moneyline
      )
  ) |>
  mutate(
    home_spread_prob = ifelse(home_spread_odds < 0, 
                              abs(home_spread_odds)/(abs(home_spread_odds) + 100),
                              100/(home_spread_odds + 100)),
    .after = home_spread_odds
  ) |>
  mutate(
    away_spread_prob = ifelse(away_spread_odds < 0, 
                              abs(away_spread_odds)/(abs(away_spread_odds) + 100),
                              100/(away_spread_odds + 100)),
    .after = away_spread_odds
  ) |>
  mutate(
    under_prob = ifelse(under_odds < 0, 
                        abs(under_odds)/(abs(under_odds) + 100),
                        100/(under_odds + 100)),
    .after = under_odds
  ) |>
  mutate(
    over_prob = ifelse(over_odds < 0, 
                       abs(over_odds)/(abs(over_odds) + 100),
                       100/(over_odds + 100)),
    .after = over_odds
  ) |>
  mutate(
    home_moneyline_prob = ifelse(home_moneyline < 0, 
                                 abs(home_moneyline)/(abs(home_moneyline) + 100),
                                 100/(home_moneyline + 100)),
    .after = home_moneyline
  ) |>
  mutate(
    away_moneyline_prob = ifelse(away_moneyline < 0, 
                                 abs(away_moneyline)/(abs(away_moneyline) + 100),
                                 100/(away_moneyline + 100)),
    .after = away_moneyline
  ) |>
  mutate(
    cover = ifelse(result > spread_line, 1, 
                   ifelse(result < spread_line, 0, NA)),
    .after = spread_line
  )
colnames(modelDataFull)

# Lag data
modelDataFullLong <- modelDataFull |>
  clean_homeaway() 

# Complete Cases
modelDataFull |> summarise(across(everything(), ~sum(complete.cases(.x)))) |> t()

modelDataFull |>
  filter(complete.cases(home_score, away_score)) |> 
  filter(!(season == 2024 & week > 9)) |> 
  summarise(across(everything(), ~sum(complete.cases(.x)))) |> t()

# Make factors
modelData <- modelDataFull |>
  filter(complete.cases(home_score, away_score)) |> 
  filter(!(season == 2024 & week > 9)) |>
  mutate(
    season = factor(season, ordered = TRUE),
    week = factor(week, ordered = TRUE),
    game_type_bin = factor(game_type,
                           levels = c("REG", "WC", "DIV", "CON", "SB"),
                           ordered = TRUE),
    game_type = factor(game_type,
                       levels = c("REG", "WC", "DIV", "CON", "SB"),
                       labels = c("REG", "POST", "POST", "POST", "POST"),
                       ordered = TRUE),
    home_team = factor(home_team),
    away_team = factor(away_team),
    div_game = factor(div_game,
                      levels = c(0,1),
                      labels = c("No", "Yes")),
    roof = factor(roof),
    surface = factor(surface)
  ) |>
  mutate(
    temp2 = ifelse(is.na(temp), 70, temp), .after = temp
  ) |>
  mutate(
    wind2 = ifelse(is.na(wind), 0, wind), .after = wind
  ) |>
  select(game_id, season, week, game_type, game_type_bin, everything())

## Plot ----
### Scatter ----
ggplot(data = modelData, aes(x = home_score, y = away_score)) +
  geom_point(alpha = 0.1) +
  geom_density2d() +
  theme_bw()

### Histogram ----
ggplot(data = modelData) +
  geom_histogram(
    aes(x = home_score, after_stat(density)),
    color = "lightblue3", fill = "lightblue") +
  geom_density(
    aes(x = home_score),
    color = "lightblue4", 
    linewidth = 1) +
  scale_x_continuous(limits = c(0,70), breaks = seq(0,70,7)) +
  facet_wrap(vars(game_type), nrow = 1) +
  theme_bw() +
  ggplot(data = modelData) +
  geom_histogram(
    aes(x = away_score, after_stat(density)),
    color = "pink3", fill = "pink") +
  geom_density(
    aes(x = away_score),
    color = "pink4", 
    linewidth = 1) +
  scale_x_continuous(limits = c(0,70), breaks = seq(0,70,7)) +
  facet_wrap(vars(game_type), nrow = 1) +
  theme_bw() +
  plot_layout(ncol = 1, axes = "collect")

## Split data ----
seasons <- sort(unique(as.numeric(as.character(modelData$season))))
seasonsTrain <- tail(seasons, 3) - 1
seasonsTest <- tail(seasons, 1)

modelDataTrain <- modelData |>
  filter(season %in% seasonsTrain) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  result, spread_line, cover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, 
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob
    ),
    function(x){scale(x)})
  )

str(modelDataTrain)

table(modelDataTrain$roof, modelDataTrain$game_type)

modelDataTest <- modelData |>
  filter(season %in% seasonsTest) |>
  mutate(
    across(where(is.numeric) & !c(home_score, away_score,
                                  result, spread_line, cover,
                                  home_spread_odds, away_spread_odds,
                                  home_spread_prob, away_spread_prob,
                                  total, total_line, 
                                  under_odds, over_odds,
                                  under_prob, over_prob,
                                  home_moneyline, away_moneyline,
                                  home_moneyline_prob, away_moneyline_prob
    ),
    function(x){scale(x,
                      center = attr(modelDataTrain |> pull(x), "scaled:center"),
                      scale = attr(modelDataTrain |> pull(x), "scaled:scale"))})
  ) 
str(gameDataTest)

save(list = ls(), file = "../../NFL Analysis Data/ModelData.RData")

## Fit ----
### Model ----
iters <- 5000
burn <- 1000
chains <- 1
sims <- (iters-burn)*chains

Fit <- brm(
  bf(cover ~ 
       #(1|H|home_team) + (1|A|away_team) +
       #(1|home_team) + (1|away_team) +
       # home_team + away_team +
       home_rest + away_rest +
       home_SRS_mov + away_SRS_mov +
       home_off_pass_epa_adj_mov + away_def_pass_epa_adj_mov +
       home_off_rush_epa_adj_mov + away_def_rush_epa_adj_mov +
       home_def_pass_epa_adj_mov + away_off_pass_epa_adj_mov +
       home_def_rush_epa_adj_mov + away_off_rush_epa_adj_mov +
       game_type +
       div_game +
       #roof +
       #surface +
       temp2 +
       wind2 +
       (1|season) + 
       (1|week)
  ), # +
    #set_rescor(rescor = FALSE),
  data = modelDataTrain,
  family = binomial(link = "logit"),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = FALSE,
  control = list(adapt_delta = 0.95)
)

fit <- 9
assign(paste0("fit", fit), Fit)

# fitFormulas <- list(
#   Fit1 = fit1$formula,
#   Fit2 = fit2$formula,
#   Fit3 = fit3$formula,
#   Fit4 = fit4$formula,
#   Fit5 = fit5$formula
# )
fitFormulas <- list()
for(i in 1:fit){
  fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
}

fileLoc <- "~/Desktop/NFL Analysis Data/" 
save(Fit33,
     file = paste0(fileLoc, "Fit", fit, ".RData"))

### Diagnostics ----
prior_summary(Fit)
posterior_summary(Fit)
launch_shinystan(Fit)

print(Fit, digits = 4)

plot(Fit)
coverPPC <- pp_check(Fit, ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Cover PPC")) +
  theme_bw()

coverPPC

Fitloo <- loo(Fit)
waic(Fit)
performance::check_distribution(Fit)
performance::check_outliers(Fit)
performance::check_heteroskedasticity(Fit)
performance_rmse(Fit)
performance_mae(Fit)
model_performance(Fit)

variance_decomposition(Fit)
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
ranef(Fit)


FitR2 <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())

bayes_factor(Fit, fit1)
bayes_factor(Fit, fit2)

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

### Prediction ----
#### Datasets ----
modelDataTrainNA <- modelDataTrain |>
  filter(!is.na(cover)) 

modelDataTestNA <- modelDataTest |>
  filter(!is.na(cover))

#### Home Score ----
## Fitted
homefinalFit <- posterior_predict(Fit)
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- posterior_predict(Fit, 
                                    newdata = modelDataTestNA,
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
                                    newdata = modelDataTestNA,
                                    allow_new_levels = TRUE, 
                                    re_formula = NULL
)
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetrics <- tibble(
  Fit = paste0("Fit", fit),
  home_MAE_fit = mean(abs(homefinalFitMean - modelDataTrainNA$home_score)),
  home_COV_fit = mean(homefinalFitLCB < modelDataTrainNA$home_score &  modelDataTrainNA$home_score < homefinalFitUCB),
  away_MAE_fit = mean(abs(awayfinalFitMean - modelDataTrainNA$away_score)),
  away_COV_fit = mean(awayfinalFitLCB < modelDataTrainNA$away_score &  modelDataTrainNA$away_score < awayfinalFitUCB),
  home_MAE_pred = mean(abs(homefinalPredsMean - modelDataTestNA$home_score), na.rm = TRUE),
  home_MAD_pred = mean(abs(homefinalPredsMed - modelDataTestNA$home_score), na.rm = TRUE),
  home_COV_pred = mean(homefinalPredsLCB < modelDataTestNA$home_score & modelDataTestNA$home_score < homefinalPredsUCB),
  away_MAE_pred = mean(abs(awayfinalPredsMean - modelDataTestNA$away_score), na.rm = TRUE),
  away_MAD_pred = mean(abs(awayfinalPredsMed - modelDataTestNA$away_score), na.rm = TRUE),
  away_COV_pred = mean(awayfinalPredsLCB < modelDataTestNA$away_score & modelDataTestNA$away_score < awayfinalPredsUCB)
)
predMetrics

#### Spread ----
spreadFit <- homefinalFit - awayfinalFit
spreadFitMean <- colMeans(spreadFit)
spreadFitMed <- apply(spreadFit, 2, function(x){quantile(x, 0.5)})
spreadFitLCB <- apply(spreadFit, 2, function(x){quantile(x, 0.025)})
spreadFitUCB <- apply(spreadFit, 2, function(x){quantile(x, 0.975)})

spreadPreds <- homefinalPreds - awayfinalPreds
spreadPredsMean <- colMeans(spreadPreds)
spreadPredsMed <- apply(spreadPreds, 2, function(x){quantile(x, 0.5)})
spreadPredsLCB <- apply(spreadPreds, 2, function(x){quantile(x, 0.025)})
spreadPredsUCB <- apply(spreadPreds, 2, function(x){quantile(x, 0.975)})

gameTrainResult <- modelDataTrainNA$result
gameTestResult <- modelDataTestNA$result
spreadPredMetrics <- tibble(
  Fit = paste0("Fit", fit),
  spread_MAE_fit = mean(abs(spreadFitMean - gameTrainResult)),
  spread_MAD_fit = mean(abs(spreadFitMed - gameTrainResult), na.rm = TRUE),
  spread_COV_fit = mean(spreadFitLCB < gameTrainResult & gameTrainResult < spreadFitUCB),
  spread_MAE_pred = mean(abs(spreadPredsMean - gameTestResult), na.rm = TRUE),
  spread_MAD_pred = mean(abs(spreadPredsMed - gameTestResult), na.rm = TRUE),
  spread_COV_pred = mean(spreadPredsLCB < gameTestResult & gameTestResult < spreadPredsUCB)
)
spreadPredMetrics

##### Probabilities ----
###### Fit ----
gameTrainSpread <- modelDataTrainNA$cover
gameTrainSpreadMat <- matrix(gameTrainSpread, nrow = 1)

spreadFitprobs <- matrix(NA, nrow = sims, ncol = length(gameTrainSpread))
for(i in 1:sims){
  spread_fit <- homefinalFit[i, ]
  spread_fit <- ifelse(spread_fit == 1, "Home", "Away")
  probs <- spread_fit == gameTrainSpread
  spreadFitprobs[i, ] <- probs
}
spreadFitBet <- colMeans(spreadFitprobs)
spreadFitBetLogical <- spreadFitBet > 0.5
spreadFitResultLogical <- ifelse(spreadFitBetLogical, "Home", "Away")
spreadFitResultProb <- mean(gameTrainSpread == spreadFitResultLogical)
spreadFitResultProb

spreadDataTrain <- modelDataTrainNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, cover,
         home_spread_odds, home_spread_prob,
         away_spread_odds, away_spread_prob) |>
  mutate(
    spreadCoverProb = spreadFitBet,
    spreadSuccess = spreadCoverProb > 0.5,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, "Home", 
                            ifelse(1 - spreadCoverProb > away_spread_prob, "Away", NA)),
    spreadCoverSuccess = spreadCoverBet == cover
  )

spreadTrainSum <- spreadDataTrain |>
  summarise(
    Model = paste0("Fit", fit),
    Fit = "Train",
    spreadSuccesProb = mean(spreadSuccess, na.rm = TRUE),
    spreadCoverProb = mean(spreadCoverSuccess, na.rm = TRUE)
  )

###### Pred ----
gameTestSpread <- modelDataTestNA$cover
gameTestSpreadMat <- matrix(gameTestSpread, nrow = 1)

spreadPredsprobs <- matrix(NA, nrow = sims, ncol = length(gameTestSpread))
for(i in 1:sims){
  spread_fit <- homefinalPreds[i, ]
  spread_fit <- ifelse(spread_fit == 1, "Home", "Away")
  probs <- spread_fit == gameTestSpread
  spreadPredsprobs[i, ] <- probs
}
spreadPredsBet <- colMeans(spreadPredsprobs)
spreadPredsBetLogical <- spreadPredsBet > 0.5
spreadPredsResultLogical <- ifelse(spreadPredsBetLogical, "Home", "Away")
spreadPredsResultProb <- mean(gameTestSpread == spreadPredsResultLogical)
spreadPredsResultProb

spreadDataTest <- modelDataTestNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, cover,
         home_spread_odds, home_spread_prob,
         away_spread_odds, away_spread_prob) |>
  mutate(
    spreadCoverProb = spreadPredsBet,
    spreadSuccess = spreadCoverProb > 0.5,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, "Home", 
                            ifelse(1 - spreadCoverProb > away_spread_prob, "Away", NA)),
    spreadCoverSuccess = spreadCoverBet == cover
  )

spreadTestSum <- spreadDataTest |>
  summarise(
    Model = paste0("Fit", fit),
    Fit = "Test",
    spreadSuccesProb = mean(spreadSuccess, na.rm = TRUE),
    spreadCoverProb = mean(spreadCoverSuccess, na.rm = TRUE)
  )

spreadCoverSum <- bind_rows(spreadTrainSum,spreadTestSum)
spreadCoverSum

#### Total ----
totalFit <- homefinalFit + awayfinalFit
totalFitMean <- colMeans(totalFit)
totalFitMed <- apply(totalFit, 2, function(x){quantile(x, 0.5)})
totalFitLCB <- apply(totalFit, 2, function(x){quantile(x, 0.025)})
totalFitUCB <- apply(totalFit, 2, function(x){quantile(x, 0.975)})

totalPreds <- homefinalPreds + awayfinalPreds
totalPredsMean <- colMeans(totalPreds)
totalPredsMed <- apply(totalPreds, 2, function(x){quantile(x, 0.5)})
totalPredsLCB <- apply(totalPreds, 2, function(x){quantile(x, 0.025)})
totalPredsUCB <- apply(totalPreds, 2, function(x){quantile(x, 0.975)})

gameTrainTotal <- modelDataTrainNA$total
gameTestTotal <- modelDataTestNA$total
totalPredMetrics <- tibble(
  Fit = paste0("Fit", fit),
  total_MAE_fit = mean(abs(totalFitMean - gameTrainTotal)),
  total_MAD_fit = mean(abs(totalFitMed - gameTrainTotal), na.rm = TRUE),
  total_COV_fit = mean(totalFitLCB < gameTrainTotal & gameTrainTotal < totalFitUCB),
  total_MAE_pred = mean(abs(totalPredsMean - gameTestTotal), na.rm = TRUE),
  total_MAD_pred = mean(abs(totalPredsMed - gameTestTotal), na.rm = TRUE),
  total_COV_pred = mean(totalPredsLCB < gameTestTotal & gameTestTotal < totalPredsUCB)
)
totalPredMetrics

##### Probabilities ----
###### Fit ----
gameTrainTotalLine <- modelDataTrainNA$total_line
gameTrainTotalLineMat <- matrix(gameTrainTotalLine, nrow = 1)

totalFitprobs <- matrix(NA, nrow = sims, ncol = length(gameTrainTotalLine))
for(i in 1:sims){
  total_fit <- totalFit[i, ]
  probs <- total_fit > gameTrainTotalLine
  totalFitprobs[i, ] <- probs
}
totalFitBet <- colMeans(totalFitprobs)
totalFitBetLogical <- totalFitBet > 0.5
totalFitTotalLogical <- gameTrainTotal > gameTrainTotalLine
totalFitTotalProb <- mean(totalFitBetLogical == totalFitTotalLogical)
totalFitTotalProb

totalDataTrain <- modelDataTrainNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         total, total_line, 
         under_odds, under_prob,
         over_odds, over_prob) |>
  mutate(
    totalFit = totalFitMean,
    totalBet = ifelse(totalFit > total_line, "Over", "Under"),
    totalResult = ifelse(total > total_line, "Over", "Under"),
    totalSuccess = totalBet == totalResult,
    totalCoverProb = totalFitBet,
    totalCoverBet = ifelse(totalCoverProb > over_prob, "Over", 
                           ifelse(1 - totalCoverProb > under_prob, "Under", NA)),
    totalCoverSuccess = totalCoverBet == totalResult
  )

totalTrainSum <- totalDataTrain |>
  summarise(
    Model = paste0("Fit", fit),
    Fit = "Train",
    totalSuccesProb = mean(totalSuccess),
    totalCoverProb = mean(totalCoverSuccess, na.rm = TRUE)
  )

###### Pred ----
gameTestTotalLine <- modelDataTestNA$total_line
gameTestTotalLineMat <- matrix(gameTestTotalLine, nrow = 1)

totalPredsprobs <- matrix(NA, nrow = sims, ncol = length(gameTestTotalLine))
for(i in 1:sims){
  total_fit <- totalPreds[i, ]
  probs <- total_fit > gameTestTotalLine
  totalPredsprobs[i, ] <- probs
}
totalPredsBet <- colMeans(totalPredsprobs)
totalPredsBetLogical <- totalPredsBet > 0.5
totalPredsTotalLogical <- gameTestTotal > gameTestTotalLine
totalPredsTotalProb <- mean(totalPredsBetLogical == totalPredsTotalLogical)
totalPredsTotalProb

totalDataTest <- modelDataTestNA |>
  select(game_id, season, week, game_type,
         home_team, home_score, away_team, away_score,
         total, total_line, 
         under_odds, under_prob,
         over_odds, over_prob) |>
  mutate(
    totalPreds = totalPredsMean,
    totalBet = ifelse(totalPreds > total_line, "Over", "Under"),
    totalResult = ifelse(total > total_line, "Over", "Under"),
    totalSuccess = totalBet == totalResult,
    totalCoverProb = totalPredsBet,
    totalCoverBet = ifelse(totalCoverProb > over_prob, "Over", 
                           ifelse(1 - totalCoverProb > under_prob, "Under", NA)),
    totalCoverSuccess = totalCoverBet == totalResult
  )

totalTestSum <- totalDataTest |>
  summarise(
    Model = paste0("Fit", fit),
    Fit = "Test",
    totalSuccesProb = mean(totalSuccess),
    totalCoverProb = mean(totalCoverSuccess, na.rm = TRUE)
  )

perfSum <- left_join(
  bind_rows(
    spreadTrainSum,
    spreadTestSum
  ),
  bind_rows(
    totalTrainSum,
    totalTestSum
  )
)
perfSum

### Combine Performance ----
#predMetricsComb <- data.frame()
predMetricsComb <- bind_rows(
  predMetricsComb,
  predMetrics
) 
predMetricsComb <- predMetricsComb |>
  arrange(home_MAE_pred, away_MAE_pred)
predMetricsComb

#perfSumComb <- data.frame()
perfSumComb <- bind_rows(
  perfSumComb,
  perfSum
)
perfSumComb <- perfSumComb |>
  arrange(Fit, desc(spreadCoverProb), desc(totalCoverProb))
perfSumComb

save(predMetricsComb,perfSumComb,
     file = "./Data/scoresFitDiagnostics.RData")

### Plotting ----
#### Fit ----
##### Home ----
ppc_dens_overlay(y = modelDataTestNA$home_score, yrep = homefinalPreds) +
  labs(title = "Home Fit Predict") +
  theme_bw()

ppc_dens_overlay(y = modelDataTestNA$away_score, yrep = awayfinalPreds) +
  labs(title = "Away Fit Predict") +
  theme_bw()

ppc_dens_overlay(y = modelDataTestNA$result, yrep = spreadPreds) +
  labs(title = "Spread Fit Predict") +
  theme_bw()

ppc_dens_overlay(y = modelDataTestNA$total, yrep = totalPreds) +
  labs(title = "Total Fit Predict") +
  theme_bw()

FitFitDF <- bind_cols(
  StormdataTrain,
  LCB = FitfinalFitLCB,
  Mean = FitfinalFitMean,
  Med = FitfinalFitMed,
  UCB = FitfinalFitUCB
)

FitstormsFitplot <- ggplot(data = FitFitDF, aes(x = StormElapsedTime)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), fill = "lightblue") +
  geom_line(aes(y = VMAX, color = "Observed")) +
  geom_line(aes(y = Mean, color = "PPD Mean")) +
  facet_wrap(vars(StormID))+
  scale_y_continuous(limits = c(0,275), breaks = seq(0,275,50)) +
  labs(title = "Fit PPD Mean vs Observed VMAX",
       subtitle = "95% Credible Interval about PPD Mean") +
  scale_color_manual(name = NULL, values = c("black","red")) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  theme_bw()
FitstormsFitplot

#### Residuals ----
FitResiduals <- residuals(Fit, 
                          method = "posterior_predict",
                          summary = FALSE)

FitResidualsSum <- colMeans(FitResiduals)

# Extract residuals and fitted values from the baseline model
residuals_baseline <- residuals(Fit, summary = TRUE)
fitted_vals <- fitted(fit_baseline)

# Plot residuals vs Sea_Surface_Temp to check for heteroscedasticity
plot(StormdataTrain5$arcsinhMINSLP, FitResidualsSum, 
     xlab = "arcsinhMINSLP", ylab = "Residuals", 
     main = "Residuals vs arcsinhMINSLP")

# Similarly, plot residuals against other predictors
plot(hurricane_data$Pressure, residuals_baseline, 
     xlab = "Pressure", ylab = "Residuals", 
     main = "Residuals vs Pressure")

modelParams <- row.names(fixef(Fit15))
modelParams <- str_subset(modelParams, "sigma", negate = TRUE)
modelParams <- str_subset(modelParams, "Intercept", negate = TRUE)
modelParams[1] <- "basin"
modelParams[4] <- "Land"
modelParams
resids_list <- list()
for(i in modelParams){
  resids_list[[i]] <- ppc_error_scatter_avg_vs_x(StormdataTrain5$VMAX, 
                                                 FitfinalFit,
                                                 as.numeric(StormdataTrain5[[i]])) +
    geom_smooth(method = "lm", orientation = "y", level = 0.95) +
    labs(y = i)
}
resids_list

ppc_error_scatter_avg(StormdataTrain5$VMAX, 
                      FitfinalFit)

ppc_error_scatter_avg_vs_x(StormdataTrain5$VMAX, 
                           FitfinalFit,
                           as.numeric(StormdataTrain5[["arcsinhMINSLP"]])) +
  geom_smooth(orientation = "y", level = 0.95) +
  labs(y = "arcsinhMINSLP")



#### Prediction ----
FitPredDF <- bind_cols(
  StormdataTest,
  LCB = FitfinalPredsLCB,
  Mean = FitfinalPredsMean,
  Med = FitfinalPredsMed,
  UCB = FitfinalPredsUCB
) |>
  mutate(
    VMAX = Actual_Yvec
  ) 

FitstormsPredplot <- ggplot(data = FitPredDF, aes(x = StormElapsedTime)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), fill = "lightblue") +
  geom_line(aes(y = VMAX, color = "Observed")) +
  geom_line(aes(y = Mean, color = "PPD Mean")) +
  facet_wrap(vars(StormID))+#, ncol = 6)+
  scale_y_continuous(limits = c(0,275), breaks = seq(0,275,50)) +
  labs(title = "Fit PPD Mean vs Observed VMAX",
       subtitle = "95% Credible Interval about PPD Mean") +
  scale_color_manual(name = NULL, values = c("black","red")) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  theme_bw()
FitstormsPredplot

### PPC ----
#### Home ----
###### Quantile 2.5 
homeLCBsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.025)
                     })
homeLCBpvalueVec <- homeLCBsims < quantile(modelDataTrainNA$home_score, 0.025)
homeLCBpvalue <- sum(homeLCBpvalueVec)
homeLCBpvalue <- round(homeLCBpvalue/(sims), 3)
homeLCBpvalue <- min(homeLCBpvalue, 1 - homeLCBpvalue)

home_ppcLCB <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = paste0("2.5% Quantile (p-val = ", homeLCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcLCB

###### Quantile 97.5 
homeUCBsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.975)
                     })
homeUCBpvalueVec <- homeUCBsims < quantile(modelDataTrainNA$home_score, 0.975)
homeUCBpvalue <- as.numeric(sum(homeUCBpvalueVec))
homeUCBpvalue <- round(homeUCBpvalue/sims, 3)
homeUCBpvalue <- min(homeUCBpvalue, 1 - homeUCBpvalue)

home_ppcUCB <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.975), freq = FALSE) +
  labs(title = paste0("97.5% Quantile (p-val = ", homeUCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcUCB

###### Mean 
homeMEANsims <- apply(homefinalFit, 
                      MARGIN = 1,
                      function(x){
                        mean(x)
                      })
homeMEANpvalueVec <- homeMEANsims < mean(modelDataTrainNA$home_score)
homeMEANpvalue <- sum(homeMEANpvalueVec)
homeMEANpvalue <- round(homeMEANpvalue/sims, 3)
homeMEANpvalue <- min(homeMEANpvalue, 1 - homeMEANpvalue)

home_ppcMEAN <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) mean(y), freq = FALSE) +
  labs(title = paste0("Mean (p-val = ", homeMEANpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcMEAN

###### Med 
homeMEDsims <- apply(homefinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.5)
                     })
homeMEDpvalueVec <- homeMEDsims < quantile(modelDataTrainNA$home_score, 0.5)
homeMEDpvalue <- sum(homeMEDpvalueVec)
homeMEDpvalue <- round(homeMEDpvalue/sims, 3)
homeMEDpvalue <- min(homeMEDpvalue, 1 - homeMEDpvalue)

home_ppcMED <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) quantile(y, 0.5), freq = FALSE) +
  labs(title = paste0("Median (p-val = ", homeMEDpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcMED

###### SD 
homeSDsims <- apply(homefinalFit, 
                    MARGIN = 1,
                    function(x){
                      sd(x)
                    })
homeSDpvalueVec <- homeSDsims < sd(modelDataTrainNA$home_score)
homeSDpvalue <- sum(homeSDpvalueVec)
homeSDpvalue <- round(homeSDpvalue/sims, 3)
homeSDpvalue <- min(homeSDpvalue, 1 - homeSDpvalue)

home_ppcSD <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) sd(y), freq = FALSE) +
  labs(title = paste0("SD (p-val = ", homeSDpvalue, ")")) +
  theme_bw() +
  legend_none()
#home_ppcSD

###### Range 
homeRANGEsims <- apply(homefinalFit, 
                       MARGIN = 1,
                       function(x){
                         max(x)-min(x)
                       })
homeRANGEpvalueVec <- homeRANGEsims < (max(modelDataTrainNA$home_score)-min(modelDataTrainNA$home_score))
homeRANGEpvalue <- sum(homeRANGEpvalueVec)
homeRANGEpvalue <- round(homeRANGEpvalue/sims, 3)
homeRANGEpvalue <- min(homeRANGEpvalue, 1 - homeRANGEpvalue)

home_ppcRANGE <- 
  ppc_stat(modelDataTrainNA$home_score,
           homefinalFit,
           stat = function(y) max(y)-min(y), freq = FALSE) +
  labs(title = paste0("Range (p-val = ", homeRANGEpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcRANGE

#### Away ----
###### Quantile 2.5 
awayLCBsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.025)
                     })
awayLCBpvalueVec <- awayLCBsims < quantile(modelDataTrainNA$away_score, 0.025)
awayLCBpvalue <- sum(awayLCBpvalueVec)
awayLCBpvalue <- round(awayLCBpvalue/(sims), 3)
awayLCBpvalue <- min(awayLCBpvalue, 1 - awayLCBpvalue)

away_ppcLCB <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = paste0("2.5% Quantile (p-val = ", awayLCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcLCB

###### Quantile 97.5 
awayUCBsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.975)
                     })
awayUCBpvalueVec <- awayUCBsims < quantile(modelDataTrainNA$away_score, 0.975)
awayUCBpvalue <- as.numeric(sum(awayUCBpvalueVec))
awayUCBpvalue <- round(awayUCBpvalue/sims, 3)
awayUCBpvalue <- min(awayUCBpvalue, 1 - awayUCBpvalue)

away_ppcUCB <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.975), freq = FALSE) +
  labs(title = paste0("97.5% Quantile (p-val = ", awayUCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcUCB

###### Mean 
awayMEANsims <- apply(awayfinalFit, 
                      MARGIN = 1,
                      function(x){
                        mean(x)
                      })
awayMEANpvalueVec <- awayMEANsims < mean(modelDataTrainNA$away_score)
awayMEANpvalue <- sum(awayMEANpvalueVec)
awayMEANpvalue <- round(awayMEANpvalue/sims, 3)
awayMEANpvalue <- min(awayMEANpvalue, 1 - awayMEANpvalue)

away_ppcMEAN <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) mean(y), freq = FALSE) +
  labs(title = paste0("Mean (p-val = ", awayMEANpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcMEAN

###### Med 
awayMEDsims <- apply(awayfinalFit, 
                     MARGIN = 1,
                     function(x){
                       quantile(x, 0.5)
                     })
awayMEDpvalueVec <- awayMEDsims < quantile(modelDataTrainNA$away_score, 0.5)
awayMEDpvalue <- sum(awayMEDpvalueVec)
awayMEDpvalue <- round(awayMEDpvalue/sims, 3)
awayMEDpvalue <- min(awayMEDpvalue, 1 - awayMEDpvalue)

away_ppcMED <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) quantile(y, 0.5), freq = FALSE) +
  labs(title = paste0("Median (p-val = ", awayMEDpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcMED

###### SD 
awaySDsims <- apply(awayfinalFit, 
                    MARGIN = 1,
                    function(x){
                      sd(x)
                    })
awaySDpvalueVec <- awaySDsims < sd(modelDataTrainNA$away_score)
awaySDpvalue <- sum(awaySDpvalueVec)
awaySDpvalue <- round(awaySDpvalue/sims, 3)
awaySDpvalue <- min(awaySDpvalue, 1 - awaySDpvalue)

away_ppcSD <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) sd(y), freq = FALSE) +
  labs(title = paste0("SD (p-val = ", awaySDpvalue, ")")) +
  theme_bw() +
  legend_none()
#away_ppcSD

###### Range 
awayRANGEsims <- apply(awayfinalFit, 
                       MARGIN = 1,
                       function(x){
                         max(x)-min(x)
                       })
awayRANGEpvalueVec <- awayRANGEsims < (max(modelDataTrainNA$away_score)-min(modelDataTrainNA$away_score))
awayRANGEpvalue <- sum(awayRANGEpvalueVec)
awayRANGEpvalue <- round(awayRANGEpvalue/sims, 3)
awayRANGEpvalue <- min(awayRANGEpvalue, 1 - awayRANGEpvalue)

away_ppcRANGE <- 
  ppc_stat(modelDataTrainNA$away_score,
           awayfinalFit,
           stat = function(y) max(y)-min(y), freq = FALSE) +
  labs(title = paste0("Range (p-val = ", awayRANGEpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcRANGE

### Bayes p-values ----
homeFitpvalues <- tibble(
  Fit = paste0("Fit", fit),
  LCB = homeLCBpvalue,
  Median = homeMEDpvalue,
  UCB = homeUCBpvalue,
  Range = homeRANGEpvalue,
  Mean = homeMEANpvalue,
  SD = homeSDpvalue
)
homeFitpvalues

awayFitpvalues <- tibble(
  Fit = paste0("Fit", fit),
  LCB = awayLCBpvalue,
  Median = awayMEDpvalue,
  UCB = awayUCBpvalue,
  Range = awayRANGEpvalue,
  Mean = awayMEANpvalue,
  SD = awaySDpvalue
)
awayFitpvalues

### Combined Plot ----
home_ppcComb <- 
  homePPC /
  (home_ppcLCB | home_ppcMED | home_ppcUCB) /
  (home_ppcRANGE | home_ppcMEAN | home_ppcSD)
home_ppcComb

away_ppcComb <- 
  awayPPC /
  (away_ppcLCB | away_ppcMED | away_ppcUCB) /
  (away_ppcRANGE | away_ppcMEAN | away_ppcSD)
away_ppcComb

### CV ----
set.seed(52)
kfoldID <- kfold_split_grouped(K = 5, StormdataTrain$StormID)
Fitkfoldgroup <- kfold(Fit,
                       folds = kfoldID,
                       chains = 1,
                       save_fits = TRUE)
save(Fitkfoldgroup,
     file = "~/Desktop/Temp Hurricane Model Data/Fit12kfold.RData")
FitkfoldPreds <- kfold_predict(Fitkfoldgroup)
FitkfoldPredsDat <- FitkfoldPreds$yrep
FitkfoldPredsMean <- colMeans(FitkfoldPredsDat)
FitkfoldPredsMed <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.5)})
FitkfoldPredsLCB <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.025)})
FitkfoldPredsUCB <- apply(FitkfoldPredsDat, 2, function(x){quantile(x, 0.975)})

FitkfoldMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_kfold = mean(abs(FitkfoldPredsMean - FitkfoldPreds$y)),
  MAD_kfold = mean(abs(FitkfoldPredsMed - FitkfoldPreds$y)),
  COV_kfold = mean(FitkfoldPredsLCB < FitkfoldPreds$y & FitkfoldPreds$y < FitkfoldPredsUCB)
)
FitkfoldMetrics
