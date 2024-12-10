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

seasonsMod <- 2006:2024
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

pbpDataMod2 <- pbpDataMod |> filter(game_id != "2024_14_GB_DET")
pbpDataMod <- pbpDataMod2

# EPA ========
## Use this section to check for anomolies ----
pbpDictionary <- dictionary_pbp

playData <- pbpDataMod |>
  select(play_id, game_id, season, week, posteam, defteam,home_team, away_team,
         play_type, play_type_nfl, play, 
         pass, pass_attempt, rush, rush_attempt,special,
         qb_dropback, qb_scramble,
         penalty, penalty_team, penalty_type,
         yards_gained, ydstogo, 
         interception, fumble, fumble_lost, epa, wpa, desc)

playData |>
  filter(
    #play == 1, pass == 0, rush == 0, special == 0, penalty == 0
    #fumble == 1 | interception == 1 
    #pass != pass_attempt
    #rush != rush_attempt
    #qb_scramble == 1, fumble == 1
    pass == 1
    #play_deleted == 1
  ) |>
  view()

playData |>
  filter(play_id == 1369, game_id == "2011_02_STL_NYG") |>
  view()


## Make off and def ----
epaOffData <- pbpDataMod |>
  filter(season %in% seasonsMod) |>
  filter(play == 1) |> 
  filter(!is.na(epa) & !is.na(ep) & !is.na(posteam)) |>
  group_by(game_id, season, week, posteam, home_team, away_team) |>
  mutate(
    scaled_vegas_wp = 1 - 4*(0.5 - vegas_wp)^2
  ) |>
  summarise(
    off_plays = n(),
    off_epa_sum = sum(epa, na.rm = TRUE),
    off_epa_mean = mean(epa, na.rm = TRUE),
    off_pass_plays = sum(play_type == "pass"),
    off_pass_epa_sum = sum(epa[play_type == "pass"], na.rm = TRUE),
    off_pass_epa_mean = mean(epa[play_type == "pass"], na.rm = TRUE),
    off_rush_plays = sum(play_type == "run"),
    off_rush_epa_sum = sum(epa[play_type == "run"], na.rm = TRUE),
    off_rush_epa_mean = mean(epa[play_type == "run"], na.rm = TRUE),
    off_penalty_plays = sum(penalty == 1, na.rm = TRUE),
    off_penalty_epa_sum = sum(epa[penalty == 1], na.rm = TRUE),
    off_penalty_epa_mean = mean(epa[penalty == 1], na.rm = TRUE)
  ) |>
  mutate(
    across(contains("off"), ~ifelse(is.nan(.x), 0, .x))
  ) |>
  ungroup() |>
  group_by(game_id) |>
  mutate(
    opponent = rev(posteam), .after = posteam
  ) |>
  rename(
    team = posteam
  )

epaData <- epaOffData |>
  left_join(
    epaOffData |> 
      select(game_id, opponent, contains("off")) |>
      rename_with(~str_replace(.x, "off", "def"), .cols = contains("off")),
    by = join_by(game_id, team == opponent)
  )

epaTimeData <- gameDataLongMod |>
  left_join(
    epaData |> select(-home_team, -away_team)
  ) |> 
  filter(!is.na(spread_line)) |>
  select(-c(
    old_game_id,
    gsis,
    nfl_detail_id,
    pfr,
    pff,
    espn,
    ftn,
    team_qb_id,
    team_qb_name,
    opponent_qb_id,
    opponent_qb_name,
    referee,
    stadium_id
  )) |>
  mutate(
    gameday = as_date(gameday)
  ) |>
  select(game_id, season, week, gameday, team, contains("mean"))

## Average season EPA ----
# epaMetric <- "def_epa_mean"
# 
# epaAvgs <- epaTimeData |> 
#   group_by(season, team) |>
#   summarise(
#     seasonAvg = mean(!!sym(epaMetric), na.rm = TRUE)
#   ) |>
#   ungroup() |>
#   group_by(team) |>
#   mutate(seasonAvglag = lag(seasonAvg, n = 1, default = 0)) |>
#   ungroup()

epaMetrics <- str_subset(colnames(epaTimeData), "epa")

epaAvgs <- epaTimeData |> 
  group_by(season, team) |>
  summarise(
    across(contains("epa"),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}Avg"
    )
    #seasonAvg = mean(!!sym(epaMetric), na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    across(contains("Avg"),
           ~lag(.x, n = 1, default = 0),
           .names = "{.col}Lag"
    )
    #seasonAvglag = lag(seasonAvg, n = 1, default = 0)
  ) |>
  ungroup()
  # select(
  #   season, team,
  #   contains("off_epa_mean"),
  #   contains("off_pass_epa_mean"),
  #   contains("off_rush_epa_mean"),
  #   contains("off_penalty_epa_mean"),
  #   contains("def_epa_mean"),
  #   contains("def_pass_epa_mean"),
  #   contains("def_rush_epa_mean"),
  #   contains("def_penalty_epa_mean")
  # )

epaTimeDataPlot <- epaTimeData |>
  left_join(epaAvgs)

## Plot times series ----
plotTeam <- "BAL"
epaPlotMetric <- "off_epa_mean"

epaTimeDataPlot |>
  group_by(season) |>
  filter(team == plotTeam) |>
  plot_time_series(
    .date_var = week, #gameday
    .value = !!sym(epaPlotMetric),
    .color_var = team,
    .interactive = FALSE,
    .facet_nrow = 4,
    .facet_ncol = 5,
    .facet_scales = "free_x",
    .legend_show = FALSE,
    .smooth = TRUE,
    .title = paste(plotTeam, epaPlotMetric)
  ) +
  geom_line(aes(x = week, y = get(paste0(epaPlotMetric, "Avg"))), color = "red", linewidth = 1) +
  geom_line(aes(x = week, y = get(paste0(epaPlotMetric, "AvgLag"))), color = "green") +
  scale_x_continuous(breaks = 0:22)


# Feature Engineer ----
## The first 6 weeks are highly noisy bc SRS and other related metrics
## are calculated fresh with the beginning of each season
epaData2 <- epaData |>
  select(
    game_id, season, week, team, opponent,
    contains("mean")
  ) |>
  group_by(season, team) |>
  mutate(
    across(contains("mean"),
           ~lag(.x, n = 1),
           .names = "{.col}Lag"
           )
  ) |>
  ungroup() |>
  left_join(
    epaAvgs |> select(season, team, contains("meanAvgLag"))
  ) |>
  group_by(season, team) |>
  mutate(
    off_epa_meanLag = ifelse(is.na(off_epa_meanLag), off_epa_meanAvgLag, off_epa_meanLag),
    off_pass_epa_meanLag = ifelse(is.na(off_pass_epa_meanLag), off_pass_epa_meanAvgLag, off_pass_epa_meanLag),
    off_rush_epa_meanLag = ifelse(is.na(off_rush_epa_meanLag), off_rush_epa_meanAvgLag, off_rush_epa_meanLag),
    off_penalty_epa_meanLag = ifelse(is.na(off_penalty_epa_meanLag), off_penalty_epa_meanAvgLag, off_penalty_epa_meanLag),
    def_epa_meanLag = ifelse(is.na(def_epa_meanLag), def_epa_meanAvgLag, def_epa_meanLag),
    def_pass_epa_meanLag = ifelse(is.na(def_pass_epa_meanLag), def_pass_epa_meanAvgLag, def_pass_epa_meanLag),
    def_rush_epa_meanLag = ifelse(is.na(def_rush_epa_meanLag), def_rush_epa_meanAvgLag, def_rush_epa_meanLag),
    def_penalty_epa_meanLag = ifelse(is.na(def_penalty_epa_meanLag), def_penalty_epa_meanAvgLag, def_penalty_epa_meanLag)
  )

epaData3 <- epaData2 |>
  mutate(
    across(contains("meanLag"),
           .fns = list(
             ewma2 = ~movavg(.x, n = 2, type = "e"),
             ewma3 = ~movavg(.x, n = 3, type = "e"),
             ewma4 = ~movavg(.x, n = 4, type = "e"),
             ewma5 = ~movavg(.x, n = 5, type = "e"),
             ewma6 = ~movavg(.x, n = 6, type = "e"),
             rwma2 = ~movavg(.x, n = 2, type = "r"),
             rwma3 = ~movavg(.x, n = 3, type = "r"),
             rwma4 = ~movavg(.x, n = 4, type = "r"),
             rwma5 = ~movavg(.x, n = 5, type = "r"),
             rwma6 = ~movavg(.x, n = 6, type = "r")
           ),
           .names = "{.col}_{.fn}"
           ),
    .keep = "unused"
  ) |>
  select(
    -contains("AvgLag")
  ) |>
  rename_with(~str_remove(.x, "Lag"), everything()) |>
  # mutate(
  #   across(contains("ewma"),
  #          .fns = list(
  #            #Diff = ~(!!sym(str_split_i(colnames(.x), "_ewma", i = 1)) - .x)
  #            Diff = ~(!!(str_split_i(names(~.x), "_ewma", i = 1)))
  #            ),
  #          .names = "{.col}_{.fn}"
  #   )
  # ) 
  ungroup()

epaDataPlot <- epaData3 |>
  select(game_id, season, week, team, opponent, contains("off_epa"), contains("def_epa")) |>
  filter(team == "BAL") |>
  pivot_longer(
    cols = contains("ma"), names_to = "Variable", values_to = "epa"
  ) |>
  mutate(
    Feature = str_split_i(Variable, "_mean_", i = 2),
    Window = str_split_i(Feature, "ma", i = 2),
    Feature = str_remove(Feature, "[:digit:]")
  ) |>
  mutate(
    Diff = off_epa_mean - epa, .after = epa
  )

epaData4 <- epaData2 |>
  group_by(season, team) |>
  tk_augment_slidify(
    .value = contains("meanLag"),
    .period = c(1:10),
    .f = mean,
    .partial = TRUE, 
    .align = "right"
  ) |>
  select(
    -contains("AvgLag"),
    -ends_with("Lag")
  ) |>
  rename_with(~str_remove(.x, "Lag"), everything()) |>
  ungroup()

offEpaData <- epaData4 |>
  select(game_id, season, week, team, opponent, contains("off_epa")) |>
  filter(team == "BAL") |>
  pivot_longer(
    cols = contains("roll"), names_to = "Variable", values_to = "epa"
  ) |>
  mutate(
    Feature = str_split_i(Variable, "_mean_", i = 2),
    Window = str_split_i(Feature, "_", i = 2),
    Feature = str_split_i(Feature, "_", i = 1)
  ) |>
  mutate(
    Diff = off_epa_mean - epa, .after = epa
  )









ggplot(data = offEpaData) +
  geom_line(aes(x = week, y = off_epa_mean)) +
  geom_line(aes(x = week, y = epa, color = Window)) +#, linetype = Window)) +
  scale_x_continuous(breaks = 0:22) +
  facet_wrap(vars(season), nrow = 5, ncol = 4)

## PLot ----
smoothTeam <- "BAL"

## MAE ----
MAEsmoothsTeam <- epaData3 |>
  #filter(season >= 2021) |> to test various season subsets
  summarise(
    across(contains("ewma"),
           ~mean(abs(.x), na.rm = TRUE))
  )

MAEsmoothsTeamLong <- MAEsmoothsTeam |> 
  group_by(team) |>
  pivot_longer(-team, names_to = "Method", values_to = "MAE") |>
  mutate(
    Window = str_extract(Method, "[:digit:]"),
    Method2 = str_split_i(Method, "[:digit:]", i = 1)
  )

### Plot ----
ggplot(data = MAEsmoothsTeamLong) +
  geom_density(aes(x = MAE, color = Method2, linetype = Window))

## Avergae over team ----
MAEsmooths <- MAEsmoothsTeam |>
  ungroup() |> 
  select(-team) |>
  summarise(
    across(everything(), mean)
  )

MAEsmooths |> 
  pivot_longer(everything(), names_to = "Method", values_to = "MAE") |>
  arrange(MAE)



























