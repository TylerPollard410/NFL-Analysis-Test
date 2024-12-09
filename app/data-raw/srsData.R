## SRS Data 

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


# Read in data ----
source("./app/data-raw/gameData.R")
source("./app/data-raw/gameDataLong.R")

seasonsMod <- 2006:2024
gameDataMod <- gameData # > filter(season %in% seasonsMod)
gameDataLongMod <- gameDataLong # > filter(season %in% seasonsMod)
#pbpDataMod <- load_pbp(seasons = seasonsMod)
load("./app/data/seasonWeekStandings.rda")
seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "NFLdata",
                 user = "postgre",
                 password = "NFLpass1234",
                 host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")
dbListTables(con)
dbDisconnect(con)

# Lag SRS ----
## Want to see when SRS stablizes
srsData <- seasonWeekStandings |>
  select(season, week, team, SRS) |>
  group_by(team) |>
  mutate(
    lagSRS = lag(SRS, n = 1),
    diffSRS = SRS - lagSRS,
  ) |>
  ungroup()

## Plot ----
srsTimeData <- gameDataLongMod |>
  select(game_id, season, week, gameday, team) |>
  left_join(
    srsData
  )

### Actual ----
srsTimeData |>
  group_by(season) |>
  plot_time_series(
    .date_var = week,
    .value = SRS,
    .color_var = team,
    .interactive = FALSE,
    .facet_nrow = 4,
    .facet_ncol = 5,
    .facet_scales = "free_x",
    .legend_show = FALSE,
    .smooth = FALSE,
    .title = "SRS"
  ) +
  scale_x_continuous(breaks = 0:22)

### diffSRS ----
srsTimeData |>
  group_by(season) |>
  plot_time_series(
    .date_var = week,
    .value = diffSRS,
    .color_var = team,
    .interactive = FALSE,
    .facet_nrow = 4,
    .facet_ncol = 5,
    .facet_scales = "free_x",
    .legend_show = FALSE,
    .smooth = FALSE,
    .title = "diffSRS"
  ) +
  scale_x_continuous(breaks = 0:22)
# Seems to converge around week 6


# Feature Engineer ----
## The first 6 weeks are highly noisy bc SRS and other related metrics
## are calculated fresh with the beginning of each season
srsData2 <- srsData |>
  group_by(team) |>
  mutate(
    across(SRS,
           .fns = list(
             sma2 = ~lag(movavg(.x, n = 2, "s")), 
             ewma2 = ~lag(movavg(.x, n = 2, "e")), 
             rma2 = ~lag(movavg(.x, n = 2, "r")), 
             sma3 = ~lag(movavg(.x, n = 2, "s")), 
             ewma3 = ~lag(movavg(.x, n = 3, "e")),
             rma3 = ~lag(movavg(.x, n = 3, "r")),
             sma4 = ~lag(movavg(.x, n = 4, "s")), 
             ewma4 = ~lag(movavg(.x, n = 4, "e")),
             rma4 = ~lag(movavg(.x, n = 4, "r")),
             sma5 = ~lag(movavg(.x, n = 5, "s")), 
             ewma5 = ~lag(movavg(.x, n = 5, "e")),
             rma5 = ~lag(movavg(.x, n = 5, "r"))
           ), 
           .names = "{.fn}_{.col}"
    )
  ) |>
  mutate(
    across(contains("ma"),
           .fns = list(smooth = ~ifelse(week < 6, .x, lagSRS)),
           .names = "{.col}{.fn}"
    )
  ) |>
  mutate(
    across(contains("smooth"),
           .fns = list(Diff = ~ifelse(week < 6, lagSRS - .x, NA)),
           .names = "{.col}{.fn}"
    )
  ) 

## MAE ----
MAEsmoothsTeam <- srsData2 |>
  #filter(season >= 2021) |> to test various season subsets
  summarise(
    across(contains("smoothDiff"),
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

### Plot
# srsData2 |>
#   group_by(season) |>
#   plot_time_series(
#     .date_var = week,
#     .value = SRS - ewma2_SRSsmooth,
#     .color_var = team,
#     .interactive = FALSE,
#     .facet_nrow = 4,
#     .facet_ncol = 5,
#     .facet_scales = "free_x",
#     .legend_show = FALSE,
#     .smooth = FALSE,
#     .title = "ewma"
#   ) +
#   scale_x_continuous(breaks = 0:22)
  
srsData <- srsData2


rm(MAEsmooths)
rm(MAEsmoothsTeam)
rm(MAEsmoothsTeamLong)
rm(srsData2)
rm(srsTimeData)
rm(gameData)
rm(gameDataLong)

### Exponential Weighted MA for window = 2 seems best
















