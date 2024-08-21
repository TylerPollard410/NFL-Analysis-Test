### Tune xgboost parameters

#load libraries
library(tidyverse)
library(data.table)
library(xgboost)
library(mlr)
library(caret)
library(nflverse)
# library(nflfastR)
# library(nflseedR)
# library(nflreadr)
# library(nflplotR)
library(lubridate)
library(plyr)
library(stringr)
library(imputeR)
library(pracma)
library(ModelMetrics)
library(useful)
library(tictoc)
library(progressr)
library(writexl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybrowser)
library(shinycssloaders)
# library(shinyalert)
# library(shinyBS)
# library(shinyjs)
# library(colourpicker)
library(bs4Dash)
library(plotly)
library(plyr)
# library(readxl)
library(readr)
# library(haven)
library(ggplot2)
library(data.table)
library(lubridate)
library(hms)
library(RColorBrewer)
# library(sp)
# library(htmltools)
library(fresh)
# library(extrafont)
library(stringr)
# library(reshape2)
# library(png)
library(ggpubr)
# library(htmlTable)
# library(tibble)
# library(EnvStats)
# library(xtable)
# library(grid)
library(DT)
# library(rhandsontable)
# library(rvest)
# library(scales)
library(caret)
library(knitr)
library(data.table)
library(markdown)
library(nflverse)
library(progressr)
library(gt)
library(dplyr)

# Activate progress updates
progressr::handlers(global = TRUE)

# Parallel processing can be activated via the following line
future::plan("multisession")

# Read in current data ----
## Games data ----
games_df2 <- load_schedules()
games_df2 <- games_df2 %>% filter(season >= 2003)
games_df <- games_df2 %>% 
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

# Get long format of games data
games_long_df <- clean_homeaway(games_df, invert = c("result", "spread_line"))

## Play-by-Play data ----
# Load play by play data from downloaded files (takes about 9 minutes)
pbp_data <- list()
pbp_files <- list.files("Play-by-Play rds Files")
pbp_seasons <- as.numeric(str_extract(pbp_files, "[:digit:]+"))
pbp_files <- pbp_files[pbp_seasons >= 2003]
for(i in pbp_files){
  pbp_season <- str_extract(i, "[:digit:]+")
  pbp_data[[i]] <- with_progress(readRDS(paste0("Play-by-Play rds Files/", i)))
}
pbp_df <- rbindlist(pbp_data)

## Teams data ----
# Load team ids
nfl_teams <- load_teams()


## Overall Spread Table ----

spread_gt <- games_long_df |>
  filter(!is.na(result)) |>
  mutate(
    spread_cover = ifelse(result > spread_line, "Cover",
                          ifelse(result > spread_line, "Loss", "Push"))
  ) |>
  group_by(team) |>
  summarise(
    `Games Played` = n(),
    `Cover %` = sum(spread_cover == "Cover", na.rm = TRUE)/n()
  ) |>
  arrange(desc(`Cover %`)) |>
  rename("Team" = team) |>
  gt(
    #rowname_col = "Team"
  ) |>
  fmt_percent(
    columns = `Cover %`,
    decimals = 2
  ) |>
  data_color(
    columns = `Cover %`,
    method = "numeric",
    palette = c("red", "white", "green")
  ) |>
  gt_nfl_wordmarks(
    columns = "Team", height = "20px"
  ) |>
  tab_options(
    table.align = "left",
    data_row.padding = px(3)
  )
spread_gt

### Team Spread Table ----
team_spread_gt <- games_long_df |>
  filter(!is.na(result)) |>
  filter(
    (between(season, 2021, 2023)) &
      (team == "BAL")
  ) |>
  mutate(
    spread_cover = ifelse(result > spread_line, "Cover",
                          ifelse(result > spread_line, "Loss", "Push"))
  ) |>
  select(
    season,
    game_type,
    week,
    opponent,
    opponent_score,
    team,
    team_score,
    location,
    result,
    spread_line,
    spread_cover
  ) |>
  gt()
team_spread_gt

# Get vectors of 2-letter country codes for each region of Oceania
Australasia <- c("AU", "NZ")
Melanesia <- c("NC", "PG", "SB", "VU")
Micronesia <- c("FM", "GU", "KI", "MH", "MP", "NR", "PW")
Polynesia <- c("PF", "WS", "TO", "TV")

# Create a gt table based on a preprocessed `countrypops`
countrypops |>
  filter(country_code_2 %in% c(
    Australasia, Melanesia, Micronesia, Polynesia)
  ) |>
  filter(year %in% c(2000, 2010, 2020)) |>
  mutate(region = case_when(
    country_code_2 %in% Australasia ~ "Australasia",
    country_code_2 %in% Melanesia ~ "Melanesia",
    country_code_2 %in% Micronesia ~ "Micronesia",
    country_code_2 %in% Polynesia ~ "Polynesia",
  )) |>
  pivot_wider(names_from = year, values_from = population) |>
  arrange(region, desc(`2020`)) |>
  select(-starts_with("country_code")) |>
  gt(
    rowname_col = "country_name",
    groupname_col = "region"
  ) |>
  tab_header(title = "Populations of Oceania's Countries in 2000, 2010, and 2020") |>
  tab_spanner(
    label = "Total Population",
    columns = everything()
  ) |>
  fmt_integer()

pizzaplace |>
  dplyr::group_by(type, size) |>
  dplyr::summarize(
    sold = n(),
    income = sum(price)#,
    #.groups = "drop_last"
  ) |>
  gt(rowname_col = "size") |>
  tab_header(title = "Pizzas Sold in 2015") |>
  fmt_integer(columns = sold) |>
  fmt_currency(columns = income) |>
  summary_rows(
    columns = sold,
    fns = list(TOTAL = "sum"),
    fmt = list(~ fmt_integer(.))
  ) |>
  summary_rows(
    columns = income,
    fns = list(TOTAL = "sum"),
    fmt = list(~ fmt_currency(.))
  ) |>
  tab_options(
    summary_row.background.color = "#ACEACE",
    row_group.background.color = "#FFEFDB"
  )


