## Testing AWS connection 
library(DBI)
#library(RMySQL)
library(data.table)
library(RPostgres)
library(nflverse)
library(tidyverse)

# MySQL ----
my_con <- dbConnect(RMySQL::MySQL(),
                    dbname = "NFLdata",
                    username = "admin",
                    password = "NFLpass1234",
                    host = "nfl-analysis.cl68ickmince.us-east-1.rds.amazonaws.com")

dbListTables(my_con)

dbWriteTable(my_con, name = "pbp_data", value = pbpData)

system.time(
  pbpData2 <- data.table::fread("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.csv")
)

update_db(force_rebuild = 2024, 
          db_connection = my_con)

dbDisconnect(my_con)


# PostgreSQL ----
my_con <- dbConnect(RPostgres::Postgres(),
                    dbname = "NFLdata",
                    user = "postgre",
                    password = "NFLpass1234",
                    host = "nfl-postgres-database.cl68ickmince.us-east-1.rds.amazonaws.com")

dbListTables(my_con)

## gameData -----
gameData <- load_schedules(2002:2024)

# Takes 1.41 seconds
system.time(
  dbWriteTable(my_con, name = "game_data", value = gameData)
)

dbListTables(my_con)

## pbpData ----
#pbpData <- load_pbp(2002:2024)
pbpData |> filter(season == 2024) |> pull(week) |> max()
pbpDataLastWeek <- pbpData |> filter(!(season == 2024 & week == 11))
rm(pbpData)

# 10 rows Takes 1.41 seconds
# 100 rows 0.955 seconds
# 1000 rows 1.493 seconds
# 10000 rows 9.412 seconds
# 50000 rows 47.144 seconds
# 100000 rows 95.726 seconds
# All Rows 

system.time(
  dbWriteTable(my_con,
               name = "pbp_data",
               value = pbpDataLastWeek,
               overwrite = TRUE)
)

dbListTables(my_con)

# 50000 rows 6.636 seconds
# 100000 rows 13.989 seconds
# All Rows 
system.time(
  dbReadTable(my_con, "pbp_data")
)

# csv 21 seconds
system.time(
  pbpData2 <- data.table::fread("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.csv")
)


## nflverse ----
system.time(
  update_db(dbdir = "~/Desktop/NFL Analysis Data/",
            force_rebuild = c(2002:2023))
)

system.time(
  update_db(db_connection = my_con)
)

dbDisconnect(my_con)

system.time(
  my_con |> tbl("pbp_data") |>
    select(game_id) |>
    summarise(
      n()
    )
)

system.time(
  my_con |> tbl("nflfastR_pbp") |>
    select(game_id) |>
    summarise(
      n()
    )
)

system.time(
  pbpData <- dbReadTable(my_con, "pbp_data")
)

system.time(
  pbpFastData <- dbReadTable(my_con, "nflfastR_pbp")
)

pbp_tbl <- tbl(my_con, "pbp_data")
pbpFast_tbl <- tbl(my_con, "nflfastR_pbp")

system.time(
  pbpData_dt <- pbp_tbl |> as.data.table()
)

system.time(
  pbpFastData_dt <- pbpFast_tbl |> as.data.table()
)


system.time(
  pbpData <- fread("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.csv")
)
# system.time(
#   pbpData2 <- readr::read_csv("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.csv")
# )
# system.time(
#   pbpData2 <- read.csv("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.csv")
# )

future::plan("multisession")

system.time(
  fst::write_fst(pbpData, 
                 "~/Desktop/NFL Analysis Data/pbpData2002_2024W11.fst")
)

system.time(
  pbpData2 <- fst::read_fst("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.fst")
)

# system.time(
#   saveRDS(pbpData, 
#           "~/Desktop/NFL Analysis Data/pbpData2002_2024W11.rds")
# )
# 
# system.time(
#   pbpData3 <- readRDS("~/Desktop/NFL Analysis Data/pbpData2002_2024W11.rds")
# )







