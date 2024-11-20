## Testing AWS connection 
library(DBI)
library(nflverse)
library(tidyverse)

my_con <- dbConnect(RMariaDB::MariaDB(),
                    dbname = "Tyler1",
                    username = "admin",
                    password = "Password1234",
                    host = "database-1.cl68ickmince.us-east-1.rds.amazonaws.com")

dbListTables(my_con)
dbListFields(my_con, "pbp_data")
pbp_tbl <- tbl(my_con, "pbp_data")
pbp_tbl %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n = dplyr::n())

pbpData <- nflreadr::load_pbp(2002:2024)

dbWriteTable(my_con, name = "mtcars", value = mtcars)
dbListTables(my_con)
dbListFields(my_con, "mtcars")
mt_tbl <- tbl(my_con, "mtcars")
mt_tbl %>%
  dplyr::group_by(cyl) %>%
  dplyr::summarize(n = dplyr::n())
dbRemoveTable(my_con, "pbp_data")

dbWriteTable(my_con, "pbp_data", pbpData)

pbp_list <- list()
for(i in 2002:2024){
  year <- as.character(i)
  pbpTemp <- load_pbp(seasons = i)
  pbp_list[[year]] <- pbpTemp
}

dbWriteTable(my_con, "pbp_data", pbp_list[["2002"]])




