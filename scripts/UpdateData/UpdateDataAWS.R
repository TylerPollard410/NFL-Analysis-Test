## R script to source current data and update
library(future)
library(glue)
library(DBI)
library(RSQLite)
library(aws.s3)
library(nflverse)
library(tidyverse)

# Set wd ----
setwd("./scripts/Update Data")

# Source data ----
source("gameData.R")
source("gameDataLong.R")
source("pbpData.R")

# Initial push to AWS ----
AWSbucket <- "nflanalysisdata"

## Put in AWS ----
put_nfl_data <- function(Robject, s3bucket = AWSbucket){
  objectName <- deparse(substitute(Robject))
  objectFile <- paste0(objectName, ".RData")
  s3save(Robject, 
         object = objectFile,
         bucket = AWSbucket)
}

# .rds
# Write time: 205 sec
# Read time: 
system.time(
  put_nfl_data(pbpData)
)

system.time(
  s3load(object = "pbpData.RData",
         bucket = "nflanalysisdata")
)

db_name <- "nfl_database.sqlite"
con <- dbConnect(SQLite(), dbname = db_name)
dbWriteTable(con,name = "pbpData",value = pbpData)

pbp_db <- dplyr::tbl(con, "pbpData")
pbp_db %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n = dplyr::n())
pbpData %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n = dplyr::n())

dbDisconnect(con)
