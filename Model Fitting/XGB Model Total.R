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
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Previous Data ----
modData2 <- modData |> 
  filter(season > 2006) |>
  #filter(!is.na(result)) |>
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
    div_game = factor(div_game)
  ) 

modPreProcess <- modData2 |>
  filter(season >= 2022, !is.na(result)) |>
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
    -contains("Score"),
    contains("roll"),
    contains("cum")
    # -total_line,
    # -spread_line,
    #-location,
    #-div_game,
    #-roof
  )

modPreProcValues <- preProcess(modPreProcess,
                               method = c("center", "scale", "YeoJohnson", "corr")
)
modPreProcess2 <- predict(modPreProcValues, 
                          newdata = modData2 |>
                            filter(season >= 2021, !is.na(result)))

trainData1 <- modData2 |> 
  filter(between(season, 2022, 2023) | (season == 2024 & week <= 6))
testData1 <- modData2 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result),
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  )

predictorData <- trainData1 |> 
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
    -contains("conversions"),
    -contains("pct"),
    -total_line,
    -totalCover,
    -spread_line,
    -overtime,
    -contains("over"),
    -contains("under"),
    contains("roll"),
    contains("cum")
    #-location,
    #-div_game,
    #-roof
  ) 
preProcValues <- preProcess(predictorData,
                            method = c("center", "scale", "pca"),
                            na.remove = TRUE
)
preProcValues2 <- preProcess(predictorData,
                             method = c("center", "scale", "YeoJohnson")
)
preProcValues
preProcValues2
predictorData2 <- predict(preProcValues, predictorData)
trainData2 <- predict(preProcValues, trainData1)
testData2 <- predict(preProcValues, testData1)

predictorData3 <- predict(preProcValues2, predictorData)
trainData3 <- predict(preProcValues2, trainData1)
testData3 <- predict(preProcValues2, testData1)

trainData <- trainData3
testData <- testData3

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

## PCA ----
pca <- prcomp(predictorData |> select(where(is.numeric)), center = TRUE, scale = TRUE)
pca
summary(pca)
screeplot(pca, type = "lines")
screeplot(predictorData2 |> select(contains("PC")), type = "lines")


## Correlations ----
totalcor <- cor(trainData2 |> select(total),
                trainData2 |> select(c(where(is.numeric))),
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
totalcor2 <- cor(trainData |> select(total, totalCorVars),
                 method = "kendall")
corrplot::corrplot.mixed(totalcor2, 
                         upper = 'ellipse', 
                         order = 'hclust')




fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10#,
  ## repeated ten times
  #repeats = 10
)

training_data <- trainData2 |>
  select(total, contains("PC", ignore.case = F))
testing_data <- testData2 |>
  select(total, contains("PC", ignore.case = F))


# xgbTree ----
system.time(
  xbm <- train(total ~ .,
               data = training_data,
               method = "xgbTree",
               trControl = fitControl
  )
)

xbm
summary(xbm)
plot(xbm)

predictionXBM <- predict(xbm, newdata = testing_data)
postResample(predictionXBM, testing_data$total)
caret::confusionMatrix(predictionXBM, testing_data$total)

mean((predictionXBM > testData2$total_line & testData2$total > testData2$total_line) |
       (predictionXBM < testData2$total_line & testData2$total < testData2$total_line))

xgbVarImp <- varImp(xbm, scale = FALSE)
plot(xgbVarImp, top = 40)


# rf ----
system.time(
  rfMod <- train(total ~ .,
                 data = training_data,
                 method = "rf",
                 trControl = fitControl
  )
)

rfMod
summary(rfMod)
plot(rfMod)

predictionRF <- predict(rfMod, newdata = testing_data)
postResample(predictionRF, testing_data$total)
caret::confusionMatrix(predictionRF, testing_data$total)

mean((predictionRF > testData1$total_line & testData1$total > testData1$total_line) |
       (predictionRF < testData1$total_line & testData1$total < testData1$total_line))

rfVarImp <- varImp(rfMod, scale = FALSE)
plot(rfVarImp, top = 20)


# brnn ----
system.time(
  brnn <- train(total ~ .,
                data = training_data,
                method = "brnn",
                trControl = fitControl
  )
)

brnn
summary(brnn)
plot(brnn)

predictionBRNN <- predict(brnn, newdata = testing_data)
postResample(predictionBRNN, testing_data$total)
caret::confusionMatrix(predictionBRNN, testData2$total)

mean((predictionBRNN > testData2$total_line & testData2$total > testData2$total_line) |
       (predictionBRNN < testData2$total_line & testData2$total < testData2$total_line))

brnnVarImp <- varImp(brnn, scale = FALSE)
plot(brnnVarImp, top = 40)

# Compare ----
testing_data <- testing_data |>
  mutate(
    total_line = testData2$total_line,
    predXBM = predictionXBM,
    predRF = predictionRF,
    predBRNN = predictionBRNN,
    outcome = ifelse(total > total_line, "over",
                     ifelse(total < total_line, "under", NA))
  ) |>
  select(
    total,
    total_line,
    outcome,
    predXBM,
    predRF,
    predBRNN
  ) |>
  mutate(
    outXBM = ifelse(predXBM > total_line & outcome == "over", TRUE,
                    ifelse(predXBM < total_line & outcome == "under", TRUE,
                           ifelse(is.na(outcome), NA, FALSE))),
    outRF = ifelse(predRF > total_line & outcome == "over", TRUE,
                    ifelse(predRF < total_line & outcome == "under", TRUE,
                           ifelse(is.na(outcome), NA, FALSE))),
    outBRNN = ifelse(predBRNN > total_line & outcome == "over", TRUE,
                   ifelse(predBRNN < total_line & outcome == "under", TRUE,
                          ifelse(is.na(outcome), NA, FALSE)))
  )

testing_data |>
  summarise(
    across(c(contains("out"), -outcome), ~mean(.x, na.rm = TRUE))
  )

