fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10#,
  ## repeated ten times
  #repeats = 10
)



training_data <- histModelData |>
  select(total, totalCorVars)
testing_data <- modelData |>
  select(total, totalCorVars)

system.time(
  xbm <- train(total ~ .,
               data = training_data,
               method = "xgbTree",
               trControl = fitControl
  )
)

xbm
summary(xbm)

predictionXBM <- predict(xbm1, newdata = testing_data)
postResample(predictionXBM, testing_data$total)
caret::confusionMatrix(prediction, testing_data$total)

mean((predictionXBM > modelData1$total_line & modelData1$total > modelData1$total_line) |
       (predictionXBM < modelData1$total_line & modelData1$total < modelData1$total_line))

xgbVarImp <- varImp(xbm1, scale = FALSE)
plot(xgbVarImp, top = 20)



system.time(
  rfMod <- train(total ~ .,
                 data = training_data,
                 method = "rf",
                 trControl = fitControl
  )
)

rfMod
summary(rfMod)

predictionRF <- predict(rfMod, newdata = testing_data)
postResample(predictionRF, testing_data$total)
caret::confusionMatrix(predictionRF, testing_data$total)

mean((predictionRF > modelData1$total_line & modelData1$total > modelData1$total_line) |
       (predictionRF < modelData1$total_line & modelData1$total < modelData1$total_line))

rfVarImp <- varImp(rfMod, scale = FALSE)
plot(rfVarImp, top = 20)


testing_data <- testing_data |>
  mutate(
    total_line = modelData1$total_line,
    predXBM = predictionXBM,
    predRF = predictionRF,
    outcome = ifelse(total > total_line, "over",
                     ifelse(total < total_line, "under", NA))
  ) |>
  select(
    total,
    total_line,
    outcome,
    predXBM,
    predRF
  ) |>
  mutate(
    outXBM = ifelse(predXBM > total_line & outcome == "over", TRUE,
                    ifelse(predXBM < total_line & outcome == "under", TRUE,
                           ifelse(is.na(outcome), NA, FALSE))),
    outRF = ifelse(predRF > total_line & outcome == "over", TRUE,
                    ifelse(predRF < total_line & outcome == "under", TRUE,
                           ifelse(is.na(outcome), NA, FALSE)))
  )



