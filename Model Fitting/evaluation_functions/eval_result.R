## function to evaluate fit and performance of result fit

### Run Fit Diagnostics ----
fitnum <- 1
fitnum <- fitnum + 1
fit_analysis(Fit = fit_Res, 
             fit = fitnum, 
             train_data = histModelData,
             test_data = modelData,
             discrete = FALSE, 
             group = F
)


FitR2

totalPPCbars
totalPPCdens
totalPPDbars
totalPPDdens

predMetrics
TotalBetSuccessDF |> group_by(Data) |> arrange(desc(BetProb), .by_group = TRUE)
successPerf |> filter(Data == "Test") |> arrange(desc(Over)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(OddsOver)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(Under)) |> head(5)
successPerf |> filter(Data == "Test") |> arrange(desc(OddsUnder)) |> head(5)
loo_compare(loo_fits)

fitFormulas <- list()
FitR2 <- data.frame()
predMetrics <- data.frame()
ResultBetSuccessDF <- data.frame()
successPerf <- data.frame()
loo_fits <- list()

# Fit <- model_nfl_fit
# fit <- fitnum
# discrete <- TRUE
# group <- FALSE

fit_analysis <- function(Fit, fit, train_data = NULL, test_data = NULL,
                         discrete = TRUE, group = FALSE){
  # Fit <- model_nfl_fit
  # fit <- 1
  assign(paste0("fit", fit), Fit, envir = .GlobalEnv)
  
  #plot(Fit, ask = FALSE)
  
  # fitFormulas <- list()
  # for(i in 1:fit){
  #   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
  # }
  fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))
  
  ## Diagnostics ----
  #prior_summary(Fit)
  #posterior_summary(Fit)
  # launch_shinystan(Fit)
  print(Fit, digits = 4)
  fixedEff <- fixef(Fit)
  fixedEff <- data.frame(fixedEff) |>
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
  #print(fixedEff, digits = 4)
  fixedSigEff <- fixedEff |> filter(p_val < 0.2)
  # fixedSigEff <- fixedSigEff |> 
  #   rownames_to_column() |>
  #   mutate(
  #     response = str_split_i(rowname, "_", i = 1),
  #     param = str_remove(rowname, paste0(response,"_"))
  #   ) |> 
  #   relocate(c(response, param), .after = "rowname") |>
  #   select(-rowname)
  print(fixedSigEff)
  
  if(group){
    randEff <- ranef(Fit, summary = TRUE)
  }
  #print(randEff, digits = 4)
  #VarCorr(Fit)
  
  #postSum <- posterior_summary(Fit)
  #postSum[grepl("^sd_", rownames(postSum)), ]
  
  ### Bayes R2 -----
  FitR2temp <- bayes_R2(Fit)
  #FitR2temp
  FitR2tempDF <- FitR2temp |>
    bind_cols(
      Fit = paste0("Fit", fit),
      Response = c("result") #, "homescore", "awayscore")
      #Distribution = "NLDW(logit,id)"
    ) |>
    select(Fit, Response, #Distribution, 
           everything())
  print(FitR2tempDF)
  
  FitR2 <- bind_rows(
    FitR2tempDF,
    FitR2
  )
  assign("FitR2",FitR2, envir = .GlobalEnv)
  #FitR2 #<- FitR2tempDF
  
  # Fitsmooth <- conditional_smooths(Fit, method = "posterior_predict")
  # plot(Fitsmooth,
  #      stype = "contour",
  #      ask = FALSE)
  # 
  # Fiteffects <- conditional_effects(Fit, 
  #                                   # effects = c(
  #                                   #   "home_OSRS_net",
  #                                   #   "home_off_epa_roll",
  #                                   #   "away_off_td",
  #                                   #   "home_def_epa_roll",
  #                                   #   "away_SRS_net",
  #                                   #   "away_off_n"
  #                                   # ),
  #                                   method = "posterior_predict", 
  #                                   re_formula = NULL,
  #                                   robust = FALSE)
  # plot(Fiteffects, 
  #      points = TRUE, 
  #      ask = FALSE)
  
  ## Fitted ----
  resultfinalFit <- posterior_predict(Fit, resp = "result")
  resultfinalFitMean <- colMeans(resultfinalFit)
  resultfinalFitMed <- apply(resultfinalFit, 2, function(x){quantile(x, 0.5)})
  resultfinalFitLCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.025)})
  resultfinalFitUCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.975)})
  
  # homefinalFit <- posterior_predict(Fit, resp = "homescore")
  # homefinalFitMean <- colMeans(homefinalFit)
  # homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
  # homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
  # homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalFit <- resultfinalFit - homefinalFit
  # awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
  # awayfinalFitMean <- colMeans(awayfinalFit)
  # awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
  # awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
  # awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})
  # 
  # spreadfinalFit <- homefinalFit - awayfinalFit
  # spreadfinalFitMean <- colMeans(spreadfinalFit)
  # spreadfinalFitMed <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.5)})
  # spreadfinalFitLCB <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.025)})
  # spreadfinalFitUCB <- apply(spreadfinalFit, 2, function(x){quantile(x, 0.975)})
  
  ### PPC ----
  #### Bars ----
  # homePPCbars <- ppc_bars(y = histModelData$home_score, 
  #                         yrep = homefinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCbars <- ppc_bars(y = histModelData$away_score, 
  #                         yrep = awayfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCbars <- ppc_bars(y = histModelData$result, 
  #                           yrep = spreadfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  if(discrete){
    resultPPCbars <- ppc_bars(y = train_data$result, 
                              yrep = resultfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Fit", fit, " Result PPC")) +
      theme_bw()
    assign("resultPPCbars",resultPPCbars, envir = .GlobalEnv)
    print(resultPPCbars)
  }
  # resultPPCbars <- ppc_bars(y = histModelData$result, 
  #                          yrep = resultfinalFit[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Result PPC")) +
  #   theme_bw()
  
  # homePPCbars
  # awayPPCbars
  # spreadPPCbars
  #resultPPCbars
  
  # homePPCbarsG <- ppc_bars_grouped(y = histModelData$home_score, 
  #                                  yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                  group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCbarsG <- ppc_bars_grouped(y = histModelData$away_score, 
  #                                  yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                  group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCbarsG <- ppc_bars_grouped(y = histModelData$result, 
  #                                    yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                    group = histModelData$home_team) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  if(discrete){
    resultPPCbarsG <- ppc_bars_grouped(y = train_data$result, 
                                       yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ],
                                       group = train_data$home_team) + 
      labs(title = paste0("Fit", fit, " Result PPC")) +
      theme_bw()
    assign("resultPPCbarsG",resultPPCbarsG)
  }
  
  # homePPCbarsG
  # awayPPCbarsG
  # spreadPPCbarsG
  #resultPPCbarsG
  
  #### Density ----
  # homePPCdens <- ppc_dens_overlay(y = histModelData$home_score, 
  #                                 yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCdens <- ppc_dens_overlay(y = histModelData$away_score, 
  #                                 yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCdens <- ppc_dens_overlay(y = histModelData$result, 
  #                                   yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  resultPPCdens <- ppc_dens_overlay(y = train_data$result, 
                                    yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Fit", fit, " Result PPC")) +
    theme_bw()
  assign("resultPPCdens",resultPPCdens, envir = .GlobalEnv)
  print(resultPPCdens)
  
  # homePPCdens
  # awayPPCdens
  # spreadPPCdens
  #resultPPCdens
  
  # homePPCdensG <- ppc_dens_overlay_grouped(y = train_data$home_score, 
  #                                          yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Home PPC")) +
  #   theme_bw()
  # awayPPCdensG <- ppc_dens_overlay_grouped(y = train_data$away_score, 
  #                                          yrep = awayfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                          group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Away PPC")) +
  #   theme_bw()
  # spreadPPCdensG <- ppc_dens_overlay_grouped(y = train_data$result, 
  #                                            yrep = spreadfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                            group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Spread PPC")) +
  #   theme_bw()
  # resultPPCdensG <- ppc_dens_overlay_grouped(y = train_data$result, 
  #                                           yrep = resultfinalFit[sample(1:sims, 100, replace = FALSE), ],
  #                                           group = train_data$home_team) + 
  #   labs(title = paste0("Fit", fit, " Result PPC")) +
  #   theme_bw()
  
  # homePPCdensG
  # awayPPCdensG
  # spreadPPCdensG
  #resultPPCdensG
  
  ## Preds ----
  resultfinalPreds <- posterior_predict(Fit,
                                        resp = "result",
                                        newdata = test_data,
                                        allow_new_levels = TRUE,
                                        re_formula = NULL
  )
  resultfinalPredsMean <- colMeans(resultfinalPreds)
  resultfinalPredsMed <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.5)})
  resultfinalPredsLCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.025)})
  resultfinalPredsUCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  
  # homefinalPreds <- posterior_predict(Fit,
  #                                     resp = "homescore",
  #                                     newdata = test_data,
  #                                     allow_new_levels = TRUE,
  #                                     re_formula = NULL
  # )
  # homefinalPredsMean <- colMeans(homefinalPreds)
  # homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5)})
  # homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025)})
  # homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975)})
  # 
  # awayfinalPreds <- resultfinalPreds - homefinalPreds
  # awayfinalPreds <- posterior_predict(Fit,
  #                                     resp = "awayscore",
  #                                     newdata = test_data,
  #                                     allow_new_levels = TRUE,
  #                                     re_formula = NULL
  # )
  # awayfinalPredsMean <- colMeans(awayfinalPreds)
  # awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5)})
  # awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025)})
  # awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975)})
  # 
  # spreadfinalPreds <- homefinalPreds - awayfinalPreds
  # spreadfinalPredsMean <- colMeans(spreadfinalPreds)
  # spreadfinalPredsMed <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.5)})
  # spreadfinalPredsLCB <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.025)})
  # spreadfinalPredsUCB <- apply(spreadfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  ### PPD ----
  #### Bars ----
  # homePPDbars <- ppc_bars(y = test_data$home_score, 
  #                         yrep = homefinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDbars <- ppc_bars(y = test_data$away_score, 
  #                         yrep = awayfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDbars <- ppc_bars(y = test_data$result, 
  #                           yrep = spreadfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  if(discrete){
    resultPPDbars <- ppc_bars(y = test_data$result, 
                              yrep = resultfinalPreds[sample(1:sims, 1000, replace = FALSE), ]) + 
      labs(title = paste0("Preds", fit, " Result PPD")) +
      theme_bw()
    assign("resultPPDbars",resultPPDbars, envir = .GlobalEnv)
    print(resultPPDbars)
  }
  
  # homePPDbars
  # awayPPDbars
  # spreadPPDbars
  #resultPPDbars
  
  #### Density ----
  # homePPDdens <- ppc_dens_overlay(y = test_data$home_score, 
  #                                 yrep = homefinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Home PPD")) +
  #   theme_bw()
  # awayPPDdens <- ppc_dens_overlay(y = test_data$away_score, 
  #                                 yrep = awayfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Away PPD")) +
  #   theme_bw()
  # spreadPPDdens <- ppc_dens_overlay(y = test_data$result, 
  #                                   yrep = spreadfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  #   labs(title = paste0("Preds", fit, " Spread PPD")) +
  #   theme_bw()
  resultPPDdens <- ppc_dens_overlay(y = test_data$result, 
                                    yrep = resultfinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
    labs(title = paste0("Preds", fit, " Result PPD")) +
    theme_bw()
  assign("resultPPDdens",resultPPDdens, envir = .GlobalEnv)
  print(resultPPDdens)
  
  # homePPDdens
  # awayPPDdens
  # spreadPPDdens
  #resultPPDdens
  
  ## Goodness of Fit ----
  # homeTrain <- train_data$home_score
  # awayTrain <- train_data$away_score
  # spreadTrain <- train_data$result
  resultTrain <- train_data$result
  
  # homeTest <- test_data$home_score
  # awayTest <- test_data$away_score
  # spreadTest <- test_data$result
  resultTest <- test_data$result
  
  predMetricsHA <- tibble(
    Fit = rep(paste0("Fit", fit), 1),
    Response = c(
      #"home", 
      #"away",
      #"spread", 
      "result"
    ),
    # Distribution = c(
    #   "NLDW(logit,id)"
    # ),
    MAE_fit = c(
      #mean(abs(homefinalFitMean - homeTrain)),
      #mean(abs(awayfinalFitMean - awayTrain)),
      #mean(abs(spreadfinalFitMean - spreadTrain)),
      mean(abs(resultfinalFitMean - resultTrain))
    ),
    COV_fit = c(
      #mean(homefinalFitLCB < homeTrain &  homeTrain < homefinalFitUCB),
      #mean(awayfinalFitLCB < awayTrain &  awayTrain < awayfinalFitUCB),
      #mean(spreadfinalFitLCB < spreadTrain &  spreadTrain < spreadfinalFitUCB),
      mean(resultfinalFitLCB < resultTrain &  resultTrain < resultfinalFitUCB)
    ),
    MAE_pred = c(
      #mean(abs(homefinalPredsMean - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMean - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMean - spreadTest), na.rm = TRUE),
      mean(abs(resultfinalPredsMean - resultTest), na.rm = TRUE)
    ),
    MAD_pred = c(
      #mean(abs(homefinalPredsMed - homeTest), na.rm = TRUE),
      #mean(abs(awayfinalPredsMed - awayTest), na.rm = TRUE),
      #mean(abs(spreadfinalPredsMed - spreadTest), na.rm = TRUE),
      mean(abs(resultfinalPredsMed - resultTest), na.rm = TRUE)
    ),
    COV_pred = c(
      #mean(homefinalPredsLCB < homeTest & homeTest < homefinalPredsUCB, na.rm = TRUE),
      #mean(awayfinalPredsLCB < awayTest & awayTest < awayfinalPredsUCB, na.rm = TRUE),
      #mean(spreadfinalPredsLCB < spreadTest & spreadTest < spreadfinalPredsUCB, na.rm = TRUE),
      mean(resultfinalPredsLCB < resultTest & resultTest < resultfinalPredsUCB, na.rm = TRUE)
    )
  )
  #predMetricsHA
  
  predMetrics <- bind_rows(
    predMetricsHA,
    predMetrics
  )
  print(predMetrics) #<- predMetricsHA
  assign("predMetrics",predMetrics, envir = .GlobalEnv)
  
  ## Prob Errors ----
  ### Spread ----
  # #### Fit ----
  # spreadLineTrain <- modData |>
  #   filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   pull(spread_line)
  # 
  # spreadLineTrain <- train_data$spread_line
  # 
  # FittedProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
  # for(j in 1:length(spreadLineTrain)){
  #   fitted <- spreadfinalFit[, j]
  #   probs <- fitted > spreadLineTrain[j]
  #   FittedProbsSpread[, j] <- probs
  # }
  # FittedBetSpread <- colMeans(FittedProbsSpread)
  # FittedBetLogicalSpread <- FittedBetSpread > 0.5
  # FittedLogicalSpread <- spreadTrain > spreadLineTrain
  # FittedProbSpread <- mean(FittedBetLogicalSpread == FittedLogicalSpread, na.rm = TRUE)
  # FittedProbSpread
  # 
  # spreadDataTrain <- modData |> filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          result, spread_line, spreadCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     spreadFit = spreadfinalFitMean,
  #     coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == spreadCover,
  #     spreadCoverProb = FittedBetSpread,
  #     spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
  #                             ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
  #     # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
  #     spreadCoverSuccess = spreadCoverBet == spreadCover
  #   )
  # sum(is.na(spreadDataTrain$spreadCoverSuccess))
  # sum(!is.na(spreadDataTrain$spreadCoverSuccess))
  # 
  # spreadSuccessTrain <- spreadDataTrain |>
  #   summarise(
  #     spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
  #     spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  #   )
  # spreadSuccessTrain
  # 
  # #### Pred ----
  # spreadLineTest <- modData |>
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(spread_line)
  # 
  # spreadLineTest <- test_data$spread_line
  # 
  # PredsProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
  # for(j in 1:length(spreadLineTest)){
  #   fitted <- spreadfinalPreds[, j]
  #   probs <- fitted > spreadLineTest[j]
  #   PredsProbsSpread[, j] <- probs
  # }
  # PredsBetSpread <- colMeans(PredsProbsSpread)
  # PredsBetLogicalSpread <- PredsBetSpread > 0.5
  # PredsLogicalSpread <- spreadTest > spreadLineTest
  # PredsProbSpread <- mean(PredsBetLogicalSpread == PredsLogicalSpread, na.rm = TRUE)
  # PredsProbSpread
  # 
  # spreadDataTest <- modData |> filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   select(game_id, season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          result, spread_line,spreadCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     spreadPred = spreadfinalPredsMean,
  #     coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == spreadCover,
  #     spreadCoverProb = PredsBetSpread,
  #     spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
  #                             ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
  #     # spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE, 
  #     #                         ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
  #     spreadCoverSuccess = spreadCoverBet == spreadCover
  #   )
  # sum(is.na(spreadDataTest$spreadCoverSuccess))
  # sum(!is.na(spreadDataTest$spreadCoverSuccess))
  # 
  # spreadSuccessTest <- spreadDataTest |>
  #   summarise(
  #     spreadProbTest = mean(coverSuccess, na.rm = TRUE),
  #     spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  #   )
  # spreadSuccessTest
  
  ### Result ----
  #### Fit ----
  #Fit <- fit33
  
  # resultfinalFit <- posterior_predict(Fit, resp = "result")
  # resultfinalFitMean <- colMeans(resultfinalFit)
  # resultfinalFitMed <- apply(resultfinalFit, 2, function(x){quantile(x, 0.5)})
  # resultfinalFitLCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.025)})
  # resultfinalFitUCB <- apply(resultfinalFit, 2, function(x){quantile(x, 0.975)})
  
  resultfinalEFit <- posterior_epred(Fit, resp = "result")
  resultfinalEFitMean <- colMeans(resultfinalEFit)
  resultfinalEFitMed <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.5)})
  resultfinalEFitLCB <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.025)})
  resultfinalEFitUCB <- apply(resultfinalEFit, 2, function(x){quantile(x, 0.975)})
  
  # resultLineTrain <- modData |>
  #   filter(season == 2023 | (season == 2024 & week <= 6)) |>
  #   pull(result_line)
  
  resultLineTrain <- train_data$spread_line
  resultTrain <- train_data$result
  resultLineTrainResult <- ifelse(resultTrain > resultLineTrain, "homeCover",
                                  ifelse(resultTrain < resultLineTrain, "awayCover", NA))
  
  # FittedProbsResult <- matrix(NA, nrow = sims, ncol = length(resultLineTrain))
  # FittedProbsResultE <- matrix(NA, nrow = sims, ncol = length(resultLineTrain))
  # for(j in 1:length(resultLineTrain)){
  #   fitted <- resultfinalFit[, j]
  #   probs <- fitted > resultLineTrain[j]
  #   fittedE <- resultfinalEFit[, j]
  #   probsE <- fittedE > resultLineTrain[j]
  #   FittedProbsResult[, j] <- probs
  #   FittedProbsResultE[, j] <- probsE
  # }
  # FittedBetResult <- colMeans(FittedProbsResult)
  # FittedBetLogicalResult <- FittedBetResult > 0.5
  # FittedLogicalResult <- resultTrain > resultLineTrain
  # FittedProbResult <- mean(FittedBetLogicalResult == FittedLogicalResult, na.rm = TRUE)
  # FittedProbResult
  # 
  # FittedBetResultE <- colMeans(FittedProbsResultE)
  # FittedBetLogicalResultE <- FittedBetResultE > 0.5
  # FittedLogicalResultE <- resultTrain > resultLineTrain
  # FittedProbResultE <- mean(FittedBetLogicalResultE == FittedLogicalResultE, na.rm = TRUE)
  # FittedProbResultE
  
  FittedOver <- t(t(resultfinalFit) > resultLineTrain)
  FittedBetResultOver <- colMeans(FittedOver)
  #FittedBetResultOver <- colMeans(FittedProbsResult)
  FittedBetLogicalResultOver <- FittedBetResultOver > 0.5
  FittedBetLogicalResultOddsOver <- FittedBetResultOver > train_data$home_spread_prob
  FittedLogicalResultOver <- resultTrain > resultLineTrain
  FittedProbResultOver <- mean(FittedBetLogicalResultOver == FittedLogicalResultOver, na.rm = TRUE)
  FittedProbResultOddsOver <- mean(FittedBetLogicalResultOddsOver == FittedLogicalResultOver, na.rm = TRUE)
  # FittedProbResultOver
  # FittedProbResultOddsOver
  
  FittedUnder <- t(t(resultfinalFit) < resultLineTrain)
  FittedBetResultUnder <- colMeans(FittedUnder)
  #FittedBetResultUnder <- colMeans(FittedProbsResult)
  FittedBetLogicalResultUnder <- FittedBetResultUnder > 0.5
  FittedBetLogicalResultOddsUnder <- FittedBetResultUnder > train_data$away_spread_prob
  FittedLogicalResultUnder <- resultTrain < resultLineTrain
  FittedProbResultUnder <- mean(FittedBetLogicalResultUnder == FittedLogicalResultUnder, na.rm = TRUE)
  FittedProbResultOddsUnder <- mean(FittedBetLogicalResultOddsUnder == FittedLogicalResultUnder, na.rm = TRUE)
  # FittedProbResultUnder
  # FittedProbResultOddsUnder
  
  FittedEOver <- t(t(resultfinalEFit) > resultLineTrain)
  FittedEBetResultOver <- colMeans(FittedEOver)
  #FittedEBetResultOver <- colMeans(FittedEProbsResult)
  FittedEBetLogicalResultOver <- FittedEBetResultOver > 0.5
  FittedEBetLogicalResultOddsOver <- FittedEBetResultOver > train_data$home_spread_prob
  FittedELogicalResultOver <- resultTrain > resultLineTrain
  FittedEProbResultOver <- mean(FittedEBetLogicalResultOver == FittedELogicalResultOver, na.rm = TRUE)
  FittedEProbResultOddsOver <- mean(FittedEBetLogicalResultOddsOver == FittedELogicalResultOver, na.rm = TRUE)
  # FittedEProbResultOver
  # FittedEProbResultOddsOver
  
  FittedEUnder <- t(t(resultfinalEFit) < resultLineTrain)
  FittedEBetResultUnder <- colMeans(FittedEUnder)
  #FittedEBetResultUnder <- colMeans(FittedEProbsResult)
  FittedEBetLogicalResultUnder <- FittedEBetResultUnder > 0.5
  FittedEBetLogicalResultOddsUnder <- FittedEBetResultUnder > train_data$away_spread_prob
  FittedELogicalResultUnder <- resultTrain < resultLineTrain
  FittedEProbResultUnder <- mean(FittedEBetLogicalResultUnder == FittedELogicalResultUnder, na.rm = TRUE)
  FittedEProbResultOddsUnder <- mean(FittedEBetLogicalResultOddsUnder == FittedELogicalResultUnder, na.rm = TRUE)
  # FittedEProbResultUnder
  # FittedEProbResultOddsUnder
  
  resultSuccessTrain <- data.frame(
    TrainOver = FittedProbResultOver,
    TrainOddsOver = FittedProbResultOddsOver,
    TrainOverE = FittedEProbResultOver,
    TrainOddsOverE = FittedEProbResultOddsOver,
    TrainUnder = FittedProbResultUnder,
    TrainOddsUnder = FittedProbResultOddsUnder,
    TrainUnderE = FittedEProbResultUnder,
    TrainOddsUnderE = FittedEProbResultOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  FittedBetLogicalResultOdds <- ifelse(FittedBetResultOver > train_data$home_spread_prob, "homeCover", 
                                       ifelse(FittedBetResultUnder > train_data$away_spread_prob, "awayCover", NA))
  FittedBetLogicalResultOddsProb <- mean(FittedBetLogicalResultOdds == resultLineTrainResult, na.rm = TRUE)
  FittedBetLogicalResultOddsProbBets <- sum(!is.na(FittedBetLogicalResultOddsProb))
  FittedEBetLogicalResultOdds <- ifelse(FittedEBetResultOver > train_data$home_spread_prob, "homeCover", 
                                        ifelse(FittedEBetResultUnder > train_data$away_spread_prob, "awayCover", NA))
  FittedEBetLogicalResultOddsProb <- mean(FittedEBetLogicalResultOdds == resultLineTrainResult, na.rm = TRUE)
  FittedEBetLogicalResultOddsProbBets <- sum(!is.na(FittedEBetLogicalResultOddsProb))
  
  # resultDataTrain <- modData |> 
  #   filter(season %in% c(2022,2023) | (season == 2024 & week <= 6)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, away_OSRS_net,
  #          result, spread_line, resultCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     resultFit = resultfinalFitMean,
  #     coverBet = ifelse(resultFit > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == resultCover,
  #     resultCoverProb = FittedBetResult,
  #     resultCoverBet = ifelse(resultCoverProb > over_prob, TRUE,
  #                            ifelse(1 - resultCoverProb > under_prob, FALSE, NA)),
  #     # resultCoverBet = ifelse(resultCoverProb > .6, TRUE, ifelse(1 - resultCoverProb > .6, FALSE, NA)),
  #     resultCoverSuccess = resultCoverBet == resultCover,
  #     
  #     resultFitE = resultfinalEFitMean,
  #     coverBetE = ifelse(resultFitE > spread_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == resultCover,
  #     resultCoverProbE = FittedBetResultE,
  #     resultCoverBetE = ifelse(resultCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - resultCoverProbE > under_prob, FALSE, NA)),
  #     # resultCoverBet = ifelse(resultCoverProb > .6, TRUE, ifelse(1 - resultCoverProb > .6, FALSE, NA)),
  #     resultCoverSuccessE = resultCoverBetE == resultCover
  #   )
  # # sum(is.na(resultDataTrain$resultCoverSuccess))
  # # sum(!is.na(resultDataTrain$resultCoverSuccess))
  # # sum(is.na(resultDataTrain$resultCoverSuccessE))
  # # sum(!is.na(resultDataTrain$resultCoverSuccessE))
  # 
  # resultSuccessTrain <- resultDataTrain |>
  #   summarise(
  #     resultProbTrain = mean(coverSuccess, na.rm = TRUE),
  #     resultOddsProbTrain = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultProbTrainE = mean(coverSuccessE, na.rm = TRUE),
  #     resultOddsProbTrainE = mean(resultCoverSuccessE, na.rm = TRUE)
  #  )
  #resultSuccessTrain
  
  #### Pred ----
  # resultLineTest <- modData |>
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result), 
  #          !is.na(home_resultTD),
  #          !is.na(away_resultTD),
  #          !is.na(home_fg_made),
  #          !is.na(away_fg_made)
  #   ) |>
  #   pull(spread_line)
  
  
  # resultfinalPreds <- posterior_predict(Fit,
  #                                      resp = "result",
  #                                      newdata = test_data,
  #                                      allow_new_levels = TRUE,
  #                                      re_formula = NULL
  # )
  # resultfinalPredsMean <- colMeans(resultfinalPreds)
  # resultfinalPredsMed <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.5)})
  # resultfinalPredsLCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.025)})
  # resultfinalPredsUCB <- apply(resultfinalPreds, 2, function(x){quantile(x, 0.975)})
  
  resultfinalEPreds <- posterior_epred(Fit,
                                       resp = "result",
                                       newdata = test_data,
                                       allow_new_levels = TRUE,
                                       re_formula = NULL
  )
  resultfinalEPredsMean <- colMeans(resultfinalEPreds)
  resultfinalEPredsMed <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.5)})
  resultfinalEPredsLCB <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.025)})
  resultfinalEPredsUCB <- apply(resultfinalEPreds, 2, function(x){quantile(x, 0.975)})
  
  resultLineTest <- test_data$spread_line
  resultTest <- test_data$result
  resultLineTestResult <- ifelse(resultTest > resultLineTest, "homeCover",
                                 ifelse(resultTest < resultLineTest, "awayCover", NA))
  
  # PredsProbsResult <- matrix(NA, nrow = sims, ncol = length(resultLineTest))
  # PredsProbsResultE <- matrix(NA, nrow = sims, ncol = length(resultLineTest))
  # for(j in 1:length(resultLineTest)){
  #   fitted <- resultfinalPreds[, j]
  #   probs <- fitted > resultLineTest[j]
  #   fittedE <- resultfinalEPreds[, j]
  #   probsE <- fittedE > resultLineTest[j]
  #   PredsProbsResult[, j] <- probs
  #   PredsProbsResultE[, j] <- probsE
  # }
  # PredsBetResult <- colMeans(PredsProbsResult)
  # PredsBetLogicalResult <- PredsBetResult > 0.5
  # PredsLogicalResult <- resultTest > resultLineTest
  # PredsProbResult <- mean(PredsBetLogicalResult == PredsLogicalResult, na.rm = TRUE)
  # PredsProbResult
  # 
  # PredsBetResultE <- colMeans(PredsProbsResultE)
  # PredsBetLogicalResultE <- PredsBetResultE > 0.5
  # PredsLogicalResultE <- resultTest > resultLineTest
  # PredsProbResultE <- mean(PredsBetLogicalResultE == PredsLogicalResultE, na.rm = TRUE)
  # PredsProbResultE
  # 
  # resultDataTest <- modData |> 
  #   filter(season == 2024 & week > 6) |>
  #   filter(!is.na(result)) |>
  #   select(season, week, #game_type,
  #          home_team, home_score, away_team, away_score,
  #          home_OSRS_net, home_OSRS,
  #          away_OSRS_net, away_OSRS,
  #          result, spread_line, resultCover,
  #          home_spread_odds, home_spread_prob,
  #          away_spread_prob, away_spread_prob,
  #          over_odds, over_prob,
  #          under_odds, under_prob) |>
  #   mutate(
  #     resultPreds = resultfinalPredsMean,
  #     coverBet = ifelse(resultPreds > spread_line, TRUE, FALSE),
  #     coverSuccess = coverBet == resultCover,
  #     resultCoverProb = PredsBetResult,
  #     resultCoverBet = ifelse(resultCoverProb > over_prob, TRUE,
  #                            ifelse(1 - resultCoverProb > under_prob, FALSE, NA)),
  #     resultCoverBet2 = ifelse(resultCoverProb > .70, TRUE,
  #                             ifelse(1 - resultCoverProb > .70, FALSE, NA)),
  #     resultCoverSuccess = resultCoverBet == resultCover,
  #     resultCoverSuccess2 = resultCoverBet2 == resultCover,
  #     
  #     resultPredsE = resultfinalEPredsMean,
  #     coverBetE = ifelse(resultPredsE > spread_line, TRUE, FALSE),
  #     coverSuccessE = coverBetE == resultCover,
  #     resultCoverProbE = PredsBetResultE,
  #     resultCoverBetE = ifelse(resultCoverProbE > over_prob, TRUE,
  #                             ifelse(1 - resultCoverProbE > under_prob, FALSE, NA)),
  #     resultCoverBet2E = ifelse(resultCoverProbE > .70, TRUE,
  #                              ifelse(1 - resultCoverProbE > .70, FALSE, NA)),
  #     resultCoverSuccessE = resultCoverBetE == resultCover,
  #     resultCoverSuccess2E = resultCoverBet2E == resultCover,
  #   )
  # sum(is.na(resultDataTest$resultCoverSuccess))
  # sum(!is.na(resultDataTest$resultCoverSuccess))
  # sum(is.na(resultDataTest$resultCoverSuccessE))
  # sum(!is.na(resultDataTest$resultCoverSuccessE))
  # 
  # resultSuccessTest <- resultDataTest |>
  #   summarise(
  #     resultProbTest = mean(coverSuccess, na.rm = TRUE),
  #     resultOddsProbTest = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultOddsProbTest2 = mean(resultCoverSuccess, na.rm = TRUE),
  #     resultProbTestE = mean(coverSuccessE, na.rm = TRUE),
  #     resultOddsProbTestE = mean(resultCoverSuccessE, na.rm = TRUE),
  #     resultOddsProbTest2E = mean(resultCoverSuccess2E, na.rm = TRUE)
  #   )
  # resultSuccessTest
  
  # resultfinalPredsResult <- ifelse(t(resultfinalPreds) > resultLineTest, "over",
  #                                 ifelse(t(resultfinalPreds) < resultLineTest, "under", NA))
  # resultfinalPredsResult <- t(resultfinalPredsResult)
  
  
  PredsOver <- t(t(resultfinalPreds) > resultLineTest)
  PredsBetResultOver <- colMeans(PredsOver)
  #PredsBetResultOver <- colMeans(PredsProbsResult)
  PredsBetLogicalResultOver <- PredsBetResultOver > 0.5
  PredsBetLogicalResultOddsOver <- PredsBetResultOver > test_data$home_spread_prob
  PredsLogicalResultOver <- resultTest > resultLineTest
  PredsProbResultOver <- mean(PredsBetLogicalResultOver == PredsLogicalResultOver, na.rm = TRUE)
  PredsProbResultOddsOver <- mean(PredsBetLogicalResultOddsOver == PredsLogicalResultOver, na.rm = TRUE)
  # PredsProbResultOver
  # PredsProbResultOddsOver
  
  PredsUnder <- t(t(resultfinalPreds) < resultLineTest)
  PredsBetResultUnder <- colMeans(PredsUnder)
  #PredsBetResultUnder <- colMeans(PredsProbsResult)
  PredsBetLogicalResultUnder <- PredsBetResultUnder > 0.5
  PredsBetLogicalResultOddsUnder <- PredsBetResultUnder > test_data$away_spread_prob
  PredsLogicalResultUnder <- resultTest < resultLineTest
  PredsProbResultUnder <- mean(PredsBetLogicalResultUnder == PredsLogicalResultUnder, na.rm = TRUE)
  PredsProbResultOddsUnder <- mean(PredsBetLogicalResultOddsUnder == PredsLogicalResultUnder, na.rm = TRUE)
  # PredsProbResultUnder
  # PredsProbResultOddsUnder
  
  
  PredsEOver <- t(t(resultfinalEPreds) > resultLineTest)
  PredsEBetResultOver <- colMeans(PredsEOver)
  #PredsEBetResultOver <- colMeans(PredsEProbsResult)
  PredsEBetLogicalResultOver <- PredsEBetResultOver > 0.5
  PredsEBetLogicalResultOddsOver <- PredsEBetResultOver > test_data$home_spread_prob
  PredsELogicalResultOver <- resultTest > resultLineTest
  PredsEProbResultOver <- mean(PredsEBetLogicalResultOver == PredsELogicalResultOver, na.rm = TRUE)
  PredsEProbResultOddsOver <- mean(PredsEBetLogicalResultOddsOver == PredsELogicalResultOver, na.rm = TRUE)
  # PredsEProbResultOver
  # PredsEProbResultOddsOver
  
  PredsEUnder <- t(t(resultfinalEPreds) < resultLineTest)
  PredsEBetResultUnder <- colMeans(PredsEUnder)
  #PredsEBetResultUnder <- colMeans(PredsEProbsResult)
  PredsEBetLogicalResultUnder <- PredsEBetResultUnder > 0.5
  PredsEBetLogicalResultOddsUnder <- PredsEBetResultUnder > test_data$away_spread_prob
  PredsELogicalResultUnder <- resultTest < resultLineTest
  PredsEProbResultUnder <- mean(PredsEBetLogicalResultUnder == PredsELogicalResultUnder, na.rm = TRUE)
  PredsEProbResultOddsUnder <- mean(PredsEBetLogicalResultOddsUnder == PredsELogicalResultUnder, na.rm = TRUE)
  # PredsEProbResultUnder
  # PredsEProbResultOddsUnder
  
  resultSuccessTest <- data.frame(
    TestOver = PredsProbResultOver,
    TestOddsOver = PredsProbResultOddsOver,
    TestOverE = PredsEProbResultOver,
    TestOddsOverE = PredsEProbResultOddsOver,
    TestUnder = PredsProbResultUnder,
    TestOddsUnder = PredsProbResultOddsUnder,
    TestUnderE = PredsEProbResultUnder,
    TestOddsUnderE = PredsEProbResultOddsUnder
  ) |> 
    mutate(across(everything(), ~round(.x, 3)))
  
  PredsBetLogicalResultOdds <- ifelse(PredsBetResultOver > test_data$home_spread_prob, "homeCover", 
                                      ifelse(PredsBetResultUnder > test_data$away_spread_prob, "awayCover", NA))
  PredsBetLogicalResultOddsProb <- mean(PredsBetLogicalResultOdds == resultLineTestResult, na.rm = TRUE)
  PredsBetLogicalResultOddsProbBets <- sum(!is.na(PredsBetLogicalResultOdds))
  PredsEBetLogicalResultOdds <- ifelse(PredsEBetResultOver > test_data$home_spread_prob, "homeCover", 
                                       ifelse(PredsEBetResultUnder > test_data$away_spread_prob, "awayCover", NA))
  PredsEBetLogicalResultOddsProb <- mean(PredsEBetLogicalResultOdds == resultLineTestResult, na.rm = TRUE)
  PredsEBetLogicalResultOddsProbBets <- sum(!is.na(PredsEBetLogicalResultOdds))
  
  ResultBetSuccessDFtemp <- data.frame(
    Fit = paste0("Fit ", fit), 
    Data = c("Train", "Test"),
    BetNum = c(FittedBetLogicalResultOddsProbBets, PredsBetLogicalResultOddsProbBets),
    BetProb = c(FittedBetLogicalResultOddsProb, PredsBetLogicalResultOddsProb),
    BetNumE = c(FittedEBetLogicalResultOddsProbBets, PredsEBetLogicalResultOddsProbBets),
    BetProbE = c(FittedEBetLogicalResultOddsProb, PredsEBetLogicalResultOddsProb)
  )
  print(ResultBetSuccessDFtemp)
  
  ResultBetSuccessDF <- bind_rows(
    ResultBetSuccessDFtemp,
    ResultBetSuccessDF
  )
  assign("ResultBetSuccessDF", ResultBetSuccessDF, envir = .GlobalEnv)
  
  ## Success Perf ----
  successPerfTemp <- bind_rows( 
    resultSuccessTrain |> rename_with(~str_remove(.x, "Train"), .cols = everything()),
    resultSuccessTest |> rename_with(~str_remove(.x, "Test"), .cols = everything())
  ) |>
    mutate(
      Fit = paste0("Fit ", fit), 
      Data = c("Train", "Test"),
      .before = 1
    )
  print(successPerfTemp)
  
  successPerf <- bind_rows(
    successPerfTemp,
    successPerf
  )
  assign("successPerf", successPerf, envir = .GlobalEnv)
  #successPerf #<- successPerfTemp
  
  ### Loo ----
  loo_fits[[paste0("Fit",fit)]] <- loo(Fit)
  looComp <- loo_compare(loo_fits)
  print(looComp)
  assign("loo_fits", loo_fits, envir = .GlobalEnv)
}
