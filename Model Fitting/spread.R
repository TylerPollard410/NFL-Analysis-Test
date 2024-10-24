# Load Libraries ----
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
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
# library(cowplot)
# library(GGally)
# library(patchwork)

## Modeling ----
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

gameData <- load_schedules(seasons = 2003:2023)
gameData2 <- gameData |>
  filter(complete.cases(result)) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
    ) 

gameDataLong <- gameData2 |>
  clean_homeaway(invert = c("result", "spread_line")) 

teamsData <- load_teams(current = FALSE)














iters <- 5000
burn <- 1000
chains <- 1
sims <- (iters-burn)*chains

gameDataTrain <- gameData2 |>
  filter(!(season %in% 2023:2024)) |>
  mutate(
    spread_line = scale(spread_line)
  )
str(gameDataTrain)

gameDataTest <- gameData2 |>
  filter(season %in% 2023:2024) |>
  mutate(
    spread_line = scale(spread_line,
                        center = attr(gameDataTrain$spread_line, "scaled:center"),
                        scale = attr(gameDataTrain$spread_line, "scaled:scale"))
  )
str(gameDataTest)

## Scatter ----
ggplot(data = gameDataTrain, aes(x = spread_line, y = result)) +
  geom_point() +
  geom_smooth()

## Histogram ----
ggplot(data = gameDataTrain) +
  geom_histogram(
    aes(x = result, after_stat(density)),
    color = "#99c7c7", fill = "#bcdcdc") +
  geom_vline(aes(xintercept = mean(result)),
             color = "orange", linewidth = 2) +
  geom_vline(aes(xintercept = median(result)),
             color = "orange3", linewidth = 2) +
  geom_density(#data = final_data3,
    aes(x = result),
    color = "#007C7C", 
    linewidth = 1) +
  theme_bw()


fit1 <- brm(
  bf(result ~ spread_line + (1|season)),
  data = gameDataFit,
  family = mixture(gaussian, gaussian, nmix = 2),
  save_pars = save_pars(all = TRUE),
  seed = 52,
  warmup = burn,
  iter = iters,
  chains = chains,
  normalize = FALSE,
  control = list(adapt_delta = 0.95)
)


### Save Fit ----
fit <- 1
Fit <- fit1
fileLoc <- "~/Desktop/Temp Hurricane Model Data/ Fits/" 
save(Fit33,
     file = paste0(fileLoc, "Fit", fit, ".RData"))

### Diagnostics ----
prior_summary(Fit)
posterior_summary(Fit)
launch_shinystan(Fit)

print(FitNULL, digits = 4)
print(FitHWRF, digits = 4)
print(FitHWRFstormID, digits = 4)
print(Fit12, digits = 4)
print(Fit12B, digits = 4)
print(Fit12C, digits = 4)
print(Fit12D, digits = 4)
print(Fit15, digits = 4)
print(Fit16, digits = 4)
print(Fit17, digits = 4)
print(Fit32, digits = 4)

plot(Fit)
FitppcFit <- pp_check(Fit, ndraws = 100) + 
  labs(title = paste0("Fit", fit, " Fit PPC")) +
  theme_bw()
FitppcFit
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

bayes_factor(Fit, FitHWRFstormID)
bayes_factor(Fit, Fit8)
bayes_factor(Fit, Fit15)

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

Fiteffects2 <- conditional_effects(Fit12B, 
                                            method = "posterior_predict",
                                            robust = FALSE,
                                            effects = "logHWRF")
plot(Fiteffects2, 
     points = TRUE, 
     ask = FALSE)

Fiteffects3 <- Fiteffects2
Fiteffects3$logHWRF <- Fiteffects3$logHWRF |>
  mutate(
    logHWRF = exp(logHWRF),
    effect1__ = exp(effect1__)
  )
plot(Fiteffects3, 
     points = TRUE, 
     ask = FALSE)

### Prediction ----
## Fitted
FitfinalFit <- posterior_predict(Fit)
# FitfinalResiduals <- t(StormdataTrain3$VMAX - t(FitfinalFit))
# FitfinalResidualsMean <- colMeans(FitfinalResiduals)
FitfinalFitMean <- colMeans(FitfinalFit)
FitfinalFitMed <- apply(FitfinalFit, 2, function(x){quantile(x, 0.5)})
FitfinalFitLCB <- apply(FitfinalFit, 2, function(x){quantile(x, 0.025)})
FitfinalFitUCB <- apply(FitfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
FitfinalPreds <- posterior_predict(Fit, 
                                            newdata = StormdataTest6,
                                            allow_new_levels = TRUE, 
                                            re_formula = NULL)
FitfinalPredsMean <- colMeans(FitfinalPreds)
FitfinalPredsMed <- apply(FitfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
FitfinalPredsLCB <- apply(FitfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
FitfinalPredsUCB <- apply(FitfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

FitpredMetrics <- tibble(
  Fit = paste0("Fit", fit),
  MAE_HWRF_fit = mean(abs(StormdataTrain$HWRF - StormdataTrain$VMAX)),
  MAE_fit = mean(abs(FitfinalFitMean - StormdataTrain$VMAX)),
  COV_fit = mean(FitfinalFitLCB < StormdataTrain$VMAX & StormdataTrain$VMAX < FitfinalFitUCB),
  MAE_HWRF_pred = mean(abs(StormdataTest$HWRF - Actual_Yvec)),
  MAE_pred = mean(abs(FitfinalPredsMean - Actual_Yvec), na.rm = TRUE),
  MAD_pred = mean(abs(FitfinalPredsMed - Actual_Yvec), na.rm = TRUE),
  COV_pred = mean(FitfinalPredsLCB < Actual_Yvec & Actual_Yvec < FitfinalPredsUCB)
)
FitpredMetrics

### Plotting ----
#### Fit ----
ppc_dens_overlay(y = Actual_Yvec, yrep = FitfinalPreds) +
  labs(title = "Fit Predict") +
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
###### Quantile 2.5 
FitLCBsims <- apply(FitfinalFit, 
                             MARGIN = 1,
                             function(x){
                               quantile(x, 0.025)
                             })
FitLCBpvalueVec <- FitLCBsims < quantile(StormdataTrain$VMAX, 0.025)
FitLCBpvalue <- sum(FitLCBpvalueVec)
FitLCBpvalue <- round(FitLCBpvalue/(sims), 3)
FitLCBpvalue <- min(FitLCBpvalue, 1 - FitLCBpvalue)

Fit_ppcLCB <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) quantile(y, 0.025), freq = FALSE) +
  labs(title = paste0("2.5% Quantile (p-val = ", FitLCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcLCB

###### Quantile 97.5 
FitUCBsims <- apply(FitfinalFit, 
                             MARGIN = 1,
                             function(x){
                               quantile(x, 0.975)
                             })
FitUCBpvalueVec <- FitUCBsims < quantile(StormdataTrain$VMAX, 0.975)
FitUCBpvalue <- as.numeric(sum(FitUCBpvalueVec))
FitUCBpvalue <- round(FitUCBpvalue/sims, 3)
FitUCBpvalue <- min(FitUCBpvalue, 1 - FitUCBpvalue)

Fit_ppcUCB <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) quantile(y, 0.975), freq = FALSE) +
  labs(title = paste0("97.5% Quantile (p-val = ", FitUCBpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcUCB

###### Mean 
FitMEANsims <- apply(FitfinalFit, 
                              MARGIN = 1,
                              function(x){
                                mean(x)
                              })
FitMEANpvalueVec <- FitMEANsims < mean(StormdataTrain$VMAX)
FitMEANpvalue <- sum(FitMEANpvalueVec)
FitMEANpvalue <- round(FitMEANpvalue/sims, 3)
FitMEANpvalue <- min(FitMEANpvalue, 1 - FitMEANpvalue)

Fit_ppcMEAN <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) mean(y), freq = FALSE) +
  labs(title = paste0("Mean (p-val = ", FitMEANpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcMEAN

###### Med 
FitMEDsims <- apply(FitfinalFit, 
                             MARGIN = 1,
                             function(x){
                               quantile(x, 0.5)
                             })
FitMEDpvalueVec <- FitMEDsims < quantile(StormdataTrain$VMAX, 0.5)
FitMEDpvalue <- sum(FitMEDpvalueVec)
FitMEDpvalue <- round(FitMEDpvalue/sims, 3)
FitMEDpvalue <- min(FitMEDpvalue, 1 - FitMEDpvalue)

Fit_ppcMED <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) quantile(y, 0.5), freq = FALSE) +
  labs(title = paste0("Median (p-val = ", FitMEDpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcMED

###### SD 
FitSDsims <- apply(FitfinalFit, 
                            MARGIN = 1,
                            function(x){
                              sd(x)
                            })
FitSDpvalueVec <- FitSDsims < sd(StormdataTrain$VMAX)
FitSDpvalue <- sum(FitSDpvalueVec)
FitSDpvalue <- round(FitSDpvalue/sims, 3)
FitSDpvalue <- min(FitSDpvalue, 1 - FitSDpvalue)

Fit_ppcSD <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) sd(y), freq = FALSE) +
  labs(title = paste0("SD (p-val = ", FitSDpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcSD

###### Range 
FitRANGEsims <- apply(FitfinalFit, 
                               MARGIN = 1,
                               function(x){
                                 max(x)-min(x)
                               })
FitRANGEpvalueVec <- FitRANGEsims < (max(StormdataTrain$VMAX)-min(StormdataTrain3$VMAX))
FitRANGEpvalue <- sum(FitRANGEpvalueVec)
FitRANGEpvalue <- round(FitRANGEpvalue/sims, 3)
FitRANGEpvalue <- min(FitRANGEpvalue, 1 - FitRANGEpvalue)

Fit_ppcRANGE <- 
  ppc_stat(StormdataTrain$VMAX,
           FitfinalFit,
           stat = function(y) max(y)-min(y), freq = FALSE) +
  labs(title = paste0("Range (p-val = ", FitRANGEpvalue, ")")) +
  theme_bw() +
  legend_none()
#Fit_ppcRANGE

### Combined Plot ----
Fit_ppcComb <- 
  FitppcFit /
  (Fit_ppcLCB | Fit_ppcMED | Fit_ppcUCB) /
  (Fit_ppcRANGE | Fit_ppcMEAN | Fit_ppcSD)
Fit_ppcComb

### Bayes p-values ----
Fitpvalues <- tibble(
  Fit = paste0("Fit", fit),
  LCB = FitLCBpvalue,
  Median = FitMEDpvalue,
  UCB = FitUCBpvalue,
  Range = FitRANGEpvalue,
  Mean = FitMEANpvalue,
  SD = FitSDpvalue
)
Fitpvalues

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

