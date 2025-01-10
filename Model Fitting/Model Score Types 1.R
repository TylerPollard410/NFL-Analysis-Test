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

seasonsMod <- 2023:2024
# gameDataMod <- gameData |> filter(season %in% seasonsMod)
# gameDataLongMod <- gameDataLong |> filter(season %in% seasonsMod)
# pbpDataMod <- load_pbp(seasons = seasonsMod)
# load("./app/data/seasonWeekStandings.rda")
# seasonWeekStandings <- seasonWeekStandings |> filter(season %in% seasonsMod)
load(url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/modData.rda"))
class(modData$home_totalTD)
class(modData$away_totalTD)
class(modData$home_fg_made)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)
class(modData$home_totalTD)

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Previous Data ----
modData2 <- modData |> 
  filter(!is.na(result)) |>
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
    home_totalTD = as.inter
  )
  mutate(
    home_totalTDScore = 6*home_totalTD,
    home_fg_madeScore = 3*home_fg_made,
    home_pat_madeScore = home_pat_made,
    home_safetiesScore = 2*home_safeties,
    home_twoPtConvScore = 2*home_twoPtConv,
    away_totalTDScore = 6*away_totalTD,
    away_fg_madeScore = 3*away_fg_made,
    away_pat_madeScore = away_pat_made,
    away_safetiesScore = 2*away_safeties,
    away_twoPtConvScore = 2*away_twoPtConv,
    home_totalTDScore2 = home_totalTDScore + home_pat_madeScore + home_twoPtConvScore,
    away_totalTDScore2 = away_totalTDScore + away_pat_madeScore + away_twoPtConvScore
  )

histModelData1 <- modData2 |> 
  filter(between(season, 2023, 2023) | (season == 2024 & week <= 6))
modelData1 <- modData2 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  )

predictorData <- histModelData1 |> 
  select(-home_score, -away_score,
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
         -total_line,
         -location,
         -div_game,
         -roof)
preProcValues <- preProcess(predictorData,
                            method = c("center", "scale"))
preProcValues
predictorData2 <- predict(preProcValues, predictorData)
histModelData2 <- predict(preProcValues, histModelData1)
modelData2 <- predict(preProcValues, modelData1)

histModelData <- histModelData2
modelData <- modelData2

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


## Correlations ----
homeTDcor <- cor(histModelData |> select(home_totalTD),
                 histModelData |> select(c(where(is.numeric), -home_totalTD)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
homeTDcorT <- t(homeTDcor)
homeTDcorT2 <- homeTDcorT[order(abs(homeTDcorT)),]
homeTDcorT2df <- data.frame(sort(abs(homeTDcorT2), decreasing = TRUE))

homeFGcor <- cor(histModelData |> select(home_fg_made),
                 histModelData |> select(c(where(is.numeric), -home_fg_made)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
homeFGcorT <- t(homeFGcor)
homeFGcorT2 <- homeFGcorT[order(abs(homeFGcorT)),]
homeFGcorT2df <- data.frame(sort(abs(homeFGcorT2), decreasing = TRUE))

spreadCovercor <- cor(histModelData |> select(spreadCover),
                      histModelData |> select(c(where(is.numeric), -spreadCover)),
                      use = "pairwise.complete.obs",
                      method = "kendall"
)
spreadCovercorT <- t(spreadCovercor)
spreadCovercorT2 <- spreadCovercorT[order(abs(spreadCovercorT)),]
spreadCovercorT2df <- data.frame(sort(round(abs(spreadCovercorT2), 5), decreasing = TRUE))



# Fit historical ----

iters <- 2000
burn <- 1000
chains <- 4
sims <- (iters-burn)*chains

## Custom Family ----
# Define a custom family for NFL scoring
nfl_scoring <- custom_family(
  name = "nfl_scoring",
  dpars = c("mu", "phi", 
            "muTD", "shapeTD", 
            "muFG", "shapeFG", 
            "muSafety", "shapeSafety",
            "muXP", "mu2pt"),
  links = c("log", "log", 
            "log", "identity", 
            "log", "identity", 
            "log", "identity", 
            "logit", "logit"),
  lb = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  # Lower bounds for mu and phi
  ub = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 1),     # No upper bounds specified
  type = "int"
)

# Define the Stan likelihood function for the custom family
# // Log-PMF for Discrete Weibull
# real discrete_weibull_lpmf(int y, real mu, real shape) {
#   return log(mu^y^shape - mu^(y + 1)^shape);
# }
# 
# // Log-CDF for Discrete Weibull
# real discrete_weibull_lcdf(int y, real mu, real shape) {
#   return log1m(mu^(y + 1)^shape);
# }
# 
# // Log-CCDF for Discrete Weibull
# real discrete_weibull_lccdf(int y, real mu, real shape) {
#   return lmultiply((y + 1)^shape, mu);
# }
# 
# // RNG function for Discrete Weibull using inverse transform sampling
# real discrete_weibull_rng(real mu, real shape) {
#   real u = uniform_rng(0, 1);  // Generate a uniform random number
#   real result_real = ceil(pow(log1m(u) / log(mu), 1 / shape));  // Compute result as real
#   real final_res = result_real - 1;  // Cast to int and subtract 1 for 0-based indexing
#   return final_res;
# }
stan_likelihood <- "
  // Log-PMF function for total score using sub-components
  real nfl_scoring_lpmf(int y, real mu, real phi, real muTD, real shapeTD, real muFG, real shapeFG, real muSafety, real shapeSafety, real muXP, real mu2pt) {
  // Compute log probabilities for scoring components
  real log_prob = discrete_weibull_lpmf(y | muTD, shapeTD)  // Log probability for TDs
                + discrete_weibull_lpmf(y | muFG, shapeFG)  // Log probability for FGs
                + discrete_weibull_lpmf(y | muSafety, shapeSafety)  // Log probability for Safeties
                + beta_binomial_lpmf(y | round(muTD), muXP * muTD, (1 - muXP) * muTD)  // Beta-Binomial for extra points
                + beta_binomial_lpmf(y | round(muTD), mu2pt * muTD, (1 - mu2pt) * muTD);  // Beta-Binomial for 2PT conversions

  // Add Negative Binomial likelihood for total score
  log_prob += neg_binomial_2_lpmf(y | mu, phi);

  return log_prob;
  }
  int nfl_scoring_rng(real mu, real phi){
    return(neg_binomial_2_rng(mu, phi));
  }
"

# Register the custom family in brms
stanvars <- stanvar(scode = stan_likelihood, block = "functions")

# Define the formula for total scores
## Spread ----
formula_spread <- 
  bf(
    result ~ 
      0 + Intercept +
      #spread_line +
      home_SRS +
      away_SRS +
      #home_SRS:away_SRS +
      
      home_off_epa_roll + away_def_epa_roll +
      home_off_epa_roll:away_def_epa_roll + 
      away_off_epa_roll + home_def_epa_roll +
      away_off_epa_roll:home_def_epa_roll + 
      
      home_off_n + 
      home_off_td +
      away_def_td +
      home_off_n:home_off_td +
      home_off_td:away_def_td +
      
      away_off_n + 
      away_off_td +
      home_def_td +
      away_off_n:away_off_td +
      away_off_td:home_def_td +
      
      home_off_fg +
      home_off_n:home_off_fg +
      
      away_off_fg +
      away_off_n:away_off_fg +
      
      home_off_punt + away_def_punt +
      home_off_punt:away_def_punt +
      
      away_off_punt + home_def_punt +
      away_off_punt:home_def_punt +
      
      home_off_to + away_def_to +
      home_off_to:away_def_to +
      
      away_off_to + home_def_to +
      away_off_to:home_def_to +
      
      (1|home_team) +
      (1|away_team)
    
    #(1|mm(home_team, away_team, id = "H"))
    # (1|home_team) +
    # (1|away_team)
    # (1|H|home_team) +
    # (1|A|away_team)
  )

## Total ----
formula_total <- 
  bf(
    totalCover ~ 
      0 + Intercept +
      #total_line +
      home_OSRS_net +
      away_OSRS_net +
      home_OSRS_net:away_OSRS_net +
      
      home_off_epa_roll + away_def_epa_roll +
      home_off_epa_roll:away_def_epa_roll + 
      away_off_epa_roll + home_def_epa_roll +
      away_off_epa_roll:home_def_epa_roll + 
      
      home_off_n + 
      home_off_td +
      away_def_td +
      home_off_n:home_off_td +
      home_off_td:away_def_td +
      
      away_off_n + 
      away_off_td +
      home_def_td +
      away_off_n:away_off_td +
      away_off_td:home_def_td +
      
      home_off_fg +
      home_off_n:home_off_fg +
      
      away_off_fg +
      away_off_n:away_off_fg +
      
      home_off_punt + away_def_punt +
      home_off_punt:away_def_punt +
      
      away_off_punt + home_def_punt +
      away_off_punt:home_def_punt +
      
      home_off_to + away_def_to +
      home_off_to:away_def_to +
      
      away_off_to + home_def_to +
      away_off_to:home_def_to +
      
      (1|H|home_team) +
      (1|A|away_team)
    
    #(1|mm(home_team, away_team, id = "H"))
    # (1|home_team) +
    # (1|away_team)
    # (1|H|home_team) +
    # (1|A|away_team)
  ) + brmsfamily(family = "bernoulli")

### TD ----
#### Home ----
formula_homeTD <- 
  bf(
    home_totalTD ~ inv_logit(homeTD),
    homeTD ~ 0 + Intercept +
      s(home_OSRS_net, bs = "cr") +
      home_off_epa_roll + 
      away_def_epa_roll + 
      home_off_epa_roll:away_def_epa_roll + 
      home_off_td +
      home_def_n + 
      home_def_td,
    shape ~  0 + Intercept +
      (1|mm(home_team,away_team, id = "H")),
    nl = TRUE
  ) + brmsfamily(family = "discrete_weibull", link = "identity")

#### Away ----
formula_awayTD <- 
  bf(
    away_totalTD ~ inv_logit(awayTD),
    awayTD ~ 0 + Intercept +
      s(home_def_epa_roll, bs = "cr") + 
      away_off_td +
      away_off_n +
      away_off_n:away_off_td +
      away_def_n,
    shape ~  0 + Intercept +
      (1|mm(home_team,away_team, id = "H")),
    nl = TRUE
  ) + brmsfamily(family = "discrete_weibull", link = "identity")

### FG ----
#### Home ----
formula_homeFG <- 
  bf(
    home_fg_made ~ bFG*mi(homeFGatt),
    homeFGatt|mi() ~ 0 + Intercept +
      home_SRS_net + 
      home_off_n + 
      home_off_fg + 
      home_off_n:home_off_fg,
    bFG ~ 1,
    #location +
    #(1 + location|mm(home_team,away_team))
    #(1 | H | home_team) + (1 | A | away_team)
    #,
    # shape ~  0 + Intercept +
    #   (1|mm(home_team,away_team, id = "H")),
    #nl = TRUE
    #(1 | H | home_team) + (1 | A | away_team)
  ) + brmsfamily(family = "gaussian")#, link = "logit")

#### Away ----
formula_awayFG <- 
  bf(
    away_fg_made ~ 
      0 + Intercept +
      away_SRS_net + 
      away_off_n + 
      away_off_fg + 
      away_off_n:away_off_fg #+
    #location +
    #(1 + location|mm(home_team,away_team))
    #(1 | H | home_team) + (1 | A | away_team)
    ,
    shape ~  0 + Intercept +
      (1|mm(home_team,away_team, id = "H"))
    #(1 | H | home_team) + (1 | A | away_team)
  ) + brmsfamily(family = "discrete_weibull", link = "logit")


### SF ----
#### Home ----
formula_homeSF <- 
  bf(
    home_safeties|trials(2) ~ 
      home_def_epa_roll + away_off_epa_roll + 
      home_def_epa_roll:away_off_epa_roll +
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "binomial")

#### Away ----
formula_awaySF <- 
  bf(
    away_safeties|trials(2) ~ 
      away_def_epa_roll + home_off_epa_roll + 
      away_def_epa_roll:home_off_epa_roll +
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "binomial")

### XP ----
#### Home ----
formula_homeXP <- 
  bf(
    home_pat_made ~ #|trials(home_totalTD)
      home_off_pat_pct_roll + 
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")

#### Away ----
formula_awayXP <- 
  bf(
    away_pat_made ~ #|trials(away_totalTD)
      away_off_pat_pct_roll + 
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "discrete_weibull")

### TP ----
#### Home ----
formula_homeTP <- 
  bf(
    home_twoPtConv|trials(2) ~ #|trials(home_totalTD-home_pat_made)
      home_off_pass_plays_cum + 
      home_off_rush_plays_cum +
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "binomial")

#### Away ----
formula_awayTP <- 
  bf(
    away_twoPtConv|trials(2)  ~ #|trials(away_totalTD-away_pat_made)
      away_off_pass_plays_cum + 
      away_off_rush_plays_cum +
      (1|H|home_team) + 
      (1|away_team)
  ) + brmsfamily(family = "binomial")

### Scores ----
#### Separate ----
formula_homeScore <-
  bf(
    home_score ~ 
      0 + Intercept +
      (1|H|home_team) + 
      (1|A|away_team)
  ) + brmsfamily(family = "discrete_weibull")

formula_awayScore <-
  bf(
    away_score ~ 
      0 + Intercept +
      (1|H|home_team) + 
      (1|A|away_team)
  ) + brmsfamily(family = "discrete_weibull")


formula_homeScore <- 
  bf(
    home_score ~ 
      6*home_totalTD + 
      3*home_fg_made +
      2*home_safeties +
      home_pat_made + 
      2*home_twoPtConv,
    nl = TRUE
  )

formula_awayScore <- 
  bf(
    away_score ~ 
      6*away_totalTD + 
      3*away_fg_made +
      2*away_safeties +
      away_pat_made + 
      2*away_twoPtConv,
    nl = TRUE
  )

#### Combined ----
##### Home ----
formula_homeScore <- 
  bf(
    home_score ~ 1,
    #home_totalTD ~ inv_logit(homeTD),
    mu1/7 ~
      0 + Intercept +
      s(home_OSRS_net, bs = "cr") +
      home_off_epa_roll + 
      away_def_epa_roll + 
      home_off_epa_roll:away_def_epa_roll + 
      home_off_td +
      home_def_n + 
      home_def_td,
    shape1 ~  #0 + Intercept +
      (1|mm(home_team,away_team, id = "H")),
    mu2/3 ~ 
      0 + Intercept +
      home_SRS_net + 
      home_off_n + 
      home_off_fg + 
      home_off_n:home_off_fg #+
    #location +
    #(1 + location|mm(home_team,away_team))
    #(1 | H | home_team) + (1 | A | away_team)
    ,
    shape2 ~ # 0 + Intercept +
      (1|mm(home_team,away_team, id = "H"))
    #nl = TRUE
  ) #+ brmsfamily(family = "discrete_weibull")

formula_homeScore <- 
  bf(
    home_score ~ 1,
    #home_totalTD ~ inv_logit(homeTD),
    mu1 ~ 0 + Intercept +
      s(home_OSRS_net, bs = "cr") +
      home_off_epa_roll + 
      away_def_epa_roll + 
      home_off_epa_roll:away_def_epa_roll + 
      home_off_td +
      home_def_n + 
      home_def_td,
    # shape1 ~  0 + Intercept +
    #   (1|mm(home_team,away_team, id = "H")),
    mu2 ~ 
      0 + Intercept +
      home_SRS_net + 
      home_off_n + 
      home_off_fg + 
      home_off_n:home_off_fg #+
    #location +
    #(1 + location|mm(home_team,away_team))
    #(1 | H | home_team) + (1 | A | away_team)
    # ,
    # shape2 ~  0 + Intercept +
    #   (1|mm(home_team,away_team, id = "H"))
    #nl = TRUE
  ) #+ brmsfamily(family = "discrete_weibull")

##### Away ----
formula_awayScore <- 
  bf(
    away_score ~ 6*awayTD + 3*awayFG + 2*awaySF + awayXP + 2*awayTP,
    awayTD ~ 
      away_OSRS_net +
      away_DSRS_net +
      away_off_epa_roll + home_def_epa_roll +
      away_off_epa_roll:home_def_epa_roll + 
      away_off_n + away_off_TD +
      away_off_n:away_off_TD +
      away_def_n + away_def_TD +
      away_def_n:away_def_TD +
      (1|home_team) +
      (1|away_team),
    awayFG ~ 
      away_SRS_net +
      away_off_n + away_off_fg +
      away_off_n:away_off_fg +
      (1|home_team) + 
      (1|away_team),
    awaySF ~ 
      away_def_epa_roll + home_off_epa_roll + 
      away_def_epa_roll:home_off_epa_roll +
      (1|home_team) + 
      (1|away_team),
    awayXP|trials(awayTD) ~ 
      away_off_pat_pct_roll + 
      (1|home_team) + 
      (1|away_team),
    awayTP|trials(awayTD-awayXP) ~ 
      away_off_pass_plays_cum + 
      away_off_rush_plays_cum +
      (1|home_team) + 
      (1|away_team),
    nl = TRUE
  ) #+ brmsfamily(family = "discrete_weibull")

#### Combined ----
##### Home ----
formula_homeScore <- 
  bf(
    home_score ~ 
      0 + Intercept +
      s(home_OSRS_net) +
      #home_DSRS_net +
      home_off_epa_roll + away_def_epa_roll +
      home_off_epa_roll:away_def_epa_roll + 
      #home_off_n + 
      home_off_td +
      #home_off_n:home_off_td +
      home_def_n + home_def_td +
      home_SRS_net +
      home_off_n + home_off_fg +
      home_off_n:home_off_fg +
      (1|H|home_team) +
      (1|A|away_team)
  ) + brmsfamily(family = "discrete_weibull",
                 link = "identity")

##### Away ----
formula_awayScore <- 
  bf(
    away_score ~
      0 + Intercept +
      #away_OSRS_net +
      #away_DSRS_net +
      #away_off_epa_roll + 
      s(home_def_epa_roll) +
      #away_off_epa_roll:home_def_epa_roll + 
      #away_off_n + 
      away_off_td +
      away_off_n:away_off_td +
      away_def_n + #away_def_td +
      #away_def_n:away_def_td +
      away_SRS_net +
      away_off_n + away_off_fg +
      away_off_n:away_off_fg +
      (1|H|home_team) + 
      (1|A|away_team)
  ) + brmsfamily(family = "discrete_weibull",
                 link = "identity")

## Fit ----
# priorPoints <- c(
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "hometotalTD"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "homefgmade"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "awaytotalTD"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "awayfgmade")
# )

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "hometotalTD"),
  prior(normal(0,5), class = "b", resp = "homefgmade"),
  prior(normal(0,5), class = "b", resp = "homesafeties"),
  prior(normal(0,5), class = "b", resp = "homepatmade"),
  prior(normal(0,5), class = "b", resp = "hometwoPtConv"),
  prior(normal(0,5), class = "b", resp = "awaytotalTD"),
  prior(normal(0,5), class = "b", resp = "awayfgmade"),
  prior(normal(0,5), class = "b", resp = "awaysafeties"),
  prior(normal(0,5), class = "b", resp = "awaypatmade"),
  prior(normal(0,5), class = "b", resp = "awaytwoPtConv")
)

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "hometotalTD"),
#prior(normal(0,5), class = "b", resp = "homefgmade"),
# prior(normal(0,5), class = "b", resp = "homesafeties"),
# prior(normal(0,5), class = "b", resp = "homepatmade"),
# prior(normal(0,5), class = "b", resp = "hometwoPtConv"),
prior(normal(0,5), class = "b", resp = "awaytotalTD"),
#prior(normal(0,5), class = "b", resp = "awayfgmade")
# prior(normal(0,5), class = "b", resp = "awaysafeties"),
# prior(normal(0,5), class = "b", resp = "awaypatmade")
# prior(normal(0,5), class = "b", resp = "awaytwoPtConv")
)

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "spreadCover"),
  prior(normal(0,5), class = "b", resp = "totalCover")
)

# Fit the model using the custom family for total scores
# model_nfl_code <- stancode(
#   formula_home + formula_away,
#   data = histModelData,
#   family = custom_family,
#   save_pars = save_pars(all = TRUE),
#   seed = 52,
#   chains = chains, 
#   cores = 4, 
#   iter = iters,
#   warmup = burn,
#   stanvars = stanvars,
#   #prior = priorPoints,
#   drop_unused_levels = FALSE,
#   control = list(adapt_delta = 0.95),
#   backend = "cmdstan"
# )
# model_nfl_code

mix <- mixture(
  gaussian, 
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  gaussian,
  order = "none"
)

system.time(
  model_nfl_fit <- brm(
    #formula_spread, #+ formula_total +
    #formula_homeTD + formula_awayTD +
    #formula_homeFG +# formula_awayFG +
    # formula_homeSF + formula_awaySF +
    # formula_homeXP + formula_awayXP +
    # formula_homeTP + formula_awayTP +
    formula_homeScore,# + formula_awayScore +
    #set_rescor(rescor = FALSE),
    data = histModelData,
    family = mixture(brmsfamily(family = "discrete_weibull", link = "logit"), 
                     brmsfamily(family = "discrete_weibull", link = "logit"),
                     order = "none"),
    save_pars = save_pars(all = TRUE),
    seed = 52,
    chains = chains, 
    cores = parallel::detectCores(),
    iter = iters,
    warmup = burn,
    #init = 0,
    #stanvars = stanvars,
    #prior = priorPoints,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
  )
)

Fit <- model_nfl_fit
fit <- 61
assign(paste0("fit", fit), Fit)
#assign(paste0("fitB", fit), Fit2)
save(fit10, file= paste0("~/Desktop/fit", fit, ".RData"))

plot(Fit, ask = FALSE)

Fit <- fit8
#fitFormulas <- list()
# for(i in 1:fit){
#   fitFormulas[[paste0("Fit",i)]] <- get(paste0("fit", i))
# }
#fitFormulas[[paste0("Fit",fit)]] <- get(paste0("fit", fit))

## Diagnostics ----
prior_summary(Fit)
# posterior_summary(Fit)
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
print(fixedEff, digits = 4)
fixedSigEff <- fixedEff |> filter(p_val < 0.2)
fixedSigEff <- fixedSigEff |> 
  rownames_to_column() |>
  mutate(
    response = str_split_i(rowname, "_", i = 1),
    param = str_remove(rowname, paste0(response,"_"))
  ) |> 
  relocate(c(response, param), .after = "rowname") |>
  select(-rowname)
fixedSigEff

assign(paste0("fixedEff", fit), fixedEff)
assign(paste0("fixedSigEff", fit), fixedSigEff)

fixedSigEffForms <- fixedSigEff |>
  select(response, param) |>
  group_by(response) |>
  summarise(
    formula = paste(param, collapse = " + ")
  )
fixedSigEffForms
fixedSigEffForms$formula[7]
fixedSigEffForms$formula[5]
fixedSigEffForms$formula[6]
fixedSigEffForms$formula[8]
fixedSigEffForms$formula[3]
fixedSigEffForms$formula[1]
fixedSigEffForms$formula[2]
fixedSigEffForms$formula[4]
# 
# fixedSigEff1
# print(fixedSigEff, digits = 4)
# save(fixedSigEff, file = paste0("./Model Fitting/Data/fixedSigEff", fit, ".RData"))
# 
# fixedSigEff13141516 <- bind_rows(
#   fixedSigEff13 |> 
#     mutate(Fit = "fit13", .before = 1) |> 
#     mutate(response = factor(response, levels = unique(response))), 
#   fixedSigEff2 |> 
#     mutate(Fit = "fit14", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff15 |> 
#     mutate(Fit = "fit15", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff16 |> 
#     mutate(Fit = "fit16", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response))),
#   fixedSigEff17 |> 
#     mutate(Fit = "fit17", .before = 1) |>
#     mutate(response = factor(response, levels = unique(response)))
#   ) |>
#   group_by(response, Fit) |>
#   arrange(p_val, .by_group = TRUE) |>
#   ungroup()
# 
# fixedSigEff1314 |> 
#   filter(response == "hometotalTD") |>
#   pull(param) |> unique()

randEff <- ranef(Fit, summary = TRUE)
print(randEff, digits = 4)
VarCorr(Fit)

#plot(Fit, ask = FALSE)

postSum <- posterior_summary(Fit)
#postSum[grepl("^sd_", rownames(postSum)), ]

### Bayes R2 -----
FitR2temp <- bayes_R2(Fit)
FitR2temp
FitR2tempDF <- FitR2temp |>
  bind_cols(
    Fit = paste0("Fit", fit)
    #Response = c("total", "homescore", "awayscore")
  ) |>
  select(Fit, everything())
FitR2tempDF

FitR2 <- bind_rows(
  FitR2tempDF,
  FitR2
)
FitR2 #<- FitR2tempDF

FitR2tempPred <- bayes_R2(Fit, newdata = modelData) |>
  bind_cols(Fit = paste0("Pred", fit)) |>
  select(Fit, everything())
FitR2tempPred

FitR2Pred <- bind_rows(
  FitR2tempPred,
  FitR2Pred
)
FitR2Pred <- FitR2tempPred

# logNormalFitsmooths <- conditional_smooths(logNormalFit,
#                                            method = "posterior_predict")
# plot(logNormalFitsmooths, 
#      stype = "raster", 
#      ask = FALSE,
#      theme = theme(legend.position = "bottom"))
# plot(logNormalFitsmooths, 
#      stype = "contour", 
#      ask = FALSE,
#      theme = theme(legend.position = "bottom"))

# condplots ----
Fitsmooth <- conditional_smooths(Fit, method = "posterior_predict")
Fitsmooth <- conditional_smooths(Fit, method = "posterior_epred")
plot(Fitsmooth,
     stype = "contour",
     ask = FALSE)

Fiteffects <- conditional_effects(Fit, 
                                  effects = c(
                                    "away_off_epa_roll",
                                    "home_def_epa_roll",
                                    "home_off_punt",
                                    "away_def_punt",
                                    "away_off_n",
                                    "away_off_fg",
                                    "away_def_punt",
                                    "away_off_punt",
                                    "away_off_to",
                                    "home_off_td:away_def_td",
                                    "away_off_td:home_def_td"
                                  ),
                                  method = "posterior_predict", 
                                  re_formula = NULL,
                                  robust = FALSE)
plot(Fiteffects, 
     points = TRUE, 
     ask = FALSE)

performance_score(Fit)
performance::check_distribution(Fit)
performance::check_zeroinflation(Fit,)
### Projpred ----
# fitVarselHomeTD <- varsel(Fit, resp = "hometotalTD")
# fitVarselHomeFG <- varsel(Fit, resp = "homefgmade")
# fitVarselAwayTD <- varsel(Fit, resp = "awaytotalTD")
# fitVarselAwayFG <- varsel(Fit, resp = "awayfgmade")

# loo7 <- loo(fit7)
# 
# loo_compare(
#   #loo7, 
#   loo8,
#   loo9,
#   loo10,
#   loo11,
#   loo12,
#   loo20
# )

## PPC Plot ----
# homePPCTD <- pp_check(Fit, resp = "hometotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC TD")) +
#   theme_bw()
# homePPCfg <- pp_check(Fit, resp = "homefgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC fg")) +
#   theme_bw()
# homePPCxp <- pp_check(Fit2, resp = "homepatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC xp")) +
#   theme_bw()
# homePPCtp <- pp_check(Fit2, resp = "hometwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC tp")) +
#   theme_bw()
# homePPCsafe <- pp_check(Fit, resp = "homesafeties", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC safe")) +
#   theme_bw()

homePPCbarsTD <- pp_check(Fit, resp = "hometotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
homePPCbarsTDnl <- pp_check(Fit, resp = "homeTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
homePPCbarsFG <- pp_check(Fit, resp = "homefgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC fg")) +
  theme_bw()
homePPCbarsSF <- pp_check(Fit, resp = "homesafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC safe")) +
  theme_bw()
homePPCbarsXP <- pp_check(Fit, resp = "homepatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC xp")) +
  theme_bw()
homePPCbarsTP <- pp_check(Fit, resp = "hometwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC tp")) +
  theme_bw()

homeTDnl <- posterior_linpred(Fit, nlpar = "homeTD")

# awayPPCTD <- pp_check(Fit, resp = "awaytotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC TD")) +
#   theme_bw()
# awayPPCfg <- pp_check(Fit, resp = "awayfgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC fg")) +
#   theme_bw()
# awayPPCxp <- pp_check(Fit2, resp = "awaypatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC xp")) +
#   theme_bw()
# awayPPCtp <- pp_check(Fit2, resp = "awaytwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC tp")) +
#   theme_bw()
# awayPPCsafe <- pp_check(Fit, resp = "awaysafeties", ndraws = 100) + 
#   labs(title = paste0("Fit", fit, " Away PPC safe")) +
#   theme_bw()

awayPPCbarsTD <- pp_check(Fit, resp = "awaytotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC TD")) +
  theme_bw()
awayPPCbarsFG <- pp_check(Fit, resp = "awayfgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC fg")) +
  theme_bw()
awayPPCbarsSF <- pp_check(Fit, resp = "awaysafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC safe")) +
  theme_bw()
awayPPCbarsXP <- pp_check(Fit, resp = "awaypatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC xp")) +
  theme_bw()
awayPPCbarsTP <- pp_check(Fit, resp = "awaytwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC tp")) +
  theme_bw()


# homePPCTD
# homePPCfg
# homePPCxp
# homePPCtp
# homePPCsafe
homePPCbarsTD
homePPCbarsFG
homePPCbarsSF
homePPCbarsXP
homePPCbarsTP

# awayPPCTD
# awayPPCfg
# awayPPCxp
# awayPPCtp
# awayPPCsafe
awayPPCbarsTD
awayPPCbarsFG
awayPPCbarsSF
awayPPCbarsXP
awayPPCbarsTP

## Fitted
homefinalFitTD <- posterior_predict(Fit, resp = "hometotalTD")
homefinalFitFG <- posterior_predict(Fit, resp = "homefgmade")
homefinalFitXP <- posterior_predict(Fit, resp = "homepatmade")
homefinalFitTP <- posterior_predict(Fit, resp = "hometwoPtConv")
homefinalFitSF <- posterior_predict(Fit, resp = "homesafeties")

## Preds
homefinalPredsTD <- posterior_predict(Fit,
                                      resp = "hometotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsFG <- posterior_predict(Fit,
                                      resp = "homefgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsXP <- posterior_predict(Fit,
                                      resp = "homepatmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsTP <- posterior_predict(Fit,
                                      resp = "hometwoPtConv",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsSF <- posterior_predict(Fit,
                                      resp = "homesafeties",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Fitted
awayfinalFitTD <- posterior_predict(Fit, resp = "awaytotalTD")
awayfinalFitFG <- posterior_predict(Fit, resp = "awayfgmade")
awayfinalFitXP <- posterior_predict(Fit, resp = "awaypatmade")
awayfinalFitTP <- posterior_predict(Fit, resp = "awaytwoPtConv")
awayfinalFitSF <- posterior_predict(Fit, resp = "awaysafeties")

## Preds
awayfinalPredsTD <- posterior_predict(Fit,
                                      resp = "awaytotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsFG <- posterior_predict(Fit,
                                      resp = "awayfgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsXP <- posterior_predict(Fit,
                                      resp = "awaypatmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsTP <- posterior_predict(Fit,
                                      resp = "awaytwoPtConv",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsSF <- posterior_predict(Fit,
                                      resp = "awaysafeties",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
sims <- 3000
homePPDbarsTD <- ppc_bars(y = modelData$home_totalTD, 
                          yrep = homefinalPredsTD[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  theme_bw()
homePPDbarsFG <- ppc_bars(y = modelData$home_fg_made, 
                          yrep = homefinalPredsFG[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD fg")) +
  theme_bw()
homePPDbarsXP <- ppc_bars(y = modelData$home_pat_made,
                          yrep = homefinalPredsXP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD xp")) +
  theme_bw()
homePPDbarsTP <- ppc_bars(y = modelData$home_twoPtConv,
                          yrep = homefinalPredsTP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()
homePPDbarsSF <- ppc_bars(y = modelData$home_safeties,
                          yrep = homefinalPredsSF[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()


awayPPDbarsTD <- ppc_bars(y = modelData$away_totalTD, 
                          yrep = awayfinalPredsTD[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD TD")) +
  theme_bw()
awayPPDbarsFG <- ppc_bars(y = modelData$away_fg_made, 
                          yrep = awayfinalPredsFG[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD fg")) +
  theme_bw()
awayPPDbarsXP <- ppc_bars(y = modelData$away_pat_made,
                          yrep = awayfinalPredsXP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Away PPD xp")) +
  theme_bw()
awayPPDbarsTP <- ppc_bars(y = modelData$away_twoPtConv,
                          yrep = awayfinalPredsTP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Away PPD tp")) +
  theme_bw()
awayPPDbarsSF <- ppc_bars(y = modelData$away_safeties,
                          yrep = awayfinalPredsSF[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()

homePPDbarsTD
homePPDbarsFG
homePPDbarsXP
homePPDbarsTP
homePPDbarsSF

awayPPDbarsTD
awayPPDbarsFG
awayPPDbarsXP
awayPPDbarsTP
awayPPDbarsSF

#### Home Score ----
homefinalFit <- 
  6*homefinalFitTD + 
  3*homefinalFitFG +
  1*homefinalFitXP +
  2*homefinalFitTP +
  2*homefinalFitSF

homefinalFit <- 
  6*homefinalFitTD + 
  3*homefinalFitFG +
  1*homefinalFitXP

homefinalFit <- 
  7*homefinalFitTD + 
  3*homefinalFitFG

## Fitted
homefinalFit <- posterior_predict(Fit, resp = "homescore")
homefinalFitMean <- colMeans(homefinalFit)
homefinalFitMed <- apply(homefinalFit, 2, function(x){quantile(x, 0.5)})
homefinalFitLCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.025)})
homefinalFitUCB <- apply(homefinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
homefinalPreds <- 
  6*homefinalPredsTD + 
  3*homefinalPredsFG +
  1*homefinalPredsXP +
  2*homefinalPredsTP +
  2*homefinalPredsSF

homefinalPreds <- 
  6*homefinalPredsTD + 
  3*homefinalPredsFG +
  1*homefinalPredsXP

homefinalPreds <- 
  7*homefinalPredsTD + 
  3*homefinalPredsFG

homefinalPreds <- posterior_predict(Fit,
                                    resp = "homescore",
                                    newdata = modelData,
                                    allow_new_levels = TRUE,
                                    re_formula = NULL
)
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


homePPCbars <- ppc_bars(y = histModelData$home_score, 
                        yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  xlim(0,100) +
  theme_bw()

homePPCdens <- ppc_dens_overlay(y = histModelData$home_score, 
                                yrep = homefinalFit[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  xlim(0,100) +
  theme_bw()
homePPCdens

homePPDbars <- ppc_bars(y = modelData$home_score, 
                          yrep = homefinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  theme_bw()

homePPDdens <- ppc_dens_overlay(y = modelData$home_score, 
                        yrep = homefinalPreds[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  xlim(0,100) +
  theme_bw()
homePPDdens

#### Away Score ----
awayfinalFit <- 
  6*awayfinalFitTD + 
  3*awayfinalFitFG +
  1*awayfinalFitXP +
  2*awayfinalFitTP +
  2*awayfinalFitSF

awayfinalFit <- 
  6*awayfinalFitTD + 
  3*awayfinalFitFG +
  1*awayfinalFitXP

awayfinalFit <- 
  7*awayfinalFitTD + 
  3*awayfinalFitFG

## Fitted
#awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <- 
  6*awayfinalPredsTD + 
  3*awayfinalPredsFG +
  1*awayfinalPredsXP +
  2*awayfinalPredsTP +
  2*awayfinalPredsSF

awayfinalPreds <- 
  6*awayfinalPredsTD + 
  3*awayfinalPredsFG +
  1*awayfinalPredsXP

awayfinalPreds <- 
  7*awayfinalPredsTD + 
  3*awayfinalPredsFG

# awayfinalPreds <- posterior_predict(Fit,
#                                     resp = "awayscore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
awayfinalPredsMean <- colMeans(awayfinalPreds)
awayfinalPredsMed <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalPredsLCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalPredsUCB <- apply(awayfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("home", "away"),
  MAE_fit = c(mean(abs(homefinalFitMean - histModelData$home_score)),
              mean(abs(awayfinalFitMean - histModelData$away_score))),
  COV_fit = c(mean(homefinalFitLCB < histModelData$home_score &  histModelData$home_score < homefinalFitUCB),
              mean(awayfinalFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalFitUCB)),
  MAE_pred = c(mean(abs(homefinalPredsMean - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalPredsMean - modelData$away_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(homefinalPredsMed - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalPredsMed - modelData$away_score), na.rm = TRUE)),
  COV_pred = c(mean(homefinalPredsLCB < modelData$home_score & modelData$home_score < homefinalPredsUCB, na.rm = TRUE),
               mean(awayfinalPredsLCB < modelData$away_score & modelData$away_score < awayfinalPredsUCB, na.rm = TRUE))
  # home_MAE_fit = mean(abs(homefinalFitMean - histModelData$home_score)),
  # home_COV_fit = mean(homefinalFitLCB < histModelData$home_score &  histModelData$home_score < homefinalFitUCB),
  # away_MAE_fit = mean(abs(awayfinalFitMean - histModelData$away_score)),
  # away_COV_fit = mean(awayfinalFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalFitUCB),
  # home_MAE_pred = mean(abs(homefinalPredsMean - modelData$home_score), na.rm = TRUE),
  # home_MAD_pred = mean(abs(homefinalPredsMed - modelData$home_score), na.rm = TRUE),
  # home_COV_pred = mean(homefinalPredsLCB < modelData$home_score & modelData$home_score < homefinalPredsUCB),
  # away_MAE_pred = mean(abs(awayfinalPredsMean - modelData$away_score), na.rm = TRUE),
  # away_MAD_pred = mean(abs(awayfinalPredsMed - modelData$away_score), na.rm = TRUE),
  # away_COV_pred = mean(awayfinalPredsLCB < modelData$away_score & modelData$away_score < awayfinalPredsUCB, na.rm = TRUE)
)
predMetricsHA


## Spread ----
#FittedSpread <- homefinalFit - awayfinalFit
FittedSpread <- posterior_predict(Fit, 
                                  resp = "result",
                                  newdata = histModelData,
                                  re_formula = NULL)
FittedMeanSpread <- colMeans(FittedSpread)
FittedMedSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.5)})
FittedLCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.025)})
FittedUCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.975)})

# Prediction
#PredsSpread <- homefinalPreds - awayfinalPreds
PredsSpread <- posterior_predict(Fit,
                                 resp = "result",
                                 newdata = modelData,
                                 allow_new_levels = TRUE,
                                 re_formula = NULL
)
PredsMeanSpread <- colMeans(PredsSpread)
PredsMedSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

FittedSpreadData <- histModelData |>
  #filter(!is.na(spreadCover)) |>
  select(
    season, week, home_team, away_team, result,
    home_spread_prob, away_spread_prob, spreadCover
  ) |>
  mutate(
    FittedSpreadProb = FittedMeanSpread,
    FittedSpreadBet = ifelse(FittedSpreadProb > home_spread_prob, TRUE, FALSE),
    FittedSpreadCorrect = spreadCover == FittedSpreadBet
  )
mean(FittedSpreadData$FittedSpreadCorrect, na.rm = TRUE)

PredsSpreadData <- modelData |>
  #filter(!is.na(spreadCover)) |>
  select(
    season, week, home_team, away_team, result,
    home_spread_prob, away_spread_prob, spreadCover
  ) |>
  mutate(
    PredsSpreadProb = PredsMeanSpread,
    PredsSpreadBet = ifelse(PredsSpreadProb > home_spread_prob, TRUE, FALSE),
    PredsSpreadCorrect = spreadCover == PredsSpreadBet
  )
mean(PredsSpreadData$PredsSpreadCorrect, na.rm = TRUE)


## Total ----
#FittedTotal <- homefinalFit - awayfinalFit
FittedTotal <- posterior_predict(Fit, 
                                 resp = "totalCover",
                                 newdata = histModelData,
                                 re_formula = NULL)
FittedMeanTotal <- colMeans(FittedTotal)
FittedMedTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.5)})
FittedLCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.025)})
FittedUCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.975)})

# Prediction
#PredsTotal <- homefinalPreds - awayfinalPreds
PredsTotal <- posterior_predict(Fit,
                                resp = "totalCover",
                                newdata = modelData,
                                allow_new_levels = TRUE,
                                re_formula = NULL
)
PredsMeanTotal <- colMeans(PredsTotal)
PredsMedTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

FittedTotalData <- histModelData |>
  #filter(!is.na(totalCover)) |>
  select(
    season, week, home_team, away_team, total,
    over_prob, under_prob, totalCover
  ) |>
  mutate(
    FittedTotalProb = FittedMeanTotal,
    FittedTotalBet = ifelse(FittedTotalProb > over_prob, TRUE, FALSE),
    FittedTotalCorrect = totalCover == FittedTotalBet
  )
mean(FittedTotalData$FittedTotalCorrect, na.rm = TRUE)

PredsTotalData <- modelData |>
  #filter(!is.na(totalCover)) |>
  select(
    season, week, home_team, away_team, total,
    over_prob, under_prob, totalCover
  ) |>
  mutate(
    PredsTotalProb = PredsMeanTotal,
    PredsTotalBet = ifelse(PredsTotalProb > over_prob, TRUE, FALSE),
    PredsTotalCorrect = totalCover == PredsTotalBet,
    PredsTotalBet2 = ifelse(PredsTotalProb > .5, TRUE, FALSE),
    PredsTotalCorrect2 = totalCover == PredsTotalBet2
  )
mean(PredsTotalData$PredsTotalCorrect, na.rm = TRUE)
mean(PredsTotalData$PredsTotalCorrect2, na.rm = TRUE)

successPerf <- data.frame(
  Fit = fit,
  SpreadTrain = round(mean(FittedSpreadData$FittedSpreadCorrect, na.rm = TRUE), 3),
  SpreadTest = round(mean(PredsSpreadData$PredsSpreadCorrect, na.rm = TRUE), 3),
  TotalTrain = round(mean(FittedTotalData$FittedTotalCorrect, na.rm = TRUE), 3),
  TotalTest = round(mean(PredsTotalData$PredsTotalCorrect, na.rm = TRUE), 3)
)
successPerf

spreadTrain <- histModelData$result
spreadTest <- modelData$result
predMetricsSpreadTemp <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Spread", 2),
  MAE_fit = mean(abs(FittedMeanSpread - spreadTrain)),
  MAD_fit = mean(abs(FittedMedSpread - spreadTrain)),
  COV_fit = mean(FittedLCBSpread < spreadTrain & spreadTrain < FittedUCBSpread),
  MAE_pred = mean(abs(PredsMeanSpread - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedSpread - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBSpread < spreadTest & spreadTest < PredsUCBSpread)
)
predMetricsSpreadTemp

predMetricsSpread <- bind_rows(
  predMetricsSpreadTemp,
  predMetricsSpread
)
predMetricsSpread #<- predMetricsSpreadTemp

##### Plot ----
set.seed(52)
spreadPPC <- ppc_dens_overlay(y = histModelData |>
                                filter(!is.na(result)) |>
                                pull(result) |>
                                as.numeric(), 
                              yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
spreadPPC

spreadPPCbars <- ppc_bars(y = histModelData$result, 
                          yrep = round(FittedSpread[sample(1:sims, 100, replace = FALSE), ])) + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
spreadPPCbars

set.seed(52)
spreadPPD <- ppc_dens_overlay(y = modelData$result, 
                              yrep = PredsSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
spreadPPD

spreadPPDbars <- ppc_bars(y = modelData$result, 
                          yrep = PredsSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
spreadPPDbars



##### Prob Errors ----
##### Fit ----
spreadLineTrain <- modData |>
  filter(season == 2023 | (season == 2024 & week <= 6)) |>
  pull(spread_line)

#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- FittedSpread[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedProbsSpread[, j] <- probs
}
FittedBetSpread <- colMeans(FittedProbsSpread)
FittedBetLogicalSpread <- FittedBetSpread > 0.5
FittedLogicalSpread <- spreadTrain > spreadLineTrain
FittedProbSpread <- mean(FittedBetLogicalSpread == FittedLogicalSpread, na.rm = TRUE)
FittedProbSpread

spreadDataTrain <- modData |> filter(season == 2023 | (season == 2024 & week <= 6)) |>
  select(season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTrain$spreadCoverSuccess))
sum(!is.na(spreadDataTrain$spreadCoverSuccess))

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

##### Pred ----
spreadLineTest <- modData |>
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  pull(spread_line)
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- PredsSpread[, j]
  probs <- fitted > spreadLineTest[j]
  PredsProbsSpread[, j] <- probs
}
PredsBetSpread <- colMeans(PredsProbsSpread)
PredsBetLogicalSpread <- PredsBetSpread > 0.5
PredsLogicalSpread <- spreadTest > spreadLineTest
PredsProbSpread <- mean(PredsBetLogicalSpread == PredsLogicalSpread, na.rm = TRUE)
PredsProbSpread

spreadDataTest <- modData |> filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE, 
    #                         ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTest$spreadCoverSuccess))
sum(!is.na(spreadDataTest$spreadCoverSuccess))

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Total ----
FittedTotal <- homefinalFit + awayfinalFit
#Fitted <- posterior_predict(Fit)
FittedMeanTotal <- colMeans(FittedTotal)
FittedMedTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.5)})
FittedLCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.025)})
FittedUCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsTotal <- homefinalPreds + awayfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMeanTotal <- colMeans(PredsTotal)
PredsMedTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBTotal <- apply(PredsTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

totalTrain <- modData |>
  filter(season == 2023 | (season == 2024 & week <= 6)) |>
  pull(total)
totalTest <- modData |>
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  pull(total)
predMetricsTotalTemp <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Total", 2),
  MAE_fit = mean(abs(FittedMeanTotal - totalTrain)),
  MAD_fit = mean(abs(FittedMedTotal - totalTrain)),
  COV_fit = mean(FittedLCBTotal < totalTrain & totalTrain < FittedUCBTotal),
  MAE_pred = mean(abs(PredsMeanTotal - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsMedTotal - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsLCBTotal < totalTest & totalTest < PredsUCBTotal)
)
predMetricsTotalTemp

predMetricsTotal <- bind_rows(
  predMetricsTotalTemp,
  predMetricsTotal
)
predMetricsTotal #<- predMetricsTotalTemp

##### Plot ----
set.seed(52)
totalPPC <- ppc_dens_overlay(y = totalTrain,
                             yrep = FittedTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPC

totalPPCbars <- ppc_bars(y =totalTrain, 
                         yrep = FittedTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPCbars

set.seed(52)
totalPPD <- ppc_dens_overlay(y = totalTest, 
                             yrep = PredsTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPD

totalPPDbars <- ppc_bars(y = totalTest, 
                         yrep = PredsTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPDbars

##### Prob Errors ----
##### Fit ----
totalLineTrain <- modData |>
  filter(season == 2023 | (season == 2024 & week <= 6)) |>
  pull(total_line)
#totalTrain <- as.numeric(totalTrainScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

FittedProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- FittedTotal[, j]
  probs <- fitted > totalLineTrain[j]
  FittedProbsTotal[, j] <- probs
}
FittedBetTotal <- colMeans(FittedProbsTotal)
FittedBetLogicalTotal <- FittedBetTotal > 0.5
FittedLogicalTotal <- totalTrain > totalLineTrain
FittedProbTotal <- mean(FittedBetLogicalTotal == FittedLogicalTotal, na.rm = TRUE)
FittedProbTotal

totalDataTrain <- modData |> filter(season == 2023 | (season == 2024 & week <= 6))|>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = FittedMeanTotal,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = FittedBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

##### Pred ----
totalLineTest <- modData |>
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  ) |>
  pull(total_line)
#totalTest <- as.numeric(totalTestScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

PredsProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- PredsTotal[, j]
  probs <- fitted > totalLineTest[j]
  PredsProbsTotal[, j] <- probs
}
PredsBetTotal <- colMeans(PredsProbsTotal)
PredsBetLogicalTotal <- PredsBetTotal > 0.5
PredsLogicalTotal <- totalTest > totalLineTest
PredsProbTotal <- mean(PredsBetLogicalTotal == PredsLogicalTotal, na.rm = TRUE)
PredsProbTotal

totalDataTest <- modData |> filter(season == 2024 & week > 6) |> 
  filter(!is.na(result), 
         !is.na(home_totalTD),
         !is.na(away_totalTD),
         !is.na(home_fg_made),
         !is.na(away_fg_made)
  )|>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = PredsMeanTotal,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = PredsBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

successPerfTemp <- bind_rows( 
  bind_cols(
    Response = "Spread",
    spreadSuccessTrain,
    spreadSuccessTest
  ) |> rename_with(~str_remove(.x, "spread"), .cols = contains("spread")),
  bind_cols(
    Response = "Total",
    totalSuccessTrain,
    totalSuccessTest
  ) |> rename_with(~str_remove(.x, "total"), .cols = contains("total"))
)|>
  mutate(
    Fit = paste0("Fit ", fit), .before = 1
  )
successPerfTemp
successPerf <- bind_rows(
  successPerf,
  successPerfTemp
)
successPerf #<- successPerfTemp
tail(successPerf, 10)

modelWeights <- model_weights(fit1, fit2, fit3, fit5, weights = "stacking")
round(modelWeights, digits = 9)


## Errors ----
ppc_error_scatter_avg(histModelData$result, FittedSpread)
ppc_error_scatter_avg(modelData$result, PredsSpread)
ppc_error_scatter_avg(histModelData$total, FittedTotal)
ppc_error_scatter_avg(modelData$total, PredsTotal)

ppc_error_scatter_avg_vs_x(histModelData$result, FittedSpread, histModelData$home_SRS_net)
ppc_error_scatter_avg_vs_x(modelData$result, PredsSpread, modelData$home_SRS_net)
ppc_error_scatter_avg_vs_x(histModelData$total, FittedTotal, histModelData$home_SRS_net)
ppc_error_scatter_avg_vs_x(modelData$total, PredsTotal, modelData$home_SRS_net)

ppc_error_scatter_avg_vs_x(histModelData$result, FittedSpread, histModelData$week)
ppc_error_scatter_avg_vs_x(modelData$result, PredsSpread, modelData$week)
ppc_error_scatter_avg_vs_x(histModelData$total, FittedTotal, histModelData$week)
ppc_error_scatter_avg_vs_x(modelData$total, PredsTotal, modelData$week)

ppc_error_scatter_avg_grouped(histModelData$result, FittedSpread, 
                              histModelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData$result, PredsSpread, 
                              modelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(histModelData$total, FittedTotal, histModelData$home_team)
ppc_error_scatter_avg_grouped(modelData$total, PredsTotal, modelData$home_team)

ppc_error_scatter_avg_grouped(histModelData$result, FittedSpread, histModelData$away_team)
ppc_error_scatter_avg_grouped(modelData$result, PredsSpread, modelData$away_team)
ppc_error_scatter_avg_grouped(histModelData$total, FittedTotal, histModelData$away_team)
ppc_error_scatter_avg_grouped(modelData$total, PredsTotal, modelData$away_team)


## Predict Week ----
testWeekData <- modData2 |>
  filter(season == 2024) |>
  filter(is.na(result) & !is.na(spread_line))

newdata = data.frame(x = 1, y1 = NA)
fy1 <- fitted(fit, newdata = newdata, resp = "y1", summary = FALSE)
newdata2 <- expand.grid(x = newdata$x, y1 = as.vector(fy1))
fy2 <- fitted(fit, newdata = newdata2, resp = "y2", summary = FALSE)
fy2 <- as.matrix(diag(fy2))
colMeans(fy2)
posterior_interval(fy2)

testWeekData2 <- predict(preProcValues, testWeekData)

## Preds
homefinalPredsTD <- posterior_predict(fit2,
                                      resp = "hometotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsfg <- posterior_predict(Fit,
                                      resp = "homefgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredsxp <- posterior_predict(Fit,
                                      resp = "homepatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
homefinalPredstp <- posterior_predict(Fit,
                                      resp = "hometwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Preds
awayfinalPredsTD <- posterior_predict(Fit,
                                      resp = "awaytotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsfg <- posterior_predict(Fit,
                                      resp = "awayfgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredsxp <- posterior_predict(Fit,
                                      resp = "awaypatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
awayfinalPredstp <- posterior_predict(Fit,
                                      resp = "awaytwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Prediction on new data
homefinalPreds <-
  6*homefinalPredsTD +
  3*homefinalPredsfg +
  1*homefinalPredsxp +
  2*homefinalPredstp #+
#2*homefinalPredssafe

# homefinalPreds <- posterior_predict(Fit,
#                                     resp = "homescore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
homefinalPredsMean <- colMeans(homefinalPreds)
homefinalPredsMed <- apply(homefinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalPredsLCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalPredsUCB <- apply(homefinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
awayfinalFit <- 
  6*awayfinalFitTD + 
  3*awayfinalFitfg +
  1*awayfinalFitxp +
  2*awayfinalFittp #+
#2*awayfinalFitsafe
## Fitted
#awayfinalFit <- posterior_predict(Fit, resp = "awayscore")
awayfinalFitMean <- colMeans(awayfinalFit)
awayfinalFitMed <- apply(awayfinalFit, 2, function(x){quantile(x, 0.5)})
awayfinalFitLCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.025)})
awayfinalFitUCB <- apply(awayfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
awayfinalPreds <-
  6*awayfinalPredsTD +
  3*awayfinalPredsfg +
  1*awayfinalPredsxp +
  2*awayfinalPredstp #+

spreadNew <- 
  
  
  # Iterate -----
## Update Priors ----
# Helper function to create updated priors
prior_summary(fit20, all = FALSE)
create_updated_priors <- function(post_summary) {
  priors <- empty_prior()
  
  # Fixed effects (coefficients)
  fixed_effects <- grep("^b_", rownames(post_summary), value = TRUE)
  for (param in fixed_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    coef_name <- sub("b_(hometotalTD|homefgmade|homepatmade|hometwoPtConv|awaytotalTD|awayfgmade|awaypatmade|awaytwoPtConv)_", "", param)
    #response <- ifelse(grepl("_homescore_", param), "homescore", "awayscore")
    response <- str_split_i(param, "_", i = 2)
    #priorTemp <- prior(normal(estimate, est_error), class = "b", coef = coef_name, resp = response)
    priorTemp <- do.call("prior",
                         list(prior = call("normal", estimate, est_error),
                              class = "b", 
                              coef = coef_name,
                              resp = response))
    priors <- c(priors, priorTemp)
  }
  
  # Correlations
  correlations <- grep("^cor_", rownames(post_summary), value = TRUE)
  for (param in correlations) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    group_name <- sub("cor_(home_team|away_team)__(.*)", "\\1", param)
    priorTemp <- do.call("prior",
                         list(prior = call("lkj_corr_cholesky", 1),
                              class = "L", 
                              group = group_name))
    priors <- c(priors, priorTemp)
    #priors <- c(priors, prior(lkj_corr_cholesky(1), class = "cor"))  # Retain LKJ prior but acknowledge posterior
  }
  
  # Random effects (standard deviations)
  random_effects <- grep("^sd_", rownames(post_summary), value = TRUE)
  for (param in random_effects) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    group_name <- sub("sd_(home_team|away_team)__(.*)", "\\1", param)
    #response <- ifelse(grepl("awayscore", param), "awayscore", "homescore")
    response <- str_split_i(param, "_", i = 5)
    priorTemp <- do.call("prior",
                         list(prior = call("student_t", 3, estimate, est_error),
                              class = "sd", 
                              coef = "Intercept",
                              group = group_name,
                              resp = response#,
                              #lb = 0
                         )
    )
    priors <- c(priors, priorTemp)
    #priors <- c(priors, prior(student_t(3, estimate, est_error), class = "sd", group = group_name, resp = response))
  }
  
  # Dispersion (shape parameter for negative binomial)
  shapes <- grep("^shape_", rownames(post_summary), value = TRUE)
  for (param in shapes) {
    mean <- post_summary[param, "Estimate"]
    variance <- post_summary[param, "Est.Error"]^2
    response <- sub("shape_", "", param)  # Extract response (e.g., homescore or awayscore)
    alpha <- (mean^2 / variance)
    beta <- variance / mean
    priorTemp <- do.call("prior",
                         list(prior = call("gamma", alpha, beta),
                              class = "shape",
                              resp = response,
                              lb = 0))
    priors <- c(priors, priorTemp)
  }
  
  stand_devs <- grep("^sigma_", rownames(post_summary), value = TRUE)
  for (param in stand_devs) {
    estimate <- post_summary[param, "Estimate"]
    est_error <- post_summary[param, "Est.Error"]
    response <- ifelse(grepl("awayscore", param), "awayscore", "homescore")
    priorTemp <- do.call("prior",
                         list(prior = call("student_t", 3, estimate, est_error),
                              class = "sigma", 
                              resp = response,
                              lb = 0
                         )
    )
    priors <- c(priors, priorTemp)
  }
  
  return(priors)
}

# Create updated priors
updated_priors <- c(create_updated_priors(post_summary = postSum))
updated_priors

## Fit Model ----
# Initialize values
predWeeks <- max(modelData$week)
iterFit <- fit9
homefinalIterFitComb <- NULL
awayfinalIterFitComb <- NULL
homefinalIterPredsComb <- NULL
awayfinalIterPredsComb <- NULL
prePriors <- posterior_summary(fit11)
prePriorCoefs <- prior_summary(fit11)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

### Run loop
for(i in 1:predWeeks){
  predictWeekData <- modelData |>
    filter(week == i)
  
  # nonUnique <- predictWeekData |>
  #   select(where(is.character))
  # 
  # nonUnique <- predictWeekData |>
  #   summarise(
  #     across(where(is.character),
  #            ~length(unique(.x)))
  #   ) |> unlist()
  # removeVars <- names(nonUnique[nonUnique == 1])
  
  # Make Prediction for week 
  homefinalIterPreds <- posterior_predict(iterFit,
                                          resp = "homescore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  awayfinalIterPreds <- posterior_predict(iterFit,
                                          resp = "awayscore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  # Update prediction matrix
  homefinalIterPredsComb <- cbind(homefinalIterPredsComb, homefinalIterPreds)
  awayfinalIterPredsComb <- cbind(awayfinalIterPredsComb, awayfinalIterPreds)
  
  iterData <- modelData |>
    filter(week == i)
  
  iterCoefs <- default_prior(
    formulaFitHome + formulaFitAway + set_rescor(FALSE),
    data = iterData,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    warmup = burn,
    iter = iters,
    chains = chains,
    #prior = updated_priors2,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan",
    drop_unused_levels = TRUE
  )$coef
  
  iterPrePrior <- posterior_summary(iterFit)
  iterPrePriorCoefs <- prior_summary(iterFit)$coef
  
  dropCoefs <- iterPrePriorCoefs[!(iterPrePriorCoefs %in% iterCoefs)]
  newCoefs <- iterCoefs[!(iterCoefs %in% iterPrePriorCoefs)]
  
  updated_priors <- create_updated_priors(post_summary = iterPrePrior)
  updated_priors_keep <- updated_priors |> filter(!(coef %in% dropCoefs))
  updated_priors_new <- old_priors |> filter(coef %in% newCoefs)
  updated_priors2 <- c(updated_priors_keep, updated_priors_new)
  
  # Fit new model
  iterFit <- brm(
    formulaFitHome + formulaFitAway + set_rescor(FALSE),
    data = iterData,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    warmup = burn,
    iter = iters,
    chains = chains,
    prior = updated_priors2,
    normalize = TRUE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan"
  )
  
  # Get fitted values for week
  homefinalIterFit <- posterior_predict(iterFit, 
                                        resp = "homescore")
  awayfinalIterFit <- posterior_predict(iterFit, 
                                        resp = "awayscore")
  
  # Update prediction matrix
  homefinalIterFitComb <- cbind(homefinalIterFitComb, homefinalIterFit)
  awayfinalIterFitComb <- cbind(awayfinalIterFitComb, awayfinalIterFit)
  
  print(paste0("Finished Week ", i))
}




## Fit Model 2 ----
# Initialize values
predWeeks <- max(modelData$week)
iterFitBase <- fit18
#iterFitBaseB <- fitB8
iterFit <- iterFitBase
#iterFitB <- iterFitBaseB
homefinalIterFitTDComb2 <- list()
homefinalIterFitfgComb2 <- list()
# homefinalIterFitxpComb2 <- list()
# homefinalIterFittpComb2 <- list()
homefinalIterFitComb2 <- list()

awayfinalIterFitTDComb2 <- list()
awayfinalIterFitfgComb2 <- list()
# awayfinalIterFitxpComb2 <- list()
# awayfinalIterFittpComb2 <- list()
awayfinalIterFitComb2 <- list()

homefinalIterPredsTDComb2 <- list()
homefinalIterPredsfgComb2 <- list()
# homefinalIterPredsxpComb2 <- list()
# homefinalIterPredstpComb2 <- list()
homefinalIterPredsComb2 <- list()

awayfinalIterPredsTDComb2 <- list()
awayfinalIterPredsfgComb2 <- list()
# awayfinalIterPredsxpComb2 <- list()
# awayfinalIterPredstpComb2 <- list()
awayfinalIterPredsComb2 <- list()

prePriors <- posterior_summary(iterFitBase)
prePriorCoefs <- prior_summary(iterFitBase)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

trainingData <- modData2 |> filter(season %in% 2023:2023)
predictorIterData <- trainingData |> 
  select(-home_score, -away_score,
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
         -total_line,
         -location,
         -div_game,
         -roof,
         -temp,
         -wind)
preProcIterValues <- preProcess(predictorIterData,
                                method = c("center", "scale"))

# prePriorsB <- posterior_summary(iterFitBaseB)
# prePriorCoefsB <- prior_summary(iterFitBaseB)$coef
# old_priorsB <- create_updated_priors(post_summary = prePriorsB)

### Run loop  ----
# took 29 minutes
system.time(
  for(i in 1:predWeeks){
    
    predictWeekData <- modData2 |> 
      filter(season == 2024, week == i) |>
      filter(!is.na(result))
    
    predictWeekData <- predict(preProcIterValues, predictWeekData)
    
    
    # Make Prediction for week 
    homefinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "hometotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    awayfinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "awaytotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    homefinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "homefgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    awayfinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "awayfgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    # homefinalIterPredsTDMed <- apply(homefinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # awayfinalIterPredsTDMed <- apply(awayfinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # predictWeekData <- predictWeekData |>
    #   mutate(
    #     home_totalTD = homefinalIterPredsTDMed,
    #     away_totalTD = awayfinalIterPredsTDMed
    #   )
    # 
    # homefinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "homepatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # homefinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "hometwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # awayfinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "awaypatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # awayfinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "awaytwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    
    homefinalIterPreds <- 
      7*homefinalIterPredsTD + 
      3*homefinalIterPredsfg #+
    # 1*homefinalIterPredsxp +
    # 2*homefinalIterPredstp
    
    awayfinalIterPreds <- 
      7*awayfinalIterPredsTD + 
      3*awayfinalIterPredsfg #+
    # 1*awayfinalIterPredsxp +
    # 2*awayfinalIterPredstp
    
    homefinalIterPredsComb2[[i]] <- homefinalIterPreds
    awayfinalIterPredsComb2[[i]] <- awayfinalIterPreds
    
    # Update prediction matrix
    homefinalIterPredsTDComb2[[i]] <- homefinalIterPredsTD
    homefinalIterPredsfgComb2[[i]] <- homefinalIterPredsfg
    # homefinalIterPredsxpComb2[[i]] <- homefinalIterPredsxp
    # homefinalIterPredstpComb2[[i]] <- homefinalIterPredstp
    awayfinalIterPredsTDComb2[[i]] <- awayfinalIterPredsTD
    awayfinalIterPredsfgComb2[[i]] <- awayfinalIterPredsfg
    # awayfinalIterPredsxpComb2[[i]] <- awayfinalIterPredsxp
    # awayfinalIterPredstpComb2[[i]] <- awayfinalIterPredstp
    
    # iterData <- modelData |>
    #   filter(week %in% i:i)
    # 
    # new_priors <- create_updated_priors(post_summary = posterior_summary(iterFit))
    trainingData <- bind_rows(
      modData2 |> filter(season %in% 2023:2023),
      modData2 |> filter(season == 2024, week <= i) 
    )
    predictorIterData <- trainingData |> 
      select(-home_score, -away_score,
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
             -total_line,
             -location,
             -div_game,
             -roof,
             -temp,
             -wind)
    preProcIterValues <- preProcess(predictorIterData,
                                    method = c("center", "scale"))
    trainingData <- predict(preProcIterValues, trainingData)
    trainingWeekData <- trainingData |> filter(season == 2024, week == i)
    
    
    iterFit <- update(iterFit,
                      newdata = trainingData,
                      drop_unused_levels = FALSE,
                      cores = parallel::detectCores()
    )
    
    #new_priors <- create_updated_priors(post_summary = posterior_summary(iterFit))
    
    # iterFitB <- update(iterFitBaseB,
    #                   newdata = iterData,
    #                   prior = old_priorsB,
    #                   drop_unused_levels = FALSE
    # )
    
    # iterCoefs <- default_prior(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   #prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan",
    #   drop_unused_levels = TRUE
    # )$coef
    # 
    # iterPrePrior <- posterior_summary(iterFit)
    # iterPrePriorCoefs <- prior_summary(iterFit)$coef
    # 
    # dropCoefs <- iterPrePriorCoefs[!(iterPrePriorCoefs %in% iterCoefs)]
    # newCoefs <- iterCoefs[!(iterCoefs %in% iterPrePriorCoefs)]
    # 
    # updated_priors <- create_updated_priors(post_summary = iterPrePrior)
    # updated_priors_keep <- updated_priors |> filter(!(coef %in% dropCoefs))
    # updated_priors_new <- old_priors |> filter(coef %in% newCoefs)
    # updated_priors2 <- c(updated_priors_keep, updated_priors_new)
    # 
    # # Fit new model
    # iterFit <- brm(
    #   formulaFitHome + formulaFitAway + set_rescor(FALSE),
    #   data = iterData,
    #   save_pars = save_pars(all = TRUE),
    #   seed = 52,
    #   warmup = burn,
    #   iter = iters,
    #   chains = chains,
    #   prior = updated_priors2,
    #   normalize = TRUE,
    #   control = list(adapt_delta = 0.95),
    #   backend = "cmdstan"
    # )
    
    # Get fitted values for week
    homefinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "hometotalTD")
    homefinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "homefgmade")
    # homefinalIterFitxp <- posterior_predict(iterFitB, resp = "homepatmade")
    # homefinalIterFittp <- posterior_predict(iterFitB, resp = "hometwoPtConv")
    
    
    awayfinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "awaytotalTD")
    awayfinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "awayfgmade")
    # awayfinalIterFitxp <- posterior_predict(iterFitB, resp = "awaypatmade")
    # awayfinalIterFittp <- posterior_predict(iterFitB, resp = "awaytwoPtConv")
    
    homefinalIterFit <- 
      7*homefinalIterFitTD + 
      3*homefinalIterFitfg #+
    # 1*homefinalIterFitxp +
    # 2*homefinalIterFittp
    
    awayfinalIterFit <- 
      7*awayfinalIterFitTD + 
      3*awayfinalIterFitfg #+
    # 1*awayfinalIterFitxp +
    # 2*awayfinalIterFittp
    
    homefinalIterFitComb2[[i]] <- homefinalIterFit
    awayfinalIterFitComb2[[i]] <- awayfinalIterFit
    
    # Update prediction matrix
    homefinalIterFitTDComb2[[i]] <- homefinalIterFitTD
    homefinalIterFitfgComb2[[i]] <- homefinalIterFitfg
    # homefinalIterFitxpComb2[[i]] <- homefinalIterFitxp
    # homefinalIterFittpComb2[[i]] <- homefinalIterFittp
    awayfinalIterFitTDComb2[[i]] <- awayfinalIterFitTD
    awayfinalIterFitfgComb2[[i]] <- awayfinalIterFitfg
    # awayfinalIterFitxpComb2[[i]] <- awayfinalIterFitxp
    # awayfinalIterFittpComb2[[i]] <- awayfinalIterFittp
    
    print(paste0("Finished Week ", i))
  }
)

iterFit2 <- iterFit

save(iterFit1,
     homefinalIterFitTDComb2,
     homefinalIterFitfgComb2,
     homefinalIterFitComb2,
     
     awayfinalIterFitTDComb2,
     awayfinalIterFitfgComb2,
     awayfinalIterFitComb2,
     
     homefinalIterPredsTDComb2,
     homefinalIterPredsfgComb2,
     homefinalIterPredsComb2,
     
     awayfinalIterPredsTDComb2,
     awayfinalIterPredsfgComb2,
     awayfinalIterPredsComb2,
     file = "~/Desktop/NFL Analysis Data/iter data Multi3.RData")

## Diagnostics ----




## Fitted
dim(homefinalIterFitComb2[[1]])

homefinalIterPredsTD2 <- do.call(cbind, homefinalIterPredsTDComb2)
homefinalIterPredsfg2 <- do.call(cbind, homefinalIterPredsfgComb2)
# homefinalIterPredsxp2 <- do.call(cbind, homefinalIterPredsxpComb2)
# homefinalIterPredstp2 <- do.call(cbind, homefinalIterPredstpComb2)
awayfinalIterPredsTD2 <- do.call(cbind, awayfinalIterPredsTDComb2)
awayfinalIterPredsfg2 <- do.call(cbind, awayfinalIterPredsfgComb2)
# awayfinalIterPredsxp2 <- do.call(cbind, awayfinalIterPredsxpComb2)
# awayfinalIterPredstp2 <- do.call(cbind, awayfinalIterPredstpComb2)

homefinalIterPreds <- 
  7*homefinalIterPredsTD2 +
  3*homefinalIterPredsfg2 #+
# 1*homefinalIterPredsxp2 +
#   2*homefinalIterPredstp2

awayfinalIterPreds <- 
  7*awayfinalIterPredsTD2 +
  3*awayfinalIterPredsfg2 #+
# 1*awayfinalIterPredsxp2 +
#   2*awayfinalIterPredstp2

## PPD Plots
homeIterPPDbarsTD <- ppc_bars(y = modelData$home_totalTD, 
                              yrep = homefinalIterPredsTD2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD TD")) +
  theme_bw()
homeIterPPDbarsfg <- ppc_bars(y = modelData$home_fg_made, 
                              yrep = homefinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD fg")) +
  theme_bw()
# homeIterPPDbarsxp <- ppc_bars(y = modelData$home_pat_made, 
#                               yrep = homefinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home IterPPD xp")) +
#   theme_bw()
# homeIterPPDbarstp <- ppc_bars(y = modelData$home_twoPtConv, 
#                               yrep = homefinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home IterPPD tp")) +
#   theme_bw()

awayIterPPDbarsTD <- ppc_bars(y = modelData$away_totalTD, 
                              yrep = awayfinalIterPredsTD2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD TD")) +
  theme_bw()
awayIterPPDbarsfg <- ppc_bars(y = modelData$away_fg_made, 
                              yrep = awayfinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD fg")) +
  theme_bw()
# awayIterPPDbarsxp <- ppc_bars(y = modelData$away_pat_made, 
#                               yrep = awayfinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away IterPPD xp")) +
#   theme_bw()
# awayIterPPDbarstp <- ppc_bars(y = modelData$away_twoPtConv, 
#                               yrep = awayfinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away IterPPD tp")) +
#   theme_bw()

homeIterPPDbarsTD
homeIterPPDbarsfg
# homeIterPPDbarsxp
# homeIterPPDbarstp

awayIterPPDbarsTD
awayIterPPDbarsfg
# awayIterPPDbarsxp
# awayIterPPDbarstp



### Home Score
homefinalIterFit <- do.call(cbind, homefinalIterFitComb2)
homefinalIterFitMean <- colMeans(homefinalIterFit)
homefinalIterFitMed <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalIterFitLCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalIterFitUCB <- apply(homefinalIterFit, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

## Prediction on new data
homefinalIterPreds <- do.call(cbind, homefinalIterPredsComb2)
homefinalIterPredsMean <- colMeans(homefinalIterPreds)
homefinalIterPredsMed <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
homefinalIterPredsLCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
homefinalIterPredsUCB <- apply(homefinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score
## Fitted
awayfinalIterFit <- do.call(cbind, awayfinalIterFitComb2)
awayfinalIterFitMean <- colMeans(awayfinalIterFit)
awayfinalIterFitMed <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalIterFitLCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalIterFitUCB <- apply(awayfinalIterFit, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

## Prediction on new data
awayfinalIterPreds <- do.call(cbind, awayfinalIterPredsComb2)
awayfinalIterPredsMean <- colMeans(awayfinalIterPreds)
awayfinalIterPredsMed <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
awayfinalIterPredsLCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
awayfinalIterPredsUCB <- apply(awayfinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predIterMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("home", "away"),
  # MAE_fit = c(mean(abs(homefinalIterFitMean - modelData$home_score)),
  #             mean(abs(awayfinalIterFitMean - modelData$away_score))),
  # COV_fit = c(mean(homefinalIterFitLCB < modelData$home_score &  modelData$home_score < homefinalIterFitUCB),
  #             mean(awayfinalIterFitLCB < modelData$away_score &  modelData$away_score < awayfinalIterFitUCB)),
  MAE_pred = c(mean(abs(homefinalIterPredsMean - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalIterPredsMean - modelData$away_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(homefinalIterPredsMed - modelData$home_score), na.rm = TRUE),
               mean(abs(awayfinalIterPredsMed - modelData$away_score), na.rm = TRUE)),
  COV_pred = c(mean(homefinalIterPredsLCB < modelData$home_score & modelData$home_score < homefinalIterPredsUCB, na.rm = TRUE),
               mean(awayfinalIterPredsLCB < modelData$away_score & modelData$away_score < awayfinalIterPredsUCB, na.rm = TRUE))
  # home_MAE_fit = mean(abs(homefinalIterFitMean - histModelData$home_score)),
  # home_COV_fit = mean(homefinalIterFitLCB < histModelData$home_score &  histModelData$home_score < homefinalIterFitUCB),
  # away_MAE_fit = mean(abs(awayfinalIterFitMean - histModelData$away_score)),
  # away_COV_fit = mean(awayfinalIterFitLCB < histModelData$away_score &  histModelData$away_score < awayfinalIterFitUCB),
  # home_MAE_pred = mean(abs(homefinalIterPredsMean - modelData$home_score), na.rm = TRUE),
  # home_MAD_pred = mean(abs(homefinalIterPredsMed - modelData$home_score), na.rm = TRUE),
  # home_COV_pred = mean(homefinalIterPredsLCB < modelData$home_score & modelData$home_score < homefinalIterPredsUCB),
  # away_MAE_pred = mean(abs(awayfinalIterPredsMean - modelData$away_score), na.rm = TRUE),
  # away_MAD_pred = mean(abs(awayfinalIterPredsMed - modelData$away_score), na.rm = TRUE),
  # away_COV_pred = mean(awayfinalIterPredsLCB < modelData$away_score & modelData$away_score < awayfinalIterPredsUCB, na.rm = TRUE)
)
predIterMetricsHA


#### Spread ----
FittedIterSpread <- homefinalIterFit - awayfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanSpread <- colMeans(FittedIterSpread)
FittedIterMedSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
FittedIterLCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
FittedIterUCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

# Prediction
PredsIterSpread <- homefinalIterPreds - awayfinalIterPreds
# PredsIter <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsIterMeanSpread <- colMeans(PredsIterSpread)
PredsIterMedSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsIterLCBSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsIterUCBSpread <- apply(PredsIterSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

spreadTrain <- modelData1$result
spreadTest <- modelData1$result
predIterMetricsSpread <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Spread", 2),
  # MAE_vegas = mean(abs(modelData1$result - modelData1$spread_line)),
  # MAE_fit = mean(abs(FittedIterMeanSpread - spreadTrain)),
  # MAD_fit = mean(abs(FittedIterMedSpread - spreadTrain)),
  # COV_fit = mean(FittedIterLCBSpread < spreadTrain & spreadTrain < FittedIterUCBSpread),
  MAE_pred = mean(abs(PredsIterMeanSpread - spreadTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsIterMedSpread - spreadTest), na.rm = TRUE),
  COV_pred = mean(PredsIterLCBSpread < spreadTest & spreadTest < PredsIterUCBSpread)
)
predIterMetricsSpread

##### Plot ----
set.seed(52)
spreadPPC <- ppc_dens_overlay(y = modelData$result, 
                              yrep = FittedIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPC

set.seed(52)
spreadPPD <- ppc_dens_overlay(y = modelData$result, 
                              yrep = PredsIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPD

spreadPPDbars <- ppc_bars(y = modelData$result, 
                          yrep = PredsIterSpread[sample(1:sims, 100, replace = FALSE), ])
spreadPPDbars

##### Prob Errors ----
##### Fit ----
spreadLineTrain <- modelData$spread_line
#spreadTrain <- as.numeric(spreadTrainScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

FittedIterProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTrain))
for(j in 1:length(spreadLineTrain)){
  fitted <- FittedIterSpread[, j]
  probs <- fitted > spreadLineTrain[j]
  FittedIterProbsSpread[, j] <- probs
}
FittedIterBetSpread <- colMeans(FittedIterProbsSpread)
FittedIterBetLogicalSpread <- FittedIterBetSpread > 0.5
FittedIterLogicalSpread <- spreadTrain > spreadLineTrain
FittedIterProbSpread <- mean(FittedIterBetLogicalSpread == FittedIterLogicalSpread, na.rm = TRUE)
FittedIterProbSpread

spreadDataTrain <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line, spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedIterMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    # spreadCoverBet = ifelse(spreadCoverProb > .6, TRUE, ifelse(1 - spreadCoverProb > .6, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTrain$spreadCoverSuccess))
sum(!is.na(spreadDataTrain$spreadCoverSuccess))

spreadSuccessTrain <- spreadDataTrain |>
  summarise(
    spreadProbTrain = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTrain = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTrain

##### Pred ----
spreadLineTest <- modelData1$spread_line
#spreadTest <- as.numeric(spreadTestScale*attr(spreadTrainScale, "scaled:scale") + attr(spreadTrainScale, "scaled:center"))

PredsIterProbsSpread <- matrix(NA, nrow = sims, ncol = length(spreadLineTest))
for(j in 1:length(spreadLineTest)){
  fitted <- PredsIterSpread[, j]
  probs <- fitted > spreadLineTest[j]
  PredsIterProbsSpread[, j] <- probs
}
PredsIterBetSpread <- colMeans(PredsIterProbsSpread)
PredsIterBetLogicalSpread <- PredsIterBetSpread > 0.5
PredsIterLogicalSpread <- spreadTest > spreadLineTest
PredsIterProbSpread <- mean(PredsIterBetLogicalSpread == PredsIterLogicalSpread, na.rm = TRUE)
PredsIterProbSpread

spreadDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, spread_line,spreadCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob
         #over_odds, over_prob,
         #under_odds, under_prob
  ) |>
  mutate(
    spreadPred = PredsIterMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > home_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > away_spread_prob, FALSE, NA)),
    spreadCoverBet = ifelse(spreadCoverProb > .7, TRUE,
                            ifelse(1 - spreadCoverProb > .7, FALSE, NA)),
    spreadCoverSuccess = spreadCoverBet == spreadCover
  )
sum(is.na(spreadDataTest$spreadCoverSuccess))
sum(!is.na(spreadDataTest$spreadCoverSuccess))

spreadSuccessTest <- spreadDataTest |>
  summarise(
    spreadProbTest = mean(coverSuccess, na.rm = TRUE),
    spreadOddsProbTest = mean(spreadCoverSuccess, na.rm = TRUE)
  )
spreadSuccessTest

#### Total ----
FittedIterTotal <- homefinalIterFit + awayfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanTotal <- colMeans(FittedIterTotal)
FittedIterMedTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
FittedIterLCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
FittedIterUCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

# Prediction
PredsIterTotal <- homefinalIterPreds + awayfinalIterPreds
# PredsIter <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsIterMeanTotal <- colMeans(PredsIterTotal)
PredsIterMedTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsIterLCBTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsIterUCBTotal <- apply(PredsIterTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

totalTrain <- modelData1$total
totalTest <- modelData1$total
predIterMetricsTotal <- tibble(
  Fit = paste0("Fit", fit),
  Response = rep("Total", 2),
  MAE_vegas = mean(abs(modelData1$total_line - totalTrain)),
  MAE_fit = mean(abs(FittedIterMeanTotal - totalTrain)),
  MAD_fit = mean(abs(FittedIterMedTotal - totalTrain)),
  COV_fit = mean(FittedIterLCBTotal < totalTrain & totalTrain < FittedIterUCBTotal),
  MAE_pred = mean(abs(PredsIterMeanTotal - totalTest), na.rm = TRUE),
  MAD_pred = mean(abs(PredsIterMedTotal - totalTest), na.rm = TRUE),
  COV_pred = mean(PredsIterLCBTotal < totalTest & totalTest < PredsIterUCBTotal)
)
predIterMetricsTotal

##### Plot ----
set.seed(52)
totalPPC <- ppc_dens_overlay(y = modelData1$total, 
                             yrep = FittedIterTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPC

set.seed(52)
totalPPD <- ppc_dens_overlay(y = modelData$total, 
                             yrep = PredsIterTotal[sample(1:sims, 100, replace = FALSE), ])
totalPPD

##### Prob Errors ----
##### Fit ----
totalLineTrain <- modelData$total_line
#totalTrain <- as.numeric(totalTrainScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

FittedIterProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTrain))
for(j in 1:length(totalLineTrain)){
  fitted <- FittedIterTotal[, j]
  probs <- fitted > totalLineTrain[j]
  FittedIterProbsTotal[, j] <- probs
}
FittedIterBetTotal <- colMeans(FittedIterProbsTotal)
FittedIterBetLogicalTotal <- FittedIterBetTotal > 0.5
FittedIterLogicalTotal <- totalTrain > totalLineTrain
FittedIterProbTotal <- mean(FittedIterBetLogicalTotal == FittedIterLogicalTotal, na.rm = TRUE)
FittedIterProbTotal

totalDataTrain <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalFit = FittedIterMeanTotal,
    coverBet = ifelse(totalFit > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = FittedIterBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTrain <- totalDataTrain |>
  summarise(
    totalProbTrain = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTrain = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTrain

##### Pred ----
totalLineTest <- modelData1$total_line
#totalTest <- as.numeric(totalTestScale*attr(totalTrainScale, "scaled:scale") + attr(totalTrainScale, "scaled:center"))

PredsIterProbsTotal <- matrix(NA, nrow = sims, ncol = length(totalLineTest))
for(j in 1:length(totalLineTest)){
  fitted <- PredsIterTotal[, j]
  probs <- fitted > totalLineTest[j]
  PredsIterProbsTotal[, j] <- probs
}
PredsIterBetTotal <- colMeans(PredsIterProbsTotal)
PredsIterBetLogicalTotal <- PredsIterBetTotal > 0.5
PredsIterLogicalTotal <- totalTest > totalLineTest
PredsIterProbTotal <- mean(PredsIterBetLogicalTotal == PredsIterLogicalTotal, na.rm = TRUE)
PredsIterProbTotal

totalDataTest <- modData |> filter(season == 2024) |> filter(!is.na(result)) |>
  select(game_id, season, week, #game_type,
         home_team, home_score, away_team, away_score,
         result, total_line, totalCover,
         home_spread_odds, home_spread_prob,
         away_spread_prob, away_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    totalPred = PredsIterMeanTotal,
    coverBet = ifelse(totalPred > total_line, TRUE, FALSE),
    coverSuccess = coverBet == totalCover,
    totalCoverProb = PredsIterBetTotal,
    totalCoverBet = ifelse(totalCoverProb > over_prob, TRUE, 
                           ifelse(1 - totalCoverProb > under_prob, FALSE, NA)),
    totalCoverSuccess = totalCoverBet == totalCover
  )

totalSuccessTest <- totalDataTest |>
  summarise(
    totalProbTest = mean(coverSuccess, na.rm = TRUE),
    totalOddsProbTest = mean(totalCoverSuccess, na.rm = TRUE)
  )
totalSuccessTest

successPerfTemp <- bind_rows( 
  bind_cols(
    Response = "Spread",
    spreadSuccessTrain,
    spreadSuccessTest
  ) |> rename_with(~str_remove(.x, "spread"), .cols = contains("spread")),
  bind_cols(
    Response = "Total",
    totalSuccessTrain,
    totalSuccessTest
  ) |> rename_with(~str_remove(.x, "total"), .cols = contains("total"))
)|>
  mutate(
    Fit = paste0("Fit ", fit), .before = 1
  )
successPerfTemp
successPerf <- bind_rows(
  successPerf,
  successPerfTemp
)

ppc_error_scatter_avg(modelData1$result, FittedIterSpread)
ppc_error_scatter_avg(modelData1$result, PredsIterSpread)
ppc_error_scatter_avg(modelData1$total, FittedIterTotal)
ppc_error_scatter_avg(modelData1$total, PredsIterTotal)

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$home_SRS)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$home_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$home_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$home_OSRS)

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$week)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, 
                              modelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, 
                              modelData$home_team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$home_team)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$home_team)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$away_team)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$away_team)





