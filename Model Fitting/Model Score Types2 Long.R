## Model team and opponent score

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
class(modData$team_totalTD)
class(modData$opponent_totalTD)
class(modData$team_fg_made)
class(modData$team_totalTD)
class(modData$team_totalTD)
class(modData$team_totalTD)
class(modData$team_totalTD)
class(modData$team_totalTD)
class(modData$team_totalTD)
class(modData$team_totalTD)

modDataLong <- modData |>
  clean_homeaway(invert = c("result", "spread_line"))

# Previous Data ----
modData2 <- modDataLong |> 
  filter(!is.na(result)) |>
  select(
    season,
    season_type,
    week,
    team,
    team_score,
    opponent,
    opponent_score,
    result,
    total,
    spread_line,
    total_line,
    location,
    div_game,
    roof,
    surface,
    temp,
    wind,
    contains("team"),
    contains("opponent")
  ) |>
  mutate(
    across(where(is.character),
           ~factor(.x))
  ) #|>

histModelData1 <- modData2 |> 
  filter(season == 2023 | (season == 2024 & week <= 6))
modelData1 <- modData2 |> 
  filter(season == 2024 & week > 6) |>
  filter(!is.na(result), 
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
  )

predictorData <- histModelData1 |> 
  select(-team_score, -opponent_score,
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

team_totalTD_range <- range(modData |> filter(!is.na(team_totalTD)) |> pull(team_totalTD))
team_fg_att_range<- range(modData |> filter(!is.na(team_fg_att)) |> pull(team_fg_att))
team_fg_made_range <- range(modData |> filter(!is.na(team_fg_made)) |> pull(team_fg_made))

opponent_totalTD_range <- range(modData |> filter(!is.na(opponent_totalTD)) |> pull(opponent_totalTD))
opponent_fg_att_range <- range(modData |> filter(!is.na(opponent_fg_att)) |> pull(opponent_fg_att))
opponent_fg_made_range <- range(modData |> filter(!is.na(opponent_fg_made)) |> pull(opponent_fg_made))

range_totalTD <- c(min(team_totalTD_range,opponent_totalTD_range), 
                   max(team_totalTD_range,opponent_totalTD_range))
range_fg_att_range <- c(min(team_fg_att_range,opponent_fg_att_range), 
                        max(team_fg_att_range,opponent_fg_att_range))
range_fg_made_range <- c(min(team_fg_made_range,opponent_fg_made_range), 
                         max(team_fg_made_range,opponent_fg_made_range))


## Correlations ----
teamTDcor <- cor(histModelData |> select(team_totalTD),
                 histModelData |> select(c(where(is.numeric), -team_totalTD)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
teamTDcorT <- t(teamTDcor)
teamTDcorT2 <- teamTDcorT[order(abs(teamTDcorT)),]
teamTDcorT2df <- data.frame(sort(abs(teamTDcorT2), decreasing = TRUE))

teamFGcor <- cor(histModelData |> select(team_fg_made),
                 histModelData |> select(c(where(is.numeric), -team_fg_made)),
                 use = "pairwise.complete.obs",
                 method = "kendall"
)
teamFGcorT <- t(teamFGcor)
teamFGcorT2 <- teamFGcorT[order(abs(teamFGcorT)),]
teamFGcorT2df <- data.frame(sort(abs(teamFGcorT2), decreasing = TRUE))

teamFGAcor <- cor(histModelData |> select(team_fg_att),
                  histModelData |> select(c(where(is.numeric), -team_fg_att)),
                  use = "pairwise.complete.obs",
                  method = "kendall"
)
teamFGAcorT <- t(teamFGAcor)
teamFGAcorT2 <- teamFGAcorT[order(abs(teamFGAcorT)),]
teamFGAcorT2df <- data.frame(sort(abs(teamFGAcorT2), decreasing = TRUE))



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
### TD ----
#### Home ----
formula_teamTD <- 
  bf(
    team_totalTD ~ 
      0 + Intercept +
      #s(team_OSRS_net, bs = "cr") +
      team_OSRS_net +
      team_PFG +
      opponent_PAG +
      #team_DSRS_net +
      team_off_epa_roll + opponent_def_epa_roll +
      team_off_epa_roll:opponent_def_epa_roll + 
      team_off_n + 
      team_off_td +
      team_off_n:team_off_td +
      team_def_n + team_def_td +
      team_def_n:team_def_td +
      location +
      #(1|mm(team, opponent, id = "H"))
      # (1|team) +
      # (1|opponent)
      (0 + location|team) +
      #(1|opponent) +
  ) + brmsfamily(family = "discrete_weibull") #, link = "logit")

#### Away ----
formula_opponentTD <- 
  bf(
    opponent_totalTD ~ 
      0 + Intercept +
      #opponent_OSRS_net +
      #opponent_DSRS_net +
      #opponent_off_epa_roll + 
      s(team_def_epa_roll, bs = "cr") +
      #opponent_off_epa_roll:team_def_epa_roll + 
      #opponent_off_n + 
      opponent_off_td +
      opponent_off_n:opponent_off_td +
      opponent_def_n + #opponent_def_td +
      #opponent_def_n:opponent_def_td +
      #(1|mm(team, opponent, id = "H"))
      # (1|team) +
      # (1|opponent)
      (1|H|team) +
      (1|A|opponent)
  ) + brmsfamily(family = "com_poisson") #, link = "logit")

### FG ----
#### Home ----
formula_teamFG <- 
  bf(
    team_fg_made ~ 
      0 + Intercept +
      team_SRS_net +
      team_off_n + team_off_fg +
      team_off_n:team_off_fg +
      #(1|mm(team, opponent, id = "H"))
      # (1|team) +
      # (1|opponent)
      (1|H|team) +
      (1|A|opponent)
  ) + brmsfamily(family = "com_poisson") #, link = "logit")

#### Away ----
formula_opponentFG <- 
  bf(
    opponent_fg_made ~ 
      0 + Intercept +
      opponent_SRS_net +
      opponent_off_n + opponent_off_fg +
      opponent_off_n:opponent_off_fg +
      #(1|mm(team, opponent, id = "H"))
      # (1|team) +
      # (1|opponent)
      (1|H|team) +
      (1|A|opponent)
  ) + brmsfamily(family = "com_poisson") #, link = "logit")

### SF ----
#### Home ----
formula_teamSF <- 
  bf(
    team_safeties|trials(2) ~ 
      team_def_epa_roll + opponent_off_epa_roll + 
      team_def_epa_roll:opponent_off_epa_roll +
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "binomial")

#### Away ----
formula_opponentSF <- 
  bf(
    opponent_safeties|trials(2) ~ 
      opponent_def_epa_roll + team_off_epa_roll + 
      opponent_def_epa_roll:team_off_epa_roll +
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "binomial")

### XP ----
#### Home ----
formula_teamXP <- 
  bf(
    team_pat_made ~ #|trials(team_totalTD)
      team_off_pat_pct_roll + 
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "discrete_weibull")

#### Away ----
formula_opponentXP <- 
  bf(
    opponent_pat_made ~ #|trials(opponent_totalTD)
      opponent_off_pat_pct_roll + 
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "discrete_weibull")

### TP ----
#### Home ----
formula_teamTP <- 
  bf(
    team_twoPtConv|trials(2) ~ #|trials(team_totalTD-team_pat_made)
      team_off_pass_plays_cum + 
      team_off_rush_plays_cum +
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "binomial")

#### Away ----
formula_opponentTP <- 
  bf(
    opponent_twoPtConv|trials(2)  ~ #|trials(opponent_totalTD-opponent_pat_made)
      opponent_off_pass_plays_cum + 
      opponent_off_rush_plays_cum +
      (1|H|team) + 
      (1|opponent)
  ) + brmsfamily(family = "binomial")

### Scores ----
#### Separate ----
formula_teamScore <- 
  bf(
    team_score ~ 
      6*team_totalTD + 
      3*team_fg_made +
      2*team_safeties +
      team_pat_made + 
      2*team_twoPtConv,
    nl = TRUE
  )

formula_opponentScore <- 
  bf(
    opponent_score ~ 
      6*opponent_totalTD + 
      3*opponent_fg_made +
      2*opponent_safeties +
      opponent_pat_made + 
      2*opponent_twoPtConv,
    nl = TRUE
  )

#### Combined ----
##### Home ----
formula_teamScore <- 
  bf(
    team_score ~ 6*teamTD + 3*teamFG + 2*teamSF + teamXP + 2*teamTP,
    teamTD ~ 
      s(team_OSRS_net) +
      team_DSRS_net +
      team_off_epa_roll + opponent_def_epa_roll +
      team_off_epa_roll:opponent_def_epa_roll + 
      team_off_n + team_off_TD +
      team_off_n:team_off_TD +
      team_def_n + team_def_TD +
      team_def_n:team_def_TD +
      (1|team) +
      (1|opponent),
    teamFG ~ 
      team_SRS_net +
      team_off_n + team_off_fg +
      team_off_n:team_off_fg +
      (1|team) + 
      (1|opponent),
    teamSF ~ 
      team_def_epa_roll + opponent_off_epa_roll + 
      team_def_epa_roll:opponent_off_epa_roll +
      (1|team) + 
      (1|opponent),
    teamXP|trials(teamTD) ~ 
      team_off_pat_pct_roll + 
      (1|team) + 
      (1|opponent),
    teamTP|trials(teamTD-teamXP) ~ 
      team_off_pass_plays_cum + 
      team_off_rush_plays_cum + + 
      (1|team) + 
      (1|opponent),
    nl = TRUE
  ) #+ brmsfamily(family = "discrete_weibull")

##### Away ----
formula_opponentScore <- 
  bf(
    opponent_score ~ 6*opponentTD + 3*opponentFG + 2*opponentSF + opponentXP + 2*opponentTP,
    opponentTD ~ 
      opponent_OSRS_net +
      opponent_DSRS_net +
      opponent_off_epa_roll + team_def_epa_roll +
      opponent_off_epa_roll:team_def_epa_roll + 
      opponent_off_n + opponent_off_TD +
      opponent_off_n:opponent_off_TD +
      opponent_def_n + opponent_def_TD +
      opponent_def_n:opponent_def_TD +
      (1|team) +
      (1|opponent),
    opponentFG ~ 
      opponent_SRS_net +
      opponent_off_n + opponent_off_fg +
      opponent_off_n:opponent_off_fg +
      (1|team) + 
      (1|opponent),
    opponentSF ~ 
      opponent_def_epa_roll + team_off_epa_roll + 
      opponent_def_epa_roll:team_off_epa_roll +
      (1|team) + 
      (1|opponent),
    opponentXP|trials(opponentTD) ~ 
      opponent_off_pat_pct_roll + 
      (1|team) + 
      (1|opponent),
    opponentTP|trials(opponentTD-opponentXP) ~ 
      opponent_off_pass_plays_cum + 
      opponent_off_rush_plays_cum +
      (1|team) + 
      (1|opponent),
    nl = TRUE
  ) #+ brmsfamily(family = "discrete_weibull")

#### Combined ----
##### Home ----
formula_teamScore <- 
  bf(
    team_score ~ 
      0 + Intercept +
      s(team_OSRS_net) +
      #team_DSRS_net +
      team_off_epa_roll + opponent_def_epa_roll +
      team_off_epa_roll:opponent_def_epa_roll + 
      #team_off_n + 
      team_off_td +
      #team_off_n:team_off_td +
      team_def_n + team_def_td +
      team_SRS_net +
      team_off_n + team_off_fg +
      team_off_n:team_off_fg +
      (1|H|team) +
      (1|A|opponent)
  ) + brmsfamily(family = "discrete_weibull",
                 link = "identity")

##### Away ----
formula_opponentScore <- 
  bf(
    opponent_score ~
      0 + Intercept +
      #opponent_OSRS_net +
      #opponent_DSRS_net +
      #opponent_off_epa_roll + 
      s(team_def_epa_roll) +
      #opponent_off_epa_roll:team_def_epa_roll + 
      #opponent_off_n + 
      opponent_off_td +
      opponent_off_n:opponent_off_td +
      opponent_def_n + #opponent_def_td +
      #opponent_def_n:opponent_def_td +
      opponent_SRS_net +
      opponent_off_n + opponent_off_fg +
      opponent_off_n:opponent_off_fg +
      (1|H|team) + 
      (1|A|opponent)
  ) + brmsfamily(family = "discrete_weibull",
                 link = "identity")

## Fit ----
# priorPoints <- c(
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "teamtotalTD"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "teamfgmade"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "opponenttotalTD"),
#   set_prior(horseshoe(df = 1, par_ratio = 0.3), class = "b", resp = "opponentfgmade")
# )

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "teamtotalTD"),
  prior(normal(0,5), class = "b", resp = "teamfgmade"),
  prior(normal(0,5), class = "b", resp = "teamsafeties"),
  prior(normal(0,5), class = "b", resp = "teampatmade"),
  prior(normal(0,5), class = "b", resp = "teamtwoPtConv"),
  prior(normal(0,5), class = "b", resp = "opponenttotalTD"),
  prior(normal(0,5), class = "b", resp = "opponentfgmade"),
  prior(normal(0,5), class = "b", resp = "opponentsafeties"),
  prior(normal(0,5), class = "b", resp = "opponentpatmade"),
  prior(normal(0,5), class = "b", resp = "opponenttwoPtConv")
)

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "teamtotalTD"),
  prior(normal(0,5), class = "b", resp = "teamfgmade"),
  # prior(normal(0,5), class = "b", resp = "teamsafeties"),
  # prior(normal(0,5), class = "b", resp = "teampatmade"),
  # prior(normal(0,5), class = "b", resp = "teamtwoPtConv"),
  prior(normal(0,5), class = "b", resp = "opponenttotalTD"),
  prior(normal(0,5), class = "b", resp = "opponentfgmade")
  # prior(normal(0,5), class = "b", resp = "opponentsafeties"),
  # prior(normal(0,5), class = "b", resp = "opponentpatmade")
  # prior(normal(0,5), class = "b", resp = "opponenttwoPtConv")
)

priorPoints <- c(
  prior(normal(0,5), class = "b", resp = "teamscore"),
  prior(normal(0,5), class = "b", resp = "opponentscore"),
  prior(normal(0,5), class = "b", resp = "teamscore"),
  prior(normal(0,5), class = "b", resp = "opponentscore")
)

# Fit the model using the custom family for total scores
# model_nfl_code <- stancode(
#   formula_team + formula_opponent,
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

system.time(
  model_nfl_fit <- brm(
    formula_teamTD, #+ formula_opponentTD +
      #formula_teamFG + formula_opponentFG +
      # formula_teamSF + formula_opponentSF +
      # formula_teamXP + formula_opponentXP +
      # formula_teamTP + formula_opponentTP +
      # formula_teamScore + formula_opponentScore +
      #set_rescor(rescor = FALSE),
    data = histModelData,
    #family = custom_family,
    save_pars = save_pars(all = TRUE),
    seed = 52,
    chains = chains, 
    cores = parallel::detectCores(),
    iter = iters,
    warmup = burn,
    init = 0,
    #stanvars = stanvars,
    #prior = priorPoints,
    drop_unused_levels = FALSE,
    control = list(adapt_delta = 0.95),
    backend = "cmdstan"
  )
)

Fit <- model_nfl_fit
fit <- 40
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
posterior_summary(Fit)
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
# fixedSigEff <- fixedSigEff |> 
#   rownames_to_column() |>
#   mutate(
#     response = str_split_i(rowname, "_", i = 1),
#     param = str_remove(rowname, paste0(response,"_"))
#   ) |> 
#   relocate(c(response, param), .after = "rowname") |>
#   select(-rowname)
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
#   filter(response == "teamtotalTD") |>
#   pull(param) |> unique()

randEff <- ranef(Fit, summary = TRUE)
print(randEff, digits = 4)
VarCorr(Fit)

#plot(Fit, ask = FALSE)

postSum <- posterior_summary(Fit)
#postSum[grepl("^sd_", rownames(postSum)), ]

### Bayes R2 -----
FitR2temp <- bayes_R2(Fit) |>
  bind_cols(Fit = paste0("Fit", fit)) |>
  select(Fit, everything())
FitR2temp

FitR2 <- bind_rows(
  FitR2temp,
  FitR2
)
FitR2 #<- FitR2temp

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
                                    "team_OSRS_net",
                                    "team_off_epa_roll",
                                    "opponent_off_td",
                                    "team_def_epa_roll",
                                    "opponent_SRS_net",
                                    "opponent_off_n"
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
# fitVarselHomeTD <- varsel(Fit, resp = "teamtotalTD")
# fitVarselHomeFG <- varsel(Fit, resp = "teamfgmade")
# fitVarselAwayTD <- varsel(Fit, resp = "opponenttotalTD")
# fitVarselAwayFG <- varsel(Fit, resp = "opponentfgmade")

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
# teamPPCTD <- pp_check(Fit, resp = "teamtotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC TD")) +
#   theme_bw()
# teamPPCfg <- pp_check(Fit, resp = "teamfgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC fg")) +
#   theme_bw()
# teamPPCxp <- pp_check(Fit2, resp = "teampatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC xp")) +
#   theme_bw()
# teamPPCtp <- pp_check(Fit2, resp = "teamtwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC tp")) +
#   theme_bw()
# teamPPCsafe <- pp_check(Fit, resp = "teamsafeties", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Home PPC safe")) +
#   theme_bw()

teamPPCbarsTD <- pp_check(Fit, resp = "teamtotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
teamPPCbarsFG <- pp_check(Fit, resp = "teamfgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Home PPC fg")) +
  theme_bw()
teamPPCbarsSF <- pp_check(Fit, resp = "teamsafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC safe")) +
  theme_bw()
teamPPCbarsXP <- pp_check(Fit, resp = "teampatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC xp")) +
  theme_bw()
teamPPCbarsTP <- pp_check(Fit, resp = "teamtwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Home PPC tp")) +
  theme_bw()

# opponentPPCTD <- pp_check(Fit, resp = "opponenttotalTD", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC TD")) +
#   theme_bw()
# opponentPPCfg <- pp_check(Fit, resp = "opponentfgmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC fg")) +
#   theme_bw()
# opponentPPCxp <- pp_check(Fit2, resp = "opponentpatmade", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC xp")) +
#   theme_bw()
# opponentPPCtp <- pp_check(Fit2, resp = "opponenttwoPtConv", ndraws = 100) +
#   labs(title = paste0("Fit", fit, " Away PPC tp")) +
#   theme_bw()
# opponentPPCsafe <- pp_check(Fit, resp = "opponentsafeties", ndraws = 100) + 
#   labs(title = paste0("Fit", fit, " Away PPC safe")) +
#   theme_bw()

opponentPPCbarsTD <- pp_check(Fit, resp = "opponenttotalTD", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC TD")) +
  theme_bw()
opponentPPCbarsFG <- pp_check(Fit, resp = "opponentfgmade", ndraws = 100, type = "bars") + 
  labs(title = paste0("Fit", fit, " Away PPC fg")) +
  theme_bw()
opponentPPCbarsSF <- pp_check(Fit, resp = "opponentsafeties", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC safe")) +
  theme_bw()
opponentPPCbarsXP <- pp_check(Fit, resp = "opponentpatmade", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC xp")) +
  theme_bw()
opponentPPCbarsTP <- pp_check(Fit, resp = "opponenttwoPtConv", ndraws = 100, type = "bars") +
  labs(title = paste0("Fit", fit, " Away PPC tp")) +
  theme_bw()


# teamPPCTD
# teamPPCfg
# teamPPCxp
# teamPPCtp
# teamPPCsafe
teamPPCbarsTD
teamPPCbarsFG
teamPPCbarsSF
teamPPCbarsXP
teamPPCbarsTP

# opponentPPCTD
# opponentPPCfg
# opponentPPCxp
# opponentPPCtp
# opponentPPCsafe
opponentPPCbarsTD
opponentPPCbarsFG
opponentPPCbarsSF
opponentPPCbarsXP
opponentPPCbarsTP

## Fitted
teamfinalFitTD <- posterior_predict(Fit, resp = "teamtotalTD")
teamfinalFitFG <- posterior_predict(Fit, resp = "teamfgmade")
teamfinalFitXP <- posterior_predict(Fit, resp = "teampatmade")
teamfinalFitTP <- posterior_predict(Fit, resp = "teamtwoPtConv")
teamfinalFitSF <- posterior_predict(Fit, resp = "teamsafeties")

## Preds
teamfinalPredsTD <- posterior_predict(Fit,
                                      resp = "teamtotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsFG <- posterior_predict(Fit,
                                      resp = "teamfgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsXP <- posterior_predict(Fit,
                                      resp = "teampatmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsTP <- posterior_predict(Fit,
                                      resp = "teamtwoPtConv",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsSF <- posterior_predict(Fit,
                                      resp = "teamsafeties",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Fitted
opponentfinalFitTD <- posterior_predict(Fit, resp = "opponenttotalTD")
opponentfinalFitFG <- posterior_predict(Fit, resp = "opponentfgmade")
opponentfinalFitXP <- posterior_predict(Fit, resp = "opponentpatmade")
opponentfinalFitTP <- posterior_predict(Fit, resp = "opponenttwoPtConv")
opponentfinalFitSF <- posterior_predict(Fit, resp = "opponentsafeties")

## Preds
opponentfinalPredsTD <- posterior_predict(Fit,
                                      resp = "opponenttotalTD",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsFG <- posterior_predict(Fit,
                                      resp = "opponentfgmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsXP <- posterior_predict(Fit,
                                      resp = "opponentpatmade",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsTP <- posterior_predict(Fit,
                                      resp = "opponenttwoPtConv",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsSF <- posterior_predict(Fit,
                                      resp = "opponentsafeties",
                                      newdata = modelData,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

teamPPDbarsTD <- ppc_bars(y = modelData$team_totalTD, 
                          yrep = teamfinalPredsTD[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD TD")) +
  theme_bw()
teamPPDbarsFG <- ppc_bars(y = modelData$team_fg_made, 
                          yrep = teamfinalPredsFG[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPD fg")) +
  theme_bw()
teamPPDbarsXP <- ppc_bars(y = modelData$team_pat_made,
                          yrep = teamfinalPredsXP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD xp")) +
  theme_bw()
teamPPDbarsTP <- ppc_bars(y = modelData$team_twoPtConv,
                          yrep = teamfinalPredsTP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()
teamPPDbarsSF <- ppc_bars(y = modelData$team_safeties,
                          yrep = teamfinalPredsSF[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()


opponentPPDbarsTD <- ppc_bars(y = modelData$opponent_totalTD, 
                          yrep = opponentfinalPredsTD[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD TD")) +
  theme_bw()
opponentPPDbarsFG <- ppc_bars(y = modelData$opponent_fg_made, 
                          yrep = opponentfinalPredsFG[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away PPD fg")) +
  theme_bw()
opponentPPDbarsXP <- ppc_bars(y = modelData$opponent_pat_made,
                          yrep = opponentfinalPredsXP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Away PPD xp")) +
  theme_bw()
opponentPPDbarsTP <- ppc_bars(y = modelData$opponent_twoPtConv,
                          yrep = opponentfinalPredsTP[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Away PPD tp")) +
  theme_bw()
opponentPPDbarsSF <- ppc_bars(y = modelData$opponent_safeties,
                          yrep = opponentfinalPredsSF[sample(1:sims, 100, replace = FALSE), ]) +
  labs(title = paste0("Fit", fit, " Home PPD tp")) +
  theme_bw()

teamPPDbarsTD
teamPPDbarsFG
teamPPDbarsXP
teamPPDbarsTP
teamPPDbarsSF

opponentPPDbarsTD
opponentPPDbarsFG
opponentPPDbarsXP
opponentPPDbarsTP
opponentPPDbarsSF

#### Home Score ----
teamfinalFit <- 
  6*teamfinalFitTD + 
  3*teamfinalFitFG +
  1*teamfinalFitXP +
  2*teamfinalFitTP +
  2*teamfinalFitSF

teamfinalFit <- 
  6*teamfinalFitTD + 
  3*teamfinalFitFG +
  1*teamfinalFitXP

teamfinalFit <- 
  7*teamfinalFitTD + 
  3*teamfinalFitFG

## Fitted
#teamfinalFit <- posterior_predict(Fit, resp = "teamscore")
teamfinalFitMean <- colMeans(teamfinalFit)
teamfinalFitMed <- apply(teamfinalFit, 2, function(x){quantile(x, 0.5)})
teamfinalFitLCB <- apply(teamfinalFit, 2, function(x){quantile(x, 0.025)})
teamfinalFitUCB <- apply(teamfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
teamfinalPreds <- 
  6*teamfinalPredsTD + 
  3*teamfinalPredsFG +
  1*teamfinalPredsXP +
  2*teamfinalPredsTP +
  2*teamfinalPredsSF

teamfinalPreds <- 
  6*teamfinalPredsTD + 
  3*teamfinalPredsFG +
  1*teamfinalPredsXP

teamfinalPreds <- 
  7*teamfinalPredsTD + 
  3*teamfinalPredsFG

# teamfinalPreds <- posterior_predict(Fit,
#                                     resp = "teamscore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
teamfinalPredsMean <- colMeans(teamfinalPreds)
teamfinalPredsMed <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
teamfinalPredsLCB <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
teamfinalPredsUCB <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
opponentfinalFit <- 
  6*opponentfinalFitTD + 
  3*opponentfinalFitFG +
  1*opponentfinalFitXP +
  2*opponentfinalFitTP +
  2*opponentfinalFitSF

opponentfinalFit <- 
  6*opponentfinalFitTD + 
  3*opponentfinalFitFG +
  1*opponentfinalFitXP

opponentfinalFit <- 
  7*opponentfinalFitTD + 
  3*opponentfinalFitFG

## Fitted
#opponentfinalFit <- posterior_predict(Fit, resp = "opponentscore")
opponentfinalFitMean <- colMeans(opponentfinalFit)
opponentfinalFitMed <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.5)})
opponentfinalFitLCB <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.025)})
opponentfinalFitUCB <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
opponentfinalPreds <- 
  6*opponentfinalPredsTD + 
  3*opponentfinalPredsFG +
  1*opponentfinalPredsXP +
  2*opponentfinalPredsTP +
  2*opponentfinalPredsSF

opponentfinalPreds <- 
  6*opponentfinalPredsTD + 
  3*opponentfinalPredsFG +
  1*opponentfinalPredsXP

opponentfinalPreds <- 
  7*opponentfinalPredsTD + 
  3*opponentfinalPredsFG

# opponentfinalPreds <- posterior_predict(Fit,
#                                     resp = "opponentscore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
opponentfinalPredsMean <- colMeans(opponentfinalPreds)
opponentfinalPredsMed <- apply(opponentfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
opponentfinalPredsLCB <- apply(opponentfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
opponentfinalPredsUCB <- apply(opponentfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("team", "opponent"),
  MAE_fit = c(mean(abs(teamfinalFitMean - histModelData$team_score)),
              mean(abs(opponentfinalFitMean - histModelData$opponent_score))),
  COV_fit = c(mean(teamfinalFitLCB < histModelData$team_score &  histModelData$team_score < teamfinalFitUCB),
              mean(opponentfinalFitLCB < histModelData$opponent_score &  histModelData$opponent_score < opponentfinalFitUCB)),
  MAE_pred = c(mean(abs(teamfinalPredsMean - modelData$team_score), na.rm = TRUE),
               mean(abs(opponentfinalPredsMean - modelData$opponent_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(teamfinalPredsMed - modelData$team_score), na.rm = TRUE),
               mean(abs(opponentfinalPredsMed - modelData$opponent_score), na.rm = TRUE)),
  COV_pred = c(mean(teamfinalPredsLCB < modelData$team_score & modelData$team_score < teamfinalPredsUCB, na.rm = TRUE),
               mean(opponentfinalPredsLCB < modelData$opponent_score & modelData$opponent_score < opponentfinalPredsUCB, na.rm = TRUE))
  # team_MAE_fit = mean(abs(teamfinalFitMean - histModelData$team_score)),
  # team_COV_fit = mean(teamfinalFitLCB < histModelData$team_score &  histModelData$team_score < teamfinalFitUCB),
  # opponent_MAE_fit = mean(abs(opponentfinalFitMean - histModelData$opponent_score)),
  # opponent_COV_fit = mean(opponentfinalFitLCB < histModelData$opponent_score &  histModelData$opponent_score < opponentfinalFitUCB),
  # team_MAE_pred = mean(abs(teamfinalPredsMean - modelData$team_score), na.rm = TRUE),
  # team_MAD_pred = mean(abs(teamfinalPredsMed - modelData$team_score), na.rm = TRUE),
  # team_COV_pred = mean(teamfinalPredsLCB < modelData$team_score & modelData$team_score < teamfinalPredsUCB),
  # opponent_MAE_pred = mean(abs(opponentfinalPredsMean - modelData$opponent_score), na.rm = TRUE),
  # opponent_MAD_pred = mean(abs(opponentfinalPredsMed - modelData$opponent_score), na.rm = TRUE),
  # opponent_COV_pred = mean(opponentfinalPredsLCB < modelData$opponent_score & modelData$opponent_score < opponentfinalPredsUCB, na.rm = TRUE)
)
predMetricsHA


#### Spread ----
FittedSpread <- teamfinalFit - opponentfinalFit
#Fitted <- posterior_predict(Fit)
FittedMeanSpread <- colMeans(FittedSpread)
FittedMedSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.5)})
FittedLCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.025)})
FittedUCBSpread <- apply(FittedSpread, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsSpread <- teamfinalPreds - opponentfinalPreds
# Preds <- posterior_predict(Fit, 
#                            newdata = modelDataTestNA,
#                            allow_new_levels = TRUE, 
#                            re_formula = NULL
# )
PredsMeanSpread <- colMeans(PredsSpread)
PredsMedSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
PredsLCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
PredsUCBSpread <- apply(PredsSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

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
spreadPPC <- ppc_dens_overlay(y = histModelData$result, 
                              yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home PPC TD")) +
  theme_bw()
spreadPPC

spreadPPCbars <- ppc_bars(y = histModelData$result, 
                          yrep = FittedSpread[sample(1:sims, 100, replace = FALSE), ]) + 
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
         team, team_score, opponent, opponent_score,
         result, spread_line, spreadCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > team_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > opponent_spread_prob, FALSE, NA)),
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
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
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
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
  ) |>
  select(game_id, season, week, #game_type,
         team, team_score, opponent, opponent_score,
         result, spread_line,spreadCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadPred = PredsMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > team_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > opponent_spread_prob, FALSE, NA)),
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
FittedTotal <- teamfinalFit + opponentfinalFit
#Fitted <- posterior_predict(Fit)
FittedMeanTotal <- colMeans(FittedTotal)
FittedMedTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.5)})
FittedLCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.025)})
FittedUCBTotal <- apply(FittedTotal, 2, function(x){quantile(x, 0.975)})

# Prediction
PredsTotal <- teamfinalPreds + opponentfinalPreds
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
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
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
         team, team_score, opponent, opponent_score,
         result, total_line, totalCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
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
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
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
         !is.na(team_totalTD),
         !is.na(opponent_totalTD),
         !is.na(team_fg_made),
         !is.na(opponent_fg_made)
  )|>
  select(game_id, season, week, #game_type,
         team, team_score, opponent, opponent_score,
         result, total_line, totalCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
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

ppc_error_scatter_avg_vs_x(histModelData$result, FittedSpread, histModelData$team_SRS_net)
ppc_error_scatter_avg_vs_x(modelData$result, PredsSpread, modelData$team_SRS_net)
ppc_error_scatter_avg_vs_x(histModelData$total, FittedTotal, histModelData$team_SRS_net)
ppc_error_scatter_avg_vs_x(modelData$total, PredsTotal, modelData$team_SRS_net)

ppc_error_scatter_avg_vs_x(histModelData$result, FittedSpread, histModelData$week)
ppc_error_scatter_avg_vs_x(modelData$result, PredsSpread, modelData$week)
ppc_error_scatter_avg_vs_x(histModelData$total, FittedTotal, histModelData$week)
ppc_error_scatter_avg_vs_x(modelData$total, PredsTotal, modelData$week)

ppc_error_scatter_avg_grouped(histModelData$result, FittedSpread, 
                              histModelData$team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData$result, PredsSpread, 
                              modelData$team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(histModelData$total, FittedTotal, histModelData$team)
ppc_error_scatter_avg_grouped(modelData$total, PredsTotal, modelData$team)

ppc_error_scatter_avg_grouped(histModelData$result, FittedSpread, histModelData$opponent)
ppc_error_scatter_avg_grouped(modelData$result, PredsSpread, modelData$opponent)
ppc_error_scatter_avg_grouped(histModelData$total, FittedTotal, histModelData$opponent)
ppc_error_scatter_avg_grouped(modelData$total, PredsTotal, modelData$opponent)


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
teamfinalPredsTD <- posterior_predict(fit2,
                                      resp = "teamtotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsfg <- posterior_predict(Fit,
                                      resp = "teamfgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredsxp <- posterior_predict(Fit,
                                      resp = "teampatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
teamfinalPredstp <- posterior_predict(Fit,
                                      resp = "teamtwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Preds
opponentfinalPredsTD <- posterior_predict(Fit,
                                      resp = "opponenttotalTD",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsfg <- posterior_predict(Fit,
                                      resp = "opponentfgmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredsxp <- posterior_predict(Fit,
                                      resp = "opponentpatmade",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)
opponentfinalPredstp <- posterior_predict(Fit,
                                      resp = "opponenttwoPtConv",
                                      newdata = testWeekData2,
                                      allow_new_levels = TRUE,
                                      re_formula = NULL
)

## Prediction on new data
teamfinalPreds <-
  6*teamfinalPredsTD +
  3*teamfinalPredsfg +
  1*teamfinalPredsxp +
  2*teamfinalPredstp #+
#2*teamfinalPredssafe

# teamfinalPreds <- posterior_predict(Fit,
#                                     resp = "teamscore",
#                                     newdata = modelData,
#                                     allow_new_levels = TRUE,
#                                     re_formula = NULL
# )
teamfinalPredsMean <- colMeans(teamfinalPreds)
teamfinalPredsMed <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
teamfinalPredsLCB <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
teamfinalPredsUCB <- apply(teamfinalPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score ----
opponentfinalFit <- 
  6*opponentfinalFitTD + 
  3*opponentfinalFitfg +
  1*opponentfinalFitxp +
  2*opponentfinalFittp #+
#2*opponentfinalFitsafe
## Fitted
#opponentfinalFit <- posterior_predict(Fit, resp = "opponentscore")
opponentfinalFitMean <- colMeans(opponentfinalFit)
opponentfinalFitMed <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.5)})
opponentfinalFitLCB <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.025)})
opponentfinalFitUCB <- apply(opponentfinalFit, 2, function(x){quantile(x, 0.975)})

## Prediction on new data
opponentfinalPreds <-
  6*opponentfinalPredsTD +
  3*opponentfinalPredsfg +
  1*opponentfinalPredsxp +
  2*opponentfinalPredstp #+

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
    coef_name <- sub("b_(teamtotalTD|teamfgmade|teampatmade|teamtwoPtConv|opponenttotalTD|opponentfgmade|opponentpatmade|opponenttwoPtConv)_", "", param)
    #response <- ifelse(grepl("_teamscore_", param), "teamscore", "opponentscore")
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
    group_name <- sub("cor_(team|opponent)__(.*)", "\\1", param)
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
    group_name <- sub("sd_(team|opponent)__(.*)", "\\1", param)
    #response <- ifelse(grepl("opponentscore", param), "opponentscore", "teamscore")
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
    response <- sub("shape_", "", param)  # Extract response (e.g., teamscore or opponentscore)
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
    response <- ifelse(grepl("opponentscore", param), "opponentscore", "teamscore")
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
teamfinalIterFitComb <- NULL
opponentfinalIterFitComb <- NULL
teamfinalIterPredsComb <- NULL
opponentfinalIterPredsComb <- NULL
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
  teamfinalIterPreds <- posterior_predict(iterFit,
                                          resp = "teamscore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  opponentfinalIterPreds <- posterior_predict(iterFit,
                                          resp = "opponentscore",
                                          newdata = predictWeekData,
                                          allow_new_levels = TRUE,
                                          re_formula = NULL
  )
  
  # Update prediction matrix
  teamfinalIterPredsComb <- cbind(teamfinalIterPredsComb, teamfinalIterPreds)
  opponentfinalIterPredsComb <- cbind(opponentfinalIterPredsComb, opponentfinalIterPreds)
  
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
  teamfinalIterFit <- posterior_predict(iterFit, 
                                        resp = "teamscore")
  opponentfinalIterFit <- posterior_predict(iterFit, 
                                        resp = "opponentscore")
  
  # Update prediction matrix
  teamfinalIterFitComb <- cbind(teamfinalIterFitComb, teamfinalIterFit)
  opponentfinalIterFitComb <- cbind(opponentfinalIterFitComb, opponentfinalIterFit)
  
  print(paste0("Finished Week ", i))
}




## Fit Model 2 ----
# Initialize values
predWeeks <- max(modelData$week)
iterFitBase <- fit18
#iterFitBaseB <- fitB8
iterFit <- iterFitBase
#iterFitB <- iterFitBaseB
teamfinalIterFitTDComb2 <- list()
teamfinalIterFitfgComb2 <- list()
# teamfinalIterFitxpComb2 <- list()
# teamfinalIterFittpComb2 <- list()
teamfinalIterFitComb2 <- list()

opponentfinalIterFitTDComb2 <- list()
opponentfinalIterFitfgComb2 <- list()
# opponentfinalIterFitxpComb2 <- list()
# opponentfinalIterFittpComb2 <- list()
opponentfinalIterFitComb2 <- list()

teamfinalIterPredsTDComb2 <- list()
teamfinalIterPredsfgComb2 <- list()
# teamfinalIterPredsxpComb2 <- list()
# teamfinalIterPredstpComb2 <- list()
teamfinalIterPredsComb2 <- list()

opponentfinalIterPredsTDComb2 <- list()
opponentfinalIterPredsfgComb2 <- list()
# opponentfinalIterPredsxpComb2 <- list()
# opponentfinalIterPredstpComb2 <- list()
opponentfinalIterPredsComb2 <- list()

prePriors <- posterior_summary(iterFitBase)
prePriorCoefs <- prior_summary(iterFitBase)$coef
old_priors <- create_updated_priors(post_summary = prePriors)

trainingData <- modData2 |> filter(season %in% 2023:2023)
predictorIterData <- trainingData |> 
  select(-team_score, -opponent_score,
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
    teamfinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "teamtotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    opponentfinalIterPredsTD <- posterior_predict(iterFit,
                                              resp = "opponenttotalTD",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    teamfinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "teamfgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    opponentfinalIterPredsfg <- posterior_predict(iterFit,
                                              resp = "opponentfgmade",
                                              newdata = predictWeekData,
                                              allow_new_levels = TRUE,
                                              re_formula = NULL
    )
    
    # teamfinalIterPredsTDMed <- apply(teamfinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # opponentfinalIterPredsTDMed <- apply(opponentfinalIterPredsTD, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
    # predictWeekData <- predictWeekData |>
    #   mutate(
    #     team_totalTD = teamfinalIterPredsTDMed,
    #     opponent_totalTD = opponentfinalIterPredsTDMed
    #   )
    # 
    # teamfinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "teampatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # teamfinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "teamtwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # opponentfinalIterPredsxp <- posterior_predict(iterFitB,
    #                                           resp = "opponentpatmade",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    # opponentfinalIterPredstp <- posterior_predict(iterFitB,
    #                                           resp = "opponenttwoPtConv",
    #                                           newdata = predictWeekData,
    #                                           allow_new_levels = TRUE,
    #                                           re_formula = NULL
    # )
    
    teamfinalIterPreds <- 
      7*teamfinalIterPredsTD + 
      3*teamfinalIterPredsfg #+
    # 1*teamfinalIterPredsxp +
    # 2*teamfinalIterPredstp
    
    opponentfinalIterPreds <- 
      7*opponentfinalIterPredsTD + 
      3*opponentfinalIterPredsfg #+
    # 1*opponentfinalIterPredsxp +
    # 2*opponentfinalIterPredstp
    
    teamfinalIterPredsComb2[[i]] <- teamfinalIterPreds
    opponentfinalIterPredsComb2[[i]] <- opponentfinalIterPreds
    
    # Update prediction matrix
    teamfinalIterPredsTDComb2[[i]] <- teamfinalIterPredsTD
    teamfinalIterPredsfgComb2[[i]] <- teamfinalIterPredsfg
    # teamfinalIterPredsxpComb2[[i]] <- teamfinalIterPredsxp
    # teamfinalIterPredstpComb2[[i]] <- teamfinalIterPredstp
    opponentfinalIterPredsTDComb2[[i]] <- opponentfinalIterPredsTD
    opponentfinalIterPredsfgComb2[[i]] <- opponentfinalIterPredsfg
    # opponentfinalIterPredsxpComb2[[i]] <- opponentfinalIterPredsxp
    # opponentfinalIterPredstpComb2[[i]] <- opponentfinalIterPredstp
    
    # iterData <- modelData |>
    #   filter(week %in% i:i)
    # 
    # new_priors <- create_updated_priors(post_summary = posterior_summary(iterFit))
    trainingData <- bind_rows(
      modData2 |> filter(season %in% 2023:2023),
      modData2 |> filter(season == 2024, week <= i) 
    )
    predictorIterData <- trainingData |> 
      select(-team_score, -opponent_score,
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
    teamfinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "teamtotalTD")
    teamfinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "teamfgmade")
    # teamfinalIterFitxp <- posterior_predict(iterFitB, resp = "teampatmade")
    # teamfinalIterFittp <- posterior_predict(iterFitB, resp = "teamtwoPtConv")
    
    
    opponentfinalIterFitTD <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "opponenttotalTD")
    opponentfinalIterFitfg <- posterior_predict(iterFit, 
                                            newdata = trainingWeekData,
                                            resp = "opponentfgmade")
    # opponentfinalIterFitxp <- posterior_predict(iterFitB, resp = "opponentpatmade")
    # opponentfinalIterFittp <- posterior_predict(iterFitB, resp = "opponenttwoPtConv")
    
    teamfinalIterFit <- 
      7*teamfinalIterFitTD + 
      3*teamfinalIterFitfg #+
    # 1*teamfinalIterFitxp +
    # 2*teamfinalIterFittp
    
    opponentfinalIterFit <- 
      7*opponentfinalIterFitTD + 
      3*opponentfinalIterFitfg #+
    # 1*opponentfinalIterFitxp +
    # 2*opponentfinalIterFittp
    
    teamfinalIterFitComb2[[i]] <- teamfinalIterFit
    opponentfinalIterFitComb2[[i]] <- opponentfinalIterFit
    
    # Update prediction matrix
    teamfinalIterFitTDComb2[[i]] <- teamfinalIterFitTD
    teamfinalIterFitfgComb2[[i]] <- teamfinalIterFitfg
    # teamfinalIterFitxpComb2[[i]] <- teamfinalIterFitxp
    # teamfinalIterFittpComb2[[i]] <- teamfinalIterFittp
    opponentfinalIterFitTDComb2[[i]] <- opponentfinalIterFitTD
    opponentfinalIterFitfgComb2[[i]] <- opponentfinalIterFitfg
    # opponentfinalIterFitxpComb2[[i]] <- opponentfinalIterFitxp
    # opponentfinalIterFittpComb2[[i]] <- opponentfinalIterFittp
    
    print(paste0("Finished Week ", i))
  }
)

iterFit2 <- iterFit

save(iterFit1,
     teamfinalIterFitTDComb2,
     teamfinalIterFitfgComb2,
     teamfinalIterFitComb2,
     
     opponentfinalIterFitTDComb2,
     opponentfinalIterFitfgComb2,
     opponentfinalIterFitComb2,
     
     teamfinalIterPredsTDComb2,
     teamfinalIterPredsfgComb2,
     teamfinalIterPredsComb2,
     
     opponentfinalIterPredsTDComb2,
     opponentfinalIterPredsfgComb2,
     opponentfinalIterPredsComb2,
     file = "~/Desktop/NFL Analysis Data/iter data Multi3.RData")

## Diagnostics ----




## Fitted
dim(teamfinalIterFitComb2[[1]])

teamfinalIterPredsTD2 <- do.call(cbind, teamfinalIterPredsTDComb2)
teamfinalIterPredsfg2 <- do.call(cbind, teamfinalIterPredsfgComb2)
# teamfinalIterPredsxp2 <- do.call(cbind, teamfinalIterPredsxpComb2)
# teamfinalIterPredstp2 <- do.call(cbind, teamfinalIterPredstpComb2)
opponentfinalIterPredsTD2 <- do.call(cbind, opponentfinalIterPredsTDComb2)
opponentfinalIterPredsfg2 <- do.call(cbind, opponentfinalIterPredsfgComb2)
# opponentfinalIterPredsxp2 <- do.call(cbind, opponentfinalIterPredsxpComb2)
# opponentfinalIterPredstp2 <- do.call(cbind, opponentfinalIterPredstpComb2)

teamfinalIterPreds <- 
  7*teamfinalIterPredsTD2 +
  3*teamfinalIterPredsfg2 #+
# 1*teamfinalIterPredsxp2 +
#   2*teamfinalIterPredstp2

opponentfinalIterPreds <- 
  7*opponentfinalIterPredsTD2 +
  3*opponentfinalIterPredsfg2 #+
# 1*opponentfinalIterPredsxp2 +
#   2*opponentfinalIterPredstp2

## PPD Plots
teamIterPPDbarsTD <- ppc_bars(y = modelData$team_totalTD, 
                              yrep = teamfinalIterPredsTD2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD TD")) +
  theme_bw()
teamIterPPDbarsfg <- ppc_bars(y = modelData$team_fg_made, 
                              yrep = teamfinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Home IterPPD fg")) +
  theme_bw()
# teamIterPPDbarsxp <- ppc_bars(y = modelData$team_pat_made, 
#                               yrep = teamfinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home IterPPD xp")) +
#   theme_bw()
# teamIterPPDbarstp <- ppc_bars(y = modelData$team_twoPtConv, 
#                               yrep = teamfinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Home IterPPD tp")) +
#   theme_bw()

opponentIterPPDbarsTD <- ppc_bars(y = modelData$opponent_totalTD, 
                              yrep = opponentfinalIterPredsTD2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD TD")) +
  theme_bw()
opponentIterPPDbarsfg <- ppc_bars(y = modelData$opponent_fg_made, 
                              yrep = opponentfinalIterPredsfg2[sample(1:sims, 100, replace = FALSE), ]) + 
  labs(title = paste0("Fit", fit, " Away IterPPD fg")) +
  theme_bw()
# opponentIterPPDbarsxp <- ppc_bars(y = modelData$opponent_pat_made, 
#                               yrep = opponentfinalIterPredsxp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away IterPPD xp")) +
#   theme_bw()
# opponentIterPPDbarstp <- ppc_bars(y = modelData$opponent_twoPtConv, 
#                               yrep = opponentfinalIterPredstp2[sample(1:sims, 100, replace = FALSE), ]) + 
#   labs(title = paste0("Fit", fit, " Away IterPPD tp")) +
#   theme_bw()

teamIterPPDbarsTD
teamIterPPDbarsfg
# teamIterPPDbarsxp
# teamIterPPDbarstp

opponentIterPPDbarsTD
opponentIterPPDbarsfg
# opponentIterPPDbarsxp
# opponentIterPPDbarstp



### Home Score
teamfinalIterFit <- do.call(cbind, teamfinalIterFitComb2)
teamfinalIterFitMean <- colMeans(teamfinalIterFit)
teamfinalIterFitMed <- apply(teamfinalIterFit, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
teamfinalIterFitLCB <- apply(teamfinalIterFit, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
teamfinalIterFitUCB <- apply(teamfinalIterFit, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

## Prediction on new data
teamfinalIterPreds <- do.call(cbind, teamfinalIterPredsComb2)
teamfinalIterPredsMean <- colMeans(teamfinalIterPreds)
teamfinalIterPredsMed <- apply(teamfinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
teamfinalIterPredsLCB <- apply(teamfinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
teamfinalIterPredsUCB <- apply(teamfinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

#### Away Score
## Fitted
opponentfinalIterFit <- do.call(cbind, opponentfinalIterFitComb2)
opponentfinalIterFitMean <- colMeans(opponentfinalIterFit)
opponentfinalIterFitMed <- apply(opponentfinalIterFit, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
opponentfinalIterFitLCB <- apply(opponentfinalIterFit, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
opponentfinalIterFitUCB <- apply(opponentfinalIterFit, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

## Prediction on new data
opponentfinalIterPreds <- do.call(cbind, opponentfinalIterPredsComb2)
opponentfinalIterPredsMean <- colMeans(opponentfinalIterPreds)
opponentfinalIterPredsMed <- apply(opponentfinalIterPreds, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
opponentfinalIterPredsLCB <- apply(opponentfinalIterPreds, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
opponentfinalIterPredsUCB <- apply(opponentfinalIterPreds, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})


predIterMetricsHA <- tibble(
  Fit = rep(paste0("Fit", fit), 2),
  Score = c("team", "opponent"),
  # MAE_fit = c(mean(abs(teamfinalIterFitMean - modelData$team_score)),
  #             mean(abs(opponentfinalIterFitMean - modelData$opponent_score))),
  # COV_fit = c(mean(teamfinalIterFitLCB < modelData$team_score &  modelData$team_score < teamfinalIterFitUCB),
  #             mean(opponentfinalIterFitLCB < modelData$opponent_score &  modelData$opponent_score < opponentfinalIterFitUCB)),
  MAE_pred = c(mean(abs(teamfinalIterPredsMean - modelData$team_score), na.rm = TRUE),
               mean(abs(opponentfinalIterPredsMean - modelData$opponent_score), na.rm = TRUE)),
  MAD_pred = c(mean(abs(teamfinalIterPredsMed - modelData$team_score), na.rm = TRUE),
               mean(abs(opponentfinalIterPredsMed - modelData$opponent_score), na.rm = TRUE)),
  COV_pred = c(mean(teamfinalIterPredsLCB < modelData$team_score & modelData$team_score < teamfinalIterPredsUCB, na.rm = TRUE),
               mean(opponentfinalIterPredsLCB < modelData$opponent_score & modelData$opponent_score < opponentfinalIterPredsUCB, na.rm = TRUE))
  # team_MAE_fit = mean(abs(teamfinalIterFitMean - histModelData$team_score)),
  # team_COV_fit = mean(teamfinalIterFitLCB < histModelData$team_score &  histModelData$team_score < teamfinalIterFitUCB),
  # opponent_MAE_fit = mean(abs(opponentfinalIterFitMean - histModelData$opponent_score)),
  # opponent_COV_fit = mean(opponentfinalIterFitLCB < histModelData$opponent_score &  histModelData$opponent_score < opponentfinalIterFitUCB),
  # team_MAE_pred = mean(abs(teamfinalIterPredsMean - modelData$team_score), na.rm = TRUE),
  # team_MAD_pred = mean(abs(teamfinalIterPredsMed - modelData$team_score), na.rm = TRUE),
  # team_COV_pred = mean(teamfinalIterPredsLCB < modelData$team_score & modelData$team_score < teamfinalIterPredsUCB),
  # opponent_MAE_pred = mean(abs(opponentfinalIterPredsMean - modelData$opponent_score), na.rm = TRUE),
  # opponent_MAD_pred = mean(abs(opponentfinalIterPredsMed - modelData$opponent_score), na.rm = TRUE),
  # opponent_COV_pred = mean(opponentfinalIterPredsLCB < modelData$opponent_score & modelData$opponent_score < opponentfinalIterPredsUCB, na.rm = TRUE)
)
predIterMetricsHA


#### Spread ----
FittedIterSpread <- teamfinalIterFit - opponentfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanSpread <- colMeans(FittedIterSpread)
FittedIterMedSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
FittedIterLCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
FittedIterUCBSpread <- apply(FittedIterSpread, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

# Prediction
PredsIterSpread <- teamfinalIterPreds - opponentfinalIterPreds
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
         team, team_score, opponent, opponent_score,
         result, spread_line, spreadCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
         over_odds, over_prob,
         under_odds, under_prob) |>
  mutate(
    spreadFit = FittedIterMeanSpread,
    coverBet = ifelse(spreadFit > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = FittedIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > team_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > opponent_spread_prob, FALSE, NA)),
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
         team, team_score, opponent, opponent_score,
         result, spread_line,spreadCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob
         #over_odds, over_prob,
         #under_odds, under_prob
  ) |>
  mutate(
    spreadPred = PredsIterMeanSpread,
    coverBet = ifelse(spreadPred > spread_line, TRUE, FALSE),
    coverSuccess = coverBet == spreadCover,
    spreadCoverProb = PredsIterBetSpread,
    spreadCoverBet = ifelse(spreadCoverProb > team_spread_prob, TRUE,
                            ifelse(1 - spreadCoverProb > opponent_spread_prob, FALSE, NA)),
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
FittedIterTotal <- teamfinalIterFit + opponentfinalIterFit
#FittedIter <- posterior_predict(Fit)
FittedIterMeanTotal <- colMeans(FittedIterTotal)
FittedIterMedTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.5, na.rm = TRUE)})
FittedIterLCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.025, na.rm = TRUE)})
FittedIterUCBTotal <- apply(FittedIterTotal, 2, function(x){quantile(x, 0.975, na.rm = TRUE)})

# Prediction
PredsIterTotal <- teamfinalIterPreds + opponentfinalIterPreds
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
         team, team_score, opponent, opponent_score,
         result, total_line, totalCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
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
         team, team_score, opponent, opponent_score,
         result, total_line, totalCover,
         team_spread_odds, team_spread_prob,
         opponent_spread_prob, opponent_spread_prob,
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

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$team_SRS)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$team_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$team_OSRS)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$team_OSRS)

ppc_error_scatter_avg_vs_x(modelData1$result, FittedIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$result, PredsIterSpread, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, FittedIterTotal, modelData$week)
ppc_error_scatter_avg_vs_x(modelData1$total, PredsIterTotal, modelData$week)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, 
                              modelData$team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, 
                              modelData$team,
                              facet_args = list(scales = "fixed"))
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$team)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$team)

ppc_error_scatter_avg_grouped(modelData1$result, FittedIterSpread, modelData$opponent)
ppc_error_scatter_avg_grouped(modelData1$result, PredsIterSpread, modelData$opponent)
ppc_error_scatter_avg_grouped(modelData1$total, FittedIterTotal, modelData$opponent)
ppc_error_scatter_avg_grouped(modelData1$total, PredsIterTotal, modelData$opponent)





