# analyze_model_effects.R
# Functions to extract, visualize, and retrieve model effects & posteriors

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom.mixed)
library(brms)
library(tidybayes)
library(ggdist)  # for interval plotting

# 1. Extract fixed effects summaries from multiple models
#    models: named list of brmsfit objects
#    returns tibble with columns: model, term, estimate, std.error, conf.low, conf.high
extract_fixed_effects <- function(models) {
  map_dfr(names(models), function(mn) {
    suppressWarnings(
      broom.mixed::tidy(models[[mn]], effects = "fixed", conf.int = TRUE, ) %>%
        mutate(model = mn) %>%
        select(model, term, estimate, std.error, conf.low, conf.high)
    )
  })
}

# 2. Plot fixed effects forest plot across models
#    For large numbers of models, facet by term and remove legend
plot_fixed_effects <- function(fixed_df, facet = FALSE, facet_nrow = NULL, facet_ncol = NULL) {
  p <- ggplot(fixed_df, # |> filter(term == {{terms}}),
              aes(x = estimate, y = term)) +
    stat_interval(aes(xmin = conf.low, xmax = conf.high), 
                       .width = c(0.5, 0.8, 0.95), point_size = 1) +
    labs(x = "Estimate", y = "Model") +
    theme_minimal()
  if (facet) {
    p <- p + facet_wrap(~ term, scales = "free_y", nrow = facet_nrow, ncol = facet_ncol)
  }
  p
}


# 3. Extract variance components (sd of random effects) from multiple models
extract_variance_components <- function(models) {
  map_dfr(names(models), function(mn) {
    broom.mixed::tidy(models[[mn]], effects = "ran_pars", conf.int = TRUE) %>%
      filter(grepl("^sd_", term)) %>%
      mutate(model = mn) %>%
      select(model, term, estimate, std.error, conf.low, conf.high)
  })
}

# 4. Plot variance components
plot_variance_components <- function(var_df) {
  ggplot(var_df, aes(x = estimate, y = term, 
                     xmin = conf.low, xmax = conf.high#, color = .data[[color_by]]
  )) +
    geom_point(position = position_dodge(width = 0.5)) +
    stat_interval(position = position_dodge(width = 0.5), height = 0) +
    labs(x = "SD Estimate", y = NULL) +#, color = color_by) +
    theme_minimal()
}

# 5. Compare fixed effects between two models by difference
compare_fixed_diff <- function(models) {
  if (length(models) != 2) stop("Provide exactly two models in a named list")
  fe <- extract_fixed_effects(models)
  wide <- fe %>%
    select(model, term, estimate, conf.low, conf.high) %>%
    pivot_wider(names_from = model, values_from = c(estimate, conf.low, conf.high))
  m1 <- names(models)[1]; m2 <- names(models)[2]
  cat("Comparing models ", names(models)[1], " and ", names(models)[2], "\n")
  wide %>% transmute(
    term,
    diff_mean = !!sym(paste0("estimate_", m2)) - !!sym(paste0("estimate_", m1)),
    diff_lwr  = !!sym(paste0("conf.low_", m2))  - !!sym(paste0("conf.high_", m1)),
    diff_upr  = !!sym(paste0("conf.high_", m2)) - !!sym(paste0("conf.low_", m1))
  )
}

# 6. Extract random slopes for a predictor across models
extract_random_slopes <- function(models, effect, group) {
  map_dfr(names(models), function(mn) {
    re_df <- as.data.frame(ranef(models[[mn]])[[group]])
    if (str_detect(colnames(re_df)[1], effect, negate = TRUE)) {
      stop("Effect not found in random effects")
    }else{
      tibble(
        model       = mn,
        group_level = rownames(re_df),
        estimate    = re_df[[paste0("Estimate.", effect)]],
        std.error   = re_df[[paste0("Est.Error.", effect)]],
        conf.low    = re_df[[paste0("Q2.5.", effect)]],
        conf.high   = re_df[[paste0("Q97.5.", effect)]]
      )
    }
  })
}

# 7. Plot random slopes
plot_random_slopes <- function(random_df) {
  ggplot(random_df, aes(x = estimate, y = group_level,
                        xmin = conf.low, xmax = conf.high, #color = .data[[color_by]]
                        )) +
    geom_point(position = position_dodge(width = 0.7)) +
    stat_interval(position = position_dodge(width = 0.7), height = 0.5) +
    labs(x = "Random Slope Estimate", y = NULL) + #, color = color_by) +
    theme_minimal()
}

# 8. Extract posterior predictions from backtest results
#    results_list: named list with elements$model(grmfit) and $test(dataframe)
#    returns named list of posterior matrices
extract_posteriors <- function(results_list, re_formula = NULL, allow_new_levels = TRUE) {
  map(results_list, function(res) {
    posterior_predict(
      res$model,
      newdata = res$test,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels
    )
  })
}

# 9. Combine posterior matrices into one matrix
combine_posteriors <- function(post_list) {
  do.call(cbind, post_list)
}

# Run function ----
models <- map(backtest_results, "model"); 
names(models) <- names(backtest_results)

fixed_df <- extract_fixed_effects(models)
fixed_eff <- unique(fixed_df$term)
plot_fixed_effects(fixed_df, 
                   facet = FALSE,
                   facet_nrow = 1)

compare_fixed_diff(models = models[sample(1:length(models), 2, replace = FALSE)])

var_df <- extract_variance_components(models);
plot_variance_components(var_df)


random_df <- extract_random_slopes(models, 
                                   effect = "Intercept", 
                                   group = "home_team"); 
plot_random_slopes(random_df)

post_list <- extract_posteriors(backtest_results)
all_posts <- combine_posteriors(post_list)
