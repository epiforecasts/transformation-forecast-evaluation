library(scoringutils) # version 1.0.1
library(scoringRules)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(here)
library(data.table)
library(kableExtra)
library(performance)
library(tidytext)
library(grid)
library(ggpubr)
library(latex2exp)
library(gh)
library(covidHubUtils) # remotes::install_github("https://github.com/reichlab/covidHubUtils")

set.seed(1234)

## ========================================================================== ##
## FIGURE 1: Numerical comparison of different measures of relative error
## ========================================================================== ##

y_hat <- 1
grid_y <- (20:500)/100

data <- data.table(
  APE = abs((y_hat - grid_y)/grid_y),
  RE = abs((y_hat - grid_y)/y_hat),
  SAPE = abs((y_hat - grid_y)/(grid_y/2 + y_hat/2)),
  `AE after log transformation` = abs(log(y_hat) - log(grid_y)),
  `AE after sqrt transformation` = abs(sqrt(y_hat) - sqrt(grid_y)),
  x = grid_y / y_hat
) |>
  pivot_longer(cols = c(APE, RE, SAPE, 
                        `AE after log transformation`, `AE after sqrt transformation`), 
               names_to = "Metric") 

plot_fct <- function(data) {
  data |>
    ggplot(aes(x = x, y = value, color = Metric)) +
    theme_scoringutils() + 
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") + 
    geom_line() +
    labs(y = "Error", 
         x = "Observed value")
}

label_fn_hat <- function(x) {
  ifelse(x%%1 == 0, 
         TeX(paste0(x, "\\hat{y}")), 
         TeX(paste0("1/", 1/x, "\\hat{y}")))
}

p1 <- data |>
  filter(Metric != "AE after sqrt transformation")|>
  plot_fct() +
  scale_x_continuous(breaks = c(1/4, 1, 2, 4), 
                     labels = label_fn_hat) + 
  scale_color_brewer(palette = "Set1")

p2 <- p1 +
  scale_x_continuous(trans = "log10", breaks = c(1/4, 0.5, 1, 2, 4), 
                     labels = label_fn_hat)

(p1 + p2) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("output/figures/different-relative-errors.png", width = 7, height = 3)


## ========================================================================== ##
## SI Version of Figure 1 (Figure SI.1) 
## Numerical comparison square-root transformation
## ========================================================================== ##

p3 <- data |>
  filter(Metric != "AE after log transformation")|>
  plot_fct() +
  scale_x_continuous(breaks = c(1/4, 1, 2, 4), 
                     labels = label_fn_hat) + 
  scale_color_brewer(palette = "Set1")

p4 <- p3 +
  scale_x_continuous(trans = "log10", breaks = c(1/4, 0.5, 1, 2, 4), 
                     labels = label_fn_hat)

(p3 + p4) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("output/figures/different-relative-errors-sqrt.png", width = 7, height = 3)




## ========================================================================== ##
## FIGURE 2: Expected CRPS scores as a function of the mean 
## and variance of the forecast quantity.
## ========================================================================== ##
if (file.exists("output/data/simulation-figure-2.Rda")) {
  scores_fig_2 <- readRDS(file = "output/data/simulation-figure-2.Rda")
} else {
  
  time_points <- 1e5
  n_means <- 60
  means = (10^seq(1, 3.3, length.out = n_means))
  model_names <- c("theta_10", "theta_1e9")
  
  df <- data.table(
    state = 1:length(mean),
    mean = means
  ) |>
    mutate(date = list(1:time_points)) |>
    unnest(cols = date) |>
    rowwise() |>
    mutate(theta = list(c(10, 1e9)), 
           model = list(model_names)) |>
    ungroup() |>
    unnest(cols = c("theta", "model")) |>
    mutate(predictive_sample = rnbinom(n = time_points * n_means * length(model_names), 
                                        size = theta,
                                        mu = mean)) 
  
  # add normal
  df_normal <- filter(df, theta == 10) |>
    mutate(model = "normal", 
           theta = 0, 
           predictive_sample = rnorm(n = time_points * n_means, 
                                     mean = mean, 
                                     sd = 1))
  
  df <- rbind(df, df_normal)
  
  scores_fig_2 <- df |>
    group_by(mean, theta, model) |> 
    summarise(
      crps = mean(abs(predictive_sample[-1] - predictive_sample[-length(predictive_sample)]))/2,
      crps_log = mean(abs(log(predictive_sample[-1] + 1) - log(predictive_sample[-length(predictive_sample)] + 1)))/2,
      crps_sqrt = mean(abs(sqrt(predictive_sample[-1]) - sqrt(predictive_sample[-length(predictive_sample)])))/2
    ) |>
    pivot_longer(cols = c(crps, crps_log, crps_sqrt), values_to = "crps", names_to = "scale") |>
    # rename scale
    mutate(scale = ifelse(scale == "crps", "natural", 
                          ifelse(scale == "crps_log", "log", "sqrt")), 
           scale = factor(scale, levels = c("natural", "log", "sqrt"))) |> 
    mutate(var = ifelse(theta == 0, # normal distribution
                        1, 
                        mean + mean^2 / theta), # negative binomial distribution 
           approximation = ifelse(scale == "natural", 
                                  sqrt(var / pi), # approx for normal 
                                  ifelse(scale == "sqrt", 
                                         sqrt(var / (2^2 * mean * pi)), # approx for sqrt
                                         sqrt(var / pi) / (mean)))) # approx for log
  
  saveRDS(scores_fig_2, file = "output/data/simulation-figure-2.Rda")
}

p1 <- scores_fig_2 |>
  group_by(model, scale) |>
  filter(mean <= 2000) |>
  mutate(crps = crps / mean(crps), 
         approximation = approximation / mean(approximation)) |>
  mutate(scale = factor(scale, levels = c("natural", "sqrt", "log"))) |>
  ggplot(aes(y = crps, x = mean, colour = model)) +
  geom_line(aes(y = approximation)) +
  geom_point(size = 0.4) +  
  theme_scoringutils() + 
  theme(axis.title.y = element_text(size=7.1)) +
  labs(x = TeX("\\mu")) +
  scale_colour_discrete(
    labels=c(
      TeX(r'($\sigma^2 = const$)'), 
      TeX(r'($\sigma^2 = \mu + 0.1 \mu^2$)'), 
      #TeX(r'($\sigma^2 = \mu + \frac{\mu^{2.5}}{1000}$)'), 
      TeX(r'($\sigma^2 = \mu$)'))
  ) +
  facet_wrap(~ scale, scales = "free_y") 

p2 <- p1 + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")

p1 / p2 +
  plot_annotation(tag_levels = "A") +   
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") &
  labs(y = TeX("CRPS (normalised)"))

ggsave("output/figures/SIM-mean-state-size.png", width = 7, height = 4.1)


## ========================================================================== ##
## SI Version of Figure 2 (Figure SI.2)
## Illustration of the approximation
## ========================================================================== ##

scores_fig_2 |>
  ungroup() |>
  mutate(
    model = factor(
      model, 
      labels=c(
        TeX(r'($\sigma^2 = const$)'), 
        TeX(r'($\sigma^2 = \mu + 0.1 \mu^2$)'), 
        #TeX(r'($\sigma^2 = \mu + \frac{\mu^{2.5}}{1000}$)'), 
        TeX(r'($\sigma^2 = \mu$)')))
  ) |>
  ggplot(aes(y = crps, x = mean)) +
  geom_line(aes(y = approximation, color = "Approximated score")) +
  geom_point(aes(color = "Score of a simulated ideal forecaster"), alpha = 0.2) + 
  facet_wrap(scale ~ model, scales = "free", labeller = label_parsed) + 
  theme_scoringutils() +
  scale_colour_manual(values = c("black", "red")) +
  scale_x_continuous(trans = "log10") + 
  labs(y = "Mean CRPS", colour = "Score") +
  expand_limits(y = 0)

ggsave("output/figures/SIM-score-approximation.png", width = 7, height = 8)



## ========================================================================== ##
## FIGURE 3: llustration of impropriety of log-transformed CRPS. 
## ========================================================================== ##

# in this plot the CRPS is approximated by the WIS with 99 quantiles
n_sim <- 1e3
epsilon <- rnorm(n_sim)
Y <- exp(epsilon)

forecasts <- expand.grid(
  sigma = 1:20/10, 
  quantile = seq(0.01, 0.99, 0.01)
)

forecasts <- forecasts |>
  as_tibble() |>
  mutate(model = 10 * sigma, 
         prediction = exp(qnorm(quantile, sd = sigma)), 
         true_value = list(Y), 
         sample_id = list(1:length(Y))) |>
  unnest(c(true_value, sample_id))

forecasts <- forecasts |>
  mutate(scale = "natural") |>
  rbind(forecasts |>
          mutate(prediction = log(prediction + 1), 
                 true_value = log(true_value + 1), 
                 scale = "log"))

scores <- score(forecasts)

summary <- scores |>
  mutate(log_wis = log(interval_score +1)) |>
  group_by(sigma, scale) |>
  summarise(wis = mean(interval_score), 
            log_wis = mean(log_wis)) |>
  mutate(log_wis = ifelse(scale == "log", log_wis, NA)) |>
  pivot_wider(values_from = wis, names_from = scale) |>
  group_by(sigma) |>
  summarise(log_wis = sum(log_wis, na.rm = TRUE), 
            wis_log = sum(log, na.rm = TRUE), 
            wis = sum(natural, na.rm = TRUE)) |>
  pivot_longer(cols = c(wis, log_wis, wis_log), values_to = "score", names_to = "type") |>
  mutate(type = factor(type, 
                       levels = c("wis", "wis_log", "log_wis"), 
                       labels = c("CRPS", "CRPS (log scale)", "log(CRPS)")))


score_plot <- function(summary) {
  summary |>
    ggplot(aes(x = sigma, y = score)) +
    geom_vline(xintercept = 1, linetype = "dashed", size = 0.3, color = "grey80") +
    geom_line() + 
    facet_wrap(~type, scales = "free_y") + 
    theme_scoringutils() +
    labs(y = "Expected score", x = TeX("$\\sigma$")) + 
    theme(panel.spacing = unit(1, "lines"))
}

summary |>
  group_by(type) |>
  filter(score == min(score))

score_plot(summary)

ggsave("output/figures/example-log-first.png", width = 7, height = 2.1)



## ========================================================================== ##
## Figure 4: Illustration of the effect of adding an offset to the logarithm
## ========================================================================== ##

x <- seq(0.05, 100, 0.05)
a <- c(0.1, 1, 10)

plot_df <- expand.grid(
  x = x, 
  a = c(0, a)
) |>
  mutate(y = log(x + a))

cols <- c("0"= "#000000", 
          "0.1" = "#E69F00", 
          "1" = "#56B4E9", 
          "10" = "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot_df |>
  ggplot(aes(x = x, y = y, color = factor(a), group = a)) +
  geom_vline(xintercept = 5 * 0.1, 
             color = "#E69F00",
             linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 5 * 1, 
             color = "#56B4E9",
             linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 5 * 10, 
             color = "#009E73",
             linetype = "dashed", alpha = 0.4) +
  geom_line() + 
  theme_scoringutils() + 
  scale_x_continuous(trans = "log10", breaks = c(0.05, 0.5, 5, 50), labels = label_fn) + 
  scale_y_continuous(labels = label_fn) + 
  scale_colour_manual(values = cols) + 
  labs(y = "log (x + a)", color = "a")

ggsave(filename = "output/figures/illustration-effect-offset-log.png", height = 2.5, width = 7)



## ========================================================================== ##
## FIGURE 5: Illustration of the effect of the log-transformation of 
##           the ranking for a single forecast
## ========================================================================== ##

# CRPS values were approximated by a WIS with 200 quantiles
quantile <- seq(0.005, 0.995, 0.005)
mu <- 10
theta_1 <- 1
theta_2 <- 1e9

data <- data.table(
  quantile = quantile,
  model_A = qnbinom(p = quantile, size = theta_1, mu = mu), 
  model_B = qnbinom(p = quantile, size = theta_2, mu = mu)
) |>
  pivot_longer(cols = c(model_A, model_B), names_to = "model", values_to = "prediction") |>
  mutate(true_value = list(1:50), 
         id = list(1:50)) |>
  unnest(cols = c(true_value, id))

scores_natural <- data |>
  score(metrics = "interval_score") |>
  summarise_scores(by = c("model", "id")) |>
  mutate(scale = "natural")

scores_log <- data |>
  mutate(true_value = log(true_value + 1), 
         prediction = log(prediction + 1)) |>
  score(metrics = "interval_score") |>
  summarise_scores(by = c("model", "id")) |>
  mutate(scale = "log")

scores_sqrt <- data |>
  mutate(true_value = sqrt(true_value), 
         prediction = sqrt(prediction)) |>
  score(metrics = "interval_score") |>
  summarise_scores(by = c("model", "id"), na.rm = TRUE) |>
  mutate(scale = "sqrt")

scores <- rbind(scores_natural, scores_log, scores_sqrt) |>
  mutate(scale = factor(scale, levels = c("natural", "log", "sqrt")), 
         Forecaster = ifelse(model == "model_A", "A", "B"))

nbinom_natural <- data.table(
  A = rnbinom(100000, mu = 10, size = theta_1), 
  B = rnbinom(100000, mu = 10, size = theta_2)
) |>
  pivot_longer(cols = c(A, B), names_to = "Forecaster") 

plot_fct <- function(scores, scale_factor, nbinom, filter_scale = "natural") {
  
  scores <- filter(scores, scale == filter_scale)
  
  scores |>
    ggplot(aes(x = id, y = interval_score / scale_factor, color = Forecaster)) + 
    geom_histogram(data = filter(nbinom, Forecaster == "A"), inherit.aes = FALSE,
                   aes(y = after_stat(density),
                       x = value, color = NULL,
                       fill = Forecaster),
                   alpha = 0.2,
                   binwidth = 1) +
    geom_histogram(data = filter(nbinom, Forecaster == "B"), inherit.aes = FALSE,
                   aes(y = after_stat(density),
                       x = value, color = NULL,
                       fill = Forecaster),
                   alpha = 0.2,
                   binwidth = 1) +
    geom_line() +
    theme_scoringutils() + 
    scale_y_continuous(label = function(x) {paste(scale_factor * x)}) + 
    labs(y = "CRPS", x = "Observed value") + 
    coord_cartesian(xlim = c(0, 30)) +
    scale_colour_discrete(
      labels=c(TeX(r'(A: $\sigma^2 = \mu + \mu^2$)'), 
               TeX(r'(B: $\sigma^2 = \mu$)'))
    ) + 
    scale_fill_discrete(
      labels=c(TeX(r'(A: $\sigma^2 = \mu + \mu^2$)'), 
               TeX(r'(B: $\sigma^2 = \mu$)'))
    ) + 
    facet_wrap(~ scale)
}

p1 <- scores |>
  plot_fct(scale_factor = 253, nbinom = nbinom_natural, filter_scale = "natural")

p2 <- scores |>
  plot_fct(scale_factor = 10, nbinom = nbinom_natural, filter_scale = "log") 

p3 <- scores |>
  plot_fct(scale_factor = 23, nbinom = nbinom_natural, filter_scale = "sqrt") 

p1 + p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(filename = "output/figures/illustration-effect-log-ranking-crps.png", 
       width = 7, height = 3)





## ========================================================================== ##
## Loading ECDC Forecast Hub Data
## ========================================================================== ##

analysis_date <- "2022-12-12"
start_date <- "2021-03-08"
end_date <- "2022-05-12"

hub_data <- rbindlist(list(
  fread(here("data", "full-data-european-forecast-hub-1.csv")), 
  fread(here("data", "full-data-european-forecast-hub-2.csv")),
  fread(here("data", "full-data-european-forecast-hub-3.csv")), 
  fread(here("data", "full-data-european-forecast-hub-4.csv"))
)) |>
  unique() |>
  filter(location != "")

scores <- fread(here("output", "data", "all-scores-european-hub.csv")) 

scores <- scores |>
  mutate(scale = factor(scale, levels = c("natural", "log", "sqrt"))) |>
  mutate(type_and_scale = factor(type_and_scale, 
                                 levels = c("Cases - natural", "Deaths - natural", 
                                            "Cases - sqrt", "Deaths - sqrt", 
                                            "Cases - log", "Deaths - log")))

scores_log_alts <- fread(here("output", "data", "all-scores-european-hub-log-variants.csv")) 

scores_log_alts <- scores_log_alts |>
  rbind(scores, 
        fill = TRUE)

scores_log_alts[scale == "log", scale := "log + 1"]
scores_log_alts[, type_and_scale := paste0(target_type, " - ", scale)]
scores_log_alts <- scores_log_alts |>
  mutate(type_and_scale = factor(type_and_scale, 
                                 levels = c("Cases - natural", "Deaths - natural", 
                                            "Cases - sqrt", "Deaths - sqrt", 
                                            "Cases - log + 10x median", "Deaths - log + 10x median", 
                                            "Cases - log + 1", "Deaths - log + 1", 
                                            "Cases - log + 0.1", "Deaths - log + 0.1", 
                                            "Cases - log + 0.001", "Deaths - log + 0.001" 
                                 )))


# dates and number of locations
hub_data |>
  summarise(min_date = min(forecast_date), 
            max_date = max(forecast_date)) |>
  mutate(week_diff = (max_date - min_date - 1) / 7)

hub_data$location |> 
  unique() |>
  length()






## ========================================================================== ##
## Figure 6: Scores for two-week-ahead predictions from the 
## EuroCOVIDhub-ensemble madein Germany.
## ========================================================================== ##

label_fn <- function(x) {
  x <- ifelse(x%%1 == 0, 
              as.integer(x), x)
  ifelse(x < 1000, 
         paste(x), 
         ifelse(x < 1e6, 
                paste0(x / 1000, "k"),
                ifelse(x < 1e9, 
                       paste0(x / 1e6, "m"), 
                       paste0(x / 1e9, "b"))
         )
  )
}

plot_pred_score <- function(hub_data, scores, model,
                            horizon = 2, type = "Cases", 
                            remove_x_text = TRUE) {
  forecast_model <- model
  h <- horizon
  filtered_hub <- hub_data |>
    filter(model == forecast_model, 
           horizon %in% h, 
           target_type == type) |>
    mutate(target_end_date = as.Date(target_end_date))
  
  locations <- unique(filtered_hub$location) 
  
  filtered_scores <- scores |>
    filter(model == forecast_model, 
           horizon %in% h, 
           location %in% locations,
           target_type == type) 
  
  plot_natural  <- filtered_hub |>  
    mutate(id = paste(type, "- natural")) |>
    plot_predictions(x = "target_end_date") +
    scale_y_continuous(labels = label_fn)  +
    facet_wrap(~ id) +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Predictions")
  
  plot_log <- filtered_hub |>
    mutate(prediction = log(prediction + 1), 
           true_value = log(true_value + 1)) |>
    mutate(id = paste(type, "- log")) |>
    plot_predictions(x = "target_end_date") +
    scale_y_continuous(labels = label_fn)  +
    facet_wrap(~ id) +
    theme(legend.position = "bottom", 
          axis.text.x = element_blank(), 
          axis.title.x = element_blank()) +
    labs(y = "Predictions")
  
  scores_natural <- filtered_scores |>
    filter(scale == "natural") |>
    summarise_scores("target_end_date") |>
    select(-interval_score) |>
    data.table::melt(measure.vars = c(
      "overprediction",
      "underprediction",
      "dispersion"
    ),
    variable.name = "wis_component_name",
    value.name = "component_value"
    ) |>
    mutate(target_end_date = as.Date(target_end_date)) |>
    head(2000) |>
    ggplot(aes(x = target_end_date)) +
    geom_col(
      position = "stack",
      aes(y = component_value, fill = wis_component_name)
    ) +
    theme_scoringutils() +
    scale_fill_discrete(type = c("#DF536B", "#61D04F", "#2297E6")) +
    scale_y_continuous(labels = label_fn) + 
    guides(fill = guide_legend(title = "WIS component")) +
    labs(y = "WIS contributions", x = "Target end date") 
  
  scores_log <- filtered_scores |>
    filter(scale == "log") |>
    summarise_scores("target_end_date") |>
    select(-interval_score) |>
    data.table::melt(measure.vars = c(
      "overprediction",
      "underprediction",
      "dispersion"
    ),
    variable.name = "wis_component_name",
    value.name = "component_value"
    ) |>
    mutate(target_end_date = as.Date(target_end_date)) |>
    head(2000) |>
    ggplot(aes(x = target_end_date)) +
    geom_col(
      position = "stack",
      aes(y = component_value, fill = wis_component_name)
    ) +
    theme_scoringutils() +
    scale_fill_discrete(type = c("#DF536B", "#61D04F", "#2297E6")) +
    guides(fill = guide_legend(title = "WIS component")) +
    labs(y = "WIS contributions", x = "Target end date") 
  
  if (remove_x_text) {
    scores_natural <- scores_natural +
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank())
  }
  
  list(plot_natural, scores_natural, plot_log, scores_log )
}


put_plot_together <- function(modelname = "EuroCOVIDhub-ensemble", 
                              locationname = "DE") {
  
  plot_cases <- plot_pred_score(hub_data |> filter(location == locationname), 
                                scores, modelname, 
                                remove_x_text = TRUE, 
                                type = "Cases")
  
  plot_deaths <- plot_pred_score(hub_data |> filter(location == locationname), 
                                 scores, modelname, 
                                 type = "Deaths")
  
  layout <- "
AE
BF
CG
DH"
  
  plot_cases[[1]] + plot_cases[[2]] + plot_cases[[3]] +
    plot_cases[[4]] + plot_deaths[[1]] +
    plot_deaths[[2]] + plot_deaths[[3]] + plot_deaths[[4]] +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect", 
                design = layout) &
    theme(legend.position = "bottom")
}

put_plot_together("EuroCOVIDhub-ensemble")
ggsave(filename = "output/figures/HUB-model-comparison-ensemble.png", width = 10, height = 8.5)


## ========================================================================== ##
## SI variant of Figure 6 (Figure SI.4)
## ========================================================================== ##

put_plot_together("epiforecasts-EpiNow2")
ggsave(filename = "output/figures/HUB-model-comparison-epinow.png", width = 10, height = 8.5)

## ========================================================================== ##
## SI variant of Figure 6 (Figure SI.5)
## ========================================================================== ##

put_plot_together("EuroCOVIDhub-baseline", locationname = "DE")
ggsave(filename = "output/figures/HUB-model-comparison-baseline.png", width = 10, height = 8.5)






## ========================================================================== ##
## Figure 7: Observations and scores across locations and forecast 
##           horizons for the European COVID-19 Forecast Hub data
## ========================================================================== ##

label_fn_within <- function(x) {
  x <- gsub(pattern = "___Deaths", "", x)
  x <- gsub(pattern = "___Cases", "", x)
  paste(x)
}

mean_observations <- hub_data |>
  dplyr::select(location, target_type, target_end_date, true_value) |>
  unique() |>
  group_by(target_type, location) |>
  summarise(mean_value = mean(true_value), 
            sd_value = sd(true_value), 
            var_value = var(true_value))

plot_means_obs <- mean_observations |>
  ggplot(aes(y = mean_value, x = reorder_within(location, -mean_value, target_type))) +   geom_bar(stat = "identity", fill = "grey50") + 
  facet_wrap(~ target_type, scales = "free") + 
  theme_scoringutils() + 
  scale_y_continuous(labels = label_fn) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2), labels = label_fn_within) + 
  labs(y = "Mean observed value", x = "") +
  theme(axis.title.x = element_blank())


box_plot_obs <- hub_data |>
  mutate(true_value = pmax(true_value, 1)) |>
  dplyr::select(location, target_type, target_end_date, true_value) |>
  unique() |>
  ggplot(aes(y = true_value, x = target_type)) + 
  geom_violin(aes(fill = target_type), alpha = 0.2, color = NA) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = label_fn, trans = "log10") + 
  labs(y = "Observations", x = "")  +
  theme(axis.title.x = element_blank())

mean_scores_plot <- function(scores) {
  plot_df <- scores |> 
    filter(model == "EuroCOVIDhub-ensemble", 
           horizon == 2) |>
    group_by(target_type, location, scale, type_and_scale) |>
    summarise(interval_score = mean(interval_score), 
              .groups = "drop_last") |> 
    full_join(mean_observations)
  
  plot_mean_scores <- plot_df |>
    ggplot(aes(y = interval_score,
               x = reorder_within(location, -mean_value, target_type))) + 
    geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
    facet_wrap(~ type_and_scale, scale = "free", ncol = 2) + 
    theme_scoringutils() + 
    theme(legend.position = "none") + 
    scale_y_continuous(labels = label_fn) + 
    scale_x_discrete(guide = guide_axis(n.dodge=2), labels = label_fn_within) +
    labs(y = "Mean weighted interval score", x = "Loaction") 
  
  return(plot_mean_scores)
}

plot_mean_scores <- mean_scores_plot(filter(scores, scale %in% c("log", "natural")))

box_plot_scores <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2, 
         scale %in% c("log","natural")) |>
  ggplot(aes(y = interval_score, x = target_type)) + 
  geom_violin(aes(fill = target_type), alpha = 0.2, color = NA) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  facet_wrap(~ scale, scale = "free", nrow = 2) + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = label_fn, trans = "log10") + 
  labs(y = "Weighted interval score", x = "Target type")


fct = function(x, x2) {
  if(length(x2) == 0) {
    return(NA)
  } else {
    return(x / x2)
  }
}

box_plot_horizon <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon <= 4, 
         scale %in% c("log", "natural")) |>
  group_by(forecast_date, model, location, target_type, scale, type_and_scale) |>
  mutate(
    interval_score = fct(interval_score, interval_score[horizon ==1])
      ) |>
  ggplot(aes(y = interval_score,
             x = as.factor(horizon), 
             fill = scale)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "grey40") +
  geom_violin(aes(fill = scale), alpha = 0.2, color = NA) +
  geom_boxplot(alpha = 0.5, position = "dodge2") + 
  scale_fill_brewer(palette = "Set2", name = "Transformation") + 
  facet_wrap(~ target_type, nrow = 1) + 
  theme_scoringutils() + 
  theme(legend.position = "right") + 
  scale_y_continuous(labels = label_fn, trans = "log10") +
  labs(y = "Rel. change in WIS", x = "Forecast horizon (weeks)") + 
  coord_cartesian(ylim = c(0.1, 10))

box_plot_horizon <- box_plot_horizon + 
  theme(legend.position = "bottom") 

layout <- "
AB
CD
CD
EF"

plot_means_obs  + box_plot_obs + plot_mean_scores + box_plot_scores + box_plot_horizon + plot_spacer() +
  plot_layout(heights = c(1, 2, 1.4), 
              widths = c(3, 1)) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = "A")

ggsave("output/figures/HUB-mean-obs-location.png", width = 10, height = 10)


## ========================================================================== ##
## (Variant of Figure 7) Figure SI.6: Mean WIS in different locations 
## for different transformations applied before scoring
## ========================================================================== ##

mean_scores_plot(scores_log_alts)

ggsave("output/figures/HUB-scores-locations-log-variants.png", width = 8, height = 7.5)


## ========================================================================== ##
## Figure 8: Regression analysis
## ========================================================================== ##

# run regressions
regression <- function(scores, s = "natural", h = 1:4, t, a = 1) {
  
  data <- scores |>
    filter(scale == s, horizon %in% h, target_type %in% t) |>
    mutate(log_wis = log(interval_score))
  
  if (s == "natural") {
    out <- data |>
      filter(is.finite(log_wis)) %>%
      lm(log_wis ~ 1 + log(median_prediction + a), data = .)
  } else if (grepl("log", s)){
    out <- lm(interval_score ~ 1 + log(median_prediction + a), data = data)
  } else if (s == "sqrt") {
    out <- lm(interval_score ~ 1 + sqrt(median_prediction), data = data)
  } else if (s == "natural_alternative") {
    data <- scores |>
      filter(scale == "natural", horizon %in% h, target_type %in% t)
    out <- lm(interval_score ~ 1 + median_prediction, data = data)
  }
  
  return(out$coefficients)
}

regression_df <- function(scores, s = "natural", horizons = "all", targets = "all", a = 1) {
  if (horizons == "all") {
    h <- 1:4
  } else {
    h <- horizons
  }
  if (targets == "all") {
    t <- c("Cases", "Deaths")
  } else {
    t <- targets
  }
  df <- regression(scores, s, h, t, a) |> 
    t() |>
    as.data.frame()
  
  names(df) <- c("alpha", "beta")
  
  df <- mutate(
    df, 
    horizon = horizons, target_type = targets, scale = s
  )
  
  return(df)
}

# check scores for median = 10 and median = 1e6
reg_check_helper <- function(reg_sqrt, reg_log) {
  print(paste("10 - sqrt:", reg_sqrt[1] + reg_sqrt[2] * sqrt(10)))
  print(paste("1e5 - sqrt:", reg_sqrt[1] + reg_sqrt[2] * sqrt(1e5)))
  
  print(paste("10 - log:", reg_log[1] + reg_log[2] * log(10)))
  print(paste("1e5 - log:", reg_log[1] + reg_log[2] * log(1e5)))
}

reg_log <- regression_df(scores, s = "log", horizons = "2", targets = "Cases", a = 1)
reg_sqrt <- regression_df(scores, s = "sqrt", horizons = "2", targets = "Cases", a = 1)

reg_check_helper(reg_sqrt, reg_log)




scatter_wis_pred <- function(scores, filterscale, a = 1) {
  
  regs_coef <- rbind(
    regression_df(scores, s = filterscale, horizons = "2", targets = "Cases", a = a),  
    regression_df(scores, s = filterscale, horizons = "2", targets = "Deaths", a = a)
  ) |>
    mutate(type_and_scale = paste(target_type, scale, sep = " - ")) |>
    mutate(type_and_scale = factor(type_and_scale, 
                                   levels = c(paste0("Cases - ", filterscale),
                                              paste0("Deaths - ", filterscale))
                                   )
           )
  
  regs_coef <- regs_coef |> 
    mutate(alpha = signif(alpha, 2), 
           beta = signif(beta, 2)) |>
    mutate(label = ifelse(
      scale == "natural",
      paste0("\\hat{WIS} = e^", alpha, " \\cdot median^", beta), # regression for natural
      
      ifelse(grepl("log", scale), 
             paste0("\\hat{WIS} = ", alpha, " - ", -beta, "\\cdot \\log (median)"), # regression for log
             paste0("\\hat{WIS} = ", alpha, " + ", beta, "\\cdot \\sqrt{median}")) # regression for sqrt
    ))
  
  regs <- regs_coef |> 
    group_by(type_and_scale) |>
    mutate(median_prediction = 
             ifelse(target_type == "Deaths", 
                    list(1:51467), 
                    list(seq(1, max(scores$median_prediction), 100)))) |>
    unnest(cols = median_prediction) |>
    mutate(yreg = ifelse(
      scale == "natural", 
      exp(alpha) * median_prediction ^ beta, # regression for natural scale
      ifelse(grepl("log", scale), 
             alpha + beta * log(median_prediction + 1), # regression for log scale
             alpha + beta * sqrt(median_prediction) # regression for sqrt scale
      )
      
    ))
  
  scores |>
    filter(model == "EuroCOVIDhub-ensemble", 
           horizon == 2, 
           scale == filterscale) |>
    ggplot(aes(y = interval_score, x = median_prediction)) +
    geom_point(size = 0.1, color = "grey20", alpha = 0.4) + 
    geom_line(
      data = filter(regs, scale == filterscale), 
      aes(y = yreg), 
      color = "red") + 
    ggpp::geom_label_npc(
      data = filter(regs_coef, scale == filterscale), 
      size = 2.5,
      label.padding = unit(0.17, "lines"),
      label.r = unit(0, "lines"),
      aes(npcx = "left", npcy = "top", 
          label = TeX(label, output = "character")), 
      parse = TRUE) +
    facet_wrap(~ type_and_scale, scale = "free", ncol = 2) + 
    theme_scoringutils() + 
    theme(legend.position = "none") + 
    scale_x_continuous(trans = "log10", labels = label_fn) +
    scale_y_continuous(trans = "log10", labels = label_fn) +
    labs(y = "WIS", x = "Median predicted value") +
    plot_annotation()
}

p_natural <- scatter_wis_pred(scores, filterscale = "natural")
p_log <- scatter_wis_pred(scores, filterscale = "log")
p_sqrt <- scatter_wis_pred(scores, filterscale = "sqrt")

p_natural / p_sqrt / p_log +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-transformation-regression.png", width = 7, height = 6.3)


## ========================================================================== ##
## Table 1: Regression for the relationship of mean and var
## ========================================================================== ##

natural_reg <- regression_df(scores, s = "natural", horizons = "all")
log_reg <- regression_df(scores, s = "log", horizons = "all")
sqrt_reg <- regression_df(scores, s = "sqrt", horizons = "all")


natural_reg_target <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "all", targets = "Cases"),
  regression_df(scores, s = "natural", horizons = "all", targets = "Deaths")
)) 

log_reg_target <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "all", targets = "Cases"),
  regression_df(scores, s = "log", horizons = "all", targets = "Deaths")
)) 

sqrt_reg_target <- rbindlist(list(
  regression_df(scores, s = "sqrt", horizons = "all", targets = "Cases"),
  regression_df(scores, s = "sqrt", horizons = "all", targets = "Deaths")
)) 


natural_reg_horizon <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "1", targets = "all"),
  regression_df(scores, s = "natural", horizons = "2", targets = "all"), 
  regression_df(scores, s = "natural", horizons = "3", targets = "all"), 
  regression_df(scores, s = "natural", horizons = "4", targets = "all")
)) 

log_reg_horizon <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "1", targets = "all"),
  regression_df(scores, s = "log", horizons = "2", targets = "all"), 
  regression_df(scores, s = "log", horizons = "3", targets = "all"), 
  regression_df(scores, s = "log", horizons = "4", targets = "all")
)) 

sqrt_reg_horizon <- rbindlist(list(
  regression_df(scores, s = "sqrt", horizons = "1", targets = "all"),
  regression_df(scores, s = "sqrt", horizons = "2", targets = "all"), 
  regression_df(scores, s = "sqrt", horizons = "3", targets = "all"), 
  regression_df(scores, s = "sqrt", horizons = "4", targets = "all")
)) 


natural_reg_horizon_cases <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "1", targets = "Cases"),
  regression_df(scores, s = "natural", horizons = "2", targets = "Cases"), 
  regression_df(scores, s = "natural", horizons = "3", targets = "Cases"), 
  regression_df(scores, s = "natural", horizons = "4", targets = "Cases")
)) 

natural_reg_horizon_deaths <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "1", targets = "Deaths"),
  regression_df(scores, s = "natural", horizons = "2", targets = "Deaths"), 
  regression_df(scores, s = "natural", horizons = "3", targets = "Deaths"), 
  regression_df(scores, s = "natural", horizons = "4", targets = "Deaths")
)) 

log_reg_horizon_cases <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "1", targets = "Cases"),
  regression_df(scores, s = "log", horizons = "2", targets = "Cases"), 
  regression_df(scores, s = "log", horizons = "3", targets = "Cases"), 
  regression_df(scores, s = "log", horizons = "4", targets = "Cases")
)) 

log_reg_horizon_deaths <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "1", targets = "Deaths"),
  regression_df(scores, s = "log", horizons = "2", targets = "Deaths"), 
  regression_df(scores, s = "log", horizons = "3", targets = "Deaths"), 
  regression_df(scores, s = "log", horizons = "4", targets = "Deaths")
)) 

sqrt_reg_horizon_cases <- rbindlist(list(
  regression_df(scores, s = "sqrt", horizons = "1", targets = "Cases"),
  regression_df(scores, s = "sqrt", horizons = "2", targets = "Cases"), 
  regression_df(scores, s = "sqrt", horizons = "3", targets = "Cases"), 
  regression_df(scores, s = "sqrt", horizons = "4", targets = "Cases")
)) 

sqrt_reg_horizon_deaths <- rbindlist(list(
  regression_df(scores, s = "sqrt", horizons = "1", targets = "Deaths"),
  regression_df(scores, s = "sqrt", horizons = "2", targets = "Deaths"), 
  regression_df(scores, s = "sqrt", horizons = "3", targets = "Deaths"), 
  regression_df(scores, s = "sqrt", horizons = "4", targets = "Deaths")
)) 



df <-rbind(
  natural_reg, 
  sqrt_reg,
  log_reg, 
  natural_reg_target, 
  sqrt_reg_target,
  log_reg_target, 
  natural_reg_horizon, 
  sqrt_reg_horizon,
  log_reg_horizon,
  natural_reg_horizon_cases, 
  natural_reg_horizon_deaths, 
  sqrt_reg_horizon_cases, 
  sqrt_reg_horizon_deaths, 
  log_reg_horizon_cases, 
  log_reg_horizon_deaths
) |>
  pivot_wider(names_from = scale, values_from = c(alpha, beta)) |>
  select(horizon, target_type, alpha_natural, beta_natural, alpha_sqrt, beta_sqrt, alpha_log, beta_log) |>
  mutate(across(c(alpha_natural, beta_natural, alpha_log, beta_log, alpha_sqrt, beta_sqrt), round, 3))

linesep<-function(x,y=character()){
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'\\addlinespace',y))  
}

df |> 
  kable(format = "latex", 
        align = c("ccrrrrrr"),
        booktabs = TRUE,
        linesep = linesep(c(1, 2, 4, 4, 4, 4)),
        col.names = c("Horizon", 
                      "Target",
                      "$\\alpha$", 
                      "$\\beta$", 
                      "$\\alpha_\\sqrt{\\ }$", 
                      "$\\beta_\\sqrt{\\ }$", 
                      "$\\alpha_\\log{}$", 
                      "$\\beta_\\log{}$"), escape = FALSE) |>
  kable_styling()




## ========================================================================== ##
## Figure 8: Correlations for rankings on the natural and logarithmic scale
## ========================================================================== ##

df <- scores |>
  filter(scale %in% c("natural", "log")) |>
  select(c(model, target_type, horizon, interval_score,
           forecast_date, target_end_date, scale, location)) |>
  pivot_wider(names_from = scale, values_from = interval_score) |> 
  group_by(horizon, target_type, forecast_date, target_end_date, location) |>
  summarise(cor = cor(natural, log), 
            spearman = cor(natural, log, method = "spearman")) |>
  group_by(horizon, target_type) |>
  summarise(cor = mean(cor, na.rm = TRUE), 
            spearman = mean(cor, na.rm = TRUE)) |>
  rename(`Target type` = target_type)

p_cor_scores <- df |>
  ggplot(aes(x = horizon, y = cor, fill = `Target type`, color = `Target type` )) + 
  scale_fill_brewer(palette = "Set1", name = "Target type") + 
  scale_color_brewer(palette = "Set1", name = "Target type") +
  geom_histogram(stat = "identity", position = "dodge", alpha = 0.4) +
  theme_scoringutils() + 
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Avg. rank cor(scores)", x = "Forecast horizon") + 
  theme(axis.title.y = element_text(size=8.5))


# Correlation between pairwise comparisons
correlation_rel_skill <- scores |>
  filter(scale %in% c("log", "natural")) |>
  select(model, location, forecast_date, horizon, target_type, scale, interval_score) |>
  summarise_scores(
    by = c("model", "scale", "horizon", "target_type"), 
    relative_skill = TRUE) |>
  select(model, horizon, target_type, relative_skill, scale) |>
  pivot_wider(values_from = relative_skill, names_from = scale) |>
  group_by(horizon, target_type) |>
  summarise(cor = cor(natural, log), 
            spearman = cor(natural, log, method = "spearman")) 

p_cor_skill <- correlation_rel_skill |>
  rename(`Target type` = target_type) |>
  ggplot(aes(x = horizon, y = cor, fill = `Target type`, color = `Target type` )) + 
  scale_fill_brewer(palette = "Set1", name = "Target type") + 
  scale_color_brewer(palette = "Set1", name = "Target type") + 
  geom_histogram(stat = "identity", position = "dodge", alpha = 0.4) +
  theme_scoringutils() + 
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Cor(rel. skill)", x = "Forecast horizon")

p_cor_scores + p_cor_skill +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-correlations.png", width = 7, height = 2.6)




## ========================================================================== ##
## Figure 9 - Changes in model ratings as measured by relative skill
## ========================================================================== ##

simple_labels <- function(x) {
  ifelse(x%%1 == 0, 
    paste(as.integer(x)),
    paste(x))
}

ranking_figure <- function(target = "Cases") {
  summarised_pairwise <- scores |>
    filter(scale %in% c("log", "natural")) |>
    select(model, location, forecast_date, horizon, target_type, scale, interval_score) |>
    filter(target_type == target, 
           horizon == 2) |>
    summarise_scores(by = c("model", "scale"), relative_skill = TRUE)
  
  ranking_log <- 
    summarised_pairwise |>
    filter(scale == "log") |>
    arrange(-relative_skill) |>
    pull(model)
  
  plot_rel_skill_natural <- summarised_pairwise |>
    filter(scale == "natural") |>
    ggplot(aes(y = reorder(model, -relative_skill), x = relative_skill)) +
    geom_bar(stat = "identity") +
    theme_scoringutils() +
    facet_wrap(~scale) +
    labs(y = "Model", x = "Relative skill") +
    scale_x_continuous(labels = simple_labels, breaks = c(0, 0.5, 1))
  
  plot_rel_skill_log <- summarised_pairwise |>
    filter(scale == "log") |>
    ggplot(aes(y = reorder(model, -relative_skill), x = relative_skill)) +
    geom_bar(stat = "identity") +
    theme_scoringutils() +
    facet_wrap(~scale) +
    labs(y = "", x = "Relative skill") +
    theme(axis.title.y = element_blank()) +
    scale_x_continuous(labels = simple_labels)
  
  diffs <- summarised_pairwise |>
    select(model, relative_skill, scale) |>
    pivot_wider(names_from = scale, values_from = relative_skill) |>
    mutate(model = factor(model, levels = ranking_log), 
           difference = log - natural)
  
  plot_diffs <- diffs |>
    mutate(title = "difference") |>
    ggplot(aes(y = model, x = difference)) +
    geom_bar(stat = "identity") +
    theme_scoringutils() +
    labs(y = "Model", x = "\u0394 Relative skill") +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.line.y = element_blank()) +
    facet_wrap(~title) +
    scale_x_continuous(labels = simple_labels)
  
  plot_rel_wis <- scores |>
    filter(horizon == 2, 
           target_type == target, 
           scale %in% c("log", "natural")) |>
    mutate(scale = factor(scale, levels = c("natural", "log"))) |>
    mutate(model = factor(model, levels = ranking_log)) |>
    summarise_scores(by = c("model", "scale")) |>
    plot_wis(relative_contributions = TRUE) +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.line.y = element_blank()) +
    scale_x_continuous(labels = label_fn) +
    facet_wrap(~scale, scales = "free_x")
  
  
  df_ranking_change <- summarised_pairwise |>
    filter(scale %in% c("natural", "log")) |>
    select(model, scale, relative_skill) |>
    group_by(scale) |>
    mutate(scale = factor(scale, levels = c("natural", "log"))) |>
    mutate(rank = rank(-relative_skill), 
           x = ifelse(scale == "natural", 0.4, 2.8), 
           x_arrow = ifelse(scale == "natural", 1.7, 2.7)
    ) |>
    group_by(model) |>
    mutate(custom_color = ifelse(diff(relative_skill) > 0, "deteriorated", "not deteriorated")) |>
    arrange(scale) 

  print(df_ranking_change)  
  
  plot_ranking_change <- df_ranking_change |>
    ggplot(aes(y = rank, group = model, label = model, color = custom_color)) +
    geom_path(
      aes(x=x_arrow), 
      arrow = arrow(length = unit(0.09,"npc")), 
      lineend = "round", linejoin = "mitre",
      size=1, show.legend = FALSE) +
    coord_cartesian( 
      ylim = c(0.75, 7.25)) +
    theme_scoringutils() +
    theme(axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position="none")
  
  return(
    list(
      plot_ranking_change, plot_rel_skill_natural, plot_rel_skill_log, plot_diffs, plot_rel_wis
    )
  )
}

ranking_cases <- ranking_figure(target = "Cases")
ranking_deaths <- ranking_figure(target = "Deaths")

layout <- "
ABCDE
FGHIJ"

ranking_cases[[2]] + ranking_cases[[1]] + 
  ranking_cases[[3]] + ranking_cases[[4]] + ranking_cases[[5]] +
  ranking_deaths[[2]] + ranking_deaths[[1]] + 
  ranking_deaths[[3]] + ranking_deaths[[4]] + ranking_deaths[[5]] +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = layout, 
              guides = "collect", 
              widths = c(0.9, 0.9, 0.9, 1.4, 2.5)) &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-pairwise-comparisons.png", width = 11.5, height = 6)







## ========================================================================== ##
## Table SI.1: Summary statistics for observations and scores for forecasts 
## from the ECDC data set
## ========================================================================== ##

summary <- hub_data |>
  filter(horizon == 2) |>
  mutate(natural = true_value, 
         log = log(pmax(0, true_value) +1)) |>
  select(location, target_type, target_end_date, natural, log) |>
  pivot_longer(cols = c(natural, log), values_to = "true_value", names_to = "scale") |>
  unique() |>
  group_by(target_type, scale) |>
  summarise(mean = mean(true_value), 
            sd = sd(true_value), 
            var = var(true_value))

table_obs <- summary |>
  pivot_longer(cols = c(mean, sd, var), names_to = "measure") |>
  pivot_wider(values_from = value, names_from = scale) |>
  select(target_type, measure, natural, log) |>
  mutate(quantity = "Observations", 
         natural = round(natural), 
         log = round(log, 2)) |>
  select(target_type, quantity, measure, natural, log)

table_scores <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(target_type, scale) |>
  summarise(mean = mean(interval_score), 
            sd = sd(interval_score), 
            .groups = "drop_last") |>
  pivot_longer(cols = c(mean, sd), names_to = "measure") |>
  pivot_wider(values_from = value, names_from = scale) |>
  mutate(quantity = "WIS", 
         natural = round(natural), 
         log = round(log, 2)) |>
  select(target_type, quantity, measure, natural, log)

table <- rbind(table_obs, 
               table_scores) |>
  arrange(quantity, target_type, measure)

table |>
  kable(format = "latex", booktabs = TRUE,
        align = c("l", "l", "l", rep("c", 2))
  ) 



## ========================================================================== ##
## Figure SI.3 - Appendix Available Forecasts
## ========================================================================== ##

hub_data |> 
  filter(horizon == 2) |>
  avail_forecasts(by = c("target_end_date", "target_type", "model")) |>
  mutate(forecast_date = as.Date(target_end_date) - 12) |>
  plot_avail_forecasts(show_numbers = FALSE, make_x_factor = TRUE) +
  facet_grid(~ target_type) + 
  labs(y = "Model", x = "Forecast date") +
  scale_x_discrete(breaks = 
                     c(as.character(as.Date("2021-03-08") + 7* 8*(0:20))))

ggsave(filename = "output/figures/number-avail-forecasts.png", height = 4, width = 10)



## ========================================================================== ##
## Table SI.3: Erroneous forecasts
## ========================================================================== ##

data.table(
  "True value" = c(">0", ">10", ">50", "=0"), 
  "Median prediction" = c(">100x true value", ">20x true value", "<1/50x true value", ">100")
) |>
  kable(format = "latex", 
        align = c("cc"),
        booktabs = TRUE) |>
  kable_styling()

