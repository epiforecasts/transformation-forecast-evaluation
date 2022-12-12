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
## FIGURE 1
## ========================================================================== ##

y_hat <- 1
grid_y <- (20:500)/100

data <- data.table(
  APE = abs((y_hat - grid_y)/grid_y),
  RE = abs((y_hat - grid_y)/y_hat),
  SAPE = abs((y_hat - grid_y)/(grid_y/2 + y_hat/2)),
  `AE after log transformation` = abs(log(y_hat) - log(grid_y)),
  x = grid_y / y_hat
) |>
  pivot_longer(cols = c(APE, RE, SAPE, `AE after log transformation`), 
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

p1 <- plot_fct(data) +
  scale_x_continuous(breaks = c(1/4, 1, 2, 4), 
                     labels = label_fn_hat) + 
  scale_color_brewer(palette = "Set1")

p2 <- plot_fct(data) +
  scale_x_continuous(trans = "log10", breaks = c(1/4, 0.5, 1, 2, 4), 
                     labels = label_fn_hat) + 
  scale_color_brewer(palette = "Set1")

(p1 + p2) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("output/figures/different-relative-errors.png", width = 7, height = 3)



## ========================================================================== ##
## FIGURE 2
## ========================================================================== ##

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
  mutate(true_value = log(true_value), 
         prediction = log(prediction)) |>
  score(metrics = "interval_score") |>
  summarise_scores(by = c("model", "id"), na.rm = TRUE) |>
  mutate(scale = "log")

scores <- rbind(scores_natural, scores_log) |>
  mutate(scale = factor(scale, levels = c("natural", "log")), 
         Forecaster = ifelse(model == "model_A", "A", "B"))

nbinom_natural <- data.table(
  A = rnbinom(100000, mu = 10, size = theta_1), 
  B = rnbinom(100000, mu = 10, size = theta_2)
) |>
  pivot_longer(cols = c(A, B), names_to = "Forecaster") |>
  mutate(scale = "natural")

nbinom_log <- nbinom_natural |>
  mutate(value = log(value), 
         scale = "log")

nbinom <- rbind(nbinom_natural, nbinom_log)

nbinom <- filter(nbinom, scale == "natural") |> 
  select(-scale)

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
    labs(y = "CRPS / WIS", x = "Observed value") + 
    coord_cartesian(xlim = c(0, 30)) +
    scale_colour_discrete(
      labels=c(TeX(r'(A: $\sigma^2 = \mu + \mu^2$)'), 
               TeX(r'(B: $\sigma^2 = \mu$)'))
    ) + 
    scale_fill_discrete(
      labels=c(TeX(r'(A: $\sigma^2 = \mu + \mu^2$)'), 
               TeX(r'(B: $\sigma^2 = \mu$)'))
    )
}

p1 <- scores |>
  plot_fct(scale_factor = 240, nbinom = nbinom, filter_scale = "natural") + 
  ylab("CRPS / WIS (natural scale)")

p2 <- scores |>
  plot_fct(scale_factor = 13, nbinom = nbinom, filter_scale = "log") + 
  ylab("CRPS / WIS (log scale)")  

p1 + p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(filename = "output/figures/illustration-effect-log-ranking-crps.png", 
       width = 7, height = 3)





## ========================================================================== ##
## FIGURE 3
## ========================================================================== ##
if (file.exists("output/data/simulation-negative-binom.Rda")) {
  scores_fig_3 <- readRDS(file = "output/data/simulation-negative-binom.Rda")
} else {
  
  time_points <- 1e5
  n_means <- 60
  means = (10^seq(1, 3.31, length.out = n_means))
  model_names <- c("theta_1000_sqrt_mu", "theta_1", "theta_1e9")
  
  df <- data.table(
    state = 1:length(mean),
    mean = means
  ) |>
    mutate(date = list(1:time_points)) |>
    unnest(cols = date) |>
    rowwise() |>
    mutate(theta = list(c(1000/sqrt(mean), 10, 1e9)), 
           model = list(model_names)) |>
    ungroup() |>
    unnest(cols = c("theta", "model")) |>
    mutate(predictive_sample = rnbinom(n = time_points * n_means * length(model_names), 
                                        size = theta,
                                        mu = mean) + 1) 
  
  scores_fig_3 <- df |>
    group_by(mean, theta, model) |> 
    summarise(
      crps = mean(abs(predictive_sample[-1] - predictive_sample[-length(predictive_sample)]))/2,
      crps_log = mean(abs(log(predictive_sample[-1]) - log(predictive_sample[-length(predictive_sample)])))/2
    ) |>
    pivot_longer(cols = c(crps, crps_log), values_to = "crps", names_to = "scale") |>
    mutate(scale = ifelse(scale == "crps", "natural", "log"), 
           scale = factor(scale, levels = c("natural", "log"))) |> 
    mutate(var = mean + mean^2 / theta, 
           approximation = ifelse(scale == "natural", 
                                  sqrt(var / pi),  
                                  sqrt(var / pi) / (mean)))
  
  saveRDS(scores_fig_3, file = "output/data/simulation-negative-binom.Rda")
}

p1 <- scores_fig_3 |>
  group_by(model, scale) |>
  filter(mean <= 2000) |>
  mutate(crps = crps / mean(crps), 
         approximation = approximation / mean(approximation)) |>
  ggplot(aes(y = crps, x = mean, colour = model)) +
  geom_line(aes(y = approximation)) +
  geom_point(size = 0.4) +  
  theme_scoringutils() + 
  theme(axis.title.y = element_text(size=7.1)) +
  labs(x = TeX("\\mu")) +
  scale_colour_discrete(
    labels=c(TeX(r'($\sigma^2 = \mu + \mu^2$)'), 
             TeX(r'($\sigma^2 = \mu + \frac{\mu^{2.5}}{1000}$)'), 
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
  labs(y = TeX("Mean CRPS (normalised)"))

ggsave("output/figures/SIM-mean-state-size.png", width = 7, height = 4.1)


## ========================================================================== ##
## FIGURE 3 - Appendix version
## ========================================================================== ##

scores_fig_3 |>
  ungroup() |>
  mutate(
    model = factor(model, 
                   labels = c(TeX(r'($\sigma^2 = \mu + \mu^2$)'), 
                              TeX(r'($\sigma^2 = \mu + \mu^{2.5}/1000$)'), 
                              TeX(r'($\sigma^2 = \mu$)')))
  ) |>
  ggplot(aes(y = crps, x = mean, color = model)) +
  geom_line(aes(y = approximation)) +
  geom_point(aes(color = model), alpha = 0.3) + 
  facet_wrap(scale ~ model, scales = "free", labeller = label_parsed) + 
  theme_scoringutils() + 
  scale_colour_discrete(
    labels=c(TeX(r'($\sigma^2 = \mu + 0.1 \cdot \mu^2$)'), 
             TeX(r'($\sigma^2 = \mu + \frac{\mu^{2.5}}{1000}$)'), 
             TeX(r'($\sigma^2 = \mu$)'))
  ) + 
  scale_x_continuous(trans = "log10") 

ggsave("output/figures/SIM-score-approximation.png", width = 7, height = 4.1)



## ========================================================================== ##
## FIGURE 4
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
## Loading ECDC Forecast Hub Data
## ========================================================================== ##

start_date <- "2021-03-08"
end_date <- "2021-10-18"

hub_data <- rbindlist(list(
  fread(here("data", "full-data-european-forecast-hub-1.csv")), 
  fread(here("data", "full-data-european-forecast-hub-2.csv")),
  fread(here("data", "full-data-european-forecast-hub-3.csv"))
)) |>
  unique()

filter_out_anomalies <- function(hub_data) {
  anomalies_file <- here::here("data", "anomalies.csv")
  if (file.exists(anomalies_file)) {
    anomalies <- data.table::fread(file = anomalies_file)
  } else {
    ## get anomalies file at date of last data made
    owner <- "covid19-forecast-hub-europe"
    repo <- "covid19-forecast-hub-europe"
    path <- "data-truth/anomalies/anomalies.csv"
    commit <- gh::gh(
      "/repos/{owner}/{repo}/commits?path={path}&until={date}",
      owner = owner,
      repo = repo,
      path = path,
      until = max(hub_data$target_end_date),
      .limit = 1
    )
    anomalies <- data.table::fread(input = URLencode(URL = paste(
      "https://raw.githubusercontent.com", owner, repo, commit[[1]]$sha, path, sep = "/"
    )))
    data.table::fwrite(x = anomalies, file = anomalies_file)
  }
  
  ## prepare for mergeing into hub_data
  anomalies <- anomalies[, list(
    target_end_date,
    location,
    target_type = paste0(stringr::str_to_title(stringr::str_remove(
      string = target_variable, pattern = "^inc ")), "s")
  )]
  
  hub_data <- hub_data[!anomalies, on = .(target_end_date, location, target_type)]
  hub_data <- hub_data[true_value > 0][]
  return(hub_data)
}
hub_data <- filter_out_anomalies(hub_data)

filter_models <- function(hub_data) {
  check <- hub_data |> 
    check_forecasts()
  
  filtermodels <- check$unique_values |>
    filter(horizon >= 4, 
           target_type == 2,
           true_value > 900,
           quantile == 23, 
           forecast_date > 16,
           location == 32) |>
    pull(model)
  
  remove <- hub_data |> 
    group_by(model, forecast_date, location, target_type) |>
    summarise(q = length(unique(quantile))) |>
    filter(q < 23) |>
    pull(model) |>
    unique() 
  
  filtermodels <- setdiff(filtermodels, remove)
  
  hub_data |>
    filter(model %in% filtermodels)
}
hub_data <- filter_models(hub_data)

hub_data <- filter(hub_data, 
                   forecast_date >= (as.Date(start_date) - 3))

get_scores <- function(hub_data) {
  if (file.exists(here("output", "data", "all-scores-european-hub.csv"))) {
    scores <- fread(here("output", "data", "all-scores-european-hub.csv"))
    return(scores)
  } else {
    scores <- hub_data |>
      mutate(scale = "natural") |>
      rbind(hub_data |>
              mutate(
                scale = "log", 
                true_value = log(true_value + 1), 
                prediction = log(pmax(prediction, 0) + 1)
              )) |>
      score(metrics = c("interval_score")) |>
      summarise_scores(by = c("model", "location",
                              "target_end_date", "forecast_date",
                              "horizon", "target_type", "scale"), 
                       na.rm = TRUE)
    
    scores[, type_and_scale := paste0(target_type, " - ", scale)]
    scores[, type_and_scale := factor(type_and_scale, 
                                      levels = c("Cases - natural", "Deaths - natural",
                                                 "Cases - log", "Deaths - log"))]
    scores[, scale := factor(scale, levels = c("natural", "log"))]
    fwrite(scores, here("output", "data", "all-scores-european-hub.csv"))
    return(scores)
  }
}
scores <- get_scores(hub_data)

scores <- scores[, type_and_scale := factor(type_and_scale, 
                                            levels = c("Cases - natural", "Cases - log", 
                                                       "Deaths - natural", "Deaths - log"))]
scores <- scores[, scale := factor(scale, 
                                   levels = c("natural", "log"))]
scores <- scores[horizon < 5]

add_median_forecast <- function(scores, hub_data) {
  medians <- hub_data |>
    filter(quantile == 0.5) |> 
    rename(median_prediction = prediction) |>
    select(-quantile)
  
  out <- scores |>
    inner_join(medians)
  
  return(out)
}
scores <- add_median_forecast(scores, hub_data)


# dates and number of locations
hub_data |>
  summarise(min_date = min(forecast_date), 
            max_date = max(forecast_date)) |>
  mutate(week_diff = (max_date - min_date - 1) / 7)

hub_data$location |> 
  unique() |>
  length()





## ========================================================================== ##
## Figure 5
## ========================================================================== ##

label_fn <- function(x) {
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
  facet_wrap(~ type_and_scale, scale = "free") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = label_fn) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2), labels = label_fn_within) +
  labs(y = "Mean interval score", x = "Loaction") 




box_plot_scores <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  ggplot(aes(y = interval_score, x = target_type)) + 
  geom_violin(aes(fill = target_type), alpha = 0.2, color = NA) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  facet_wrap(~ scale, scale = "free", nrow = 2) + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = label_fn, trans = "log10") + 
  labs(y = "Interval score", x = "Target type")


fct = function(x, x2) {
  if(length(x2) == 0) {
    return(NA)
  } else {
    return(x / x2)
  }
}

box_plot_horizon <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon <= 4) |>
  group_by(forecast_date, model, location, target_type, scale, type_and_scale) |>
  mutate(
    interval_score = fct(interval_score, interval_score[horizon ==1])
      ) |>
  ggplot(aes(y = interval_score,
             x = as.factor(horizon), fill = target_type)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "grey40") +
  geom_violin(aes(fill = target_type), alpha = 0.2, color = NA) +
  geom_boxplot(alpha = 0.5, position = "dodge2") + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  facet_wrap(~ scale, nrow = 1) + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = label_fn, trans = "log10") +
  labs(y = "Rel. change in WIS", x = "Forecast horizon (weeks)")

layout <- "
AB
CD
CD
EF"

plot_means_obs  + box_plot_obs + plot_mean_scores + box_plot_scores + box_plot_horizon + plot_spacer() +
  plot_layout(heights = c(1, 2, 1), 
              widths = c(3, 1)) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = "A")

ggsave("output/figures/HUB-mean-obs-location.png", width = 10, height = 10)





## ========================================================================== ##
## Figure 6
## ========================================================================== ##


scatter_wis_pred <- scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  ggplot(aes(y = interval_score, x = median_prediction)) +
  geom_point(size = 0.1, color = "grey20") + 
  facet_wrap(~ type_and_scale, scale = "free") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_x_continuous(trans = "log10", labels = label_fn) +
  scale_y_continuous(trans = "log10", labels = label_fn) +
  labs(y = "WIS", x = "Median predicted value") +
  ggpubr::stat_cor(method = "spearman", p.accuracy = 0.001, 
                   cor.coef.name = "rho", size = 3)

df <- scores |>
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


(scatter_wis_pred) / (p_cor_scores + p_cor_skill) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1, 1), heights = c(3, 1)) &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-correlations.png", width = 7, height = 7)



## ========================================================================== ##
## Figure 7 - Example comparisons of forecasts on the log and the natural scale
## ========================================================================== ##

plot_pred_score <- function(hub_data, scores, model,
                            horizon = 2, type = "Cases", 
                            remove_x_text = TRUE) {
  forecast_model <- model
  h <- horizon
  filtered_hub <- hub_data |>
    filter(model == forecast_model, 
           horizon %in% h, 
           target_type == type) 
  
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
    mutate(target_end_date = factor(target_end_date)) |>
    plot_wis(x = "target_end_date", flip = TRUE) +
    scale_x_continuous(labels = label_fn) +
    labs(y = "Target end date")
  
  scores_log <- filtered_scores |>
    filter(scale == "log") |>
    summarise_scores("target_end_date") |>
    mutate(target_end_date = factor(target_end_date)) |>
    plot_wis(x = "target_end_date", flip = TRUE) +
    labs(y = "Target end date")
  
  if (remove_x_text) {
    scores_natural <- scores_natural +
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank())
    # scores_log <- scores_log +
    #   theme(axis.text.x = element_blank(), 
    #         axis.title.x = element_blank())
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

put_plot_together("epiforecasts-EpiNow2")
ggsave(filename = "output/figures/HUB-model-comparison-epinow.png", width = 10, height = 8.5)

put_plot_together("EuroCOVIDhub-baseline", locationname = "DE")
ggsave(filename = "output/figures/HUB-model-comparison-baseline.png", width = 10, height = 8.5)


## ========================================================================== ##
## Figure 8 - Look at pairwise comparisons and changes in rankings (plus Appendix figures)
## ========================================================================== ##

ranking_figure <- function(target = "Cases") {
  summarised_pairwise <- scores |>
    select(-median_prediction) |>
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
    labs(y = "Model", x = "Relative skill")
  
  plot_rel_skill_log <- summarised_pairwise |>
    filter(scale == "log") |>
    ggplot(aes(y = reorder(model, -relative_skill), x = relative_skill)) +
    geom_bar(stat = "identity") +
    theme_scoringutils() +
    facet_wrap(~scale) +
    labs(y = "", x = "Relative skill") +
    theme(axis.title.y = element_blank())
  
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
    facet_wrap(~title)
  
  plot_rel_wis <- scores |>
    filter(horizon == 2, 
           target_type == "Cases") |>
    mutate(model = factor(model, levels = ranking_log)) |>
    summarise_scores(by = c("model", "scale")) |>
    plot_wis(relative_contributions = TRUE) +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.line.y = element_blank()) +
    scale_x_continuous(labels = label_fn) +
    facet_wrap(~scale, scales = "free_x")
  
  plot_ranking_change <- summarised_pairwise |>
    select(model, scale, relative_skill) |>
    group_by(scale) |>
    mutate(rank = rank(-relative_skill), 
           x = ifelse(scale == "natural", 0.4, 2.8), 
           x_arrow = ifelse(scale == "natural", 1.7, 2.7)
    ) |>
    group_by(model) |>
    mutate(color = ifelse(diff(relative_skill) > 0, "deteriorated", "not deteriorated")) |>
    ggplot(aes(y = rank, x = x, group = model, label = model)) +
    geom_path(
      aes(x=x_arrow, color = color), 
      arrow = arrow(length = unit(0.09,"npc")), 
      lineend = "round", linejoin = "mitre",
      size=1, show.legend = FALSE) +
    coord_cartesian( 
      ylim = c(0.75, 8.25)) +
    theme_scoringutils() +
    theme(axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position="none")
  
  plot_ranking_change

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
              widths = c(1, 1, 1, 1, 2.5)) &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-pairwise-comparisons.png", width = 10.5, height = 6)







## ========================================================================== ##
## Table 1 - Appendix
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
## Figure 9 - Appendix Available Forecasts
## ========================================================================== ##

hub_data |> 
  filter(horizon == 2) |>
  avail_forecasts(by = c("target_end_date", "target_type", "model")) |>
  mutate(forecast_date = as.Date(target_end_date) - 12) |>
  plot_avail_forecasts(show_numbers = FALSE) +
  facet_grid(~ target_type) + 
  labs(y = "Model", x = "Forecast date") 

ggsave(filename = "output/figures/number-avail-forecasts.png", height = 4, width = 10)



## ========================================================================== ##
## Figure 10/11: see Figure 7 
## ========================================================================== ##



## ========================================================================== ##
## Table 2: Regression for the relationship of mean and var
## ========================================================================== ##

regression <- function(scores, s = "natural", h = 1:4, t) {
  
  data <- scores |>
    filter(scale == s, horizon %in% h, target_type %in% t) |>
    mutate(log_wis = log(interval_score))
  
  
  if (s == "natural") {
    out <- data |>
      filter(is.finite(log_wis)) %>%
      lm(log_wis ~ 1 + log(median_prediction + 1), data = .)
  } else {
    out <- lm(interval_score ~ 1 + log(median_prediction + 1), data = data)
  }
  return(out$coefficients)
}

regression_df <- function(scores, s = "natural", horizons = "all", targets = "all") {
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
  df <- regression(scores, s, h, t) |> 
    t() |>
    as.data.frame()
  
  names(df) <- c("alpha", "beta")
  
  df <- mutate(
    df, 
    horizon = horizons, target_type = targets, scale = s
  )
  
  return(df)
}

natural_reg <- regression_df(scores, s = "natural", horizons = "all")
log_reg <- regression_df(scores, s = "log", horizons = "all")

log_reg_target <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "all", targets = "Cases"),
  regression_df(scores, s = "log", horizons = "all", targets = "Deaths")
)) 

natural_reg_target <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "all", targets = "Cases"),
  regression_df(scores, s = "natural", horizons = "all", targets = "Deaths")
)) 

log_reg_horizon <- rbindlist(list(
  regression_df(scores, s = "log", horizons = "1", targets = "all"),
  regression_df(scores, s = "log", horizons = "2", targets = "all"), 
  regression_df(scores, s = "log", horizons = "3", targets = "all"), 
  regression_df(scores, s = "log", horizons = "4", targets = "all")
)) 

natural_reg_horizon <- rbindlist(list(
  regression_df(scores, s = "natural", horizons = "1", targets = "all"),
  regression_df(scores, s = "natural", horizons = "2", targets = "all"), 
  regression_df(scores, s = "natural", horizons = "3", targets = "all"), 
  regression_df(scores, s = "natural", horizons = "4", targets = "all")
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


df <-rbind(
  natural_reg, 
  log_reg, 
  natural_reg_target, 
  log_reg_target, 
  natural_reg_horizon, 
  log_reg_horizon, 
  natural_reg_horizon_cases, 
  natural_reg_horizon_deaths, 
  log_reg_horizon_cases, 
  log_reg_horizon_deaths
) |>
  pivot_wider(names_from = scale, values_from = c(alpha, beta)) |>
  select(horizon, target_type, alpha_natural, beta_natural, alpha_log, beta_log) |>
  mutate(across(c(alpha_natural, beta_natural, alpha_log, beta_log), round, 3))

linesep<-function(x,y=character()){
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'\\addlinespace',y))  
}

df |> 
  kable(format = "latex", 
        align = c("ccrrrr"),
        booktabs = TRUE,
        linesep = linesep(c(1, 2, 4, 4, 4, 4)),
        col.names = c("Horizon", 
                      "Target",
                      "$\\alpha$", 
                      "$\\beta$", 
                      "$\\alpha^*$", 
                      "$\\beta^*$"), escape = FALSE) |>
  kable_styling()

