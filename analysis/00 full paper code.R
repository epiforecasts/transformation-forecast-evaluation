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

recompute_scores <- FALSE

# situations with negative values: France 2021-05-22, CZ 2021-08-07, ES 2021-03-06

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

label_fn <- function(x) {
  ifelse(x%%1 == 0, x, 
         paste0("1/", 1/x))
}

p1 <- plot_fct(data) +
  scale_x_continuous(breaks = c(1/4, 0.5, 1, 2, 4), 
                     labels = label_fn) + 
  scale_color_brewer(palette = "Set1")

p2 <- plot_fct(data) +
  scale_x_continuous(trans = "log10", breaks = c(1/4, 0.5, 1, 2, 4), 
                     labels = label_fn) + 
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
theta_1 <- 0.5
theta_2 <- 10

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

nbinom_log <- nbinom |>
  mutate(value = log(value), 
         scale = "log")

nbinom <- rbind(nbinom_natural, nbinom_log)

nbinom <- filter(nbinom, scale == "natural") |> 
  select(-scale)

plot_fct <- function(scores, scale_factor, nbinom, filter_scale = "natural") {
  
  scores <- filter(scores, scale == filter_scale)
  # nbinom <- filter(nbinom, scale == filter_scale)
  
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
    # facet_wrap(~ scale, scales = "free", ncol = 2) + 
    theme_scoringutils() + 
    scale_y_continuous(label = function(x) {paste(scale_factor * x)}) + 
    labs(y = "CRPS / WIS", x = "Observed value") + 
    coord_cartesian(xlim = c(0, 50))
}

p1 <- scores |>
  plot_fct(scale_factor = 200, nbinom = nbinom, filter_scale = "natural") + 
  ylab("CRPS / WIS (natural scale)")

p2 <- scores |>
  plot_fct(scale_factor = 11, nbinom = nbinom, filter_scale = "log") + 
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
  scores <- readRDS(file = "output/data/simulation-negative-binom.Rda")
} else {
  
  time_points <- 1e5
  n_means <- 60
  means = 100 * (10^seq(0, 1.2, length.out = n_means))
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
                                        mu = mean)) 
  
  scores <- df |>
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
  
  saveRDS(scores, file = "output/data/simulation-negative-binom.Rda")
}

p1 <- scores |>
  group_by(model, scale) |>
  filter(mean <= 1500) |>
  mutate(crps = crps / mean(crps), 
         approximation = approximation / mean(approximation)) |>
  ggplot(aes(y = crps, x = mean, colour = model)) +
  geom_line(aes(y = approximation)) +
  geom_point(size = 0.4) +  
  labs(y = "WIS", x = "Mean") +
  theme_scoringutils() + 
  scale_colour_discrete(
    labels=c(TeX(r'($\sigma^2 = \mu + 0.1 \cdot \mu^2$)'), 
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
  labs(y = "Normalised CRPS") 

ggsave("output/figures/SIM-mean-state-size.png", width = 7, height = 4.1)


## FIGURE 3 - Appendix version
## ========================================================================== ##

scores |>
  ungroup() |>
  mutate(
    model = factor(model, 
                   labels = c(TeX(r'($\sigma^2 = \mu + 0.1 \cdot \mu^2$)'), 
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

n_sim <- 1000
epsilon <- rnorm(n_sim)
Y <- exp(epsilon)

forecasts <- expand.grid(
  sigma = 1:20/10, 
  quantile = c(0.01, 0.025, 1:19/20, 0.975, 0.99)
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
                       labels = c("WIS", "WIS (log scale)", "log(WIS)")))


score_plot <- function(summary) {
  summary |>
    ggplot(aes(x = sigma, y = score)) +
    geom_vline(xintercept = 1, linetype = "dashed", size = 0.3, color = "grey80") +
    geom_point() + 
    facet_wrap(~type, scales = "free_y") + 
    theme_scoringutils() +
    labs(y = "Score", x = "Standard deviation of predictive distribution") + 
    theme(panel.spacing = unit(1, "lines"))
}

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

refetch_data <- function() {
  
  # download folder using SVN
  # svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed
  
  # load truth data using the covidHubutils package ------------------------------
  truth <- covidHubUtils::load_truth(hub = "ECDC") |>
    filter(target_variable %in% c("inc case", "inc death")) |>
    mutate(target_variable = ifelse(target_variable == "inc case", 
                                    "Cases", "Deaths")) |>
    rename(target_type = target_variable, 
           true_value = value) |>
    select(-model)
  
  fwrite(truth, "data/weekly-truth-Europe.csv")
  
  
  # get the correct file paths to all forecasts ----------------------------------
  folders <- here("data-processed", list.files("data-processed"))
  folders <- folders[
    !(grepl("\\.R", folders) | grepl(".sh", folders) | grepl(".csv", folders))
  ]
  
  file_paths <- purrr::map(folders, 
                           .f = function(folder) {
                             files <- list.files(folder)
                             out <- here::here(folder, files)
                             return(out)}) %>%
    unlist()
  file_paths <- file_paths[grepl(".csv", file_paths)]
  
  # load all past forecasts ------------------------------------------------------
  # ceate a helper function to get model name from a file path
  get_model_name <- function(file_path) {
    split <- str_split(file_path, pattern = "/")[[1]]
    model <- split[length(split) - 1]
    return(model)
  }
  
  # load forecasts
  prediction_data <- map_dfr(file_paths, 
                             .f = function(file_path) {
                               data <- fread(file_path)
                               data[, `:=`(
                                 target_end_date = as.Date(target_end_date),
                                 quantile = as.numeric(quantile),
                                 forecast_date = as.Date(forecast_date), 
                                 model = get_model_name(file_path)
                               )]
                               return(data)
                             }) %>%
    filter(grepl("case", target) | grepl("death", target)) %>%
    mutate(target_type = ifelse(grepl("death", target), 
                                "Deaths", "Cases"), 
           horizon = as.numeric(substr(target, 1, 1))) %>%
    rename(prediction = value) %>%
    filter(type == "quantile", 
           grepl("inc", target)) %>%
    select(location, forecast_date, quantile, prediction, 
           model, target_end_date, target, target_type, horizon)
  
  # merge forecast data and truth data and save
  hub_data <- merge_pred_and_obs(prediction_data, truth, 
                                 by = c("location", "target_end_date", 
                                        "target_type")) |>
    filter(target_end_date >= "2021-01-01") |>
    select(-location_name, -population, -target)
  
  # split forecast data into two to reduce file size
  split <- floor(nrow(hub_data) / 2)
  
  # harmonise forecast dates to be the date a submission was made
  hub_data <- mutate(hub_data, 
                     forecast_date = calc_submission_due_date(forecast_date))
  
  # function that performs some basic filtering to clean the data
  filter_hub_data <- function(hub_data) {
    
    # define the unit of a single forecast
    unit_observation <- c("location", "forecast_date", "horizon", "model", "target_type")
    
    h <- hub_data |>
      # filter out unnecessary horizons and dates
      filter(horizon <= 4, 
             forecast_date > "2021-03-08") |>
      # filter out all models that don't have all quantiles
      group_by_at(unit_observation) |>
      mutate(n = n()) |>
      ungroup() |>
      filter(n == max(n)) |>
      # filter out models that don't have all horizons
      group_by_at(unit_observation) |>
      ungroup(horizon) |>
      mutate(n = length(unique(horizon))) |>
      ungroup() |>
      filter(n == max(n)) 
    
    return(h)
  }
  
  hub_data <- filter_hub_data(hub_data)
  
  fwrite(hub_data[1:split, ], 
         file = "data/full-data-european-forecast-hub-1.csv")
  fwrite(hub_data[(split + 1):nrow(hub_data), ], 
         file = "data/full-data-european-forecast-hub-2.csv")
  
}

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
  hub_data <- hub_data[true_value < 0, true_value := NA][]
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
                prediction = log(pmax(prediction, 0))
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










## ========================================================================== ##
## Figure 5
## ========================================================================== ##






make_table <- function(x, caption = "") {
  x %>%
    rename_all(gsub, pattern = "_", replacement = " ") %>%
    rename_all(str_to_sentence) %>%
    mutate_if(is.numeric, round, 2) %>%
    kable(caption = caption) %>%
    kable_styling()
}

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











# plot with means across locations ---------------------------------------------
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







## =============================================================================
## Figure 5 - Correlations between everything
```{r}
wis_vs_total <- function(scores, summary_fct = mean, fulldata = hub_data) {
  
  mean_values <- 
    fulldata |> 
    filter(!is.na(prediction)) |>
    group_by(location, target_type, target_end_date) |>
    summarise(true_value = unique(true_value)) |>
    group_by(location, target_type) |>
    summarise(mean_obs = mean(true_value))
  
  plot_df <- scores |> 
    filter(model == "EuroCOVIDhub-ensemble", 
           horizon == 2) |>
    group_by(location, target_type, scale, type_and_scale) |>
    summarise(wis = summary_fct(interval_score), 
              .groups = "drop_last") |> 
    inner_join(mean_values) 
  
  plot_df |>
    group_by(target_type, scale) |>
    summarise(correlation = cor(wis, mean_obs)) |> 
    print()
  
  plot_df |>
    ggplot(aes(y = wis, x = mean_obs)) + 
    geom_point() + 
    facet_wrap(~ type_and_scale, scale = "free") + 
    theme_scoringutils() + 
    theme(legend.position = "none") + 
    scale_x_continuous(labels = scale_fn) +
    scale_y_continuous(labels = scale_fn) +
    labs(y = "REPLACE", x = "Mean observed value")
} 


scatter_wis_obs <- wis_vs_total(scores) + 
  labs(y = "Mean WIS") + 
  scale_x_continuous(labels = scale_fn, trans = "log10") +
  scale_y_continuous(labels = scale_fn, trans = "log10") + 
  ggpubr::stat_cor(method = "spearman", p.accuracy = 0.001, 
                   cor.coef.name = "rho", size = 3)


df <- scores |>
  filter(horizon < 5) |>
  select(-c(coverage_deviation, bias, ae_median, dispersion, underprediction, overprediction, type_and_scale)) |>
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
  filter(horizon < 5) |>
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


(scatter_wis_obs) / (p_cor_scores + p_cor_skill) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1, 1), heights = c(3, 1)) &
  theme(legend.position = "bottom")

ggsave("output/figures/HUB-correlations.png", width = 7, height = 7)

```


## =============================================================================
## Figure 6 - Example comparisons of forecasts on the log and the natural scale

```{r}
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


```


## =============================================================================
## Figure 7 - Look at pairwise comparisons and changes in rankings (plus Appendix figures)

```{r}

ranking_figure <- function(target = "Cases") {
  summarised_pairwise <- scores |>
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
    # geom_text(size=3.3, hjust = 0) +
    coord_cartesian(#xlim = c(0.5, 4), 
      ylim = c(0.75, 8.25)) +
    theme_scoringutils() +
    theme(axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position="none")
  
  
  
  plot_rel_wis + plot_ranking_change
  
  # layout <- "
  # AAA
  # BCD"
  
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




# summarised_pairwise |>
#   select(model, scale, relative_skill) |>
#   pivot_wider(names_from = scale, values_from = relative_skill) |>
#   ggplot(aes(y = log, x = natural)) +
#   geom_point() + 
#   # geom_abline(slope = 1) +
#   theme_scoringutils()




```



## =============================================================================
## Summary numbers for the text + Figure Available Forecasts fo Appendix

```{r}
hub_data |>
  summarise(min_date = min(forecast_date), 
            max_date = max(forecast_date)) |>
  mutate(week_diff = (max_date - min_date - 1) / 7)

hub_data$location |> 
  unique() |>
  length()

hub_data |> 
  filter(horizon == 2) |>
  avail_forecasts(by = c("target_end_date", "target_type", "model")) |>
  mutate(forecast_date = as.Date(target_end_date) - 12) |>
  plot_avail_forecasts(show_numbers = FALSE) +
  facet_grid(~ target_type) + 
  labs(y = "Model", x = "Forecast date") 

ggsave(filename = "output/figures/number-avail-forecasts.png", height = 4, width = 10)
```


## =============================================================================
## Summary table for Appendix


# Table with relevant values

```{r}
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
        align = c("l", "l", "l", rep("c", 2)),
        # col.names = c(" ", names(scores)[-(1:2)])) %>%
  ) #|>
# collapse_rows(columns = 1) %>%
# kable_styling(latex_options = c("scale_down",
#                                 "hold_position")) %>%
# column_spec(1, background = "white") %>%
# pack_rows("Observations", 1, 6, indent = FALSE,
#           hline_after = TRUE) %>%
# pack_rows("WIS", 7, 10, indent = FALSE,
#           hline_after = TRUE)

```
























## Scores across forecast targets

This uses the Hub ensemble

```{r}
scores |>
  dplyr::select(location, target_type, scale, target_end_date, interval_score) |>
  unique() |>
  group_by(target_type, scale) |>
  summarise(mean_value = mean(interval_score), 
            sd_value = sd(interval_score), 
            var_value = var(interval_score))


ggsave("output/figures/HUB-average-scores.png", width = 7, height = 2.5)
```

Mean scores: 
  ```{r}
scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(scale, target_type) |>
  summarise(interval_score = mean(interval_score), 
            .groups = "drop_last") |>
  make_table()
```

## Correlation between scores

```{r}
scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(target_type, location, scale) |>
  summarise(interval_score = mean(interval_score), 
            .groups = "drop_last") %>%
  pivot_wider(values_from = interval_score, names_from = scale) %>%
  group_by(target_type) %>%
  summarise(correlation = cor(natural, log))
```




# Correlations for ranks, scores and pairwise comparisons

```{r}

# 1. observations natural vs. log --> pearson
# 2. correlation scores natural vs. log (single forecasts) --> spearman


# Table
# correlation between values
cor_data <- hub_data |>
  select(location, target_end_date, target_type, true_value) |>
  unique() |>
  mutate(log_obs = log(pmax(true_value, 0) + 1)) |>
  group_by(target_type) |>
  summarise(cor = cor(true_value, log_obs))

# # correlation between scores
# # not really meaningful: if we do ranks it's just going to be ranks over everything which doesn't make sense
# # if we do individual scores than it's a bit unclear what the "correct" correlation should be
# cor_scores <- scores |>
#   filter(horizon <=4) |>
#   select(interval_score, scale, forecast_date, model, horizon, location, target_type) |>
#   pivot_wider(names_from = scale, values_from = interval_score) |>
#   group_by(horizon, target_type) |>
#   summarise(cor = cor(natural, log), 
#             spearman = cor(natural, log, method = "spearman"))

# Correlation between ranks when going from natural to log
correlation_ranks <- rankings_wide |>
  filter(horizon < 5) |>
  group_by(horizon, target_type) |>
  summarise(cor = cor(natural, log), 
            spearman = cor(natural, log, method = "spearman")) |>
  rename(`Target type` = target_type)

# we could also do spearman per target and then plot an average of the spearman correlation. 

# Correlation between pairwise comparisons
correlation_rel_skill <- scores |>
  filter(horizon < 5) |>
  summarise_scores(
    by = c("model", "scale", "horizon", "target_type"), 
    relative_skill = TRUE) |>
  select(model, horizon, target_type, relative_skill, scale) |>
  pivot_wider(values_from = relative_skill, names_from = scale) |>
  group_by(horizon, target_type) |>
  summarise(cor = cor(natural, log), 
            spearman = cor(natural, log, method = "spearman")) 

p_cor_ranks <- correlation_ranks |>
  ggplot(aes(x = horizon, y = cor, 
             fill = `Target type`, color = `Target type`)) + 
  geom_histogram(stat = "identity", position = "dodge") +
  theme_scoringutils() + 
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Corr. ranks per forecast", x = "Forecast horizon")

# p_cor_scores <- cor_scores |> 
#   mutate(`Target type` = target_type) |>
#   ggplot(aes(x = horizon, y = cor, 
#              fill = `Target type`, color = `Target type`)) + 
#   geom_histogram(stat = "identity", position = "dodge") +
#   theme_scoringutils() + 
#   coord_cartesian(ylim = c(0, 1)) +
#   labs(y = "Spearman corr. scores single forecasts", x = "Forecast horizon")

p_cor_skill <- correlation_rel_skill |>
  rename(`Target type` = target_type) |>
  ggplot(aes(x = horizon, y = cor, fill = `Target type`, color = `Target type` )) + 
  geom_histogram(stat = "identity", position = "dodge") +
  theme_scoringutils() + 
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Correlation rel. skill", x = "Forecast horizon")

combined <- scores[, c("model", "location", "target_end_date", "scale", "target_type", "interval_score", "horizon")] |>
  full_join(hub_data[, c("location", "target_end_date", "target_type", "model", "true_value")]) |>
  mutate(true_value = ifelse(scale == "natural", true_value, log(true_value + 1)))

combined |>
  filter(horizon <= 4) |>
  rename(`Target type` = target_type) |>
  group_by(horizon, scale, `Target type`) |>
  summarise(cor = cor(interval_score, true_value)) |>
  ggplot(aes(x = horizon, y = cor, fill = `Target type`, color = `Target type` )) + 
  geom_histogram(stat = "identity", position = "dodge") +
  theme_scoringutils() + 
  facet_wrap(~ scale) +
  expand_limits(ylim = c(0, 1)) +
  labs(y = "Correlation score and observation", x = "Forecast horizon")


plot_cors <- p_cor_ranks  + p_cor_skill +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave(filename = "output/figures/HUB-cors.png", width = 7, height = 3, 
       plot = plot_cors)

```




```{r}
scores |>
  summarise_scores(c("model", "target_type", "scale")) |>
  summarise_scores(c("model", "target_type", "scale"), fun = signif, digits = 2) |>
  plot_score_table(by = c("target_type", "scale")) +
  facet_wrap(target_type ~ scale)
```

```{r}

split_data <- split(scores, by = c("type_and_scale"))
cors <- lapply(split_data, FUN = correlation, 
               metrics = c("interval_score", 
                           "bias", "overprediction", 
                           "underprediction",
                           "dispersion"))

# scoringutils dev stuff
# split_data <- split(scores, by = c("target_type", "scale"))
# list_name_as_col <- function(list, by = NULL) {
#   names <- names(list)
#   print(names(list_element))
# }

plots <- lapply(cors, plot_correlation)

(plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]])

correlation
```



```{r}
s <- hub_data |>
  filter(model %in% c("EuroCOVIDhub-ensemble", "epiforecasts-EpiNow2")) |>
  score()

s_summarised <- s |>
  summarise_scores(by = c("model", "range", "horizon"), metric = c("interval_score"))

s_summarised |>
  select(c(model, interval_score, range, horizon)) |>
  pivot_wider(names_from = model, values_from = interval_score) |>
  mutate(rel = `epiforecasts-EpiNow2` / `EuroCOVIDhub-ensemble`) |>
  filter(horizon == 4)
ggplot(aes(y = rel, x = range, 
           color = factor(horizon), group = factor(horizon))) +
  geom_line()

s_summarised |>
  filter(horizon == 1) |>
  select(c(model, interval_score, range, horizon)) |>
  ggplot(aes(y = interval_score, x = range, 
             color = model, group = model)) +
  geom_line()

```




































--- 
  # Code parking lot
  
  ## Scores over time
  restrict to a few locations, e.g. GB, FR, ES (highest average scores there)

```{r}
scores |> 
  filter(model == "EuroCOVIDhub-ensemble",
         location %in% c("GB", "ES", "DE"),
         horizon == 2) |>
  rename(Location = location) |>
  group_by(target_type, scale, Location) |>
  mutate(multiplicative_score = interval_score / min(interval_score)) |>
  ggplot(aes(y = multiplicative_score, x = forecast_date, colour = Location)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(target_type ~ scale, scale = "free_y") + 
  theme_scoringutils() + 
  labs(x = "Forecast date", y = "WIS relative to smallest observed value")

ggsave("output/figures/HUB-scores-over-time.png", width = 7, height = 5)
```

*Overall it looks like there can still be substantial fluctuation in log scores, but maybe slightly less than when scored on an absolute scale. Interestingly, it seems like the smoothing effect is stronger for deaths than for cases.*
  
  ```{r}
# create version of data where the true value is the true growth rate
# interesting question: does it make a difference, whether we transform it 
# to be the weekly growth rate, i.e. take the n-th root, where n is the horizon? 

latest_truth <- fread(here("data", "weekly-truth-Europe.csv")) |>
  rename(last_known_true_value = true_value) |>
  mutate(forecast_date = target_end_date + 2) |>
  select(location, target_type, forecast_date, last_known_true_value) |>
  unique()

# unsure whether the pmax hack is the right way to do it
growth_data <- hub_data |>
  inner_join(latest_truth) |>
  mutate(true_value = true_value / pmax(last_known_true_value + 1),
         prediction = prediction / pmax(last_known_true_value + 1)) |>
  select(-last_known_true_value)
```

## Variance of scores vs. mean of scores across locations 

```{r}
scores |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(target_type, location, scale) |>
  summarise(mean_wis = mean(interval_score), 
            sd_wis = sd(interval_score), 
            .groups = "drop_last") |>
  ggplot(aes(y = sd_wis, x = mean_wis)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.5) + 
  geom_point() + 
  facet_wrap(~ target_type + scale, scale = "free") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  # scale_x_continuous(labels = scale_fn) + 
  # scale_y_continuous(labels = scale_fn) + 
  scale_x_continuous(labels = scale_fn, trans = "log10") +
  scale_y_continuous(labels = scale_fn, trans = "log10") +
  labs(x = "Mean WIS", y = "Sd of WIS") 

ggsave("output/figures/HUB-sd-vs-mean-scores.png", width = 7, height = 5)
```

```{r}
# negative binomial
# MASS::glm.nb(true_value ~ target_type, 
#        data = hub_data %>%
#          mutate(true_value = pmax(true_value, 0)), 
#        link = "identity")
```

## WIS vs. the totsl number of observed values
probably not needed anymore, as we have similar things

```{r}


# wis_vs_total(scores) + 
#   labs(y = "Mean WIS")
# ggsave("output/figures/HUB-mean-scores-vs-total.png", width = 7, height = 5)
# 
# wis_vs_total(scores, summary_fct = stats::sd) + 
#   labs(y = "Sd WIS")
# ggsave("output/figures/HUB-sd-scores-vs-total.png", width = 7, height = 5)


# wis_vs_total(scores, summary_fct = stats::sd) + 
#   labs(y = "Sd WIS") + 
#   scale_x_continuous(labels = scale_fn, trans = "log10") +
#   scale_y_continuous(labels = scale_fn, trans = "log10") 
# ggsave("output/figures/HUB-sd-scores-vs-total-log-log.png", width = 7, height = 5)

```

Test for a single example forecast to verify that rankings change
```{r}
# we see that rankings change even for a single forecast. So we need to check everything is still correct. 
# take one example
test_data <- hub_data |>
  filter(model %in% c("MUNI-ARIMA", "epiforecasts-EpiNow2"), 
         forecast_date == "2021-10-18", 
         target_type == "Cases", 
         location == "AT")

test_data |>
  score(metrics = c("interval_score")) |>
  summarise_scores()

test_data |>
  mutate(prediction = log(prediction), 
         true_value = log(true_value)) |>
  score(metrics = c("interval_score")) |>
  summarise_scores()
## indeed everything seems to be correct, but rankings still change FOR A SINGLE FORECAST. 
# It seems this is because the trade-off between calibration and sharpness changes? 
# need to check in what way
```


## Distribution of scores

```{r}
scores |>
  ggplot(aes(x = interval_score)) + 
  geom_density() + 
  facet_wrap(~ target_type + scale, scale = "free") + 
  scale_x_continuous(labels = scale_fn)
```

## Fit a model to data

Check fit of the log scale model
```{r echo=TRUE}
reg_log <- scores |>
  filter(scale == "log") |>
  glm(formula = interval_score ~ 1 + model + location + target_type + horizon, 
      family = gaussian)
```

```{r}
# check distribution
performance::check_distribution(reg_log) |>
  plot()

# check normality of residuals
check_normality(reg_log) |>
  plot(type = "qq")

# heteroskedasticity of error terms - the plot takes extremly long to run
performance::check_heteroskedasticity(reg_log)
# performance::check_heteroskedasticity(reg_log) |>
#   plot()

# check influence of outliers - plot also takes very long to run
performance::check_outliers(reg_log)
# performance::check_outliers(reg_log) |>
#   plot()

```



--- 
  
  
  ## Comparison of the distribution of model rankings
  
  ```{r}
rankings <- scores |>
  select(-c(coverage_deviation, bias, ae_median, dispersion, underprediction, overprediction)) |>
  group_by(location, forecast_date, horizon, target_type, scale) |>
  mutate(ranking = rank(interval_score) / n()) |>
  ungroup()

rankings_wide <- rankings |>
  select(model, location, target_end_date, forecast_date, horizon,
         interval_score, scale, ranking, target_type) |>
  pivot_wider(names_from = scale, values_from = ranking, 
              id_cols = -interval_score)

plot_ranking_diffs <- function(
    rankings, 
    rankings_wide,
    target_type
) {
  type <- target_type
  # maybe replace this by the same ranking for all plots
  factor_levels <- rankings |>
    filter(horizon == 2, scale == "natural", target_type == type) |>
    group_by(model) |>
    summarise(ranking = mean(ranking)) |>
    arrange(-ranking) |>
    pull(model)
  
  # p_rankings <- rankings |>
  #   filter(horizon == 2) |>
  #   mutate(model = factor(model, levels = factor_levels)) |>
  #   ggplot(aes(y = model, x = ranking)) +
  #   ggdist::stat_halfeye() +
  #   facet_wrap(~ scale) +
  #   theme_scoringutils() +
  #   labs(y = "Model", x = "Normalised ranking")
  
  
  ranking_plot <- function(
    hor
  ) {
    p <- rankings_wide |>
      mutate(diff_ranking = natural - log) |>
      mutate(model = factor(model, levels = factor_levels)) |>
      filter(horizon == hor, 
             !is.na(model)) |>
      ggplot(aes(y = model, x = diff_ranking)) +
      geom_vline(linetype = "dashed", xintercept = 0, 
                 size = 0.6, color = "grey40") +
      ggdist::stat_pointinterval() +
      theme_scoringutils() +
      labs(y = "", x = "Differences in ranking") 
    
    if (hor > 1) {
      p <- p +
        theme(axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(), 
              axis.line.y = element_blank())
    }
    return(p)
  }
  
  ranking_plot(hor = 1) + ranking_plot(2) + ranking_plot(3) + ranking_plot(4) +
    plot_layout(nrow = 1)
}

# should probably have one plot for both cases and deaths since this is rankings
p_diff_cases <- plot_ranking_diffs(rankings, rankings_wide, 
                                   target_type = "Cases")

ggsave(filename = "output/figures/HUB-ranking-diffs.png", 
       plot = p_diff_cases,
       width = 9, height = 5)
```




# Growth Rate stuff - Looking at growth rates directly

## Scores of the growth rate across targets

This uses the Hub ensemble
```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  ggplot(aes(y = interval_score, x = target_type)) + 
  geom_violin(aes(fill = target_type), alpha = 0.2, color = NA) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  facet_wrap(~ scale, scale = "free") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = scale_fn, trans = "log10") + 
  labs(y = "Interval score", x = "Target type")
```
*Scoring the growth rate makes things more comparable*
  
  Mean scores: 
  ```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(scale, target_type) |>
  summarise(interval_score = mean(interval_score), 
            .groups = "drop_last") |>
  make_table()
```

## Scores for the growth rate across locations
```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble", 
         horizon == 2) |>
  group_by(target_type, location, scale) |>
  summarise(interval_score = mean(interval_score), 
            .groups = "drop_last") |>
  ggplot(aes(y = interval_score, x = reorder(location, -interval_score))) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Set1", name = "Forecast target") + 
  facet_wrap(~ target_type + scale, scale = "free") + 
  theme_scoringutils() + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = scale_fn) + 
  labs(y = "Interval score", x = "Target type")
```

## Scores for growth rate over time
restrict to a few locations, e.g. GB, FR, ES (highest average scores there)

UK: 
  ```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble",
         location == "GB",
         horizon == 2) |>
  group_by(target_type, scale) |>
  mutate(multiplicative_score = interval_score / min(interval_score)) |>
  ggplot(aes(y = multiplicative_score, x = forecast_date)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(target_type ~ scale, scale = "free_y") + 
  theme_scoringutils() + 
  labs(x = "Forecast date", y = "WIS relative to smallest observed value")
```

France: Looks like a data revision
```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble",
         location == "FR",
         horizon == 2) |>
  group_by(target_type, scale) |>
  mutate(multiplicative_score = interval_score / min(interval_score)) |>
  ggplot(aes(y = multiplicative_score, x = forecast_date)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(target_type ~ scale, scale = "free_y") + 
  theme_scoringutils() + 
  labs(x = "Forecast date", y = "WIS relative to smallest observed value")
```

Spain: 
  ```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble",
         location == "ES",
         horizon == 2) |>
  group_by(target_type, scale) |>
  mutate(multiplicative_score = interval_score / min(interval_score)) |>
  ggplot(aes(y = multiplicative_score, x = forecast_date)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(target_type ~ scale, scale = "free_y") + 
  theme_scoringutils() + 
  labs(x = "Forecast date", y = "WIS relative to smallest observed value")
```

## Change of average scores for the growth rate for increasing forecast horizons
```{r}
scores_gr |> 
  filter(model == "EuroCOVIDhub-ensemble") |>
  group_by(scale, target_type, horizon) |>
  summarise(interval_score = mean(interval_score), 
            .groups = "drop_last") |>
  mutate(multiplicative_score = interval_score / min(interval_score)) |>
  ggplot(aes(y = multiplicative_score, x = horizon)) + 
  geom_line() + 
  geom_point() + 
  facet_grid(target_type ~ scale) + 
  theme_scoringutils() + 
  labs(x = "Forecast horizon in weeks", y = "Multplicative change in interval score")
```

## Distribution of scores growth rate

```{r}
scores_gr |>
  ggplot(aes(x = interval_score)) + 
  geom_density() + 
  facet_wrap(~ target_type + scale, scale = "free") + 
  scale_x_continuous(labels = scale_fn)
```

## Fit a model to growth rate data

Check fit of the log scale model
```{r echo=TRUE}
reg_gr <- scores_gr |>
  filter(scale == "natural") |>
  glm(formula = interval_score ~ 1 + model + location + target_type + horizon, 
      family = gaussian)

reg_gr_log <- scores_gr |>
  filter(scale == "log") |>
  glm(formula = interval_score ~ 1 + model + location + target_type + horizon, 
      family = gaussian)
```

Check distribution
```{r}
# check distribution
performance::check_distribution(reg_gr_log) |>
  plot()

# check normality of residuals
check_normality(reg_gr_log) |>
  plot(type = "qq")

# heteroskedasticity of error terms - the plot takes extremly long to run
performance::check_heteroskedasticity(reg_gr_log)
# performance::check_heteroskedasticity(reg_log) |>
#   plot()

# check influence of outliers - plot also takes very long to run
performance::check_outliers(reg_gr_log)
# performance::check_outliers(reg_log) |>
#   plot()

```







Figure
Currently discontinuoued and just there to discuss it. 
that shows Variance vs. mean of the observations. Makes the point that the
data looks neg. binomial. 
```{r}
# plot with mean vs. variance --------------------------------------------------
plot_var_mean <- function(hub_data, log = FALSE) {
  df <- hub_data |>
    mutate("x" = true_value) |>
    dplyr::select(location, target_type, target_end_date, true_value, x) |>
    unique() |>
    group_by(location, target_type)
  
  f <- function(x) {x + x^2}
  
  if (log) {
    df <- mutate(df, true_value = log(pmax(true_value, 0) + 1))
    
    f <- function(x) {log(log(x) + log(x)^2 + 1)}
  } 
  
  out <- df |>
    summarise(mean_value = mean(x), 
              sd_value = sd(true_value), 
              var_value = var(true_value)) |>
    ggplot(aes(y = var_value, x = mean_value)) +
    geom_point() + 
    facet_wrap(~ target_type, scales = "free") + 
    scale_x_continuous(trans = "log10", labels = label_fn) +
    scale_y_continuous(trans = "log10", labels = label_fn) +
    labs(y = "Var observations", x = "Mean observations") + 
    # geom_function(fun = f, linetype = "dashed", size = 0.5) +
    theme_scoringutils() + 
    geom_smooth(alpha = 0.2, size = 0.2)
  
  if (log) {
    out <- out +
      labs(y = "Var log(observations)", x = "Mean observations")
  }
  
  return(out)
}

p1 <- plot_var_mean(hub_data)
p2 <- plot_var_mean(hub_data, log = TRUE)


wis_vs_total <- function(scores, summary_fct = mean, fulldata = hub_data) {
  
  mean_values <- 
    fulldata |> 
    filter(!is.na(prediction)) |>
    group_by(location, target_type, target_end_date) |>
    summarise(true_value = unique(true_value)) |>
    group_by(location, target_type) |>
    summarise(mean_obs = mean(true_value))
  
  plot_df <- scores |> 
    filter(model == "EuroCOVIDhub-ensemble", 
           horizon == 2) |>
    group_by(location, target_type, scale, type_and_scale) |>
    summarise(wis = summary_fct(interval_score), 
              .groups = "drop_last") |> 
    inner_join(mean_values) 
  
  plot_df |>
    group_by(target_type, scale) |>
    summarise(correlation = cor(wis, mean_obs)) |> 
    print()
  
  plot_df |>
    ggplot(aes(y = wis, x = mean_obs)) + 
    geom_point() + 
    facet_wrap(~ type_and_scale, scale = "free") + 
    theme_scoringutils() + 
    theme(legend.position = "none") + 
    scale_x_continuous(labels = scale_fn) +
    scale_y_continuous(labels = scale_fn) +
    labs(y = "REPLACE", x = "Mean observed value")
} 

# log-log versions of that plot

scatter_wis_obs <- wis_vs_total(scores) + 
  labs(y = "Mean WIS") + 
  scale_x_continuous(labels = scale_fn, trans = "log10") +
  scale_y_continuous(labels = scale_fn, trans = "log10") 

p1 / p2 / scatter_wis_obs + 
  plot_layout(heights = c(1, 1, 2))

ggsave("output/figures/HUB-mean-scores-vs-total-log-log.png", width = 7, height = 7)
```






## ========================================================================== ##
## OLD FIGURE 1
## ========================================================================== ##
figure_1 <- function() {
  true_values <- seq(0.2, 5, length.out = 1000)
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
  
  vals <- expand.grid(
    true_value = true_values, 
    quantile = quantiles
  ) |>
    mutate(id = true_value) |>
    arrange(true_value, quantile) |>
    mutate(prediction = qnorm(p = quantile, mean = 1, sd = 0.4), 
           model = "Model")
  
  scores <- vals |>
    mutate(scale = "natural") |>
    score() |> 
    summarise_scores(by = c("scale", "id")) 
  
  scores_log <- vals |>
    mutate(true_value = log(true_value), 
           prediction = log(prediction), 
           scale = "log") |>
    score() |>
    summarise_scores(by = c("scale", "id"))
  
  scores <- scores |>
    rbind(scores_log) |>
    group_by(scale) |>
    mutate(score = interval_score / min(interval_score), 
           scale = factor(scale, levels = c("natural", "log"))) 
  
  # scale factor for the density in the plot to make it look nicer
  scale_factor <- 3
  
  plot_fct <- function(scores, filter = "natural", relative_x = FALSE) {
    p <- scores |>
      filter(scale == filter) |>
      ggplot(aes(x = id, y = interval_score/scale_factor)) + 
      geom_area(stat = "function", 
                fun = function(x) {dnorm(x, mean = 1, sd = 0.4)}, 
                color = "grey", fill = "grey", alpha = 0.5) +
      geom_line() + 
      labs(y = "WIS", x = "Observed value") + 
      scale_y_continuous(label = function(x) {paste(scale_factor * x)}) + 
      facet_wrap(~ scale, ncol = 1, scales = "free") + 
      theme_scoringutils() + 
      theme(panel.spacing = unit(1, "lines"))
    
    if (relative_x) {
      p <- p +
        scale_x_continuous(
          trans = "log", 
          label = function(x) {
            ifelse(x < 1, 
                   paste0("1/", (1 / x)), 
                   paste(x))
          }, 
          breaks = c(0.2, 1/2, 1, 2, 5)
        )
    }
    return(p)
  }
  
  p1 <- plot_fct(scores, filter = "natural")
  p2 <- plot_fct(scores, filter = "log")
  p3 <- plot_fct(scores, filter = "natural", relative_x = TRUE)
  p4 <- plot_fct(scores, filter = "log", relative_x = TRUE)
  
  (p1 + p3) / (p2 + p4) +
    plot_annotation(tag_levels = "A")
  
  ggsave("output/figures/SIM-effect-log-score.png", width = 7, 
         height = 4)
}
figure_1()
