library(data.table)
library(dplyr)
library(scoringutils)
library(ggplot2)
library(tidyr)

mean_county <- 100
time_points <- 1000
state_sizes <- 10^seq(0, 2, 0.01)
q <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

df <- data.table(
  state = 1:length(state_sizes),
  state_size = state_sizes,
  quantile = list(q)
) |>
  unnest(quantile) |>
  mutate(date = list(1:time_points)) |>
  unnest(cols = date) |>
  group_by(state, state_size, date) |>
  mutate(true_value = rpois(1, mean_county * state_size), 
         prediction = qpois(p = quantile, lambda = mean_county * state_size)) 


scores_state <- df |>
  eval_forecasts()

scores_state_log <- df |>
  mutate(true_value = log(true_value), 
         prediction = log(prediction)) |>
  eval_forecasts()

make_plot <- function(scores, summary_fct = mean) {
  scores |>
    group_by(state_size) |>
    summarise(interval_score = summary_fct(interval_score)) |>
    ggplot(aes(y = interval_score, x = state_size)) +
    geom_point() +  
    labs(y = "WIS", x = "Size of state") + 
    theme_minimal() 
}

mean_state <- scores_state |> 
  make_plot() 

mean_state2 <- scores_state |> 
  make_plot() +
  scale_x_continuous(trans = "log10")

mean_state_log <- scores_state_log |> 
  make_plot() 

mean_state_log2 <- scores_state_log |> 
  make_plot() +
  scale_x_continuous(trans = "log10")

(mean_state + mean_state_log) /
  (mean_state2 + mean_state_log2)

ggsave("output/figures/SIM-mean-sd-state-size.png", width = 7, height = 4)


# plots mean vs. sd of wis -----------------------------------------------------
mean_wis_vs_sd <- function(scores) {
  scores |>
    group_by(state_size) |>
    summarise(mean_wis = mean(interval_score), 
              sd_wis = sd(interval_score)) |>
    ggplot(aes(y = mean_wis, x = sd_wis)) +
    geom_point() +  
    labs(y = "Mean WIS", x = "Sd WIS") + 
    theme_minimal() 
}

mean_wis_vs_sd(scores_state_log)











# # know that sum of independent poisson variables is poisson with lambda = lambda1 + lambda2 etc. 
# 
# n_counties <- 10000
# mean_county <- 100
# dates <- 1:15
# n_states <- 300
# q <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
# 
# state_sizes <- sample.int(n_counties, size = n_states - 1, replace = TRUE)
# state_sizes <- sort(c(0, state_sizes, n_counties)) |>
#   diff() 
# 
# df <- data.table(
#   county = 1:n_counties,
#   state = rep(1:n_states, state_sizes), 
#   state_size = rep(state_sizes, state_sizes),
#   quantile = list(q)
# ) |>
#   unnest(quantile) |>
#   mutate(date = list(dates)) |>
#   unnest(cols = date) |>
#   group_by(county, state, state_size, date) |>
#   mutate(true_value = rpois(1, mean_county), 
#          prediction_county = qpois(p = quantile, lambda = mean_county)) |>
#   group_by(state, state_size) |>
#   mutate(prediction_state = qpois(p = quantile, lambda = mean_county * state_size)) 
# 
# 
# # scores -----------------------------------------------------------------------
# scores_state <- df |>
#   group_by(state, state_size, quantile, date) |>
#   summarise(true_value = sum(true_value), 
#             prediction = unique(prediction_state)) |>
#   eval_forecasts()
# 
# scores_state_log <- df |>
#   group_by(state, state_size, quantile, date) |>
#   summarise(true_value = sum(true_value), 
#             prediction = unique(prediction_state)) |>
#   mutate(true_value = log(true_value + 1), 
#          prediction = log(prediction +1)) |>
#   eval_forecasts()
# 
# 
# # plots ------------------------------------------------------------------------
# scores_state |>
#   group_by(state, state_size) |>
#   summarise(interval_score = mean(interval_score)) |>
#   ggplot(aes(y = interval_score, x = state_size)) +
#   geom_point() + 
#   geom_smooth() + 
#   labs(y = "WIS", x = "Size of state") + 
#   theme_minimal()
# 
# scores_state |>
#   group_by(state, state_size) |>
#   summarise(interval_score = sd(interval_score)) |>
#   ggplot(aes(y = interval_score, x = state_size)) +
#   geom_point() + 
#   labs(y = "WIS - sd", x = "Size of state") + 
#   geom_smooth() + 
#   scale_x_continuous(trans = "log10") + 
#   geom_smooth() + 
#   theme_minimal()
# 
# 
# scores_state_log |>
#   group_by(state, state_size) |>
#   summarise(interval_score = mean(interval_score)) |>
#   ggplot(aes(y = interval_score, x = state_size)) +
#   geom_point() + 
#   labs(y = "WIS", x = "Size of state") + 
#   geom_smooth() + 
#   scale_x_continuous(trans = "log10")
# 
# scores_state_log |>
#   group_by(state, state_size) |>
#   summarise(interval_score = sd(interval_score)) |>
#   ggplot(aes(y = interval_score, x = state_size)) +
#   geom_point() + 
#   labs(y = "WIS - sd", x = "Size of state") + 
#   geom_smooth() + 
#   scale_x_continuous(trans = "log10")
