library(data.table)
library(dplyr)
library(scoringutils)
library(ggplot2)
library(tidyr)
library(patchwork)

setup_df <- function(state_sizes = 10^seq(0, 2, 0.01), 
                     q = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99), 
                     time_points = 1000) {
  df <- data.table(
    state = 1:length(state_sizes),
    state_size = state_sizes,
    quantile = list(q)
  ) |>
    unnest(quantile) |>
    mutate(date = list(1:time_points)) |>
    unnest(cols = date) |>
    group_by(state, state_size, date)  
}

score_states <- function(df) {
  scores_state <- df |>
    eval_forecasts() |>
    mutate(scale = "natural")
  
  scores_state_log <- df |>
    mutate(true_value = log(true_value + 1), 
           prediction = log(prediction + 1)) |>
    eval_forecasts() |>
    mutate(scale = "log")
  
  scores <- 
    rbind(scores_state, 
          scores_state_log) |>
    mutate(scale = factor(scale, levels = c("natural", "log")))
  
  return(scores)
}

label_fn <- function(x) {
  return(100*x)
}

make_plot <- function(scores, summary_fct = mean) {
  p1 <- scores |>
    group_by(state_size, scale, Theta) |>
    summarise(interval_score = summary_fct(interval_score)) |>
    group_by(Theta, scale) |>
    mutate(interval_score = interval_score / mean(interval_score), 
           Variance = as.factor(as.character(Theta)),
           Variance = recode_factor(Variance, 
             "0.1" = "\u03C3 = \u03BC + 10 \u00B7 \u03BC^2", 
             "1" = "\u03C3 = \u03BC + \u03BC^2", 
             "1e+09" = "\u03C3 = \u03BC"),
           Theta = ifelse(Theta == "1e+09", "1b", Theta)) |>
    ggplot(aes(y = interval_score, x = state_size, colour = Variance)) +
    geom_point(size = 0.4) +  
    labs(y = "WIS", x = "Mean") +
    scale_x_continuous(labels = label_fn) +
    theme_scoringutils() + 
    facet_wrap(~ scale, scales = "free_y")
  
  p2 <- p1 + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
  
  p1 / p2 +
    plot_annotation(tag_levels = "A")
}




# sample Negative Binomial -----------------------------------------------------
# var = mu + mu^2/size --> increases a lot with mu
# theta = size very high --> poisson distribution
mean_county <- 100

sizes_nbinom <- c(0.1, 1, 1e9)

# res <- list()
# for (size_nbinom in sizes_nbinom) {
#   df <- setup_df(time_points = 1000) |>
#     mutate(true_value = rnbinom(n = 1, size = size_nbinom, 
#                                 mu = mean_county * state_size), 
#            prediction = qnbinom(p = quantile, size = size_nbinom, 
#                                 mu = mean_county * state_size)) 
#   
#   scores <- score_states(df) |>
#     mutate(Theta = as.character(size_nbinom))
#   
#   res[[paste(size_nbinom)]] <- scores
# }

# saveRDS(res, file = "output/data/simulation-negative-binom.Rda")
res <- readRDS(file = "output/data/simulation-negative-binom.Rda")

out <- rbindlist(res) 

make_plot(out, summary_fct = mean) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") &
  labs(y = "Normalised WIS") 

ggsave("output/figures/SIM-mean-state-size.png", width = 7, height = 4.1)

# make_plot(out, summary_fct = stats::sd) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom") &
#   labs(y = "Realtive WIS sd")
# 
# ggsave("output/figures/SIM-sd-state-size.png", width = 7, height = 4)











# example poisson --------------------------------------------------------------
mean_county <- 100
df <- setup_df() |>
  mutate(true_value = rpois(1, mean_county * state_size), 
         prediction = qpois(p = quantile, lambda = mean_county * state_size)) 

scores <- score_states(df)
make_plot(scores)

ggsave("output/figures/SIM-mean-sd-state-size.png", width = 7, height = 4)

# Example very small county mean -----------------------------------------------
mean_county <- 1
df <- setup_df() |>
  mutate(true_value = rpois(1, mean_county * state_size), 
         prediction = qpois(p = quantile, lambda = mean_county * state_size)) 

scores <- score_states(df)

make_plot(scores)






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
