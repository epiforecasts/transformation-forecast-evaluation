library(data.table)
library(scoringutils)
library(ggplot2)
library(scoringRules)
library(dplyr)
library(patchwork)

# define simulation parameters
n_steps = 500
n_rep <- 5000
true_mean = 0
true_sd = 5
true_values <- rnorm(n = n_rep, mean = true_mean, sd = true_sd)
sd <- 10^(seq(-1, 1.6, length.out = n_steps))
mu <- seq(0, 100, length.out = n_steps)


true_values <- seq(0.2, 5, length.out = 1000)

vals <- expand.grid(
  true_value = true_values, 
  quantile = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
) |>
  mutate(id = true_value) |>
  arrange(true_value, quantile) |>
  mutate(prediction = qnorm(p = quantile, mean = 1, sd = 0.4), 
         model = "Model") 

scores <- vals |>
  mutate(scale = "natural") |>
  eval_forecasts() |> 
  rbind(
    vals |>
      mutate(true_value = log(true_value), 
             prediction = log(prediction), 
             scale = "log") |>
      eval_forecasts()
  ) |>
  group_by(scale) |>
  mutate(score = interval_score / min(interval_score), 
         scale = factor(scale, levels = c("natural", "log"))) 

scale_factor <- 3 # scale factor for the density

p1 <- scores |>
  ggplot(aes(x = id, y = interval_score/scale_factor)) + 
  geom_area(stat = "function", 
            fun = function(x) {dnorm(x, mean = 1, sd = 0.4)}, 
            color = "grey", fill = "grey", alpha = 0.5) +
  geom_line() + 
  labs(y = "WIS", x = "Observed value") + 
  scale_y_continuous(label = function(x) {paste(scale_factor * x)}) + 
  facet_wrap(~ scale) + 
  theme_minimal()

scale_factor2 <- 3
p2_log <- scores |>
  ggplot(aes(x = id, y = interval_score/scale_factor2)) + 
  geom_area(stat = "function", 
            fun = function(x) {dnorm(x, mean = 1, sd = 0.4)}, 
            color = "grey", fill = "grey", alpha = 0.5) +
  geom_line() + 
  labs(y = "WIS", x = "Observed value") + 
  scale_y_continuous(label = function(x) {paste(scale_factor2 * x)}) + 
  scale_x_continuous(trans = "log10", 
                     label = function(x) {
                       ifelse(x < 1, 
                              paste0("1/", (1 / x)), 
                              paste(x))
                     }, 
                     breaks = c(0.2, 1/2, 1, 2, 5)) + 
  facet_wrap(~ scale) + 
  theme_minimal()

p1 / p2_log

ggsave("output/figures/SIM-effect-log-score.png", width = 6, 
       height = 4)

