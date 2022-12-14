library(dplyr)
library(tidyr)
library(scoringutils)
library(ggplot2)
library(patchwork)

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

