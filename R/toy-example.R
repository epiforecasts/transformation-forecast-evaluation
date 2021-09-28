#' Simulate Count data using an underlying exponential process and a Poisson
#' Observation model.
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr unnest
simulate_exp_poisson <- function(init = 100, time = 10, growth = 0,
                                 additive_error = 0, sims = 1) {
  y <- init * exp(cumsum(rep(growth, time)))
  y <- y + cumsum(rep(additive_error, time))
  y <- max(0, y)
  obs <- tibble::tibble(
    sim = list(1:sims),
    time = 1:time,
    y = y,
    obs = purrr::map(y, function(lambda) {
      set.seed(runif(1, 0, 100) + lambda)
      rpois(sims, lambda)
      }
    )
  )
  obs <- tidyr::unnest(obs, cols = c(obs, sim))
  return(obs)
}

#' Plot simulated observations and forecasts
#' @import ggplot2
#' @importFrom dplyr filter
plot_toy_forecasts <- function(forecasts, growth = 0.01, samples = 10,
                               alpha = 0.2) {
  forecasts |>
    filter(sample <= samples, r == growth) |>
    ggplot() +
    aes(x = time, y = prediction, group = sample) +
    geom_point(aes(y = true_value), alpha = 0.9, size = 1.1) +
    geom_line(alpha = alpha) +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_grid(vars(additive_error), vars(r_error))
}

#' Plot scores by horizon
#' @import ggplot2
#' @importFrom dplyr group_by mutate
plot_toy_scores_by_horizon <- function(scores) {
  scores |>
    group_by(additive_error, r) |>
    mutate(relative_crps = crps / max(crps)) |>
    ungroup() |>
    ggplot() +
    aes(y = relative_crps, x = time, col = r_error, group = r_error) +
    geom_point(alpha = 0.8) +
    geom_line(alpha = 0.8) +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_grid(vars(additive_error), vars(r))
}

#' Plot scores by scale
#' @import ggplot2
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr pivot_wider
plot_toy_scores_by_scale <- function(scores) {
  scores |>
    group_by(additive_error, r, scale) |>
    mutate(relative_crps = crps / max(crps)) |>
    ungroup() |>
    select(-crps) |>
    pivot_wider(names_from = c("scale"), values_from = "relative_crps") |>
    ggplot() +
    aes(y = log, x = natural, col = r_error, group = r_error) +
    geom_abline(intercept = 0, slope = 1, linetype = 2) +
    geom_point(alpha = 0.8) +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_grid(vars(additive_error), vars(r))
}