#' Simulate Count data using an underlying exponential process and a Poisson
#' Observation model.
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr unnest
simulate_exp_poisson <- function(init = 100, time = 10, growth = 0,
                                 additive_error = 0, sims = 1) {
  y <- init * exp(growth * 1:time)
  y <- y + additive_error
  y <- max(0, y)
  obs <- tibble::tibble(
    sim = list(1:sims),
    time = 1:time,
    y = y,
    obs = purrr::map(y, ~ rpois(sims, .))
  )
  obs <- tidyr::unnest(obs, cols = c(obs, sim))
  return(obs)
}