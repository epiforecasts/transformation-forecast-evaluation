tar_target(toy_simulations, {
  toy_scenarios |>
    rowwise() |>
    mutate(y = list(
      simulate_exp_poisson(init = init, time = 4, growth = r + r_error,
                           additive_error = additive_error, sims = 1000)
      )
    ) |>
    unnest(cols = c(y))
})
