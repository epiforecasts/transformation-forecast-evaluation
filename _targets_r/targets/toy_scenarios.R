tar_target(toy_scenarios, {
  expand_grid(
    init = c(100),
    r = toy_growth_rates$r,
    r_error = c(0, 0.01, 0.05, 0.1, 0.25, -0.01, -0.05, -0.1, -0.25),
    additive_error = c(0, 5, 10, -5, -10)
  ) |> 
    mutate(id = 1:n())
})
