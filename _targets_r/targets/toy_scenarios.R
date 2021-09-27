tar_target(toy_scenarios, {
  expand_grid(
    init = c(100),
    r = c(0, 0.01, 0.1, -0.01, -0.1),
    r_error = c(0, 0.01, 0.025, 0.05, 0.1, -0.01, -0.025, -0.05, -0.1),
    additive_error = c(0, 1, 10, -1, -10)
  ) |> 
    mutate(id = 1:n())
})
