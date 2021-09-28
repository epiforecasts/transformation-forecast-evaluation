tar_target(toy_forecasts, {
  toy_simulations |>
    select(init, r, r_error, additive_error, time, sample = sim,
           prediction = obs) |>
    left_join(toy_truth, by = c("init", "r", "time"))
})
