tar_target(toy_forecasts, {
  toy_simulations |>
    filter(!(r_error == 0 & additive_error == 0)) |>
    select(init, r, r_error, additive_error, time, sample = sim,
           prediction = obs) |>
    left_join(toy_truth, by = c("init", "r", "time"))
})
