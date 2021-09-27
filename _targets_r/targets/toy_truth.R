tar_target(toy_truth, {
  toy_simulations |>
    filter(r_error == 0, additive_error == 0, sim == 1) |>
    select(init, r, time, true_value = obs)
})
