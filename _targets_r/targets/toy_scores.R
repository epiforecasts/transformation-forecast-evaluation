tar_target(toy_scores, {
  toy_forecasts |>
    eval_forecasts(
      summarise_by = c("init", "r", "r_error", "additive_error", "time"),
      metrics = c("crps")
    )
})
