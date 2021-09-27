tar_target(toy_log_scores, {
  toy_forecasts |>
    mutate(across(.cols = c("true_value", "prediction"), ~ log(.x))) |>
    eval_forecasts(
      summarise_by = c("init", "r", "r_error", "additive_error", "time"),
      metrics = c("crps")
    )
})
