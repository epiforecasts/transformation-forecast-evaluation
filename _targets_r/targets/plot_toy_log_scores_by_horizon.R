tar_target(plot_toy_log_scores_by_horizon, {
  toy_log_scores |>
    filter(time <= 4) |>
    plot_scores_by_horizon()
})
