tar_target(toy_log_scores_by_horizon_plot, {
  toy_log_scores |>
    filter(time <= 4) |>
    plot_toy_scores_by_horizon()
})
