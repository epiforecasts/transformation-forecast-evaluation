tar_target(plot_toy_scores_by_horizon, {
  toy_scores |>
    filter(time <= 4) |>
    plot_scores_by_horizon()
})
