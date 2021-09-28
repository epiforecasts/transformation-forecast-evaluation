tar_target(toy_scores_by_horizon_plot, {
  toy_scores |>
    filter(time <= 4) |>
    plot_toy_scores_by_horizon()
})
