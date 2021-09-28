tar_target(toy_scores_by_scale_plot, {
  toy_all_scores |>
    filter(time == 4) |>
    plot_toy_scores_by_scale()
})
