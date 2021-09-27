tar_target(plot_toy_scores_by_scale, {
  toy_all_scores |>
    filter(time == 4) |>
    plot_scores_by_scale()
})
