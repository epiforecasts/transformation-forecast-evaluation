list(
  tar_target(
    toy_scores_csv, 
    save_csv(toy_all_scores, here("output/data/toy_all_scores.csv")),
    format = "file"
  ),
  tar_target(
    toy_scores_by_scale_file, 
    save_plot(plot_toy_scores_by_scale, 
              here("output", "figures", "toy_scores_by_scale.png"),
    format = "file")
            ),
  tar_target(
    toy_scores_by_horizon_file, 
    save_plot(plot_toy_scores_by_horizon, 
              here("output", "figures", "toy_scores_by_horizon.png"),
    format = "file")
            ),
  tar_target(
    toy_log_scores_by_horizon_file, 
    save_plot(plot_toy_log_scores_by_horizon, 
              here("output", "figures", "toy_log_scores_by_horizon.png"),
    format = "file")
            )
)
