list(
  tar_target(
    toy_forecasts_csv, 
    save_csv(toy_forecasts, here("output/data/toy_forecasts.csv")),
    format = "file"
  ),
  tar_target(
    toy_scores_csv, 
    save_csv(toy_all_scores, here("output/data/toy_all_scores.csv")),
    format = "file"
  ),
  tar_target(
    toy_scores_by_scale_file, 
    save_plot(toy_scores_by_scale_plot, 
              here("output", "figures", "toy_scores_by_scale.png"),
    format = "file")
            ),
  tar_target(
    toy_scores_by_horizon_file, 
    save_plot(toy_scores_by_horizon_plot, 
              here("output", "figures", "toy_scores_by_horizon.png"),
    format = "file")
            ),
  tar_target(
    toy_log_scores_by_horizon_file, 
    save_plot(toy_log_scores_by_horizon_plot, 
              here("output", "figures", "toy_log_scores_by_horizon.png"),
    format = "file")
            )
)
