tar_map(
  toy_growth_rates,
  tar_target(
    toy_forecasts_plot,
    plot_toy_forecasts(toy_forecasts, growth = r[[1]],
                       alpha = 0.2, samples = 10)
  ),
  tar_target(
    plot_toy_forecasts_file,
    save_plot(
      toy_forecasts_plot, 
      here("output", "figures", 
           paste0("plot_toy_forecasts_", r[[1]], ".png"))
    ),
    format = "file")
)
