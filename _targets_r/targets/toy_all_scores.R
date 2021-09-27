tar_target(toy_all_scores, {
  toy_log_scores |>
    mutate(scale = "log") |>
    bind_rows(
      toy_scores |>
        mutate(scale = "natural")
    )
})
