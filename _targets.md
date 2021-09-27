Supplementary information
================

# Setup

Set up the workflow pipeline and options. We first load the `targets`
package and remove the potentially outdated workflow.

``` r
library(targets)
library(tibble)
library(tidyr)
library(dplyr)
library(scoringutils)
library(ggplot2)
library(purrr)
library(here)
tar_unscript()
```

We now define shared global options across our workflow and load R
functions from the `R` folder.

``` r
options(tidyverse.quiet = TRUE)
library(purrr)
library(here)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
tar_option_set(
  packages = c("tibble", "tidyr", "dplyr", "scoringutils", "ggplot2", "purrr")
)
#> Establish _targets.R and _targets_r/globals/globals.R.
```

# Toy example

As a toy example we simulate a grid of data with varying exponential
growth rates and a Poisson observation model. As example forecasts we
use the same simulation model but add error to either the growth rate or
the mean of observations. We do this using the following steps,

  - Define a grid of scenarios including the true values (zero growth
    rate or cumulative linear error).

<!-- end list -->

``` r
tar_target(toy_scenarios, {
  expand_grid(
    init = c(100),
    r = c(0, 0.01, 0.1, -0.01, -0.1),
    r_error = c(0, 0.01, 0.025, 0.05, 0.1, -0.01, -0.025, -0.05, -0.1),
    additive_error = c(0, 1, 10, -1, -10)
  ) |> 
    mutate(id = 1:n())
})
#> Define target toy_scenarios from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_scenarios.R.
```

  - Simulate each scenario a 1000 times.

<!-- end list -->

``` r
tar_target(toy_simulations, {
  toy_scenarios |>
    rowwise() |>
    mutate(y = list(
      simulate_exp_poisson(init = init, time = 4, growth = r + r_error,
                           additive_error = additive_error, sims = 1000)
      )
    ) |>
    unnest(cols = c(y))
})
#> Define target toy_simulations from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_simulations.R.
```

  - Filter for the “truth” scenario and keep only the first simulation.

<!-- end list -->

``` r
tar_target(toy_truth, {
  toy_simulations |>
    filter(r_error == 0, additive_error == 0, sim == 1) |>
    select(init, r, time, true_value = obs)
})
#> Define target toy_truth from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_truth.R.
```

  - Filter for all other scenarios.

<!-- end list -->

``` r
tar_target(toy_forecasts, {
  toy_simulations |>
    filter(!(r_error == 0 & additive_error == 0)) |>
    select(init, r, r_error, additive_error, time, sample = sim,
           prediction = obs) |>
    left_join(toy_truth, by = c("init", "r", "time"))
})
#> Define target toy_forecasts from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_forecasts.R.
```

  - Score example forecasts and summarise by growth rate, growth rate
    error, additive error, and forecast horizon.

<!-- end list -->

``` r
tar_target(toy_scores, {
  toy_forecasts |>
    eval_forecasts(
      summarise_by = c("init", "r", "r_error", "additive_error", "time"),
      metrics = c("crps")
    )
})
#> Define target toy_scores from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_scores.R.
```

  - Repeat the scoring process but on the log scale.

<!-- end list -->

``` r
tar_target(toy_log_scores, {
  toy_forecasts |>
    mutate(across(.cols = c("true_value", "prediction"), ~ log(.x))) |>
    eval_forecasts(
      summarise_by = c("init", "r", "r_error", "additive_error", "time"),
      metrics = c("crps")
    )
})
#> Define target toy_log_scores from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_log_scores.R.
```

  - Join log and natural scale scores.

<!-- end list -->

``` r
tar_target(toy_all_scores, {
  toy_log_scores |>
    mutate(scale = "log") |>
    bind_rows(
      toy_scores |>
        mutate(scale = "natural")
    )
})
#> Define target toy_all_scores from chunk code.
#> Establish _targets.R and _targets_r/targets/toy_all_scores.R.
```

  - Plot natural scale scores by horizon.

<!-- end list -->

``` r
tar_target(plot_toy_scores_by_horizon, {
  toy_scores |>
    filter(time <= 4) |>
    plot_scores_by_horizon()
})
#> Define target plot_toy_scores_by_horizon from chunk code.
#> Establish _targets.R and _targets_r/targets/plot_toy_scores_by_horizon.R.
```

  - Plot log scale scores by horizon.

<!-- end list -->

``` r
tar_target(plot_toy_log_scores_by_horizon, {
  toy_log_scores |>
    filter(time <= 4) |>
    plot_scores_by_horizon()
})
#> Define target plot_toy_log_scores_by_horizon from chunk code.
#> Establish _targets.R and _targets_r/targets/plot_toy_log_scores_by_horizon.R.
```

  - Plot log and natural scale scores for the 4 week horizon.

<!-- end list -->

``` r
tar_target(plot_toy_scores_by_scale, {
  toy_all_scores |>
    filter(time == 4) |>
    plot_scores_by_scale()
})
#> Define target plot_toy_scores_by_scale from chunk code.
#> Establish _targets.R and _targets_r/targets/plot_toy_scores_by_scale.R.
```

  - Save output for external use.

<!-- end list -->

``` r
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
#> Establish _targets.R and _targets_r/targets/save_toy_output.R.
```

# Pipeline

If all`{targets}` were run in non-interactive mode, then the pipeline is
ready to run using the following,

``` r
tar_make()
```

The complete pipeline can be visualised using,

``` r
tar_visnetwork()
```
