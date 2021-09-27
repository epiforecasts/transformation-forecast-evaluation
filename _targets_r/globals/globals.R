options(tidyverse.quiet = TRUE)
library(purrr)
library(here)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
tar_option_set(
  packages = c("tibble", "tidyr", "dplyr", "scoringutils", "ggplot2", "purrr")
)
