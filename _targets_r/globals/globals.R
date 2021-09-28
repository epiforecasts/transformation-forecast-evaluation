options(tidyverse.quiet = TRUE)
library(purrr)
library(here)
library(tibble)
library(tarchetypes)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
tar_option_set(
  packages = c("tibble", "tidyr", "dplyr", "scoringutils", "ggplot2", "purrr",
               "readr"),
  error = "workspace"
)
