#' Save a plot and return the ppath for targets
#' @importFrom ggplot2 ggsave
save_plot <- function(plot, filename, ...) {
  ggplot2::ggsave(filename, plot, height = 8, width = 8)
  return(filename)
}

#' Save a dataframe to a csv and return the path for targets
save_csv <- function(dt, path) {
  readr::write_csv(dt, path)
  return(path)
}