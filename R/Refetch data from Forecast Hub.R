# Code to refetch data from the Forecast Hub

library(covidHubUtils)
library(dplyr)
library(data.table)
library(purrr)
library(scoringutils)
library(here)
library(stringr)

analysis_date <- "2022-12-12"
  
# download folder using SVN
# svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed

# load truth data using the covidHubutils package ------------------------------

if (file.exists("data/weekly-truth-Europe.csv")) {
  truth <- fread("data/weekly-truth-Europe.csv")
} else {
  truth <- covidHubUtils::load_truth(hub = "ECDC") |>
    filter(target_variable %in% c("inc case", "inc death")) |>
    mutate(target_variable = ifelse(target_variable == "inc case", 
                                    "Cases", "Deaths")) |>
    rename(target_type = target_variable, 
           true_value = value) |>
    select(-model)
  
  fwrite(truth, "data/weekly-truth-Europe.csv")
}


# get the correct file paths to all forecasts ----------------------------------
folders <- here("data-processed", list.files("data-processed"))
folders <- folders[
  !(grepl("\\.R", folders) | grepl(".sh", folders) | grepl(".csv", folders))
]

file_paths <- purrr::map(folders, 
                         .f = function(folder) {
                           files <- list.files(folder)
                           out <- here::here(folder, files)
                           return(out)}) %>%
  unlist()
file_paths <- file_paths[grepl(".csv", file_paths)]

# load all past forecasts ------------------------------------------------------
# ceate a helper function to get model name from a file path
get_model_name <- function(file_path) {
  split <- str_split(file_path, pattern = "/")[[1]]
  model <- split[length(split) - 1]
  return(model)
}

# load forecasts
prediction_data <- map_dfr(file_paths, 
                           .f = function(file_path) {
                             data <- fread(file_path)
                             data[, `:=`(
                               target_end_date = as.Date(target_end_date),
                               quantile = as.numeric(quantile),
                               forecast_date = as.Date(forecast_date), 
                               model = get_model_name(file_path)
                             )]
                             return(data)
                           }) %>%
  filter(grepl("case", target) | grepl("death", target)) %>%
  mutate(target_type = ifelse(grepl("death", target), 
                              "Deaths", "Cases"), 
         horizon = as.numeric(substr(target, 1, 1))) %>%
  rename(prediction = value) %>%
  filter(type == "quantile", 
         grepl("inc", target)) %>%
  select(location, forecast_date, quantile, prediction, 
         model, target_end_date, target, target_type, horizon)

# merge forecast data and truth data and save
hub_data <- merge_pred_and_obs(prediction_data, truth, 
                               by = c("location", "target_end_date", 
                                      "target_type")) |>
  filter(target_end_date >= "2021-01-01") |>
  select(-location_name, -population, -target)

# harmonise forecast dates to be the date a submission was made
hub_data <- mutate(hub_data, 
                   forecast_date = calc_submission_due_date(forecast_date))

# function that performs some basic filtering to clean the data
# filter_hub_data <- function(hub_data) {
#   
#   # define the unit of a single forecast
#   unit_observation <- c("location", "forecast_date", "horizon", "model", "target_type")
#   
#   h <- hub_data |>
#     # filter out unnecessary horizons and dates
#     filter(horizon <= 4, 
#            forecast_date >= "2021-03-08") |>
#     # filter out all models that don't have all quantiles
#     group_by_at(unit_observation) |>
#     mutate(n = n()) |>
#     ungroup() |>
#     filter(n == max(n)) |>
#     # filter out models that don't have all horizons
#     group_by_at(unit_observation) |>
#     ungroup(horizon) |>
#     mutate(n = length(unique(horizon))) |>
#     ungroup() |>
#     filter(n == max(n)) 
#   
#   return(h)
# }
# hub_data <- filter_hub_data(hub_data)


filter_out_anomalies <- function(hub_data) {
  anomalies_file <- here::here("data", "anomalies.csv")
  if (file.exists(anomalies_file)) {
    anomalies <- data.table::fread(file = anomalies_file)
  } else {
    ## get anomalies file at date of last data made
    owner <- "covid19-forecast-hub-europe"
    repo <- "covid19-forecast-hub-europe"
    path <- "data-truth/anomalies/anomalies.csv"
    commit <- gh::gh(
      "/repos/{owner}/{repo}/commits?path={path}&until={date}",
      owner = owner,
      repo = repo,
      path = path,
      until = analysis_date,
      .limit = 1
    )
    anomalies <- data.table::fread(input = URLencode(URL = paste(
      "https://raw.githubusercontent.com", owner, repo, commit[[1]]$sha, path, sep = "/"
    )))
    data.table::fwrite(x = anomalies, file = anomalies_file)
  }
  
  ## prepare for mergeing into hub_data
  anomalies <- anomalies[, list(
    target_end_date,
    location,
    target_type = paste0(stringr::str_to_title(stringr::str_remove(
      string = target_variable, pattern = "^inc ")), "s")
  )]
  
  setDT(hub_data)
  hub_data <- hub_data[!anomalies, on = .(target_end_date, location, target_type)]
  hub_data <- hub_data[true_value >= 0][]
  return(hub_data)
}
hub_data <- filter_out_anomalies(hub_data)

filter_models <- function(hub_data) {
  check <- hub_data |> 
    check_forecasts()
  
  filtermodels <- check$unique_values |>
    filter(horizon >= 4, 
           target_type == 2,
           true_value > 900,
           quantile >= 23, 
           forecast_date > 92/2,
           location == 32) |>
    pull(model) |>
    sort()
  
  # remove models that don't have 23 quantiles everywhere
  remove <- hub_data |> 
    group_by(model, forecast_date, location, target_type) |>
    summarise(q = length(unique(quantile))) |>
    filter(q < 23) |>
    pull(model) |>
    unique() 
  
  filtermodels <- setdiff(filtermodels, remove)
  
  hub_data |>
    filter(model %in% filtermodels)
}
hub_data <- filter_models(hub_data)

hub_data <- filter(hub_data, target_type != "")


# Remove spurious forecasts. We define these as forecasts with a median of 0
# but with an actual observed value of larger than 150
remove <- hub_data |> 
  filter(quantile == 0.5) |>
  filter((true_value > 10) & (prediction > 20*true_value) |
         (true_value > 0) & (prediction > 100*true_value) |
         (true_value > 50) & (prediction < 1/50 * true_value) |
         (true_value == 0) & (prediction > 100)) |>
  select(-quantile)
remove[, forecast_date := as.Date(forecast_date)]
setDT(hub_data)
hub_data[, forecast_date := as.Date(forecast_date)]
hub_data <- hub_data[!remove, on = .(target_end_date, true_value, forecast_date, location, target_type, model, horizon)]

split_data <- function(data, n_splits) {
  n <- floor(nrow(data) / n_splits)
  indices <- list()
  for (i in 1:n_splits) {
    indices[[i]] <- seq(
      from = (i-1) * n + 1, 
      to = (i * n)
    )
  }
  indices[[n_splits]] <- c(
    indices[[n_splits]], 
    seq(max(indices[[n_splits]]) + 1, nrow(hub_data))
  )
  return(indices)
}


indices <- split_data(hub_data, 4)

for (i in 1:length(indices)) {
  fwrite(hub_data[indices[[i]]], 
         file = paste0("data/full-data-european-forecast-hub-", i, ".csv"))
}



## Score forecasts
scores <- hub_data |>
  mutate(scale = "natural") |>
  rbind(hub_data |>
          mutate(
            scale = "log", 
            true_value = log(true_value + 1), 
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  rbind(hub_data |>
          mutate(scale = "sqrt", 
                 true_value = sqrt(true_value), 
                 prediction = sqrt(prediction))) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("model", "location",
                          "target_end_date", "forecast_date",
                          "horizon", "target_type", "scale"), 
                   na.rm = TRUE)

scores[, type_and_scale := paste0(target_type, " - ", scale)]
scores[, type_and_scale := factor(type_and_scale, 
                                  levels = c("Cases - natural", "Deaths - natural",
                                             "Cases - log", "Deaths - log", 
                                             "Cases - sqrt", "Deaths - sqrt"))]

scores <- scores[, type_and_scale := factor(type_and_scale, 
                                            levels = c("Cases - natural", "Cases - log", 
                                                       "Deaths - natural", "Deaths - log"))]
scores <- scores[, scale := factor(scale, levels = c("natural", "log", "sqrt"))]

add_median_forecast <- function(scores, hub_data) {
  medians <- hub_data |>
    filter(quantile == 0.5) |> 
    rename(median_prediction = prediction) |>
    select(-quantile)
  
  out <- scores |>
    inner_join(medians)
  
  return(out)
}
scores <- add_median_forecast(scores, hub_data)

fwrite(scores, here("output", "data", "all-scores-european-hub.csv"))

