library(pins)
library(dplyr)
library(DALEX)
library(DALEXtra)

restore_explainer <- function(board, pin_name, y, version) {
  model <- pin_read(board, pin_name, version = version)
  model_fit <- model$model
  data <- model$data
  features <- model$features
  DALEXtra::explain_tidymodels(
    model_fit, 
    data = data |> dplyr::select(dplyr::all_of(features)) |> dplyr::select(-!!y), 
    y = as.integer(data[[y]]),
    verbose = FALSE
  )
}

get_last_model_version <- function(board, pin_name) {
  board |> 
    pins::pin_versions(pin_name) |>
    dplyr::arrange(desc(created)) |>
    dplyr::slice(1) |>
    dplyr::pull(version)
}