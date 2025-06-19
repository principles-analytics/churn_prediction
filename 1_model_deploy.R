
# we retrieve the latest model version from the
# pins board and expose a vetiver model through
# a plumber API.

# we use vetiver for all the convenient utilities
# and MLOps approach in the future.

# We also expose a explainer endpoint to the plumber API.

source(here::here("env-setup.R"))

version <- get_last_model_version(board, model_name)
model <- pins::pin_read(board = board, name = model_name, version = version)
features <- model$features
y <- "churn"

model_version <- board |> 
  pins::pin_versions(model_name_vetiver) |>
  dplyr::arrange(desc(created)) |>
  dplyr::slice(1) |>
  dplyr::pull(version)

model_vetiver <- board |>
  vetiver::vetiver_pin_read(
    name = model_name_vetiver, 
    version = model_version
  )

model_explainer <- DALEXtra::explain_tidymodels(
  model_vetiver, 
  data = model$data |> dplyr::select(dplyr::all_of(features)) |> dplyr::select(-!!y), 
  y = as.integer(model$data[[y]])
)

handler_explain <- function(req) {

  shap <- DALEX::predict_parts(
    explainer = model_explainer,
    new_observation = req$body,
    type = "shap",
    B = 20
  )

  shap %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(mean_val = mean(contribution), sd_val = sd(contribution)) %>%
    dplyr::arrange(mean_val)
}

#* @plumber
function(pr) {
  pr %>%
    vetiver::vetiver_api(model_vetiver) %>%
    plumber::pr_post(path = "/explain", handler = handler_explain)
}


## Serve the model through a plumber API
## run in console
# plumber::plumb("1_model_deploy.R") |> plumber::pr_run(port = 8080, host = "0.0.0.0")
## run in terminal
# Rscript -e 'plumber::plumb("1_model_deploy.R") |> plumber::pr_run(port = 8080, host = "0.0.0.0")'