model_name <- "churn_model_xgboost"

board <- pins::board_folder("./model_versions")

model_version <- board |> 
  pins::pin_versions(model_name) |>
  dplyr::arrange(desc(created)) |>
  dplyr::slice(1) |>
  dplyr::pull(version)

model <- vetiver::vetiver_pin_read(board, model_name, version = model_version)

# deploy the model on a local server

plumber::pr() |>
  vetiver::vetiver_api(model) |>
  plumber::pr_run(port = 8080, host = "0.0.0.0")

# endpoint <- vetiver_endpoint("http://127.0.0.1:8080/predict")
# new_car <- tibble(cyl = 4,  disp = 200, hp = 100, drat = 3, wt = 3,   qsec = 17, vs = 0,   am = 1, gear = 4, carb = 2)
# predict(endpoint, new_car)
