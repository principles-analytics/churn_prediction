source(here::here("env-setup.R"))

# Load the data

data <- readr::read_delim(fpath_data, delim = ";", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(churn = as.factor(churn)) |>
  dplyr::select(-client_id)

# Fit the model (we assume final model is the one with the best AUC)

bdt_auc <- run_xgboost(data)

# Save the model

features <- c(
  "transactions", 
  "contacts_with_advisor", 
  "customer_satisfaction", 
  "products_held", 
  "age", 
  "income",
  "churn")

model_object <- list( 
  "model" = bdt_auc$fit, 
  "features" = features, 
  "data" = bdt_auc$test_data
)

pins::pin_write(
  board, 
  x = model_object, 
  name = model_name, 
  type = "rds", 
  versioned = TRUE
)

## vetiver approach to expose on plumber API

vetiver_model <- vetiver::vetiver_model(
  model = bdt_auc$fit,
  model_name = model_name_vetiver,
  description = "XGBoost model for churn prediction. Model trained on 60% of the data \\
  and hyperparameters tuned on 20% of the data. 20% of the data is used for final performancetesting",
  save_prototype = TRUE
)

vetiver::vetiver_pin_write(board, vetiver_model)