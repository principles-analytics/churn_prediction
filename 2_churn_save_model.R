library(pins)
#library(vetiver)

source("utils.R")
source("1_churn_modeling.R")

fpath <- "data/churn_data.csv"
board <- pins::board_folder("./model_versions", versioned = TRUE)
model_pin_name <- "xgb_model"

# Load the data

data <- readr::read_delim(fpath, delim = ";", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(churn = as.factor(churn)) |>
  dplyr::select(-client_id)

# Fit the model

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

model_version <- list( 
  "model" = bdt_auc$fit, 
  "features" = features, 
  "data" = bdt_auc$test_data
)

pins::pin_write(
  board, 
  x = model_version, 
  name = model_pin_name, 
  type = "rds", 
  versioned = TRUE
)

### vetiver approach... doesn't seem to work
# vetiver_model <- vetiver::vetiver_model(
#   model = bdt_auc$fit,
#   model_name = "churn_model_xgboost",
#   description = "XGBoost model for churn prediction. Model trained on 60% of the data \\
#   and hyperparameters tuned on 20% of the data. 20% of the data is used for final performancetesting",
#   save_prototype = TRUE
# )
# vetiver_pin_write(board, vetiver_model)
# pin_write(board, bdt_auc$test_data, "churn_model_xgboost_churn-explainer", versioned = TRUE, type = "rds")