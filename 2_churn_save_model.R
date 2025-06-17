library(pins)
library(vetiver)

source("1_churn_modeling.R")

# Load the data

fpath <- "data/churn_data.csv"

data <- readr::read_delim(fpath, delim = ";", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(churn = as.factor(churn)) |>
  dplyr::select(-client_id)

# Fit the model

bdt_auc <- run_xgboost(data)

# explain the model



set.seed(555)
shap_values <-
  DALEX::predict_parts(
    explainer = bdt_auc$explainability$explainer, 
    new_observation = data[111,], 
    type = "shap",
    B = 20
  )

plot(shap_values)

# Save the model

board <- pins::board_folder("./model_versions", versioned = TRUE)

vetiver_model <- vetiver::vetiver_model(
  model = bdt_auc$fit,
  model_name = "churn_model_xgboost",
  description = "XGBoost model for churn prediction. Model trained on 60% of the data \\
  and hyperparameters tuned on 20% of the data. 20% of the data is used for final performancetesting",
  save_prototype = TRUE
)

vetiver_pin_write(board, vetiver_model)
pin_write(board,bdt_auc$explainability$explainer, "churn_model_xgboost_churn-explainer")
