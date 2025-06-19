source(here::here("env-setup.R"))

set.seed(42)
n <- 5000
data <- generate_client_data(n)
predictions <- predict_churn(data)
table_name_clients <- "clients"
table_name_predictions <- "churn_predictions"
db_path <- here::here("data", "crm-db.sqlite")
save_to_db(data, db_path, table_name_clients)
save_to_db(predictions, db_path, table_name_predictions)