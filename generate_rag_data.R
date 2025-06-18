library(here)
library(DBI)
library(RSQLite)

db_path <- here::here("data", "crm-db.sqlite")
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

churn_data <- dplyr::tbl(con, "churn_predictions") |>
  dplyr::collect()

client_data <- dplyr::tbl(con, "clients") |>
  dplyr::collect()

DBI::dbDisconnect(con)
model_version_sel <- get_last_model_version(board, "xgb_model")
model_sel <- pins::pin_read(board = board, name = "xgb_model", version = model_version_sel)
model_explainer <- restore_explainer(board, "xgb_model", "churn", version = model_version_sel)
features <- model_sel$features


# 2. Generate “Reason Codes” or Short Textual Explanations

# Bankers appreciate business-language explanations, e.g.:
# 	•	“Flagged due to recent drop in assets under management and lack of contact in last 90 days.”

# How to do this:
# 	•	Set up a mapping from top-contributing features to human-readable “reason codes.”
# 	•	If a feature crosses a threshold or is top-ranked for a client, attach its mapped reason.