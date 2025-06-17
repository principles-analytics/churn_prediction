
generate_products <- function(n, prob_values = c(0.25, 0.25, 0.20, 0.15, 0.07, 0.05, 0.02, 0.01)) {
  sample(1:8, n, replace = TRUE, prob = prob_values)
}

generate_age <- function(n, mean = 50, sd = 12) {
  age <- round(rnorm(n, mean, sd))
  age[age < 18] <- 18
  age[age > 90] <- 90
  return(age)
}

generate_income <- function(n, products_held, age) {
  income <- round(25000 + 2000 * products_held + 600 * age + rnorm(n, 0, 15000))
  income[income < 15000] <- 15000
  return(income)
}

generate_transactions <- function(n, products_held) {
  transactions <- round(rnorm(n, mean = 12 * products_held, sd=7))
  transactions[transactions < 0] <- 0
  return(transactions)
}

generate_customer_satisfaction <- function(n, products_held) {
  customer_satisfaction <- pmax(1, pmin(5, round(5 - 0.2 * products_held + rnorm(n, 0, 1))))
  return(customer_satisfaction)
}

generate_contact_with_advisor <- function(n, customer_satisfaction) {
  contact_with_advisor <- round(5 - customer_satisfaction + rpois(n, 2))
  contact_with_advisor[contact_with_advisor < 0] <- 0
  return(contact_with_advisor)
}

generate_names <- function(n) {
  names <- randomNames::randomNames(n)
  last_name <- vapply(strsplit(names, ", "), function(x) x[1], FUN.VALUE = character(1))
  first_name <- vapply(strsplit(names, ", "), function(x) x[2], FUN.VALUE = character(1))
  return(
    list(
      "last_name" = last_name,
      "first_name" = first_name
    )
  )
}

generate_client_data <- function(n) {

  names <- generate_names(n)
  last_name <- names$last_name
  first_name <- names$first_name
  products_held <- generate_products(n)
  age <- generate_age(n)
  income <- generate_income(n, products_held, age)
  transactions <- generate_transactions(n, products_held)
  contact_with_advisor <- generate_contact_with_advisor(n, products_held)
  customer_satisfaction <- generate_customer_satisfaction(n, products_held)

  # Churn: proba plus forte si peu de produits, faible satisfaction, faible revenu
  churn_proba <- plogis(-1 + 0.9 * (products_held < 2) + 1.4 * (customer_satisfaction <= 2) - 0.00001 * income)
  churn <- rbinom(n, 1, churn_proba)

  tibble::tibble(
    client_id = 1:n,
    last_name = last_name,
    first_name = first_name,
    transactions = transactions,
    contacts_with_advisor = contact_with_advisor,
    customer_satisfaction = customer_satisfaction,
    products_held = products_held,
    age = age,
    income = income
  )
}

predict_churn <- function(data) {
  model_name <- "churn_model_xgboost"
  model_explainer <- "churn_model_xgboost_churn-explainer"
  board <- pins::board_folder("./model_versions")

  model_version <- board |> 
    pins::pin_versions(model_name) |>
    dplyr::arrange(desc(created)) 
  
  predictions <- purrr::map_dfr(
    .x = model_version$version,
    .f = function(x) {
      model <- board |>
        vetiver::vetiver_pin_read(
          name = model_name, 
          version = x
        )
      model |>
        parsnip::augment(data) |>
        dplyr::select(client_id, churn = ".pred_class", prediction_prob = .pred_0) |>
        dplyr::mutate(model_name = model_name) |>
        dplyr::mutate(model_version = x) |>
        dplyr::mutate(risk_class = dplyr::case_when(
          churn == 1 & prediction_prob < 0.4 ~ "low",
          churn == 1 & prediction_prob < 0.7 ~ "medium",
          churn == 1 & prediction_prob < 1.0 ~ "high",
          churn == 0 & prediction_prob < 0.6 ~ "low",
          TRUE ~ "not-at-risk"
        )) |>
        dplyr::mutate(prediction_timestamp = Sys.time())
    })

  return(predictions)
}

save_to_db <- function(data, db_path, tbl_name) {
  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, tbl_name, data, overwrite = TRUE)
}

set.seed(42)
n <- 5000
data <- generate_client_data(n)
predictions <- predict_churn(data)
table_name_clients <- "clients"
table_name_predictions <- "churn_predictions"
db_path <- here::here("data", "crm-db.sqlite")
save_to_db(data, db_path, table_name_clients)
save_to_db(predictions, db_path, table_name_predictions)