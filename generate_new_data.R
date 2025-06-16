set.seed(42)
n <- 5000

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

generate_new_data <- function(n) {
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
    transactions = transactions,
    contacts_with_advisor = contact_with_advisor,
    customer_satisfaction = customer_satisfaction,
    products_held = products_held,
    age = age,
    income = income,
    churn = as.factor(churn)
  )
}

data <- generate_new_data(n)

# Sauvegarde
readr::write_excel_csv(data, "data/churn_data_synth.csv")