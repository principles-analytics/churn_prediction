
# Note
# Given the data, we are basically trying to predict churn based on the other variables


data <- readr::read_delim("data/churn_data.csv", delim = ";") |>
  janitor::clean_names()

# check data completeness
apply(data, 2, function(x) sum(is.na(x)))




# check class
data |> dplyr::count(churn)

