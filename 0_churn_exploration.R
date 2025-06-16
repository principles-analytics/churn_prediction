
test_missing_values <- function(data) {
  missing_values <- apply(data, 2, function(x) sum(is.na(x)))
  return(missing_values)
}

class_proportions <- function(data, class_var) {
  data |> 
    dplyr::count(!!dplyr::sym(class_var)) |>
    dplyr::mutate(prop = n / sum(n))
}

calc_corr_matrix <- function(data, excl_cols) {
  data |>
    dplyr::select(-dplyr::all_of(excl_cols)) |>
    cor()
}

# plot_corr_matrix <- function(corr_matrix) {
#   corr_matrix |>
#     corrplot::corrplot(
#       method = "number", 
#       diag = FALSE)
# }

build_scatter_plots <- function(data, class_var, sel_cols) {
  GGally::ggpairs(
    data |> dplyr::mutate(churn = as.factor(churn)), 
    columns = sel_cols)

    # mapping = ggplot2::aes(color = !!dplyr::sym(class_var)))
}

build_scatter_plot <- function(data, class_var, x, y, legend = TRUE) {
  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!dplyr::sym(x),
        y = !!dplyr::sym(y),
        color = !!dplyr::sym(class_var)
      )
    ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::labs(
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()

  if (!legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

build_histograms <- function(data, excl_cols) {
  num_vars_long <- data |>
    dplyr::select(-dplyr::all_of(excl_cols)) |>
    dplyr::select(dplyr::where(is.numeric)) |>
    tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valeur")
  histograms <- ggplot2::ggplot(num_vars_long, ggplot2::aes(x = Valeur)) +
    ggplot2::geom_histogram(bins = 20, fill = "deepskyblue3", color = "white") +
    ggplot2::facet_wrap(~ Variable, scales = "free", ncol = 2)+
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Histogrammes de toutes les variables numériques")
  return(histograms)
}

explore_data <- function(data, class_var, excl_cols, sel_cols) {

  missing_values <- test_missing_values(data)
  class_prop <- class_proportions(data, class_var)
  corr_matrix <- calc_corr_matrix(data, excl_cols)
  # corr_matrix_plot <- plot_corr_matrix(corr_matrix)
  scatter_plots <- build_scatter_plots(data, class_var, sel_cols)
  histograms <- build_histograms(data, excl_cols)

  return(
    list(
      missing_values = missing_values,
      class_prop = class_prop, 
      corr_matrix = corr_matrix,
      # corr_matrix_plot = corr_matrix_plot,
      scatter_plots = scatter_plots,
      histograms = histograms
    )
  )
}

# class_var <- "churn"
# excl_cols <- c("client_id", "churn")
# sel_cols <- c(
#   "transactions", 
#   "contacts_with_advisor", 
#   "customer_satisfaction", 
#   "products_held", 
#   "age", 
#   "income"
# )

# data_lombard <- readr::read_delim("data/churn_data.csv", delim = ";") |>
#   janitor::clean_names() |>
#   dplyr::mutate(churn = as.factor(churn))

# data_synth <- readr::read_delim("data/churn_data_synth.csv") |>
#   janitor::clean_names() |>
#   dplyr::mutate(churn = as.factor(churn))

# data_iranian <- readr::read_csv("data/iranian_customer_churn.csv") |>
#   janitor::clean_names() |>
#   dplyr::mutate(churn = as.factor(churn))

# class_var <- "churn"
# excl_cols <- c("client_id", "churn")
# sel_cols <- c(
#   "transactions", 
#   "contacts_with_advisor", 
#   "customer_satisfaction", 
#   "products_held", 
#   "age", 
#   "income"
# )

# data_lombard_explore <- explore_data(data_lombard, class_var, excl_cols, sel_cols)
# data_synth_explore <- explore_data(data_synth, class_var, excl_cols, sel_cols)



# class_var <- "churn"
# excl_cols <- c("churn")
# sel_cols <- c(
#   "call_failure", 
#   "complains", 
#   "subscription_length", 
#   "charge_amount", 
#   "seconds_of_use", 
#   "frequency_of_use",
#   "frequency_of_sms",
#   "distinct_called_numbers",
#   "age_group",
#   "tariff_plan",
#   "status",
#   "age",
#   "customer_value"
# )

# data_iranian_explore <- explore_data(data_iranian, class_var, excl_cols, sel_cols)



