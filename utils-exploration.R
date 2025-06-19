
# Functions to simplify the data exploration process
# mainly used in the Quarto report.

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

build_scatter_plots <- function(data, class_var, sel_cols) {
  GGally::ggpairs(
    data |> dplyr::mutate(churn = as.factor(churn)), 
    columns = sel_cols)
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
  scatter_plots <- build_scatter_plots(data, class_var, sel_cols)
  histograms <- build_histograms(data, excl_cols)

  return(
    list(
      missing_values = missing_values,
      class_prop = class_prop, 
      corr_matrix = corr_matrix,
      scatter_plots = scatter_plots,
      histograms = histograms
    )
  )
}