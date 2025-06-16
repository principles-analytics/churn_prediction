
data_iranian <- readr::read_csv("data/iranian_customer_churn.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(churn = as.factor(churn))

lr_auc <- run_logistic_regression(data_iranian)
lrp_auc <- run_penalised_logistic_regression(data_iranian)
rf_auc <- run_random_forest(data_iranian)
bdt_auc <- run_xgboost(data_iranian)

bind_rows(lr_auc$roc_curve, lrp_auc$roc_curve, rf_auc$roc_curve, bdt_auc$roc_curve) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)