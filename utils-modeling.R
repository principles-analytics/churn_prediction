# Functions to simplify the churn modeling process
# Templated approach to each model.

tune_parameters <- function(workflow, resamples, grid) {

  tuning_results <- workflow |> 
    tune_grid(
      resamples = resamples,
      grid = grid,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc)
    )

  best_params <- tuning_results |> 
    select_best(metric = "roc_auc")
  
  wflow <- workflow |>
    finalize_workflow(best_params)

  return(
    list(
      "tuning_results" = tuning_results,
      "best_params" = best_params,
      "final_workflow" = wflow
    )
  )
}

fit_model <- function(train_data, work_flow) {

  fit <- 
    work_flow |> 
    fit(data = train_data)

  fit_results <- fit |> 
    extract_fit_parsnip()

  return(
    list(
      "fit" = fit,
      "fit_results" = fit_results
    )
  )
}

assess_model_performance <- function(fit, test_data, model_label) {

  churn_aug <- fit |>
    augment(test_data)

  # confusion matrix
  conf <- conf_mat(churn_aug, churn, .pred_class)
  
  # conf_matrix <- matrix(conf$value, byrow = TRUE, ncol = 2)
  # rownames(conf_matrix) <- levels(churn_aug$churn)
  # colnames(conf_matrix) <- levels(churn_aug$churn)

  # sensitivity, specificity, precision, recall, f1 score

  sensitivity <- yardstick::sens(churn_aug, truth = churn, estimate = .pred_class, event_level = "second")
  specificity <- yardstick::spec(churn_aug, truth = churn, estimate = .pred_class, event_level = "second")
  perf_metrics <- churn_aug |>
    metrics(churn, .pred_class) |>
    bind_rows(sensitivity, specificity) |> 
    dplyr::mutate(model = model_label)

  # receiver operating characteristic curve
  roc_curve <- churn_aug |>
    roc_curve(churn, .pred_1, event_level = "second") |> 
    dplyr::mutate(model = model_label)

  roc_curve_plot <- roc_curve |> 
    autoplot()

  roc_auc_value <- churn_aug |>
    roc_auc(truth = churn, .pred_1, event_level = "second")

  # lift curve
  lift <- churn_aug |>
    lift_curve(churn, .pred_1, event_level = "second")

  lift_curve_plot <- lift |> 
    autoplot()

  return(
    list(
      "predictions" = churn_aug,
      "conf_matrix" = conf,
      "perf_metrics" = perf_metrics,
      "roc_curve" = roc_curve,
      "roc_curve_plot" = roc_curve_plot,
      "roc_auc_value" = roc_auc_value,
      "lift_curve" = lift,
      "lift_curve_plot" = lift_curve_plot
    )
  )
}

ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(
    metric_name, 
    "after permutations\n(higher indicates more important)"
  )
  
  full_vip <- dplyr::bind_rows(obj) %>%
    dplyr::filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    dplyr::filter(variable == "_full_model_") %>% 
    dplyr::group_by(label) %>% 
    dplyr::summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    dplyr::filter(variable != "_full_model_") %>% 
    dplyr::mutate(variable = forcats::fct_reorder(variable, dropout_loss)) %>%
    ggplot2::ggplot(aes(dropout_loss, variable))

  if(length(obj) > 1) {
    p <- p + 
      ggplot2::facet_wrap(vars(label)) +
      ggplot2::geom_vline(
        data = perm_vals, 
        aes(xintercept = dropout_loss, color = label),
        linewidth = 1.4, 
        lty = 2, 
        alpha = 0.7) +
      ggplot2::geom_boxplot(ggplot2::aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p + 
      ggplot2::geom_vline(
        data = perm_vals, 
        aes(xintercept = dropout_loss),
        linewidth = 1.4, 
        lty = 2, 
        alpha = 0.7) +
      ggplot2::geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = metric_lab, 
      y = NULL,  
      fill = NULL,  
      color = NULL)
}

model_explainability <- function(fit, data) {
  
  features <- data |> 
    dplyr::select(-churn)

  explainer <- 
    DALEXtra::explain_tidymodels(
      fit, 
      data = features, 
      y = as.integer(data$churn),
      verbose = FALSE
    )
  
  global_shap <- 
    DALEX::model_parts(
      explainer = explainer, 
      loss_function = DALEX::loss_root_mean_square 
    )

  plot <- ggplot_imp(global_shap)

  return(
    list(
      "explainer" = explainer, 
      "global_shap" = global_shap, 
      "plot" = plot
    )
  )

  # we coild also wprk on some global explanations from local explanations
  # DALEX::model_profile()
}

run_logistic_regression <- function(data) { 
  
  set.seed(222)

  # Use strata to keep the class distribution in the splits
  data_split <- initial_split(data, strata = churn, prop = 0.75)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # tidymodel style: recipe, model and workflow
  train_data_rec <- 
    recipe(churn ~ ., data = train_data)
  
  lr_mod <- 
    logistic_reg() |>
    set_engine("glm")

  wflow <- 
    workflow() |>
    add_model(lr_mod) |> 
    add_recipe(train_data_rec)

  # let's fit the model
  churn_fit <- fit_model(train_data, wflow)

  # let's predict the churn for the test data
  churn_fit_perf <- assess_model_performance(churn_fit$fit, test_data, "Logistic Regression")

  # let's get some global model explainability
  explainability <- model_explainability(churn_fit$fit, test_data)

  return(
    list(
      "test_data" = test_data,
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot,
      "explainability" = explainability
    )
  )

}

run_penalised_logistic_regression <- function(data) {
  
  set.seed(222)

  # Use strata to keep the class distribution in the splits
  data_split <- initial_validation_split(data, strata = churn, prop = c(0.6, 0.2))
  train_data <- training(data_split)
  test_data  <- testing(data_split)

  # tidymodel style: recipe, model and workflow
  lrp_mod <- 
    logistic_reg(penalty = tune(), mixture = 1) |>
    set_engine("glmnet")

  lrp_recipe <- 
    recipe(churn ~ ., data = train_data) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_predictors())

  lrp_workflow <- 
    workflow() |>
    add_model(lrp_mod) |>
    add_recipe(lrp_recipe)

  tuning_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

  # let's tune the parameters
  tuning <- tune_parameters(lrp_workflow, validation_set(data_split), tuning_grid)
  wflow <- tuning$final_workflow

  # let's fit the model
  churn_fit <- fit_model(train_data, wflow)

  # let's predict the churn for the test data
  churn_fit_perf <- assess_model_performance(churn_fit$fit, test_data, "Penalized Logistic Regression")

  # let's get some global model explainability
  explainability <- model_explainability(churn_fit$fit, test_data)

  return(
    list(
      "test_data" = test_data,
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot,
      "explainability" = explainability,
      "explainability" = explainability
    )
  )

}

run_random_forest <- function(data) {

  set.seed(222)

  # Use strata to keep the class distribution in the splits
  data_split <- initial_validation_split(data, strata = churn, prop = c(0.6, 0.2))
  train_data <- training(data_split)
  test_data  <- testing(data_split)

  # tidymodel style: recipe, model and workflow
  rf_mod <- 
    rand_forest(
      mtry = tune(), 
      min_n = tune(), 
      trees = 1000
    ) |> 
    set_engine("ranger", num.threads = cores) |> 
    set_mode("classification")

  rf_recipe <- 
    recipe(churn ~ ., data = train_data)

  rf_workflow <- 
    workflow() |> 
    add_model(rf_mod) |> 
    add_recipe(rf_recipe)

   # let's tune the parameters
  tuning <- tune_parameters(rf_workflow, validation_set(data_split), 25)
  wflow <- tuning$final_workflow

  # let's fit the model
  churn_fit <- fit_model(train_data, wflow)

  # let's predict the churn for the test data
  churn_fit_perf <- assess_model_performance(churn_fit$fit, test_data, "Random Forest")

  # let's get some global model explainability
  explainability <- model_explainability(churn_fit$fit, test_data)

  return(
    list(
      "test_data" = test_data,
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot,
      "explainability" = explainability
    )
  )

}

run_xgboost <- function(data) {

  set.seed(222)

  # Put 80% of the data into the training set 
  # Use strattification to keep the class distribution in the training set
  data_split <- initial_validation_split(data, strata = churn, prop = c(0.6, 0.2))

  # Create data frames for the two sets:
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  val_set <- validation(data_split) # to assess model performance

  bdt_mod <- boost_tree(
    mode = "classification", 
    trees = 1000, 
    tree_depth = tune(), 
    min_n = tune(), 
    mtry = tune(), 
    learn_rate = tune()) |>
    set_engine("xgboost")

  bdt_recipe <- 
    recipe(churn ~ ., data = train_data)

  bdt_workflow <- 
    workflow() |>
    add_model(bdt_mod) |>
    add_recipe(bdt_recipe)

  # let's tune the parameters
  tuning <- tune_parameters(bdt_workflow, validation_set(data_split), 25)
  wflow <- tuning$final_workflow

  # let's fit the model
  churn_fit <- fit_model(train_data, wflow)

  # let's predict the churn for the test data
  churn_fit_perf <- assess_model_performance(churn_fit$fit, test_data, "XGBoost")

  # let's get some global model explainability
  explainability <- model_explainability(churn_fit$fit, test_data)

  return(
    list(
      "test_data" = test_data,
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot,
      "explainability" = explainability
    )
  )
}



predict_churn <- function(data) {

  model_version <- board |> 
    pins::pin_versions(model_name) |>
    dplyr::arrange(desc(created))

  predictions <- purrr::map_dfr(
    .x = model_version$version,
    .f = function(x) {
   
      model_sel <- pins::pin_read(board = board, name = model_name, version = x)

      cat("model_sel", class(model_sel$model), "\n")

      pred <- model_sel$model |>
        parsnip::augment(data) |>
        dplyr::select(client_id, churn = ".pred_class", prediction_prob = .pred_1) |>
        dplyr::mutate(model_name = model_name) |>
        dplyr::mutate(model_version = x) |>
        dplyr::mutate(risk_class = dplyr::case_when(
          churn == 1 & prediction_prob > 0.2 & prediction_prob < 0.4 ~ "low",
          churn == 1 & prediction_prob >= 0.4 & prediction_prob < 0.7 ~ "medium",
          churn == 1 & prediction_prob >= 0.7 & prediction_prob < 1.0 ~ "high",
          churn == 0 & prediction_prob >= 0.2 & prediction_prob < 0.6 ~ "low",
          TRUE ~ "not-at-risk"
        )) |>
        dplyr::mutate(prediction_timestamp = Sys.time())
      
    })

  return(predictions)
}

# to time expensive for now. Will be calculated on 
# the fly in the app.
add_explanability <- function(data, predictions) {

  model_version_sel <- get_last_model_version(board, model_name)
  model_sel <- pins::pin_read(board = board, name = model_name, version = model_version_sel)
  model_explainer <- restore_explainer(board, model_name, "churn", version = model_version_sel)
  features <- model_sel$features

  sel_cols <- c(features, "churn")

  predictions <- predictions |>
    dplyr::left_join(data, by = "client_id") |>
    dplyr::mutate(churn = as.factor(churn))

  explainability <- purrr::map_dfr(
    .x = predictions$client_id,
    .f = function(x) {

      sel_client <- predictions |>
        dplyr::filter(client_id == x) |>
        dplyr::select(dplyr::all_of(sel_cols))

      shap_summary <- DALEX::predict_parts(
        explainer = model_explainer, 
        new_observation = sel_client[1, ], 
        type = "shap",
        B = 20
      ) |>
      tibble::as_tibble() |>
      dplyr::group_by(variable_name, variable_value) |>
      dplyr::summarise(
        mean_contribution = mean(contribution),
        median_contribution = median(contribution),
        min_contribution = min(contribution),
        max_contribution = max(contribution),
        q1 = quantile(contribution, 0.25),
        q3 = quantile(contribution, 0.75),
        .groups = "drop"
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        mean_contribution = round(mean_contribution, 2),
        median_contribution = round(median_contribution, 2),
        min_contribution = round(min_contribution, 2),
        max_contribution = round(max_contribution, 2),
        q1 = round(q1, 2),
        q3 = round(q3, 2)
      ) |>
      dplyr::arrange(desc(mean_contribution)) |>
      dplyr::select(
        Feature = "variable_name", 
        Value = "variable_value", 
        Mean = "mean_contribution", 
        Median = "median_contribution",
        Min = "min_contribution",
        Max = "max_contribution",
        Q1 = "q1",
        Q3 = "q3"
      ) |>
      dplyr::mutate(
        client_id = x,
        model_name = model_name,
        model_version = model_version_sel
      )

    
    }
  )

  return(explainability)
}