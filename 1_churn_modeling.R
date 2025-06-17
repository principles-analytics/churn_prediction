library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) 
library(yardstick)

cores <- parallel::detectCores()

# fpath1 <- "data/churn_data.csv"
# fpath2 <- "data/churn_data_synth.csv"

# data1 <- readr::read_delim(fpath1, delim = ";") |>
#   janitor::clean_names() |>
#   dplyr::mutate(churn = as.factor(churn))

# data2 <- readr::read_csv(fpath2) |>
#   janitor::clean_names() |>
#   dplyr::mutate(churn = as.factor(churn))


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

  return(
    list(
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot
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

  return(
    list(
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot
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

  return(
    list(
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot
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

  return(
    list(
      "fit" = churn_fit$fit,
      "fit_results" = churn_fit$fit_results,
      "predictions" = churn_fit_perf$predictions,
      "conf_matrix" = churn_fit_perf$conf_matrix,
      "perf_metrics" = churn_fit_perf$perf_metrics,
      "roc_curve" = churn_fit_perf$roc_curve,
      "roc_curve_plot" = churn_fit_perf$roc_curve_plot,
      "roc_auc_value" = churn_fit_perf$roc_auc_value,
      "lift_curve" = churn_fit_perf$lift_curve,
      "lift_curve_plot" = churn_fit_perf$lift_curve_plot
    )
  )
}