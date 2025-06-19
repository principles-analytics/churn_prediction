suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ellmer))
suppressPackageStartupMessages(library(shinychat))  
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinychat))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(DALEX))
suppressPackageStartupMessages(library(DALEXtra))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(randomNames))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(markdown))

source(here::here("utils-pins.R"))

db_path <- here::here("data", "crm-db.sqlite")
board <- pins::board_folder(here::here("model_versions"))

prompt_template <- paste(
  readLines(file.path(here::here("prompts", "prompts-explain-churn.md"))),
  collapse = "\n"
)

chat <- ellmer::chat_openai(
  system_prompt = "You are a helpful assistant that can help with churn inspection.", # prompt
  #api_args = list(temperature = 0),
  model = "gpt-4o-mini"
)

# Define server logic
function(input, output, session) {

  model_version_sel <- get_last_model_version(board, "xgb_model")
  model_sel <- pins::pin_read(board = board, name = "xgb_model", version = model_version_sel)
  model_explainer <- restore_explainer(board, "xgb_model", "churn", version = model_version_sel)
  features <- model_sel$features

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  churn_data <- dplyr::tbl(con, "churn_predictions") |>
    dplyr::left_join(dplyr::tbl(con, "clients"), by = "client_id") |>
    dplyr::collect() |>
    dplyr::mutate(name = paste(last_name, first_name)) |>
    dplyr::filter(model_version == model_version_sel) |>
    dplyr::sample_n(1500)
  
  main_table <- churn_data

  DBI::dbDisconnect(con)

  output$total_clients <- renderText({
    length(unique(main_table$client_id))
  })

  output$low_churn_probability <- renderText({
    main_table |>
      dplyr::filter(risk_class == "low") |>
      nrow()
  })

  output$median_churn_probability <- renderText({
    main_table |>
      dplyr::filter(risk_class == "medium") |>
      nrow()
  })

  output$high_churn_probability <- renderText({
    main_table |>
      dplyr::filter(risk_class == "high") |>
      nrow()
  })

  client_table_data <- reactiveVal()

  observeEvent(input$sel_risk_profile, {
    req(input$sel_risk_profile)

    sel_risk <- c()
    if ("High Risk" %in% input$sel_risk_profile) {
      sel_risk <- c(sel_risk, "high")
    }
    if ("Medium Risk" %in% input$sel_risk_profile) {
      sel_risk <- c(sel_risk, "medium")
    }
    if ("Low Risk" %in% input$sel_risk_profile) { 
      sel_risk <- c(sel_risk, "low")
    }
    if ("Not at Risk" %in% input$sel_risk_profile) {
      sel_risk <- c(sel_risk, "not-at-risk")
    }

    client_table_data(main_table |> dplyr::filter(risk_class %in% sel_risk))

  })

  output$client_table <- DT::renderDataTable({
    client_table_data()  |>
      dplyr::select(name, income, products_held, risk_class) |>
      dplyr::mutate(
        risk_class = dplyr::case_when(
          risk_class == "low" ~ sprintf('<div style="display: inline-block; background:#84c522; color: black; border-radius: 10px; width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "medium" ~ sprintf('<div style="display: inline-block; background:#e4950c; color: black; border-radius: 10px; min-width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "high" ~ sprintf('<div style="display: inline-block; background:#da3b55; color: black; border-radius: 10px; min-width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "not-at-risk" ~ sprintf('<div style="display: inline-block; background:#cbe0f6; black: white; border-radius: 10px; min-width: 80px; text-align:center;">%s</div>', risk_class),
          TRUE ~ risk_class
        )
      ) |>
      DT::datatable(
        options = list(
          pageLength = 11,
          lengthChange = FALSE,
          columnDefs = list(list(className = "dt-center", targets = c(1, 2, 3))),
          dom = "tp"
        ),
        #filter = "bottom", # this kills the rest of the app
        escape = FALSE,
        style = "bootstrap4",
        class = "cell-border stripe",
        rownames = FALSE,
        colnames = c("Name", "Income", "# Products", "Churn Risk"),
        selection = list(mode = "single", selected = 1)
      )
  })

  output$churn_details <- DT::renderDataTable({

    key_info <- client_table_data() |>
      dplyr::slice(input$client_table_rows_selected) |>
      dplyr::select(
        name, age, income, contacts_with_advisor, 
        customer_satisfaction, products_held, churn, 
        prediction_prob, risk_class, model_name, model_version) 
    
    key_info <- tibble::as_tibble(cbind(nms = names(key_info), t(key_info)))
      
    key_info |>
      DT::datatable(
        options = list(
          pageLength = 15,
          lengthChange = FALSE,
          dom = "tp"
        ),
        class = "cell-border stripe",
        style = "bootstrap4",
        rownames = FALSE,
        colnames = c("Key", "Value")
      )
  })

  shap_calc <- reactive({
    req(input$client_table_rows_selected)
    sel_cols <- c(features, "churn")
    sel_client <- main_table |>
      dplyr::mutate(churn = as.factor(churn)) |>
      dplyr::slice(input$client_table_rows_selected) |>
      dplyr::select(dplyr::all_of(sel_cols))

    DALEX::predict_parts(
      explainer = model_explainer, 
      new_observation = sel_client[1, ], 
      type = "shap",
      B = 20
    )
  })

  output$shap_values <- DT::renderDataTable({
    
    shap_calc() |>
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
      DT::datatable(
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          dom = "tp"
        ),
        class = "cell-border stripe",
        style = "bootstrap4",
        rownames = FALSE
      )

  })

  output$plot_shap <- renderPlot({
    plot(shap_calc())
  })

  ################## AI Agent ##################

  # Initial population of choices
  
  new_names <- main_table |>
    dplyr::arrange(desc(churn), desc(prediction_prob)) |>
    dplyr::mutate(new_name = paste0(last_name, ", ", first_name, " (churn risk: ", risk_class, ")"))
  
  output$client_select <- renderUI({

    selectizeInput(
      "client_name_select",
      label = "Select a client",
      choices = unique(new_names$new_name),
      selected = new_names$new_name[1]
    )
  })

  prompt_client_data <- reactive({
    req(input$client_name_select)
    new_names |>
      dplyr::filter(new_name == input$client_name_select) |>
      t()
  })

  prompt_client_shap_data <- reactive({
    req(input$client_name_select)
    sel_cols <- c(features, "churn")

    client_sel <- new_names |>
      dplyr::filter(new_name == input$client_name_select) |>
      dplyr::select(name, risk_class)

    sel_client <- main_table |>
      dplyr::mutate(churn = as.factor(churn)) |>
      dplyr::filter(name == client_sel$name) |>
      dplyr::select(dplyr::all_of(sel_cols))

    DALEX::predict_parts(
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
      )
  })
  

  output$selected_client <- renderText({
    req(input$client_name_select)
    client_sel <- new_names |>
      dplyr::filter(new_name == input$client_name_select) |>
      dplyr::select(name, risk_class)
    
    return(
      paste0(
        "Client: ", 
        client_sel$name, 
        " (churn risk evaluated to ", 
        client_sel$risk_class, ")")
    )
  })

  observeEvent(input$client_name_select, {
    shinychat::chat_clear("chat")
  })

  observeEvent(input$chat_user_input, {

    md_client_table <- knitr::kable(prompt_client_data(), format = "markdown")
    md_client_table <- as.character(paste(md_client_table, collapse = "\n"))
    md_shap_table <- knitr::kable(prompt_client_shap_data(), format = "markdown")
    md_shap_table <- as.character(paste(md_shap_table, collapse = "\n"))

    prompt <- interpolate_file(
      here::here("prompts", "prompts-explain-churn.md"), 
      client_data = md_client_table, 
      shap_data = md_shap_table
    )

    prompt <- paste(prompt, input$chat_user_input)

    tryCatch({
      stream <- chat$stream_async(prompt)
      if (!is.null(stream)) {
        shinychat::chat_append("chat", stream)
      }
    }, error = function(e) {
      message("Error in chat stream: ", e$message)
      shinychat::chat_append("chat", "Sorry, there was an error processing your message. Please try again.")
    })
  })

}