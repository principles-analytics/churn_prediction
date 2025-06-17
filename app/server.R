library(shiny)
library(ellmer)
library(shinychat)
library(DT)
library(shinychat)

db_path <- here::here("data", "crm-db.sqlite")

# Define server logic
function(input, output, session) {

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  churn_data <- dplyr::tbl(con, "churn_predictions") |>
    dplyr::left_join(
      dplyr::tbl(con, "clients"), 
      by = "client_id"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(name = paste(last_name, first_name)) 
  
  main_table <- churn_data |>
    dplyr::group_by(client_id) |>
    dplyr::arrange(desc(prediction_timestamp)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  DBI::dbDisconnect(con)

  sel_cols <- c("last_name", "first_name", "income", "products_held", "churn")

  output$total_clients <- renderText({
    length(unique(churn_data$client_id))
  })

  output$low_churn_probability <- renderText({
    churn_data |>
      dplyr::filter(risk_class == "low") |>
      nrow()
  })

  output$median_churn_probability <- renderText({
    churn_data |>
      dplyr::filter(risk_class == "medium") |>
      nrow()
  })

  output$high_churn_probability <- renderText({
    churn_data |>
      dplyr::filter(risk_class == "high") |>
      nrow()
  })

  output$client_table <- DT::renderDataTable({
    main_table  |>
      dplyr::select(name, income, products_held, risk_class) |>
      dplyr::mutate(
        risk_class = dplyr::case_when(
          risk_class == "low" ~ sprintf('<div style="display: inline-block; background:#79ce42; color: black; border-radius: 10px; padding: 4px 12px; width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "medium" ~ sprintf('<div style="display: inline-block; background:#eaab72; color: black; border-radius: 10px; padding: 4px 12px; min-width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "high" ~ sprintf('<div style="display: inline-block; background:#f15353; color: black; border-radius: 10px; padding: 4px 12px; min-width: 80px; text-align:center;">%s</div>', risk_class),
          risk_class == "not-at-risk" ~ sprintf('<div style="display: inline-block; background:#e5e5e5; black: white; border-radius: 10px; padding: 4px 12px; min-width: 80px; text-align:center;">%s</div>', risk_class),
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
        escape = FALSE,
        style = "bootstrap4",
        class = "cell-border stripe",
        rownames = FALSE,
        colnames = c("Name", "Income", "# Products", "Churn Risk"),
        selection = list(mode = "single", selected = 1)
        #caption = "Client List with predicted churn"
      )
  })

  output$churn_details <- DT::renderDataTable({

    key_info <- main_table |>
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
        #caption = "Client List with predicted churn"
      )
  })

  chat <- ellmer::chat_ollama(
    system_prompt = "You're a helpful assistant that can help with churn inspection.",
    model = "deepseek-r1"
  )

  chat_mod_server("chat", chat)
} 