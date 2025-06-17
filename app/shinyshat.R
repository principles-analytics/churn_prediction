library(shiny)
library(bslib)
library(ellmer)
library(promises)
library(shinychat)

db_path <- here::here("data", "crm-db.sqlite")

ui <- page_sidebar(
  title = "Churn Mitigation Tool",
  sidebar = sidebar(
    textAreaInput("user_query", "Any intriguing behaviors today?"),
    input_task_button("ask_chat", label = "Generate a story")
  ),
  DT::dataTableOutput("client_table"),
  card(
    card_header(textOutput("story_title")),
    shinychat::output_markdown_stream("response"),
  )
)

server <- function(input, output) {

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  data <- DBI::dbReadTable(con, "clients")
  DBI::dbDisconnect(con)

  output$client_table <- DT::renderDataTable({
    data
  })

  chat_task <- ExtendedTask$new(function(user_query) {
    # We're using an Extended Task for chat completions to avoid blocking the
    # app. We also start the chat fresh each time, because the UI is not a
    # multi-turn conversation.
    chat <- chat_ollama(
      system_prompt = "You are a helpful assistant that can help with churn inspection.",
      model = "deepseek-r1"
    )

    # Stream the chat completion into the markdown stream. `markdown_stream()`
    # returns a promise onto which we'll chain the follow-up task of providing
    # a story title.
    stream <- chat$stream_async(user_query)
    stream_res <- shinychat::markdown_stream("response", stream)

    # Follow up by asking the LLM to provide a title for the story that we
    # return from the task.
    stream_res$then(function(value) {
      chat$chat_async(
        "Any intriguing behaviors today? Reply with only the information and nothing else."
      )
    })
  })

  bind_task_button(chat_task, "ask_chat")

  observeEvent(input$ask_chat, {
    chat_task$invoke(input$user_query)
  })

  observe({
    # Update the card title during generation and once complete
    switch(
      chat_task$status(),
      success = story_title(chat_task$result()),
      running = story_title("Searching clients' history..."),
      error = story_title("An error occurred while generating your story.")
    )
  })

  story_title <- reactiveVal("Relevant information will appear here!")
  output$story_title <- renderText(story_title())
}

shinyApp(ui = ui, server = server)
