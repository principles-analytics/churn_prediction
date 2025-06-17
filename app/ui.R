library(shiny)
library(ellmer)
library(shinychat)
library(DT)
library(shinychat)

# Define UI for application
fluidPage(
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .title-panel {
        background-color: #081c32;
        color: white;
        padding: 40px;
        margin-bottom: 30px;
      }
    "))
  ),
  
  # Application title
  div(class = "title-panel",
    titlePanel("Churn Prediction Mitigation Tool")
  ),
  
  # Tabset Panel
  tabsetPanel(
    # Tracking Statistics Tab
    tabPanel("Tracking Statistics",
      br(),
      fluidRow(
        column(3,
          div(class = "well",
            h4("Total Clients"),
            textOutput("total_clients")
          )
        ),
        column(3,
          div(class = "well", 
            h4("Low Risk"),
            textOutput("low_churn_probability")
          )
        ),
        column(3,
          div(class = "well",
            h4("Median Risk"),
            textOutput("median_churn_probability")
          )
        ),
        column(3,
          div(class = "well",
            h4("High Risk"),
            textOutput("high_churn_probability") 
          )
        )
      ),
      fluidRow(
        column(7,
          h3("Churn Tracking Results"),
          DT::dataTableOutput("client_table")
        ),
        column(5,
          h3("Client Details"),
          DT::dataTableOutput("churn_details")
        )
      )
    ),
    
    # Churn Agent Tab
    tabPanel("Churn Agent",
      fluidRow(
        column(12,
          h3("Churn Mitigation Agent"),
          chat_mod_ui("chat", height = "500px")
        )
      )
    )
  )
)