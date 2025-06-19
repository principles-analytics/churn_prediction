library(shiny)
library(ellmer)
library(shinychat)
library(DT)
library(shinychat)
library(shinythemes)

# Define UI for application
navbarPage(
  title = "Churn Management",
  theme = shinytheme("flatly"),
  header = includeCSS("www/styles.css"),
  
  tabPanel("Tracking Statistics",
    br(),
    fluidRow(
      column(3,
        div(class = "well", style = "background-color: #2c3e50; color: white; border-radius: 10px; border-color: #cbe0f6; border-width: 5px;",
          h4("Total Clients"),
          textOutput("total_clients")
        )
      ),
      column(3,
        div(class = "well", style = "background-color: #2c3e50; color: white;border-radius: 10px; border-color: #da3b55; border-width: 5px;",
          h4("High Risk Churn"),
          textOutput("high_churn_probability") 
        )
      ),
      column(3,
        div(class = "well", style = "background-color: #2c3e50; color: white;border-radius: 10px; border-color: #e4950c; border-width: 5px;",
          h4("Median Risk Churn"),
          textOutput("median_churn_probability")
        )
      ),
      column(3,
        div(class = "well", style = "background-color: #2c3e50; color: white;border-radius: 10px; border-color: #84c522; border-width: 5px;", 
          h4("Low Risk Churn"),
          textOutput("low_churn_probability")
        )
      )
    ),
    fluidRow(
      column(12,
        checkboxGroupInput(
          "sel_risk_profile", 
          "Select Risk Profile",
          choices = c("High Risk", "Medium Risk", "Low Risk", "Not at Risk"),
          selected = c("High Risk", "Medium Risk", "Low Risk"),
          inline = TRUE
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
    ),
    hr(),
    br(),
    fluidRow(
      column(6,
        h3("SHAP Values"),
        DT::dataTableOutput("shap_values")
      ),
      column(6,
        h3("SHAP Plot"),
        plotOutput("plot_shap")
      )
    )
  ),
  
  # Churn Agent Tab
  tabPanel("Churn Agent",
    fluidRow(
      column(12,
        uiOutput("client_select"),
        div(
          class = "well",
          style = "background-color: #ecf0f1;",
          h3("Churn Mitigation Agent", style = "text-align: center;"),
          h5(
            "Let's discuss your client's situation and see if we can help them stay with us.", 
            style = "text-align: center;"
          ),
          h5(textOutput("selected_client"), style = "text-align: center; font-weight: bold;"),
          shinychat::chat_ui("chat", height = "500px")
        )
      )
    )
  )
)