library(shiny)
library(tidyverse)
library(ggplot2)

# Define UI
# Define UI
ui <- fluidPage(
  titlePanel("Durable Goods Demand Estimation"),
  
  tabsetPanel(
    # Data Tab
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Upload CSV File",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ",")
               ),
               mainPanel(
                 DT::DTOutput("data_table"),  # Updated
                 verbatimTextOutput("data_summary")
               )
             )),
    
    # Data Visualization Tab
    tabPanel("Data Visualization",
             plotOutput("scatter_plot")),
    
    # Customer Demand Tab
    tabPanel("Customer Demand",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Choose Demand Model",
                             choices = c("Linear Demand", "Exponential Demand", "Sigmoid Demand"))
               ),
               mainPanel(
                 plotOutput("demand_plot"),
                 tabsetPanel(
                   tabPanel("Interpretation",
                            verbatimTextOutput("interpretation")),
                   tabPanel("Summary",
                            verbatimTextOutput("model_summary"))
                 )
               )
             ))
  )
)

# Define Server
server <- function(input, output) {
  # Reactive: Upload and read data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })
  
  # Reactive: Data summary
  output$data_summary <- renderPrint({
    req(userData())
    summary(userData())
  })
  
  # Output: Display uploaded data
  output$data_table <- DT::renderDT({
    req(userData())
    userData()
  })
  
  # Reactive: Transform data to compute quantity
  transformedData <- reactive({
    req(userData())
    userData() %>%
      group_by(wtp) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(wtp)) %>%
      mutate(quantity = cumsum(count))
  })
  
  # Output: Scatterplot of demand data
  output$scatter_plot <- renderPlot({
    req(transformedData())
    ggplot(transformedData(), aes(x = wtp, y = quantity)) +
      geom_point() +
      labs(title = "Demand Data", x = "Willingness to Pay (Price)", y = "Quantity")
  })
  
  # Reactive: Fit selected demand model and create functions
  demandModels <- reactive({
    req(transformedData())  # Ensure transformed data exists
    tb_quantity <- transformedData()  # Get transformed data
    
    # Fit models
    lin_model <- lm(quantity ~ wtp, data = tb_quantity)
    exp_model <- lm(log(quantity) ~ wtp, data = tb_quantity)
    sig_model <- nls(quantity ~ SSlogis(wtp, Asym, xmid, scal), data = tb_quantity)
    
    # Create demand functions
    fQ_lin <- function(P) coef(lin_model)[1] + coef(lin_model)[2] * P
    fQ_exp <- function(P) exp(coef(exp_model)[1] + coef(exp_model)[2] * P)
    fQ_sig <- function(P) {
      coef(sig_model)[1] / (1 + exp((coef(sig_model)[2] - P) / coef(sig_model)[3]))
    }
    
    # Select the appropriate model and function
    selected_model <- switch(
      input$model,
      "Linear Demand" = lin_model,
      "Exponential Demand" = exp_model,
      "Sigmoid Demand" = sig_model,
      NULL  # Default case if input$model is invalid
    )
    
    selected_function <- switch(
      input$model,
      "Linear Demand" = fQ_lin,
      "Exponential Demand" = fQ_exp,
      "Sigmoid Demand" = fQ_sig,
      NULL  # Default case if input$model is invalid
    )
    
    # Stop execution if no valid model or function is selected
    if (is.null(selected_model) || is.null(selected_function)) {
      stop("Invalid model selected: Please choose a valid demand model")
    }
    
    # Return the model and function as a list
    list(
      model = selected_model,
      function = selected_function
    )
  })  # End of demandModels reactive

    
  # Output: Demand curves
  output$demand_plot <- renderPlot({
    ggplot() + labs(title = "Test Plot", x = "X-Axis", y = "Y-Axis")
  })

  # Output: Interpretation
  output$interpretation <- renderText({
    req(demandModels())
    model <- demandModels()$model
    if (inherits(model, "lm")) {
      paste("R-squared:", summary(model)$r.squared)
    } else if (inherits(model, "nls")) {
      "NLS model fitted."
    } else {
      "No interpretation available."
    }
  })
  
  # Output: Model summary
  output$model_summary <- renderPrint({
    req(demandModels())
    summary(demandModels()$model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
