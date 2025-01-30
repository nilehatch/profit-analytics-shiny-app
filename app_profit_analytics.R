library(shiny)
library(tidyverse)
library(DT)  # For interactive table display

# Define UI
ui <- fluidPage(
  
  titlePanel("Integrated Demand Estimation"),
  
  tabsetPanel(id = "tabs",  # Add ID to track active tab
    
    # Data Upload & Validation Tab
    tabPanel("Upload Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Upload CSV File", accept = c(".csv")),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator", choices = c(Comma = ",", Tab = "\t"), selected = ","),
                 checkboxInput("show_transformed", "Show Transformed Data?", FALSE)
               ),
               mainPanel(
                 h4("Data Preview (First 10 Rows)"),
                 DTOutput("data_preview"),
                 h4("Column Names"),
                 verbatimTextOutput("column_names"),
                 h4("Summary Statistics"),
                 verbatimTextOutput("summary_stats"),
                 
                 # FIX: Ensure transformed data is always inside the Upload Data tab
                 h4("Transformed Data"),
                 DTOutput("transformed_data")  
                 )
               )
             ),
    
    # Demand Estimation Tabs
    tabPanel("Durable Goods",
             sidebarLayout(
               sidebarPanel(
                 selectInput("wtpCol_durable", "Select WTP Column", choices = NULL)
               ),
               mainPanel(
                 h4("Durable Goods Demand Data"),
                 DTOutput("durable_transformed")
               )
             )),
    
    tabPanel("Non-Durable (Prices)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("start_price", "First Price Column", choices = NULL),
                 selectInput("end_price", "Last Price Column", choices = NULL)
               ),
               mainPanel(
                 h4("Non-Durable Goods Demand (Prices)"),
                 DTOutput("price_transformed")
               )
             )),
    
    tabPanel("Non-Durable (WTP)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("wtpCol_nondurable", "Select WTP Column", choices = NULL),
                 selectInput("quantityCol", "Select Quantity Column", choices = NULL),
                 selectInput("quantityHalfCol", "Select Quantity at Fraction WTP Column", choices = NULL),
                 sliderInput("fraction", "Fraction of WTP", min = 0.1, max = 1, value = 0.5, step = 0.1)
               ),
               mainPanel(
                 h4("Non-Durable Goods Demand (WTP)"),
                 DTOutput("wtp_transformed")
               )
             )),
    
    # Add Demand Estimation UI
    tabPanel("Demand Estimation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_type", "Select Demand Model", 
                             choices = c("Linear", "Exponential", "Sigmoid")),
                 h4("Model Summary"),
                 verbatimTextOutput("model_summary"),
                 h4("Model Interpretation"),
                 verbatimTextOutput("model_interpretation")
               ),
               mainPanel(
                 h4("Demand Curve"),
                 plotOutput("demand_plot")
                 )
               )
             )
  )
)


# Define the server -------------------------------------------------------

server <- function(input, output, session) {
  
  # Load and process data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })

    
  # Dynamically update dropdown choices after data upload
  observeEvent(userData(), {
    req(userData())  # Ensure data is available
    
    # Get column names
    col_names <- names(userData())
    
    # Update dropdowns for Durable Goods
    updateSelectInput(session, "wtpCol_durable", choices = col_names)
    
    # Update dropdowns for Non-Durable (Prices)
    updateSelectInput(session, "start_price", choices = col_names)
    updateSelectInput(session, "end_price", choices = col_names)
    
    # Update dropdowns for Non-Durable (WTP)
    updateSelectInput(session, "wtpCol_nondurable", choices = col_names)
    updateSelectInput(session, "quantityCol", choices = col_names)
    updateSelectInput(session, "quantityHalfCol", choices = col_names)
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    req(userData())
    datatable(head(userData(), 10))
  })
  
  # Show Column Names
  output$column_names <- renderPrint({
    req(userData())
    names(userData())
  })
  
  # Summary Statistics
  output$summary_stats <- renderPrint({
    req(userData())
    summary(userData())
  })
  
  ### Demand Transformations ###
  
  # 1. Durable Goods Transformation
  durableData <- reactive({
    req(userData(), input$wtpCol_durable)
    userData() %>%
      rename(wtp = !!sym(input$wtpCol_durable)) %>%
      filter(!is.na(wtp)) %>%
      group_by(wtp) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(wtp)) %>%
      mutate(quantity = cumsum(count),
             price = wtp)
  })
  
  # 2. Non-Durable Goods (Prices) Transformation
  priceData <- reactive({
    req(userData(), input$start_price, input$end_price)
    
    start_index <- which(names(userData()) == input$start_price)
    end_index <- which(names(userData()) == input$end_price)
    
    userData() %>%
      pivot_longer(cols = start_index:end_index, 
                   names_to = "price", 
                   values_to = "quantity") %>%
      mutate(price = as.numeric(str_extract(price, "\\d+\\.?\\d*")),  # Extracts first numeric value
             quantity = as.numeric(quantity)) %>%
      filter(!is.na(price), !is.na(quantity)) %>%
      relocate(c(price, quantity))
  })
  
  # 3. Non-Durable Goods (WTP) Transformation
  wtpData <- reactive({
    req(userData(), input$wtpCol_nondurable, input$quantityCol, input$quantityHalfCol)
    
    tb <- userData() %>%
      rename(wtp = !!sym(input$wtpCol_nondurable),
             q = !!sym(input$quantityCol),
             q_half = !!sym(input$quantityHalfCol)) %>%
      filter(!is.na(wtp), !is.na(q), !is.na(q_half)) %>%
      mutate(wtp_half = wtp * input$fraction) %>%  # Calculate the second price based on the fraction slider
      pivot_longer(cols = c(wtp, wtp_half), names_to = "price_type", values_to = "price") %>%
      pivot_longer(cols = c(q, q_half), names_to = "quantity_type", values_to = "quantity") %>%
      filter((price_type == "wtp" & quantity_type == "q") |
               (price_type == "wtp_half" & quantity_type == "q_half")) %>%
      group_by(price) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(price)) %>%      
      mutate(quantity = cumsum(quantity))  # Ensure cumulative quantity for correct demand representation
  })
  
  # Store the last valid transformed dataset
  last_transformed <- reactiveVal(NULL)
  
  transformedData <- reactive({
    req(input$file1)  # Ensure data is uploaded before proceeding
    
    active_tab <- input$tabs  # Track which demand tab is selected
    print(paste("Active Tab:", active_tab))  # Debugging
    
    data_selected <- NULL
    
    if (active_tab == "Durable Goods") {
      data_selected <- durableData()
    } else if (active_tab == "Non-Durable (Prices)") {
      data_selected <- priceData()
    } else if (active_tab == "Non-Durable (WTP)") {
      data_selected <- wtpData()
    } else {
      # When in "Upload Data" tab, return the last transformed dataset
      print("Upload Data Tab Active - Returning Last Transformed Data")
      data_selected <- last_transformed()
    }
    
    # Update last_transformed only if we actually have new data
    if (!is.null(data_selected) && nrow(data_selected) > 0) {
      last_transformed(data_selected)
    }
    
    print("Returning transformed data:")
    print(head(data_selected, 10))  # Print first 10 rows to console
    
    return(data_selected)
  })
  
  
  output$transformed_data <- renderDT({
    req(input$show_transformed)  # Only process when checkbox is checked
    data <- transformedData()  # Get transformed data
    
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "No transformed data available"), options = list(dom = 't')))
    }
    
    datatable(head(data, 10))  # Show first 10 rows
  })


# Demand model fitting ----------------------------------------------------

  # Reactive: Fit demand models for selected scenario
  demandModel <- reactive({
    req(transformedData(), input$model_type)
    tb <- transformedData()
    
    if (nrow(tb) < 3) {
      return(NULL)  # Ensure enough data points to fit models
    }
    
    # Fit models
    lin_model <- tryCatch(lm(quantity ~ price, data = tb), error = function(e) NULL)
    exp_model <- tryCatch(lm(log(quantity) ~ price, data = tb), error = function(e) NULL)
    sig_model <- tryCatch(nls(quantity ~ SSlogis(price, Asym, xmid, scal), 
                              data = tb, start = list(Asym = max(tb$quantity, na.rm = TRUE), 
                                                      xmid = mean(tb$price, na.rm = TRUE), 
                                                      scal = diff(range(tb$price, na.rm = TRUE)) / 5)),
                          error = function(e) NULL)
    
    # Select the appropriate model
    model <- switch(input$model_type,
                    "Linear" = lin_model,
                    "Exponential" = exp_model,
                    "Sigmoid" = sig_model)
    
    return(model)
  })
  
  # Output: Model Summary
  output$model_summary <- renderPrint({
    req(demandModel())
    if (is.null(demandModel())) return("Model fitting failed. Try a different model.")
    summary(demandModel())
  })
  
  # Output: Model Interpretation
  output$model_interpretation <- renderPrint({
    req(demandModel())
    
    model <- demandModel()
    if (is.null(model)) return("No valid model available.")
    
    # Generate interpretation text
    case_when(
      input$model_type == "Linear" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        paste0("Linear Demand Interpretation:\n",
               sprintf("Intercept: %.2f (Expected quantity at price = 0).\n", intercept),
               sprintf("Slope: %.2f (Change in quantity per $1 increase in price).\n", slope),
               sprintf("R²: %.4f", summary(model)$r.squared))
      },
      input$model_type == "Exponential" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        paste0("Exponential Demand Interpretation:\n",
               sprintf("Base quantity at price=0: %.2f.\n", exp(intercept)),
               sprintf("Sales drop by %.2f%% per $1 increase in price.\n", (exp(slope) - 1) * 100),
               sprintf("R²: %.4f", summary(model)$r.squared))
      },
      input$model_type == "Sigmoid" ~ {
        asym <- coef(model)["Asym"]
        xmid <- coef(model)["xmid"]
        scal <- coef(model)["scal"]
        paste0("Sigmoid Demand Interpretation:\n",
               sprintf("Max quantity: %.2f units.\n", asym),
               sprintf("Inflection Point (most sensitive price): $%.2f.\n", xmid),
               sprintf("Growth Rate: %.2f units per price change.\n", scal))
      }
    )
  })
  
  # Output: Demand Curve Plot
  output$demand_plot <- renderPlot({
    req(transformedData(), demandModel())
    
    tb <- transformedData()
    model <- demandModel()
    
    # Define demand function
    demand_func <- switch(input$model_type,
                          "Linear" = function(P) coef(model)[1] + coef(model)[2] * P,
                          "Exponential" = function(P) exp(coef(model)[1] + coef(model)[2] * P),
                          "Sigmoid" = function(P) coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))
    
    ggplot(tb, aes(x = price, y = quantity)) +
      geom_point() +
      geom_function(fun = demand_func, color = "blue") +
      labs(title = paste(input$model_type, "Demand Curve"),
           x = "Price", y = "Quantity") +
      theme_minimal()
  })
    
}

# Run App
shinyApp(ui = ui, server = server)