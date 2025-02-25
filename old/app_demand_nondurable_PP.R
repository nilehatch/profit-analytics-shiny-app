library(shiny)
library(tidyverse)
library(ggplot2)


# Helper: format the demand equation for ggplot annotation ----------------

formatDemandEquation <- function(model, model_type, r_squared = NULL) {
  if (model_type == "Linear Demand") {
    intercept <- as.numeric(coef(model)[1])
    slope <- as.numeric(coef(model)[2])
    r2 <- round(summary(model)$r.squared, 4)
    return(bquote(atop(Q == .(round(intercept, 2)) - .(abs(round(slope, 4))) * P, R^2 == .(r2))))
  } else if (model_type == "Exponential Demand") {
    intercept <- as.numeric(coef(model)[1])
    slope <- as.numeric(coef(model)[2])
    r2 <- round(summary(model)$r.squared, 4)
    return(bquote(atop(Q == e^{.(round(intercept, 2)) - .(abs(round(slope, 4))) * P}, R^2 == .(r2))))
  } else if (model_type == "Sigmoid Demand") {
    asym <- as.numeric(coef(model)["Asym"])
    xmid <- as.numeric(coef(model)["xmid"])
    scal <- as.numeric(coef(model)["scal"])
    pseudo_r2 <- if (!is.null(r_squared)) round(r_squared, 4) else NA
    return(bquote(atop(Q == frac(.(round(asym, 4)), 1 + e^{frac(.(round(xmid, 4)) - P,  .(round(scal, 4)))}), R^2 == .(pseudo_r2))))
  }
}


# Define the UI -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Expected Customer Demand for Non-durable Products"),
  
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
                              selected = ","),
                 selectInput("start_col", "First Price Column", choices = NULL),
                 selectInput("end_col", "Last Price Column", choices = NULL)                 
               ),
               mainPanel(
                 DT::DTOutput("data_table"),
                 verbatimTextOutput("data_summary")
               )
             )),
    
    # Demand Visualization and Analysis Tab
    tabPanel("Customer Demand",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Choose Demand Model",
                             choices = c("Linear Demand", "Exponential Demand", "Sigmoid Demand")),
#                 selectInput("demand_level", "Choose Demand Level", choices = c("Individual Demand", "Sample Demand"), selected = "Sample Demand"),
                 sliderInput("price", "Price", min = 0, max = 100, value = 50, step = 1)
               ),

               mainPanel(
                 
                 tabsetPanel(
                   id = "demand_tab",  # Add an ID to track the active tab
                   tabPanel("Sample Demand",
                            plotOutput("sample_demand_plot"),
                            verbatimTextOutput("sample_interpretation"),
                            verbatimTextOutput("sample_model_summary")
                            ),
                   tabPanel("Individual Demand",
                            plotOutput("individual_demand_plot"),
                            verbatimTextOutput("individual_interpretation"),
                            verbatimTextOutput("individual_model_summary")
                            )
                   ),
                 
                 tabsetPanel(
                   tabPanel("Interpretation",
                            verbatimTextOutput("interpretation")
                            ),
                   tabPanel("Model Summary",
                            verbatimTextOutput("model_summary")
                            )
                   )
                 )
               ))
    )
  )


# Define the server -------------------------------------------------------

server <- function(input, output, session) {


# Reactive: Upload the data -----------------------------------------------
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep) |> 
    relocate(where(is.numeric), .before = where(is.character))
  })


# Choose the price/quantity columns for pivot_longer ----------------------
  # Update column selection dynamically
  observeEvent(userData(), {
    updateSelectInput(session, "start_col", choices = names(userData()))
    updateSelectInput(session, "end_col", choices = names(userData()))
  })

  observeEvent(userData(), {
    if (any(is.na(userData()))) {
      showNotification("Warning: Dataset contains missing values. Rows with NA will be excluded.", type = "warning")
    }
  })
  
# Pivot the data ----------------------------------------------------------
  transformedData <- reactive({
    req(userData(), input$start_col, input$end_col)
    
    tb_wide <- userData()
    
    # Validate selected columns
    if (input$start_col == input$end_col || is.null(input$start_col) || is.null(input$end_col)) {
      return(NULL)
    }
    
    start_index <- which(names(tb_wide) == input$start_col)
    end_index <- which(names(tb_wide) == input$end_col)
    
    # Pivot longer
    tb <- tb_wide |> 
      pivot_longer(cols = start_index:end_index,
                   names_prefix = "P",
                   names_to = "price",
                   values_to = "quantity") |> 
      mutate(
        price = as.numeric(price),   # Ensure price is numeric
        quantity = as.numeric(quantity)  # Ensure quantity is numeric
      ) |> 
      relocate(c(price, quantity)) |> 
      filter(!is.na(price) & !is.na(quantity))  # Remove rows with NA values
    
    # Update price slider range
    updateSliderInput(session, "price", 
                      min = 0, 
                      max = max(tb$price, na.rm = TRUE), 
                      value = max(tb$price, na.rm = TRUE) / 5,
                      step = max(tb$price, na.rm = TRUE) /25)
    return(tb)
  })


# Reactive: Sample demand dataset -----------------------------------------
  sampleData <- reactive({
    req(transformedData())
    transformedData() |> 
      group_by(price) |> 
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")
  })


# Reactive: Individual demand dataset -------------------------------------

  individualData <- reactive({
    req(transformedData())
    transformedData() |> 
      mutate(quantity = quantity + 0.001)  # Add small constant to avoid log(0) issues
  })


# Output:  Display uploaded data and summary statistics -------------------
    
  # Output: Display uploaded data
  output$data_table <- DT::renderDT({
    req(userData())
    userData()
  })
  
  # Output: Data summary
  output$data_summary <- renderPrint({
    req(userData())
    summary(userData())
  })
  
# Reactive:  fit selected demand models -----------------------------------

  # Reactive: Sample demand models
  sampleDemandModels <- reactive({
    req(sampleData())
    tb <- sampleData()
    fitDemandModels(tb)  # Use a helper function to fit models
  })
  
  # Reactive: Individual demand models
  individualDemandModels <- reactive({
    req(individualData())
    tb <- individualData()
    fitDemandModels(tb)  # Use the same helper function to fit models
  })
  
  # Helper function to fit models
  fitDemandModels <- function(tb) {
    lin_model <- lm(quantity ~ price, data = tb)
    exp_model <- lm(log(quantity) ~ price, data = tb)
    sig_model <- nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb)
    
    y_observed <- tb$quantity
    y_predicted <- predict(sig_model)
    pseudo_r2 <- 1 - sum((y_observed - y_predicted)^2) / sum((y_observed - mean(y_observed))^2)
    
    list(
      lin_model = lin_model,
      exp_model = exp_model,
      sig_model = sig_model,
      pseudo_r2 = pseudo_r2
    )
  }

  output$debug_demandModels <- renderPrint({
    req(demandModels())
    str(demandModels())  # Check the structure of the object
    })  

# Output: Demand plot -----------------------------------------------------

  # Output: Sample demand plot
  output$sample_demand_plot <- renderPlot({
    req(sampleData(), sampleDemandModels(), input$price)
    generateDemandPlot(sampleData(), sampleDemandModels(), input$model, input$price)
  })
  
  # Output: Individual demand plot
  output$individual_demand_plot <- renderPlot({
    req(individualData(), individualDemandModels(), input$price)
    generateDemandPlot(individualData(), individualDemandModels(), input$model, input$price)
  })
  
  # Helper function to generate plots
  generateDemandPlot <- function(data, models, model_type, price) {
    model <- switch(model_type,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)

    demand_function <- switch(model_type,
                              "Linear Demand" = function(P) coef(model)[1] + coef(model)[2] * P,
                              "Exponential Demand" = function(P) exp(coef(model)[1] + coef(model)[2] * P),
                              "Sigmoid Demand" = function(P) coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))

    quantity_at_price <- demand_function(price)
    demand_equation <- formatDemandEquation(model, model_type, models$pseudo_r2)
    ymax <- max(max(data$quantity, na.rm = TRUE), demand_function(0))    
    
    ggplot(data, aes(x = price, y = quantity)) +
#      geom_function(fun = demand_function, color = "blue") +
      geom_function(fun = demand_function, color = "royalblue", linewidth = 2) +
      geom_point() +
      
      annotate("segment", x = input$price, xend = input$price, y = 0, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("segment", x = 0, xend = input$price, y = quantity_at_price, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("point", x = input$price, y = quantity_at_price, color = "royalblue", fill = "white", shape = 21, size = 4) +

      labs(title = paste(input$model, "Plot"), x = "Price", y = "Quantity") +      

      scale_x_continuous(limits = c(0, 1.5*max(data$price, na.rm = T)), labels = scales::dollar_format()) + 
      scale_y_continuous(limits = c(0, ymax), labels = scales::comma) +      

      annotate("text", 
               x = max(data$price) * 1.4, # Place in the midlower-right corner
               y = max(data$quantity) * 0.5,
               label = paste0("Price: $", input$price, "\nQuantity: ", round(quantity_at_price, 2)),
               hjust = 1, vjust = 1, color = "royalblue", fontface = "bold", size = 5) +

      # Add the background rectangle for the text
      #      annotate("rect",               xmin = max(data$price) * 0.03, xmax = max(data$price) * 0.35,               ymin = max(data$quantity) *0.02, ymax = max(data$quantity) * 0.35,               fill = "white", alpha = 0.8) +  # Semi-transparent white background
      
      annotate("text",
               x = max(data$price) * 1.4,  # Place in the upper-right corner
               y = max(data$quantity) * 0.80, 
               label = as.expression(demand_equation),
               hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7, parse = TRUE) +
      
      theme_minimal()
  }
    
# Output: Interpretation --------------------------------------------------

  output$interpretation <- renderText({
    req(input$demand_tab)
    
    # Select models based on active tab
    models <- if (input$demand_tab == "Sample Demand") {
      sampleDemandModels()
    } else if (input$demand_tab == "Individual Demand") {
      individualDemandModels()
    } else {
      return("No model available for interpretation.")
    }
    
    # Select the specific model based on user input
    model <- switch(input$model,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)
    
    if (is.null(model)) {
      return("No model available for interpretation.")
    }
    
    # Generate interpretation text
    case_when(
      input$model == "Linear Demand" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        paste0(
          "Linear Demand Interpretation:\n",
          sprintf("R²: %.4f (%.2f%% of the variation in quantity is explained by price).\n",
                  summary(model)$r.squared,
                  summary(model)$r.squared * 100),
#          sprintf("R²: %.2f (%.2f%% of the variation in quantity is explained by price).\n", round(summary(model)$r.squared, 4), summary(model)$r.squared * 100),
          sprintf("Intercept: If the price is $0, we expect to sell %.2f units.\n", intercept),
          sprintf("Slope: For every $1 increase in price, we lose %.2f units of quantity sold.\n", slope)
        )
      },
      input$model == "Exponential Demand" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        percent_change <- abs((exp(slope) - 1) * 100)
        paste0(
          "Exponential Demand Interpretation:\n",
          sprintf("R²: %.4f (%.2f%% of the variation in log(quantity) is explained by price).\n", 
                  summary(model)$r.squared, 
                  summary(model)$r.squared * 100),
          sprintf("Intercept: Base quantity is %.2f units when price is $0.\n", exp(intercept)),
          sprintf("Slope: For every $1 increase in price, sales drop by %.2f%%.\n", percent_change)
        )
      },
      input$model == "Sigmoid Demand" ~ {
        asym <- coef(model)["Asym"]
        xmid <- coef(model)["xmid"]
        scal <- coef(model)["scal"]
        pseudo_r2 <- models$pseudo_r2
        paste0(
          "Sigmoid Demand Interpretation:\n",
          "Pseudo-R²: ", sprintf("%.4f", pseudo_r2), " (", sprintf("%.2f%%", pseudo_r2 * 100), " of the variation in quantity is explained by price).\n",
          "Asymptote: Maximum quantity is ", sprintf("%.2f", asym), " units.\n",
          "Inflection Point: At a price of $", sprintf("%.2f", xmid), ", demand is most sensitive to price changes.\n",
          "Growth Rate: Demand decreases sharply over a price range of approximately ", sprintf("%.2f", abs(scal)), " units.\n"
        )
      }
    )
  })
# Output: Model summary ---------------------------------------------------

  output$model_summary <- renderPrint({
    req(input$demand_tab)
    
    # Select models based on active tab
    models <- if (input$demand_tab == "Sample Demand") {
      sampleDemandModels()
    } else if (input$demand_tab == "Individual Demand") {
      individualDemandModels()
    } else {
      return("No model available for summary.")
    }
    
    # Select the specific model based on user input
    model <- switch(input$model,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)
    
    if (!is.null(model)) {
      summary(model)
    } else {
      cat("No model available for summary.")
    }
  })

}
  
# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)

