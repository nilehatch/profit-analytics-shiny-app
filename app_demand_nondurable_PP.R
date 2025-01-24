library(shiny)
library(tidyverse)
library(ggplot2)

formatDemandEquation <- function(model, model_type, r_squared = NULL) {
  if (model_type == "Linear Demand") {
    intercept <- as.numeric(coef(model)[1])
    slope <- as.numeric(coef(model)[2])
    r2 <- round(summary(model)$r.squared, 2)
    return(bquote(atop(Q == .(round(intercept, 2)) - .(abs(round(slope, 4))) * P, R^2 == .(r2))))
  } else if (model_type == "Exponential Demand") {
    intercept <- as.numeric(coef(model)[1])
    slope <- as.numeric(coef(model)[2])
    r2 <- round(summary(model)$r.squared, 2)
    return(bquote(atop(Q == e^{.(round(intercept, 2)) - .(abs(round(slope, 4))) * P}, R^2 == .(r2))))
  } else if (model_type == "Sigmoid Demand") {
    asym <- as.numeric(coef(model)["Asym"])
    xmid <- as.numeric(coef(model)["xmid"])
    scal <- as.numeric(coef(model)["scal"])
    pseudo_r2 <- if (!is.null(r_squared)) round(r_squared, 2) else NA
    return(bquote(atop(Q == frac(.(round(asym, 4)), 1 + e^{frac(.(round(xmid, 4)) - P,  .(round(scal, 4)))}), R^2 == .(pseudo_r2))))
  }
}

# Define UI
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
                 sliderInput("price", "Price", min = 0, max = 100, value = 50, step = 1)
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
             )),
    
    tabPanel("Debug Transformed Data", verbatimTextOutput("debug_transformed"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive: Upload and read data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep) |> 
    relocate(where(is.numeric), .before = where(is.character))
  })
  
  # Update column selection dynamically
  observeEvent(userData(), {
    updateSelectInput(session, "start_col", choices = names(userData()))
    updateSelectInput(session, "end_col", choices = names(userData()))
  })
  
  # Pivot the data
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
    updateSliderInput(session, "price", min = 0, max = max(tb$price, na.rm = TRUE), value = max(tb$price, na.rm = TRUE) / 5)
    return(tb)
  })
  
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
  
  # Output: Debug the transformed data
  if (Sys.getenv("SHINY_DEBUG") == "true") {
    output$debug_transformed <- renderPrint({
      req(transformedData())
      print(summary(transformedData()))

#    req(demandModels())
#    demandModels()
#    transformedData()$quantity    
#    transformedData()$quantity        
#    typeof(transformedData()$price)
#    typeof(transformedData()$quantity)    
    })
    }

  # Reactive: Fit selected demand model
  demandModels <- reactive({
    req(transformedData())
    tb <- transformedData()
    
    # Debugging: Print the transformed data summary
    print("Debugging demandModels: Transformed Data Summary")
    print(summary(tb))
    
    # Validate that transformedData is not NULL and has sufficient data
    if (is.null(tb) || nrow(tb) == 0 || any(is.na(tb$quantity))) {
      print("Transformed data is NULL, empty, or contains NA in 'quantity'.")
      return(NULL)
    }
    
    # Debugging: Ensure price and quantity are numeric
    print("Debugging demandModels: Data Types")
    print(str(tb))

    print("Debugging: Checking quantity column for NA/NaN/Inf values")
    print(summary(tb$quantity))
    print(any(is.na(tb$quantity)))
    print(any(is.nan(tb$quantity)))
    print(any(is.infinite(tb$quantity)))
        
    tryCatch({
      # Fit models
      lin_model <- lm(quantity ~ price, data = tb)
      exp_model <- lm(log(quantity) ~ price, data = tb)
      sig_model <- nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb)
      
      # Debugging: Check if models were created
      print("Linear Model Coefficients:")
      print(coef(lin_model))
      print("Exponential Model Coefficients:")
      print(coef(exp_model))
      print("Sigmoid Model Coefficients:")
      print(coef(sig_model))
      
      # Calculate pseudo-R2 for sigmoid
      y_observed <- tb$quantity
      y_predicted <- predict(sig_model)
      ss_residual <- sum((y_observed - y_predicted)^2)
      ss_total <- sum((y_observed - mean(y_observed))^2)
      pseudo_r2 <- 1 - (ss_residual / ss_total)
      
      # Create demand functions
      fQ_lin <- function(P) coef(lin_model)[1] + coef(lin_model)[2] * P
      fQ_exp <- function(P) exp(coef(exp_model)[1] + coef(exp_model)[2] * P)
      fQ_sig <- function(P) {
        coef(sig_model)[1] / (1 + exp((coef(sig_model)[2] - P) / coef(sig_model)[3]))
      }
      
      # Return the selected model and function
      return(list(
        model = switch(input$model,
                       "Linear Demand" = lin_model,
                       "Exponential Demand" = exp_model,
                       "Sigmoid Demand" = sig_model),
        func = switch(input$model,
                      "Linear Demand" = fQ_lin,
                      "Exponential Demand" = fQ_exp,
                      "Sigmoid Demand" = fQ_sig),
        pseudo_r2 = if (input$model == "Sigmoid Demand") pseudo_r2 else NULL
      ))
    }, error = function(e) {
      print("Error in demandModels:")
      print(e)
      return(NULL)
    })
  })

  # Output: debug the demand models
#  output$debug_demandModels <- renderPrint({
#    req(demandModels())
#    demandModels()
#  })
  
  # Output: Demand plot
  output$demand_plot <- renderPlot({
    req(transformedData(), demandModels(), input$price)
    
    tb <- transformedData()
    if (is.null(tb) || nrow(tb) == 0) {
      return(NULL)  # Stop rendering if transformed data is invalid
    }
    
    demand_function <- demandModels()$func
    
    # Evaluate demand at the chosen price
    quantity_at_price <- demand_function(input$price)

    # Generate the demand equation and R² as an expression
#    demand_equation <- formatDemandEquation(demandModels()$model, input$model)
    demand_equation <- formatDemandEquation(demandModels()$model, input$model, demandModels()$pseudo_r2)
    
    
    ggplot(tb, aes(x = price, y = quantity)) +
      geom_function(fun = demand_function, color = "royalblue", linewidth = 2) +
      geom_point() +
      annotate("segment", x = input$price, xend = input$price, y = 0, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("segment", x = 0, xend = input$price, y = quantity_at_price, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("point", x = input$price, y = quantity_at_price, color = "royalblue", fill = "white", shape = 21, size = 4) +
      labs(title = paste(input$model, "Plot"), x = "Price", y = "Quantity") +
      scale_x_continuous(labels = scales::dollar_format()) + 
      scale_y_continuous(limits = c(0, max(tb$quantity, na.rm = TRUE)), labels = scales::comma) +
      annotate("text", x = max(tb$price) * 0.95, y = max(tb$quantity) * 0.95,
               label = paste0("Price: $", input$price, "\nQuantity: ", round(quantity_at_price, 2)),
               hjust = 1, vjust = 1, color = "royalblue", fontface = 2, size = 5) +
      annotate("text",
        x = max(tb$price) * 0.95,  # Place in the lower-right corner
        y = max(tb$quantity) * 0.5, # Place in the lower-right corner
        label = as.expression(demand_equation),
        hjust = 1, vjust = 1, color = "black", fontface = 2, size = 7, parse = TRUE
      )
  })

    
  # Output: Interpretation
  output$interpretation <- renderText({
    req(demandModels())
    model <- demandModels()$model
    r_squared <- demandModels()$pseudo_r2
    
    case_when(
      input$model == "Linear Demand" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        paste0(
          "Linear Demand Interpretation:\n",
          sprintf("R²: %.2f (%.2f%% of the variation in quantity is explained by price).\n", summary(model)$r.squared, summary(model)$r.squared * 100),
          sprintf("Intercept: If the price is $0, we expect to sell %.2f units.\n", intercept),
          sprintf("Slope: For every $1 increase in price, we lose %.2f units of quantity sold.\n", slope)
        )
      },
      
      input$model == "Exponential Demand" ~ {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        percent_change <- abs((exp(slope) - 1) * 100)  # Convert to positive percentage
        paste0(
          "Exponential Demand Interpretation:\n",
          sprintf("R²: %.2f (%.2f%% of the variation in log(quantity) is explained by price).\n", summary(model)$r.squared, summary(model)$r.squared * 100),
          sprintf("Intercept: Base quantity is %.2f units when price is $0.\n", exp(intercept)),
          sprintf("Slope: For every $1 increase in price, sales drop by %.2f%%.\n", percent_change)
        )
      },
      
      input$model == "Sigmoid Demand" ~ {
        asym <- coef(model)["Asym"]
        xmid <- coef(model)["xmid"]
        scal <- coef(model)["scal"]
        paste0(
          "Sigmoid Demand Interpretation:\n",
          "Pseudo-R²: ", sprintf("%.2f", r_squared), " (", sprintf("%.2f%%", r_squared * 100), " of the variation in quantity is explained by price).\n",
          "Asymptote: Maximum quantity is ", sprintf("%.2f", asym), " units.\n",
          "Inflection Point: At a price of $", sprintf("%.2f", xmid), ", demand is most sensitive to price changes.\n",
          "Growth Rate: Demand decreases sharply over a price range of approximately ", sprintf("%.2f", abs(scal)), " units.\n"
        )
      }
    )
  })
  
  # Output: Model summary
  output$model_summary <- renderPrint({
    req(demandModels())
    summary(demandModels()$model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

