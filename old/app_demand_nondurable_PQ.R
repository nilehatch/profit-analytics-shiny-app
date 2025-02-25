library(shiny)
library(tidyverse)
library(ggplot2)

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

# UI
ui <- fluidPage(
  titlePanel("Customer Demand Estimation of Non-durable Goods (quantity at maximum willingness to pay)"),
  
  tabsetPanel(
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Upload CSV File", accept = c("text/csv", ".csv")),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                 sliderInput("fraction", "Fraction of WTP for second price:", min = 0.1, max = 0.9, value = 0.5, step = 0.05)
               ),
               mainPanel(
                 DT::DTOutput("data_table"),
                 verbatimTextOutput("data_summary")
               )
             )),
    
    tabPanel("Demand Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Choose Demand Model", choices = c("Linear Demand", "Exponential Demand", "Sigmoid Demand")),
                 sliderInput("price", "Price", min = 0, max = 100, value = 50, step = 1)
               ),
               mainPanel(
                 plotOutput("demand_plot"),
                 verbatimTextOutput("interpretation"),
                 verbatimTextOutput("model_summary")
               )
             ))
  )
)

# Server
server <- function(input, output, session) {
  
  # Upload and read data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })
  
  # Transform data
  transformedData <- reactive({
    req(userData())
    fraction <- input$fraction
    tb <- userData()
    
    # Ensure required columns are present
    if (!all(c("wtp", "quantity", "quantity_at_fraction") %in% names(tb))) {
      stop("The dataset must contain 'wtp', 'quantity', and 'quantity_at_fraction' columns.")
    }
    
    # Add computed price at fraction
    tb <- tb %>% mutate(price_at_fraction = wtp * fraction)
    
    # Pivot `wtp` and `quantity` together
    tb_long <- tb %>%
      pivot_longer(
        cols = c(wtp, price_at_fraction),
        names_to = "price_type",
        values_to = "price"
      ) %>%
      pivot_longer(
        cols = c(quantity, quantity_at_fraction),
        names_to = "quantity_type",
        values_to = "quantity"
      ) %>%
      filter(
        # Keep only aligned rows: wtp with quantity and price_at_fraction with quantity_at_fraction
        (price_type == "wtp" & quantity_type == "quantity") |
          (price_type == "price_at_fraction" & quantity_type == "quantity_at_fraction")
      ) %>%
      select(price, quantity) %>%
      group_by(price) %>%
      summarise(sum_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(price)) %>%
      mutate(quantity = cumsum(sum_quantity))  # Apply cumulative sum
    
    # Update the slider dynamically
    updateSliderInput(session, "price",
                      min = 0,
                      max = round(max(tb_long$price, na.rm = TRUE), 1),
                      value = round(max(tb_long$price, na.rm = TRUE),1) / 5,
                      step = round(max(tb_long$price, na.rm = TRUE),1) / 25)
    
    return(tb_long)
  })

  # Display uploaded data
  output$data_table <- DT::renderDT({
    req(userData())
    userData()
  })
  
  # Display data summary
  output$data_summary <- renderPrint({
    req(userData())
    summary(userData())
  })
  
  # Fit models
  demandModels <- reactive({
    req(transformedData())
    tb <- transformedData()
    
    # Linear and exponential models
    lin_model <- lm(quantity ~ price, data = tb)
    exp_model <- lm(log(quantity) ~ price, data = tb)
    
    # Sigmoid model
    sig_model <- tryCatch(
      nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb),
      error = function(e) NULL
    )
    
    # Pseudo R² for sigmoid
    pseudo_r2 <- if (!is.null(sig_model)) {
      y_obs <- tb$quantity
      y_pred <- predict(sig_model)
      1 - sum((y_obs - y_pred)^2) / sum((y_obs - mean(y_obs))^2)
    } else NULL
    
    list(
      lin_model = lin_model,
      exp_model = exp_model,
      sig_model = sig_model,
      pseudo_r2 = pseudo_r2
    )
  })
  
  # Plot demand
  output$demand_plot <- renderPlot({
    req(transformedData(), demandModels(), input$price)
    tb <- transformedData()
    models <- demandModels()
    model_type <- input$model
    
    # Select model and create demand function
    model <- switch(model_type,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)
    demand_func <- switch(model_type,
                          "Linear Demand" = function(P) coef(model)[1] + coef(model)[2] * P,
                          "Exponential Demand" = function(P) exp(coef(model)[1] + coef(model)[2] * P),
                          "Sigmoid Demand" = if (!is.null(model)) {
                            function(P) coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3]))
                          } else NULL)

    
    quantity_at_price <- demand_func(input$price)
    demand_equation <- formatDemandEquation(model, model_type, models$pseudo_r2)
    ymax <- max(max(tb$quantity, na.rm = TRUE), demand_func(0))    
    
    # Plot
    ggplot(tb, aes(x = price, y = quantity)) +
#      geom_point() +
#      geom_function(fun = demand_func, color = "blue") +
      geom_function(fun = demand_func, color = "royalblue", linewidth = 2) +
      geom_point() +
      
      annotate("segment", x = input$price, xend = input$price, y = 0, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("segment", x = 0, xend = input$price, y = quantity_at_price, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("point", x = input$price, y = quantity_at_price, color = "royalblue", fill = "white", shape = 21, size = 4) +

      labs(title = paste(model_type, "Demand"), x = "Price", y = "Quantity") +
      
      scale_x_continuous(limits = c(0, 1.5*max(tb$price, na.rm = T)), 
                         labels = scales::dollar_format()) + 
      scale_y_continuous(limits = c(0, ymax),
                         labels = scales::comma) +   
      
      annotate("text", 
               x = max(tb$price) * 1.4, # Place in the midlower-right corner
               y = max(tb$quantity) * 0.5,
               label = paste0("Price: $", input$price, "\nQuantity: ", round(quantity_at_price, 2)),
               hjust = 1, vjust = 1, color = "royalblue", fontface = "bold", size = 5) +
      
      annotate("text",
               x = max(tb$price) * 1.4,  # Place in the upper-right corner
               y = max(tb$quantity) * 0.80, 
               label = as.expression(demand_equation),
               hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7, parse = TRUE) +
      
      theme_minimal()
  })


# Output: interpretation --------------------------------------------------

  output$interpretation <- renderText({
    req(demandModels(), input$model)
    
    # Retrieve the models and selected model
    models <- demandModels()
    model_type <- input$model
    
    model <- switch(model_type,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)
    
    # Check if the model exists
    if (is.null(model)) {
      return("The selected demand model could not be fitted. Please try a different model.")
    }
    
    # Interpret based on the model type
    interpretation <- 
      switch(model_type,
             "Linear Demand" = {
               intercept <- coef(model)[1]
               slope <- coef(model)[2]
               r_squared <- summary(model)$r.squared
               paste0(
                 "Linear Demand Interpretation:\n",
                 sprintf("R²: %.4f (%.2f%% of the variation in quantity is explained by price).\n", r_squared, r_squared * 100),
                 sprintf("Intercept: If the price is $0, we expect to sell %.2f units.\n", intercept),
                 sprintf("Slope: For every $1 increase in price, we lose %.2f units of quantity sold.\n", slope)
                 )
               },
             "Exponential Demand" = {
               intercept <- coef(model)[1]
               slope <- coef(model)[2]
               r_squared <- summary(model)$r.squared
               percent_change <- abs((exp(slope) - 1) * 100)
               paste0(
                 "Exponential Demand Interpretation:\n",
                 sprintf("R²: %.4f (%.2f%% of the variation in log(quantity) is explained by price).\n", r_squared, r_squared * 100),
                 sprintf("Intercept: Base quantity is %.2f units when price is $0.\n", exp(intercept)),
                 sprintf("Slope: For every $1 increase in price, sales drop by %.2f%%.\n", percent_change)
                 )
               },
             "Sigmoid Demand" = {
               r_squared <- models$pseudo_r2
               asym <- coef(model)["Asym"]
               xmid <- coef(model)["xmid"]
               scal <- coef(model)["scal"]
               paste0(
                 "Sigmoid Demand Interpretation:\n",
                 sprintf("Pseudo-R²: %.4f (%.2f%% of the variation in quantity is explained by price).\n", r_squared, r_squared * 100),
                 sprintf("Asymptote: Maximum quantity is %.2f units.\n", asym),
                 sprintf("Inflection Point: At a price of $%.2f, demand is most sensitive to price changes.\n", xmid),
                 sprintf("Growth Rate: Demand decreases sharply over a price range of approximately %.2f units.\n", abs(scal))
                 )
               },
             "Unknown model type. Please check your selection."
             )
    
    return(interpretation)
  })
  
  
  # Model summary
  output$model_summary <- renderPrint({
    models <- demandModels()
    model <- switch(input$model,
                    "Linear Demand" = models$lin_model,
                    "Exponential Demand" = models$exp_model,
                    "Sigmoid Demand" = models$sig_model)
    summary(model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

