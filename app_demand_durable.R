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
    pseudo_r2 <- if (!is.null(r_squared)) r_squared else NA
    return(bquote(atop(Q == frac(.(round(asym, 4)), 1 + e^{frac(.(round(xmid, 4)) - P,  .(round(scal, 4)))}), R^2 == .(pseudo_r2))))
  }
}


# Define the UI -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Expected Customer Demand"),
  
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
                 DT::DTOutput("data_table"),
                 verbatimTextOutput("data_summary")
               )
             )),
    
    # Demand Visualization and Analysis Tab
    tabPanel("Customer Demand",
             sidebarLayout(
               sidebarPanel(
                 selectInput("wtpCol", "Select WTP Column", choices = NULL),
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
             ))
  )
)


# Define the server -------------------------------------------------------

server <- function(input, output, session) {
  # Reactive: Upload and read data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })
  
  # Update WTP column choices dynamically
  observeEvent(userData(), {
    updateSelectInput(session, "wtpCol", choices = names(userData()))
  })

  observeEvent(userData(), {
    if (any(is.na(userData()))) {
      showNotification("Warning: Dataset contains missing values. Rows with NA will be excluded.", type = "warning")
    }
  })
  
  
# transform the data ------------------------------------------------------

  # Reactive: Transform data to compute quantity
  transformedData <- reactive({
    req(userData(), input$wtpCol)
    tb <- userData()
    tb_quantity <- tb |> 
      rename(wtp = !!sym(input$wtpCol)) |>  # Dynamically rename the selected column to "wtp"
      filter(!is.na(wtp)) |>                # Remove rows with NA in "wtp"      
      group_by(wtp) |> 
      summarise(count = n(), .groups = 'drop') |> 
      arrange(desc(wtp)) |> 
      mutate(quantity = cumsum(count))
    
    # Update price slider range dynamically
    updateSliderInput(session, "price", 
                      min = 0, 
                      max = round(max(tb_quantity$wtp, na.rm = TRUE), 1), 
                      value = round(max(tb_quantity$wtp, na.rm = TRUE) / 5, 1) ,
                      step = round(max(tb_quantity$wtp, na.rm = TRUE), 1) /25)
    return(tb_quantity)
  })


# Output: display data and summary statistics -----------------------------

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


# Reactive: fit selected demand model -------------------------------------

  demandModels <- reactive({
    req(transformedData())
    tb_quantity <- transformedData()
    
    # Fit models
    lin_model <- lm(quantity ~ wtp, data = tb_quantity)
    exp_model <- lm(log(quantity) ~ wtp, data = tb_quantity)
    sig_model <- nls(quantity ~ SSlogis(wtp, Asym, xmid, scal), data = tb_quantity)

    # Calculate pseudo-R2 for sigmoid
    y_observed <- tb_quantity$quantity
    y_predicted <- predict(sig_model)
    ss_residual <- sum((y_observed - y_predicted)^2, na.rm=TRUE)
    ss_total <- sum((y_observed - mean(y_observed))^2, na.rm = TRUE)
    pseudo_r2 <- round(1 - (ss_residual / ss_total), 4)

    residuals <- y_observed - y_predicted
    ggplot(data.frame(y_observed, residuals), aes(x = y_observed, y = residuals)) +
      geom_point() +
      labs(title = "Residuals vs Observed", x = "Observed", y = "Residuals") +
      theme_minimal()
    
    # Create demand functions
    fQ_lin <- function(P) coef(lin_model)[1] + coef(lin_model)[2] * P
    fQ_exp <- function(P) exp(coef(exp_model)[1] + coef(exp_model)[2] * P)
    fQ_sig <- function(P) {
      coef(sig_model)[1] / (1 + exp((coef(sig_model)[2] - P) / coef(sig_model)[3]))
    }
    
    # Return the selected model and function
    list(
      model = switch(input$model,
                     "Linear Demand" = lin_model,
                     "Exponential Demand" = exp_model,
                     "Sigmoid Demand" = sig_model),
      func = switch(input$model,
                    "Linear Demand" = fQ_lin,
                    "Exponential Demand" = fQ_exp,
                    "Sigmoid Demand" = fQ_sig),
      pseudo_r2 = if (input$model == "Sigmoid Demand") pseudo_r2 else NULL
    )
  })


# Output: demand plot -----------------------------------------------------

  output$demand_plot <- renderPlot({
    req(transformedData(), demandModels(), input$price)
    tb_quantity <- transformedData()
    demand_function <- demandModels()$func
    
    # Evaluate demand at the chosen price
    quantity_at_price <- demand_function(input$price)

    # Generate the demand equation and R² as an expression
#    demand_equation <- formatDemandEquation(demandModels()$model, input$model)
    demand_equation <- formatDemandEquation(demandModels()$model, input$model, demandModels()$pseudo_r2)
    
    
    ggplot(tb_quantity, aes(x = wtp, y = quantity)) +
      geom_function(fun = demand_function, color = "royalblue", linewidth = 2) +
      geom_point() +
      annotate("segment", x = input$price, xend = input$price, y = 0, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("segment", x = 0, xend = input$price, y = quantity_at_price, yend = quantity_at_price, linetype = "dashed", color = "royalblue") +
      annotate("point", x = input$price, y = quantity_at_price, color = "royalblue", fill = "white", shape = 21, size = 4) +
      labs(title = paste(input$model, "Plot"), x = "Price (WTP)", y = "Quantity") +
      scale_x_continuous(limits = c(0, 1.25*max(tb_quantity$wtp, na.rm = T)), labels = scales::dollar_format()) + 
      scale_y_continuous(limits = c(0, max(tb_quantity$quantity, na.rm = TRUE)), labels = scales::comma) +
      annotate("text", 
               x = max(tb_quantity$wtp, na.rm = T) * 1.2, 
               y = max(tb_quantity$quantity, na.rm = T) * 0.5,
               label = paste0("Price: $", input$price, "\nQuantity: ", round(quantity_at_price, 2)),
               hjust = 1, vjust = 1, color = "royalblue", fontface = 2, size = 5) +
      annotate("text",
        x = max(tb_quantity$wtp, na.rm = T) * 1.2,  # Place in the lower-right corner
        y = max(tb_quantity$quantity, na.rm = T) * 0.95, # Place in the lower-right corner
        label = as.expression(demand_equation),
        hjust = 1, vjust = 1, color = "black", fontface = 2, size = 7, parse = TRUE
      ) +
      theme_minimal()
  })


# Output: interpretation --------------------------------------------------

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


# Output: model summary ---------------------------------------------------

  output$model_summary <- renderPrint({
    req(demandModels())
    summary(demandModels()$model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

