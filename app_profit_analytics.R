library(shiny)
library(tidyverse)
library(DT)


# -------------------------------------------------------------------------
# Helper Functions --------------------------------------------------------
# -------------------------------------------------------------------------


# Format the demand model for ggplot annotation ---------------------------

formatDemandEquation <- function(model, model_type, r_squared = NULL) {
  
  if (is.null(model)) {
    cat("[ERROR] Model is NULL\n")
    return("")
  }
  
  equation <- switch(model_type,
                     "Linear" = {
                       intercept <- as.numeric(coef(model)[1])
                       slope <- as.numeric(coef(model)[2])
                       r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q == .(round(intercept, 2)) - .(abs(round(slope, 4))) * P, 
                                                 R^2 == .(r2))))
                     },
                     "Exponential" = {
                       intercept <- as.numeric(coef(model)[1])
                       slope <- as.numeric(coef(model)[2])
                       r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q == e^{.(round(intercept, 2)) - .(abs(round(slope, 4))) * P}, 
                                                 R^2 == .(r2))))
                     },
                     "Sigmoid" = {
                       asym <- as.numeric(coef(model)["Asym"])
                       xmid <- as.numeric(coef(model)["xmid"])
                       scal <- as.numeric(coef(model)["scal"])
                       pseudo_r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q == frac(.(round(asym, 4)), 1 + e^{frac(.(round(xmid, 4)) - P, .(round(scal, 4)))}), 
                                                 R^2 == .(pseudo_r2))))
                     },
                     {
                       cat("[ERROR] Unknown model type:", model_type, "\n")
                       return("")
                     }
  )
  
  return(equation)
}


# -------------------------------------------------------------------------
# Define the UI -----------------------------------------------------------
# -------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Profit Analytics for Entrepreneurs"),


# 🛠 Tabset 1: upload and transform data ----------------------------------

  tabsetPanel(id = "tabset_data",
              
              # 📂 Upload Data
              tabPanel("Upload Data",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Upload CSV File", accept = c(".csv")),
                           checkboxInput("header", "Header", TRUE),
                           radioButtons("sep", "Separator", choices = c(Comma = ",", Tab = "\t"), selected = ",")
                         ),
                         mainPanel(
                           h4("Raw Data Preview (First 10 Rows)"),
                           DTOutput("data_preview"),
                           h4("Column Names"),
                           verbatimTextOutput("column_names"),
                           h4("Summary Statistics"),
                           verbatimTextOutput("summary_stats")
                         )
                       )),
              
              # 📊 Durable Goods
              tabPanel("Durable Goods",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("wtpCol_durable", "Select WTP Column", choices = NULL)
                         ),
                         mainPanel(
                           h4("Durable Goods: Transformed Data"),
                           DTOutput("durable_transformed")
                         )
                       )),
              
              # 📈 Non-Durable Goods (Prices)
              tabPanel("Non-Durable (Prices)",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("start_price", "First Price Column", choices = NULL),
                           selectInput("end_price", "Last Price Column", choices = NULL)
                         ),
                         mainPanel(
                           h4("Non-Durable (Prices): Transformed Data"),
                           DTOutput("price_transformed")
                         )
                       )),
              
              # 📊 Non-Durable Goods (WTP)
              tabPanel("Non-Durable (WTP)",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("wtpCol_nondurable", "Select WTP Column", choices = NULL),
                           selectInput("quantityCol", "Select Quantity Column", choices = NULL),
                           selectInput("quantityHalfCol", "Select Quantity at Fraction WTP Column", choices = NULL),
                           sliderInput("fraction", "Fraction of WTP", min = 0.1, max = 1, value = 0.5, step = 0.1)
                         ),
                         mainPanel(
                           h4("Non-Durable (WTP): Transformed Data"),
                           DTOutput("wtp_transformed")
                         )
                       ))
  ),



# 🛠 Tabset 2: Sample Demand Estimation  ----------------------------------

#  tabsetPanel(id = "tabset_demand",
              
              # 📊 Demand Model Selection
              # 📊 Demand Model Selection (with Sub-Tabs)
#              tabPanel("Demand Estimation",
#                       sidebarLayout(
#                         sidebarPanel(
#                           selectInput("model_type", "Choose Demand Model", 
#                                       choices = c("Linear", "Exponential", "Sigmoid")),
#                           sliderInput("price", "Select Price", min = 0, max = 100, value = 10, step = 1)
#                         ),
#                         mainPanel(
#                           tabsetPanel(
                             # 📌 Default Tab: Demand Plot
#                             tabPanel("Demand Curve", plotOutput("demand_plot")),
                             
                             # 📈 Model Summary Tab
#                             tabPanel("Model Summary", verbatimTextOutput("model_summary")),
                             
                             # 📊 Model Interpretation Tab
#                             tabPanel("Model Interpretation", verbatimTextOutput("model_interpretation"))
#                           )                       )                       )              )))

tabsetPanel(id = "tabset_demand",
            
            # 📈 Sample Demand Estimation
            tabPanel("Sample Demand",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("model_type", "Choose Demand Model", 
                                     choices = c("Linear", "Exponential", "Sigmoid")),
                         sliderInput("price", "Select Price", min = 0, max = 100, value = 10, step = 1)
                       ),
                       mainPanel(
                         h4("Sample Demand Curve"),
                         plotOutput("sample_demand_plot"),
                         h4("Model Summary"),
                         verbatimTextOutput("sample_model_summary"),
                         h4("Model Interpretation"),
                         verbatimTextOutput("sample_model_interpretation")
                       )
                     )),
            
            # 📈 Market Demand Scaling
            tabPanel("Market Demand",
                     sidebarLayout(
                       sidebarPanel(
                         numericInput("market_size", "Target Market Population", 
                                      value = 10000, min = 1, step = 100),
                         radioButtons("demand_view", "View Demand Data:",
                                      choices = c("Sample Demand" = "sample", 
                                                  "Market Demand" = "market"),
                                      selected = "market")
                       ),
                       mainPanel(
                         h4("Market Demand Curve"),
                         plotOutput("market_demand_plot"),
                         h4("Market-Level Model Summary"),
                         verbatimTextOutput("market_model_summary"),
                         h4("Market-Level Model Interpretation"),
                         verbatimTextOutput("market_model_interpretation")
                       )
                     ))
)
)

# -------------------------------------------------------------------------
# Define the Server -------------------------------------------------------
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Load user data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })
  
  # Dynamically update column selection
  observeEvent(userData(), {
    updateSelectInput(session, "wtpCol_durable", choices = names(userData()))
    updateSelectInput(session, "start_price", choices = names(userData()))
    updateSelectInput(session, "end_price", choices = names(userData()))
    updateSelectInput(session, "wtpCol_nondurable", choices = names(userData()))
    updateSelectInput(session, "quantityCol", choices = names(userData()))
    updateSelectInput(session, "quantityHalfCol", choices = names(userData()))
  })

# 🛠 Tabset 1: upload and transform data ---------------------------------    

# 📌 Output: Display Raw Data --------------------------------------------

  output$data_preview <- renderDT({
    req(userData())
    datatable(head(userData(), 10))
  })
  
  output$column_names <- renderPrint({
    req(userData())
    names(userData())
  })
  
  output$summary_stats <- renderPrint({
    req(userData())
    summary(userData())
  })


  observeEvent(userData(), {
#    cat("[DEBUG] Checking is.na() on userData():", class(userData()), "\n")  
    if (any(is.na(userData()))) {
      showNotification("Warning: Dataset contains missing values. Rows with NA will be excluded.", type = "warning")
    }
  })  

# 🚀 Reactive: Demand Data Transformations 🚀 ------------------------------
# 📌 Output: Display Data Transformations 📌 -------------------------------  

  # 1️⃣ Durable Goods
  durableData <- reactive({
    req(userData(), input$wtpCol_durable)
    data <- userData() %>%
      rename(wtp = !!sym(input$wtpCol_durable)) %>%
      filter(!is.na(wtp)) %>%
      group_by(wtp) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(wtp)) %>%
      mutate(quantity = cumsum(count),
             price = wtp)
    
#    cat("[DEBUG] Checking is.na() on durableData():", any(is.na(data)), "\n")  # ✅ Correct check
    return(data)
  })
  
  output$durable_transformed <- renderDT({
    req(durableData())
    datatable(durableData())
  })
  
  # 2️⃣ Non-Durable Goods (Prices)
  priceData <- reactive({
    req(userData(), input$start_price, input$end_price)
    
    start_index <- which(names(userData()) == input$start_price)
    end_index <- which(names(userData()) == input$end_price)
    
    data <- userData() %>%
      pivot_longer(cols = start_index:end_index, 
                   names_to = "price", 
                   values_to = "quantity") %>%
      mutate(price = as.numeric(str_extract(price, "\\d+\\.?\\d*")),  
             quantity = as.numeric(quantity)) %>%
      filter(!is.na(price), !is.na(quantity)) %>%
      group_by(price) |> 
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") |> 
      relocate(c(price, quantity))
    
#    cat("[DEBUG] Checking is.na() on priceData():", any(is.na(data)), "\n")  # ✅ Correct check
    return(data)
  })
  
  output$price_transformed <- renderDT({
    req(priceData())
    datatable(priceData())
  })
  
  # 3️⃣ Non-Durable Goods (WTP)
  wtpData <- reactive({
    req(userData(), input$wtpCol_nondurable, input$quantityCol, input$quantityHalfCol)
    
    data <- userData() %>%
      rename(wtp = !!sym(input$wtpCol_nondurable),
             q = !!sym(input$quantityCol),
             q_half = !!sym(input$quantityHalfCol)) %>%  
      filter(!is.na(wtp), !is.na(q), !is.na(q_half)) %>%
      mutate(wtp_half = wtp * input$fraction) %>%
      pivot_longer(cols = c(wtp, wtp_half), names_to = "price_type", values_to = "price") %>%
      pivot_longer(cols = c(q, q_half), names_to = "quantity_type", values_to = "quantity") %>%
      filter((price_type == "wtp" & quantity_type == "q") |
               (price_type == "wtp_half" & quantity_type == "q_half")) %>%
      group_by(price) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(price)) %>%
      mutate(quantity = cumsum(quantity))  
    
#    cat("[DEBUG] Checking is.na() on wtpData():", any(is.na(data)), "\n")  # ✅ Correct check
    return(data)
  })
  
  output$wtp_transformed <- renderDT({
    req(wtpData())
    datatable(wtpData())
  })
 

# 🚀 Reactive: select transformed data from the correct scenario 🚀 -------

    # 🔄 Reactive: Select transformed data from the correct scenario
  transformedData <- reactive({
      req(input$file1, input$tabset_data)  # Ensure data is uploaded and a tab is selected
      
      active_tab <- input$tabset_data  # Track selected tab
      
      # Select appropriate transformed dataset
      data <- switch(active_tab,
                     "Durable Goods" = durableData(),
                     "Non-Durable (Prices)" = priceData(),
                     "Non-Durable (WTP)" = wtpData(),
                     NULL)
      
      # Debugging check
     if (is.null(data) || nrow(data) == 0) {
        cat("[WARNING] Transformed Data is NULL or Empty\n")  # Debug print
      } else {
        print(head(data))  # Show first few rows in console
      }
      
      return(data)
    })
  
# 🛠 Tabset 2: Demand Model Estimation ------------------------------------    

# 🚀 Reactive: Fit Demand Models 🚀 ---------------------------------------

  demandModel <- reactive({
    req(transformedData(), input$model_type)
    tb <- transformedData()
    
    if (is.null(tb) || nrow(tb) < 3) {
      cat("[WARNING] Not enough data points to fit models\n")
      return(NULL)
    }
    
    lin_model <- tryCatch(lm(quantity ~ price, data = tb), error = function(e) NULL)
    exp_model <- tryCatch(lm(log(quantity) ~ price, data = tb), error = function(e) NULL)
    
    sig_model <- tryCatch(
      nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb),
      error = function(e) {
        cat("[WARNING] Sigmoid model fitting failed\n")
        NULL
      }
    )
    
    # ✅ Update price slider dynamically
    updateSliderInput(session, "price",
                      min = 0,
                      max = round(max(tb$price, na.rm = TRUE), 2),
                      value = round(max(tb$price, na.rm = TRUE), 2) / 5,
                      step = pmax(round(max(tb$price, na.rm = TRUE) / 100, 2), 0.01))  # ✅ Ensure minimum step
    
    pseudo_r2 <- if (!is.null(sig_model)) {
      y_obs <- tb$quantity
      y_pred <- predict(sig_model)
      1 - sum((y_obs - y_pred)^2) / sum((y_obs - mean(y_obs))^2)
    } else NULL  # Ensure it's not undefined
    
    model <- switch(input$model_type,
                    "Linear" = lin_model,
                    "Exponential" = exp_model,
                    "Sigmoid" = sig_model)
    
    if (is.null(model)) {
      cat("[WARNING] Model fitting failed for:", input$model_type, "\n")} 
    
    # ✅ Ensure pseudo_r2 is included in the returned list
    return(list(model = model, pseudo_r2 = pseudo_r2))
    
  })
  

# 🚀 Reactive: Market Models 🚀 -------------------------------------------
  marketDemand <- reactive({
    req(demandModel(), input$market_size)
    model_list <- demandModel()
    model <- model_list$model
    
    if (is.null(model)) return(NULL)  # Handle errors
    
    scaling_factor <- input$market_size / max(transformedData()$quantity, na.rm = TRUE)
    
    # Compute Market Demand Function
    market_demand_func <- switch(
      input$model_type,
      "Linear" = function(P) scaling_factor * (coef(model)[1] + coef(model)[2] * P),
      "Exponential" = function(P) scaling_factor * exp(coef(model)[1] + coef(model)[2] * P),
      "Sigmoid" = function(P) scaling_factor * coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3]))
    )
    
    return(list(model = model, func = market_demand_func, scaling_factor = scaling_factor))
  })
  
  
# 📌 Output: demand model PLOT 📌 -----------------------------------------
  
  # 📌 Output: demand model PLOT
  output$demand_plot <- renderPlot({
    req(transformedData(), demandModel(), input$price)
    tb <- transformedData()
    model_list <- demandModel()
    model <- model_list$model
    pseudo_r2 <- model_list$pseudo_r2
    model_type <- input$model_type
    
    # Compute demand function
    demand_func <- switch(model_type,
                          "Linear" = function(P) coef(model)[1] + coef(model)[2] * P,
                          "Exponential" = function(P) exp(coef(model)[1] + coef(model)[2] * P),
                          "Sigmoid" = function(P) coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))
    
    quantity_at_price <- demand_func(input$price)
    demand_equation <- formatDemandEquation(model, model_type, pseudo_r2)
    ymax <- max(max(tb$quantity, na.rm = TRUE), demand_func(0))        
    
    # ✅ Debugging
    print(demand_equation)
    
    # Plot
    ggplot(tb, aes(x = price, y = quantity)) +
      geom_function(fun = demand_func, color = "royalblue", linewidth = 2) +
      geom_point() +
      
      # Dashed reference lines
      annotate("segment", x = input$price, xend = input$price, y = 0, yend = quantity_at_price, 
               linetype = "dashed", color = "royalblue") +
      annotate("segment", x = 0, xend = input$price, y = quantity_at_price, yend = quantity_at_price, 
               linetype = "dashed", color = "royalblue") +
      annotate("point", x = input$price, y = quantity_at_price, color = "royalblue", shape = 21, fill = "white", size = 4) +
      
      # Demand equation annotation
      annotate("text",
               x = max(tb$price) * 1.4,
               y = max(tb$quantity) * 0.80, 
               label = demand_equation,
               hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7, parse = TRUE) +
      
      # Price / Quantity annoation
      annotate("text", 
               x = max(tb$price) * 1.4, # Place in the midlower-right corner
               y = max(tb$quantity) * 0.5,
               label = paste0("Price: $", input$price, "\nQuantity: ", round(quantity_at_price, 2)),
               hjust = 1, vjust = 1, color = "royalblue", fontface = "bold", size = 5) +
      
      # Labels and Formatting
      labs(title = paste(model_type, "Demand Curve"), x = "Price", y = "Quantity") +

      scale_x_continuous(limits = c(0, 1.5*max(tb$price, na.rm = T)), 
                         labels = scales::dollar_format()) + 
      scale_y_continuous(limits = c(0, ymax),
                         labels = scales::comma) +   

      theme_minimal()
  })
  

# 📌 Output: MARKET demand model PLOT 📌 ----------------------------------
  
  output$market_demand_plot <- renderPlot({
    req(marketDemand(), input$demand_view)
    tb <- transformedData()
    
    market_list <- marketDemand()
    market_func <- market_list$func
    scaling_factor <- market_list$scaling_factor
    
    # Select appropriate demand function
    demand_func <- if (input$demand_view == "sample") {
      switch(input$model_type,
             "Linear" = function(P) coef(market_list$model)[1] + coef(market_list$model)[2] * P,
             "Exponential" = function(P) exp(coef(market_list$model)[1] + coef(market_list$model)[2] * P),
             "Sigmoid" = function(P) coef(market_list$model)[1] / (1 + exp((coef(market_list$model)[2] - P) / coef(market_list$model)[3])))
    } else {
      market_func
    }
    
    # Plot demand curve
    ggplot(tb, aes(x = price, y = ifelse(input$demand_view == "sample",
                                         quantity,
                                         scaling_factor * quantity))) +
      geom_function(fun = demand_func, color = ifelse(input$demand_view == "sample", 
                                                      "blue", "red"), linewidth = 2) +
      geom_point() +
      labs(title = ifelse(input$demand_view == "sample", 
                          "Sample Demand Curve", "Market Demand Curve"),
           x = "Price", y = "Quantity") +
      theme_minimal()
  })
  
# 📌 Output: demand model SUMMARY 📌 --------------------------------------

  output$model_summary <- renderPrint({
    req(demandModel())  # Ensure model exists
    
    model_list <- demandModel()  # Extract the list
    model <- model_list$model  # ✅ Extract actual model
    
    if (is.null(model)) {
      return("Model fitting failed. Try a different model.")
    }
    
    summary(model)  # ✅ Apply summary() to the extracted model
  })
  
  
# 📌 Output: demand model INTERPRETATION 📌 -------------------------------

  output$model_interpretation <- renderPrint({
    req(demandModel())  # Ensure model exists
    
    model_list <- demandModel()  # Extract list
    model <- model_list$model  # ✅ Extract actual model
    
    if (is.null(model)) {
      return("Model fitting failed. Try a different model.")
    }
    
    pseudo_r2 <- if (!is.null(model_list$pseudo_r2) && !is.na(model_list$pseudo_r2)) 
      round(model_list$pseudo_r2, 4) else "N/A"  # ✅ Prevent errors
    
    interpretation <- switch(
      input$model_type,
      "Linear" = {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        r2 <- round(summary(model)$r.squared, 4)
        c(
          "Linear Demand Interpretation:",
          sprintf("R²: %.2f (%.2f%% of variation in quantity is explained by price).", r2, r2 * 100),
          sprintf("Intercept: If the price is $0, we expect to sell %.2f units.", intercept),
          sprintf("Slope: For every $1 increase in price, we lose %.2f units of quantity sold.", slope)
        )
      },
      "Exponential" = {
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        r2 <- round(summary(model)$r.squared, 4)
        percent_change <- abs((exp(slope) - 1) * 100)
        c(
          "Exponential Demand Interpretation:",
          sprintf("R²: %.2f (%.2f%% of variation in log(quantity) is explained by price).", r2, r2 * 100),
          sprintf("Intercept: Base quantity is %.2f units when price is $0.", exp(intercept)),
          sprintf("Slope: For every $1 increase in price, sales drop by %.2f%%.", percent_change)
        )
      },
      "Sigmoid" = {
        asym <- coef(model)["Asym"]
        xmid <- coef(model)["xmid"]
        scal <- coef(model)["scal"]
        c(
          "Sigmoid Demand Interpretation:",
          sprintf("Asymptote: Maximum quantity is %.2f units.", asym),
          sprintf("Inflection Point: At price $%.2f, demand is most sensitive.", xmid),
          sprintf("Growth Rate: Demand decreases sharply over a price range of approximately %.2f units.", abs(scal)),
          sprintf("Pseudo R²: %s (Measures model fit accuracy)", pseudo_r2)
        )
      }
    )
    
    writeLines(interpretation)  # ✅ Properly formats new lines in Shiny
  })

}


# -------------------------------------------------------------------------
# Run App  ----------------------------------------------------------------
# -------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
