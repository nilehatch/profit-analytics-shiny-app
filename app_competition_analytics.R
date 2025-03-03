library(shiny)
library(shinyWidgets)
library(DT)

ui <- fluidPage(
  
  # Custom CSS for consistent styling
  tags$head(
    tags$style(HTML("
      .chosen-model-box {
        background-color: #F0F8FF !important;
        border: 1px solid #ccc !important;
        padding: 5px 10px !important;
        border-radius: 4px !important;
        font-size: 14px !important;
        height: 40px !important;
        display: flex;
        align-items: center;
        justify-content: left;
      }
      /* Additional custom styles can go here */
    "))
  ),
  
  titlePanel("Competition Analytics for Entrepreneurs"),
  
  tags$hr(style = "border-top: 1px solid #000;"),
  
  # -------------------------------
  # Tabset 1: Data Upload & Transformation
  # -------------------------------
  tabsetPanel(
    id = "data_tabset",
    
    tabPanel("Upload & Preview Data",
             sidebarLayout(
               sidebarPanel(
                 # Traditional file input (unchanged)
                 fileInput("file1", "Upload CSV Data", accept = ".csv"),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator", 
                              choices = c(Comma = ",", Tab = "\t"), 
                              selected = ","),
                 radioButtons("product_type", "Product Type", 
                              choices = c("Durable Goods", "Non-Durable Goods"),
                              selected = "Durable Goods")
               ),
               mainPanel(
                 h4("Raw Data Preview"),
                 DTOutput("data_preview"),
                 h4("Column Names"),
                 verbatimTextOutput("column_names")
               )
             )
    ),
    
    tabPanel("Transform Data",
             sidebarLayout(
               sidebarPanel(
                 # Use pickerInput for column selection to manage long names
                 conditionalPanel(
                   condition = "input.product_type == 'Durable Goods'",
                   pickerInput("wtpCol_product1", "Select WTP Column for Product 1", choices = NULL,
                               options = list(`live-search` = TRUE)),
                   pickerInput("wtpCol_product2", "Select WTP Column for Product 2", choices = NULL,
                               options = list(`live-search` = TRUE)),
                   pickerInput("quantityCol_durable", "Select Quantity Column", choices = NULL,
                               options = list(`live-search` = TRUE))
                 ),
                 conditionalPanel(
                   condition = "input.product_type == 'Non-Durable Goods'",
                   pickerInput("pricePair", "Select Price Column", choices = NULL,
                               options = list(`live-search` = TRUE)),
                   pickerInput("quantityPair", "Select Quantity Column", choices = NULL,
                               options = list(`live-search` = TRUE)),
                   pickerInput("wtpCol_nondurable", "Select WTP Column", choices = NULL,
                               options = list(`live-search` = TRUE))
                 )
               ),
               mainPanel(
                 h4("Transformed Data Preview"),
                 DTOutput("transformed_data")
               )
             )
    )
  ),
  
  tags$hr(style = "border-top: 1px solid #000;"),
  
  # -------------------------------
  # Tabset 2: Demand Estimation
  # -------------------------------
  tabsetPanel(
    id = "demand_tabset",
    
    tabPanel("Demand Estimation",
             sidebarLayout(
               sidebarPanel(
                 # Allow separate selection of demand model for each firm
                 selectInput("demand_model_firm1", "Select Demand Model for Firm 1",
                             choices = c("Linear", "Exponential"), selected = "Linear"),
                 selectInput("demand_model_firm2", "Select Demand Model for Firm 2",
                             choices = c("Linear", "Exponential"), selected = "Linear"),
                 # Numeric inputs for precise price values
                 numericInput("firm1_price", "Firm 1 Price", value = 10, min = 0, step = 0.1),
                 numericInput("firm2_price", "Firm 2 Price", value = 10, min = 0, step = 0.1),
                 numericInput("market_size_firm1", "Market Size for Firm 1", value = 10000, min = 1, step = 100),
                 numericInput("market_size_firm2", "Market Size for Firm 2", value = 10000, min = 1, step = 100)
               ),
               mainPanel(
                 h4("Demand Curve - Firm 1"),
                 plotOutput("demand_plot_firm1"),
                 h4("Demand Curve - Firm 2"),
                 plotOutput("demand_plot_firm2"),
                 h4("Demand Model Summary"),
                 DTOutput("demand_model_summary")
               )
             )
    )
  ),
  
  tags$hr(style = "border-top: 1px solid #000;"),
  
  # -------------------------------
  # Tabset 3: Profit & Equilibrium Analysis
  # -------------------------------
  tabsetPanel(
    id = "profit_tabset",
    
    tabPanel("Profit & Equilibrium Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Firm 1 Cost Inputs"),
                 numericInput("fixed_cost_firm1", "Fixed Cost (Firm 1)", value = 1000, min = 0, step = 1),
                 numericInput("variable_cost_firm1", "Variable Cost per Unit (Firm 1)", value = 10, min = 0, step = 0.01),
                 h4("Firm 2 Cost Inputs"),
                 numericInput("fixed_cost_firm2", "Fixed Cost (Firm 2)", value = 1000, min = 0, step = 1),
                 numericInput("variable_cost_firm2", "Variable Cost per Unit (Firm 2)", value = 10, min = 0, step = 0.01),
                 h4("Profit Analysis Price Inputs"),
                 numericInput("profit_price_firm1", "Firm 1 Price for Profit Analysis", value = 10, min = 0, step = 0.1),
                 numericInput("profit_price_firm2", "Firm 2 Price for Profit Analysis", value = 10, min = 0, step = 0.1)
               ),
               mainPanel(
                 h4("Profit Maximization & Reaction Functions"),
                 plotOutput("profit_plot"),
                 verbatimTextOutput("equilibrium_info")
               )
             )
    )
  )
)

shinyApp(ui = ui, server = function(input, output, session) {})