library(shiny)
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
                 # For durable goods: simple willingness-to-pay inputs for two products
                 conditionalPanel(
                   condition = "input.product_type == 'Durable Goods'",
                   selectInput("wtpCol_product1", "Select WTP Column for Product 1", choices = NULL),
                   selectInput("wtpCol_product2", "Select WTP Column for Product 2", choices = NULL)
                 ),
                 # For non-durable goods: price pair inputs (advanced approach)
                 conditionalPanel(
                   condition = "input.product_type == 'Non-Durable Goods'",
                   selectInput("pricePair1", "Select Price Column for Price Pair 1", choices = NULL),
                   selectInput("quantityPair1", "Select Quantity Column for Price Pair 1", choices = NULL),
                   selectInput("pricePair2", "Select Price Column for Price Pair 2", choices = NULL),
                   selectInput("quantityPair2", "Select Quantity Column for Price Pair 2", choices = NULL)
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
                 selectInput("demand_model", "Select Demand Model",
                             choices = c("Linear", "Exponential")),
                 # Price sliders for each firm
                 sliderInput("firm1_price", "Firm 1 Price", min = 0, max = 100, value = 10, step = 1),
                 sliderInput("firm2_price", "Firm 2 Price", min = 0, max = 100, value = 10, step = 1),
                 # Market size for each firm
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
                 numericInput("variable_cost_firm2", "Variable Cost per Unit (Firm 2)", value = 10, min = 0, step = 0.01)
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

# End of UI definition

shinyApp(ui = ui, server = function(input, output, session) {})