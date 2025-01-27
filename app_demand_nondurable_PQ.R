# Define the server with modifications for new case
server <- function(input, output, session) {
  # Reactive: Upload and read data
  userData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header, sep = input$sep)
  })
  
  # Update WTP column choices dynamically
  observeEvent(userData(), {
    updateSelectInput(session, "wtpCol", choices = names(userData()))
    updateSelectInput(session, "quantityCol", choices = names(userData()))
  })
  
  # Reactive: Transform data for WTP + Quantity at WTP and WTP/2 + Quantity at WTP/2
  transformedData <- reactive({
    req(userData(), input$wtpCol, input$quantityCol)
    tb <- userData()
    
    tb_transformed <- tb %>%
      mutate(
        WTP_half = !!sym(input$wtpCol) / 2,  # Calculate WTP/2
        Quantity_half = !!sym(input$quantityCol)  # Assuming quantity at WTP/2 is provided
      ) %>%
      select(!!sym(input$wtpCol), !!sym(input$quantityCol), WTP_half, Quantity_half) %>%
      rename(
        WTP = !!sym(input$wtpCol),
        Quantity = !!sym(input$quantityCol)
      ) %>%
      pivot_longer(cols = c(WTP, WTP_half), names_to = "Price_Type", values_to = "Price") %>%
      pivot_longer(cols = c(Quantity, Quantity_half), names_to = "Quantity_Type", values_to = "Quantity") %>%
      filter(!is.na(Price), !is.na(Quantity)) %>%
      arrange(desc(Price)) %>%
      group_by(Price) %>%
      summarise(Quantity = sum(Quantity), .groups = "drop") %>%
      mutate(Cumulative_Quantity = cumsum(Quantity))
    
    # Update price slider dynamically
    updateSliderInput(session, "price", min = 0, max = max(tb_transformed$Price, na.rm = TRUE), step = max(tb_transformed$Price) / 25)
    
    return(tb_transformed)
  })
  
  # Remaining reactive and output logic stays similar but adapts to the new data
  ...
}

# Define the UI modifications
ui <- fluidPage(
  titlePanel("Expected Customer Demand (WTP + Quantity at Two Prices)"),
  
  tabsetPanel(
    # Data Tab
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Upload CSV File", accept = c("text/csv", ".csv")),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                 selectInput("wtpCol", "Select WTP Column", choices = NULL),
                 selectInput("quantityCol", "Select Quantity Column", choices = NULL)
               ),
               mainPanel(DT::DTOutput("data_table"), verbatimTextOutput("data_summary"))
             )),
    
    # Demand Visualization and Analysis Tab
    tabPanel("Customer Demand",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Choose Demand Model", choices = c("Linear Demand", "Exponential Demand", "Sigmoid Demand")),
                 sliderInput("price", "Price", min = 0, max = 100, value = 50, step = 1)
               ),
               mainPanel(plotOutput("demand_plot"), verbatimTextOutput("interpretation"), verbatimTextOutput("model_summary"))
             ))
  )
)

# Run the modified app
shinyApp(ui = ui, server = server)
