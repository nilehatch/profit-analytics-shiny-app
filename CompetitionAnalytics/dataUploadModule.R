# Module UI for data upload and preview
dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file1"), "Upload CSV Data", accept = ".csv"),
    checkboxInput(ns("header"), "Header", TRUE),
    radioButtons(ns("sep"), "Separator", 
                 choices = c(Comma = ",", Tab = "\t"), 
                 selected = ","),
    h4("Raw Data Preview"),
    DT::DTOutput(ns("data_preview")),
    h4("Column Names"),
    verbatimTextOutput(ns("column_names")),
    h4("Data Summary"),
    verbatimTextOutput(ns("summary_stats"))
    
  )
}

# Module server for data upload and preview
dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read the uploaded CSV file
    userData <- reactive({
      req(input$file1)
      read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    })
    
    # Display the first 10 rows of the data
    output$data_preview <- DT::renderDT({
      req(userData())
      #DT::datatable(head(userData(), 10))
      DT::datatable(userData())
    })
    
    # Display column names
    output$column_names <- renderPrint({
      req(userData())
      names(userData())
    })
    
    output$summary_stats <- renderPrint({
      req(userData())
      summary(userData())
    })
    
    observeEvent(userData(), {
      req(userData())  
      if (sum(is.na(userData())) > 0) {  
        showNotification(paste0("Warning: Dataset contains ", sum(is.na(userData())), " missing values. Rows with NA will be excluded."), 
                         type = "warning")
      }
    })
    
    # Return the reactive data for use in other modules
    return(userData)
  })
}