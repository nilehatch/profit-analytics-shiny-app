library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(rlang)

# Module UI for data transformation
transformDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("transformation_type"), "Select Data Transformation Type",
                choices = c("Durable Goods", "Non-Durable (Prices)", "Non-Durable (WTP)"),
                selected = "Durable Goods"),
    
    # Conditional UI for Durable Goods: one picker input (for now)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Durable Goods'", ns("transformation_type")),
      pickerInput(ns("wtpCol_durable"), "Select WTP Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    # Conditional UI for Non-Durable (Prices)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable (Prices)'", ns("transformation_type")),
      pickerInput(ns("pricePair"), "Select Price Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    # Conditional UI for Non-Durable (WTP)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable (WTP)'", ns("transformation_type")),
      pickerInput(ns("wtpCol_nondurable"), "Select WTP Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    actionButton(ns("transform_btn"), "Transform Data"),
    hr(),
    uiOutput(ns("transform_result"))
  )
}

# Module server for data transformation
transformDataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update the picker inputs with column names from the uploaded data.
    observeEvent(data(), {
      req(data())
      cols <- names(data())
      updatePickerInput(session, "wtpCol_durable", choices = cols, selected = "")
      updatePickerInput(session, "pricePair", choices = cols, selected = "")
      updatePickerInput(session, "wtpCol_nondurable", choices = cols, selected = "")
    })
    
    # When the user clicks the "Transform Data" button,
    # perform the transformation based on the chosen type.
    transformed <- eventReactive(input$transform_btn, {
      req(data())
      trans_type <- input$transformation_type
      
      if (trans_type == "Durable Goods") {
        req(input$wtpCol_durable)
        # Example transformation for durable goods using WTP data:
        transformed_data <- data() %>%
          rename(wtp = !!sym(input$wtpCol_durable)) %>%
          filter(!is.na(wtp)) %>%
          group_by(wtp) %>%
          summarise(count = n(), .groups = "drop") %>%
          arrange(desc(wtp)) %>%
          mutate(quantity = cumsum(count), price = wtp)
        return(DT::datatable(transformed_data))
        
      } else if (trans_type == "Non-Durable (Prices)") {
        # For now, just return a placeholder message.
        return(tags$p("Non-Durable (Prices) transformation not implemented yet."))
        
      } else if (trans_type == "Non-Durable (WTP)") {
        # For now, just return a placeholder message.
        return(tags$p("Non-Durable (WTP) transformation not implemented yet."))
        
      } else {
        return(tags$p("Unknown transformation type."))
      }
    })
    
    output$transform_result <- renderUI({
      req(transformed())
      transformed()
    })
  })
}