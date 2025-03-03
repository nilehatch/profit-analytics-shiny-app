library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(rlang)

# Module UI for data transformation (Competition Version)
transformDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("transformation_type"), "Select Data Transformation Type",
                choices = c("Durable Goods (Competition)", 
                            "Non-Durable (Prices)", 
                            "Non-Durable (WTP)"),
                selected = "Durable Goods (Competition)"),
    
    # Conditional UI for Durable Goods (Competition): Two picker inputs
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Durable Goods (Competition)'", ns("transformation_type")),
      pickerInput(ns("wtpCol_firm1"), "Select WTP Column for Firm 1", choices = NULL,
                  options = list(`live-search` = TRUE)),
      pickerInput(ns("wtpCol_firm2"), "Select WTP Column for Firm 2", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    # Conditional UI for Non-Durable (Prices) (Placeholder)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable (Prices)'", ns("transformation_type")),
      pickerInput(ns("pricePair"), "Select Price Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    # Conditional UI for Non-Durable (WTP) (Placeholder)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable (WTP)'", ns("transformation_type")),
      pickerInput(ns("wtpCol_nondurable"), "Select WTP Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    
    actionButton(ns("transform_btn"), "Transform Data"),
    hr(),
    uiOutput(ns("transform_result")),
    #uiOutput(ns("transform_summary_stats"))    
    verbatimTextOutput(ns("transform_summary_stats"))
  )
}

# Module server for data transformation (Competition Version)
transformDataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update picker inputs with column names from the uploaded data.
    observeEvent(data(), {
      req(data())
      cols <- names(data())
      updatePickerInput(session, "wtpCol_firm1", choices = cols, selected = "")
      updatePickerInput(session, "wtpCol_firm2", choices = cols, selected = "")
      updatePickerInput(session, "pricePair", choices = cols, selected = "")
      updatePickerInput(session, "wtpCol_nondurable", choices = cols, selected = "")
    })
    
    # When the user clicks "Transform Data", perform the appropriate transformation.
    rawTransformed <- eventReactive(input$transform_btn, {
      req(data())
      trans_type <- input$transformation_type
      
      if (trans_type == "Durable Goods (Competition)") {
        req(input$wtpCol_firm1, input$wtpCol_firm2)
        
        transformed_data <- data() %>%
          rename(wtp1 = !!sym(input$wtpCol_firm1),
                 wtp2 = !!sym(input$wtpCol_firm2)) %>%
          filter(!is.na(wtp1) & !is.na(wtp2)) %>%
          group_by(wtp1, wtp2) %>%
          summarize(wtp_1_2_count = n(), .groups = "drop") %>%
          arrange(desc(wtp1), wtp2) %>%
          mutate(quantity1 = cumsum(wtp_1_2_count)) %>%
          arrange(desc(wtp2), wtp1) %>%
          mutate(quantity2 = cumsum(wtp_1_2_count)) %>%
          mutate(price1 = wtp1,
                 price2 = wtp2)
        return(transformed_data)
        
      } else if (trans_type == "Non-Durable (Prices)") {
        return(tibble(Message = "Non-Durable (Prices) transformation not implemented yet."))
        
      } else if (trans_type == "Non-Durable (WTP)") {
        return(tibble(Message = "Non-Durable (WTP) transformation not implemented yet."))
        
      } else {
        return(tibble(Message = "Unknown transformation type."))
      }
    })
    
    # Use the raw data to create the UI element (datatable)
    transformed <- reactive({
      req(rawTransformed())
      # If the raw data has a column named Message, display that text instead.
      if ("Message" %in% names(rawTransformed())) {
        return(tags$p(rawTransformed()$Message[1]))
      }
      DT::datatable(rawTransformed())
    })
    
    # Render the UI element for transformed data.
    output$transform_result <- renderUI({
      req(transformed())
      transformed()
    })
    
    # Render summary stats based on the raw transformed data.
    output$transform_summary_stats <- renderPrint({
      req(rawTransformed())
      summary(rawTransformed())
    })

  })
}