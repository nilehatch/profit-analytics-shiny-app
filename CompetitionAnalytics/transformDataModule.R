library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(rlang)
library(tidyr)


###########
#
# Module UI for data transformation -----------------------------------
#
###########

transformDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Primary selection: Durable vs Non-Durable
    radioGroupButtons(
      ns("primary_transformation"),
      label = "Select Product Type",
      choices = c("Durable Goods", "Non-Durable"),
      selected = "Durable Goods",
      justified = TRUE
    ),
    # Secondary selection (only shown if Non-Durable is chosen)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable'", ns("primary_transformation")),
      radioGroupButtons(
        ns("secondary_transformation"),
        label = "Select Transformation for Non-Durable",
        choices = c("Prices", "WTP"),
        selected = "Prices",
        justified = TRUE
      )
    ),
    # The UI elements that depend on the overall transformation type:
    # For Durable Goods:
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Durable Goods'", ns("primary_transformation")),
      pickerInput(ns("wtpCol_firm1"), "Select WTP Column for Firm 1", choices = NULL,
                  options = list(`live-search` = TRUE)),
      pickerInput(ns("wtpCol_firm2"), "Select WTP Column for Firm 2", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    # For Non-Durable (Prices):
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable' && input['%s'] == 'Prices'", 
                          ns("primary_transformation"), ns("secondary_transformation")),
      radioButtons(ns("mapping_mode"), "Select Mode for Price Pair Mapping",
                   choices = c("Standard Naming Convention", "Custom Mapping"),
                   selected = "Standard Naming Convention"),
      # Help text for standard naming convention.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Standard Naming Convention'", ns("mapping_mode")),
        helpText("Columns must follow a naming convention such as QaPa.5Pb.5 (i.e., Qa = quantity for product A; Pa.5 = price for A = 0.5; Pb.5 = price for B = 0.5)")
      ),
      # Custom Mapping UI: ask for number of price pairs, then display that many mapping rows.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Custom Mapping'", ns("mapping_mode")),
        numericInput(ns("numPairs"), "Number of Price Pairs", value = 1, min = 1, step = 1),
        uiOutput(ns("mappingUI"))
      )
    ),
    # For Non-Durable (WTP):
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable' && input['%s'] == 'WTP'", 
                          ns("primary_transformation"), ns("secondary_transformation")),
      pickerInput(ns("wtpCol_nondurable"), "Select WTP Column", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    actionButton(ns("transform_btn"), "Transform Data"),
    hr(),
    uiOutput(ns("transform_result")),
    verbatimTextOutput(ns("transform_summary_stats"))
  )
}


###########
#
# Module server for data transformation -----------------------------------
#
###########

transformDataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    transformation_type <- reactive({
      if (input$primary_transformation == "Durable Goods") {
        "Durable Goods (Competition)"
      } else if (input$primary_transformation == "Non-Durable") {
        if (input$secondary_transformation == "Prices") {
          "Non-Durable (Prices)"
        } else {
          "Non-Durable (WTP)"
        }
      }
    })
    
    # Update picker inputs with column names from the uploaded data.
    observeEvent(data(), {
      req(data())
      cols <- names(data())
      updatePickerInput(session, "wtpCol_firm1", choices = cols, selected = "")
      updatePickerInput(session, "wtpCol_firm2", choices = cols, selected = "")
      updatePickerInput(session, "wtpCol_nondurable", choices = cols, selected = "")
    })
    
    # --- Custom Mapping UI for Non-Durable (Prices) ---
    # In Custom Mapping mode, generate a fixed number of mapping rows based on input$numPairs.
    output$mappingUI <- renderUI({
      req(input$numPairs)
      n <- input$numPairs
      # For each mapping row, generate four inputs:
      # Price 1, Price 2, picker for Firm 1 quantity column, and picker for Firm 2 quantity column.
      uiList <- lapply(seq_len(n), function(i) {
        fluidRow(
          column(3, numericInput(ns(paste0("price1_", i)), 
                                 paste("Price 1 (Mapping", i, ")"), 
                                 value = 0, min = 0, step = 0.1)),
          column(3, numericInput(ns(paste0("price2_", i)), 
                                 paste("Price 2 (Mapping", i, ")"), 
                                 value = 0, min = 0, step = 0.1)),
          column(3, pickerInput(ns(paste0("qtyFirm1_", i)), 
                                paste("Firm 1 Qty Column (Mapping", i, ")"), 
                                choices = names(data()), selected = "",
                                options = list(`live-search` = TRUE))),
          column(3, pickerInput(ns(paste0("qtyFirm2_", i)), 
                                paste("Firm 2 Qty Column (Mapping", i, ")"), 
                                choices = names(data()), selected = "",
                                options = list(`live-search` = TRUE)))
        )
      })
      do.call(tagList, uiList)
    })
    

# Non-durable WTP transformation function ---------------------------------

    transformNonDurableWTP <- function(data, selected_cols) {
      # selected_cols should be a vector of 6 column names:
      # For Product A: [P_max_A, Q_max_A, Q0_A]
      # For Product B: [P_max_B, Q_max_B, Q0_B]
      
      if (length(selected_cols) != 6) {
        stop("Please select exactly 6 columns for Non-Durable (WTP) transformation.")
      }
      
      df <- data %>%
        rename(
          P_max_A = !!sym(selected_cols[1]),
          Q_max_A = !!sym(selected_cols[2]),
          Q0_A    = !!sym(selected_cols[3]),
          P_max_B = !!sym(selected_cols[4]),
          Q_max_B = !!sym(selected_cols[5]),
          Q0_B    = !!sym(selected_cols[6])
        ) %>%
        mutate(across(c(P_max_A, Q_max_A, Q0_A, P_max_B, Q_max_B, Q0_B), as.numeric)) %>%
        filter(!is.na(P_max_A) & !is.na(Q_max_A) & !is.na(Q0_A) &
                 !is.na(P_max_B) & !is.na(Q_max_B) & !is.na(Q0_B) &
                 P_max_A > 0 & P_max_B > 0) %>%
        mutate(
          slope_A = (Q_max_A - Q0_A) / P_max_A,
          slope_B = (Q_max_B - Q0_B) / P_max_B,
          intercept_A = Q0_A,
          intercept_B = Q0_B
        )
      
      # Generate unique price sequences for each product (including 0)
      price_seq_A <- unique(sort(na.omit(df$P_max_A)))
      price_seq_A <- c(0, price_seq_A)
      price_seq_B <- unique(sort(na.omit(df$P_max_B)))
      price_seq_B <- c(0, price_seq_B)
      
      # Create a grid of all possible price pairs:
      grid <- expand.grid(P_A = price_seq_A, P_B = price_seq_B)
      
      # For each respondent, compute predicted quantities at every price pair.
      # Here we ignore cross-price effects for simplicity.
      df_pred <- df %>% rowwise() %>% mutate(
        Q_A_pred = list(map_dbl(grid$P_A, function(P_A) {
          if (P_A > P_max_A) return(0)
          intercept_A + slope_A * P_A
        })),
        Q_B_pred = list(map_dbl(grid$P_B, function(P_B) {
          if (P_B > P_max_B) return(0)
          intercept_B + slope_B * P_B
        }))
      ) %>% ungroup()
      
      # Combine predictions into a long data frame.
      # For each respondent, replicate the grid and attach the predictions.
      final_predictions <- df_pred %>%
        select(Respondent, Q_A_pred, Q_B_pred) %>%
        mutate(row = row_number()) %>%
        group_by(Respondent) %>%
        do({
          respondent <- .
          n <- nrow(grid)
          tibble(
            Respondent = respondent$Respondent,
            P_A = grid$P_A,
            P_B = grid$P_B,
            Q_A_pred = respondent$Q_A_pred[[1]],
            Q_B_pred = respondent$Q_B_pred[[1]]
          )
        }) %>% ungroup()
      
      # Aggregate across respondents to get market predictions:
      market_quantity <- final_predictions %>%
        group_by(P_A, P_B) %>%
        summarise(
          quantityA = sum(Q_A_pred, na.rm = TRUE),
          quantityB = sum(Q_B_pred, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(P_A), desc(P_B))
      
      return(list(
        individual = final_predictions,
        market = market_quantity
      ))
    }
    
    
    
    # --- Transformation Logic ---
    rawTransformed <- eventReactive(input$transform_btn, {
      req(data())
      # Use the reactive transformation_type, not input$transformation_type
      trans_type <- transformation_type()
      
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
        if (input$mapping_mode == "Standard Naming Convention") {
          transformed_data <- data() %>%
            pivot_longer(
              cols = matches("^Q[AaBb]P[aA](?:\\d*\\.?\\d+)P[bB](?:\\d*\\.?\\d+)$"),
              names_to = c("prod", "priceA", "priceB"),
              names_pattern = "^Q([AaBb])P[aA]((?:\\d*\\.?\\d+))P[bB]((?:\\d*\\.?\\d+))$",
              values_to = "quantity"
            ) %>%
            mutate(
              prod = if_else(toupper(prod) == "A", "quantityA", "quantityB"),
              priceA = as.numeric(priceA),
              priceB = as.numeric(priceB)
            ) %>%
            pivot_wider(
              names_from = prod,
              values_from = quantity
            ) %>%
            group_by(priceA, priceB) %>% 
            summarise(quantityA = sum(quantityA, na.rm = TRUE),
                      quantityB = sum(quantityB, na.rm = TRUE),
                      .groups = "drop")
          return(transformed_data)
          
        } else if (input$mapping_mode == "Custom Mapping") {
          req(input$numPairs)
          mappingValues <- lapply(seq_len(input$numPairs), function(i) {
            list(
              price1 = input[[paste0("price1_", i)]],
              price2 = input[[paste0("price2_", i)]],
              qtyFirm1 = input[[paste0("qtyFirm1_", i)]],
              qtyFirm2 = input[[paste0("qtyFirm2_", i)]]
            )
          })
          transformed_data_list <- lapply(mappingValues, function(m) {
            if (!(m$qtyFirm1 %in% names(data())) || !(m$qtyFirm2 %in% names(data()))) {
              return(NULL)
            }
            df_subset <- data() %>%
              select(any_of(c("Respondent", m$qtyFirm1, m$qtyFirm2))) %>%
              rename(
                quantityFirm1 = !!sym(m$qtyFirm1),
                quantityFirm2 = !!sym(m$qtyFirm2)
              ) %>%
              mutate(
                Price1 = m$price1,
                Price2 = m$price2
              )
            return(df_subset)
          })
          transformed_data_list <- Filter(Negate(is.null), transformed_data_list)
          transformed_data <- bind_rows(transformed_data_list)
          return(transformed_data)
        }
        
      } else if (trans_type == "Non-Durable (WTP)") {
        return(tibble::tibble(Message = "Non-Durable (WTP) transformation not implemented yet."))
      } else {
        return(tibble::tibble(Message = "Unknown transformation type."))
      }
    })

    # Create a UI element for the transformed output.
    transformed <- reactive({
      req(rawTransformed())
      if ("Message" %in% names(rawTransformed())) {
        return(tags$p(rawTransformed()$Message[1]))
      }
      if ("Mappings" %in% names(rawTransformed())) {
        return(DT::datatable(rawTransformed()))
      }
      DT::datatable(rawTransformed())
    })
    
    output$transform_result <- renderUI({
      req(transformed())
      transformed()
    })
    
    output$transform_summary_stats <- renderPrint({
      req(rawTransformed())
      summary(rawTransformed())
    })
    
  })
}