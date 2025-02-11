#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#install.packages("shiny")
#install.packages("bslib")
#install.packages("devtools")
#devtools::install_git("https://github.com/probably-jaden/Pricer", force = TRUE, upgrade = "always")

library(shiny)
library(Pricer)
library(bslib)


# Define UI for data upload app ----
ui <- fluidPage(
  withMathJax(),
  # App title ----
  titlePanel("Profit Analytics - Monopoly"),
  #theme = bs_theme(``
  #  bootswatch = "darkly",
  #  base_font = font_google("Inter"),
  #  navbar_bg = "#25443B"
  #),
  
  # Sidebar layout with input and output definitions ----
  #fluidRow(
  sidebarLayout(
    # Sidebar panel for inputs ----
    #column(4,
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
    ),
    mainPanel(
      dataTableOutput("contents")
    )
  ),
  
  tags$head(
    tags$style(HTML("
      .scrollable-text {
        max-height: 450px;
        overflow-y: auto;
        white-space: pre-wrap;  /* preserve formatting */
      }
    "))
  ),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("wtpCol", "Name of Willingness-to-Pay Column", choices = c("FILE NEEDS TO BE UPLOADED")),
             selectInput("regressType", "Demand Curve Shape", choices = c("Linear", "Exponential", "Sigmoid")),
             numericInput("pop", "Target Customer Population Size", min = 0, value = 1000),
             sliderInput("price", "Price", min = 0, max = 10, value = 3, step = .25)
           )
    ),
    column(4,
           tabsetPanel(
             tabPanel("Demand",
                      plotOutput("demand_Plot_fit")
             )
           )
    ),
    column(4,
           #plotOutput("revenue_Plot_fit")
           tabsetPanel(
             tabPanel("Interpretations",  align = "center",
                      br(), br(), br(),
                      uiOutput("rSq_text"),
                      div(style = "font-size: 14px;",
                          uiOutput("rSq_interpretation")
                      ),
                      br(), br(), br(),
                      uiOutput("demand_math_formula"),
                      div(style = "font-size: 14px;",
                          uiOutput("intercept_interpretation")
                      ),
                      #br(),
                      div(style = "font-size: 14px;",
                          uiOutput("slope_interpretation")
                      )
                      #br(),
                      
             ),
             
             tabPanel("Summary",
                      div(class = "scrollable-text", verbatimTextOutput("demand_summary", placeholder = TRUE))
             )
           )
    )
  ),
  fluidRow(
    column(4,
           wellPanel(
             numericInput("var", "Variable Cost of Product", min = 0, value = 10),
             numericInput("fix", "Fixed Cost (Overhead)", min = 0, value = 1000)
           )
    ),
    column(4,
           tabsetPanel(
             tabPanel("Profit",
                      plotOutput("profit_Plot_func"),
             )
           )
    ),
    column(4,
           tabsetPanel(
             tabPanel("optimized profit", align = "center",
                      br(), br(),
                      div(style = "font-size: 20px;",
                          uiOutput("opt_price")
                      ),
                      br(),
                      div(style = "font-size: 20px;",
                          uiOutput("opt_profit")
                      )
             ),
             tabPanel("revenue & cost",
                      plotOutput("profit_Plot")
             )
           )
    )
  )
)



server <- function(input, output) {
  
  userData <- reactive({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    df
  })
  
  output$contents <- renderDataTable(userData(), options = list(pageLength = 8))
  
  observeEvent(input$file1, {
    updateSelectInput(inputId = "wtpCol", choices = names(userData()))
  })
  
  userWTPCol <- reactive({
    if(input$wtpCol == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      return(userData()[[input$wtpCol]])
    }
  })
  
  userCleanData <- reactive({
    if(is.null(userWTPCol())){
      return(NULL)
    }
    demandDurable(userData(), input$wtpCol)
  })
  
  userPop <- reactive(input$pop)
  userSample <- reactive(nrow(userCleanData()))
  userType <- reactive(input$regressType)
  
  observeEvent(input$wtpCol, {
    #browser()
    if(is.null(userWTPCol())){
      return(NULL)
    }
    updateSliderInput(inputId = "price", min = min(userWTPCol()), max = max(userWTPCol()), value = mean(userWTPCol()))
  })
  
  userPrice <- reactive(input$price)
  
  observeEvent(input$wtpCol, {
    if(is.null(userWTPCol())){
      return(NULL)
    }
    updateNumericInput(inputId = "var", value = roundLog(mean(userWTPCol()) * .4))
    updateNumericInput(inputId = "fix", value = roundLog(mean(userWTPCol()) * userPop() * .1))
  })
  
  output$demand_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlot(price = userPrice(), data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })
  
  output$revenue_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    revenuePlot(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })
  
  output$demand_summary <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandSummary(data = userCleanData(), type = userType())
  })
  
  output$intercept_interpretation <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    
    intText <- demandInterpret(data = userCleanData(),
                               type = userType(),
                               population = userPop(),
                               sample = userSample())[[1]]
    
    HTML(intText)
  })
  
  output$slope_interpretation <- renderText({
    if(is.null(userCleanData())){
      return(NULL)
    }
    
    slopeText <- demandInterpret(data = userCleanData(),
                                 type = userType(),
                                 population = userPop(),
                                 sample = userSample())[[2]]
    HTML(slopeText)
  })
  
  output$demand_math_formula <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    latexMaths <- demandFormula2(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
    
    withMathJax(
      helpText(paste0("$$\\large{ \\text{Quantity Sold} \\ = \\ ", latexMaths, "}$$"))
    )
  })
  
  output$demand_formula <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandFormula(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })
  
  output$rSq_text  <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    rSq <- round(rSquaredDemand(data = userCleanData(), userType()), 2)
    withMathJax(
      helpText(paste0("$$\\Large{R^2 \\ = \\ ", rSq, "}$$"))
    )
  })
  
  output$rSq_interpretation <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    rSq <- round(rSquaredDemand(data = userCleanData(), userType()), 3)
    rSqInterp <- paste0("<b>", rSq * 100, "%</b> of the variation in quantity sold can be explained by the variation in price.")
    HTML(rSqInterp)
  })
  
  userVar <- reactive(input$var)
  userFix <- reactive(input$fix)
  
  output$opt_price <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    
    fQ <- fQ(data = userCleanData(), userType(), userPop(), userSample())
    if (class(fQ) == class(NA)) {
      return(NA)
    }
    fR <- fR(userCleanData(), userType(), userPop(), userSample())
    fC <- fC(userVar(), userFix(), fQ)
    fPi <- fPi(fR, fC)
    
    opt_Price <- optimize(fPi, lower = 0, upper = max(userCleanData()$wtp), maximum = TRUE)[[1]]
    show_Price <- paste0("<b>Price</b>: $", conNum_short(round(opt_Price, 2)))
    
    HTML(show_Price)
  })
  
  output$opt_profit <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    
    fQ <- fQ(data = userCleanData(), userType(), userPop(), userSample())
    if (class(fQ) == class(NA)) {
      return(NA)
    }
    fR <- fR(userCleanData(), userType(), userPop(), userSample())
    fC <- fC(userVar(), userFix(), fQ)
    fPi <- fPi(fR, fC)
    
    opt_Profit <- optimize(fPi, lower = 0, upper = max(userCleanData()$wtp), maximum = TRUE)[[2]]
    show_Profit <- paste0("<b>Profit</b>: $", conNum_long(round(opt_Profit, 2)))
    
    HTML(show_Profit)
  })
  
  output$profit_Plot <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitPlot(data = userCleanData(), type = userType(), variable = userVar(), fixed = userFix(), population = userPop(), sample = userSample())
  })
  
  output$profit_Plot_func <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitFunctionPlot(price = userPrice(), data = userCleanData(), type = userType(), variable = userVar(), fixed = userFix(), population = userPop(), sample = userSample())
  })
}

# Create Shiny app ----
shinyApp(ui, server)


#demand
#demandFormula(cpC, "Linear", 1, 1)
#demandFormula2(cpC, "Exponential", 1, 1)[[1]]
#demandInterpret(cpC, "Linear", 1, 1)[[1]]