library(shiny)
library(shinyWidgets)
library(DT)

# Source the module file
source("transformDataModule.R")
source("dataUploadModule.R")  # Assuming you have a module for data upload

ui <- fluidPage(
  titlePanel("Competition Analytics for Entrepreneurs"),
  
  tabsetPanel(
    tabPanel("Data Upload",
             h3("Upload and Preview Customer Data"),
             dataUploadUI("dataUpload1")  # Your data upload module
    ),
    tabPanel("Data Transformation",
             h3("Transform Customer Data into Demand Data"),
             transformDataUI("transformData1")  # The transformation module

    )
    # Other tabs...
  )
)