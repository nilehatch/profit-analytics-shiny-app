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
             dataUploadUI("dataUpload1")  # Your data upload module
    ),
    tabPanel("Data Transformation",
             transformDataUI("transformData1")  # The transformation module
    )
    # Other tabs...
  )
)