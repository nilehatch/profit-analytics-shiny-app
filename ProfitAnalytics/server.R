library(shiny)

# Source module definitions
source("transformDataModule.R")
source("dataUploadModule.R")  # Your data upload module

server <- function(input, output, session) {
  # Call the data upload module; it returns reactive data.
  uploadedData <- dataUploadServer("dataUpload1")
  
  # Call the data transformation module, passing the uploaded data.
  transformDataServer("transformData1", data = uploadedData)
}

shinyApp(ui, server)

