library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  #title
  titlePanel(h2(
    "Model Selection Criteria in Regression ", align = "center"
  )),
  
  # Sidebar panel for inputs
  sidebarPanel(
    # Input file 
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values')),
    
    # Selection of criteria
    checkboxGroupInput(
      "criteria",
      h4("Selection criteria"),
      choices = list(
        "AIC" = 1,
        "BIC" = 2,
        "DIC" = 3,
        "HQC" = 4,
        "GAIC(TIC)" = 5,
        "WAIC" = 6
      ),
      selected = 1
    ),
  
    # Selection of variables 
    uiOutput("variables"),
    
   
    # Submit & downdload  buttons
    actionButton("submitButton", "Submit"),
    downloadButton("downloadData", "Download")
  ),
  
  # Main panel for displaying outputs
  mainPanel (
  textOutput(("inputValidation")),
  tags$head(tags$style("#inputValidation{
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
  )
  ), 
  # Horizontal line ----
  tags$hr(),
  
  withSpinner(
  dataTableOutput("result"))
  )
)