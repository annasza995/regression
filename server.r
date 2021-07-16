library(shiny)
library(DT)
library(stringr)
library(shinyjs)
library(rjags)
library(coda)
library(gamlss)
library(brms)
library(tibble)
library(sjmisc)
library(qpcR)
library(bamlss)
library(rgl)
library(xfun)
library(BMS)
library(caret)


# Define server logic
server <- function(input, output, session) {
  
  output$inputValidation <- renderText({
    "Please select the input file by clicking Browse ... in the side menu. The input file should have a .csv extension"
  })
  
  # Disable submit and downdload buttons 
  disable("submitButton")
  disable("downloadData")
  
  # Create empty dataframe, which will be stored: criterion, variables and criterion value
  
  df <-
    data.frame(
      "Criteria" = character(),
      "Variables" = character(),
      "Value" = double()
    )
  
  # Flag indicating the validity of the input file
  correctInput <- "no"
  
  # Load dataset
  observe({
    file1 = input$datafile
    if (is.null(file1)) {
      return(NULL)
    }
    
    # Dataset path 
    dataPath <- file1$datapath
    
    # Check if the last four characters in the path are different as .csv
    if (str_sub(dataPath, start = -4) != ".csv") {
      # The message that the file is not valid
      output$inputValidation <- renderText({
        "The input file is invalid. Please, select a file with the extension .csv"
      })
      
      correctInput <<- "no"
    }
    # File with the extension .csv file
    
    else {
      # Displaying information about successful file loading and description of the next steps
      output$inputValidation <- renderText({
        "Correctly loaded data file. Please select the selection criteria and variable and click 'Submit' button "
      })
      
      # Change flag
      correctInput <<- "yes"
      
      # Dataset from the selected file 
      myDataset <<- read.csv(dataPath, header = T, sep = ";")
      
      # Display checkboxes with variables
      output$variables <- renderUI({
        
        # If missing input, return to avoid error later in function
        if (is.null(myDataset)) {
          return()
        }
        
        # All variables (with the last one - dependent variable)
        myColumns <<- colnames(myDataset, do.NULL = FALSE)
        
        # All variables without last one (dependent variable)
        myColumnsWithoutLast <<- ""
        
        numberOfColumns <<- length(myColumns)
        i <- 0
        
        # Loop to complete myColumnsWithoutlast
        for (i in 1:numberOfColumns - 1) {
          myColumnsWithoutLast[i] <<- paste(myColumns[i])
        }
        
        # Create the checkboxes containing all independent variables
        checkboxGroupInput("independentVariables", # to get selected variables in server
                           "Variables", # name of uiOutput in Ui.R 
                           choices = myColumnsWithoutLast,
                           choiceValues = 1)
      })
      # After loading checkboxes with independent variables enable submit button 
      enable("submitButton")
    }
  })
  
  
  #
  runModel <- eventReactive(input$submitButton, {
    # check if input is ok 
    if (correctInput == "no") {
      # Display the relevant message 
      output$inputValidation <- renderText({
        "The input file is invalid. Please, select a file with the extension .csv"
      })
      
      # Return dataframe 
      output$result <-
        return(datatable(df))
    }
    # Correct input 
    else {
      
      # Hide message
      output$inputValidation <- renderText({
        ""
      })
      
      # Get names of critera and variables
      selectedCriteria <- input$criteria
      columns <<- input$independentVariables
      
      counter = 0
      selectedVariables <- ""
      for (i in 1:numberOfColumns) {
        selectedVariables[i] <- str_contains(columns, myColumns[i])
        if ((str_contains(columns, myColumns[i]) == "TRUE")) {
          counter = counter + 1
        }
      }
    }
    
    atLeastOneVariable <- str_contains(selectedVariables, "TRUE")
    
    #check if correctly choosen selection criteria and variables
    if (length(selectedCriteria) == 0 && atLeastOneVariable == "FALSE") {
      output$inputValidation <- renderText({
        "Please select the selection criteria and the variables."
      })
      return()
    }
    if (length(selectedCriteria) == 0) {
      output$inputValidation <- renderText({
        "Please select the selection criteria."
      })
      return()
      
    }
    if (atLeastOneVariable == "FALSE") {
      output$inputValidation <- renderText({
        "Please select the variables."
      })
      return()
    }
    else {
      output$inputValidation <- renderText({
        ""
      })
    }
    i = 0
    for (i in 1:length(selectedCriteria)) {
      myCriteria <- ""
      myCriteria <- switch(
        as.integer(selectedCriteria[i]),
        "AIC",
        "BIC",
        "DIC",
        "HQC",
        "GAIC(TIC)",
        "WAIC"
      )
      myVariable <- ""
      val <- ""
      for (i in 1:counter + 1) {
        
        if (i < counter + 2 & i > 2) {
          val <- columns[i - 1]
          myVariable <- paste(myVariable, '+')
          myVariable <- paste(myVariable, val)
        }
        else {
          val <- columns[i - 1]
          
          myVariable <- paste(myVariable, val)
        }
      }
      
      output$inputValidation <- renderText({
        "Correctly loaded data file. Please select the selection criteria and variable and click 'submit'button "
      })
      
      columnNumbers <- length(myColumns)
      
      myDatasetWithoutLast <- myDataset[-c(columnNumbers)]
      myDatasetLast <- myDataset[columnNumbers]
      myDatasetLastName <- colnames(myDatasetLast)
      
      if (length(selectedCriteria) == 1) {
        bma()
        crossvalidation(myVariable, myDatasetLastName)
      }
      x1 <- calculate(myCriteria, myVariable, myDatasetLastName)
      output$inputValidation <- renderText({
        " "
      })
      enable("downloadData")
      #create new dataframe
      b <- reactive({
        data.frame(
          "Criteria" = myCriteria,
          "Variables" = myVariable,
          "Value" = x1
        )
      })
      
      #add new dataframe into main dataframe
      df <<- rbind(df, b())
    }
    return(datatable(df))
  })
  
  
  bma <- function() {
    columnNumbers <- length(myColumns)
    
    myDatasetWithoutLast <- myDataset[-c(columnNumbers)]
    myDatasetLast <- myDataset[columnNumbers]
    
    myDatasetChangeOrderOfColumns <- cbind(myDatasetLast, myDatasetWithoutLast)
    
    bma1 = bms(myDatasetChangeOrderOfColumns, burn = 1000, iter = 2000, mprior = "uniform")
    coef(bma1)
  }
  
  
  crossvalidation <- function(variables, dependentVariable) {
    model1 <- paste(dependentVariable, "~", variables)
    set.seed(100)
    default_glm_mod = caret::train(
      form = eval(parse(text = model1)),
      data = myDataset,
      trControl = trainControl(method = "cv", number = 10),
      method = "glm",
      family = "binomial"
    )
    print(default_glm_mod)
  }
  
  # Function which calculates values of information criteria
  calculate <- function(criteria, variables, myDatasetLastName) {
    
    model1 <- paste(myDatasetLastName, " ~ ", variables)
    if (criteria == "AIC") {
      mAIC <- glm(eval(parse(text = model1)), family = binomial(link = "logit"), data = myDataset)
      x1 <- AIC(mAIC)
      x1 <- round(x1, digits = 2)
      
    }
    else if (criteria == "BIC") {
      mBIC <- glm(eval(parse(text = model1)), family = binomial(link = "logit"), data = myDataset)
      x1 <- BIC(mBIC)
      x1 <- round(x1, digits = 2)
    }
    else if (criteria == "GAIC(TIC)") {
      mGAIC <- gamlss(eval(parse(text = model1)), family = BI(mu.link = "logit"), data = myDataset)
      x1 <- GAIC(mGAIC, k = 3)
      x1 <- round(x1, digits = 2)
    }
    else if (criteria == "WAIC") {
      mWAIC <- bamlss(eval(parse(text = model1)), family = binomial(link = "logit"), data = myDataset)
      x1 <- WAIC(mWAIC)
      x1 <- x1$WAIC1
      x1 <- round(x1, digits = 2)
    }
    else if (criteria == "HQC") {
      mHQC <- glm(eval(parse(text = model1)), family = binomial(link = "logit"), data = myDataset)
      x1 <- HQIC(mHQC)
      x1 <- round(x1, digits = 2)
      
    }
    else if (criteria == "DIC") {
      mDIC <- bamlss(eval(parse(text = model1)), family = binomial(link = "logit"), data = myDataset)
      x1 <- DIC(mDIC)
      x1 <- x1$DIC
      x1 <- round(x1, digits = 2)
    }
    
    return(x1)
  }
  
  # Fuction which rendered table
  output$result <- renderDataTable({
    runModel()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
}