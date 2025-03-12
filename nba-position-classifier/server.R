##################################################
# NBA Position Analysis with ML - Server
# 
# This file defines the server-side logic
# for processing inputs, running the model,
# and generating outputs.
# 
# Author: James Brainard
##################################################

# Load necessary libraries for server logic
library(shiny)
library(dplyr)
library(ggplot2)
library(randomForest)

# Define server function
server <- function(input, output, session) {
  # Reactive value to store player data
  players <- reactiveVal(NULL)
  
  # Loads and preprocesses data
  observeEvent(input$load_data, {
    req(input$year)
    showModal(modalDialog("Loading data, please wait...", footer = NULL))
    
    data <- tryCatch({
      extract_and_process_player_data(year = input$year)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to load data. Please check the NBA season year or your internet connection.",
        easyClose = TRUE
      ))
      return(NULL)
    })
    
    removeModal()
    
    if (!is.null(data)) {
      players(data)
      
      # Excludes "Pos", the target variable, from selectable features.
      # Would cause the app to error.
      feature_choices <- colnames(data %>% select(-Player, -Pos))
      
      updateCheckboxGroupInput(
        session, 
        "features", 
        choices = feature_choices, 
        selected = feature_choices[1:min(5, length(feature_choices))]
      )
    }
  })
  
  # Runs the bootstrapping model
  model_results <- eventReactive(input$run_model, {
    req(players(), input$features)
    showModal(modalDialog("Running model, please wait...", footer = NULL))
    
    results <- tryCatch({
      brainard_bootstrapping(
        data = players(),
        features = input$features,
        num_iterations = input$num_iterations,
        ntree = input$ntree,
        mtry = input$mtry
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Model failed to run. Please check the selected parameters.",
        easyClose = TRUE
      ))
      return(NULL)
    })
    
    removeModal()
    results
  })
  
  # Renders confusion matrix plot
  output$confusion_plot <- renderPlot({
    req(model_results())
    plot_confusion_matrix(model_results()$aggregate_confusion_matrix)
  })
  
  # Renders feature importance plot
  output$feature_plot <- renderPlot({
    req(model_results())
    plot_feature_importance(model_results()$average_feature_importance)
  })
}