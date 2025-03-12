##################################################
# NBA Position Analysis with ML - UI
# 
# This file defines the user interface (UI) for
# the Shiny application, including inputs, the
# layout, and other visual elements.
# 
# Author: James Brainard
##################################################

# Needed shiny library to create UI
library(shiny)

# Defines UI
ui <- fluidPage(
  titlePanel("NBA Player Analysis and Bootstrapping"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Options"),
      numericInput("year", "NBA Season (Year):", value = 2025, min = 2000, max = 2025),
      actionButton("load_data", "Load Data"),
      
      h4("Model Parameters"),
      checkboxGroupInput(
        "features", 
        "Select Features:",
        choices = NULL
      ),
      numericInput("num_iterations", "Number of Iterations:", value = 10, min = 1),
      numericInput("ntree", "Number of Trees (ntree):", value = 201, min = 1),
      numericInput("mtry", "Number of Features in Tree (mtry):", value = 4, min = 1),
      actionButton("run_model", "Run Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Confusion Matrix", plotOutput("confusion_plot")),
        tabPanel("Feature Importance", plotOutput("feature_plot"))
      )
    )
  )
)
