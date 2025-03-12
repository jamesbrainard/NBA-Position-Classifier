##################################################
# NBA Position Analysis with ML - Main App
# 
# This file initializes the Shiny app by sourcing:
# - Global settings
# - UI
# - Server logic
# 
# Author: James Brainard
##################################################

# Loads everything
source("global.R")
source("ui.R")
source("server.R")

# Runs app
shinyApp(ui = ui, server = server)