##################################################
# NBA Position Analysis with ML - Server
# 
# This file defines the server-side logic
# for processing inputs, running the model,
# and generating outputs.
# 
# Author: James Brainard
##################################################

# Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(randomForest)

# Scripts
source("scripts/nba_data_processing.R")
source("scripts/nba_modeling.R")
source("scripts/plots.R")