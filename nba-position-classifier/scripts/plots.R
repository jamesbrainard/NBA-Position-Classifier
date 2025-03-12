##################################################
# NBA Position Visualization Functions
# 
# This file contains functions for plotting
# model evaluation metrics.
# 
# Author: James Brainard
##################################################

library(ggplot2)

# Confusion matrix plot
plot_confusion_matrix <- function(confusion_matrix) {
  confusion_df <- as.data.frame(as.table(confusion_matrix))
  
  ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(
      title = "Aggregated Predicted vs Actual Positions",
      x = "Actual",
      y = "Predicted"
    )
}

# Feature importance plot
plot_feature_importance <- function(feature_importance) {
  ggplot(feature_importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini, fill = Feature)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = "Top Features by Mean Decrease in Gini",
      x = "Feature",
      y = "Mean Decrease in Gini"
    ) +
    scale_fill_brewer(palette = "Set3") +
    theme(legend.position = "none")
}