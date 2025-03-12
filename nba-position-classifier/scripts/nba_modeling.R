##################################################
# NBA Position Bootstrapping Model
# 
# This file implements a bootstrapping-based
# random forest model to classify NBA player
# positions using selected features
# 
# Author: James Brainard
##################################################

library(randomForest)
library(dplyr)
library(ggplot2)
library(tibble)

# Brainard Bootstrapping takes in required data and features arguments (specify 
# dataset and which features to use) but also allows for adjustable default values 
# in target, num_iterations, ntree, mtry, and levels.
brainard_bootstrapping <- function(data, features, target = "Pos", num_iterations = 25, ntree = 201, mtry = 4, levels = c("PG", "SG", "SF", "PF", "C")) {
  results <- list()  # Initializes a results list
  
  # Resets total data from last call
  all_confusion_matrices <- list()
  all_accuracies <- numeric()
  all_feature_importance <- data.frame()
  
  # Changes Pos (or whatever other target) to a factor
  data[[target]] <- as.factor(data[[target]])
  
  for (i in seq_len(num_iterations)) {
    # Bootstrap resampling!
    indices <- sample(1:nrow(data), replace = TRUE)
    train_data <- data[indices, ]
    test_data <- data[-indices, ]
    
    # Formula for the model based on the parameters from the function
    formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
    
    # RF model
    rf_model <- randomForest(
      formula,
      data = train_data,
      na.action = na.omit,
      ntree = ntree,
      mtry = mtry
    )
    
    # Predictions and actual values
    forest_predictions <- predict(rf_model, test_data, type = "class")
    forest_predictions <- factor(forest_predictions, levels = levels)
    actual <- factor(test_data[[target]], levels = levels)
    
    # Confusion matrix
    confusion_matrix <- table(Predicted = forest_predictions, Actual = actual)
    
    # Accuracy
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    # Quadratic Weighted Kappa
    actual_numeric <- as.numeric(actual)
    predicted_numeric <- as.numeric(forest_predictions)
    
    qwk <- Metrics::ScoreQuadraticWeightedKappa(
      predicted_numeric,
      actual_numeric,
      min.rating = 1,
      max.rating = 5
    )
    
    # Feature importance
    feature_importance <- importance(rf_model) %>%
      as.data.frame() %>%
      rownames_to_column(var = "Feature") %>%
      arrange(desc(MeanDecreaseGini))
    
    # Store results
    results[[i]] <- list(
      confusion_matrix = confusion_matrix,
      accuracy = accuracy,
      qwk = qwk,
      feature_importance = feature_importance
    )
  }
  
  # Adds all results together
  all_confusion_matrices <- lapply(results, `[[`, "confusion_matrix")
  all_accuracies <- sapply(results, `[[`, "accuracy")
  all_qwks <- sapply(results, `[[`, "qwk")
  all_feature_importance <- bind_rows(
    lapply(results, function(res) res$feature_importance),
    .id = "Iteration"
  )
  
  # Aggregates accuracy
  mean_accuracy <- mean(all_accuracies)
  
  # Aggregates QWK
  mean_qwk <- mean(all_qwks)
  
  # Aggregates feature importances
  average_feature_importance <- all_feature_importance %>%
    group_by(Feature) %>%
    summarize(MeanDecreaseGini = mean(MeanDecreaseGini, na.rm = TRUE)) %>%
    arrange(desc(MeanDecreaseGini)) %>%
    head(n = 10)
  
  # Aggregated confusion matrix plot
  # Must create confusion_plot object or it won't print out of a function
  aggregate_confusion_matrix <- Reduce(`+`, all_confusion_matrices)
  confusion_df <- as.data.frame(as.table(aggregate_confusion_matrix))
  confusion_plot <- ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "orange") +
    labs(
      title = "Aggregated Predicted vs Actual Positions",
      x = "Actual",
      y = "Predicted",
      caption = "Figure 1. A Confusion Matrix for the model's aggregated predictions."
    )
  
  # Explicitly printing confusion matrix plot
  print(confusion_plot)
  
  # Aggregated feature importance plot
  # Must create feature_plot object or it won't print out of a function
  feature_plot <- ggplot(average_feature_importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini, fill = Feature)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = "Top Features by Mean Decrease in Gini",
      x = "Feature",
      y = "Mean Decrease in Gini",
      caption = "Figure 2. A bar plot showing the most important features from the model."
    ) +
    scale_fill_brewer(palette = "Set3") +
    theme(legend.position = "none")
  
  # Explicitly printing feature importance plot
  print(feature_plot)
  
  # Mean accuracy but it looks a little more fancy
  cat(sprintf("Mean Model Accuracy: %.2f%%\n", mean_accuracy * 100))
  
  # QWK but it looks a little more fancy
  cat(sprintf("Mean Quadratic Weighted Kappa: %.4f\n", mean_qwk))
  
  # Creates a list of results if needed outside the function
  invisible(list(
    mean_accuracy = mean_accuracy,
    average_feature_importance = average_feature_importance,
    aggregate_confusion_matrix = aggregate_confusion_matrix,
    all_accuracies = all_accuracies,
    all_confusion_matrices = all_confusion_matrices
  ))
}
