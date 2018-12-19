## Get Accuracy/Precision/Recall/F1-Score table
get_prediction_results <- function(x, y, positive) {
  matrix <- confusionMatrix(as.factor(x), as.factor(y), positive)
  accuracy <- matrix$overall[1] # Correctness of model
  precision <- matrix$byClass[3] # Positive prediction value
  neg_precision <- matrix$byClass[4] # Negative prediction value
  sensitivity <- matrix$byClass[1] # True positive recognition rate (aka recall)
  specificity <- matrix$byClass[2] # True negative recognition rate
  f1_score <- 2 * ((precision * sensitivity) / (precision + sensitivity))
  names(f1_score) <- 'F1-Score'
  results <- c(accuracy, precision, sensitivity, specificity, f1_score)
  results
}

