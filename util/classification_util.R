# Get Accuracy/Precision/Recall/F1-Score table
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

#' Discretizes a numeric vector into 3, 5 or 7 and returns a factor vector with values as:
#' LOW, MEDIUM, HIGH; VERY_LOW, LOW, MEDIUM, HIGH, VERY_HIGH; VERY_LOW, LOW, MEDIUM_LOW, MEDIUM, MEDIUM_HIGH, HIGH, VERY_HIGH
discretize_naturally <- function(x, breaks=7) {
  if (mode(x) != 'numeric') {
    x
  } else {
    if (breaks == 3) {
      discretize(x, method="interval", breaks, labels=c('L','M','H'))
    } else if (breaks == 5) {
      discretize(x, method="interval", breaks, labels=c('VL','L','M','H','VH'))
    } else {
      discretize(x, method="interval", breaks, labels=c('VL','L','ML','M','MH','H','VH'))
    }
  }
}
