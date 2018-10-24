list.of.packages <- c("jsonlite", "uuid", "sampling", "digest", "RWeka", "doMC", "snow", "doSNOW", "iterpc", "foreach", "igraph", "caret", "e1071", "party", "rpart", "rpart.plot", "randomForest", "RColorBrewer", "nnet", "rattle", "ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
library(c(list.of.packages))

## Get Precision/Recall/Accuracy table
getresults <- function(x, y, positive) {
  matrix <- confusionMatrix(x, y, positive)
  accuracy <- matrix$overall[1] # Correctness of model
  precision <- matrix$byClass[3] # Positive prediction value
  neg_precision <- matrix$byClass[4] # Negative prediction value
  sensitivity <- matrix$byClass[1] # True positive recognition rate (aka recall)
  specificity <- matrix$byClass[2] # True negative recognition rate
  type1_error <- 0 # FP
  type2_error <- 0 # FN
  results <- c(accuracy, precision, sensitivity, specificity)
  results
}
