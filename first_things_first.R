list.of.packages <- c("jsonlite", "uuid", "sampling", "digest", "RWeka", "doMC", "snow", "doSNOW", "iterpc", "foreach", "igraph", "caret", "e1071", "party", "rpart", "rpart.plot", "randomForest", "RColorBrewer", "nnet", "rattle", "ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
library(c(list.of.packages))

