library(caret)
library(xgboost)
library(mlr)
library(parallel)
library(parallelMap)

# Get Accuracy/Precision/Recall/F1-Score table
get_prediction_results <- function(x, y, positive) {
  x <- as.factor(x)
  y <- as.factor(y)
  levels(y) <- levels(x)
  matrix <- confusionMatrix(x, y, positive)
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

#' @title Learn XGradientBoosting classifier
#' @name learn_xgboost_classifier
#' @param formula: the formula for learning
#' @param training: training dataset
#' @param label: target variable name
#' @param nrounds (100 - 10000): maximum number of iterations. For classification, it is similar to the number of trees to grow.
#' @param nthread: number of CPU threads to use
#' @param learn_hyperparameters: when true, enables learning the optimal parameter values. Caution! This option significantly increases execution time
#' @return XGBoost model
learn_xgboost_classifier <- function(formula, training, learning_features, label, nrounds=100, nthread=4, learn_hyperparameters=FALSE,
                                     default_params=list(booster="gbtree", objective="binary:logistic", eta=0.3, gamma=0, max_depth=5, min_child_weight=1, subsample=1, colsample_bytree=1)) {
  model <- NULL
  if (learn_hyperparameters) {
    dtraining <- xgb.DMatrix(data=as.matrix(training[, learning_features]), label=training[, label])
    # Tune number of iterations
    xgbcv <- xgb.cv(data=dtraining, params=default_params, nrounds=nrounds, nfold=5, showsd=TRUE, stratified=TRUE, print_every_n=10, early_stopping_rounds=20, maximize=FALSE)
    nrounds <- xgbcv$best_iteration
    # Create parameter learner
    learner <- makeLearner("classif.xgboost", predict.type="response")
    learner$par.vals <- list(objective="binary:logistic", eval_metric="error", nrounds=nrounds, eta=0.1)
    # Set parameter space
    tune_params <- makeParamSet(
      makeDiscreteParam("booster",values=c("gbtree","gblinear")),
      makeIntegerParam("max_depth", lower=1, upper=10),
      makeNumericParam("min_child_weight", lower=1, upper=5),
      makeNumericParam("subsample", lower=0.5, upper=0.8),
      makeNumericParam("colsample_bytree", lower=0.5, upper=0.8))
    # Resampling strategy
    resampling <- makeResampleDesc("CV", stratify=TRUE, iters=5)
    # Search parameter
    control <- makeTuneControlRandom(maxit=10)
    # Enable parallelizing
    parallelStartSocket(cpus=nthread)
    # Tune
    tune_training <- training[, c(learning_features, label)]
    tune_training[, label] <- as.factor(tune_training[, label])
    trainingtask <- makeClassifTask (data=tune_training, target=label)
    tuner <- tuneParams(learner=learner, task=trainingtask, resampling=resampling, measures=acc, par.set=tune_params, control=control, show.info=TRUE)
    print(tuner$y)
    default_params = list(booster=tuner$x$booster, objective="binary:logistic", eta=0.3, gamma=1, max_depth=tuner$x$max_depth, min_child_weight=tuner$x$min_child_weight, subsample=tuner$x$subsample, colsample_bytree=tuner$x$colsample_bytree)
  }
  model <- xgboost(formula=formula, data=as.matrix(training[, learning_features]), label=training[, label], nthread=cores, nrounds=nrounds, params=default_params)
  model
}

