#' This script is for analysis of the results from Optimal node ML model experiment.
#' The Experiemnts/optimal directory contains the training and test graphs with network traits, one file per graph.
#' The results are stored in results.json and test_results.json files, which contain the set of optimally influential nodes and their resilience
#' 
#' This file reads all the training and test graphs, learns a ML model and tests the efficiency on test data

# Execute these lines to install all the required packages
list.of.packages <- c("jsonlite", "uuid", "sampling", "digest", "RWeka", "doMC", "snow", "doSNOW", "iterpc", "foreach", "igraph", "caret", "e1071", "party", "rpart", "rpart.plot", "randomForest", "RColorBrewer", "nnet", "rattle", "ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
#  install.packages(new.packages)
}

# Load required libraries
library(jsonlite)
library(igraph)
library(caret)
library(e1071)
library(randomForest)
library(rpart)
library(dplyr)

setwd('Experiments/optimal/')

# Load required source files
source('../../graph_util.R')
source('../../influence_maximization.R')

train <- NULL
test <- NULL

# Read the results
results <- fromJSON(paste(readLines("results.json")))
test_results <- fromJSON(paste(readLines("test_results.json")))
head(results)
head(test_results)

# For all rows in results
for (i in 1:nrow(results)) {
  # Read the respective graph data
  file_name <- paste(results[i, "uuid"], ".csv", sep='')
  graph <- read.csv(file_name)
  # Add a graph ID column
  graph$graph_id <- results[i, "uuid"]
  # Append a column to label influential nodes
  graph$influential <- 0
  # Extract IDs of influential nodes from results
  influential <- unlist(results[i, "nodes"])
  # Label these as influential
  graph[influential, "influential"] <- 1
  train <- rbind(train, graph)
}

# For all rows in test data
for (i in 1:nrow(test_results)) {
  # Read the respective graph data
  file_name <- paste(test_results[i, "uuid"], ".out", sep='')
  graph <- read.csv(file_name)
  # Add a graph ID column
  graph$graph_id <- test_results[i, "uuid"]
  # Append a column to label influential nodes
  graph$influential <- 0
  # Extract IDs of influential nodes from results
  influential <- unlist(test_results[i, "nodes"])
  # Label these as influential
  graph[influential, "influential"] <- 1
  test <- rbind(test, graph)
}

# Normalize data
train$degree <- normalize_trait(train$degree)
train$closeness <- normalize_trait(train$closeness)
train$betweenness <- normalize_trait(train$betweenness)
train$eigenvalue <- normalize_trait(train$eigenvalue)
train$eccentricity <- normalize_trait(train$eccentricity)

test$degree <- normalize_trait(test$degree)
test$closeness <- normalize_trait(test$closeness)
test$betweenness <- normalize_trait(test$betweenness)
test$eigenvalue <- normalize_trait(test$eigenvalue)
test$eccentricity <- normalize_trait(test$eccentricity)

# Influence by network traits
newtest <- NULL
for (graph_id in unique(test$graph_id)) {
  graph <- test[test$graph_id == graph_id,]
  influential_size <- nrow(graph) * 0.1
  # Using dplyr
  { # Label top n high-degree nodes as influential
    inf_by_degree <- head(select(arrange(graph, desc(degree)), node), influential_size)
    graph$inf_by_degree <- 0
    graph$inf_by_degree[graph$node %in% inf_by_degree[[1]]] <- 1
  }
  { # Label top n high-closeness nodes as influential
    inf_by_closeness <- head(select(arrange(graph, desc(closeness)), node), influential_size)
    graph$inf_by_closeness <- 0
    graph$inf_by_closeness[graph$node %in% inf_by_closeness[[1]]] <- 1
  }
  { # Label top n high-betweenness nodes as influential
    inf_by_betweenness <- head(select(arrange(graph, desc(betweenness)), node), influential_size)
    graph$inf_by_betweenness <- 0
    graph$inf_by_betweenness[graph$node %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-eigenvalue nodes as influential
    inf_by_eigenvalue <- head(select(arrange(graph, desc(eigenvalue)), node), influential_size)
    graph$inf_by_eigenvalue <- 0
    graph$inf_by_eigenvalue[graph$node %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-eigenvalue nodes as influential
    inf_by_eigenvalue <- head(select(arrange(graph, desc(eigenvalue)), node), influential_size)
    graph$inf_by_eigenvalue <- 0
    graph$inf_by_eigenvalue[graph$node %in% inf_by_eigenvalue[[1]]] <- 1
  }
  { # Label top n high-pagerank nodes as influential
    inf_by_pagerank <- head(select(arrange(graph, desc(pagerank)), node), influential_size)
    graph$inf_by_pagerank <- 0
    graph$inf_by_pagerank[graph$node %in% inf_by_pagerank[[1]]] <- 1
  }
  newtest <- rbind(newtest, graph)
}

test <- newtest

# Prediction phase
# Formula considering both node and graph traits
#formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank + graph_size + graph_edges + graph_avg_degree + graph_max_degree + graph_apl + graph_clust_coef + graph_diameter + graph_density + graph_assortativity + avg_distance + graph_triads + graph_girth
# Formula considering only node traits
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank

head(train)
head(test[,-(21)])

# Learn Recursive Patitioning model
method <- "rpart" # rpart, lm, rforest
testset <- test[,-(21)] # Hide the actual results
test$prediction <- NULL
if (method == "rpart") {
  # Learn the recursive partitioning model
  model <- rpart(formula, data=train)
} else if (method == "lm") {
  # Learn the linear regression model
  model <- glm(formula, family=binomial(link='logit'), data=train)
} else if (method == "rforest") {
  # Learn the random forest model
  model <- randomForest(formula, data=train, importance=TRUE, ntree=50)
}

summary(model)
# Store probabilities and decisions separately
test$prediction_prob <- predict(model, testset)
test$prediction <- as.numeric(test$prediction_prob >= 0.5)

test_graph_sizes <- c(30, 35, 40, 45)
for (size in test_graph_sizes) {
  eval_set <- test[test$graph_size == size,]
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$prediction), '1')
  print(paste('Accuracy by machine learning on graph size', size))
  print(results)

  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_degree), '1')
  print(paste('Accuracy by high-degree on graph size', size))
  print(results)
  
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_betweenness), '1')
  print(paste('Accuracy by high-betweenness on graph size', size))
  print(results)
  
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_closeness), '1')
  print(paste('Accuracy by high-closeness on graph size', size))
  print(results)
  
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_eigenvalue), '1')
  print(paste('Accuracy by high-eigen vector on graph size', size))
  print(results)
  
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_pagerank), '1')
  print(paste('Accuracy by high-pagerank on graph size', size))
  print(results)
}

# Create larger networks and compare the resilience of model with other heuristics
g <- generate_small_world(5000, 0.001)
graph <- get_graph_traits(g, TRUE)
head(graph)
influential_size <- nrow(graph) * 0.1

# Apply the model on this graph to classify influential nodes
graph$prediction_prob <- predict(model, graph)
graph$prediction <- as.numeric(graph$prediction_prob >= 0.5)

# Influential nodes by all traits
size <- influential_size
inf <- arrange(graph, desc(degree))[1:size, "node"]
resilience(g, inf)
inf <- arrange(graph, desc(betweenness))[1:size, "node"]
resilience(g, V(g)[inf])
inf <- arrange(graph, desc(closeness))[1:size, "node"]
resilience(g, V(g)[inf])
inf <- arrange(graph, desc(eigenvalue))[1:size, "node"]
resilience(g, V(g)[inf])
inf <- arrange(graph, desc(pagerank))[1:size, "node"]
resilience(g, V(g)[inf])
# Resilience by model. Pick top n by probability
inf <- arrange(graph, desc(prediction_prob))[1:size, "node"]
resilience(g, V(g)[inf])



#### CONCLUSION:
#' 1. The accuracy of the model surpasses the heuristics in all instances
#' 2. The model is also ahead in terms of predicting most resilient nodes as long as the structure of generated graph is similar to training graphs
#' 3. The method drops its performance on graphs having different network properties than those in the training set
