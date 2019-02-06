#' This script is for analysis of the results from Optimal node ML model experiment.
#' The Experiemnts/optimal directory contains the graphs with network traits, one file per graph.
#' The results are stored in results.json, which contain the set of influential nodes and their resilience
#' 
#' This file reads all the data, learns a ML model and tests the efficiency on test data

# Load required libraries
library(jsonlite)

library(uuid)
library(igraph)
library(arules)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(C50)
library(party)
library(doParallel)

# Load required source files
source('util/graph_util.R')
source('util/classification_util.R')
source('util/influence_maximization.R')

# Read the results
results <- fromJSON(paste(readLines("Experiments/results/optimal_results.json")))

train <- NULL
# Read data for model training
for (i in 1:nrow(results)) {
  # Graph ID
  graph_id <- paste('Experiments/data/optimal/graph_', results[i, "size"], "_", results[i, "uuid"], sep='')
  # Read the respective graph data
  graph <- read.csv(paste(graph_id, ".csv", sep=''))
  # Add a graph ID column
  graph$graph_id <- graph_id
  # Add a seed column
  graph$seed <- results[i, "seed"]
  # Append a column to label influential nodes
  graph$influential <- 0
  # Extract IDs of influential nodes from results
  influential <- unlist(results[i, "nodes"])
  # Label these as influential
  graph[influential, "influential"] <- 1
  train <- rbind(train, graph)
}

# Normalize data
train <- normalize_data(train, columns=c("degree", "closeness", "betweenness", "eigenvalue", "eccentricity", "graph_avg_degree"))

# Discretize traits
train$degree <- discretize_naturally(train$degree)
train$closeness <- discretize_naturally(train$closeness)
train$betweenness <- discretize_naturally(train$betweenness)
train$eigenvalue <- discretize_naturally(train$eigenvalue)
train$eccentricity <- discretize_naturally(train$eccentricity)

## Learn prediction model
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank # + graph_clust_coef + graph_density + graph_assortativity + graph_apl

start <- Sys.time()
method <- "lm" # lm, rpart, svm, rforest, nnet, cboost
if (method == "lm") {
  model <- glm(formula, family=binomial(link='logit'), data=train)
} else if (method == "rpart") {
  model <- rpart(formula, data=train)
} else if (method == "svm") {
  model <- svm(formula, data=train)
} else if (method == "rforest") {
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  model <- foreach(ntree=rep(100, cores), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    randomForest(formula, data=train, ntree=ntree, mtry=3, na.action=na.exclude)
  }
  stopCluster(cl)
} else if (method == "nnet") {
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  model <- avNNet(formula, train, allowParallel=TRUE, size=100, MaxNWts=10000)
  stopCluster(cl)
} else if (method == "cboost") {
  train$influential <- as.factor(train$influential)
  model <- C5.0(formula, train, trials=100, rules=TRUE, control=C5.0Control(earlyStopping=FALSE))
}
print(Sys.time() - start)
summary(model)

# Read test data set
#graph <- largest_component(read.graph("Experiments/data/my_twitter_network.txt", directed=FALSE))
#graph <- largest_component(read.graph("Experiments/data/author_netscience.txt", directed=FALSE))
graph <- largest_component(read.graph("Experiments/data/as-caida.txt", directed=FALSE))

test <- get_graph_traits(graph)
test$graph_id <- UUIDgenerate()
test <- normalize_data(test, columns=c("degree", "closeness", "betweenness", "eigenvalue", "eccentricity", "graph_avg_degree"))

# Discretize traits
test$degree <- discretize_naturally(test$degree)
test$closeness <- discretize_naturally(test$closeness)
test$betweenness <- discretize_naturally(test$betweenness)
test$eigenvalue <- discretize_naturally(test$eigenvalue)
test$eccentricity <- discretize_naturally(test$eccentricity)

# Make predictions using model
test$prediction_prob <- predict(model, newdata=test, type="response")

# Influential nodes by all traits
results <- NULL
size <- nrow(test) * 0.05
inf <- arrange(test, desc(degree))[1:size, "node"]
results$degree <- resilience(graph, V(graph)[inf])
inf <- arrange(test, desc(betweenness))[1:size, "node"]
results$betweenness <- resilience(graph, V(graph)[inf])
inf <- arrange(test, desc(closeness))[1:size, "node"]
results$closeness <- resilience(graph, V(graph)[inf])
inf <- arrange(test, desc(eigenvalue))[1:size, "node"]
results$eigenvalue <- resilience(graph, V(graph)[inf])
inf <- arrange(test, desc(pagerank))[1:size, "node"]
results$pagerank <- resilience(graph, V(graph)[inf])
# Resilience by model. Pick top n by probability
inf <- arrange(test, desc(prediction_prob))[1:size, "node"]
results$model <- resilience(graph, V(graph)[inf])
unlist(results)

#### CONCLUSION:
# 1. In Author-Netscience network, the model outperforms all other heuristics
