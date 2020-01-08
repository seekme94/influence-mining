#' This script is for analysis of the results from Optimal node ML model experiment.
#' The Experiemnts/optimal directory contains the graphs with network traits, one file per graph.
#' The results are stored in results.json, which contain the set of influential nodes and their resilience
#' 
#' This file reads all the data, learns a ML model and tests the efficiency on test data

# Load required libraries
library(jsonlite)
library(igraph)
library(dplyr)
library(caret)
library(rpart)
library(e1071)
library(randomForest)
library(C50)
library(party)
library(doParallel)

# Load required source files
source('util/graph_util.R')
source('util/classification_util.R')
source('util/influence_maximization.R')

root_dir <- "Experiments/influential_node_prediction/"

# Read the results
results <- fromJSON(paste(readLines(paste(root_dir, "optimal_nodes.json", sep=''))))

data <- NULL
cores <- 8

# For all rows in results
for (i in 1:nrow(results)) {
  # Graph ID
  graph_id <- paste(root_dir, "optimal/graph_", results[i, "size"], "_", results[i, "uuid"], sep='')
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
  data <- rbind(data, graph)
}

# View a brief summary of data
summary(data)

# Normalize data
data$degree <- normalize_trait(data$degree)
data$closeness <- normalize_trait(data$closeness)
data$betweenness <- normalize_trait(data$betweenness)
data$eigenvalue <- normalize_trait(data$eigenvalue)
data$eccentricity <- normalize_trait(data$eccentricity)
data$graph_avg_degree <- normalize_trait(data$graph_avg_degree)

# Split into 50/50 training and test sets
train <- data[data$seed <= 500,]
test <- data[data$seed > 500,]

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
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank + graph_size + graph_edges + graph_clust_coef + graph_density + graph_assortativity + graph_girth

## Learn prediction model
# lm = Linear (logistic regression) model
# rpart = Recursive partitioning
# svm = Support vector machines
# rforest = Random forests
# nnet = Neural network
# ccboost = C50 boosting
start <- Sys.time()
method <- c("lm") #c("lm", "rpart", "svm", "rforest", "nnet", "cboost")
# Limit training data
#train <- train[train$seed == 500,]
if ("lm" %in% method) {
  model <- glm(formula, family=binomial(link='logit'), data=train)
  test$lm_prediction <- as.factor(round(predict(model, test[,-(21)], type="response")))
}
if ("rpart" %in% method) {
  model <- rpart(formula, data=train)
  test$rpart_prediction <- as.factor(round(predict(model, test[,-(21)])))
}
if ("svm" %in% method) {
  model <- svm(formula, data=train)
  test$svm_prediction <- as.factor(round(predict(model, newdata=test[,-(21)], probability = TRUE)))
}
if ("rforest" %in% method) {
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  model <- foreach(ntree=rep(100, cores), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    randomForest(formula, data=train, ntree=ntree, mtry=3, na.action=na.exclude)
  }
  stopCluster(cl)
#  model <- randomForest(formula, data=train, ntree=500, mtry=3, na.action=na.exclude)
  test$rforest_prediction <- as.factor(round(predict(model, test[,-(21)])))
}
if ("nnet" %in% method) {
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  model <- avNNet(formula, train, allowParallel=TRUE, size=100, MaxNWts=10000)
  stopCluster(cl)
  test$nnet_prediction <- as.factor(round(predict(model, test[,-(21)])))
}
if ("cboost" %in% method) {
  train$influential <- as.factor(train$influential)
  model <- C5.0(formula, train, trials=100, rules=TRUE, control=C5.0Control(earlyStopping=FALSE))
  test$cboost_prediction <- as.factor(round(predict(model, test[,-(21)])))
}
print(Sys.time() - start)
summary(model)
get_prediction_results(test$influential, test$lm_prediction, '1')


graph_size <- unique(test$graph_size[test$graph_size >= 30])
# Empty data set to contain results
resultset <- data.frame(size=c(), method=c(), accuracy=c(), pos_pred_value=c(), sensitivity=c(), specificity=c(), f1_score=c())
# Machine learning model
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$prediction[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='ML', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
# High degree
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$inf_by_degree[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='Degree', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
# High betweenness
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$inf_by_betweenness[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='Betweenness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
# High closeness
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$inf_by_closeness[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='Closeness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
# High Eigenvalue
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$inf_by_eigenvalue[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='Eigenvector', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
# High Pagerank
for (i in graph_size) {
  actual <- as.factor(test$influential[test$graph_size == i])
  results <- get_prediction_results(actual, as.factor(test$inf_by_pagerank[test$graph_size == i]), '1')
  row <- data.frame(size=i, method='Pagerank', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
  resultset <- rbind(resultset, row)
}
head(resultset)

##################################################################################
# Analysis of results
##################################################################################
# Check the number of instances where ML performed better than the rest in terms of accuracy
dt <- select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30)
dt

g <- generate_holme_kim(1000, 2, 0.05)
graph <- get_graph_traits(g, TRUE)
influential_size <- nrow(graph) * 0.1
# Apply the model on this graph to classify influential nodes
graph$prediction_prob <- predict(model, graph, type="response")
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

# Arrange by accuracy

arrange(resultset, desc(accuracy))[1:size, "node"]


#### CONCLUSION:
#' 1. The accuracy of the model surpasses the heuristics in all instances
#' 2. ML model is also ahead in terms of predicting most resilient nodes as long as the structure of generated graph is similar to training graphs
#' 3. The method drops its performance on graphs having different network properties than those in the training set
#' 4. Closeness and Eigenvector traits are always outperformed by other traits
