library(jsonlite)
library(igraph)
library(caret)
library(e1071)
library(randomForest)
library(rpart)
library(dplyr)

source('graph_util.R')

setwd('Experiments/optimal/')

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
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank + graph_size + graph_edges + graph_avg_degree + graph_max_degree + graph_apl + graph_clust_coef + graph_diameter + graph_density + graph_assortativity + avg_distance + graph_triads + graph_girth

head(train)
head(test[,-(21)])

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

# Record how prediction model performs
#summary(model)
test$prediction_prob <- predict(model, testset)
test$prediction <- as.numeric(test$prediction_prob >= 0.5)

test_graph_sizes <- c(30, 35, 40)
for (size in test_graph_sizes) {
  eval_set <- test[test$graph_size == size,]
  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$prediction), '1')
  print(paste('Accuracy by machine learning on graph size', size))
  print(results)

  results <- getresults(as.factor(eval_set$influential), as.factor(eval_set$inf_by_degree), '1')
  print(paste('Accuracy by high-degree on graph size', size))
  print(results)
}


