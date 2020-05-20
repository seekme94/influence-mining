#' This script is for analysis of the results from Optimal node ML model experiment.
#' The Experiemnts/optimal directory contains the graphs with network traits, one file per graph.
#' The results are stored in results.json, which contain the set of influential nodes and their resilience
#'
#' This file reads all the data, learns a ML model and tests the efficiency on test data

# Load required libraries
library(igraph)
library(dplyr)
library(caret)
library(e1071)
library(uuid)

# Load required source files
source('util/graph_util.R')
source('util/heuristics.R')
source('util/classification_util.R')
source('util/influence_maximization.R')

root_dir <- "Experiments/influential_node_prediction/"
cores <- 7

#1. Select a large network
graph <- largest_component(read.graph("dataset/jdk6_dependencies.txt", directed=FALSE))
graph$name <- UUIDgenerate()

formula <- influential ~ degree + coreness + pagerank + ci + graph_apl + graph_clust_coef
node_traits <- c("degree", "coreness", "pagerank", "ci")
graph_traits <- c("graph_apl", "graph_clust_coef")
subgraph_size_limit <- 1000 # Sub graphs greater than this size will be excluded
data <- data.frame(name=c(), degree=c(), coreness=c(), pagerank=c(), ci=c(), graph_apl=c(), graph_clust_coef=c(), graph_id=c())

#2. Extract 'n' sub-graphs using various methods

#2a. For each node, extract sub-graph of their immediate neighbours
sizes <- c()
for (v in V(graph)) {
  egonet <- induced_subgraph(graph, neighbors(graph, v))
  egonet$graph_id <- UUIDgenerate()
  # Exclude outliers by restricting to 3rd degree of freedom
  if (vcount(egonet) < subgraph_size_limit) {
    print(paste("Computing for node ", v))
    # Calculate graph traits
    traits <- get_graph_traits(graph=egonet, node_traits=node_traits, graph_traits=graph_traits, verbose=FALSE)
    # Convert to data frame
    traits <- as.data.frame(traits)
    influential <- get_influential_nodes_greedy(egonet, budget=0.1, parallel=TRUE)
    traits$graph_id <- egonet$graph_id
    data <- rbind(data, traits)
  }

  # # Label most influential nodes via Greedy method
  # # Make predictions using model
  # traits$prediction_prob <- predict(model, newdata=as.matrix(traits[, model$feature_names]))
  # test$graph_id <- UUIDgenerate()
}


#2b. Pick n high-degree nodes and extract sub-graph of their immediate neighbours
#2c. Pick n high-coreness nodes and extract sub-graphcs of their immediate neighbors
#2d. Pick n communities using a balanced community detection algorithm
#2e. Pick friends of friends network of n high-degree nodes

data <- normalize_data(data, columns=c("degree", "betweenness", "closeness", "eigenvalue", "eccentricity", "coreness", "pagerank", "ci"))

# Split into 50/50 training and test sets
train <- data[data$seed <= 5, columns]
test <- data[data$seed > 5, columns]

#3. For all the networks, calculate features to create a learning dataset
#4. For all networks, identify most influential nodes via Greedy method and label them
#5. Learn a DNN to classify influential nodes
#6. Classify k influential nodes from the model
#7. Compare the results with Degree, Coreness, Pagerank and CI



## Learn prediction model
start <- Sys.time()

method <- c("nnet", "xgboost")
# Limit training data
#train <- train[train$seed == 500,]
if ("xgboost" %in% method) {
  # Ensure that the order of columns is same in both sets
  train$influential <- train$influential
  # Exclude non-numeric and label data
  columns <- !(names(train) %in% c("graph_id", "seed", "influential"))
  xgboost_model <- xgboost(formula=formula, data=as.matrix(train[, columns]), label=train$influential, max.depth=3, nthread=6, nrounds=500, objective= "binary:logistic")
  # Keep only the feature names in the model, or else...
  test$xgboost_prediction <- round(predict(xgboost_model, as.matrix(test[, xgboost_model$feature_names])))
  performance = get_prediction_results(test$influential, test$xgboost_prediction, '1')
  write.csv(performance, file=paste(root_dir, "xgboost_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "xgboost_prediction")
  write.csv(resultset, file=paste(root_dir, "xgboost_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}

print(Sys.time() - start)



##################################################################################
# Analysis of results
##################################################################################

## IMPORTANT: FIRST DEFINE WHICH MODEL IS TO BE ANALYZED
model <- xgboost_model

# Test on newly generated graphs
sizes <- c(50, 100, 150, 200, 250, 300, 400, 500)
resilience_results <- data.frame(type=c(), size=c(), by_model=c(), by_degree=c(), by_betweenness=c(), by_closeness=c(), by_eigenvalue=c(), by_pagerank=c(), by_eccentricity=c(), by_coreness=c(), by_ci=c())
for (type in c('SF', 'SW', 'HK')) {
  for (size in sizes) {
    if (type == 'SF') {
      graph <- generate_scale_free(size)
    } else if (type == 'SW') {
      graph <- generate_small_world(size, probability=1/sqrt(size))
    } else if (type == 'HK') {
      graph <- generate_holme_kim(size, 2, 1/sqrt(size))
    }

    traits_data <- get_graph_traits(graph=graph, node_traits=node_traits, graph_traits=graph_traits, verbose=TRUE)
    graph_data <- as.data.frame(traits_data)
    influential_size <- ceiling(sqrt(size))
    # Apply the model on this graph to classify influential nodes
    pred <- attr(predict(svm_model, newdata=test[1:10,], probability=TRUE), "probabilities")[,1]
    graph_data$prediction_prob <- pred
    prob_cut <- min(head(sort(graph_data$prediction_prob, decreasing=TRUE), n=influential_size * 0.1))
    graph_data$prediction <- as.numeric(graph_data$prediction_prob >= prob_cut)

    # Influential nodes by all traits
    inf <- arrange(graph_data, desc(degree))[1:influential_size, "name"]
    by_degree <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(betweenness))[1:influential_size, "name"]
    by_betweenness <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(closeness))[1:influential_size, "name"]
    by_closeness <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(eigenvalue))[1:influential_size, "name"]
    by_eigenvalue <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(eccentricity))[1:influential_size, "name"]
    by_eccentricity <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(coreness))[1:influential_size, "name"]
    by_coreness <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(pagerank))[1:influential_size, "name"]
    by_pagerank <- resilience(graph, V(graph)[inf])
    inf <- arrange(graph_data, desc(ci))[1:influential_size, "name"]
    by_ci <- resilience(graph, V(graph)[inf])

    # Resilience by model. Pick top n by probability
    inf <- arrange(graph_data, desc(prediction_prob))[1:influential_size, "name"]
    by_model <- resilience(graph, V(graph)[inf])
    results <- data.frame(type=type, size=size, by_model=by_model,
                          by_degree=by_degree, by_betweenness=by_betweenness, by_closeness=by_closeness, by_eigenvalue=by_eigenvalue, by_pagerank=by_pagerank,
                          by_eccentricity=by_eccentricity, by_coreness=by_coreness, by_ci=by_ci)
    print(results)
    resilience_results <- rbind(resilience_results, results)
  }
}
write.csv(resilience_results, file=paste(root_dir, "resilience_results.csv", sep=''), row.names=FALSE, quote=TRUE)


###########################
# Test on real-world graphs
###########################
start <- Sys.time()

test <- as.data.frame(test)
test$a_ci <- 0 # This is because we excluded adaptive CI due to expense
# Make predictions using model
test$prediction_prob <- predict(model, newdata=as.matrix(test[, model$feature_names]))
test$graph_id <- UUIDgenerate()
# Influential nodes by all traits
results <- NULL
size <- nrow(test) * 0.05
# Random nodes
results$random <- resilience(graph, sample(V(graph), size))
# By degree
inf <- arrange(test, desc(degree))[1:size, "name"]
results$degree <- resilience(graph, V(graph)[inf])
# By betweenness
inf <- arrange(test, desc(betweenness))[1:size, "name"]
results$betweenness <- resilience(graph, V(graph)[inf])
# By closeness
inf <- arrange(test, desc(closeness))[1:size, "name"]
results$closeness <- resilience(graph, V(graph)[inf])
# By Eigenvector centrality
inf <- arrange(test, desc(eigenvalue))[1:size, "name"]
results$eigenvalue <- resilience(graph, V(graph)[inf])
# By pagerank
inf <- arrange(test, desc(pagerank))[1:size, "name"]
results$pagerank <- resilience(graph, V(graph)[inf])
# By eccentricity
inf <- arrange(test, desc(eccentricity))[1:size, "name"]
results$eccentricity <- resilience(graph, V(graph)[inf])
# By coreness
inf <- arrange(test, desc(coreness))[1:size, "name"]
results$coreness <- resilience(graph, V(graph)[inf])
# By collective influence
inf <- arrange(test, desc(ci))[1:size, "name"]
results$ci <- resilience(graph, V(graph)[inf])

# Resilience by model. Pick top n by probability
inf <- arrange(test, desc(prediction_prob))[1:size, "name"]
results$model <- resilience(graph, V(graph)[inf])
print(unlist(results))


#### CONCLUSION:
