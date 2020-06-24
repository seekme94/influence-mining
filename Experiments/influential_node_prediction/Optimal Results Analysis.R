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
library(xgboost)
library(uuid)

# Load required source files
source('util/graph_util.R')
source('util/heuristics.R')
source('util/classification_util.R')
source('util/influence_maximization.R')

root_dir <- "Experiments/influential_node_prediction/"

# Read the results
results <- fromJSON(paste(readLines(paste(root_dir, "optimal_results.json", sep=''))))

data <- NULL
cores <- 6

# For all rows in results
for (i in 1:nrow(results)) {
  # Graph ID
  graph_id <- paste(root_dir, "data/graph_", results[i, "size"], "_", results[i, "uuid"], sep='')
  filename <- paste(graph_id, ".csv", sep='')
  if (file.exists(filename) == FALSE) {
    print(paste(filename, "does not exist or is unreadable!"))
    next
  }
  # Read the respective graph data
  graph <- read.csv(filename)
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
length(unique(data$graph_id))
summary(data)

# Normalize data
data <- normalize_data(data, columns=c("degree", "betweenness", "closeness", "eigenvalue", "eccentricity", "coreness", "pagerank", "ci", "a_degree", "a_betweenness", "a_closeness", "a_eigenvalue", "a_coreness", "a_pagerank", "a_ci"))

# Prediction formula considering both node and graph traits
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + coreness + pagerank + ci + graph_clust_coef + graph_density + graph_assortativity
node_traits <- c("degree", "betweenness", "closeness", "eigenvalue", "eccentricity", "coreness", "pagerank", "ci")
graph_traits <- c("graph_size", "graph_edges", "graph_avg_degree", "graph_max_degree", "graph_apl", "graph_clust_coef", "graph_diameter", "graph_density", "graph_assortativity", "graph_avg_distance", "graph_triads", "graph_girth")
columns <- c("name", "graph_id", "seed", "influential", node_traits, graph_traits)

# Split into 50/50 training and test sets
train <- data[data$seed <= 5, columns]
test <- data[data$seed > 5, columns]

# Influence by network traits
newtest <- NULL
for (graph_id in unique(test$graph_id)) {
  graph <- test[test$graph_id == graph_id,]
  influential_size <- nrow(graph) * 0.1
  # Using dplyr
  { # Label top n high-degree nodes as influential
    inf_by_degree <- head(select(arrange(graph, desc(degree)), name), influential_size)
    graph$inf_by_degree <- 0
    graph$inf_by_degree[graph$name %in% inf_by_degree[[1]]] <- 1
  }
  { # Label top n high-betweenness nodes as influential
    inf_by_betweenness <- head(select(arrange(graph, desc(betweenness)), name), influential_size)
    graph$inf_by_betweenness <- 0
    graph$inf_by_betweenness[graph$name %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-closeness nodes as influential
    inf_by_closeness <- head(select(arrange(graph, desc(closeness)), name), influential_size)
    graph$inf_by_closeness <- 0
    graph$inf_by_closeness[graph$name %in% inf_by_closeness[[1]]] <- 1
  }
  { # Label top n high-eigenvalue nodes as influential
    inf_by_eigenvalue <- head(select(arrange(graph, desc(eigenvalue)), name), influential_size)
    graph$inf_by_eigenvalue <- 0
    graph$inf_by_eigenvalue[graph$name %in% inf_by_betweenness[[1]]] <- 1
  }
  { # Label top n high-eccentricity nodes as influential
    inf_by_eccentricity <- head(select(arrange(graph, desc(eccentricity)), name), influential_size)
    graph$inf_by_eccentricity <- 0
    graph$inf_by_eccentricity[graph$name %in% inf_by_eccentricity[[1]]] <- 1
  }
  { # Label top n high-coreness nodes as influential
    inf_by_coreness <- head(select(arrange(graph, desc(coreness)), name), influential_size)
    graph$inf_by_coreness <- 0
    graph$inf_by_coreness[graph$name %in% inf_by_coreness[[1]]] <- 1
  }
  { # Label top n high-pagerank nodes as influential
    inf_by_pagerank <- head(select(arrange(graph, desc(pagerank)), name), influential_size)
    graph$inf_by_pagerank <- 0
    graph$inf_by_pagerank[graph$name %in% inf_by_pagerank[[1]]] <- 1
  }
  { # Label top n high-pagerank nodes as influential
    inf_by_ci <- head(select(arrange(graph, desc(ci)), name), influential_size)
    graph$inf_by_ci <- 0
    graph$inf_by_ci[graph$name %in% inf_by_ci[[1]]] <- 1
  }
  #"a_degree", "a_betweenness", "a_closeness", "a_eigenvalue", "a_coreness", "a_pagerank", "a_ci"
  newtest <- rbind(newtest, graph)
}
test <- newtest


get_test_data_results <- function(test, prediction_column) {
  graph_sizes <- unique(test$graph_size[test$graph_size >= 30])
  # Empty data set to contain results
  resultset <- data.frame(size=c(), method=c(), accuracy=c(), pos_pred_value=c(), sensitivity=c(), specificity=c(), f1_score=c())
  for (i in graph_sizes) {
    # Machine learning model
    results <- get_prediction_results(test$influential[test$graph_size == i], test[test$graph_size == i, prediction_column], '1')
    row <- data.frame(size=i, method='ML', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High degree
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_degree[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Degree', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High betweenness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_betweenness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Betweenness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High closeness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_closeness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Closeness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High eigenvalue
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_eigenvalue[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Eigenvector', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High eccentricity
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_eccentricity[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Eccentricity', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High coreness
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_coreness[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Coreness', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High pagerank
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_pagerank[test$graph_size == i], '1')
    row <- data.frame(size=i, method='Pagerank', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
    # High collective influence
    results <- get_prediction_results(test$influential[test$graph_size == i], test$inf_by_ci[test$graph_size == i], '1')
    row <- data.frame(size=i, method='CI', accuracy=results[[1]], pos_pred_value=results[[2]], sensitivity=results[[3]], specificity=results[[4]], f1_score=results[[5]])
    resultset <- rbind(resultset, row)
  }
  resultset
}



## Learn prediction model
# lm = Linear (logistic regression) model
# rpart = Recursive partitioning
# svm = Support vector machines
# rforest = Random forests
# nnet = Neural network
# ccboost = C50 boosting
start <- Sys.time()
method <- c("xgboost")
# Limit training data
#train <- train[train$seed == 500,]
if ("lm" %in% method) {
  lm_model <- glm(formula, family=binomial(link='logit'), data=train)
  test$lm_prediction <- as.factor(round(predict(lm_model, test, type="response")))
  performance = get_prediction_results(test$influential, test$lm_prediction, '1')
  write.csv(performance, file=paste(root_dir, "lm_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "lm_prediction")
  write.csv(resultset, file=paste(root_dir, "lm_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}
if ("rpart" %in% method) {
  rpart_model <- rpart(formula, data=train)
  test$rpart_prediction <- as.factor(round(predict(rpart_model, test)))
  performance = get_prediction_results(test$influential, test$rpart_prediction, '1')
  write.csv(performance, file=paste(root_dir, "rpart_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "rpart_prediction")
  write.csv(resultset, file=paste(root_dir, "rpart_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}
if ("rforest" %in% method) {
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  rforest_model <- foreach(ntree=seq(10, cores), .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    randomForest(formula, data=train, ntree=ntree, mtry=3, na.action=na.exclude)
  }
  stopCluster(cl)
  test$rforest_prediction <- as.factor(round(predict(rforest_model, test, type = "response")))
  performance = get_prediction_results(test$influential, test$rforest_prediction, '1')
  write.csv(performance, file=paste(root_dir, "rforest_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "rforest_prediction")
  write.csv(resultset, file=paste(root_dir, "rforest_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}
if ("svm" %in% method) {
  svm_model <- svm(formula, data=train[1:100,], kernel="radial", probability=TRUE)
  pred <- predict(svm_model, newdata=test, probability=TRUE)
  names(pred) <- NULL
  test$svm_prediction <- as.factor(round(pred))
  performance = get_prediction_results(test$influential, test$svm_prediction, '1')
  write.csv(performance, file=paste(root_dir, "svm_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "svm_prediction")
  write.csv(resultset, file=paste(root_dir, "svm_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}
if ("cboost" %in% method) {
  train$influential <- as.factor(train$influential)
  cboost_model <- C5.0(formula, train, trials=100, rules=TRUE, control=C5.0Control(earlyStopping=FALSE))
  test$cboost_prediction <- as.factor(predict(cboost_model, test))
  performance = get_prediction_results(test$influential, test$cboost_prediction, '1')
  write.csv(performance, file=paste(root_dir, "cboost_accuracy.txt", sep=''))
  resultset <- get_test_data_results(test, "cboost_prediction")
  write.csv(resultset, file=paste(root_dir, "cboost_comparison.csv", sep=''), row.names=FALSE, quote=TRUE)
  # Check the number of instances where ML performed better than the rest in terms of accuracy
  print(select(resultset, size:accuracy) %>% arrange(size, desc(accuracy)) %>% filter(size >= 30))
}
if ("xgboost" %in% method) {
  # Ensure that the order of columns is same in both sets
  train$influential <- train$influential
  # Exclude non-numeric and label data
  columns <- !(names(train) %in% c("graph_id", "seed", "influential"))
  xgboost_model <- xgboost(formula=formula, data=as.matrix(train[, columns]), label=train$influential, max.depth=7, nthread=cores, nrounds=500, objective="binary:logistic")
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

################
# Test on graphs
################
# author <- largest_component(read.graph("dataset/author_netscience.txt", directed=FALSE))
# ita2000 <- largest_component(read.graph("dataset/ita2000.txt", directed=FALSE))
#caida <- largest_component(read.graph("dataset/as-caida.txt", directed=FALSE))
# jdk <- largest_component(read.graph("dataset/jdk6_dependencies.txt", directed=FALSE))
# contact <- largest_component(read.graph("dataset/human_contact.txt", directed=FALSE))
citynet2010 <- largest_component(read.graph("dataset/citycity_weighted_2010.csv", directed=FALSE, format="ncol"))
citynet2013 <- largest_component(read.graph("dataset/citycity_weighted_2013.csv", directed=FALSE, format="ncol"))
citynet2016 <- largest_component(read.graph("dataset/citycity_weighted_2016.csv", directed=FALSE, format="ncol"))
citynet2019 <- largest_component(read.graph("dataset/citycity_weighted_2019.csv", directed=FALSE, format="ncol"))

graphs <- list(citynet2010, citynet2013, citynet2016, citynet2019)

for(graph in graphs) {
  start <- Sys.time()
  # print(fit_power_law(graph))

  node_traits <- c("degree", "betweenness", "closeness", "eigenvalue", "eccentricity", "coreness", "pagerank", "ci")
  graph_traits <- c("graph_size", "graph_edges", "graph_avg_degree", "graph_max_degree", "graph_apl", "graph_clust_coef", "graph_diameter", "graph_density", "graph_assortativity", "graph_avg_distance", "graph_triads", "graph_girth")
  test <- get_graph_traits(graph=graph, node_traits=node_traits, graph_traits=graph_traits, verbose=FALSE)
  print(Sys.time() - start)

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
}

####################
# For large networks
####################
# graph <- wordnet
#
# start <- Sys.time()
# print(fit_power_law(graph))
# test <- NULL
# test$name <- 1:vcount(graph) - 1
# print("Degree...")
# test$degree <- degree(graph)
# print("Betweenness...")
# test$betweenness <- betweenness(graph)
# print("Closeness...")
# test$closeness <- closeness(graph)
# print("Eigenvector...")
# test$eigenvalue <- normalize_trait(evcent(graph)$vector)
# print("Eccentricity...")
# test$eccentricity <- normalize_trait(eccentricity(graph))
# print("Coreness...")
# test$coreness <- coreness(graph)
# print("Pagerank...")
# test$pagerank <- normalize_trait(page_rank(graph)$vector)
# print("CI...")
# test$ci <- sapply(V(graph), function(x) { collective_influence(graph, neighborhood_distance=2, x) })
# test$graph_size <- vcount(graph)
# test$graph_edges <- ecount(graph)
# test$graph_avg_degree <- mean(test$degree)
# test$graph_max_degree <- max(test$degree)
# print("APL...")
# test$graph_apl <- average.path.length(graph)
# print("CC...")
# test$graph_clust_coef <- transitivity(graph)
# print("Diameter...")
# test$graph_diameter <- diameter(graph)
# print("Density...")
# test$graph_density <- graph.density(graph)
# print("Assortativity...")
# test$graph_assortativity <- assortativity.degree(graph)
# print("Avg Distance...")
# test$graph_avg_distance <- mean_distance(graph)
# print("Triads...")
# test$graph_triads <- length(triangles(graph))
# print("Girth...")
# test$graph_girth <- girth(graph)$girth
# # Preserve current state
# write.csv2(test, paste(root_dir, "test_wordnet.dat", sep=''), sep=";", row.names=FALSE)
#
# print(Sys.time() - start)
#
# test <- as.data.frame(test)
# # test$a_degree <- test$a_betweenness <- test$a_closeness <- test$a_eigenvalue <- test$a_eccentricity <- test$a_coreness <- test$a_pagerank <- test$a_ci <- 0
# test$prediction_prob <- predict(model, newdata=as.matrix(test[, model$feature_names]))
# test$graph_id <- UUIDgenerate()
# results <- NULL
# size <- nrow(test) * 0.05
# results$random <- resilience(graph, sample(V(graph), size))
# inf <- arrange(test, desc(degree))[1:size, "name"]
# results$degree <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(betweenness))[1:size, "name"]
# results$betweenness <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(closeness))[1:size, "name"]
# results$closeness <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(eigenvalue))[1:size, "name"]
# results$eigenvalue <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(pagerank))[1:size, "name"]
# results$pagerank <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(eccentricity))[1:size, "name"]
# results$eccentricity <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(coreness))[1:size, "name"]
# results$coreness <- resilience(graph, V(graph)[inf])
# inf <- arrange(test, desc(ci))[1:size, "name"]
# results$ci <- resilience(graph, V(graph)[inf])
# # Resilience by model. Pick top n by probability
# inf <- arrange(test, desc(prediction_prob))[1:size, "name"]
# results$model <- resilience(graph, V(graph)[inf])
# print(unlist(results))


#### CONCLUSION:
#' 1. The accuracy of the model surpasses the heuristics in all instances
#' 2. ML model is also ahead in terms of predicting most resilient nodes as long as the structure of generated graph is similar to training graphs
#' 3. Closeness and Eigenvector traits are always outperformed by other traits
#' 4. On real world scenarios, the ML model outperforms the others regardless of size and structure
