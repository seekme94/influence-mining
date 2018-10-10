library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
library(nnet)
library(neuralnet)
library(sampling)
library(digest)
library(e1071)
library(igraph)
library(jsonlite)
library(uuid)

setwd('Experiments/optimal/')

source('../../graph_util.R')

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


# Formula for the model
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank + graph_size + graph_edges + graph_avg_degree + graph_max_degree + graph_apl + graph_clust_coef + graph_diameter + graph_density + graph_assortativity + avg_distance + graph_triads + graph_girth

# Learn the linear regression model
model <- glm(formula, family=binomial(link='logit'), data=train)

head(train)


# TODO
summary(model)
anova(model)
plot(model)

# Create test data
head(test[,-(21)])

predicted <- predict(model, newdata=test[,-(21)], type='response')
test$prediction <- as.numeric(predicted >= 0.5)

error <- mean(test$influential != test$prediction)
print(paste('Accuracy', 1 - error))

