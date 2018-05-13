library(caret)
library(e1071)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
library(RWeka)
library(elmNN)
library(nnet)
library(neuralnet)
library(sampling)
library(digest)
library(igraph)
library(jsonlite)
library(uuid)

setwd('Experiments/optimal/')

source('../../graph_util.R')

train <- NULL

# Read the results
results <- fromJSON(paste(readLines("results.json")))
head(results)

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
# Formula for the model
formula <- influential ~ degree + closeness + betweenness + eigenvalue + eccentricity + pagerank + graph_size + graph_edges + graph_avg_degree + graph_max_degree + graph_apl + graph_clust_coef + graph_diameter + graph_density + graph_assortativity + avg_distance + graph_triads + graph_girth

# Learn the linear regression model
model <- glm(formula, family=binomial(link='logit'),data=train)

head(train)


# TODO
summary(model)
anova(model)
plot(model)

# Create test data
size <- 50
seeds <- c(3, 60, 900, 1000)
prob <- 0.1
set.seed(3)
test_graph <- generate_scale_free(size, 1)
test <- get_graph_traits(test_graph)
tkplot(test_graph)

predicted <- predict(model, newdata=test, type='response')
order(predicted)

error <- mean(predicted != test$influential)
print(paste('Accuracy', 1 - error))
