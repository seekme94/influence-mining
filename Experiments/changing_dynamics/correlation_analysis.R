######################################################################################
##################################### Hypothesis #####################################
# By correlating coreness of nodes with other heuristics and traits, we can discover
# which nodes will not be vital
######################################################################################

# Load required libraries
library(igraph)
library(arules)
library(dplyr)
library(caret)
library(party)
library(doParallel)
library(corrplot)

# Load required source files
source('util/graph_util.R')
source('util/classification_util.R')
source('util/influence_maximization.R')
source('util/heuristics.R')

get_traits <- function(graph) {
  data <- NULL
  data$degree <- degree(graph)
  data$betweenness <- betweenness(graph)
  data$closeness <- closeness(graph)
  data$eigenvector <- evcent(graph)$vector
  data$coreness <- coreness(graph)
  data$pagerank <- page_rank(graph)$vector
  data$ci <- sapply(V(graph), function(x) { collective_influence(graph, neighborhood_distance=2, x) })
  data$a_degree <- get_adaptive_ranking(graph, "degree")
  data$a_betweenness <- get_adaptive_ranking(graph, "betweenness")
  data$a_closeness <- get_adaptive_ranking(graph, "closeness")
  data$a_eigenvector <- get_adaptive_ranking(graph, "eigenvector")
  data$a_coreness <- get_adaptive_ranking(graph, "coreness")
  data$a_pagerank <- get_adaptive_ranking(graph, "pagerank")
  data$a_ci <- get_adaptive_ranking(graph, "collective_influence")
  data
}


# Read test data set
author <- largest_component(read.graph("Experiments/data/author_netscience.txt", directed=FALSE))
ita2000 <- largest_component(read.graph("Experiments/data/ita2000.txt", directed=FALSE))
caida <- largest_component(read.graph("Experiments/data/as-caida.txt", directed=FALSE))
jdk <- largest_component(read.graph("Experiments/data/jdk6_dependencies.txt", directed=FALSE))

# Plot correlation matrix of dataset
author_data <- get_traits(author)
corrplot(round(cor(as.data.frame(author_data)), 2))

ita2000_data <- get_traits(ita2000)
corrplot(round(cor(as.data.frame(ita2000_data)), 2))

caida_data <- get_traits(caida)
corrplot(round(cor(as.data.frame(caida_data)), 2))

jdk_data <- get_traits(jdk)
corrplot(round(cor(as.data.frame(jdk_data)), 2))


# Influential nodes by all traits
results <- NULL
size <- nrow(test) * 0.05
# By degree
inf <- arrange(test, desc(degree))[1:size, "node"]
results$degree <- resilience(graph, V(graph)[inf])
# By betweenness
inf <- arrange(test, desc(betweenness))[1:size, "node"]
results$betweenness <- resilience(graph, V(graph)[inf])
# By closeness
inf <- arrange(test, desc(closeness))[1:size, "node"]
results$closeness <- resilience(graph, V(graph)[inf])
# By Eigen-vector centrality
inf <- arrange(test, desc(eigenvalue))[1:size, "node"]
results$eigenvalue <- resilience(graph, V(graph)[inf])
# By pagerank
inf <- arrange(test, desc(pagerank))[1:size, "node"]
results$pagerank <- resilience(graph, V(graph)[inf])
# By eccentricity
inf <- arrange(test, desc(eccentricity))[1:size, "node"]
results$eccentricity <- resilience(graph, V(graph)[inf])
# By coreness
inf <- arrange(test, desc(coreness))[1:size, "node"]
results$coreness <- resilience(graph, V(graph)[inf])
# By collective influence
inf <- arrange(test, desc(ci))[1:size, "node"]
results$ci <- resilience(graph, V(graph)[inf])

print(unlist(results))

# Rank coreness against all other traits and their adaptive variations


#### CONCLUSION:
