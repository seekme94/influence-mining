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
library(influence.mining)

# Load required source files
source('util/graph_util.R')
source('util/classification_util.R')
source('util/influence_maximization.R')
source('util/heuristics.R')


# Read test data set
author <- largest_component(read.graph("Experiments/data/author_netscience.txt", directed=FALSE))
ita2000 <- largest_component(read.graph("Experiments/data/ita2000.txt", directed=FALSE))
caida <- largest_component(read.graph("Experiments/data/as-caida.txt", directed=FALSE))
jdk <- largest_component(read.graph("Experiments/data/jdk6_dependencies.txt", directed=FALSE))

# Plot correlation matrix of dataset
author_data <- get_node_influence_traits(author)
author_corr <- round(cor(as.data.frame(author_data)), 2)
write.table(author_corr, file="Experiments/results/author_correlation.csv", quote=FALSE, sep=",")
corrplot(author_corr)

ita2000_data <- get_traits(ita2000)
ita2000_corr <- round(cor(as.data.frame(ita2000_data)), 2)
write.table(ita2000_corr, file="Experiments/results/ita2000_correlation.csv", quote=FALSE, sep=",")
corrplot(round(cor(as.data.frame(ita2000_data)), 2))

caida_data <- get_traits(caida)
caida_corr <- round(cor(as.data.frame(caida_data)), 2)
write.table(caida_corr, file="Experiments/results/caida_correlation.csv", quote=FALSE, sep=",")
corrplot(round(cor(as.data.frame(caida_data)), 2))

jdk_data <- get_traits(jdk)
jdk_corr <- round(cor(as.data.frame(jdk_data)), 2)
write.table(jdk_corr, file="Experiments/results/jdk_correlation.csv", quote=FALSE, sep=",")
corrplot(round(cor(as.data.frame(jdk_data)), 2))
