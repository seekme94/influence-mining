###################################
#### Load required resources
###################################

# THIS CODE IS TESTED ON R-3.5

library(igraph)
#devtools::install_github("seekme94/influence.mining")
library(jsonlite)
library(uuid)
library(influence.mining)

library(parallel)
#library(doMC) # For linux
library(doSNOW) # For Windows
# Ubuntu requires apt install build-essential libssl-dev libcurl4-openssl-dev libgmp-dev libcairo2-dev liblzma-dev libblas-dev
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc
library(future.apply)
library(dplyr)
library(centiserve)
library(xgboost)

source("util/classification_util.R")
root_dir <- "Experiments/FZExperiment/"

graph <- largest_component(read.graph("dataset/protein_barabasi.txt", directed=FALSE))
inf <- influence(graph, budget = 10, prob = 0.5, test_method = "RESILIENCE", heuristic = "CENTRALITY" , centrality_method = "BETWEENNESS", logging = FALSE)
inf

inf <- influence(graph, budget = 10, prob = 0.5, test_method = "RESILIENCE", heuristic = "CENTRALITY" , centrality_method = "CLOSENESS", logging = FALSE)
inf

inf <- influence(graph, budget = 10, prob = 0.5, test_method = "RESILIENCE", heuristic = "ADAPTIVE_CENTRALITY" , centrality_method = "DEGREE", logging = FALSE)
inf
