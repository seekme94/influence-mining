###################################
#### Load required resources
###################################

# THIS CODE IS TESTED ON R-3.4.4

library(igraph)
library(parallel)
#library(doMC) # For linux
library(doSNOW) # For Windows
library(foreach)
library(jsonlite)
library(uuid)
# Ubuntu requires apt install build-essential libssl-dev libcurl4-openssl-dev libgmp-dev libcairo2-dev liblzma-dev libblas-dev
library(iterpc)
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc
library(future.apply)

source('util/graph_util.R')
source('util/influence_maximization.R')

###################################
#### Experiment functions
###################################

# Get results in a consolidated way
write_results <- function(uuid, graph, results) {
  data <- get_graph_traits(graph)
  filename <- paste("data/optimal/graph_", vcount(graph), "_", uuid, ".csv", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=TRUE, sep=',')
  write.table(results, file="results/optimal_results.json", quote=FALSE, row.names=FALSE, append=TRUE)
}

###################################
#### Experiment settings
###################################

# Define parameters
# Repeat experiement 4 times
# 3 for training; 3 for test
seeds <- c(2, 30, 500, 7000, 110000, 1300000)
prob <- 0.1

# Repeat experiement for multiple sizes
sizes <- c()
for (size in sizes) {
  budget <- size * prob
  # SCALE FREE
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale free graph ", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
  
  # SMALL WORLD
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph ", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
  
  # HOLME AND KIM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Holme and Kim graph ", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
}


###################################
#### Greedy approach for large size 
###################################

#** Note: Use future.apply function to execute in parallel
sizes <- seq(from=2600, to=3000, by=100)
prob <- 0.025

for (size in sizes) {
  budget <- size * prob
  # SCALE FREE
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
  
  # SMALL WORLD
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
  
  # HOLME AND KIM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Holme and Kim graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph)
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  }
}
