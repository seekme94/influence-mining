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
source('util/heuristics.R')
source('util/influence_maximization.R')

root_dir <- "Experiments/influential_node_prediction/"

###################################
#### Experiment functions
###################################

# Get results in a consolidated way
write_results <- function(uuid, graph, results) {
  data <- get_graph_traits(graph)
  write.graph(graph, paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".el", sep=''), format='edgelist')
  filename <- paste(root_dir, "data/graph_", vcount(graph), "_", uuid, ".csv", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=FALSE, sep=',')
  write.table(results, file=paste(root_dir, "optimal_nodes.json", sep=''), quote=FALSE, row.names=FALSE, append=TRUE)
}

###################################
#### Experiment settings
###################################

# Define parameters
# Repeat experiement 4 times
# 3 for training; 3 for test
seeds <- c(2, 3, 5, 8, 13, 21)
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
    V(graph)$name <- 1:vcount(graph) - 1
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
    V(graph)$name <- 1:vcount(graph) - 1
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
    V(graph)$name <- 1:vcount(graph) - 1
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

sizes <- c()
prob <- 0.025

#** Note: Using future.apply functions to execute in parallel
plan(multiprocess)

for (size in sizes) {
  budget <- size * prob
  # SCALE FREE
  #  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale_free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    graph <- generate_scale_free(size)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })

  # SMALL WORLD
#  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    graph <- generate_small_world(size, prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })
  
  # HOLME AND KIM
#  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Holme and Kim graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })
}


###################################
#### Greedy on Real Datasets 
###################################

datasets <- c("karate_club.txt", "world_trade.txt", "nematode_neural_network.txt", "my_twitter_network.txt", "political_blog.txt", "citation_network_influence.txt", "protein_barabasi.txt")
# Deal with this large graph: "arxiv_collaboration.txt"

for (dataset in datasets) {
  graph <- largest_component(read.graph(paste("dataset/", dataset, sep=''), directed=FALSE, format="ncol"))
  size <- vcount(graph)
  prob <- 0.1
  if (size > 50) {
    prob <- 0.025
  }
  #  for (seed in seeds) {
  future_lapply(seeds, function(seed) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on graph_", dataset, "(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    V(graph)$name <- 1:vcount(graph) - 1
    start <- as.numeric(Sys.time())
    top_nodes <- get_influential_nodes_greedy(graph, budget)
    end <- as.numeric(Sys.time())
    min_resilience <- resilience(graph, top_nodes)
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","size":"', size, '","uuid":"', uuid, '","seed":"', seed, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, results=results)
  })
}
