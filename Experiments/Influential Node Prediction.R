###################################
#### Load required resources
###################################

# THIS CODE IS TESTED ON R-3.4.4

library(igraph)
library(parallel)
library(doMC) # For linux
library(doSNOW) # For Windows
library(foreach)
library(jsonlite)
library(uuid)
# Ubuntu requires apt install libssl-dev libcurl4-openssl-dev libgmp-dev
library(iterpc)
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc
library(future.apply)

source('graph_util.R')
source('influence_maximization.R')

###################################
#### Experiment functions
###################################

# Get results in a consolidated way
write_results <- function(uuid, seed, graph, results) {
  data <- get_graph_traits(graph)
  filename <- paste("Experiments/optimal/graph_", vcount(graph), "_", uuid, ".out", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=TRUE, sep=',')
  write.table(results, file="Experiments/optimal/results.out", quote=FALSE, row.names=FALSE, append=TRUE)
}

#' This method returns resiliences of all combinations of sets of budget size from given graph
#' @name get_resiliences
#' @param graph is the igraph object
#' @param budget defines size of combinations of nodes. Value must be between 0 and 1
#' @param parallel flag defines whether the execution will use parallel processing or not. Default is FALSE
#' @return vector of resiliences of provided combinations
get_resiliences <- function(graph, budget, parallel=FALSE) {
  # Fetch all combinations of given budget
  combinations <- getall(iterpc(vcount(graph), round(budget)))
  samples <- 1:nrow(combinations)
  resiliences <- NULL
  if (parallel) {
    # Initiate parallel processing
    cores <- detectCores() - 1
    cl <- makeCluster(cores)
    # For linux
    if (Sys.info()[[1]] == "Linux") {
      registerDoMC(cores)
      # Loop for each combination in the sample
      resiliences <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience","largest_component")) %dopar% {
        # Pick a random sample
        seed <- combinations[i, 1:budget]
        # Calculte the resilience after removal of nodes seed
        resilience(graph, V(graph)[seed])
      }
    } else {
      registerDoSNOW(cl)
      # Loop for each combination in the sample
      resiliences <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience","largest_component")) %dopar% {
        # Pick a random sample
        seed <- combinations[i, 1:budget]
        # Calculte the resilience after removal of nodes seed
        resilience(graph, V(graph)[seed])
      }
      # Stop parallel processing cluster
      stopCluster(cl)
    }
  } else {
    for(i in samples) {
      # Pick sample
      seed <- combinations[i, 1:budget]
      # Calculte the resilience after removal of nodes seed
      resiliences <- c(resiliences, resilience(graph, V(graph)[seed]))
    }
  }
  cbind(combinations, unlist(resiliences))
}

#' This method returns resiliences from given graph using greedy approach
#' @name get_resiliences_greedy
#' @param graph is the igraph object
#' @param budget defines size of combinations of nodes. Value must be between 0 and 1
#' @param parallel flag defines whether the execution will use parallel processing or not. Default is FALSE
#' @return vector of resiliences of provided graph
get_resiliences_greedy <- function(graph, budget) {
  if (budget < 1) {
    budget <- budget * size
  }
  nodes <- V(graph)
  # Run Greedy method
  top_nodes <- NULL
  # While seed < budget
  while (length(top_nodes) < budget) {
    max_resilience <- size
    most_influential <- NULL
    output <- NULL
    # For all nodes except seed
    for (node in setdiff(nodes, top_nodes)) {
      # Find resilience of node with existing nodes in seed
      output <- resilience(graph, c(top_nodes, node))
      # If current node causes more influence than maximum so far, then swap
      if (output < max_resilience) {
        most_influential <- node
        max_resilience <- output
      }
    }
    # At the end, we should have node with maximum influence to add to seed
    top_nodes <- c(top_nodes, most_influential)
  }
  V(graph)[top_nodes]
}

###################################
#### Experiment settings
###################################

# Define parameters
# Repeat experiement for multiple sizes
sizes <- c(30, 35, 40, 45)
# Repeat experiement 4 times
# For model training
seeds <- c(1, 30, 600, 9000)
prob <- 0.1
parallel <- TRUE

for (size in sizes) {
  budget <- size * prob
  # RANDOM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Random graph_", "generate_random(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_random(size, prob)
    # Start timer
    start <- as.numeric(Sys.time())
    # Store resiliences of all combinations
    combinations <- get_resiliences(graph, budget, parallel)
    # Stop timer
    end <- as.numeric(Sys.time())
    # Collect best seed set and its resilience
    top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
    min_resilience <- combinations[which.min(combinations[,budget + 1]), budget + 1]
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    print(results)
    #    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # SCALE FREE
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale-free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_scale_free(size)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
    # Fetch all combinations of given budget
    combinations <- getall(iterpc(vcount(graph), round(budget)))
    # Start timer
    start <- as.numeric(Sys.time())
    # Store resiliences of all combinations
    combinations <- cbind(combinations, get_resiliences(combinations, graph, budget, parallel))
    # Stop timer
    end <- as.numeric(Sys.time())
    # Collect best seed set and its resilience
    top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
    min_resilience <- combinations[which.min(combinations[,budget + 1]), budget + 1]
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # SMALL WORLD
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_small_world(size, prob)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
    # Fetch all combinations of given budget
    combinations <- getall(iterpc(vcount(graph), round(budget)))
    # Start timer
    start <- as.numeric(Sys.time())
    # Store resiliences of all combinations
    combinations <- cbind(combinations, get_resiliences(combinations, graph, budget, parallel))
    # Stop timer
    end <- as.numeric(Sys.time())
    # Collect best seed set and its resilience
    top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
    min_resilience <- combinations[which.min(combinations[,budget + 1]), budget + 1]
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # HOLME AND KIM
  for (seed in seeds) {
    set.seed(seed)
    experiment <- paste("Resilience experiment on Small world graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
    # Fetch all combinations of given budget
    combinations <- getall(iterpc(vcount(graph), round(budget)))
    # Start timer
    start <- as.numeric(Sys.time())
    # Store resiliences of all combinations
    combinations <- cbind(combinations, get_resiliences(combinations, graph, budget, parallel))
    # Stop timer
    end <- as.numeric(Sys.time())
    # Collect best seed set and its resilience
    top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
    min_resilience <- combinations[which.min(combinations[,budget + 1]), budget + 1]
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
}


###################################
#### Greedy approach for large size 
###################################

#** Note: Use future.apply function to execute in parallel
sizes <- seq(from=1000, to=4500, by=100)
prob <- 0.05
for (size in sizes) {
  budget <- size * prob
  experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale-free(size=", size, ",", "preference=1)", sep='')
  # Generate scale free graph
  graph <- generate_scale_free(size)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  # Start timer
  start <- as.numeric(Sys.time())
  top_nodes <- get_resiliences_greedy(graph, budget)
  # Stop timer
  end <- as.numeric(Sys.time())
  # Format the output as a single string
  uuid <- UUIDgenerate()
  results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(uuid=uuid, graph=graph, seed=seed, results=results)

  experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate small world graph
  graph <- generate_small_world(size, prob)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  # Start timer
  start <- as.numeric(Sys.time())
  top_nodes <- get_resiliences_greedy(graph, budget)
  # Stop timer
  end <- as.numeric(Sys.time())
  # Format the output as a single string
  uuid <- UUIDgenerate()
  results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  
  experiment <- paste("Resilience experiment on Small world graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate Holme & Kim graph
  new_connections <- 2
  graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  # Start timer
  start <- as.numeric(Sys.time())
  top_nodes <- get_resiliences_greedy(graph, budget)
  # Stop timer
  end <- as.numeric(Sys.time())
  # Format the output as a single string
  uuid <- UUIDgenerate()
  results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(uuid=uuid, graph=graph, seed=seed, results=results)
}

