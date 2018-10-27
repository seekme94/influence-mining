library(igraph)
library(parallel)
library(snow) # For linux
library(doMC) # For Linux
library(doSNOW) # For Windows
library(foreach)
library(iterpc)
library(jsonlite)
library(uuid)

source('graph_util.R')
source('influence_maximization.R')

# Get resiliences from given set of samples
get_resiliences <- function(combinations, graph, budget, parallel=FALSE) {
  #samples <- sample(1:nrow(combinations), nrow(combinations))
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
  unlist(resiliences)
}

# Define parameters
sizes <- c(40)
seeds <- c(1, 3, 60)
parallel <- TRUE

for (size in sizes) {
  budget <- size * 0.1
  prob <- 0.1
  # RANDOM
  for (seed in seeds) {
    experiment <- paste("Resilience experiment on Random graph_", "generate_random(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_random(size, prob)
    # Fetch all combinations of given budget
    combinations <- getall(iterpc(vcount(graph), budget))
    # Start timer
    start <- as.numeric(Sys.time())
    # Store resiliences of all combinations
    combinations <- cbind(combinations, get_resiliences(combinations, graph, budget, parallel))
    # Stop timer
    end <- as.numeric(Sys.time())
    # Collect best seed set and its resilience
    top_nodes <- V(graph)[combinations[which.min(combinations[,budget + 1]), 1:budget]]
    min_resilience <- combinations[which.min(combinations[,budget + 1]), budget + 1]
    results <- paste('{"experiment":"', experiment, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    print(results)
  }
}
