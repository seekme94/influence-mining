###################################
#### Load required resources
###################################

library(igraph)
library(parallel)
library(snow) # For linux
#library(doSNOW) # For Windows
library(foreach)
#devtools::install_github("randy3k/iterpc") # Alternative way to install iterpc

devtools::install_github("randy3k/iterpc")

library(iterpc)
library(jsonlite)
library(uuid)

source('graph_util.R')
source('influence_maximization.R')

###################################
#### Experiment settings
###################################

# Calculates several traits from given graph and returns as data frame
get_graph_traits <- function(graph) {
  degrees <- degree(graph)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  data <- data.frame(node=V(graph)$name,
                     degree=degrees,
                     closeness=closeness(graph),
                     betweenness=betweenness(graph),
                     eigenvalue=eigen_centrality(graph)$vector,
                     eccentricity=eccentricity(graph),
                     pagerank=page_rank(graph)$vector,
                     graph_size=vcount(graph),
                     graph_edges=ecount(graph),
                     graph_avg_degree=mean(degrees),
                     graph_max_degree=max(degrees),
                     graph_apl=average.path.length(graph),
                     graph_clust_coef=transitivity(graph),
                     graph_diameter=diameter(graph),
                     graph_density=graph.density(graph),
                     graph_assortativity=assortativity.degree(graph),
                     avg_distance=mean_distance(graph),
                     graph_triads=length(triangles(graph)),
                     graph_girth=girth(graph)$girth)
  data
}

# Get results in a consolidated way
write_results <- function(uuid, seed, graph, results) {
  data <- get_graph_traits(graph)
  filename <- paste("Experiments/optimal/test_graph_", vcount(graph), "_", uuid, ".out", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=TRUE, sep=',')
  write.table(results, file="Experiments/optimal/test_results.out", quote=FALSE, row.names=FALSE, append=TRUE)
}

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
      registerDoSEQ()
    } else {
      registerDoSNOW(cl)
    }
    # Loop for each combination in the sample
    resiliences <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience","largest_component")) %dopar% {
      # Pick a random sample
      seed <- combinations[i, 1:budget]
      # Calculte the resilience after removal of nodes seed
      resilience(graph, V(graph)[seed])
    }
    # Stop parallel processing cluster
    stopCluster(cl)
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
# Repeat experiement for multiple sizes
sizes <- c(29)
# Repeat experiement 4 times
# For model training
seeds <- c(1, 30, 600, 9000)
# For testing
seeds <- c(3, 60, 900, 1000)
#seeds <- c(1)
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
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # SCALE FREE
  for (seed in seeds) {
    experiment <- paste("Resilience experiment on Scale-free graph_", "generate_scale-free(size=", size, ",", "preference=1)", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_scale_free(size)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
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
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # SMALL WORLD
  for (seed in seeds) {
    experiment <- paste("Resilience experiment on Small world graph_", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    graph <- generate_small_world(size, prob)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
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
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
  
  # HOLME AND KIM
  for (seed in seeds) {
    experiment <- paste("Resilience experiment on Small world graph_", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
    print(experiment)
    # Generate graph
    new_connections <- 2
    graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
    # Allocate node indices as node names
    V(graph)$name <- 1:vcount(graph)
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
    # Format the output as a single string
    uuid <- UUIDgenerate()
    results <- paste('{"experiment":"', experiment, '","uuid":"', uuid, '","time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","nodes":', toJSON(top_nodes$name), '}', sep='')
    write_results(uuid=uuid, graph=graph, seed=seed, results=results)
  }
}
