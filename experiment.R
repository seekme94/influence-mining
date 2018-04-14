###################################
#### Load required resources
###################################

library(igraph)
library(RMySQL)
library(parallel)
#library(snow) # For linux
library(doSNOW) # For windows
library(foreach)
library(iterpc)
library(jsonlite)
library(uuid)

source('graph_util.R')
source('influence_maximization.R')

###################################
#### Experiment settings
###################################

# Get results in a consolidated way
write_results <- function(uuid, seed, graph, results) {
  degrees <- degree(graph)
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
  filename <- paste("graph_", vcount(graph), "_", uuid, ".out", sep='')
  write.table(data, file=filename, quote=FALSE, row.names=FALSE, append=TRUE, sep=',')
  write.table(results, file="results.out", quote=FALSE, row.names=FALSE, append=TRUE)
}

# Define parameters
size <- 50
budget <- size * 0.1
prob <- 0.1
# Repeat experiement 4 times
#seeds <- c(1, 300, 600, 9000)
seeds <- c(1)

parallel <- FALSE

# RANDOM
for (seed in seeds) {
  print(paste("Setting seed ", seed))
  set.seed(seed)
  experiment <- paste("Resilience experiment on Random graph", "generate_random(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  graph <- generate_random(size, prob)
  # Allocate node indices as node names
  V(graph)$name <- 1:vcount(graph)
  # Fetch all combinations of given budget
  combinations <- getall(iterpc(vcount(graph), budget))
  # Add another column for resilience
  combinations <- cbind(combinations, 0)
  # This will be used to trial on random seed sets
  #samples <- sample(1:nrow(combinations), nrow(combinations))

  if (parallel) {
    # Initiate parallel processing
    cores <- detectCores() - 2
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
    # Start timer
    start <- as.numeric(Sys.time())
    resiliences <- NULL
    # Loop for each combination in the sample
    resiliences <- foreach (i = samples, .packages=c("igraph"), .export=c("resilience")) %dopar% {
      # Pick a random sample
      seed <- combinations[i, 1:budget]
      nodes <- V(graph)[seed]
      # Calculte the resilience after removal of nodes seed
      resilience(graph, nodes)
    }
    # Stop parallel processing cluster
    stopCluster(cl)
  } else {
    max_influence <- vcount(graph)
    top_seed <- NULL
    for(i in samples) {
      # Pick a random sample
      seed <- combinations[i, 1:budget]
      nodes <- V(graph)[seed]
      # Calculte the resilience after removal of nodes seed
      current <- resilience(graph, nodes)
      if (current < max_influence) {
        max_influence <- current
        top_seed <- seed
        print(current)
      }
    }
  }
  combinations[, budget + 1] <- unlist(resiliences)
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
  experiment <- paste("Resilience experiment on Scale-free graph", "generate_scale-free(size=", size, ",", "preference=1)", sep='')
  # Generate graph
  graph <- generate_scale_free(size)
}

# SMALL WORLD
for (seed in seeds) {
  experiment <- paste("Resilience experiment on Small world graph", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  graph <- generate_small_world(size, prob)
}

# HOLME AND KIM
for (seed in seeds) {
  experiment <- paste("Resilience experiment on Small world graph", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  new_connections <- 2
  graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
}
