###################################
#### Load required resources
###################################

source('./graph_util.R')
source('./influence_maximization.R')

###################################
#### Experiment settings
###################################

# Get results in a consolidated way
write_results <- function(experiment, graph, results) {
  degrees <- degree(graph)
  data <- data.frame(node=V(graph)$name,
                     degree=degrees,
                     closeness=closeness(graph),
                     betweenness=betweenness(graph),
                     eigenvalue=eigen_centrality(graph)$vector,
                     eccentricity=eccentricity(graph),
                     pagerank=page_rank(graph)$vector,
                     graph_size=size,
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
  write.table(data, file="results.out", quote=FALSE, row.names=FALSE, append=TRUE)
  write.table(results, file="results.out", quote=FALSE, row.names=FALSE, append=TRUE)
}

# Define parameters
size <- 60
budget <- size * 0.1
prob <- 0.1
# Repeat experiement 4 times
seeds <- c(1, 30, 600, 9000)
for (seed in seeds) {
  print(paste("Setting seed ", seed))
  set.seed(seed)
  experiment <- paste("Resilience experiment on Random graph", "generate_random(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  graph <- generate_random(size, prob)
  V(graph)$name <- 1:vcount(graph)
  combinations <- getall(iterpc(vcount(graph), budget))
  # Loop for each combination in the sample
  min_resilience <- size
  top_nodes <- NULL
  start <- as.numeric(Sys.time())
  # Dfine parameter to find out how many attempts after no update happens
  total_steps <- 0
  for (i in samples) {
    seed <- combinations[i, 1:budget]
    # Calculte the resilience after removal of nodes seed
    nodes <- V(graph)[seed]
    current <- resilience(graph, nodes)
    if (current < min_resilience) {
      min_resilience <- current
      top_nodes <- nodes
      total_steps <- total_steps + 1
      print(paste("Resilience:", min_resilience))
    }
  }
  foreach (i = samples) %dopar% {
    seed <- combinations[i, 1:budget]
    nodes <- V(graph)[seed]
    current <- resilience(graph, nodes)
    if (current < min_resilience) {
      min_resilience <- current
      top_nodes <- nodes
      total_steps <- total_steps + 1
      print(paste("Resilience:", min_resilience))
    }
  }
  end <- as.numeric(Sys.time())
  registerDoSEQ()
  stopCluster(cl)
  results <- paste('{"time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","total_steps":"', total_steps, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(experiment=experiment, graph=graph, results=results)
}

# SCALE FREE
for (seed in seeds) {
  print(paste("Setting seed ", seed))
  set.seed(seed)
  experiment <- paste("Resilience experiment on Scale-free graph", "generate_scale-free(size=", size, ",", "preference=1)", sep='')
  # Generate graph
  graph <- generate_scale_free(size)
  V(graph)$name <- 1:vcount(graph)
  combinations <- getall(iterpc(vcount(graph), budget))
  # Loop for each combination in the sample
  min_resilience <- size
  top_nodes <- NULL
  start <- as.numeric(Sys.time())
  # Dfine parameter to find out how many attempts after no update happens
  total_steps <- 0
  for (i in samples) {
    seed <- combinations[i, 1:budget]
    # Calculte the resilience after removal of nodes seed
    nodes <- V(graph)[seed]
    current <- resilience(graph, nodes)
    if (current < min_resilience) {
      min_resilience <- current
      top_nodes <- nodes
      total_steps <- total_steps + 1
      print(paste("Resilience:", min_resilience))
    }
  }
  end <- as.numeric(Sys.time())
  results <- paste('{"time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","total_steps":"', total_steps, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(experiment=experiment, graph=graph, results=results)
}

# SMALL WORLD
for (seed in seeds) {
  print(paste("Setting seed ", seed))
  set.seed(seed)
  experiment <- paste("Resilience experiment on Small world graph", "generate_small_world(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  graph <- generate_small_world(size, prob)
  V(graph)$name <- 1:vcount(graph)
  combinations <- getall(iterpc(vcount(graph), budget))
  # Loop for each combination in the sample
  min_resilience <- size
  top_nodes <- NULL
  start <- as.numeric(Sys.time())
  # Dfine parameter to find out how many attempts after no update happens
  total_steps <- 0
  for (i in samples) {
    seed <- combinations[i, 1:budget]
    # Calculte the resilience after removal of nodes seed
    nodes <- V(graph)[seed]
    current <- resilience(graph, nodes)
    if (current < min_resilience) {
      min_resilience <- current
      top_nodes <- nodes
      total_steps <- total_steps + 1
      print(paste("Resilience:", min_resilience))
    }
  }
  end <- as.numeric(Sys.time())
  results <- paste('{"time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","total_steps":"', total_steps, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(experiment=experiment, graph=graph, results=results)
}

# HOLME AND KIM
for (seed in seeds) {
  print(paste("Setting seed ", seed))
  set.seed(seed)
  experiment <- paste("Resilience experiment on Small world graph", "generate_holme_kim(size=", size, ",", "probability=", prob, ")", sep='')
  # Generate graph
  new_connections <- 2
  graph <- generate_holme_kim(size, new_connections, triad_prob=prob)
  V(graph)$name <- 1:vcount(graph)
  combinations <- getall(iterpc(vcount(graph), budget))
  # Loop for each combination in the sample
  min_resilience <- size
  top_nodes <- NULL
  start <- as.numeric(Sys.time())
  # Dfine parameter to find out how many attempts after no update happens
  total_steps <- 0
  for (i in samples) {
    seed <- combinations[i, 1:budget]
    # Calculte the resilience after removal of nodes seed
    nodes <- V(graph)[seed]
    current <- resilience(graph, nodes)
    if (current < min_resilience) {
      min_resilience <- current
      top_nodes <- nodes
      total_steps <- total_steps + 1
      print(paste("Resilience:", min_resilience))
    }
  }
  end <- as.numeric(Sys.time())
  results <- paste('{"time":"', (end - start), '","date":"', date(), '","resilience":"', min_resilience, '","total_steps":"', total_steps, '","nodes":', toJSON(top_nodes$name), '}', sep='')
  write_results(experiment=experiment, graph=graph, results=results)
}
