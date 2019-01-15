######################################################################################
##################################### Hypothesis #####################################
# Connecting few nodes with degree = 1, and have greater distances between them will 
# change influential nodes
######################################################################################

library(igraph)

source("util/graph_util.R")
source("util/influence_maximization.R")

# Generate a graph
seed <- 100
budget <- 0.05
set.seed(seed)
graphs <- data.frame()
results <- data.frame()
for (size in seq(100, 1000, by=100)) {
  # graph <- generate_small_world(size, 3/size)
  graph <- generate_scale_free(size, 1 + (1/log(size)))
  # Remove disconnected nodes
  graph <- largest_component(graph)
  graphs <- rbind(graphs, as.data.frame(graph_summary(graph)))
  V(graph)$label <- 1:length(V(graph))
  plot(graph, vertex.size=2)

  # Extract influential nodes using Greedy method
  influential <- get_influential_nodes_greedy(graph, budget)

  # Find shortest paths for every pair of nodes
  paths <- shortest.paths(graph, V(graph), V(graph))

  # Select row-wise maximum shortest path
  max_paths <- apply(paths, 1, function(x) max(x))
  max_path <- max(max_paths)
  distinct <- data.frame()
  for (i in 1:length(paths[1,])) {
    for (j in i:length(paths[,1])) {
      if (paths[i,j] == max_path) {
        distinct <- rbind(distinct, c(i, j))
      }
    }
  }
  names(distinct) <- c("from", "to")

  # (Optional) Create a fraction of links instead of all
  set.seed(seed)
  distinct <- distinct[sample(1:nrow(distinct), ceiling(sqrt(nrow(distinct)))),]
  
  # Sequentially create an edge between each set of distinct nodes
  edges <- unlist(mapply(c, V(graph)[distinct$from], V(graph)[distinct$to], SIMPLIFY=FALSE))
  graph <- add.edges(graph, edges)
  plot(graph, vertex.size=2)
  
  # Extract influential nodes again
  new_influential <- get_influential_nodes_greedy(graph, budget)

  # Compare both influential node sets
  diff <- setdiff(influential, new_influential)

  # Capture results
  results <- rbind(results, c(size, length(influential), length(diff)))
}
names(results) <- c("size", "inf_size", "diff")
results$change <- round(results$diff / results$inf_size * 100)
graphs <- cbind(graphs, results)
graphs

# Conclusion (small-world):
# The experiment analyzed change in influential nodes by connecting distinct nodes of 10 networks of sizes from 100 to 1000.
# Distinct nodes are considered to be those with the longest shortest path.
# We create an edge between some (a Sq.root subset) of the distinct nodes.
# There was visible difference in set of influential nodes before and after node wiring. The avg. change measured to be 13%
# Possible concerns:
# 1. There is still no straight way to ensure that only the size of the network grows, while other traits remain same
# 2. 
