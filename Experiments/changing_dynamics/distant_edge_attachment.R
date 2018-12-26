######################################################################################
##################################### Hypothesis #####################################
# Connecting few nodes with degree = 1, and have greater distances between them will 
# change influential nodes
######################################################################################

library(igraph)

source("util/graph_util.R")
source("util/influence_maximization.R")

# Generate a graph
set.seed(1)
size <- 100
budget <- 0.05
graph <- generate_small_world(size, log(size)/size)

# Remove disconnected nodes
graph <- largest_component(graph)
V(graph)$label <- 1:length(V(graph))
plot(graph, vertex.size=2)

# Extract influential nodes using Greedy method
influential <- get_influential_nodes_greedy(graph, budget)

## Algorithm: select n nodes which are most distinct
# First, find shortest paths for every pair of nodes
paths <- shortest.paths(graph, V(graph), V(graph))
# Select row-wise maximum shortest path
max_path <- apply(paths, 1, function(x) max(x))
max_path <- max(max_path)
for (i in 1:(size-1)) {
  for (j in 1:(size-1)) {
    if (paths[i,j] == max_path) {
      print(paste(i,j))
    }
  }
}


# Sequentially create an edge between both node sets
edges <- unlist(mapply(c, degree_1, degree_0, SIMPLIFY=FALSE))
graph <- add.edges(graph, edges)
plot(graph, vertex.size=2)

# Readjust budget so that the number of nodes stays same
budget <- size / length(V(graph)) * budget

# Extract influential nodes again
new_influential <- get_influential_nodes_greedy(graph, budget)

# Compare both influential node sets
influential
new_influential
setdiff(influential, new_influential)

