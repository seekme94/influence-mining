# This function inputs a graph object G, k as percentage and a seed method and returns k% nodes as seed using the adaptive method provided
# If the network is disconnected, then the function picks only the largest component in each iteration
data <- data.frame(f=c(1,1,1,1,1,1,2,2,2,2,3,3,3,4,4,5,6,7,7,8,8), t=c(2,3,4,5,9,10,6,11,12,7,4,9,17,13,5,6,8,14,15,15,16))
G <- G <- graph.data.frame(data, directed=FALSE, vertices=NULL)
k <- 25
seed_method="a-degree"
seed <- NULL
# Calculate the actual number of nodes to select as seed
G <- largest_component(G)
nodes <- V(G)
size <- length(nodes) * k / 100
while (length(seed) < size) {
  degree <- degree(G, V(G))
  max_node <- names(which.max(degree))
  seed <- c(seed, max_node)
  G <- delete.vertices(G, max_node)
  G <- largest_component(G)
}
seed
