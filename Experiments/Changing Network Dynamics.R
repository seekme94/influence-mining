library(igraph)

# This experiment learns the impact of small changes to the network which do and do not impact influential nodes

# Hypothesises:
# 1. Attaching a node to all nodes with degree = 1 will not change influential nodes
# 2. From 3 random nodes such that there is need for only single edge to make a triad, adding that edge will not change influential nodes
# 3. Connecting few nodes with degree = 1 and greater distances between them will change influential nodes


## Hypothesis 1

# Generate an Erdos graph
set.seed(1)
graph <- erdos.renyi.game(100, 0.05)
V(graph)$label <- 1:100

# Plot the graph
plot(graph)

# Calculate degrees
degrees <- degree(graph)

# Delete vertices where degree = 1
graph <- delete.vertices(graph, V(graph)[which(degrees == 1)])
V(graph)$label

# Plot the graph
plot(graph)

