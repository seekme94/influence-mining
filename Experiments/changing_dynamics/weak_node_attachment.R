########################################################################################
##################################### Hypothesis A #####################################
# Attaching a node to all nodes with degree = 1 will not change influential nodes
########################################################################################

library(igraph)
library(influence.mining)

# Generate a graph
set.seed(1)
size <- 100
budget <- 5
#graph <- generate_small_world(size, log(size)/size)
graph <- generate_scale_free(size, 1.5)

# Remove disconnected nodes
graph <- largest_component(graph)
V(graph)$label <- 1:length(V(graph))
plot(graph, vertex.size=2)

# Extract influential nodes using Greedy method
influential <- greedy_influential(graph, budget, test_method="RESILIENCE")

# Algorithm: For each existing node with degree = 1, add a new node to the network
# Fetch nodes with degree = 1
degree_1 <- V(graph)[which(degree(graph) == 1)]

# Attach a new node with each degree_1 node
graph <- add.vertices(graph, length(degree_1))

# Assign missing labels to new nodes
V(graph)$label[is.na(V(graph)$label)] <- seq(size + 1, size + length(degree_1))

degree_1 <- V(graph)[which(degree(graph) == 1)]
degree_0 <- V(graph)[which(degree(graph) == 0)]

# Sequentially create an edge between both node sets
edges <- unlist(mapply(c, degree_1, degree_0, SIMPLIFY=FALSE))
graph <- add.edges(graph, edges)
plot(graph, vertex.size=2)

# Readjust budget so that the number of nodes stays same
budget <- size / length(V(graph)) * budget

# Extract influential nodes again
new_influential <- greedy_influential(graph, budget, test_method="RESILIENCE")

# Compare both influential node sets
influential
new_influential
setdiff(influential$influential_nodes, new_influential$influential_nodes)


########################################################################################
##################################### Hypothesis B #####################################
# Creating a new triad by adding an edge with the third node (connection of connection)
########################################################################################

# Generate a graph
set.seed(1)
size <- 100
budget <- 5
#graph <- generate_small_world(size, log(size)/size)
graph <- generate_scale_free(size, 1.5)

# Remove disconnected nodes
graph <- largest_component(graph)
V(graph)$label <- 1:length(V(graph))
plot(graph, vertex.size=2)

# Extract influential nodes using Greedy method
influential <- greedy_influential(graph, budget, test_method="RESILIENCE")

# Algorithm: For each existing node with degree = 1, add a new node to the network
# Fetch nodes with degree = 1
degree_1 <- V(graph)[which(degree(graph) == 1)]

# Attach a new node with each degree_1 node
graph <- add.vertices(graph, length(degree_1))

# Assign missing labels to new nodes
V(graph)$label[is.na(V(graph)$label)] <- seq(size + 1, size + length(degree_1))

degree_1 <- V(graph)[which(degree(graph) == 1)]
degree_0 <- V(graph)[which(degree(graph) == 0)]

# Fetch the nodes which the degree_1 nodes were initially connected with
first_node <- V(graph)[sapply(degree_1, function(x) { unlist(neighborhood(graph, x, order=1))[2] })]

# Sequentially create an edge between both node sets
edges <- unlist(mapply(c, degree_1, degree_0, SIMPLIFY=FALSE))

# Also create edges with the first_node set
edges <- c(edges, unlist(mapply(c, first_node, degree_0, SIMPLIFY=FALSE)))

graph <- add.edges(graph, edges)
plot(graph, vertex.size=2)

# Readjust budget so that the number of nodes stays same
budget <- size / length(V(graph)) * budget

# Extract influential nodes again
new_influential <- greedy_influential(graph, budget, test_method="RESILIENCE")

# Compare both influential node sets
influential
new_influential
setdiff(influential$influential_nodes, new_influential$influential_nodes)

# Conclusions of Hypothesis A(a & b):
# 1. For small-world graphs of length ranging between 100 and 600, no change was noticed in the set of influential nodes (top 5%)
# 2. The possible cause is the formation of star, since in a star network, the only influential node is the core node
# 3. The hypothesis goes against Fitness model of network theory, in a sense that new nodes are connecting to most unfit nodes
# 4. In case of Scale-free, difference of exactly 2 nodes was observed in 3 out of 5 cases
# 5. The above conclusions stayed true even after creating a triad between newly added node and the node which was the connection of degree_1 node
