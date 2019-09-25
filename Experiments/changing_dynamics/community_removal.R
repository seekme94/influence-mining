######################################################################################
##################################### Hypothesis #####################################
# If the most vital node in a community is not influential, none of the other nodes in
# that community will be influential
######################################################################################

library(igraph)

# Load required source files
source('util/graph_util.R')
source('util/influence_maximization.R')
source('util/heuristics.R')
source('util/community_detection.R')

# Read test data set
author <- largest_component(read.graph("Experiments/data/author_netscience.txt", directed=FALSE))
# ita2000 <- largest_component(read.graph("Experiments/data/ita2000.txt", directed=FALSE))
# caida <- largest_component(read.graph("Experiments/data/as-caida.txt", directed=FALSE))
# jdk <- largest_component(read.graph("Experiments/data/jdk6_dependencies.txt", directed=FALSE))

##########################
######### AUTHOR #########
##########################
# Detect communities using various algorithms
graph <- author
V(graph)$name <- V(graph)
influential <- get_influential_nodes_greedy(graph, 0.05)
graph <- find_communities(graph, plot=TRUE, method="multilevel")

community_influential <- NULL
# For each community, create sub-graph from that community and extract set of influential nodes
for (group in unique(V(graph)$group)) {
  sub_g <- delete.vertices(graph, which(V(communities)$group != group))
  traits <- get_node_influence_traits(sub_g, traits=c("degree", "betweenness", "pagerank", "ci"))
  inf_metric <- traits$ci
  community_influential <- c(community_influential, names(which(inf_metric == max(inf_metric))))
}
# Create a graph of influential nodes from all communities
sub_g <- induced_subgraph(graph, V(graph)[community_influential])
plot(sub_g)

# Identify disconnected communities
degrees <- degree(sub_g)
non_inf_communities <- V(sub_g)[which(degrees == 0)]$group

# Separate out all nodes from original graph which belong to disconnected communities
non_inf_nodes <- V(graph)[which(V(graph)$group %in% non_inf_communities)]
match <- length(influential[influential %in% non_inf_nodes]) / length(influential)
match

# Plot the graph, showing communities
graph$layout <- layout.circle
V(graph)$color <- "green"
V(graph)[influential]$color = "red"
plot(graph)
