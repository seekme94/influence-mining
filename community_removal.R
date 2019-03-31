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
ita2000 <- largest_component(read.graph("Experiments/data/ita2000.txt", directed=FALSE))
caida <- largest_component(read.graph("Experiments/data/as-caida.txt", directed=FALSE))
jdk <- largest_component(read.graph("Experiments/data/jdk6_dependencies.txt", directed=FALSE))

# Detect communities using various algorithm
V(author)$name <- V(author)
communities <- find_communities(author, plot=TRUE, method="multilevel")

high_degreers <- NULL
high_pagerankers <- NULL
high_ciers <- NULL
for (group in unique(V(communities)$group)) {
  graph <- author
  sub_g <- delete.vertices(graph, which(V(communities)$group == group))
  sub_g_traits <- get_node_influence_traits(sub_g, traits=c("degree", "betweenness", "pagerank", "ci"))
  high_degree <- which(sub_g_traits$degree == max(sub_g_traits$degree))
  high_pagerank <- which(sub_g_traits$pagerank == max(sub_g_traits$pagerank))
  high_ci <- which(sub_g_traits$ci == max(sub_g_traits$ci))
  high_degreers <- c(high_degreers, high_degree)
  high_pagerankers <- c(high_pagerankers, high_pagerank)
  high_ci <- c(high_ciers, high_ci)
}

