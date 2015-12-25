# Resillience of network: how fast a network goes down when nodes go down

library(igraph)
ds <- read.csv("D:/Datasets/Twitter/my_twitter_network.txt", sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=TRUE)
graph_summary(g, TRUE)


# Seelct 100% nodes as seed for ranking
# RANKING NODES
deg_seed <- select_adaptive_seed(g, 100, seed_method="a-degree")
deg_rank <- sort(deg_seed)
bet_seed <- select_adaptive_seed(g, 100, seed_method="a-betweenness")
bet_rank <- sort(bet_seed)
clo_seed <- select_adaptive_seed(g, 100, seed_method="a-closeness")
clo_rank <- sort(clo_seed)
cor_seed <- select_adaptive_seed(g, 100, seed_method="a-coreness")
cor_rank <- sort(cor_seed)
eig_seed <- select_adaptive_seed(g, 100, seed_method="a-eigenvector")
eig_rank <- sort(eig_seed)

# CHECKING NETWORK CONNECTIVITY FALL
