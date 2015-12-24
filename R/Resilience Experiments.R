# Resillience of network: how fast a network goes down when nodes go down
# Read paper: Resilience of Social Networks Under Different Attack Strategies.pdf

library(igraph)
library(tkplot)
setwd("D:/Datasets/Twitter")
ds <- read.csv("my_twitter_network.txt", sep=' ', header=FALSE)
g <- graph.data.frame(ds, directed=TRUE, )
tkplot(g)

s <- select_adaptive_seed(g, k=)


Experiment (adaptive):
- Pick first node with highest degree/betweenness/closeness/coreness and rank it 1
- Remove this node
- Repeat this process to rank all nodes in the network
* To fasten the experiment, we can remove 1% nodes in each step rather than 1 node

Checking
- Remove rank 1 node and calculate size of largest connected component
- Repeat for whole network
