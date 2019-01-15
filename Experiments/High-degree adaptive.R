## High Degree adaptive algorithm. First highest degree gets rank = 1; then that node is deleted and the graph
## is reread, then the highest degree node is ranked 2; repeated till all nodes are ranked

library(igraph)
edgesFile <- "data/sample_edgelist.txt"
edgedata <- read.table(edgesFile)
graph <- graph.data.frame(edgedata, directed = FALSE)
ranks <- data.frame(nodes = V(graph)$name, ranks = rep(0, length(V(graph))))
rank <- 1
# Repeat for all nodes have a rank
while (TRUE)
{
  degrees <- degree(graph)
  max_degree <- max(degrees)
  newnodes <- c()
  nodes <- V(graph)
  if (length(nodes) == 0)
    break
  for (node in nodes)
  {
    if (degree(graph, node) == max_degree)
    {
      ranks[node, 2] <- rank
      newnodes <- rbind(newnodes, node)
    }
  }
  print(paste("Removing nodes:", newnodes))
  graph <- delete.vertices(graph, newnodes)
  rank <- rank + 1
}
#ranks
