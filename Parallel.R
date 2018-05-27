library(snow)
library(parallel)
library(foreach)
library(igraph)


# Apply example
#system.time(clusterApply(cl, 1:cores, function(x) Sys.sleep(1)))
# Foreach example
#system.time(foreach(x=list(1:cores))  %dopar% {Sys.sleep(1)})
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoSEQ()
start <- as.numeric(Sys.time())

k <- 4
graph <- erdos.renyi.game(250, p.or.m = 0.1)
clq <- cliques(graph, min=k, max=k)
edges <- c()
foreach (i=seq_along(clq)) %dopar% {
  for (j in seq_along(clq)) {
    unq <- unique(c(clq[[i]], clq[[j]]))
    if (length(unq) == k+1) {
      edges <- c(edges, c(i,j))
    }
  }
}

clq.graph <- simplify(graph(edges))
V(clq.graph)$name <- seq_len(vcount(clq.graph))
comps <- decompose.graph(clq.graph)
lapply(comps, function(x) { unique(unlist(clq[ V(x)$name ])) })


end <- as.numeric(Sys.time())
# Unregister cluster
stopCluster(cl)
print(end - start)
