source('./graph_util.R')
source('./heuristics.R')
source('./community_detection.R')
source('./graph_util.R')
source('./influence_maximization.R')

require(igraph)

library(iterpc)

x <- NULL
y <- NULL
for (n in 100:125) {
  r <- 5
  l <- length(getall(iterpc(n, r)))
  x <- c(x, n)
  y <- c(y, l)
}
x
y
